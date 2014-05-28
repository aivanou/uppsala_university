-module (three_sat_imp).

-export([solve_sat_problem/3,simplify_sat_expressions/1,
		 add_unused_variables/3,build_init_mapping/2,run_find_solution/4,
		 solve/3,test/1,find_true_expressions/3,evaluate_formula/2,minus/3]).

% @@ solves three-sat problem. 
% @Formula - list of tuples: [{v1,v2,v3},..]
% @_Jobs - number of threads, which will be used to find solution
% @ParentPid - the answer will be sent to this  pid

-spec (solve_sat_problem(list(),integer(),integer()) -> X when X::tuple()).

solve_sat_problem(Formula,_Jobs,_) when length(Formula) == 0 -> unsat;
solve_sat_problem(Formula,_Jobs,ParentPid) -> 
	io:format("trying to solve: ~p my pid: ~p~n",[Formula,self()]),
	% process_flag(trap_exit, true),
	Answer = case is_sat_formula(Formula) of
		false -> unsat;
		true -> NewFormula = preprocess_formula(Formula,[]),
				DistVars = distinct_variables(NewFormula,sets:new()),
				io:format("distinct_variables ~p ~n",[length(DistVars)]),
				Result = case solve(lists:reverse(NewFormula),DistVars,_Jobs) of
					{sat,Ans} -> {sat,to_boolean_list(lists:sort(add_unused_variables(Ans,1,lists:max(DistVars))),[])};
					unsat -> unsat;
					false -> unsat
				end,
				Result				
	end,
	% io:format("getting answer ~p~n",[Answer]),
	% io:format("I SHOULD SEND ANSWER HERE ~p~n",["rs"]),
	% Answer.
	ParentPid!Answer.
	
to_boolean_list([],Lst) -> Lst;
to_boolean_list([{_,V}|Rest],Lst) -> to_boolean_list(Rest,lists:append(Lst,[V])).

% @@ input formula can have view: [{v1,v200,v400}],
% @@ to avoid excess computation we use only variables 1,200,400
% @@ and then use add_unused_variables function to add "dummy" variables

-spec (add_unused_variables(list(),integer(),integer()) -> list()).

add_unused_variables(Ans,CurrVariable,MaxVariable) when CurrVariable > MaxVariable -> Ans;
add_unused_variables(Ans,CurrVariable,MaxVariable) -> 
	case has_variable(CurrVariable,Ans) of
		true -> add_unused_variables(Ans,CurrVariable+1,MaxVariable);
		false -> add_unused_variables([{CurrVariable,true}|Ans],CurrVariable+1,MaxVariable)
	end.


has_variable(_,[]) -> false;
has_variable(Var,[{V,_}|_]) when V==Var -> true;
has_variable(Var,[_|Rest]) -> has_variable(Var,Rest).


% @@ deletes same tuples

-spec (preprocess_formula(list(),list()) -> list()).

preprocess_formula([],NewFormula) -> NewFormula;
preprocess_formula([{Var1,Var2,Var3}|Rest],NewFormula) -> 
	case has_similar_pair({Var1,Var2,Var3},Rest) of
		unsat -> unsat;
		true -> preprocess_formula(Rest,NewFormula);
		false -> preprocess_formula(Rest,[{Var1,Var2,Var3}|NewFormula])
	end.

-spec (has_similar_pair(tuple(),list()) -> boolean()).

has_similar_pair(_,[]) -> false;
has_similar_pair({V1,V2,V3},[{Var1,Var2,Var3}|Rest]) ->
	case lists:sort([V1,V2,V3]) == lists:sort([Var1,Var2,Var3]) of
		true ->true;
		false -> has_similar_pair({V1,V2,V3},Rest)
	end.

% @@ checks if formula has a 3-sat view

-spec (is_sat_formula(list()) -> boolean()).

is_sat_formula([]) -> true;
is_sat_formula([{Var1,Var2,Var3}|Rest]) when is_integer(Var1) andalso 
											 is_integer(Var2) andalso 
											 is_integer(Var3)  ->
											 is_sat_formula(Rest);
is_sat_formula(_) -> false.
	
% @@ returns list of distinct variables
% @@ example: [{v1,v2,v3},{v1,v2,v4}] -> [1,2,3,4]

-spec (distinct_variables(list(),list()) -> list()).

distinct_variables([],Variables) -> lists:sort(sets:to_list(Variables));
distinct_variables([{Var1,Var2,Var3}|Rest],Variables) -> 
	distinct_variables(Rest,sets:add_element(abs(Var3),
					   sets:add_element(abs(Var2),sets:add_element(abs(Var1),Variables)))).

% @@ solves the 3-sat problem
% @@ it spawns processes, which are trying to solve the problem
% @@ then, it will wait for the solution

-spec (solve(list(),list(),integer()) -> tuple()).

solve(Formula,Variables,Jobs) ->
	Args = build_args(Formula,Variables,Jobs,self()),
	% io:format("ARGS: ~p ~n",[Args]),
	Children = spawn_processes(Args),
	wait_for_solution(Children,0,unsat).

wait_for_solution(Processes,ReceivedAnswers,A) when length(Processes) == ReceivedAnswers -> A;
wait_for_solution(Processes,ReceivedAnswers,A) ->
	receive
		{sat,Ans} -> %io:format("ANR ~p ~n",[Ans]),
					 exit_processes(Processes,finished),
					 {sat,Ans};
		unsat -> wait_for_solution(Processes,ReceivedAnswers+1,A);
		false -> wait_for_solution(Processes,ReceivedAnswers+1,A)
	end.

spawn_processes([]) -> [];
spawn_processes([Args|Rest]) ->P=spawn(three_sat_imp,run_find_solution,Args),link(P), [P | spawn_processes(Rest)].

exit_processes([],_) -> nothing; 
exit_processes([Pid|Rest],Reason) -> exit(Pid,Reason), exit_processes(Rest,Reason).

% @@ builds the args for child processes
% @@ if we have N jobs, we need to build N initial boolean mappings
% @@ for example: if N= 4, we should build: [[true,true],[true,false],[false,false],[false,true]]
% @@ so, each process will solve a part of problem

build_args(Formula,Variables,Jobs,Pid) -> 
	% we suppose, that the jobs are the power of 2
	NVars=round(math:log(Jobs) / math:log(2)),
	Mapping = build_init_mapping(NVars,[]),
	case Mapping of
		[] -> [[Pid,Formula,Variables,dict:new()]];
		_ -> {NewVariables,IVariables} = select_variables(Formula,Variables,NVars), 
			 [[Pid,Formula,NewVariables,Lst] || Lst <- translate_to_dicts(Mapping,IVariables)]
	end.

select_variables(Formula,Variables,NVars) -> 
	InitVars = select_variables_from_dict(build_frequency_array(Formula,dict:new()),NVars),
	% io:format("AAA ~p ~p ~p~n",[minus(Variables,InitVars,[]),InitVars,Variables]),
	{minus(Variables,InitVars,[]),InitVars}.

minus(LST,[],L)  -> lists:sort(lists:append(L,LST));
minus([],_,L)  -> lists:sort(L);
minus([V1|R1],[V2|R2],L) when V1==V2 -> minus(R1,R2,L);
minus([V1|R1],[V2|R2],L) when V1>V2 -> minus([V1|R1],R2,L);
minus([V1|R1],[V2|R2],L) when V1<V2 -> minus(R1,[V2|R2],[V1|L]).

	
select_variables_from_dict(_,0) -> [];
select_variables_from_dict([{_,V}|Rest],N) -> [V|select_variables_from_dict(Rest,N-1)].

build_frequency_array([],Freq) -> %io:format("FREQ ARR: ~p~n",[lists:sort(fun(X,Y) -> X>Y end, swap_keys_values(dict:to_list(Freq)))]),
								  lists:sort(fun(X,Y) -> X>Y end, swap_keys_values(dict:to_list(Freq)));
build_frequency_array([{V1,V2,V3}|Rest],Freq) -> build_frequency_array(Rest,update_freq_dict([V1,V2,V3],Freq)).

swap_keys_values([]) -> [];
swap_keys_values([{K,V}|R]) -> [{V,K}|swap_keys_values(R)].

update_freq_dict([],Dict) -> Dict;
update_freq_dict([V|Rest],Dict) -> 
	case dict:find(abs(V),Dict) of
		error -> update_freq_dict(Rest,dict:store(abs(V),1,Dict));
		{ok,N} -> update_freq_dict(Rest,dict:store(abs(V),N+1,Dict))
	end.


translate_to_dicts([],_) -> [];
translate_to_dicts([InitMapping|Rest],Variables) ->
	[translate_to_dict(InitMapping,Variables,dict:new())|translate_to_dicts(Rest,Variables)].


translate_to_dict(_,[],Dict) -> Dict;
translate_to_dict([],_,Dict) -> Dict;
translate_to_dict([Value|Rest],[Variable|RestVars],Dict) ->
	translate_to_dict(Rest,RestVars,dict:store(Variable,Value,Dict)).
	

build_init_mapping(0,Mapping) -> Mapping;
build_init_mapping(Len,[]) -> build_init_mapping(Len-1,[[true],[false]]);
build_init_mapping(Len,Mapping) -> 	build_init_mapping(Len-1,lists:append([[true|Lst] || Lst <-Mapping],[[false|Lst] || Lst <-Mapping])).


% calls solution function and then sends a message to parend process

run_find_solution(ParentPid,Formula,Variables,BooleanDict) ->
	Solution = find_solution_imp(Formula,BooleanDict,Variables),
	%io:format("some proc got solution: ~p~n",[Solution]),
	ParentPid!Solution.

% evaluates formula and call's searchig algorithm

find_solution_imp(Formula,BooleanDict,Variables) ->
	% io:format("~p~n",[NFormula]),
	case evaluate_formula(Formula,BooleanDict) of
    	unsat -> false;
    	F -> NVariables=intersect(lists:sort(Variables),lists:sort(distinct_variables(F,sets:new()))),
    		 % io:format("NNF : ~p ~p ~n",[F,NVariables]),
			 run_solution_imp(F,BooleanDict,NVariables)
	end.

run_solution_imp([],BooleanDict,_) -> {sat,dict:to_list(BooleanDict)};
run_solution_imp(Formula,BooleanDict,[]) -> 
	T=simplify_sat_expressions(substitute_variables(Formula,BooleanDict)),
	% io:format("Simplification: ~p   ~p   ~n",[dict:to_list(BooleanDict),Formula]),
	T;
run_solution_imp(Formula,BooleanDict,[Variable|Rest]) ->
	% io:format("running with ~p       ~p~n",[dict:to_list(BooleanDict),Formula]),

	case find_solution_imp(Formula,dict:store(Variable,true,BooleanDict),Rest) of
		true ->  {sat,dict:to_list(dict:store(Variable,true,BooleanDict))};
		false ->
		 case find_solution_imp(Formula,dict:store(Variable,false,BooleanDict),Rest) of
		 	true -> {sat,dict:to_list(dict:store(Variable,false,BooleanDict))};
		 	false -> false;
		 	{sat,AnswerMapping} -> {sat,AnswerMapping};
		 	unsat -> unsat
		 end;
		{sat,AnswerMapping} -> {sat,AnswerMapping};
		unsat -> unsat
	end.	



% partially solves the formula and deletes expressions that produce true
% it theory this can simplify the formula and reduce the amout of unknows variables

evaluate_formula(Formula,BooleanDict) ->
	case find_true_expressions(Formula,BooleanDict,[]) of
		unsat -> unsat;
		[] -> Formula;
		TExpr -> delete_expressions(TExpr,Formula)
	end.

find_true_expressions([],_,TExpr) -> TExpr;
find_true_expressions([{V1,V2,V3}|Rest],BooleanDict,TExpr) ->
	case partial_solve({V1,V2,V3},BooleanDict) of
		true -> find_true_expressions(Rest,BooleanDict,[{V1,V2,V3}|TExpr]);
		unknown -> find_true_expressions(Rest,BooleanDict,TExpr);
		false -> unsat
	end.

delete_expressions([],Formula) -> Formula;
delete_expressions([Expr|Rest],Formula) ->	delete_expressions(Rest,delete_expression(Expr,Formula)).

delete_expression(_,[]) -> [];
delete_expression({V1,V2,V3},[{Var1,Var2,Var3}|Rest]) ->
	case  lists:sort([V1,V2,V3]) == lists:sort([Var1,Var2,Var3]) of
		true ->	delete_expression({V1,V2,V3},Rest);
		false ->  [{Var1,Var2,Var3}|delete_expression({V1,V2,V3},Rest)]
	end.


% partially solves the formula

partial_solve({Var1,Var2,Var3},BooleanDict) ->
	simplify_from_dict({dict:find(Var1,BooleanDict),dict:find(Var2,BooleanDict),dict:find(Var3,BooleanDict)}).

simplify_from_dict({{ok,true},_,_}) -> true;
simplify_from_dict({_,{ok,true},_}) -> true;
simplify_from_dict({_,_,{ok,true}}) -> true;
simplify_from_dict({error,_,_}) -> unknown;
simplify_from_dict({_,error,_}) -> unknown;
simplify_from_dict({_,_,error}) -> unknown;
simplify_from_dict({_,_,_}) -> false.



% substitutes variables from the dictionary

substitute_variables([],_) -> [];
substitute_variables([{Var1,Var2,Var3}|Rest],BooleanDict) ->
	[{subs_variable(Var1,dict:find(abs(Var1),BooleanDict)),
	  subs_variable(Var2,dict:find(abs(Var2),BooleanDict)),
	  subs_variable(Var3,dict:find(abs(Var3),BooleanDict))}|substitute_variables(Rest,BooleanDict)].
      

subs_variable(_,error) -> error;
subs_variable(V,{ok,BVal}) ->
	case V >0 of
		true -> BVal;
		false -> not(BVal) 
	end.


% @@ input - list of boolean tuples which represent sat-boolean expression
% @@ output - simplified boolean value
% @@ example: [{true,true,false}] -> true

-spec (simplify_sat_expressions(list()) -> boolean()).

simplify_sat_expressions([]) -> true;
simplify_sat_expressions([Expression|Rest]) ->
	case simplify_sat_expression(Expression) of
		true -> simplify_sat_expressions(Rest);
		unknown -> simplify_sat_expressions(Rest);
		false -> false
	end.

-spec (simplify_sat_expression(tuple()) -> boolean()).

simplify_sat_expression({true,_,_}) -> true;
simplify_sat_expression({_,true,_}) -> true;
simplify_sat_expression({_,_,true}) -> true;
simplify_sat_expression({error,_,_}) -> unknown;
simplify_sat_expression({_,error,_}) -> unknown;
simplify_sat_expression({_,_,error}) -> unknown;
simplify_sat_expression({_,_,_}) -> false.

% ------------- HELPING FUNCTIONS -------------------------------------

intersect(_,[]) -> [];
intersect([],_) -> [];
intersect([El1|Rest1],[El2|Rest2]) when El1 == El2 -> [El1|intersect(Rest1,Rest2)];
intersect([El1|Rest1],[El2|Rest2]) when El1 > El2 -> intersect([El1|Rest1],Rest2);
intersect([El1|Rest1],[El2|Rest2]) when El1 < El2 -> intersect(Rest1,[El2|Rest2]).



now_us({MegaSecs,Secs,MicroSecs}) -> (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

% --------------------- TESTING ----------------------

test(T) ->
	StartTime=now_us(now()),
	Formula=[{30,-23,-8},
                  {-15,21,3},
                  {-1,-2,-1},
                  {-21,11,-6},
                  {-4,-6,-1},
                  {10,-13,-15},
                  {-8,-15,-20},
                  {15,-26,1},
                  {-29,25,11},
                  {22,-23,-29},
                  {20,-14,-11},
                  {7,-16,14},
                  {-26,-22,7},
                  {3,-29,-11},
                  {5,-3,-29},
                  {-1,-28,19},
                  {-9,8,-22},
                  {11,27,-6},
                  {18,-2,25},
                  {-1,-13,4}],
	 {_,A}=solve_sat_problem(Formula,T,1),
		io:format("~p Took:  ~p~n",[A,(now_us(now())-StartTime)/100000]),
		io:format("~p~n",[simplify_sat_expressions(substitute_variables(Formula,to_dict(A,1,dict:new())))]).

to_dict([],_,D) -> D;
to_dict([V|R],I,D) -> to_dict(R,I+1,dict:store(I,V,D)).

% [{1,1,1},{-1,-1,-1},{2,2,2},{-2,-2,-2},{3,3,3},{-3,-3,-3},{4,4,4},{-4,-4,-4},{5,5,5},{-5,-5,-5},{6,6,6},{-6,-6,-6},{7,7,7},{-7,-7,-7},{8,8,8},{-8,-8,-8},{9,9,9},{-9,-9,-9},{10,10,10},{-10,-10,-10},{11,11,11},{-11,-11,-11},{12,12,12},{-12,-12,-12},{13,13,13},{-13,-13,-13},{14,14,14},{-14,-14,-14},{15,15,15},{-15,-15,-15}]

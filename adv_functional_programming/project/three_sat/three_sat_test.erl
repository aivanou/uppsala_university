

% @@ TODO: make three-sat multithreading


-module (three_sat_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type digits() :: -1|-2|-3|-4|-5|-6|-7|-8|-9|-10|-11|-12|-13|-14|-15|-16|-17|-18|-19|-20|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20.

-type expr() :: {digits(),digits(),digits()}.

-export ([solve_formula/2]).


prop_ordered() ->
	?FORALL(Formula, list(expr()), 
		begin
			TestAns = case three_sat_imp:solve_sat_problem(Formula,1,1) of
						{sat,Ans} -> solve_formula(Formula,to_dict(Ans,1,dict:new()));
						unsat   -> true
					 end,
			?WHENFAIL(
					begin
						io:format("~p   ~p   ~n",[Formula,TestAns])	
					end	,TestAns =:= true)
		end).

to_dict([],_,D) -> D;
to_dict([V|R],I,D) -> to_dict(R,I+1,dict:store(I,V,D)).

solve_formula([],_) -> true;
solve_formula([{V1,V2,V3}|Rest],Dict) ->
	case solve_expr([V1,V2,V3],Dict) of
		true -> solve_formula(Rest,Dict);
		false -> false
	end.


solve_expr([],_) ->false;
solve_expr([V|Rest],Dict) -> 
	case dict:find(abs(V),Dict) of
		{ok,BoolVal} -> case solve_variable(V,{ok,BoolVal}) of
					 		true -> true;
					 		false -> solve_expr(Rest,Dict)
					 end;
		_ -> solve_expr(Rest,Dict)
	end.

solve_variable(V,{ok,BVal}) ->
	case V>0 of
		true -> BVal;
		false -> not(BVal)
	end.


subs(_,[]) -> [];
subs(Ans,[Expr|Rest]) -> [subs_expression(Ans,Expr)|subs(Ans,Rest)].

	

subs_expression(Ans,{V1,V2,V3}) -> 
	{subs_variable(Ans,V1,1),subs_variable(Ans,V2,1),subs_variable(Ans,V3,1)}.



subs_variable([BlVar|Rest],Var,Index) when Index == abs(Var) ->
	case Var > 0  of
		true -> BlVar;
		false -> not(BlVar)
	end;
subs_variable([BlVar|Rest],Var,Index) -> subs_variable(Rest,Var,Index+1).



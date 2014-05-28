-module(bughunt).

-export([computeExpr/2,evaluateResponse/2,test/1,prop_evaluate/1,testAll/2]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type vector() :: [integer(),...].

-type expr() ::  vector()
			  |  {vector_op(),expr(),expr()}
			  |  {scalar_op(),int_expr(),expr()}.

-type int_expr() :: integer()
                | {norm_op(),expr()}.

-type vector_op() :: 'add'|'sub'|'dot'.
-type scalar_op() :: 'mul'|'div'.
-type norm_op() :: 'norm_one'|'norm_inf'.

testAll(Iter,Failed) when Iter>50 -> Failed;
testAll(Iter,Failed) -> Resp=test(Iter),
						case Resp=:=true of
							true-> testAll(Iter+1,Failed);
							false -> testAll(Iter+1,Failed+1)
						end.


test(Id) ->
	_Name = list_to_atom(lists:flatten(io_lib:format("vector_~p",[Id]))),
	Func=vectors:vector(Id),
	proper:quickcheck(?MODULE:prop_evaluate(Func),2000).


prop_evaluate(Func) -> 
		?FORALL(Expr, expr(),
			begin
				VectorResult=Func(Expr),
				MyResult=computeExpr(Expr,0),
				?WHENFAIL(
					begin
						{Msg,_Temg} = evaluateResponse(Expr,Func),
						io:format("~p~n",[{Expr,MyResult,VectorResult,Msg}])	
					end	,VectorResult =:= MyResult)
			end).

evaluateResponse({'mul',V1,V2},Func) -> 
	evaluateVector(evaluateResponse(V1,Func),evaluateResponse(V2,Func),{'mul',V1,V2},Func);
evaluateResponse({'div',V1,V2},Func) ->
	evaluateVector(evaluateResponse(V1,Func),evaluateResponse(V2,Func),{'div',V1,V2},Func);
evaluateResponse({'add',V1,V2},Func) ->
	evaluateVector(evaluateResponse(V1,Func),evaluateResponse(V2,Func),{'add',V1,V2},Func);
evaluateResponse({'sub',V1,V2},Func) ->
	evaluateVector(evaluateResponse(V1,Func),evaluateResponse(V2,Func),{'sub',V1,V2},Func);
evaluateResponse({'dot',V1,V2},Func) ->
	evaluateVector(evaluateResponse(V1,Func),evaluateResponse(V2,Func),{'dot',V1,V2},Func);
		 
evaluateResponse({'norm_one',V},Func) -> 
	evaluateScalar(evaluateResponse(V,Func),{'norm_one',V},Func);
evaluateResponse({'norm_inf',V},Func) ->
	evaluateScalar(evaluateResponse(V,Func),{'norm_inf',V},Func);

evaluateResponse(V,Func) when is_list(V) -> 
	VectorValue=Func(V),
	case is_list(VectorValue) of
 		true -> {"Ok",V};
 		false -> {"Wrong type, expected list",'error'}
	end;

evaluateResponse(V,Func) when is_integer(V) ->  
	VectorValue=Func(V),
	case VectorValue=:='error' of
 		true -> {"Ok",V};
 		false -> {"Wrong type, expected integer",'error'}
	end.

evaluateScalar({Msg,'error'},{_Op,_V},_Func) -> {Msg,'error'};
evaluateScalar({_Msg,TempV},{Op,_V},Func) -> 
	Value=Func({Op,TempV}),
	MyValue=computeExpr({Op,TempV},0),
	compareResponses(Value,MyValue,Op).

evaluateVector({_Msg1,_TempV1},{Msg2,'error'},{_Op,_V1,_V2},_Func) -> {Msg2,'error'};
evaluateVector({Msg1,'error'},{_Msg2,_TempV2},{_Op,_V1,_V2},_Func) -> {Msg1,'error'};
evaluateVector({_Msg1,TempV1},{_Msg2,TempV2},{Op,_V1,_V2},Func) -> 
	Value=Func({Op,TempV1,TempV2}),
	MyValue=computeExpr({Op,TempV1,TempV2},0),
	compareResponses(Value,MyValue,Op).
	


compareResponses(Value,MyValue,Op) ->
	case MyValue /= Value of
		true ->
				case Value =:= 'error' of
					true -> {"Unsupported "++atom_to_list(Op)++" operation",'error'};
					false -> {"Wrong answer to "++atom_to_list(Op)++" operation",'error'}
				end;
		false -> {"Ok",Value}

	end.


%% Just expression computation  
computeExpr(_,Depth) when Depth>100 -> 'error';

computeExpr({dot,V1,V2},Depth) ->
	runVectorFunction(computeExpr(V1,Depth+1),computeExpr(V2,Depth+1),fun(A,B)-> A*B end);
computeExpr({sub,V1,V2},Depth) ->
	runVectorFunction(computeExpr(V1,Depth+1),computeExpr(V2,Depth+1),fun(A,B)-> A-B end);
computeExpr({add,V1,V2},Depth) ->
	runVectorFunction(computeExpr(V1,Depth+1),computeExpr(V2,Depth+1),fun(A,B)-> A+B end);

computeExpr({'div',V1,V2},Depth) ->
	
	TempV1=computeExpr(V1,Depth+1),
	case TempV1 /= 0 of
		true -> runScalarFunction(TempV1,computeExpr(V2,Depth+1),fun(A,B)-> A div B end);
		false -> 'error'
	end;	
computeExpr({mul,V1,V2},Depth) ->
	runScalarFunction(computeExpr(V1,Depth+1),computeExpr(V2,Depth+1),fun(A,B)-> A*B end);

computeExpr({norm_one,V},Depth) ->
	TempV=computeExpr(V,Depth+1),
	case TempV /='error' of
		true -> lists:foldl(fun(El,Sum) -> abs(El)+Sum end, 0 ,TempV);
		false -> 'error'
	end;
	
computeExpr({norm_inf,V},Depth) -> 
	TempV=computeExpr(V,Depth+1),
	case TempV /='error' of
		true -> lists:foldl(fun(El,Max) -> max(abs(El),Max) end,-1,TempV);
		false -> 'error'
	end;
	

computeExpr(Value,_Depth) when is_integer(Value) -> Value;
computeExpr(Value,_Depth) when is_list(Value) andalso length(Value) >0 andalso length(Value) < 101 -> Value;
computeExpr(Value,_Depth) when is_list(Value)  -> 'error'.

runVectorFunction('error',_Arg2,_OP) -> error;
runVectorFunction(_Arg1,'error',_OP) -> error;
runVectorFunction(Arg1,Arg2,OP) -> 
	computeListProduct(Arg1,Arg2,[],OP).


runScalarFunction('error',_Arg2,_Fun) -> error;
runScalarFunction(_Arg1,'error',_Fun) -> error;
runScalarFunction(Arg1,Arg2,Fun) -> 
	lists:map(fun(Arg) -> Fun(Arg,Arg1) end,Arg2).

computeListProduct([],[],Acc,_Op) -> lists:reverse(Acc);
computeListProduct(Lst1,Lst2,_Acc,_Op) when length(Lst1) /= length(Lst2) -> 'error';
computeListProduct([El1|Rest1],[El2|Rest2],Acc,Op) ->
	computeListProduct(Rest1,Rest2,[Op(El1,El2)|Acc],Op).


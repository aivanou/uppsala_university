-module(vector_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
         start_link/1,
         start_link/0,
         stop/0
         ]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock}).


start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
    start_link(?DEFAULT_PORT).

stop() ->
    gen_server:cast(?SERVER, stop).


init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [compute(RawData)])),
    {noreply, State};

handle_info({tcp_closed, Socket}, #state{lsock = LSock} = State) ->
	gen_tcp:close(Socket),
	gen_tcp:accept(LSock),
    {noreply, State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

compute(RawData) ->
	try
		[Expr|_Rest]=args_to_terms(RawData),
		computeExpr(Expr,0)
	catch error:_Error -> 'error'
	end.		


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



% @@@ Specification: 
% @@@ client can enter message that can be consisted of multiple lines, so we suppose that the last character he enters is : "]"
% @@@ client can enter not full message and wait forever, eventually we should drop connection, heartbeat responses for that
% @@@ we can accept maximum: 8 permannent connections, after finishing request, these clients can enter another request
% @@@ if all 8 connections are busy, there can be additional 8 clients which status will be: temporary, 
% @@@ so, the maximum amout of simultaneous clients: 16
% @@@ temporary clients can enter only 1 request, and wait maximum: 10 seconds
% @@@ after finishig, permanent listeners are staying alive
% @@@       temporary listeners will die
% @@@ unfortunately, if client unexpectedly closes connection, gen_tcp:close(client_socket) will do nothing
% @@@ so, based on client type and response from @wait_solution we are building exit message

% @@@ At the beginning 9 listeners are waiting for connections
% @@@ After accepting connection, they send message to scheduler, which decides what type will client have
% @@@ there can be 3 types: permanent,temporary,forbidden
% @@@ permanent -- usual client, that can enter multiple tasks 
% @@@ temporary -- it can enter only 1 task and wait maximum 10 seconds before closing
% @@@ forbidden -- just sending "ignored message" and closing connection
% @@@ Every server has its own global name, that has template: server_id[integer()]
% @@@  it is necessary for heartbeat to send stopping requests
% @@@ Now , when listener accepts client, new heartbeat process starts


-module (sat_server).

-behaviour(gen_server).

-export([
         start_link/3,
         strip/2,
         string_to_pair/3,
         heartbeat/1,
         start/0
       ]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3,solver/3]).

-define(SERVER, ?MODULE).

% @@ represents the client request
% @@ message - request from client
% @@ heartbeat_pid - pid of heartbeat process
% @@ type - client type (permanent,temporary,forbidden)
% @@ listener_id - unique listener integer number
% @@ sat_regexp - checks the correctness of sat formula

-record(state, {server_socket,client_socket,
                message="",max_symbols=1000,
                heardbeat_pid,
                scheduler_pid,
                solver_pid=none,
                type,
                is_busy,
                listener_id
                }).

start() -> server_supervisor:start_link(3547).

%  @@ atom(): server_id ++ (listener_id)

get_server_id(ListenerId) -> 
    {_,[{_,_,ServerId}],_} = erl_scan:string("sat_server_"++lists:flatten(io_lib:format("~p", [ListenerId]))),
    ServerId.

% @@ start_ling will call init function
% @@ init function will send asynchronous message to gen_server
% @@ this will trigger handle_cast function with atom: accept
% @@ it is okay, that multiple listeners are calling accept at the same time, erlang handles this.

start_link(Socket,SPid,ListenerId) ->
    log_message("starting server with id: ~p ~n",[ListenerId]),
    gen_server:start_link({global,get_server_id(ListenerId)},?MODULE, [Socket,SPid,ListenerId], []).

init([Socket,SPid,ListenerId]) ->
    % process_flag(trap_exit, true),
    gen_server:cast(self(),accept),
    {ok, #state{server_socket = Socket,scheduler_pid=SPid,listener_id=ListenerId,is_busy=false}}.



handle_call(finished_task,_From,State=#state{listener_id=LId,type=Type})->
    io:format("In the finished task area~n"),
    case Type of
        permanent -> {reply,{ok,nothing},State#state{is_busy=false,solver_pid=none}};
        _ -> gen_server:cast({global,get_server_id(LId)},stop), {reply, {ok, State}, State}
    end;
handle_call(finished_with_error,_From,State=#state{listener_id=LId}) ->  
    io:format("In the finished with error task area~n"),
    gen_server:cast({global,get_server_id(LId)},stop),
    {reply, {ok, State}, State};
handle_call(_, _From, State) ->
    {reply, {ok, State}, State}.



handle_cast(accept, State=#state{server_socket = LSock,scheduler_pid=SPid,listener_id=LId}) ->
    log_message("Server ~p : trying to accept socket~n",[LId]),
    {ok,ClientSocket}=gen_tcp:accept(LSock),
    log_message("Server ~p : accepted socket~n",[LId]),
    SPid!{acquire,self()},
    Type = receive % after sending acquire signal we are waiting for response from scheduler
        temporary -> temporary;
        permanent -> permanent;
        forbidden -> forbidden
    end,      
    log_message("Server ~p : Client type : ~p ~n",[LId,Type]),
    handle_accepted_socket(State#state{client_socket=ClientSocket,message=""},Type);  

handle_cast(stop, State=#state{client_socket=ClSocket,listener_id=LId}) ->
    log_message("Server ~p : trying to close connection~n",[LId]),
    ok = gen_tcp:close(ClSocket),
    {stop, normal, State}.

handle_accepted_socket(State=#state{client_socket=_ClientSocket},forbidden)->
    send(State,busy),
    % gen_tcp:close(ClientSocket),
    % gen_server:cast({global,get_server_id(LId)},accept),
    {stop,normal,State};
handle_accepted_socket(State=#state{client_socket=ClientSocket,listener_id=LId},Type) ->
    log_message("Server ~p : Starting heartbeat~n",[LId]),
    case send(State,hello) of
        ok -> HPid = spawn(sat_server,heartbeat,[State#state{client_socket=ClientSocket,type=Type}]),
              {noreply,State#state{client_socket = ClientSocket,heardbeat_pid=HPid,type=Type}};
        {error,Error} -> log_message("Server ~p : Recieved Error while sending ~p~n",[LId,Error]),
                         % gen_tcp:close(ClientSocket),
                         {stop,normal,State}
    end.


send(#state{client_socket=ClientSocket,server_socket=ServerSocket,listener_id=LId},Message) ->
    SendSignal = case gen_tcp:send(ClientSocket, io_lib:fwrite("~p~n", [Message])) of
        ok -> ok ;
        {error,closed} -> log_message("Server ~p : Error, received closed connection, while sending~n",[LId]),
                {error,closed};
        {error,timeout} -> log_message("Server ~p : Error, received timeout , while sending~n",[LId]),
                {error,timeout};
        {error,_SomethingElse} ->  log_message("Server ~p : Error, received ~p , while sending~n",[LId,_SomethingElse]),
                {error,_SomethingElse}
    end,
    ok = inet:setopts(ServerSocket, [{active, once}]),
    SendSignal.

handle_info({tcp,_ClientSocket,Data},State=#state{message=Message,max_symbols=MaxSymbols}) 
                                        when length(Data)+length(Message) > MaxSymbols ->
    send(State,ignored),
    {noreply,State#state{message=""}};

% @@ when client finishes his request, it spawns a new process and waits for the solution
% @@ for permanent clients, it waits for the solution forever or closes, after connection lost
% @@ for temporary clients, it waits for 10 seconds and then finishes, even if the task has not been finished yet
% @@ it sends "processing_request" to Heartbeat, so heartbeat does not drop the connection

handle_info({tcp,ClientSocket,_},State=#state{type=forbidden}) ->
    gen_tcp:close(ClientSocket),
    {stop, normal, State};

handle_info({tcp,ClientSocket,Data},State=#state{is_busy=IsBusy,solver_pid=SoPid,
                heardbeat_pid=HPid,listener_id=LId}) when is_pid(SoPid) andalso IsBusy==true->
    log_message("Server ~p received2: ~p ~n",[LId,Data]),
    HPid!processing_request,
    inet:setopts(ClientSocket, [{active, once}]),
    case strip(Data,[]) of
        "abort" -> SoPid!aborted,
                   send(State,aborted),
                     {noreply,State#state{is_busy=false,solver_pid=none,message=""}};
        _ -> send(State,ignored),
             {noreply,State}
    end;

handle_info({tcp,ClientSocket,Data},State=#state{message=Message,
                 heardbeat_pid=HPid,listener_id=LId,type=Type}) when Type==permanent orelse Type==temporary ->
    log_message("Server ~p received1 : ~p ~n",[LId,Data]),
    HPid!processing_request,
    inet:setopts(ClientSocket, [{active, once}]),

    % 93 = "]" symbol in ASCII, so we are thinking that client will stop his request when he enters this symbol
    case lists:member(93,Data) of
        true  -> case preprocess(lists:append(Message,Data)) of 
                    {error,_} -> send(State,ignored),
                                 {noreply,State#state{message=""}};
                    SatFormula -> case send(State,trying) of
                        % running solution process with 2 parallel threads
                                    ok -> SolverPid=spawn(sat_server,solver,[State,SatFormula,2]),
                                          % link(SolverPid),
                                          {noreply,State#state{solver_pid=SolverPid,is_busy=true}};
                                    _ -> {stop,normal,State}
                                  end
                 end;
        false -> {noreply,State#state{message=lists:append(Message,Data)}}
    end;    

handle_info({tcp,_,_},State) ->
    do_nothing,
    {stop, normal, State};

handle_info({tcp_closed, _ClientSocket}, State=#state{listener_id=LId}) ->
    log_message("Server ~p :Closing client socket~n",[LId]),
    {stop, normal, State};

handle_info({tcp_error, _Socket, _}, State=#state{listener_id=LId}) ->
    log_message("Server ~p :Received tcp error~n",[LId]),
    {stop, normal, State};
    
handle_info(timeout, #state{server_socket = _LSock} = State) ->
    {noreply, State}.



terminate(_Reason, #state{client_socket=CSocket,scheduler_pid=SPid,is_busy=IsBusy,solver_pid=SolverPid,
                                    heardbeat_pid=HPid,type=Type,listener_id=LId}) when Type==temporary orelse Type==permanent->
    log_message("Server ~p, , ~p trying to shutdown ~n",[LId,is_pid(SolverPid)]),
    case IsBusy of
        true -> exit(SolverPid,terminate);
        false -> nothing
    end,
    gen_tcp:close(CSocket),
    exit(HPid,finished),
    SPid!{free,Type,LId},
    log_message("Server ~p, calling1 new listener!  ~p ~n",[LId,is_pid(SPid)]),
    % server_supervisor:listen(LId+10),
    % SPid!{new_listener,LId+100},
    server_supervisor:listen(LId+100),
    ok;
terminate(_Reason, #state{client_socket=CSocket,listener_id=LId,scheduler_pid=SPid})->
    log_message("Server ~p, trying to shutdown ~n",[LId]),
    gen_tcp:close(CSocket),
    log_message("Server ~p, calling2 new listener! ~p ~n",[LId,SPid]),
    % server_supervisor:listen(LId+10),
    SPid!{new_listener,LId+100},
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%cleaning sat formula from symbols that have less then 33 number in ASCII
preprocess(Data) -> 
    SatFormula = strip(Data,[]),
    case is_sat_formula(SatFormula) of
        true -> SatFormula;
        false -> {error, not_sat_formula}
    end.


solver(State=#state{listener_id=LId},SatFormula,Jobs) -> 
        P=spawn(three_sat_imp,solve_sat_problem,[string_to_pair(SatFormula,[],[]),Jobs,self()]),
        link(P),
        case wait_solution(State,P) of
            ok ->  gen_server:call({global,get_server_id(LId)},finished_task);
            user_aborted -> do_nothing;
            _ ->    exit(P,error), gen_server:call({global,get_server_id(LId)},finished_with_error)
        end.



% @@ waits solution from another process

wait_solution(State=#state{type=Type,listener_id=LId,heardbeat_pid=_HPid},SolvePid) ->
    receive 
        unsat -> send(State,unsat);
        {sat,Ans} -> log_message("Server ~p , received: ~p~n",[LId,Ans]),
                    send(State,{sat,Ans});
        aborted -> exit(SolvePid,aborted), user_aborted
    after 
        10000 -> case send(State,trying) of
                    ok ->   case Type of
                                permanent -> wait_solution(State,SolvePid);
                                temporary -> send(State,aborted), aborted
                            end;

                    {error,Msg} -> {error,Msg}
                end
    end.

heartbeat(State=#state{listener_id=LId}) ->
    receive
        processing_request -> heartbeat(State);
        stop -> io:format("Server ~p stopping heartbeat~n",[LId]),
                ok
    after 
        60000 ->
            log_message("Heartbeat ~p: Sending stop request~n",[LId]),
            % gen_tcp:close(ClientSocket),
            send(State,timeout),
            gen_server:cast({global,get_server_id(LId)},stop)                
    end.

% ----------------------------------
% @@@ helping functions 

is_sat_formula(Data) -> 
    {ok,Patt}=re:compile("^\\[\\{\\-?\\d+\\,\\-?\\d+\\,\\-?\\d+\\}(\\,\\{\\-?\\d+\\,\\-?\\d+\\,\\-?\\d+\\})*\\]$"),
    case re:run(Data,Patt) of
        {match,_} -> true;
        nomatch -> false
    end.

strip([],NewData) -> lists:reverse(NewData);
strip([Symb|Rest],NewData) when Symb < 33-> strip(Rest,NewData);
strip([Symb|Rest],NewData) -> strip(Rest,[Symb|NewData]).

string_to_pair([],Formula,_) -> Formula;
string_to_pair(Str,Formula,[V1,V2,V3]) -> string_to_pair(Str,[{V1,V2,V3}|Formula],[]);
string_to_pair(Str,Formula,CurrExpression) ->
    case string:to_integer(Str) of
        {error,_} -> string_to_pair(string:substr(Str,2),Formula,CurrExpression);
        {Var,Rest} -> string_to_pair(Rest,Formula,[Var|CurrExpression])
    end.

log_message(Message,Args) ->
    io:format(Message,Args).

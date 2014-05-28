

% @@@ Supervisor takes care of sat_servers
% @@@ At the beginning it spawns 9 listeners and scheduler
% @@@ scheduler uses ets table to track who has clients
% @@@ and if the amout of clients greater than the amount of listeners, it spawns new listener
% @@@ it is necessary, because we should answer to every client in 1 second



-module (server_supervisor).

-behaviour (supervisor).

-export([start_link/1, listen/1,start_scheduler/2,empty_listeners/3]).
-export([init/1]).

% @@ starts supervisor
start_link(Port) -> supervisor:start_link({local,?MODULE},?MODULE,[Port]).

% @@ upper function @start_link calls this function
% @@ @init spawn scheduler process and N first listeners
init([Port]) ->
	MaxPermanentListeners=parse_int_arg(max_perml,8),
	MaxTemporaryListeners=parse_int_arg(max_templ,4),
	ForbiddenListeners=parse_int_arg(forbidl,4),
	SPid=spawn(server_supervisor,start_scheduler,[MaxPermanentListeners,MaxTemporaryListeners]),
	{ok,ServerSocket} = gen_tcp:listen(Port,[{active,once}]),
	io:format("listen socket~n"),
	% spawn_link(fun empty_listeners/1),
	spawn_link(?MODULE,empty_listeners,[MaxPermanentListeners,MaxTemporaryListeners,ForbiddenListeners]),
	{ok, {{simple_one_for_one, 60, 3600},
	[	{socket,
			{sat_server, start_link, [ServerSocket,SPid]}, % pass socket and scheduler pid to all spawned processes
				temporary, 100, worker, [sat_server]} %actually, brutal_kill may be changed to some timeout
	]}}.

parse_int_arg(Name,Default) ->
	case init:get_argument(Name) of 
		{ok,[[Val]]} -> io:format("I MADE THIS! ~p ~p~n",[Val,string:to_integer(Val)]), 
						{Ret,_} = string:to_integer(Val),
						Ret;
		_ -> Default
	end.

% @@ spawns a new listen process
listen(Id) when Id > 500 -> listen(Id-500);
listen(Id) -> io:format("Supervisor: Starting new listener with id: ~p ~n",[Id]),
			supervisor:start_child(?MODULE,[Id]).

% @@starts first N listeners
empty_listeners(PThreads,TThreads,FThreads) ->
	[listen(Id) || Id <- lists:seq(1,PThreads)],
	[listen(Id) || Id <- lists:seq(PThreads+1,PThreads+TThreads)],
	[listen(Id) || Id <- lists:seq(PThreads+TThreads,PThreads+TThreads+FThreads)],
	ok.

% starts the scheduler and checks ets-table
start_scheduler(MaxPThreads,MaxTThreads) ->
	case lists:member(th_pool,ets:all()) of 
		true -> ets:delete(th_pool);
		false  -> nothing
	end,
	ets:new(th_pool,[set,named_table,public]),
	ets:insert(th_pool,{temporary,0}),
	ets:insert(th_pool,{permanent,0}),
	scheduler(MaxPThreads,MaxTThreads).

% shceduler takes care of new listeners
% according to assignment, first N listeners can accept task
% after that, if all of them are busy and new task arrives we assign "temporary" to it

scheduler(MaxPThreads,MaxTThreads) ->
	io:format("Scheduler: starting ~n"),
	receive
		{acquire,Pid} -> 
					[{temporary,TThreads}] = ets:lookup(th_pool,temporary),
					[{permanent,PThreads}] = ets:lookup(th_pool,permanent),
					io:format("received busy;  Perm. Threads: ~p Temp threads: ~p ~n",[PThreads,TThreads]),
					Pid!evaluate(PThreads,TThreads,MaxPThreads,MaxTThreads),
					scheduler(MaxPThreads,MaxTThreads);
		{free,Type,LId} -> 
					[{temporary,TThreads}] = ets:lookup(th_pool,temporary),
					[{permanent,PThreads}] = ets:lookup(th_pool,permanent),
					case Type of
						permanent -> ets:insert(th_pool,{permanent,PThreads-1});
						temporary -> ets:insert(th_pool,{temporary,TThreads-1})
					end,
					io:format("received free from ~p with type: ~p ;  Perm. Threads: ~p Temp threads: ~p ~n",[LId,Type,PThreads,TThreads]),
					scheduler(MaxPThreads,MaxTThreads);
		{new_listener,Id} -> io:format("SCHEDULER: RECEIVED NEW LISTNERE REQUEST ~p~n",[Id]),
					listen(Id),
					scheduler(MaxPThreads,MaxTThreads)
	end.

% when there are more then N listeners, all tasks will have a "temporary" status
% if listeners more then 100, new tasks will have "forbidden" status

evaluate(PThreads,_,MaxPThreads,_) when PThreads < MaxPThreads ->
	ets:insert(th_pool,{permanent,PThreads+1}),	
	permanent;
evaluate(PThreads,TThreads,MaxPThreads,MaxTThreads) when TThreads+PThreads >= MaxPThreads+MaxTThreads  -> forbidden;
evaluate(_,TThreads,_,MaxTThreads) when TThreads < MaxTThreads ->
	ets:insert(th_pool,{temporary,TThreads+1}),	
	temporary.


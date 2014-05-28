-module (sat_supervisor).

-behaviour (supervisor).

-export([start_link/1, start_new/1]).
-export([init/1]).

start_link(Jobs) -> supervisor:start_link({local,?MODULE},?MODULE,[Jobs]).

init([Jobs]) ->
	io:format("Starting Sat supervisor~n"),
	{ok, {{one_for_one, 60, 3600},
	[	{test_function,
			{sat_server, start_test, ["hi from init function"]},
				permanent, 1000, worker, [sat_server]}
	]}}.


start_new(Message) ->
	supervisor:start_child(?MODULE,[test_function]).

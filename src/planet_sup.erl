%% @doc Basic universe behaviour.
%% This is not supposed to be accessed directly; please use the "galaxy_game" module
%% @end

-module(planet_sup).
-behaviour(supervisor).

-export([start_link/1,stop/0]).
-export([init/1]).

%% @doc Start the universe with some planets in it.
-spec start_link([galaxy_game:planet()]) -> ok.
%% @end
start_link(Args) -> 
	supervisor:start_link({local, universe}, planet_sup, Args).

%% @doc Destroy down the universe and it's planets.
-spec stop() -> any().
%% @end
stop() ->
	exit(whereis(universe),normal).

init(Planets) ->
	{ok,{{one_for_one, 0, 1},
		[child_spec(PlanetName) || PlanetName <- Planets]}}.

child_spec(PlanetName) ->
	{PlanetName, {planet_server, start_link, [PlanetName]},
	 temporary, 100, worker, [planet_server]
	}.
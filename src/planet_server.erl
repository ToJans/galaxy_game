-module(planet_server).
-behaviour (gen_server).

-export([start/1,stop/1,exists/1]).
-export([enable_shield/1,ally/1,attack/1]).
-export([init/1,handle_call/3,handle_info/2,terminate/2]).

%%===============================
%% API
%%===============================

%% @doc Start a new planet instance
-spec start(galaxy_game:planet()) -> pid().
%% @end
start(PlanetName) ->
	gen_server:start({local,PlanetName},?MODULE,[PlanetName],[]).

%% @doc Destroy a planet
-spec stop(galaxy_game:planet()) -> pid().
%% @end
stop(PlanetName) ->
	PPid = whereis(PlanetName),
	exit(PPid,kill).

%% @doc Checks whether a planet exists
-spec exists(galaxy_game:planet()) -> boolean().
%% @end
exists(PlanetName) ->
	PPid = whereis(PlanetName),
    PPid /= undefined andalso erlang:is_process_alive(PPid).


%% @doc Enable the shield on a planet
-spec enable_shield(galaxy_game:planet()) -> ok.
%% @end
enable_shield(PlanetName) ->
	enable_shield(PlanetName,exists(PlanetName)).

enable_shield(PlanetName,_Exists=true) ->
	gen_server:call(PlanetName,enable_shield);
enable_shield(_PlanetName,_Exists=false) ->
	ok.


%% @doc  Build an alliance with planets.
%% If one planet dies, the allied planet does as well.
-spec ally(galaxy_game:alliance()) -> ok.
%% @end
ally({PlanetA,PlanetB}) ->
	gen_server:call(PlanetA,{ally,PlanetB}).

%% @doc Shoot a planet.
%% It survives if the shield was enabled; otherwise it is destructed.
-spec attack(galaxy_game:attack()) -> ok.
%% @end
attack({Weapon,PlanetName}) ->
	PPid = whereis(PlanetName),
	exit(PPid,shot),
	io:format("Shot planet ~p with a ~p~n",[PlanetName,Weapon]),
	ok.

%%===============================
%% Behaviour
%%===============================
init(PlanetName) ->
	io:format("Planet ~p was added to the universe~n",[PlanetName]),
	{ok,PlanetName}.

handle_call(enable_shield,_From,PlanetName) ->
	process_flag(trap_exit, true),
	io:format("The shield was enabled for planet ~p~n",[PlanetName]),
	{reply, ok, PlanetName};
handle_call({ally,OtherPlanet},_From,PlanetName) ->
	OPid = whereis(OtherPlanet),
	link(OPid),
	io:format("Planet ~p allied with ~p ~n",[PlanetName,OtherPlanet]),
	{reply, ok, PlanetName}.

handle_info({'EXIT', _Pid, shot}, PlanetName) ->
	io:format("Planet ~p was shot; luckily the shield was enabled!~n",[PlanetName]),
    {noreply, PlanetName};
handle_info({'EXIT', _Pid, kill}, PlanetName) ->
	io:format("Planet ~p  killed~n",[PlanetName]),
    {stop, PlanetName}.	

terminate(shutdown, PlanetName) ->
	io:format("Planet ~p was destroyed~n",[PlanetName]),
    ok.

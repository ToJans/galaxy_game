-module(planet_server).

-behaviour(gen_server).

-export([start/1, stop/1, exists/1]).
-export([enable_shield/1, ally/1, attack/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

%%===============================
%% API
%%===============================

%% @doc Start a new planet instance and returns when it is running
-spec start(galaxy_game:planet()) -> pid().
%% @end
start(PlanetName) ->
    gen_server:start_link({local,PlanetName}, ?MODULE, PlanetName, []),
    PPid = whereis(PlanetName),
    unlink(PPid).

%% @doc Destroy a planet
-spec stop(galaxy_game:planet()) -> pid().
%% @end
stop(PlanetName) ->
    maybe_do(exists(PlanetName), fun ()-> 
        PPid = whereis(PlanetName),
        exit(PPid, normal) 
    end).

maybe_do(false, _Fun) ->
    ok;
maybe_do(true, Fun) ->
    Fun.

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
    gen_server:call(PlanetName,enable_shield).

%% @doc  Build an alliance with planets.
%% If one planet dies, the allied planet does as well, unless it 
%% has enabled it's shield.
-spec ally(galaxy_game:alliance()) -> ok.
%% @end
ally({PlanetA, PlanetB}) ->
    gen_server:call(PlanetA, {ally,PlanetB}).

%% @doc Shoot a planet.
%% It survives if the shield was enabled; otherwise it is destructed.
-spec attack(galaxy_game:attack()) -> ok.
%% @end
attack({Weapon,PlanetName}) ->
    PPid = whereis(PlanetName),
    exit(PPid, Weapon),
    io:format("Shot planet ~p with a ~p~n", [PlanetName, Weapon]),
    ok.


%%===============================
%% Behaviour
%%===============================
init(PlanetName) ->
    io:format("Planet ~p was added to the universe~n", [PlanetName]),
    {ok, PlanetName}.

handle_call(enable_shield, _From, PlanetName) ->
    process_flag(trap_exit, true),
    io:format("The shield was enabled for planet ~p~n", [PlanetName]),
    {reply, ok, PlanetName};
handle_call({ally, OtherPlanet}, _From, PlanetName) ->
    OPid = whereis(OtherPlanet),
    link(OPid),
    io:format("Planet ~p allied with ~p ~n", [PlanetName, OtherPlanet]),
    {reply, ok, PlanetName}.

handle_info({'EXIT', _Pid, nuclear}, PlanetName) ->
    io:format("The shield of planet ~p was not strong enough to survive a nuclear attack!~n", [PlanetName]),
    {stop, normal, PlanetName};
handle_info({'EXIT', _Pid, normal}, PlanetName) ->
    io:format("Planet ~p  exited~n", [PlanetName]),
    {stop, killed, PlanetName};
handle_info({'EXIT', _Pid, Weapon}, PlanetName) ->
    io:format("The shield of planet ~p blocked the ~p attack!~n", [PlanetName, Weapon]),
    {noreply, PlanetName}.

terminate(shutdown, [Reason,PlanetName]) ->
    io:format("Planet ~p was destroyed, reason: ~p~n", [PlanetName, Reason]),
    ok.
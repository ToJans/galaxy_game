%% @doc 
%% Basic planet behaviour.
%% This is not supposed to be used directly; please use the "galaxy_game" module.
%% @end

-module(planet_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

%%===============================
%% API
%%===============================

%% @doc Start a new planet instance.
-spec start_link(galaxy_game:planet()) -> pid().
%% @end
start_link(PlanetName) ->
    gen_server:start_link({local,PlanetName}, ?MODULE, PlanetName, []).

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
    {stop, shutdown, PlanetName};
handle_info({'EXIT', _Pid, Weapon}, PlanetName) when Weapon == laser ->
    io:format("The shield of planet ~p blocked the ~p attack!~n", [PlanetName, Weapon]),
    {noreply, PlanetName};
handle_info({'EXIT', _Pid, Reason}, PlanetName) ->
    io:format("Planet ~p destroyed, reason: ~p~n", [PlanetName,Reason]),
    {stop, normal, PlanetName}.

terminate(normal, [PlanetName]) ->
    io:format("Planet ~p was destroyed~n", [PlanetName]),
    ok.
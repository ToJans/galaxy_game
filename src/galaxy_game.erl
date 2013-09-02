%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).
-export([create_universe/1, destroy_universe/0]).
-export([enable_shield/1, ally/1, attack/1, exists/1]).


%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    create_universe(Planets),
    [enable_shield(PlanetName)||PlanetName <- Shields],
    [ally(Alliance)|| Alliance <- Alliances],
    ok.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
%% @end
-spec teardown_universe([planet()]) -> ok.
teardown_universe(_Planets) ->
    destroy_universe(),
    timer:sleep(100),
    ok.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    [attack(Action) || Action <- Actions],
    lists:filter(fun(Planet) ->
        exists(Planet)
    end, Planets).

%% @doc Destroys the universe and the planets in it.
-spec destroy_universe() -> any().
%% @end
destroy_universe() ->
    planet_sup:stop().

%% @doc Creates the universe with some planets in it.
-spec create_universe([planet()]) -> any().
%% @end
create_universe(Planets) ->
     planet_sup:start_link(Planets).

%% @doc Checks whether a planet exists in the universe.
-spec exists(planet()) -> boolean().
%% @end
exists(PlanetName) ->
    PPid = whereis(PlanetName),
    PPid /= undefined andalso erlang:is_process_alive(PPid).

%% @doc Enable the shield on a planet.
-spec enable_shield(planet()) -> ok.
%% @end
enable_shield(PlanetName) ->
    gen_server:call(PlanetName, enable_shield).

%% @doc Create an alliance between 2 planets.
%% If one planet dies, the allied planet does as well, unless it 
%% has enabled it's shield.
-spec ally(alliance()) -> ok.
%% @end
ally({PlanetA, PlanetB}) ->
    gen_server:call(PlanetA, {ally, PlanetB}).

%% @doc Shoot a planet.
%% It survives if the shield was enabled; otherwise it is destructed.
-spec attack(attack()) -> ok.
%% @end
attack({Weapon, PlanetName}) ->
    PPid = whereis(PlanetName),
    exit(PPid, Weapon),
    io:format("Shot planet ~p with a ~p~n", [PlanetName, Weapon]),
    ok.

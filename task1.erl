-module(task1).
-export([start/0, truck_spawner/1, package_creator/0, conveyor_belt/1]).


-define (TruckLimit, 30).
-define (TruckSpawnDelay, 300).

-define (PackageCreationDelay, 250).

-define (N_CONVEYORS, 3).


spawn_truck(Id) ->
    io:fwrite("Truck Spawner: Creating truck with id ~w~n", [Id]),
    timer:sleep(?TruckSpawnDelay),
    if 
        Id < ?TruckLimit -> spawn_truck(Id + 1);
        true -> io:fwrite("Truck Spawner: No more trucks~n", [])
    end.

truck_spawner(PackageCreatorId) ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner starting... Proccess: ~p~n", [TruckSpawnerId]),

    spawn_truck(1), %start loop of spawning trucks till we have no more

    PackageCreatorId ! stop.
    


create_package(Id) ->
    io:fwrite("Package creator: Creating package with id ~w~n", [Id]),
    receive
        stop ->  io:fwrite("Package creator: Received message to stop, stopping...~n", [])
        after ?PackageCreationDelay -> create_package(Id + 1)
    end.


package_creator() -> 
    io:fwrite("Package creator starting... Proccess: ~p~n", [self()]),
    create_package(1).



conveyor_belt(Id) ->
    io:fwrite("Conveyor belt ~w: starting... Proccess: ~p~n", [Id, self()]).




start() ->

    StartPid = self(),

    io:fwrite("Started main function... Proccess: ~p~n", [StartPid]),

    PackageCreatorId = spawn(?MODULE, package_creator, []),

    TruckSpawnerPid = spawn(?MODULE, truck_spawner, [PackageCreatorId]),

    ConveyorsIds = [spawn(?MODULE, conveyor_belt, [C]) || C <- lists:seq(1, ?N_CONVEYORS)].
    

-module(task1).
-export([start/0, truck_spawner/1]).


-define (TruckLimit, 30).

spawn_truck(Id) ->
    io:fwrite("Truck Spawner: Creating truck with id ~w~n", [Id]),
    timer:sleep(300),
    if 
        Id < ?TruckLimit -> spawn_truck(Id + 1);
        true -> io:fwrite("Truck Spawner: No more trucks~n", [])
    end.



truck_spawner(StartPid) ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner starting... Proccess: ~p~n", [TruckSpawnerId]),

    spawn_truck(1).
    


package_creator() -> 
    io:fwrite("Package creator starting... Proccess: ~p~n", [self()]).



conveyor_belt() ->
    io:fwrite("Conveyor belt starting... Proccess: ~p~n", [self()]).


start() ->

    StartPid = self(),

    io:fwrite("Started main function... Proccess: ~p~n", [StartPid]),

    TruckSpawnerPid = spawn(task1, truck_spawner, [StartPid]).
    

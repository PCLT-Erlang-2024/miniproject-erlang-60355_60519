-module(task1).
-export([start/0, truck_spawner/1]).

truck_spawner(StartPid) ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner starting... Proccess: ~p~n", [TruckSpawnerId]),
    StartPid ! {done, TruckSpawnerId }.
    
package_creator() ->
    io:fwrite("Package creator starting... Proccess: ~p~n", [self()]).
    
conveyor_belt() ->
    io:fwrite("Conveyor belt starting... Proccess: ~p~n", [self()]).


start() ->

    StartPid = self(),

    io:fwrite("Started main function... Proccess: ~p~n", [StartPid]),

    TruckSpawnerPid = spawn(task1, truck_spawner, [StartPid]),
    
    receive {done, TruckSpawnerPid } ->
        io:fwrite("Truck spawner was noticed as finished")
    end.
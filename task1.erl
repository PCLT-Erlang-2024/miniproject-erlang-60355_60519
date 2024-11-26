-module(task1).
-export([start/0]).

truck_spawner() ->
    io:fwrite("Truck spawner starting").
    
package_creator() ->
    io:fwrite("Package creator starting").
    
conveyor_belt() ->
    io:fwrite("Conveyor belt starting").


start() ->
    io:fwrite("Hello, world!\n").
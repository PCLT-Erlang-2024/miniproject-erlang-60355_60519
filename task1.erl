-module(task1).
-export([start/0, truck_spawner/1, package_creator/0, conveyor_belt/3]).


-define (TruckLimit, 30).
-define (TruckSpawnDelay, 300).

-define (PackageCreationDelay, 250).

-define (N_CONVEYORS, 3).


    
%current time in milliseconds
now_in_milli() ->
    {Mega, Sec, Micro} = os:timestamp(),
    TimeInMicro = (Mega * 1000000 * 1000000) + (Sec * 1000000) + Micro,
    TimeInMicro * 1000.


%broadcast the same message to a list of proccesses
broadcast_msg([], _) ->
    ok;
broadcast_msg([Pid | PidList], Msg) ->
    Pid ! Msg,
    broadcast_msg(PidList, Msg).


%generates a truck {Id, capacity, packages}
generate_truck(Id) ->
    io:fwrite("Truck Spawner: Creating truck with id ~w~n", [Id]),
    {Id, 10, []}.


%used to verify if there are or not more trucks and if the delay is correct
truck_spawner_loop_check(Trucks, NextTruckId, DelayToCreateTruck) ->
    if 
        NextTruckId =< ?TruckLimit -> %check if there are still available trucks
            
            if 
                DelayToCreateTruck =< 0 -> %in the meantime the wait time has already ended, create immediatly a new truck and continue the loop
                    truck_spawner_loop([generate_truck(NextTruckId) | Trucks], NextTruckId + 1, ?TruckSpawnDelay);

                DelayToCreateTruck > 0 -> %There is still time to wait
                    truck_spawner_loop(Trucks, NextTruckId, ?TruckSpawnDelay)
            end;

        true -> %only checked if previous condition false
            io:fwrite("Truck Spawner: No more trucks~n", [])
    end.

%when there are no trucks, we don't even receive the requests to return them
truck_spawner_loop([], NextTruckId, DelayToCreateTruck) ->

        timer:sleep(DelayToCreateTruck),
        truck_spawner_loop_check([generate_truck(NextTruckId)], NextTruckId + 1, ?TruckSpawnDelay);


%the truck spawner loop when trucks is not empty, so it can receive requests to get trucks
truck_spawner_loop(Trucks, NextTruckId, DelayToCreateTruck) ->
   
    Now = now_in_milli(), %get current time to track change in delay

    %while waiting to create new truck, be open to messages (request)
    receive

        %received request to have truck
        {receive_truck_request, RequesterPid} ->

            io:fwrite("Truck spawner: Received request to receive a truck from ~w...~n", [RequesterPid]),
            
            [TruckToGive | RestOfTrucks] = Trucks, %extract truck to give (we're sure that exist because Packages is not [])

            RequesterPid ! TruckToGive, %send the truck to the requester
            
            truck_spawner_loop_check(RestOfTrucks, NextTruckId + 1, DelayToCreateTruck -  (now_in_milli() - Now)) %continue loop without the given package and waiting the remaining time
        
        %create package after time having waited
        after DelayToCreateTruck -> 
            truck_spawner_loop_check([generate_truck(NextTruckId) | Trucks], NextTruckId + 1, ?TruckSpawnDelay)
    end.


truck_spawner(ParentPid) ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner starting... Proccess: ~p~n", [TruckSpawnerId]),

    truck_spawner_loop([], 1, ?TruckSpawnDelay), %start loop of spawning trucks till we have no more

    %There are no more trucks at this point, sending stop message to parent
    ParentPid ! stop. 



%does the package_creator_loop and checks if the delay is already over
package_creator_loop_check(Packages, NextPackageId, DelayToCreatePackage) ->

    if
        DelayToCreatePackage =< 0 -> %if the delay was already waited, then simply create a new package
            package_creator_loop([{NextPackageId}], NextPackageId + 1, ?PackageCreationDelay);

        DelayToCreatePackage > 0 ->
            package_creator_loop(Packages, NextPackageId, DelayToCreatePackage)
    
    end.

%when there are no packages, we don't even receive the requests to return them
package_creator_loop([], NextPackageId, DelayToCreatePackage) ->

    %while waiting to create new package, be open to messages (stop)
    receive
        stop ->  
            io:fwrite("Package creator: Received message to stop, stopping...~n", [])
        
        %create package after time having waited
        after DelayToCreatePackage ->
            io:fwrite("Package creator: Creating package with id ~w~n", [NextPackageId]), 
            package_creator_loop_check([{NextPackageId}], NextPackageId + 1, ?PackageCreationDelay)
    end;

package_creator_loop(Packages, NextPackageId, DelayToCreatePackage) ->
   
    Now = now_in_milli(), %get current time to track change in delay

    %while waiting to create new package, be open to messages (stop or request package)
    receive
        stop ->  
            io:fwrite("Package creator: Received message to stop, stopping...~n", []);

        %received request to have package
        {receive_package_request, RequesterPid} ->

            io:fwrite("Package creator: Received request to receive a package from ~w...~n", [RequesterPid]),
            
            %extract package to give (we're sure that exist because Packages is not [])
            [PackageToGive | RestOfPackages] = Packages,
            RequesterPid ! {PackageToGive},
            package_creator_loop_check(RestOfPackages, NextPackageId + 1, DelayToCreatePackage -  (now_in_milli() - Now)) %continue loop without the given package and waiting the remaining time
        
        %create package after time having waited
        after DelayToCreatePackage ->
            io:fwrite("Package creator: Creating package with id ~w~n", [NextPackageId]), 
            package_creator_loop_check([{NextPackageId} | Packages], NextPackageId + 1, ?PackageCreationDelay)
    end.


package_creator() -> 
    io:fwrite("Package creator: starting... Proccess: ~p~n", [self()]),
    package_creator_loop([], 1, ?PackageCreationDelay).


conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, {TruckId, TruckCapacity, Packages}) ->


    if 
        %if the truck is not filled
        TruckCapacity > 0 ->

            %Ask for package
            PackageCreatorId ! {receive_package_request, self()},

            receive
                stop -> io:fwrite("Conveyor belt ~w: Received message to stop, stopping...~n", [Id]);
            
                %in the case we have a package
                {Package} ->
                    io:fwrite("Conveyor belt ~w: Received package ~w to put in truck ~w with current capacity ~w~n", [Id, Package, TruckId, TruckCapacity]),

                    %we put the package in the truck and reduce its capacity
                    conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, {TruckId, TruckCapacity - 1, [Package | Packages]}) %we put the package in the truck        
                
            end;

        TruckCapacity == 0 -> 
            io:fwrite("Conveyor belt ~w: Sending off truck ~w with current capacity ~w and packages ~w~n", [Id, TruckId, TruckCapacity, Packages]), 
            conveyor_belt_loop(Id, PackageCreatorId, TruckSpawnerId) %if the truck is filled, continue the loop
    end.


%The begining of the conveyor belt loop, in which it gets a truck and then fils it with packages
conveyor_belt_loop(Id, PackageCreatorId, TruckSpawnerId) ->


    %ask for truck
    TruckSpawnerId ! {receive_truck_request, self()},

    receive
        stop -> io:fwrite("Conveyor belt ~w: Received message to stop, stopping...~n", [Id]);

        Truck ->
            io:fwrite("Conveyor belt ~w: Received truck ~w~n", [Id, Truck]),
            conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, Truck)
    end.
                    



    

conveyor_belt(Id, PackageCreatorPid, TruckSpawnerId) ->

    io:fwrite("Conveyor belt ~w: starting... Proccess: ~p~n", [Id, self()]),
    conveyor_belt_loop(Id, PackageCreatorPid, TruckSpawnerId).




start() ->

    StartPid = self(),

    io:fwrite("Started main function... Proccess: ~p~n", [StartPid]),


    TruckSpawnerPid = spawn(?MODULE, truck_spawner, [StartPid]),

    PackageCreatorId = spawn(?MODULE, package_creator, []),

    %starts and gets ConveyorIds
    ConveyorsIds = [spawn(?MODULE, conveyor_belt, [C, PackageCreatorId, TruckSpawnerPid]) || C <- lists:seq(1, ?N_CONVEYORS)],

    %we wait for the TruckSpawner proccess to warn that there will be no more trucks
    receive
        stop -> broadcast_msg([PackageCreatorId | ConveyorsIds], stop) %we tell the remaining proccesses to stop too

    end.    

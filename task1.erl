-module(task1).
-export([start/0, truck_spawner/2, package_creator/0, conveyor_belt/2]).


-define (TruckLimit, 30).
-define (TruckSpawnDelay, 300).

-define (PackageCreationDelay, 250).

-define (N_CONVEYORS, 3).



broadcast_msg([], _) ->
    ok;
broadcast_msg([Pid | PidList], Msg) ->
    Pid ! Msg,
    broadcast_msg(PidList, Msg).
    

now_in_milli() ->
    {Mega, Sec, Micro} = os:timestamp(),
    TimeInMicro = (Mega * 1000000 * 1000000) + (Sec * 1000000) + Micro,
    TimeInMicro * 1000.


%Spawn a single truck
spawn_truck(Id) ->
    io:fwrite("Truck Spawner: Creating truck with id ~w~n", [Id]),
    timer:sleep(?TruckSpawnDelay),
    if 
        Id < ?TruckLimit -> 
            spawn_truck(Id + 1);
        
        %Id has reached the limit, no more trucks
        true -> 
            io:fwrite("Truck Spawner: No more trucks~n", [])

    end.


truck_spawner(PackageCreatorId, ConveyorPids) ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner starting... Proccess: ~p~n", [TruckSpawnerId]),

    spawn_truck(1), %start loop of spawning trucks till we have no more

    %There are no more trucks at this point, sending stop messages

    broadcast_msg([PackageCreatorId, ConveyorPids], stop).
    



%when there are no packages, we don't even receive the requests to return them
package_creator_loop([], NextPackageId, DelayToCreatePackage) ->

    io:fwrite("Package creator: There are currently no packages~n", []),

    if
        DelayToCreatePackage =< 0 -> 
            package_creator_loop([{NextPackageId}], NextPackageId + 1, ?PackageCreationDelay);

        true -> %if there is time to wait

            %while waiting to create new package, be open to messages (stop)
            receive
                stop ->  
                    io:fwrite("Package creator: Received message to stop, stopping...~n", [])
                
                %create package after time having waited
                after DelayToCreatePackage ->
                    io:fwrite("Package creator: Creating package with id ~w~n", [NextPackageId]), 
                    package_creator_loop([{NextPackageId}], NextPackageId + 1, ?PackageCreationDelay)
            end
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
            TimeWaited = now_in_milli() - Now,
            io:fwrite("Package creator: Time waited ~w~n", [TimeWaited]), 
            package_creator_loop(RestOfPackages, NextPackageId + 1, DelayToCreatePackage -  TimeWaited) %continue loop without the given package and waiting the remaining time
        
        %create package after time having waited
        after DelayToCreatePackage ->
            io:fwrite("Package creator: Creating package with id ~w~n", [NextPackageId]), 
            package_creator_loop([{NextPackageId} | Packages], NextPackageId + 1, ?PackageCreationDelay)
    end.


package_creator() -> 
    io:fwrite("Package creator: starting... Proccess: ~p~n", [self()]),
    package_creator_loop([], 1, ?PackageCreationDelay).


conveyor_belt_loop(Id, PackageCreatorId) ->

    %Ask for package
    PackageCreatorId ! {receive_package_request, self()},

    receive
        stop ->
            io:fwrite("Conveyor belt ~w: Received message to stop, stopping...~n", [Id]);

        {PackageToGive} ->
            io:fwrite("Conveyor belt ~w: Received package ~w~n", [Id, PackageToGive]),
            conveyor_belt_loop(Id, PackageCreatorId)

    end.


    

conveyor_belt(Id, PackageCreatorPid) ->

    io:fwrite("Conveyor belt ~w: starting... Proccess: ~p~n", [Id, self()]),
    conveyor_belt_loop(Id, PackageCreatorPid).




start() ->

    StartPid = self(),

    io:fwrite("Started main function... Proccess: ~p~n", [StartPid]),

    PackageCreatorId = spawn(?MODULE, package_creator, []),

    %starts and gets ConveyorIds
    ConveyorsIds = [spawn(?MODULE, conveyor_belt, [C, PackageCreatorId]) || C <- lists:seq(1, ?N_CONVEYORS)],
    
    TruckSpawnerPid = spawn(?MODULE, truck_spawner, [PackageCreatorId, ConveyorsIds]).

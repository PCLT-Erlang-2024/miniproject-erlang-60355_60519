-module(task3).
-export([start/0, truck_spawner/0, package_creator/0, conveyor_belt/3]).


-define (TruckSpawnDelay, 300).

-define (PackageCreationDelay, 250).

-define (N_CONVEYORS, 3).

%Time in milliseconds to run the program
-define (TIME_TO_RUN, 20000).

-define (TRUCK_CAPACITY, 10).

-define (MAX_PACKAGE_SIZE, 3).

%Time in milliseconds to wait for a truck replacement range
-define (MIN_TIME_TO_WAIT_FOR_TRUCK, 100). %This is exclusive
-define (MAX_TIME_TO_WAIT_FOR_TRUCK, 500). %This is inclusive

    
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


%used to verify if the delay is correct (and maybe other conditions in the future)
generator_loop_check(GeneratorFunction, GeneratorName, GeneratedEntities, NextId, DelayToGenerate, InitialDelay) ->
            
    if 
        DelayToGenerate =< 0 -> %if in the meantime the wait time has already ended, create immediatly a new entity and continue the loop
            generator_loop(GeneratorFunction, GeneratorName, [GeneratorFunction(NextId) | GeneratedEntities], NextId + 1, InitialDelay, InitialDelay);

        DelayToGenerate > 0 -> %There is still time to wait
            generator_loop(GeneratorFunction, GeneratorName, GeneratedEntities, NextId, DelayToGenerate, InitialDelay)
    end.


%when there are no entities, we don't even receive the requests to return them
generator_loop(GeneratorFunction, GeneratorName, [], NextId, DelayToGenerate, InitialDelay) ->

    %while waiting to create new entity, be open to messages (stop)
    receive
        stop ->  
            io:fwrite("~s: Received message to stop, stopping...~n", [GeneratorName])
        
        %create entity after time having waited
        after DelayToGenerate ->
            generator_loop_check(GeneratorFunction, GeneratorName, [GeneratorFunction(NextId)], NextId + 1, InitialDelay, InitialDelay)
    end;


%in this body, the entity spawner loop when Entities is not empty, so it can receive requests to get them
generator_loop(GeneratorFunction, GeneratorName, Entities, NextId, DelayToGenerate, InitialDelay) ->
   
    Now = now_in_milli(), %get current time to track change in delay

    %while waiting to create new entity, be open to messages (request and stop)
    receive

        stop -> io:fwrite("~s: Received message to stop, stopping...~n", [GeneratorName]);

        %received request to receive one of the entities of this generator
        {request_for_entity, RequesterPid} ->

            io:fwrite("~s: Received request from ~w...~n", [GeneratorName, RequesterPid]),
            
            [EntityToGive | RestOfEntities] = Entities, %extract entity to give (we're sure that exist because Entities is not [])

            RequesterPid ! EntityToGive, %send the entity to the requester
            
            generator_loop_check(GeneratorFunction, GeneratorName, RestOfEntities, NextId, DelayToGenerate -  (now_in_milli() - Now), InitialDelay) %continue loop without the given entity and waiting the remaining time
        
        %create entity after time having waited
        after DelayToGenerate -> 
            generator_loop_check(GeneratorFunction, GeneratorName, [GeneratorFunction(NextId) | Entities], NextId + 1, InitialDelay, InitialDelay)
    end.


%generates a truck {Id, capacity, packages}
generate_truck(Id) ->
    io:fwrite("Truck Spawner: Creating truck with id ~w~n", [Id]),
    {Id, ?TRUCK_CAPACITY, []}.


truck_spawner() ->

    TruckSpawnerId = self(),

    io:fwrite("Truck spawner: starting... Proccess: ~p~n", [TruckSpawnerId]),

    generator_loop(fun generate_truck/1, "Truck spawner", [], 1, ?TruckSpawnDelay, ?TruckSpawnDelay).



generate_package(Id) ->
    Size = rand:uniform(?MAX_PACKAGE_SIZE),
    io:fwrite("Package creator: Creating package with id ~w and size ~w~n", [Id, Size]),
    {Id, Size}.


package_creator() -> 
    io:fwrite("Package creator: starting... Proccess: ~p~n", [self()]),
    generator_loop(fun generate_package/1, "Package creator", [], 1, ?PackageCreationDelay, ?PackageCreationDelay).


conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, {TruckId, TruckCapacity, Packages}) ->


    if 
        %if the truck is not filled
        TruckCapacity > 0 ->

            %Ask for package
            PackageCreatorId ! {request_for_entity, self()},

            receive
                stop -> io:fwrite("Conveyor belt ~w: Received message to stop, stopping...~n", [Id]);
            
                %in the case we have a package
                Package ->
                    {PackageId, PackageSize} = Package,
                    io:fwrite("Conveyor belt ~w: Received package ~w with size ~w to put in truck ~w with current capacity ~w~n", [Id, PackageId, PackageSize, TruckId, TruckCapacity]),

                    if 
                        %if there is space in the truck
                        PackageSize =< TruckCapacity ->

                            %we put the package in the truck and reduce its capacity
                            conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, {TruckId, TruckCapacity - PackageSize, [Package | Packages]}); %we put the package in the truck

                        %if there is no space in the truck
                        PackageSize > TruckCapacity ->
                                TimeToWait = ?MIN_TIME_TO_WAIT_FOR_TRUCK + rand:uniform(?MAX_TIME_TO_WAIT_FOR_TRUCK - ?MIN_TIME_TO_WAIT_FOR_TRUCK),
                                io:fwrite("Conveyor belt ~w: Sending off truck ~w with current capacity ~w and packages ~w, leaving leftover package ~w. Will wait for ~w milli before asking for other truck~n", [Id, TruckId, TruckCapacity, Packages, PackageId, TimeToWait]),
                                timer:sleep(TimeToWait),
                                conveyor_belt_loop(Id, PackageCreatorId, TruckSpawnerId, Package) %if the truck is filled, continue the loop

                    end
                
            end;

        TruckCapacity == 0 -> 
            TimeToWait = ?MIN_TIME_TO_WAIT_FOR_TRUCK + rand:uniform(?MAX_TIME_TO_WAIT_FOR_TRUCK - ?MIN_TIME_TO_WAIT_FOR_TRUCK),
            io:fwrite("Conveyor belt ~w: Sending off truck ~w with current capacity ~w and packages ~w. Will wait for ~w before getting other truck~n", [Id, TruckId, TruckCapacity, Packages, TimeToWait]), 
            timer:sleep(TimeToWait),
            conveyor_belt_loop(Id, PackageCreatorId, TruckSpawnerId, none) %if the truck is filled, continue the loop
    end.


%The begining of the conveyor belt loop, in which it gets a truck and then fils it with packages
conveyor_belt_loop(Id, PackageCreatorId, TruckSpawnerId, PreviousPackage) ->


    %ask for truck
    TruckSpawnerId ! {request_for_entity, self()},

    receive
        stop -> io:fwrite("Conveyor belt ~w: Received message to stop, stopping...~n", [Id]);

        Truck ->
            io:fwrite("Conveyor belt ~w: Received truck ~w~n", [Id, Truck]),
            
            case PreviousPackage of
                none -> %if there was no leftover package
                    conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, Truck);

                Package -> %in the case there was a leftover package
                    {TruckId, TruckCapacity, _} = Truck,
                    {PackageId, PackageSize} = Package,
                    io:fwrite("Conveyor belt ~w: Leftover package ~w with size ~w will be put in truck ~w~n", [Id, PackageId, PackageSize, TruckId]),
                    conveyor_belt_with_truck_loop(Id, PackageCreatorId, TruckSpawnerId, {TruckId, TruckCapacity - PackageSize, [Package]}) %we put the package in the truck

            end
    end.
                    

    

conveyor_belt(Id, PackageCreatorPid, TruckSpawnerId) ->

    io:fwrite("Conveyor belt ~w: starting... Proccess: ~p~n", [Id, self()]),
    conveyor_belt_loop(Id, PackageCreatorPid, TruckSpawnerId, none).




start() ->

    io:fwrite("Started main function... Proccess: ~p~n", [self()]),

    TruckSpawnerPid = spawn(?MODULE, truck_spawner, []),

    PackageCreatorId = spawn(?MODULE, package_creator, []),

    %starts and gets ConveyorIds
    ConveyorsIds = [spawn(?MODULE, conveyor_belt, [C, PackageCreatorId, TruckSpawnerPid]) || C <- lists:seq(1, ?N_CONVEYORS)],

    timer:sleep(?TIME_TO_RUN), %we wait for a certain time before naturally ending the process

    io:fwrite("Main Function: Time to run has passed (~w milliseconds), ending proccess...~n", [?TIME_TO_RUN]),

    broadcast_msg([TruckSpawnerPid | [ PackageCreatorId | ConveyorsIds ]], stop). %we tell the remaining proccesses to stop too

# **Mini Project Task 1** #
This task implements the creation of packages and their process from the creation to the conveyor belt and to the trucks.
The application will run until the time reaches the one specific in the TIME_TO_RUN variable.


## **Implementation** ##
- The program starts by generating the trucks and packages each with an identifiable id, and will keep spawning them until it reaches the maximum capacity and with a certain wait time between each of them.

- After this a certain number of belts will be created and handle the operations between the packages, belts and trucks.

- The main process will then sleep for the amount of time we defined that our program will run in order for the operations keep working and when it wakes up sends a signal to the threads so the operations stop and the program ends.

## **Main Functions** ##

### **Truck Spawner** ###
- Handles the truck creation.
- Creates a truck after each interval defined in TruckSpawnDelay.
- Each truck is defined by a tuple containing the truck id, capacity and a list of packages.
- When the maximum number of trucks is reaches the function will stop producing new trucks and only handle the processes for the respective trucks.
- When a request is received the spawner sends a truck to the process.
- The spawner will keep doing this operations until it receives the stop signal from the main

### **Package Creator** ###
- Works in a similar way as the previous function for the trucks but instead of receiving packages from the belt it sends them to it.
- Each package only stores it's respective id in a tuple.


### **Conveyor Belts** ###
- Each belt initializes and starts starts another function for the belts.
- Then the belt requests a truck from the spawner.
- Once the belt receives a truck starts loading the packages.
- In the loading loop the belt will request packages to the package creator and process them to the truck.
- Once the truck is full dispatches it and requests a new truck.
- Like the previous functions, it stops working when receives a signal from the main.



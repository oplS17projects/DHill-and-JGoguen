# The Racket Gardener

### Statement
  
  The project we are doing involves an arduino, sensors, plants, plots, and databases. Our program will control an arduino microcontroller with a few sensors for temperature, light, soil moisture, and humidity. These sensors will keep track of not only if you need to water the plant, but also provide an in depth analysis on trends in water consumption of a particular plant type and age. Relation between the consumption of water and environmental variables will provide a better determination for how much water you can expect your plant to consume. Database entries can be made, containing information on the watering needs for certain types of plants in certain conditions.
  
### Analysis

- Data abstraction: Simplified representation of program into separate functional pieces: GUI, database, plotting, and sensor files.
  
- Recursion: Reading sensors and writing/analyzing this information will be done in infinite loops.
  
- Map: Currently using map for a 3d plot implementation, vectorizing all plot points.

- Object orientation: For certain arduino control features involving objects to represent external sensors and their associated pins/ pin functionality. Also for database interactivity, will need objects to represent the strings of plant/sensor information pulled from or put into database (using sqlite).


### External Technologies

We will be using an arduino microcontroller with sensors to obtain data and store in a database for analysis.


### Deliverable and Demonstration

 The live demo will likely be much less interactive and much more going through a demonstration/ example of how one may set up and begin the plant monitoring system. 

 The software/ hardware produced will be instantly applicative in anyones back yard. (If we also get to include a solenoid valve and hose, could extend it to irrigation system)


### Evaluation of Results
If we create sensors that accurately read information, store and analyze that data effectively, and output appropriate alerts and results, we have created a succesful project.

## Architecture Diagram

![architecture diagram](/arch2.2.png?raw=true "architecture diagram")

Raw data is obtained from arduino sensors and stored in an sqlite database for ease of access. From the database, tables of formatted information are created and lists of values are passed to plotting and data analysis functions. All of this will be controlled through a user controlled GUI.

## Schedule

### First Milestone (Sun Apr 9)
Initial implementation of arduino sensors and functionality. Outline structure of data analysis.

### Second Milestone (Sun Apr 16)
Create databases of information from sensors. Create plots from information.

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
Implement user alert/GUI functionality. 

## Group Responsibilities

### James Goguen @jamesin67895
Will work on the ardunio sensors and databasing the information.

### Devon Hills @devonjhills
Will work on data analysis with plots and data processing.

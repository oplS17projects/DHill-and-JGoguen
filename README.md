# The Racket Gardener

### Statement
  
  The project we are doing involves an arduino, sensors, plants, plots, and databases. Our program controls an arduino microcontroller with a few sensors for temperature, light, soil moisture, and humidity. These sensors keep track of not only if you need to water the plant, but also provide an in depth analysis on trends in water consumption of a particular plant type and age. Relation between the consumption of water and environmental variables provides a better determination for how much water you can expect your plant to consume. Database entries are made, containing information on the watering needs for certain types of plants in certain conditions. And a user controlled GUI is used to interface with the data, and view plots and analysis of the sensor readings.
  
### Analysis

- Data abstraction: Simplified representation of program into separate functional pieces: GUI, database, plotting, and sensor files.
  
- Recursion: Reading sensors and writing/analyzing this information will be done in infinite loops.
  
- Map: Currently using map for a 3d plot implementation, vectorizing all plot points.

- Object orientation: For certain arduino control features involving objects to represent external sensors and their associated pins/ pin functionality. Also for database interactivity, objects represent the strings of plant/sensor information pulled from or put into the database (using sqlite).


### External Technologies

We will be using an arduino microcontroller with attached sensors to obtain data and store in a database for analysis.


### Deliverable and Demonstration

 The live demo is much less interactive and much more going through a demonstration/example of how one may set up and begin the plant monitoring system. We first show how the sensors read and store information with a live example of the ardunio sensor readings. Then we run a script to show a full sample run, including full databases of information, and relevant plots and analysis.

 The software/ hardware produced will be instantly applicative in anyones back yard.


### Evaluation of Results
We successfully interfaced arduino sensors with racket, which accurately read information. We stored and analyzed that data effectively into databases, created plots from the data and analyzed the results. And then created a GUI for user interaction and displaying the data.

## Architecture Diagram

![architecture diagram](/arch2.2.png?raw=true "architecture diagram")

Raw data is obtained from arduino sensors and stored in an sqlite database for ease of access. From the database, tables of formatted information are created and lists of values are passed to plotting and data analysis functions. All of this will be controlled through a user controlled GUI.

## Schedule

### First Milestone (Sun Apr 9)
Initial implementation of arduino sensors and functionality. Outlined structure of data analysis.

### Second Milestone (Sun Apr 16)
Created databases of information from sensors. Created plots from information.

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28)
Implemented GUI functionality. 

## Group Responsibilities

### James Goguen @jamesin67895
Worked on interfacing the ardunio sensors with racket, databasing the information, and creating a GUI.

### Devon Hills @devonjhills
Worked on data analysis with plots, data processing, and plot-GUI interface.

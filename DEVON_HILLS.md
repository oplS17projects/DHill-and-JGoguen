# The Racket Gardener

## Devon Hills
### April 30, 2017

# Overview
This program controls an arduino microcontroller with a few sensors for temperature, light, and soil moisture. Database entries are made, containing information on the watering needs for certain types of plants in certain temperature and lighting conditions. And a user controlled GUI is used to interface with the data, and view plots and analysis of the sensor readings.

**Authorship note:** All of the code described here was written by myself, unless otherwise noted.

# Libraries Used
The code uses four libraries:

```
(require db)
(require "firmata.rkt")
(require racket/gui)
(require plot)
```

* The ```db``` library provides interfacing with the SQLite database system to store information gained from sensor readings.
* The```firmata.rkt``` code is external and used to make the arduino microcontroller compatible with racket.
* The ```plot``` library is used to visually represent the information.
* The ```racket/gui``` library is used to create a user interface to interact with the program.

# Key Code Excerpts

## 1. Data Abstraction

code description 

```
insert code here
```

code description 
 
 
## 2. Higher-Order Procedures

code description 

```
insert code here
```

code description 


## 3. Recursion

code description 

```
insert code here
```

code description 

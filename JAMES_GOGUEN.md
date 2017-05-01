## James Goguen
### May 1, 2017

# Overview

The code we wrote takes advantage of an arduino microcontroller and a temperature, soil-moisture, ligh-intensity sensor. The goal
was to enable a user to keep track of the watering needs of a plant. A loop is run infinitely, where the readings for temperature 
and light are taken every ten seconds and sumanted. The soil-moisture is  also taken every ten seconds, and is compared against a 
user designated "min-moisture-threshold". If the moisture drops below this threshold, the program updates a table in a database
for the given cycles average light-intensity and temperature, with the cycles total time in seconds. These entries are made 
into one of 11 tables [table0 - table10]. Each table has 11 rows, one for each temperature range [0-10], where temp_range
0 = 0 - 5 degrees F, 1 = 5-15 degrees F... etc... This allows for plots to be made that show the relationship for any
given light-intensity, of the cycle_time to average_temperature. 


**Authorship note:** All of the code described here was written by myself.

# Libraries Used
The code uses four libraries: 

(require db)
(require sql)
(require sqlite-table)
(require racket/gui/base)
(require plot)
(require plot/utils)

* The ```db``` library is used to open up a connection to a microcontroller (Arduino) via the serial port it is connected with. 
* The associated ```sql```library has specific function to create insert, delete, and update statements for
manipulating actual SQLite tables. 
* The ```sqlite-table``` library adds easier functionality for dealing with sql tables.
* The ```racket/gui/base``` library provides means for a classis GUI creation and management.
* The ```plot``` and ```plot/utils``` library allow users to create drawings of plots for functions or lists through abstract
procedures.


# Global Definitions 

 The use of global defines is essential to our program. The code sumnates temperature readings and light readings, 
 along with the number of readings, in order produces averages for a given water cycle. While maintaining  
 
 Some of the globals used are...


```
(define min-moisture-threshold 100)
(define sum_temp_readings_ 0)
(define previous_timestamp_ (current-seconds))
(define temp_reading_count_ 0)
(define cycle_tics_ 0) ;; just so can get an idea of average tics -> milliseconds
(define cycle_light_ 0)
```

The ```min-moisture-threshold``` is used to determine whether a plants water cycle is complete or not. 

The ```sum_temp_readings_``` and ```cycle_light_``` are sumnations of all temperature (in degrees F) and light-intensity readings in a given
cycle. They are used with ```temp_reading_count``` to determins the averages for either at the end of a cycle. 

A ```previous_timestamp_``` is the (current-seconds) taken at the beginning of each cycle, and is means for determining the cycle_time in seconds.

Together, these globals make up the backbone of the program. 
A few other global defines....
```
(define cycle_temperatures '())
(define cycle_lights '())
```
 ... are lists of the same readings used to make the sum of temperaturea and light readings. 

Also,  ```(define cycle_moistures '())``` keeps track of the moisture-readings taken.

All three of these lists are not for practical use, but provide the GUI a way to to show the user the cycles progress.


#2 Sensor Loop Recursion

To fulfill the programs main goal, our program uses an infinitely recursive loop...

```
(define (sensor-loop)
  (cond 
    ( (and ( > min-moisture-threshold (curr-soil-moisture)) (not (= cycle_tics_ 0))) (complete-water-cycle)
                                                        (rm-curr-plots)
                                                        (send-avg-vals sum_temp_readings_ temp_reading_count_ cycle_light_ previous_timestamp_)
                                                        (cycle-complete)
                                                        (reset-cycle-vals) (reset-cycle-lists) (sleep 10) (sensor-loop) )
    ( else (update-globals)
           (rm-curr-plots)
           (update-curr-plots cycle_temperatures "temperature" 100)
           (update-curr-plots cycle_moistures "moisture" 1000)
           (update-curr-plots cycle_lights "light" 1000)
           (sleep 10) (sensor-loop) )
    ))
 ```
 
 This is the heart of the program, that which pumps the valued blood and breathes the real world sensored oxygen. There are 
 only two possible paths this function can take, dependent on whether the ```(current-soil-moisture)``` has dropped below the 
 minimum threshold or not. If the current moisture is below the minumum threshold, then the program completes the current 
 cycle by updating the database and reseting appropriate global values and lists. The average values of the cycle are printed 
 on a newly opened gui window, and the program is "paused". If the current moisture is not below the threshold, then sensor 
 readings are taken and globals are updated. Both paths ultiamtely terminate in the loop calling itself again. 

 
 What actually is going on, allowing the program to "pause" without freezing the gui, is the ```(sensor-loop)``` is being run 
 by a thread of the program. This code is found in our main source code ....
 
 ```
(define sensor-thread
  (thread (lambda ()
           (let loop ()
                    (sensor-loop)
    (loop)))))
```

I unnecesarily made this a "looping" thread, but it never actually loops. Regardless, when a cycle is completed, the program
puts this loop to sleep, and await the user to click a "Next Cycle" gui button to resume the thread. 


#3 Database dependency on Procedural Abstraction

The database interactivity is dependent on executing SQLite statements in the form of strings. The ```sql``` library provides
a fairly simple way to create these staments by way of function calls. However, it does not provide much abstraction.

I provided myself with some (sloppy) abstraction as follows,

```
(define (init-start i) (string-append "INSERT INTO " table_name (number->string i)))
(define init-mid " (avg_temp_range, avg_time, time_count) VALUES (" )
(define init-end ", 0, 0)")


(define (update-start i) (string-append "UPDATE " table_name (number->string i) " SET avg_time = "))
(define update-mid ", time_count = ")
(define update-where " WHERE (avg_temp_range = ")

(define select-start "SELECT ")
(define (select-temp-range-end i) (string-append " FROM " table_name (number->string i) " WHERE (avg_temp_range = "))
(define (select-all-range-end i) (string-append " FROM " table_name (number->string i)))
```

Each set of defines is simply appened in the order -start -mid -end, and passed to an ```sql``` function (```query-exec```),
which performs the statement the database. I unfrtunately never provided a final abstracton for these functions. They are used
in many database functions however.

Two of these functions that are also widely called upon are...

```
;;returns time_avg for database column where avg_temp_range = range
(define (get-range-time-avg lite_i range)
  (query-value sqlc (string-append select-start "avg_time" (select-temp-range-end (inexact->exact lite_i)) (number->string (inexact->exact range)) ")")))

;;returns number of times averaged so far for database column where avg_temp_range = range
(define (get-range-time-count lite_i range)
 (query-value sqlc (string-append select-start "time_count" (select-temp-range-end (inexact->exact lite_i)) (number->string (inexact->exact range)) ")")))
```

The two functions are used to get the avg_time  (in seconds) or time_count for a specified light-intensity and temperture-
range. They are also widely-used by other database functions. Without proedural abstraction in our code, the amount of 
repetition would be shameful.


#4 Let, Set, and Lists: Personal Pair "mapping"

Although a littel redundant with procedural abstraction, the program particularly needing a certain
degree of correctness in correlating average temperatuer and light-intensity vlaues to their associated database
tables and row indexes. It is a simple enough function but is a solid little hunk of code to be rewritten if necessary.

Here is the code which takes a sum of temperture and light-intensities, along with the number of readings, and
returns a list of two values, the temp_range (table row index) and light_range (table index).

```
(define (temp-and-light-ranges sum_temp_ sum_light_ reading_count_)
  (let ((temp_range sum_temp_)
        (light_range sum_light_))
    (if (= temp_range 0)
        0
        (set! temp_range (round (/ (/ temp_range reading_count_) 10)))
        )
    (if (= light_range 0)
        0
        (set! light_range (round (/ (/ light_range reading_count_) 100)))
        )
    (list temp_range light_range)
    )
  )
  ``` 
  
  It really is quite simple, but a few functions need this code, and bundling up the values in a list created by
  another function reduced 11 lines of code to one per needed time.
  
  Two quick examples are...
  
  ```
  (define (curr-exp-time sum_temp_ sum_light_ reading_count_ prev_ts_)
  (let ((ranges (temp-and-light-ranges sum_temp_ sum_light_ reading_count_)))
    (- (get-range-time-avg (cadr ranges) (car ranges)) (- (current-seconds) prev_ts_))))
 ```
 
 and
 
 ```
 (define (complete-water-cycle)
  (let ((ranges (temp-and-light-ranges sum_temp_readings_ cycle_light_ temp_reading_count_)))
    (begin
      (display "\n Updating database with values \n sum_temp_readings: ") (display sum_temp_readings_)
      (display "temp_reading_count: ") (display temp_reading_count_) (display "::::::::::::\n\n")
      (update-current-db (car ranges) (- (current-seconds) previous_timestamp_) (cadr ranges) )
      )
    )
  )
 ```
 
 It could be deemed  prideful to call importance to this or relate it to "mapping", but the amount it
 cleaned up my code was definately not worthy.

 
 

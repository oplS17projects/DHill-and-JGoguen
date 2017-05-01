# The Racket Gardener


## Devon Hills
### April 30, 2017


# Overview
This program controls an arduino microcontroller with a few sensors for temperature, light, and soil moisture. Database entries are made, containing information on the watering needs for certain types of plants in certain temperature and lighting conditions. And a user controlled GUI is used to interface with the data, and view plots and analysis of the sensor readings.

**Authorship note:** All of the code described here was written by myself, unless otherwise noted.


# Libraries Used
The code uses four libraries:

```racket
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

Separation of interface and implementation:

The user is presented with GUI functionality for displaying a run of the gardener program, which reads in, stores, and displays sensor data. Under the hood, as each cycle is updated, the ``` (plot-cycle) ``` function is called to display various data based on the parameter values passed in (i.e for temperature, light, or moisture plots). James and I worked on the GUI together.

The GUI for updating and presenting each cycle of information:
```racket
(define (update-curr-plots cycle_vals y-lab y-max)
  (let ((x-offset
         (cond
           ((equal? "temperature" y-lab) 0)
           ((equal? "moisture" y-lab) 200)
           (else 400))))
    
    (send plot-curr-pb insert (plot-cycle-vals cycle_vals y-lab y-max) x-offset 10)
    )
  )
  
 ...
 
 (define plot-avg-c (new editor-canvas%
               [parent plot-frame]
               [min-height 200] ))

(define plot-avg-pb (new pasteboard%))

(define (show-avg-plot cyclelist xlabel)
  (send plot-avg-pb insert (plot-cycle cyclelist xlabel (+ (length cyclelist) 50)) 10 10)
  (send plot-avg-c show #t)
  )
  
 ...
 
```

As shown above, ``` (plot-cycle) ``` is called, which takes in a given list, data type (xlabel), and graph height based on the amount (length) of data in the list. I created flexible parameters here in order to use a single function to display all three graph types. The beginning of the function is shown here, using a let statement to make each graph type distinct by color:

```racket
(define (plot-cycle cyclelist xlabel ymax)
  (let ((col
         (cond
           ((equal? "light" xlabel) "yellow")
           ((equal? "temperature" xlabel) "orange")
           ((equal? "moisture" xlabel) "blue")
           (else "red"))))
           
      ...
```

 
## 2. Higher-Order Procedures

When creating plot points in my graphing function, I found that when feeding in raw fractional numbers, the plots sometimes gave innacurate and skewed points and lines. To that end I first made a simple function using a map to convert the values to decimal. After doing so, the results were displayed accurately

```racket
(define (decimal_list lst)
  (map exact->inexact
       lst))
```

I then went back over my implementation of creating plot points based on the given input list. I had originally used a for/list, but realized it would be much cleaner to first create a local list using let, which defines the range of values to be presented from 0 - (length of input list).

```racket
(let ((range_list
           (build-list (length cycle_vals) values)))
```

I then use both apply and map together in order to take the input list and the local range_list, and output a neat list of (x y) coordinates, which is exactly what the contract of the plot function is expecting.

```racket

;plot data
(plot-snip (list (axes) ;snip

            ;line
            (lines (apply map list (list range_list cyclelist))

                   #:color col
                   #:width 4)

            ;points
            (points (apply map list (list range_list cyclelist))

                    #:color col
                    #:line-width 4
                    #:sym 'odot))
```

So for example, given a cyclelist of:
```racket
'(102.8 91.4 80.0 70.0 53.0 41.6 40.0 30.0 20.0 10.0 -1.4)
```
this would create a range_list of:
```racket
'(0 1 2 3 4 5 6 7 8 9 10)
```
and running it through the apply map would produce the list of (x y) coordinates:
```racket
> (apply map list (list range_list cyclelist))
'((0 102.8) (1 91.4) (2 80.0) (3 70.0) (4 53.0) (5 41.6) (6 40.0) (7 30.0) (8 20.0) (9 10.0) (10 -1.4))
```

This is also my favorite piece of code in my function. Although very simple, it employs a clever use of higher order procedures in order to create the necessary list of values to plot the data. 


I also created a 3D graph to show the interaction between light, temperature, and cycle time all at once, although we did not end up using it in the final implementation. The 3D plot takes advantage of map as well, in order to vectorize three lists into (x, y, z) coordinates.
```racket
(plot3d-snip (list
         ;points
         (points3d
          (map vector cyclelist cyclelist2 range_list))

         ;lines
         (lines3d
          (map vector cyclelist cyclelist2 range_list)))
```


## 3. Recursion

The core of the program uses a recursive call to the arduino sensors in an infinite loop (done by James). However, recursion is also used for filling in lists. Raw data is taken from the database, averaged together to remove null elements from the raw data, and recursively output into a new list.

```racket
(define (fill-front avg times_)
  
  (cond
    ;base case
    [(or (null? times_) (= 1 (length times_)))
     '()]
    
    [(and (= 0 (list-ref times_ 0)) (not (= 0 (list-ref times_ 1))))
     (cons (+ avg (list-ref times_ 1)) (cdr times_))]
    
    [(and (= 0 (list-ref times_ 0)) (= 0 (list-ref times_ 1)))
     (fill-front avg (cons 0 (fill-front avg (cdr times_))))]
    
    [else
     times_]
    
    )
  )
  
  ...
  
  (define (fill-down avg times_)
  (cond
    ;base case
    [(or (null? times_) (= 1 (length times_)))
     '()]
    
    [( = 0 (list-ref times_ 1))
     (cons (car times_) (cons (- (list-ref times_ 0) avg) (fill-down avg  (cddr times_))))]
    
    [else
     (cons (car times_) (fill-down avg (cdr times_)))]
    
    )
  )
```

These functions are called from within James' database code as follows:

```racket
;; Constructs list from light range [0 -> 10] for temperature_range (temp_range) of AVG_TIME's for each light level 
(define (table-light-times->list temp_range)
  (let ((partial-list (light-times->partial-list temp_range))
        )
    (let ((avg (average-diff partial-list))
          )
      (fill-down avg (fill-front avg partial-list))
      ))
  ) 
```

#lang racket

(require sql db)
(require sqlite-table)
(require racket/async-channel)
(require racket/gui/base)
(require "firmata.rkt")
(require "newgui.rkt")
(require "newdb.rkt")

;; opens up connection to arduino at the given input_port
(open-firmata "/dev/cu.usbmodem1421")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INITIALIZE NEW DB TABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(send frame set-label (string-append "TABLE: " table_name))  

;(send frame show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define OUTPUT 0)
(define INPUT 1)

(define min-moisture-threshold 100)
(define sum_temp_readings_ 0)
(define previous_timestamp_ (current-seconds))
;;(define current_timestamp_ (current-seconds))
(define temp_reading_count_ 0)
(define cycle_tics_ 0) ;; just so can get an idea of average tics -> milliseconds
(define cycle_light_ 0)

(define cycle_temperatures '())
(define cycle_moistures '())
(define cycle_lights '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP THE PINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-analog-pin! 0 1) ;; pin 0 = temp
(report-analog-pin! 1 1) ;; pin 1 = soil_moisture
(report-analog-pin! 2 1) ;; pin 2 = light

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATABASE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;indefinate sleep loop -
;;in
;; resets all columns (avg_tmp_range = [ 0 -> 10 ] ) to [avg_time = 0] [time_count = 0] in all tables [table_name0 -> table_name10]

(define (reset-db)
  (define (help i)
    (cond ((= i 11) (print "Finish init 0-10\n"))
          (else
           (query-exec sqlc  (string-append (update-start i) "0" update-mid "0")  )               
           (help (+ i 1))))
    )
  (help 0)
  )
(define (temp-and-light-ranges sum_temp_ sum_light_ reading_count_)
  (let ((temp_range sum_temp_)
        (light_range sum_light_))
    (if (= temp_range 0)
        (display "SUM RANGE 0\n")
        (set! temp_range (round (/ (/ temp_range reading_count_) 10)))
        )
    (if (= light_range 0)
        (display "SUM RANGE 0\n")
        (set! light_range (round (/ (/ light_range reading_count_) 100)))
        )
    (list temp_range light_range)
    )
  )
;;returns time_avg for database column where avg_temp_range = range
(define (get-range-time-avg lite_i range)
  (query-value sqlc (string-append select-start "avg_time" (select-temp-range-end lite_i) (number->string range) ")")))

;;returns number of times averaged so far for database column where avg_temp_range = range
(define (get-range-time-count lite_i range)
 (query-value sqlc (string-append select-start "time_count" (select-temp-range-end lite_i) (number->string range) ")")))

;;expected time till completion of current water cycle
(define (curr-exp-time sum_temp_ sum_light_ reading_count_ prev_ts_)
  (let ((ranges (temp-and-light-ranges sum_temp_ sum_light_ reading_count_)))
    (- (get-range-time-avg (cadr ranges) (car ranges)) (- (current-seconds) prev_ts_))))

;;updates the avg_time between waterings for the avg_temp_range = curr_temp_range 
(define (update-current-db curr_temp_range time_ lite_i)
  (let ((times_avged   (get-range-time-count lite_i curr_temp_range))
        )
    (let (( range_time_total (* times_avged (get-range-time-avg lite_i curr_temp_range))) 
          ( times_avged_inc (+ times_avged 1))
          )
      (let (( new_avg (/ (+ range_time_total time_) times_avged_inc)) ;; avg_time[n+1] = ((avg_time[n] * n) + time)  /  (n+1)
            ) 
      (query-exec sqlc
                  (string-append (update-start lite_i) (number->string new_avg) update-mid (number->string times_avged_inc) update-where (number->string curr_temp_range) ")"))
      ))))

;;dumps db entries for avg_temp_range = [0 -> 10]
(define (dump-current-db lite_i)
  (query sqlc
       (string-append select-start "avg_time time_count" (select-all-range-end lite_i)))
  )

;;function for plotting temp-range 0 - 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SENSORS FUNTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TEMPERATURE: analog reading -> celsius
(define (curr-temp-c)
  (/ (read-analog-pin 0) 2))
;;TEMPERATURE: analog reading -> fahrenheit
(define (curr-temp-f)
 (+ (/ ( * (curr-temp-c) 9) 5) 32 ))

;;SOIL MOISTURE: analog reading (relative)
(define (curr-soil-moisture)
      (read-analog-pin 1)
  )

;;AMBIENT LIGHT: analog reading (relative)
(define (curr-light)
  (let ((r (read-analog-pin 2)))
        (if (> r 1000) 
            1000
            r
            )
    )            
)

;;quick set of avg's for temp
(define (quick-avg-temp y)
    (define (helper x tmp_avg_temp)
      (if (= x 0)
          (/ tmp_avg_temp y)
          (helper ( - x 1) (+ tmp_avg_temp (curr-temp-f))))
      )
    (helper y 0))

;;quick set of avgs for soil moisture
(define (quick-avg-soil y)
  (define (helper x tmp_avg_soil)
    (if (= x 0)
        (/ tmp_avg_soil y)
        (helper ( - x 1) (+ tmp_avg_soil (curr-soil-moisture))))
    )
  (helper y 0))

;;quick set of avgs for ambient light
(define (quick-avg-light y)
  (define (helper x tmp_avg_light)
    (if (= x 0)
        (/ tmp_avg_light y)
        (helper ( - x 1) (+ tmp_avg_light (curr-light))))
    )
  (helper y 0))

;;
(define (reset-cycle-lists)
  (set! cycle_temperatures '()) (set! cycle_moistures '()) (set! cycle_lights '()))

;;
(define (reset-cycle-vals)
  (set! previous_timestamp_ (current-seconds)) (set! cycle_light_ 0) (set! sum_temp_readings_ 0) (set! temp_reading_count_ 0))

;; UPDATE global values for a single temp sensor-reading / quick-avg for temp sensor
(define (update-globals)
  (begin
    (set! cycle_temperatures (append cycle_temperatures (list (quick-avg-temp 5))))
    (set! cycle_moistures (append cycle_moistures (list (curr-soil-moisture))))
    (set! cycle_lights (append cycle_lights (list (curr-light))))
    (set! sum_temp_readings_ (+ sum_temp_readings_ (quick-avg-temp 5)));; avg = ((avg * #readings) + new_reading)
    (set! cycle_light_ (+ cycle_light_ (quick-avg-light 5)))
    (set! temp_reading_count_ (+ temp_reading_count_ 1))
    (set! cycle_tics_ (+ cycle_tics_ 1))
    (display "GLOBALS:\n")
    (display "sum_temp_readings: ") (display sum_temp_readings_ )
    (display "\ntemp_reading_count: ") (display temp_reading_count_)
    (display "\nwater-soil-moisture-level: ") (display (curr-soil-moisture))
    (display "\nTOTAL-ambient-light-level: ") (display cycle_light_)
    (display "\nCURRENT-ambient-light-level: ") (display (curr-light))
    (display "\nprevious_timestamp: ") (display previous_timestamp_)
    (display "\nElapsed Time: ") (display ( - (current-seconds) previous_timestamp_))
    (display "\ncycle-tics: ") (display cycle_tics_)
    (display "\nTemperatures: ") (display cycle_temperatures)
    (display "\nMoistures: ") (display cycle_moistures) 
    (display "\nLights: ") (display cycle_lights)
    
    ))

;;called when water-soil-moisture is below given level
(define (complete-water-cycle)
  (let ((ranges (temp-and-light-ranges sum_temp_readings_ cycle_light_ temp_reading_count_)))
    (begin
      (display "\n Updating database with values \n sum_temp_readings: ") (display sum_temp_readings_)
      (display "temp_reading_count: ") (display temp_reading_count_) (display "::::::::::::\n\n")
      (update-current-db (car ranges) (- (current-seconds) previous_timestamp_) (cadr ranges) )
      )
    )
  )
    

;;loops indefinately, if soil moisture < min_moisture -> complete water cycle, else -> read & update values, continue
(define (sensor-loop)
  (cond
    ( ( > cycle_tics_ 50) (display "Completed: " ) (display cycle_tics_) (display " Cycles\n") (set! cycle_tics_ 0) (display "********************\n*******************\n") ) 
    ( ( > min-moisture-threshold (quick-avg-soil 5) ) (set! cycle_tics_ 0) (complete-water-cycle)
                                                      (rm-curr-plots)
                                                      (cycle-complete)
                                                      (send-avg-vals sum_temp_readings_ temp_reading_count_ cycle_light_ previous_timestamp_)
                                                      (reset-cycle-vals) (reset-cycle-lists) (sleep 10) (sensor-loop) )
    ( else (update-globals)
           (rm-curr-plots)
           (update-curr-plots cycle_temperatures "temperature" 100)
           (update-curr-plots cycle_moistures "moisture" 1000)
           (update-curr-plots cycle_lights "light" 1000)
           (send-curr-vals ( curr-exp-time sum_temp_readings_ cycle_light_ temp_reading_count_ previous_timestamp_)
                           previous_timestamp_ (curr-soil-moisture) (curr-temp-f) (curr-light) )
           (sleep 10) (sensor-loop) )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;init the database

(init-table-name)

(if (table-exists? sqlc table_name)
    (begin (print "TABLE-EXISTS!\n"))
    (begin 
      (query-exec sqlc (create_data_db 11)) ;; just so can check table exists in future
      (init-db-loop)
      )
    )


(define sensor-thread
  (thread (lambda ()
           (let loop ()
                    (sensor-loop)
    (loop)))))

(init-gui table_name sensor-thread)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


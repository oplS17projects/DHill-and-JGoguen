#lang racket

(require sql db)
(require sqlite-table)
(require racket/async-channel)
(require racket/gui/base)
(require "firmata.rkt")
(require "newgui.rkt")
(require "newdb.rkt")

(provide (all-defined-out))

;; opens up connection to arduino at the given input_port
(open-firmata "/dev/cu.usbmodem1421")

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

;;initialize min-moisture threshold
(define (init-min-moisture)
  (let ((tmp_min (make-string 3)))
    (print "Please Enter '#i followed by a 2-3 digit minimal water moisture level for plant\n")
    (print "Well watered = 300, Average = 200, Dry = 100, Cacti = 50") 
    ;;(write (current-input-port))
    (set! min-moisture-threshold (read (current-input-port)))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SENSORS FUNTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TEMPERATURE: analog reading -> celsius
(define (curr-temp-c)
  (* 500 (/ (read-analog-pin 0) 1024)))

;;TEMPERATURE: analog reading -> fahrenheit
(define (curr-temp-f)
 (+ (/ ( * (curr-temp-c) 9) 5) 32 ))

;;SOIL MOISTURE: analog reading (relative)
(define (curr-soil-moisture)
      (read-analog-pin 1)
  )

;;AMBIENT LIGHT: analog reading (relative)
(define (curr-light)
  (let ((r (/ (* (read-analog-pin 2) 3) 5)))
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
        (exact->inexact (/ tmp_avg_temp y))
        (helper ( - x 1) (+ tmp_avg_temp (curr-temp-f))))
    )
  (helper y 0))

;;quick set of avgs for ambient light
(define (quick-avg-light y)
  (define (helper x tmp_avg_light)
    (if (= x 0)
        (exact->inexact (/ tmp_avg_light y))
        (helper ( - x 1) (+ tmp_avg_light (curr-light))))
    )
  (helper y 0))

(define (quick-avg-moisture y)
  (define (helper x tmp_avg_moisture)
    (if (= x 0)
        (exact->inexact (/ tmp_avg_moisture y))
        (helper ( - x 1) (+ tmp_avg_moisture (curr-soil-moisture))))
    )
  (helper y 0))

;;reset global current-lists
(define (reset-cycle-lists)
  (set! cycle_temperatures '()) (set! cycle_moistures '()) (set! cycle_lights '()))

;;reset global current-values
(define (reset-cycle-vals)
  (set! previous_timestamp_ (current-seconds)) (set! cycle_light_ 0) (set! sum_temp_readings_ 0) (set! temp_reading_count_ 0) (set! cycle_tics_ 0))

;; UPDATE global values for a single temp sensor-reading / quick-avg for temp sensor
(define (update-globals)
  (let ((temp_ (quick-avg-temp 5))
        (moist_ (quick-avg-moisture 5))
        (light_ (quick-avg-light 5))
        )
    (begin
      (set! cycle_temperatures (append cycle_temperatures (list temp_)))
      (set! cycle_moistures (append cycle_moistures (list moist_)))
      (set! cycle_lights (append cycle_lights (list light_)))
      (set! sum_temp_readings_ (+ sum_temp_readings_ temp_));; avg = ((avg * #readings) + new_reading)
      (set! cycle_light_ (+ cycle_light_ light_))
      (set! temp_reading_count_ (+ temp_reading_count_ 1))
      (set! cycle_tics_ (+ cycle_tics_ 1))
      (send-curr-vals ( curr-exp-time sum_temp_readings_ cycle_light_ temp_reading_count_ previous_timestamp_)
                           previous_timestamp_ moist_ temp_ light_  )
      (display "GLOBALS:\n")
      (display "sum_temp_readings: ") (display sum_temp_readings_ )
      (display "\ntemp_reading_count: ") (display temp_reading_count_)
      (display "\nwater-soil-moisture-level: ") (display moist_)
      (display "\nTOTAL-ambient-light-level: ") (display cycle_light_)
      (display "\nCURRENT-ambient-light-level: ") (display light_)
      (display "\nprevious_timestamp: ") (display previous_timestamp_)
      (display "\nElapsed Time: ") (display ( - (current-seconds) previous_timestamp_))
      (display "\ncycle-tics: ") (display cycle_tics_)
      (display "\nTemperatures: ") (display cycle_temperatures)
      (display "\nMoistures: ") (display cycle_moistures) 
      (display "\nLights: ") (display cycle_lights)
      )
    )
  )

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

#lang racket

(require sql db)
(require sqlite-table)
(require "plots.rkt")

(provide (all-defined-out))


;; opens up connection to database
(define sqlc
  (sqlite3-connect #:database "test3.db" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INITIALIZE NEW DB TABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DYNAMIC SQL STATEMENT STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;create-table statement

(define start "CREATE TABLE ")
(define end " (avg_temp_range integer, avg_time integer NOT NULL, time_count integer)")
(define table_name (make-string 12))

(define (init-table-name)
  (print "Please Enter a (exactly) 12 letter table name (first = char, [2-8] = char || digit )")
  (read-string! table_name (current-input-port) 0 12)
)

(define (create_data_db i)
  (if (= i 11)
      (string-append start table_name end)
      (string-append start table_name (number->string i) end)))


;;INSERT INTO ,table_name(i) (avg_temp_range, avg_time, time_count) VALUES ( *??* , 0, 0)
(define (init-start i) (string-append "INSERT INTO " table_name (number->string i)))
(define init-mid " (avg_temp_range, avg_time, time_count) VALUES (" )
(define init-end ", 0, 0)")


;;UPDATE ,table_name(i)  SET avg_time = *??* , time_count = *??* WHERE (avg_temp_range = *??* ) 
(define (update-start i) (string-append "UPDATE " table_name (number->string i) " SET avg_time = "))
(define update-mid ", time_count = ")
(define update-where " WHERE (avg_temp_range = ")

;;SELECT **? avg_time|time_count ?** FROM ,table_name(i) WHERE (avg_temp_range = *??* )
(define select-start "SELECT ")
(define (select-temp-range-end i) (string-append " FROM " table_name (number->string i) " WHERE (avg_temp_range = "))
(define (select-all-range-end i) (string-append " FROM " table_name (number->string i)))


(define (init-db-loop)
        (define (help i)
          (cond ((= i 11) (print "Finish init 0-10\n"))
                (else
                 (query-exec sqlc (create_data_db i))
                 (query-exec sqlc            (string-append (init-start i) init-mid "0" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "1" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "2" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "3" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "4" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "5" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "6" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "7" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "8" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "9" init-end))
                 (query-exec sqlc            (string-append (init-start i) init-mid "10" init-end))
                 (help (+ i 1))))
          )
        (help 0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATABASE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; resets all columns (avg_tmp_range = [ 0 -> 10 ] ) to [avg_time = 0] [time_count = 0] in all tables [table_name0 -> table_name10]
(define (reset-db)
  (define (help i)
    (cond ((= i 11) (print "Finish reset-init 0-10\n"))
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
;;returns time_avg for database column where avg_temp_range = range
(define (get-range-time-avg lite_i range)
  (query-value sqlc (string-append select-start "avg_time" (select-temp-range-end (inexact->exact lite_i)) (number->string (inexact->exact range)) ")")))

;;returns number of times averaged so far for database column where avg_temp_range = range
(define (get-range-time-count lite_i range)
 (query-value sqlc (string-append select-start "time_count" (select-temp-range-end (inexact->exact lite_i)) (number->string (inexact->exact range)) ")")))

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
                  (string-append (update-start (inexact->exact lite_i)) (number->string new_avg) update-mid (number->string times_avged_inc)
                                 update-where (number->string (inexact->exact curr_temp_range)) ")"))
      ))))

;;dumps db entries for avg_temp_range = [0 -> 10]
(define (dump-current-db lite_i)
  (query sqlc
       (string-append select-start "avg_time time_count" (select-all-range-end (inexact->exact lite_i))))
  )

;;function for plotting temp-range/light-level 0 - 10 by lists

(define (light-times->partial-list temp_range)
  (define (helper i)
    (if (= 11 i)
        '()
        (begin (cons (get-range-time-avg i temp_range) (helper (+ i 1))))
        )
    )
  (helper 0)
)

(define (temp-times->partial-list lite_i)
  (define (helper i)
    (if (= 11 i)
        '()
        (begin (cons (get-range-time-avg lite_i i) (helper (+ i 1))))
        )
    )
  (helper 0)
)

;; Constructs list from temperature range [0 -> 10] for light_level (lite_i) of AVG_TIME's for each temp_range 
(define (table-temperature-times->list lite_i)
  (let ((partial-list (temp-times->partial-list lite_i))
        )
    (let ((avg (average-diff partial-list))
          )
      (fill-down avg (fill-front avg partial-list))
      ))
  ) 

;; Constructs list from light range [0 -> 10] for temperature_range (temp_range) of AVG_TIME's for each light level 
(define (table-light-times->list temp_range)
  (let ((partial-list (light-times->partial-list temp_range))
        )
    (let ((avg (average-diff partial-list))
          )
      (fill-down avg (fill-front avg partial-list))
      ))
  ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST SCRIPTS, WILL INIT DB WITH CERTAIN SETS OF VALUES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;updates each temperature range for a given light level with avg_times starting at max_time, and decremented by interval for each temp_range increment
(define (update-temps light_level max_time interval)
  (define (help i)
    (if (= i 11)
        0
        (begin (update-current-db i max_time light_level) (set! max_time (- max_time interval)) (help (+ i 1)))
        )
    )
  (help 0)
  )
;;similar to first update temps, however uses different incrementation 
(define (update-temps-2 light_level max_time interval)
  (let ((sqr_rt (sqrt max_time)))
    (define (help i)
        (if (= i 11)
            0
            (begin (update-current-db i (* sqr_rt sqr_rt) light_level) (set! sqr_rt (- sqr_rt (sqrt interval))) (help (+ i 1)))
            )
        )
    (help 0)
    )
  )

;;;;;;;;;; script for demo ;;;;;;;;;;;;;;;;;
(define (demo-script)
  (update-temps 0 150 9)

  (update-temps 1 140 9)

  (update-temps 2 130 8)

  (update-temps 3 120 8)
  
  (update-temps 4 120 7)
  (update-temps 4 120 7)
  
  (update-temps 5 110 7)
  (update-temps 5 110 7)
  (update-temps 5 110 7)
  
  (update-temps 6 105 7)
  (update-temps 6 105 7)
  (update-temps 6 105 7)
  (update-temps 6 105 7)
  
  (update-temps 7 100 6)
  (update-temps 7 100 6)
  (update-temps 7 100 6)
  (update-temps 7 100 6)
  
  (update-temps 8 95 6)
  (update-temps 8 95 6)
  (update-temps 8 95 6)
  (update-temps 8 95 6)
  
  (update-temps 9 90 6)
  (update-temps 9 90 6)
  (update-temps 9 90 6)
  
  (update-temps 10 85 6)
  (update-temps 10 85 6)
  )

;;;;;;;;;; linear script ;;;;;;;;;;;;;
(define (linear-script)
  (update-temps 0 36000 2000)

  (update-temps 1 35500 2000)

  (update-temps 2 35000 2000)

  (update-temps 3 34500 2000)
  
  (update-temps 4 34000 2000)
  (update-temps 4 34000 2000)
  
  (update-temps 5 33500 2000)
  (update-temps 5 33500 2000)
  (update-temps 5 33500 2000)
  
  (update-temps 6 33000 2000)
  (update-temps 6 33000 2000)
  (update-temps 6 33000 2000)
  (update-temps 6 33000 2000)
  
  (update-temps 7 32500 2000)
  (update-temps 7 32500 2000)
  (update-temps 7 32500 2000)
  (update-temps 7 32500 2000)
  
  (update-temps 8 32000 2000)
  (update-temps 8 32000 2000)
  (update-temps 8 32000 2000)
  (update-temps 8 32000 2000)
  
  (update-temps 9 31500 2000)
  (update-temps 9 31500 2000)
  (update-temps 9 31500 2000)
  
  (update-temps 10 31000 2000)
  (update-temps 10 31000 2000)
  )
;;;;;; quadratic script ;;;;;;;;;;
(define (quad-script)
  (update-temps-2 0 36000 100)

  (update-temps-2 1 35500 100)

  (update-temps-2 2 35000 100)

  (update-temps-2 3 34500 100)
  
  (update-temps-2 4 34000 100)
  (update-temps-2 4 34000 100)
  
  (update-temps-2 5 33500 100)
  (update-temps-2 5 33500 100)
  (update-temps-2 5 33500 100)
  
  (update-temps-2 6 33000 90)
  (update-temps-2 6 33000 90)
  (update-temps-2 6 33000 90)
  (update-temps-2 6 33000 90)
  
  (update-temps-2 7 32500 90)
  (update-temps-2 7 32500 90)
  (update-temps-2 7 32500 90)
  (update-temps-2 7 32500 90)
  
  (update-temps-2 8 32000 85)
  (update-temps-2 8 32000 85)
  (update-temps-2 8 32000 85)
  (update-temps-2 8 32000 85)
  
  (update-temps-2 9 31000 85)
  (update-temps-2 9 31000 85)
  (update-temps-2 9 31000 85)
  
  (update-temps-2 10 30000 80)
  (update-temps-2 10 30000 80)
  )

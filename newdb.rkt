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


;;INSERT INTO ,table_name(i) (avg_temp_range, avg_time, time_count) VALUES ( *?range-row-val?* , 0, 0)
(define (init-start i) (string-append "INSERT INTO " table_name (number->string i)))
(define init-mid " (avg_temp_range, avg_time, time_count) VALUES (" )
(define init-end ", 0, 0)")

;;UPDATE evap_time_avg


;;UPDATE ,table_name(i)  SET avg_time = *?avg_time?* , time_count = *?time_count?* WHERE (avg_temp_range = *?temp_range?* ) 
(define (update-start i) (string-append "UPDATE " table_name (number->string i) " SET avg_time = "))
(define update-mid ", time_count = ")
(define update-where " WHERE (avg_temp_range = ")

;;SELECT *? avg_time || time_count ?* FROM ,table_name(i) WHERE (avg_temp_range = *?temp_range?* )
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

(define (table-temperature-times->list lite_i)
  (let ((partial-list (temp-times->partial-list lite_i))
        )
    (let ((avg (average-diff partial-list))
          )
      (fill-down avg (fill-front avg partial-list))
      ))
  ) 

(define (table-light-times->list temp_range)
  (let ((partial-list (light-times->partial-list temp_range))
        )
    (let ((avg (average-diff partial-list))
          )
      (fill-down avg (fill-front avg partial-list))
      ))
  ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

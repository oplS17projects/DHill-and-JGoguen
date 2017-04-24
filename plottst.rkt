#lang racket

(require plot)
(require plot/utils)

(plot-new-window? #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; placeholder test values

(define cycle_temperatures
  (list 10 20 30 40 50 60 70 80 90 100))

(define cycle_moistures
  (list 100 200 300 400 500 600 700 800 900 1000))
  
(define cycle_lights
  (list 10 20 30 40 50 60 70 80 90 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;these are for CURRENT cycle values, nothing to do with sql... just
;;kept as lists, each reading is appended to the end of proper list every
;;ten seconds

;;each list just (val1 val2 val3....), correlating with 0 seconds, 10
;;seconds, 20 seconds.....

;;x-axis for the CURRENT plots should be time (starting at cycle-time 0
;;to cycle-time MAX),


#|

;;temperatures from 0 - 100

(define (plot-cycle-temperatures)
  (parameterize ([plot-width    500]
                 [plot-height   500]
                 [plot-x-label  "time"]
                 [plot-y-label  "temperature"])
    (plot
     (points (for/list ([i cycle_temperatures]
                        [j (in-range (length cycle_temperatures))])
               (list j i)))
     #:x-min 0 #:x-max (+ 1 (length cycle_temperatures)) #:y-min 0 #:y-max 100))
  )


;;moistures 0 to 1500

(define (plot-cycle-moistures)
  (parameterize ([plot-width    500]
                 [plot-height   500]
                 [plot-x-label  "time"]
                 [plot-y-label  "moisture"])
    (plot
      (points (for/list ([i cycle_moistures]
                         [j (in-range (length cycle_moistures))])
                (list j i)))
      #:x-min 0 #:x-max (+ 1 (length cycle_moistures)) #:y-min 0 #:y-max 1500))
  )


;;lights 0 - 100

(define (plot-cycle-lights)
  (parameterize ([plot-width    500]
                 [plot-height   500]
                 [plot-x-label  "time"]
                 [plot-y-label  "light"])
    (plot
     (points (for/list ([i cycle_lights]
                        [j (in-range (length cycle_lights))])
               (list j i)))
     #:x-min 0 #:x-max (+ 1 (length cycle_lights)) #:y-min 0 #:y-max 100))
  )

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rewrite as more generic implementation

(define (plot-cycle cyclelist xlabel ylabel ymax)
  (parameterize ([plot-width    500]
                 [plot-height   500]
                 [plot-x-label  xlabel]
                 [plot-y-label  ylabel])
    (plot
     (points (for/list ([i cyclelist]
                        [j (in-range (length cyclelist))])
               (list j i)))
     #:x-min 0 #:x-max (+ 1 (length cyclelist)) #:y-min 0 #:y-max ymax))
  )




;;sample calls

;(plot-cycle cycle_temperatures "Time" "Temperature" 100)
;(plot-cycle cycle_moistures "Time" "Moisture" 1500)
;(plot-cycle cycle_lights "Time" "Light" 100)












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list fill functions



;;this first function requires use of database....

;;it makes a list of the avg_times for each avg_temp_range (0 - 10) for
;;a given light level (lite_i)....

;;if there are NO readings for some avg_temp_range i.... then the
;;avg_time is 0, because any temp_range should have at least time 10


#|
(define (temp-times->partial-list lite_i)
   (define (helper i)
     (if (= 11 i)
         '()
         (begin (cons (get-range-time-avg lite_i i) (helper (+ i 1))))
         )
     )
   (helper 0)
)
|#

;;gets the average difference between all sets of adjacent indexes with
;;values not 0 for any list,

;;if either or both are zero, then ignores....

(define (average-diff times_)
  (define (helper i avg num)
    (if ( = i 11)
        (/ avg num)
        (if (or (= 0 (list-ref times_ i)) (= 0 (list-ref times_ (+ i 1))))
            (helper (+ i 1) avg num)
            (helper (+ i 1) (+ avg ( - (list-ref times_ i) (list-ref
                                                            times_ (+ i 1)))) (+ num 1))
            )
        )
    )
  (helper 0 0 0)
  )

;;fills up to first index with values based on average difference
;;between existent indexes


(define (fill-front avg times_)
  (cond
    ((null? times_)
     '())
    ((and (= 0 (list-ref times_ 0)) (not (= 0 (list-ref times_ 1))))
     (cons (+ avg (list-ref times_ 1)) (cdr times_)))
    ((and (= 0 (list-ref times_ 0)) (= 0 (list-ref times_ 1)))
     (fill-front avg (cons 0 (fill-front avg (cdr times_)))))
    (else
     times_)
    )
  )


;;fills down to last index for any list where the atleast the first
;;index is not a 0 ( first two will not be 0 if first isn't ...
;;unimportant side note)


(define (fill-down avg times_)
  (cond
    ((null? times_)
     '())
    (( = 0 (list-ref times_ 1))
     (cons (car times_) (cons (- (list-ref times_ 0) avg) (fill-down
                                                           avg  (cddr times_)))))
    (else
     (cons (car times_) (fill-down avg (cdr times_))))
    )
  )

;;this is how it all together,

;;to eliminate need for database dependent function,
;;"temp-times->partial-list"

;;COULD replace lite_i with "partial-list", just pass this function a
;;premade "partial-list", and remove the first let-statement


#|
(define (table-temp-times->list lite_i)
  (let ((partial-list (temp-times->partial-list lite_i))
         )
     (let ((avg (average-diff partial-list))
           )
       (fill-down avg (fill-front avg partial-list))
       ))
  )
|#

(define (table-temp-times->list partial-list)
  (let ((avg (average-diff partial-list))
        )
    (fill-down avg (fill-front avg partial-list))
    )
  )




;;//when i run my "complete" function//


(table-temp-times->list '(0 0 80 70 53 0 40 30 20 10 0))
;'(102 4/5 91 2/5 80 70 53 41 3/5 40 30 20 10 -1 2/5)



;;//the first partial list//

;> (temp-times->partial-list 8)
;'(0 0 80 70 53 0 40 30 20 10 0)



;;//average for partial-list is (11 2/5) = (57 / 5)//

(average-diff '(0 0 80 70 53 0 40 30 20 10 0))
;11 2/5


;;//fill-front for "avg = 57 / 5" and "partial-list = ...."//

(fill-front (/ 57 5) '(0 0 80 70 53 0 40 30 20 10 0))
;'(102 4/5 91 2/5 80 70 53 0 40 30 20 10 0)


;;//fill-down for "avg = 57 / 2" and previous fill-front created list....
;;output is same as my first "complete" fn

(fill-down (/ 57 5) (fill-front (/ 57 5) '(0 0 80 70 53 0 40 30 20 10 0)))
;'(102 4/5 91 2/5 80 70 53 41 3/5 40 30 20 10 -1 2/5)


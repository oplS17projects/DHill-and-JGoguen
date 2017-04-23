#lang racket

(require plot)
(require plot/utils)

(plot-new-window? #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; placeholder test values

(define cycle_temperatures
  (list 1 2 3 4 5 6 7 8 9 10))

(define cycle_moistures
  (list 1 2 3 4 5 6 7 8 9 10))
  
(define cycle_lights
  (list 1 2 3 4 5 6 7 8 9 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;these are for CURRENT cycle values, nothing to do with sql... just
;;kept as lists, each reading is appended to the end of proper list every
;;ten seconds

;;each list just (val1 val2 val3....), correlating with 0 seconds, 10
;;seconds, 20 seconds.....

;;x-axis for the CURRENT plots should be time (starting at cycle-time 0
;;to cycle-time MAX),


;;temperatures from 0 - 100

(define (plot-cycle-temperatures)
   (parameterize ([plot-width    150]
                  [plot-height   150]
                  [plot-x-label  "time"]
                  [plot-y-label  "temperature"])
     (plot
      (points (for/list ([i cycle_temperatures] [j (in-range (length
cycle_temperatures))])
                (list j i)))
      #:x-min 0 #:x-max (+ 1 (length cycle_temperatures)) #:y-min 0 #:y-max
100))
)


;;moistures 0 to 1500

(define (plot-cycle-moistures)
   (parameterize ([plot-width    150]
                  [plot-height   150]
                  [plot-x-label  "time"]
                  [plot-y-label  "moisture"])
     (plot
      (points (for/list ([i cycle_moistures] [j (in-range (length
cycle_moistures))])
                (list j i)))
      #:x-min 0 #:x-max (+ 1 (length cycle_moistures)) #:y-min 0 #:y-max
1000))
)

;;lights 0 - 100

(define (plot-cycle-lights)
     (parameterize ([plot-width    150]
                  [plot-height   150]
                  [plot-x-label  "time"]
                  [plot-y-label  "light"])
     (plot-frame
      (points (for/list ([i cycle_lights] [j (in-range (length
cycle_lights))])
                (list j i)))
      #:x-min 0 #:x-max (+ 1 (length cycle_lights)) #:y-min 0 #:y-max 100))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







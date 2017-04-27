#lang racket

(require plot)
(require plot/utils)

(provide (all-defined-out))

(plot-new-window? #t)


;plot functions

(define (plot-cycle cyclelist xlabel ymax)
  
  (let ((col
         (cond
           ((equal? "light" xlabel) "yellow")
           ((equal? "temperature" xlabel) "orange")
           ((equal? "moisture" xlabel) "blue")
           (else "red"))))
    
    ;plot data
    (plot (list (axes)
                
                ;line
                (lines (for/list ([i cyclelist]
                                  [j (in-range (length cyclelist))])
                         (list j i))
                       
                       #:color col
                       #:width 4)
              
                ;points
                (points (for/list ([i cyclelist]
                                   [j (in-range (length cyclelist))])
                          (list j i))
                      
                        #:color col
                        #:line-width 4
                        #:sym 'odot))
        
          ;graph
          #:x-min 0
          #:x-max (+ 1 (length cyclelist))
          #:y-min 0
          #:y-max ymax
          #:x-label xlabel
          #:y-label "average cycle time"
          #:width 500
          #:height 500
          #:bgcolor "gray"
          #:title (string-append "average cycle time" " vs. " xlabel))
    )
  )




(define (plot-cycle-vals cycle_vals y-lab y-max)
  
  (let ((col
         (cond
           ((equal? "light" y-lab) "yellow")
           ((equal? "temperature" y-lab) "orange")
           ((equal? "moisture" y-lab) "blue")
           (else "red"))))
  
    ;plot data
    (plot (list (axes)
              
                ;line
                (lines (for/list ([i cycle_vals] [j (in-range (length cycle_vals))])
                         (list j i))

                       #:color col
                       #:width 4)

                ;points
                (points (for/list ([i cycle_vals] [j (in-range (length cycle_vals))])
                          (list j i))

                        #:color col
                        #:line-width 4
                        #:sym 'odot))

          ;graph
          #:x-min 0
          #:x-max (+ 1 (length cycle_vals))
          #:y-min 0
          #:y-max y-max
          #:x-label "time"
          #:y-label y-lab
          #:width 500
          #:height 500
          #:bgcolor "gray"
          #:title (string-append y-lab " vs. " "time"))
    )
  )

    


;;3D test function






;average function

(define (average-diff times_)
  (define (helper i avg num)
    (if ( = i 11)
        (if (= 0 num)
            0
            (/ avg num))
        (if (or (= 0 (list-ref times_ i)) (= 0 (list-ref times_ (+ i 1))))
            (helper (+ i 1) avg num)
            (helper (+ i 1) (+ avg ( - (list-ref times_ i) (list-ref times_ (+ i 1)))) (+ num 1))
            )
        )
    )
  (helper 0 0 0)
  )



;fill functions

(define (fill-front avg times_)
  (cond
    ((or (null? times_) (= 1 (length times_)))
     '())
    ((and (= 0 (list-ref times_ 0)) (not (= 0 (list-ref times_ 1))))
     (cons (+ avg (list-ref times_ 1)) (cdr times_)))
    ((and (= 0 (list-ref times_ 0)) (= 0 (list-ref times_ 1)))
     (fill-front avg (cons 0 (fill-front avg (cdr times_)))))
    (else
     times_)
     )
  )


(define (fill-down avg times_)
  (cond
    ((or (null? times_) (= 1 (length times_)))
     '())
    (( = 0 (list-ref times_ 1))
     (cons (car times_) (cons (- (list-ref times_ 0) avg) (fill-down avg  (cddr times_)))))
    (else
     (cons (car times_) (fill-down avg (cdr times_))))
    )
  )




;; convert to decimal
(define (decimal_list lst)
  (map exact->inexact
       lst
  ))






#|
;sample output
(define temp_lst
  (fill-down (/ 57 5) (fill-front (/ 57 5) '(0 0 80 70 53 0 40 30 20 10 0))))

(define sample_output
  (decimal_list temp_lst))



;sample plots

(plot-cycle sample_output "temperature" 110)

(plot-cycle sample_output "light" 110)

(plot-cycle-vals sample_output "moisture" 110)

|#


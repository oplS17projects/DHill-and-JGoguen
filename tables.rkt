#lang racket

(require racket/gui)
(require "newdb.rkt")
(provide (all-defined-out))

(define tables-frame (new frame%
                   [label "TABLES"]
                   [width 1400]
                   [height 1400]
                   ))


(define h0 (new horizontal-panel%
                [parent tables-frame]
                ))

(define p0 (new vertical-panel%
                [parent h0]
                ))

(define p1 (new vertical-panel%
                [parent h0]
                ))

(define p2 (new vertical-panel%
                [parent h0]
                ))

(define p3 (new vertical-panel%
                [parent h0]
                ))



  (define table0 (new text-field%
                      [parent p0]
                      [label "Table 0"]
                      [min-height 250]
                      )
    )
  (define table1 (new text-field%
                      [parent p1]
                      [label "Table 1"]
                      
                      [min-height 250]
                      ))
  (define table2 (new text-field%
                      [parent p2]
                      [label "Table 2"]
                      [min-height 250]
                      ))
  (define table3 (new text-field%
                      [parent p3]
                      [label "Table 3"]
                      [min-height 250]
                      ))
  (define table4 (new text-field%
                      [parent p0]
                      [label "Table 4"]
                      [min-height 250]
                      ))
  (define table5 (new text-field%
                      [parent p1]
                      [label "Table 5"]
                      [min-height 250]
                      ))
  (define table6 (new text-field%
                      [parent p2]
                      [label "Table 6"]
                      [min-height 250]
                      ))
  (define table7 (new text-field%
                    [parent p3]
                    [label "Table 7"]
                      [min-height 250]
                    ))
  (define table8 (new text-field%
                    [parent p0]
                    [label "Table 8"]
                      [min-height 250]
                    ))
  (define table9 (new text-field%
                      [parent p1]
                      [label "Table 9"]
                      [min-height 250]
                      ))
  (define table10 (new text-field%
                    [parent p2]
                    [label "Table 10"]
                      [min-height 250]
                    ))


(define (set-table0 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table0 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table1 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table1 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table2 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table2 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table3 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table3 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table4 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table4 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table5 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table5
     set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table6 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table6 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table7 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table7 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table8 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table8 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table9 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) " |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table9 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))
(define (set-table10 avg_times_)
  (let ((str (make-string 120))
        )
    (set! str "Temperature Range | Average Cycle Time \n ______________________________________\n")
    (define (help i)
      (if (= i 10)
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n"))
                 (send table10 set-value str))
          (begin (set! str (string-append str "               " (number->string i) "  |   " (number->string (list-ref avg_times_ i)) "\n")) (help (+ i 1)))
        )
      )
    (help 0)
    ))


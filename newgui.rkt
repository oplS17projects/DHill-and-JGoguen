#lang racket

(require racket/gui)
(require "newdb.rkt")
(require "plots.rkt")
(require "tables.rkt")

(provide (all-defined-out))

;;will hold thread that gui oversees... aka sensor-thread....
(define ctrl-thread "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main GUI Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define frame (new frame%
                   [label "Example"]
                   [width 1000]
                   [height 600]
                   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Database plotting interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-functions (new group-box-panel%
                           [label "Database Interface: "]
                           [parent frame]
                           [spacing 0]
                           [vert-margin 0]
                           [min-height 200]
                           ))

(define p0 (new vertical-panel%
                [parent top-functions]
                ))

(define h0 (new horizontal-panel%
                [parent p0]
                ))


(define temp-vs-plot (new button%
                        [parent h0]
                        [label "Temp/Time Plot"]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (send plot-frame show #t)
                                    (send plot-avg-c show #t)
                                    (show-avg-plot (table-temperature-times->list (send light_level_slider get-value)) "temperature range (10's of degrees)")
                                    )
                                  ]
                        )
  )

(define light_level_slider (new slider%
                                [label "LightLevel[0-10]"]
                                [parent h0]
                                [min-value 0]
                                [max-value 10]
                                ))

(define h1 (new horizontal-panel%
                [parent p0]
                ))

(define light-vs-plot (new button%
                        [parent h1]
                        [label "Light/Time Plot"]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (send plot-frame show #t)
                                    (send plot-avg-c show #t)
                                    (show-avg-plot (table-light-times->list (send temp_range_slider get-value)) "light intensity")
                                    )
                                  ]
                        )
  )

(define temp_range_slider (new slider%
                                [label "TemperatureRange[0-10]"]
                                [parent h1]
                                [min-value 0]
                                [max-value 10]
                                ))
(define h2 (new horizontal-panel%
                [parent p0]
                ))

;;;;;;;;;;;;;;;;;; AVERAGE PLOT FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plot-frame (new frame%
                        [parent frame]
                        [label " "]
                        [width 500]
                        [height 500]
                        ))
(define exit-avg-plot (new button%
                        [parent plot-frame]
                        [label "Exit"]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (rm-avg-plots)
                                    (send plot-avg-c show #f)
                                    (send plot-frame show #f)
                                    )
                                  ]
                        )
  )
                           

(define plot-avg-c (new editor-canvas%
               [parent plot-frame]
               [min-height 200] ))


(define plot-avg-pb (new pasteboard%))

;;updates and displays avg-plot of "cyclelist" specified by "xlabel"
(define (show-avg-plot cyclelist xlabel)
  (send plot-avg-pb insert (plot-cycle cyclelist xlabel (+ (max-list cyclelist) 50)) 10 10)
  (send plot-avg-c show #t)
  )


(define (rm-avg-plots)
  (send plot-avg-pb select-all)
  (send plot-avg-pb delete)
  ) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CURRENT values GUI box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define curr-readings (new group-box-panel%
                           [label "Current Values: "]
                           [parent frame]
                           [spacing 0]
                           [vert-margin 0]
                           ))




(define curr-time-panel (new horizontal-panel%
                             [parent curr-readings]
                             [vert-margin 0]
                             ))

(define curr-exp-time-panel (new horizontal-panel%
                                 [parent curr-readings]
                                 [vert-margin 0]
                                 ))
(define curr-sensor-panel (new horizontal-panel%
                                 [parent curr-readings]
                                 [vert-margin 0]
                                 )) 
(new message%
     [parent curr-time-panel]
     [vert-margin 0]
     [label "Current Cycle Time: "]
     )

(new message%
     [parent curr-exp-time-panel]
     [vert-margin 0]
     [label "Expected time until Water: "]
     )
(define curr-time-msg (new message%
                           [parent curr-time-panel]
                           [vert-margin 0]
                           [label "nothing...."]))
(define curr-exp-time-msg (new message%
                               [parent curr-exp-time-panel]
                               [vert-margin 0]
                               [label "nothing...."]))
(new message%
     [parent curr-time-panel]
     [vert-margin 0]
     [label "seconds"]
     )

(new message%
     [parent curr-exp-time-panel]
     [vert-margin 0]
     [label "seconds"]
     )
(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Temperature: "]
     )
(define curr-temperature-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."])
  )
(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "degrees F    "]
     )
(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Soil Moisture: "]
     )
(define curr-moisture-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."])
  )

(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Light Intensity: "]
     )
(define curr-light-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."])
  )

(define plot-curr-c (new editor-canvas%
               [parent curr-readings]
               [min-height 200] ))


(define plot-curr-pb (new pasteboard%))

;;adds plot for "cycle_vals" specified by "y-lab"
(define (update-curr-plots cycle_vals y-lab y-max)
  (let ((x-offset 0))
    (cond
      ((equal? "temperature" y-lab) (set! x-offset 0))
      ((equal? "moisture" y-lab) (set! x-offset 200))
      (else (set! x-offset 400)))
    (send plot-curr-pb insert (plot-cycle-vals cycle_vals y-lab y-max) x-offset 10)
    )
  )

(define (rm-curr-plots)
  (send plot-curr-pb select-all)
  (send plot-curr-pb delete)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SENSOR -> GUI FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;avg-temp-msg update
(define (send-avg-temp sum_temp_ reading_count_)
  (if (equal? sum_temp_ 0)
      ( send avg-temp-msg set-label (number->string sum_temp_) )
      ( send avg-temp-msg set-label (number->string (exact->inexact (round (/ sum_temp_ reading_count_)))) )
      )
  )

;;avg-temp-msg update
(define (send-avg-light light_ reading_count_)
  (if (equal? light_ 0)
      ( send avg-light-msg set-label (number->string light_) )
      ( send avg-light-msg set-label (number->string (exact->inexact (round (/ (/ light_ reading_count_) 100)))) )
      )
  )

;;curr-moisuture-msg update
(define (send-curr-moisture moisture_ )
  ( send curr-moisture-msg set-label (number->string (exact->inexact moisture_)) )
      )
(define (send-curr-temperature temperature_ )
  ( send curr-temperature-msg set-label (number->string (exact->inexact temperature_)) )
      )                                                 
(define (send-curr-light light_ )
  ( send curr-light-msg set-label (number->string (exact->inexact light_)) )
  )

;;curr-expected-time update
(define (send-curr-exp-time exp_time)
  (send curr-exp-time-msg set-label (number->string exp_time)) 
      )
      
;;cycle-time update  
(define (send-cycle-time prev_ts_) 
  ( send avg-time-msg set-label (number->string (- (current-seconds) prev_ts_)) )
  ( send curr-time-msg set-label (number->string (- (current-seconds) prev_ts_)) )
  )


;;send gui avgs
(define (send-avg-vals sum_temp_ reading_count_ light_ prev_ts_)
  (send-avg-temp sum_temp_ reading_count_) (send-avg-light light_ reading_count_) (send-cycle-time prev_ts_))

;;send gui currents
(define (send-curr-vals exp_time prev_ts_ moisture_ temp_ light_ )
  (send-curr-moisture moisture_) (send-curr-temperature temp_) (send-curr-light light_) (send-cycle-time prev_ts_) (send-curr-exp-time exp_time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AVG cycle GUI box  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define avg-readings (new frame%
                           [label "Cycle Values: "]
                           [parent frame]
                           [height 100]
                           [width 100]
                           [spacing 0]
                           
                           ))

(define avg-temp-panel (new horizontal-panel%
                       [parent avg-readings]
                       [vert-margin 0]))

(define avg-light-panel (new horizontal-panel%
                       [parent avg-readings]
                       [vert-margin 0]))

(define avg-time-panel (new horizontal-panel%
                       [parent avg-readings]
                       [vert-margin 0]))
(new message%
     [parent avg-temp-panel]
     [label "AvgTemp: "]
     [vert-margin 0])

(new message%
     [parent avg-light-panel]
     [label "AvgLight: "]
     [vert-margin 0])

(new message%
     [parent avg-time-panel]
     [label "CycleTime: "]
     [vert-margin 0])

(define avg-temp-msg (new message%
                      [parent avg-temp-panel]
                      [label "nothing...."]
                      [vert-margin 0]))

(define avg-light-msg (new message%
                      [parent avg-light-panel]
                      [label "nothing...."]
                      [vert-margin 0]))

(define avg-time-msg (new message%
                      [parent avg-time-panel]
                      [label "nothing...."]
                      [vert-margin 0]))

(define next-cycle (new button%
                        [parent avg-readings]
                        [label "NEXT CYCLE!!"]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (send avg-readings show #f)
                                    (send curr-readings show #t)
                                    (send plot-curr-c show #t)
                                    (thread-resume ctrl-thread)
                                    )
                                  ]
                        )
  )

(define exit-button (new button%
                        [parent frame]
                        [label "Exit"]
                        ; Callback procedure for a button click:
                        [callback (lambda (button event)
                                    (exit 1)
                                    )
                                  ]
                        )
  )


(define tables-button (new button%
                           [parent frame]
                           [label "Tables"]
                           [callback (lambda (button event)
                                       (show-tables-frame)
                                       )
                                     ]
                           )
  )

(define (set-all-tables)
  (set-table0 (table-temperature-times->list 0))
  (set-table1 (table-temperature-times->list 1))
  (set-table2 (table-temperature-times->list 2))
  (set-table3 (table-temperature-times->list 3))
  (set-table4 (table-temperature-times->list 4))
  (set-table5 (table-temperature-times->list 5))
  (set-table6 (table-temperature-times->list 6))
  (set-table7 (table-temperature-times->list 7))
  (set-table8 (table-temperature-times->list 8))
  (set-table9 (table-temperature-times->list 9))
  (set-table10 (table-temperature-times->list 10))
       
  )
(define (show-tables-frame)
  (set-all-tables)
  (send tables-frame focus)
  (send tables-frame show #t)
  (send table0 show #t)
  (send table1 show #t)
  (send table2 show #t)
  (send table3 show #t)
  (send table4 show #t)
  (send table5 show #t)
  (send table6 show #t)
  (send table7 show #t)
  (send table8 show #t)
  (send table9 show #t)
  (send table10 show #t)
  (send tables-frame show #t)
  )


(define (init-gui table_name thd)
  (send frame set-label (string-append "Current Table Name:  " table_name))
  (send top-functions show #t)
  (send plot-frame show #f)
  (send curr-readings show #t)
  (send avg-readings show #f)
  (send frame show #t)
  (send avg-readings client->screen 500 500)
  (send plot-avg-c set-editor plot-avg-pb)
  (send plot-avg-c show #f)
  (send plot-curr-c set-editor plot-curr-pb)
  (send plot-curr-c show #t)
  (set! ctrl-thread thd)
)

(define (cycle-complete)
  (send avg-readings show #t)
  (send curr-readings show #f)
  (send plot-curr-c show #f)
  (thread-suspend ctrl-thread)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
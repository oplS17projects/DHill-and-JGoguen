#lang racket

(require racket/gui)
(require plot)
(provide (all-defined-out))

(define ctrl-thread "")

;(define cycle_temperatures '(90 91 90 89 86 87))
;(define cycle_moistures '(800 750 700 650 610 580))
;(define cycle_lights '(70 90 88 89 90 75))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialization Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define initial_frame (new frame%
  ;                         [label "Set Up"]
   ;                        [width 400]
    ;                       [height 200]))

;(define init_h (new horizontal-panel%
 ;                   [parent initial_frame]
  ;                  ))

;(define init-button (new button%
 ;                       [parent init_h]
  ;                      [label "Initialize Program!"]
   ;                     ; Callback procedure for a button click:
    ;                    [callback (lambda (button event)
     ;                               (print (send table-inquiry get-text))
      ;                              (send curr-readings show #t)
       ;                             (send avg-readings show #f)
        ;                            (send frame show #t)
         ;                           (send avg-readings client->screen 500 500)
          ;                          (send c set-editor pb)
           ;                         (send c show #t)
                                    ;(update-plots cycle_temperatures "temperature" 100)
                                    ;(update-plots cycle_moistures "moisture" 1000)
                                    ;(update-plots cycle_lights "light" 100)
            ;                        (send initial_frame show #f)
             ;                       )
              ;                    ]
               ;         )
  ;)


;;;;;;;;;;;;; text input for intiailze button ;;;;;;;;;;;;;;;;;;;;;;;;

;(define tfc (new editor-canvas%
 ;              [parent init_h]
  ;             [stretchable-height 10] ))
;
;(define table-inquiry (new text%))

;(send initial_frame enable #t)
;(send initial_frame show #t)
;(send tfc set-editor table-inquiry)
;(send tfc show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main GUI Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define frame (new frame%
                   [label "Example"]
                   [width 1000]
                   [height 600]
                   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Database plotting interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                    (print (send light_level_slider get-value))
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
                                    (print (send temp_range_slider get-value))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CURRENT values GUI box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Soil Moisture: "]
     )

(define curr-moisture-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."]))

(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Temperature: "]
     )
(define curr-temperature-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."]))

(new message%
     [parent curr-sensor-panel]
     [vert-margin 0]
     [label "Current Light Intensity (/ 1000 max): "]
     )

(define curr-light-msg (new message%
                               [parent curr-sensor-panel]
                               [vert-margin 0]
                               [label "nothing...."]))

(define c (new editor-canvas%
               [parent curr-readings]
               [min-height 200] ))


(define pb (new pasteboard%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SENSOR -> GUI FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;sets up gui for appropriate display values 
;(define (set-frame-header c)
 ; (cond
  ;  ((equal? c "cwc") (send curr-readings show #f) (send avg-readings show #t) (send msg set-label "Completeted Water Cycle!"))
   ; ((equal? c "db")  (send msg set-label "Database Values"))
    ;(else (send curr-readings show #t) (send avg-readings show #f) (send msg set-label "Current Readings..."))
     ;))

;;avg-temp-msg update
(define (send-avg-temp sum_temp_ reading_count_)
  (if (equal? sum_temp_ 0)
      ( send avg-temp-msg set-label (number->string sum_temp_) )
      ( send avg-temp-msg set-label (number->string (round (/ sum_temp_ reading_count_))) )
      )
  )

;;avg-temp-msg update
(define (send-avg-light light_ reading_count_)
  (if (equal? light_ 0)
      ( send avg-light-msg set-label (number->string light_) )
      ( send avg-light-msg set-label (number->string (round (/ (/ light_ reading_count_) 100))) )
      )
  )

;;curr-moisuture-msg update
(define (send-curr-moisture moisture_ )
  ( send curr-moisture-msg set-label (number->string moisture_) )
      )
(define (send-curr-temperature temperature_ )
  ( send curr-temperature-msg set-label (number->string temperature_) )
      )                                                 
(define (send-curr-light light_ )
  ( send curr-light-msg set-label (number->string light_) )
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

(define (plot-cycle-vals cycle_vals y-lab y-max)
    (parameterize (
                   [plot-width    180]
                   [plot-height   180]
                   [plot-x-label  "time"]
                   [plot-y-label  y-lab])
      (plot-snip
       (points (for/list ([i cycle_vals] [j (in-range (length cycle_vals))])
                 (list j i)))
       #:x-min 0 #:x-max (length cycle_vals) #:y-min 0 #:y-max y-max))
  )


(define (update-plots cycle_vals y-lab y-max)
  (let ((x-offset 0))
    (cond
      ((equal? "temperature" y-lab) (set! x-offset 0))
      ((equal? "moisture" y-lab) (set! x-offset 200))
      (else (set! x-offset 400)))
    (send pb insert (plot-cycle-vals cycle_vals y-lab y-max) x-offset 10)
    )
  )

(define (rm-plots)
  (send pb select-all)
  (send pb delete)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AVG cycle GUI box  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define avg-readings (new frame%
                           [label "Cycle Values: "]
                           [parent frame]
                           [stretchable-height 30]
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
                                    (send c show #t)
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

(define (init-gui table_name thd)
  (send frame set-label (string-append "Current Plant" table_name))
  (send curr-readings show #t)
  (send avg-readings show #f)
  (send frame show #t)
  (send avg-readings client->screen 500 500)
  (send c set-editor pb)
  (send c show #t)
  (set! ctrl-thread thd)
)

(define (cycle-complete)
  (send avg-readings show #t)
  (send curr-readings show #f)
  (send c show #f)
  (thread-suspend ctrl-thread)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
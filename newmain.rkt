#lang racket

(require db)
(require sql)
(require "newgui.rkt")
(require "newdb.rkt")
(require "sensors.rkt")

;;init the database


(init-table-name)

(if (table-exists? sqlc table_name)
    (begin (print "TABLE-EXISTS!\n"))
    (begin 
      (query-exec sqlc (create_data_db 11)) ;; just so can check table exists in future
      (init-db-loop)
      )
    )

(init-min-moisture)

(reset-db)

(demo-script)

(define sensor-thread
  (thread (lambda ()
           (let loop ()
                    (sensor-loop)
    (loop)))))

(init-gui table_name sensor-thread)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


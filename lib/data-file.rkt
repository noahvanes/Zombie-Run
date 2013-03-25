;LIB/DATA-FILE.RKT
;=================

#lang r5rs

(#%require (only racket/base eof))
(#%require "OO-syntactic-sugar.rkt")

(#%provide data-file)

;************************************************************
;* DATA FILE                                                *
;* - Simple ADT to read and write contents to a file/path   *
;*                                                          *
;*                                      - By Noah Van Es    *
;************************************************************

; DATA-FILE
; ---------
(define (data-file path)
  
  (define (initialize)
    (let ((output-port (open-output-file path #:exists 'can-update)))
      (close-output-port output-port)))
  
  (define (get-path) path)
  
  (define (get-content)
    (let* ((input-port (open-input-file path))
           (content (read input-port)))
      (close-input-port input-port)
      content))
  
  (define (set-content! content)
    (let ((output-port (open-output-file path #:exists 'replace)))
      (write content output-port)
      (close-output-port output-port)))
  
  (define (update-content! content-updater)
    (let ((content (get-content)))
      (set-content! (content-updater content))))
  
  (define (clear-content!)
    (let ((output-port (open-output-file path #:exists 'replace)))
      (close-output-port output-port)))
  
  (define (empty?)
    (eq? (get-content) eof))
  
  (initialize)
  
  (define dispatch-datafile
    (dispatcher get-path
                get-content
                set-content!
                update-content!
                clear-content!
                empty?))
  dispatch-datafile)
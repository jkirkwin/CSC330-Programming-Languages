#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Write a function sequence that takes 3 arguments low, high, and
; stride, all assumed to be numbers. Further, assume stride is
; positive. sequence produces a list of numbers from low to high
; (including low and possibly high) separated by stride and in
; sorted order. Sample solution: 4 lines
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))  
  ))

; Write a function string-append-map that takes a list of strings xs
; and a string suffix and returns a list of strings. Each element of
; the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix).
; Use Racket-library functions map and string-append. 
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define list-nth-mod null)

(define stream-for-n-steps null)

(define funny-number-stream null)

(define cat-then-dog null)

(define stream-add-zero null)

(define cycle-lists null)

(define vector-assoc null)

(define cached-assoc null)



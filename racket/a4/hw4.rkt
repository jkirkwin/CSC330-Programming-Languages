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
      (cons low (sequence (+ low stride) high stride))))

; Write a function string-append-map that takes a list of strings xs
; and a string suffix and returns a list of strings. Each element of
; the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix).
; Use Racket-library functions map and string-append. 
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; Write a function list-nth-mod that takes a list xs and a number n.
; If the number is negative, terminate the computation with (error
; "list-nth-mod: negative number"). Else if the list is empty,
; terminate the computation with (error "list-nth-mod: empty list").
; Else return the i-th element of the list where we count from zero
; and i is the remainder produced when dividing n by the list’s length.
; Library functions length, remainder, car, and list-tail are all useful
; - see the Racket documentation.
(define (list-nth-mod xs n)
  (if (< n 0 )
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (let*
              ([position (remainder n (length xs))]
              [tail (list-tail xs position)])
              (car tail)))))

; Write a function stream-for-n-steps that takes a stream s and a number
; n. It returns a list holding the first n values produced by s in order.
(define (stream-for-n-steps s n)
  (if (< n 0) (error "stream-for-n-steps: negative count")
      (if (= n 0) null
          (let ([result (s)])
            (cons (car result) (stream-for-n-steps (cdr result) (- n 1)))))))

; Write a stream funny-number-stream that is like the stream of natural
; numbers (i.e., 1, 2, 3, ...) except numbers divisible by 5 are negated
; (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is
; a thunk that when called produces a pair. Here the car of the pair
; will be a number and the cdr will be another stream.
(define (funny-number-stream)
  (define (helper seed)
  (let* ([prev (abs seed)]
         [next (+ prev 1)]
         [remainder (modulo next 5)]
         [result (if (equal? 0 remainder) (- next) next)])
    (cons seed (lambda () (helper result)))))
  (helper 1))

; Write a stream cat-then-dog, where the elements of the stream alternate
; between the strings "cat.jpg" and "dog.jpg" (starting with "cat.jpg"). More
; specifically, cat-then-dog should be a thunk that when called produces a pair
; of "cat.jpg" and a thunk that when called produces a pair of "dog.jpg" and a
; thunk that when called... etc. Sample solution: 4 lines.
(define (cat-then-dog)
  (letrec ([cat (lambda () (cons "cat.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" cat))])
    (cat)))

; Write a function stream-add-zero that takes a stream s and returns another
; stream. If s would produce v for its i-th element, then (stream-add-zero s)
; would produce the pair (0 . v) for its i-th element. Sample solution: 4 lines.
; Hint: Use a thunk that when called uses s and recursion. Note: One of the
; provided tests uses (stream-add-zero cat-then-dog) with place-repeatedly.
(define (stream-add-zero s)
  (let* ([pair (s)]
        [result (cons 0 (car pair))]
        [s2 (cdr pair)])
    (lambda () (cons result (stream-add-zero s2)))))

; Write a function cycle-lists that takes two lists xs and ys and returns a
; stream. The lists may or may not be the same length, but assume they are both
; non-empty. The elements produced by the stream are pairs where the first part
; is from xs and the second part is from ys. The stream cycles forever through
; the lists.

; A helper function to create a stream from a list.
(define (cycle-list l pos)
  (lambda () (cons (list-nth-mod l pos) (cycle-list l (+ pos 1)))))

; A helper function to create a stream of pairs of other streams' results.
(define (zip-streams s1 s2)
  (let* ([s1-result (s1)]
        [s2-result (s2)]
        [result (cons (car s1-result) (car s2-result))])
    (lambda () (cons result (zip-streams (cdr s1-result) (cdr s2-result))))))

(define (cycle-lists xs ys)
  (zip-streams (cycle-list xs 0) (cycle-list ys 0)))

; Write a function vector-assoc that takes a value v and a vector vec. It should
; behave like Racket’s assoc library function except:
;   (a) it processes a vector (Racket’s name for an array) instead of a list and
;   (b) it allows vector elements not to be pairs in which case it skips them.
; Process the vector elements in order starting from 0. Use library functions
; vector-length, vector-ref, and equal?. Return #f if no vector element is a
; pair with a car field equal to v, else return the first pair with an equal car
; field. Sample solution is 9 lines, using one local recursive helper function.
(define (vector-assoc val vec)
  (define (helper pos)
    (if (>= pos (vector-length vec)) #f
        (let ([item (vector-ref vec pos)])
          (if (not (pair? item)) (helper (+ pos 1))
              (if (equal? (car item) val) item (helper (+ pos 1)))))))
  (helper 0))

; Write a function cached-assoc that takes a list xs and a number n and returns
; a function that takes one argument v and returns the same thing that
; (assoc v xs) would return. However, you should use an n-element cache of
; recent results to possibly make this function faster than just calling assoc
; (if xs is long and a few elements are returned often). The cache should be a
; vector of length n that is created by the call to cached-assoc and
; used-and-possibly-mutated each time the function returned by cached-assoc is
; called.
;
; The cache starts empty (all elements #f). When the function returned by
; cached-assoc is called, it first checks the cache for the answer. If it is not
; there, it uses assoc and xs to get the answer and if the result is not #f
; (i.e., xs has a pair that matches), it adds the pair to the cache before
; returning (using vector-set!). The cache slots are used in a round-robin
; fashion: the first time a pair is added to the cache it is put in position 0,
; the next pair is put in position 1, etc. up to position n - 1 and then back to
; position 0 (replacing the pair already there), then position 1, etc.
(define (cached-assoc xs n)
  (define cache (make-vector n))
  (vector-fill! cache #f)
  (define cache-ptr 0)
  (define (add-to-cache val)
    (vector-set! cache cache-ptr val)
    (set! cache-ptr (modulo (+ cache-ptr 1) n))
    val)
  
  (lambda (v)
    (define cache-lookup (vector-assoc v cache))
    (if cache-lookup cache-lookup
        (let ([lookup (assoc v xs)])
          (if (not lookup) #f (add-to-cache lookup))))))

; Define a macro that is used like (while-less e1 do e2) where e1 and e2 are
; expressions and while-less and do are syntax (keywords).
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([test e1]
           [th (lambda () (if (>= e2 test) #t (th)))])
       (th))]))


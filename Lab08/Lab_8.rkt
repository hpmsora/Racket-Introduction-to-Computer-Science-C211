;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Lab 10

;------------------------------------------------------------------------------------------
;Exercise 1
;A function Create-List is:
; has one argument, a natural number, that returns a list of that many random natural
; numbers (whose values are between 0 and 10).
; Number -> ListOfNumber
(define (create-list num)
  (cond
    [(= 0 num) empty]
    [else
     (cons (random 11) (create-list (- num 1)))]
    ))

;checking the function
(check-expect (create-list 0) empty)

;------------------------------------------------------------------------------------------
;Exercise 2

;A function Insert is:
; two arguments, a natural number and a list of lists of numbers, and adds the numbers to
; the front of the first element in the list if the furst element in the list isn't full
; yet(that is, if is has size strictly less than 3) or adds the number as a list of one
; element to the front of the list, otherwise.
; Number LoN(ListOfNumber) -> LoN
(define (insert num lon)
  (cond
    [(empty? lon) (cons (cons num empty) empty)]
    [else
     (cond
       [(> 3 (length (first lon))) (cons (cons num (first lon)) (rest lon))]
       [else (cons (list num) lon)]
       )]
    ))

;checking the function
(define llon-1 (list (list 3)))
(define llon-2 (list (list 2 3)))
(define llon-3 (list (list 5 2 3)))
(define llon-4 (list (list 6) (list 5 2 3)))
(define llon-5 (list (list 1 2) (list 3 4 5)))
(check-expect (insert 3 empty) llon-1)
(check-expect (insert 6 '((5 2 3))) llon-4)
(check-expect (insert 2 llon-1) llon-2)
(check-expect (insert 5 '((2 3))) llon-3)
(check-expect (insert 1 (insert 2 (insert 3 (insert 4 (insert 5 empty))))) llon-5)

;------------------------------------------------------------------------------------------
;Exercise 3

;A function Bundle is:
; takes a list of numbers and groups them in "chunks" (lists of numbers) of size 3.
; If the initial list of numbers has a length that is not a multiple of 3 the "chunk" in
; front has size equal to the remainder.
; LoN -> LoN
(define (bundle lon)
  (cond
    [(empty? lon) empty]
    [else
     (insert (first lon) (bundle (rest lon)))]
    ))

;checking the function
(check-expect (bundle empty) empty)
(check-expect (bundle '(1)) (list (list 1)))
(check-expect (bundle '(4 2)) (list (list 4 2)))
(check-expect (bundle '(3 5 2)) (list (list 3 5 2)))
(check-expect (bundle '(-1 7 5 6)) (list (list -1) (list 7 5 6)))
(check-expect (bundle '(3 2 1 4 6)) (list (list 3 2) (list 1 4 6)))
(check-expect (bundle '(1 2 3 4 5 6 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;------------------------------------------------------------------------------------------
;Exercise 4

;A function Insert-A is:
; receives three arguments: the desired chunk size plus the other two arguments that insert
; already had.
; Number Number LoN -> LoN
(define (insert-up-to size num lon)
  (cond
    [(empty? lon) (cons (cons num empty) empty)]
    [else
     (cond
       [(> size (length (first lon))) (cons (cons num (first lon)) (rest lon))]
       [else (cons (list num) lon)]
       )]
    ))

;checking the function
(check-expect (insert-up-to 3 3 empty) (list (list 3)))
(check-expect (insert-up-to 3 7 '((3))) (list (list 7 3)))
(check-expect (insert-up-to 3 2 '((7 3))) (list (list 2 7 3)))
(check-expect (insert-up-to 3 5 '((2 7 3))) (list (list 5) (list 2 7 3)))

;------------------------------------------------------------------------------------------
;Exercise 5

;A function Bundle-Of is:
; customization process of bundle by writing a function bundle-of. It receives an extra
; argument representing the chunk size.
; Number LoN -> LoN
(define (bundle-of size lon)
  (cond
    [(empty? lon) empty]
    [else
     (insert-up-to size (first lon) (bundle-of size (rest lon)))]
    ))

;checking the function
(check-expect (bundle-of 3 empty) empty)
(check-expect (bundle-of 3 '(1)) (list (list 1)))
(check-expect (bundle-of 3 '(1 2)) (list (list 1 2)))
(check-expect (bundle-of 3 '(1 2 3)) (list (list 1 2 3)))
(check-expect (bundle-of 3 '(1 2 3 4)) (list (list 1) (list 2 3 4)))
(check-expect (bundle-of 3 '(1 2 3 4 5)) (list (list 1 2) (list 3 4 5)))
(define c (list 2 4 6 6 7 3 4 0 6))
(check-expect (bundle-of 3 c) (list (list 2 4 6) (list 6 7 3) (list 4 0 6)))
(check-expect (bundle-of 4 c) (list (list 2) (list 4 6 6 7) (list 3 4 0 6)))
(check-expect (bundle-of 5 c) (list (list 2 4 6 6) (list 7 3 4 0 6)))

;------------------------------------------------------------------------------------------
;Exercise 6

;A function Flatten is:
; takes a list of lists of numbers (possibly produced by bundle-of) and flattens it.
; LoN -> LoN
(define (flatten lon)
  (cond
    [(empty? lon) empty]
    [(cons? (first lon)) (remove-e (cons (first (first lon)) (flatten (cons (rest (first lon)) (flatten (rest lon))))))]
    [else (cons (first lon) (flatten (rest lon)))]))

;Implicit function of Flatten
; LoN -> LoN
(define (remove-e lon)
  (cond
    [(empty? lon) empty]
    [(empty? (first lon)) (remove-e (rest lon))]
    [else (cons (first lon) (remove-e (rest lon)))]))

;checking the function
(check-expect (flatten empty) empty)
(check-expect (flatten (bundle-of 3 c)) c)
(check-expect (flatten (bundle-of 4 c)) c)

;------------------------------------------------------------------------------------------
;Exercise 7

;A function Take is:
; has number and a list of numbers. take returns the list of the first that many (first argument)
; elements of the list passed as the second argument. If the list is shorter than the number
; passed as the first argument it is returned in its entirety.
; Number LoN -> LoN
(define (take num lon)
  (cond
    [(empty? lon) empty]
    [(= 0 num) empty]
    [else
     (cons (first lon) (take (- num 1) (rest lon)))]
    ))

;checking the function
(define f (list 3 7 3 3 4 8 1 9 2))
(check-expect (take 3 f) (list 3 7 3))
(check-expect (take 8 f) (list 3 7 3 3 4 8 1 9))
(check-expect (take 9 f) (list 3 7 3 3 4 8 1 9 2))
(check-expect (take 10 f) (list 3 7 3 3 4 8 1 9 2))
(check-expect (take 11 f) (list 3 7 3 3 4 8 1 9 2))
(check-expect (take 0 f) empty)

;------------------------------------------------------------------------------------------
;Exercise 8

;A function Drop is:
; It receives two arguments: a natural number and a list of numbers. It returns what’s left
; after we eliminate the first that many (first argument) elements from the list passed as
; the second argument.
; Number LoN -> LoN
(define (drop num lon)
  (cond
    [(empty? lon) empty]
    [(= 0 num) lon]
    [else
     (drop (- num 1) (rest lon))]
    ))

;checking the function
(define g (list 3 4 8 8 5 0 4 1 4 0 6 2))
(check-expect (drop 0 g) (list 3 4 8 8 5 0 4 1 4 0 6 2))
(check-expect (drop 3 g) (list 8 5 0 4 1 4 0 6 2))
(check-expect (drop 7 g) (list 1 4 0 6 2))
(check-expect (drop 11 g) (list 2))
(check-expect (drop 12 g) empty)
(check-expect (drop 13 g) empty)
(check-expect (append (take 5 g) (drop 5 g)) g)

;No it is not possible
;Lon -> Lon
(define (first-a lon)
  (take 1 lon))

;checking the function
(check-expect (first-a (list 4 3 5 1 2)) (list 4))

;But the data definition is different it is not possible.
; The (first (list 4 3 5 1 2) is 4 not (list 4)

;------------------------------------------------------------------------------------------
;Exercise 9

;A function Sorted? is:
; two arguments: a list of numbers and a predicate (a function that takes two numbers and
; returns a boolean). For example < is a predicate, (< n m) returns true or false
; depending on whether n is less than m or not. The function sorted? should determine if
; the list received as the first argument is sorted or not according to the predicate.
; (Note: a list is sorted according to a certain predicate if every two adjacent elements
; of the list satisfy the predicate.)
; LoN Predicate -> Boolean
(define (sorted? lon fn)
  (cond
    [(or (empty? lon) (empty? (rest lon))) true]
    [else
     (cond
       [(fn (first lon) (first (rest lon))) (sorted? (rest lon) fn)]
       [else false]
       )]
    ))

;checking the function
(check-expect (sorted? empty <) true)
(check-expect (sorted? (list 1) <) true)
(check-expect (sorted? (list 1 2 3 4 5 6) <) true)
(check-expect (sorted? (list 1 2 3 4 6 5) <) false)

;------------------------------------------------------------------------------------------
;Exercise 10

;A function One-Pass is:
; two arguments: a list of numbers and a predicate. The function goes through the list from
; left to right, once, and for every pair of adjacent elements that don’t satisfy the
; predicate the elements are swapped.
; LoN Predicate -> LoN
(define (one-pass lon fn)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) lon]
    [else
     (cond
       [(or (fn (first lon) (first (rest lon))) (= (first lon) (first (rest lon))))
        (cons (first lon) (one-pass (rest lon) fn))]
       [else
        (one-pass 
         (cons (first (rest lon))
               (cons (first lon)
                     (rest (rest lon)))) fn)]
       )]
    ))

;checking the function
(check-expect (one-pass empty <) empty)
(define h (list 1 8 2 0 2 9 6 7 7 9 5))
(check-expect (one-pass h <) (list 1 2 0 2 8 6 7 7 9 5 9))
(check-expect (one-pass h >) (list 8 2 1 2 9 6 7 7 9 5 0))
(check-expect (one-pass (list 1 2 3 4 5 6) >) (list 2 3 4 5 6 1))
(check-expect (one-pass (one-pass (list 1 2 3 4 5 6) >) >) (list 3 4 5 6 2 1))
(check-expect (one-pass (one-pass (one-pass (list 1 2 3 4 5 6) >) >) >)
 (list 4 5 6 3 2 1))

;A function Bubble-Sort is:
; attempts to sort the list of numbers according to the predicate.
; [ListOf Number] [Number Number -> Boolean] -> [ListOf Number]
(define (bubble-sort lon fn)
  (cond
    [(sorted? lon fn) lon]
    [else
     (bubble-sort (one-pass lon fn) fn)]
    ))

(check-expect (bubble-sort (list 1 2 3 4 5 6) >) (list 6 5 4 3 2 1))
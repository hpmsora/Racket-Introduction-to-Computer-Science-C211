;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment_11_Generative_Recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment 11:Generative Recursion

;--------------------------------------------------------------------------------------------------------
;1. Book Exercise

;------------------------------------------------------------------------------
;Exercise 365

;A ListOfTokens(LoT) is one of:
; - empty
; - (cons lString LoT)
; - (cons String LoT)

; A Line is [List-of 1String]

; A 1String is a string of length 1,  
; including " " (the space bar), "\t" (tab), 
; "\r" (return), and "\b" (backspace). 
; interpretation represents a key on a keyboard  

;A function Tokenize is:
; takes Line and returns ListOfTokens(LoT).
; Also, all white-space lStrings are dropped; all other non-letters remains as is;
; and all consecutive letters are bundled into "words."
; Line -> LoT
(define (tokenize los)
  (local
    (
     ;LoS -> String
     (define (attaching los)
       (local
         (;LoS -> String
          (define (attached los)
            (cond
              [(empty? los) ""]
              [(string-whitespace? (first los)) ""]
              [(or (string=? "." (first los)) (string=? "!" (first los)) (string=? "?" (first los))
                   (string=? "," (first los)) (string=? ";" (first los)) (string=? ":" (first los)))
                   ""]
              [else (string-append (first los) (attached (rest los)))]
              ))
          )(attached los)))
     ;LoS -> LoS
     (define (rest-los los)
       (local
         (;LoS -> LoS
          (define (find-rest los)
            (cond
              [(empty? los) empty]
              [(string-whitespace? (first los)) (rest los)]
              [(or (string=? "." (first los)) (string=? "!" (first los)) (string=? "?" (first los))
                   (string=? "," (first los)) (string=? ";" (first los)) (string=? ":" (first los)))
                   los]
              [else (find-rest (rest los))]
              ))
          )(find-rest los)))
     ;LoS -> LoS
     (define (tokenizing los)
       (cond
         [(empty? los) empty]
         [(or (string=? "." (first los)) (string=? "!" (first los)) (string=? "?" (first los))
                   (string=? "," (first los)) (string=? ";" (first los)) (string=? ":" (first los)))
                   (cons (first los) (tokenizing (rest-los (rest los))))]
         [(not (string-whitespace? (first los)))
          (cons (attaching los) (tokenizing (rest-los los)))]
         [(string-whitespace? (first los))
          (cons (attaching los) (tokenizing (rest-los los)))]
         ))
    )(tokenizing los)))

;checking the function
(check-expect (tokenize (list "h" "e" "l" "l" "o" " " "w" "o" "r" "l" "d" "."))
              (list "hello" "world" "."))
(check-expect (tokenize empty) empty)
(check-expect (tokenize (list "a" "a" "a" "b" "c" "\n"
                              "b" "s" "e" "e" "e" "\n"))
              (list "aaabc" "bseee"))
(check-expect (tokenize (list "a" "a"))
              (list "aa"))
(check-expect (tokenize (list "h" "o" "w" " " "a" "r" "e" " " "y" "o" "u" "\n" 
                              "d" "o" "i" "n" "g" "?" "\n" 
                              "a" "n" "y" " " "p" "r" "o" "g" "r" "e" "s" "s" "?"))
              (list "how" "are" "you" "doing" "?" "any" "progress" "?"))

;------------------------------------------------------------------------------
;Exercise 366

;A function Create-Matrix is:
; consumes a number n and a list of n^2 numbers and produces a list of n lists
; of n numbers.
; Number ListOfNumber(LoN) -> [List (LoN)]
(define (create-matrix num lon)
  (local
    (;Number LoN -> LoN
     (define (one-matrixing num lon)
       (cond
         [(or (= 0 num) (empty? lon)) empty]
         [else
          (cons (first lon) (one-matrixing (- num 1) (rest lon)))]
         ))
     ;Number LoN -> LoN
     (define (rest-lon num lon)
       (cond
         [(empty? lon) empty]
         [(= 0 num) lon]
         [else
          (rest-lon (- num 1) (rest lon))]
         ))
     ;Number LoN -> LoN
     (define (matrixing num lon)
       (cond
         [(empty? lon) empty]
         [else
          (cons (one-matrixing num lon) (matrixing num (rest-lon num lon)))]
         ))
    )(matrixing num lon)
    ))

(check-expect (create-matrix 2 (list 1 2 3 4)) (list (list 1 2) (list 3 4)))
(check-expect (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
              (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;--------------------------------------------------------------------------------------------------------
;2. A Ball on a Table

;------------------------------------------------------------------------------
;Exercise 1

;A Ball is Stucture
(define-struct ball (position velocity))
;(make-stucture (position velocity))

;A Position is a Structure
;(define-struct posn (x y))
; (make-posn (Number Number))

;A Velocity is a Structure
(define-struct velocity (horizontal vertical))
; (make-velocity (Number Number))

;------------------------------------------------------------------------------
;Exercise 2

;A Table is a Structure
(define-struct table (posn1 posn2 posn3 posn4))
; (make-table ((make-posn x y) (make-posn x y) (make-posn x y) (make-posn x y)))

(define table1 (make-table (make-posn 100 50) (make-posn 150 100) (make-posn 100 150) (make-posn 50 100)))
(define ball1 (make-ball (make-posn 40 140) (make-velocity 2 2)))
(define ball2 (make-ball (make-posn 90 90) (make-velocity 1 2)))
(define ball3 (make-ball (make-posn 140 60) (make-velocity 2 3)))
(define ball4 (make-ball (make-posn 160 140) (make-velocity 1 1)))
(define ball5 (make-ball (make-posn 60 40) (make-velocity 2 4)))

;A function On-Table? is:
; determines if a Ball is on a Table.
; Ball Table -> Boolean
(define (on-table? ball table)
  (and (p1_p2_n_p3_p4 ball table) (p1_p4_n_p2_p3 ball table)))

;checking the function
(check-expect (on-table? ball1 table1) false)
(check-expect (on-table? ball2 table1) true)
(check-expect (on-table? ball3 table1) false)
(check-expect (on-table? ball4 table1) false)
(check-expect (on-table? ball5 table1) false)

;Ball Table -> Boolean
(define (p1_p2_n_p3_p4 ball table)
  (or (and (>= 0 (- (+ (* (/ (- (posn-y (table-posn1 table)) (posn-y (table-posn2 table)))
                             (- (posn-x (table-posn1 table)) (posn-x (table-posn2 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn1 table))))
                       (posn-y (table-posn1 table)))
                    (posn-y (ball-position ball))))
           (<= 0 (- (+ (* (/ (- (posn-y (table-posn4 table)) (posn-y (table-posn3 table)))
                             (- (posn-x (table-posn4 table)) (posn-x (table-posn3 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn4 table))))
                       (posn-y (table-posn4 table)))
                    (posn-y (ball-position ball)))))
      (and (<= 0 (- (+ (* (/ (- (posn-y (table-posn1 table)) (posn-y (table-posn2 table)))
                             (- (posn-x (table-posn1 table)) (posn-x (table-posn2 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn1 table))))
                       (posn-y (table-posn1 table)))
                    (posn-y (ball-position ball))))
           (>= 0 (- (+ (* (/ (- (posn-y (table-posn4 table)) (posn-y (table-posn3 table)))
                             (- (posn-x (table-posn4 table)) (posn-x (table-posn3 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn4 table))))
                       (posn-y (table-posn4 table)))
                    (posn-y (ball-position ball)))))))

;checking the function
(check-expect (p1_p2_n_p3_p4 ball1 table1) false)
(check-expect (p1_p2_n_p3_p4 ball2 table1) true)
(check-expect (p1_p2_n_p3_p4 ball3 table1) false)
(check-expect (p1_p2_n_p3_p4 ball4 table1) true)
(check-expect (p1_p2_n_p3_p4 ball5 table1) true)

;Ball Table -> Boolean
(define (p1_p4_n_p2_p3 ball table)
  (or (and (>= 0 (- (+ (* (/ (- (posn-y (table-posn1 table)) (posn-y (table-posn4 table)))
                             (- (posn-x (table-posn1 table)) (posn-x (table-posn4 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn1 table))))
                       (posn-y (table-posn1 table)))
                    (posn-y (ball-position ball))))
           (<= 0 (- (+ (* (/ (- (posn-y (table-posn2 table)) (posn-y (table-posn3 table)))
                             (- (posn-x (table-posn2 table)) (posn-x (table-posn3 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn2 table))))
                       (posn-y (table-posn2 table)))
                    (posn-y (ball-position ball)))))
      (and (<= 0 (- (+ (* (/ (- (posn-y (table-posn1 table)) (posn-y (table-posn4 table)))
                             (- (posn-x (table-posn1 table)) (posn-x (table-posn4 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn1 table))))
                       (posn-y (table-posn1 table)))
                    (posn-y (ball-position ball))))
           (>= 0 (- (+ (* (/ (- (posn-y (table-posn2 table)) (posn-y (table-posn3 table)))
                             (- (posn-x (table-posn2 table)) (posn-x (table-posn3 table))))
                          (- (posn-x (ball-position ball)) (posn-x (table-posn2 table))))
                       (posn-y (table-posn2 table)))
                    (posn-y (ball-position ball)))))))

;checking the function
(check-expect (p1_p4_n_p2_p3 ball1 table1) true)
(check-expect (p1_p4_n_p2_p3 ball2 table1) true)
(check-expect (p1_p4_n_p2_p3 ball3 table1) true)
(check-expect (p1_p4_n_p2_p3 ball4 table1) false)
(check-expect (p1_p4_n_p2_p3 ball5 table1) false)

;------------------------------------------------------------------------------
;Exercise 3

;A function Move-Ball is:
; moves a ball by its particular velocity, producing a new ball.
; Ball(Structure) -> Ball(Structure)
(define (move-ball ball)
  (make-ball (make-posn (+ (posn-x (ball-position ball)) (velocity-horizontal (ball-velocity ball)))
                        (+ (posn-y (ball-position ball)) (velocity-vertical (ball-velocity ball))))
             (make-velocity (velocity-horizontal (ball-velocity ball)) (velocity-vertical (ball-velocity ball)))))

;checking the function
(check-expect (move-ball ball1) (make-ball (make-posn 42 142) (make-velocity 2 2)))
(check-expect (move-ball ball2) (make-ball (make-posn 91 92) (make-velocity 1 2)))
(check-expect (move-ball ball3) (make-ball (make-posn 142 63) (make-velocity 2 3)))
(check-expect (move-ball ball4) (make-ball (make-posn 161 141) (make-velocity 1 1)))
(check-expect (move-ball ball5) (make-ball (make-posn 62 44) (make-velocity 2 4)))

;------------------------------------------------------------------------------
;Exercise 4

;A function How-Long is:
; determines how many steps it will take for the given Ball to fall off the
; given Table. If the ball isn't on the table to start with, the answer is 0.
; Ball Table -> Number
(define (how-many ball table)
  (local
    (;Ball Table -> Number
     (define (counting ball table steps)
       (cond
         [(not (on-table? ball table)) steps]
         [else
          (counting (move-ball ball) table (+ steps 1))]
         ))
     )(counting ball table 0)
    ))

;checking the function
(check-expect (how-many ball1 table1) 0)
(check-expect (how-many ball2 table1) 24)
(check-expect (how-many ball3 table1) 0)
(check-expect (how-many ball4 table1) 0)
(check-expect (how-many ball5 table1) 0)

;--------------------------------------------------------------------------------------------------------
;2. Binary Search

;------------------------------------------------------------------------------
;Exercise 5

;A function First-Bad is:
; takes a list of numbers and a function from numbers to booleans, and finds the
; first number in the list where the funtion produces false.
; ListOfNumber Function -> [Number or Boolean]
(define (first-bad lon fn)
  (cond
    [(empty? lon) false]
    [(fn (first lon)) (first-bad (rest lon) fn)]
    [else (first lon)]
    ))

;checking the function
(check-expect (first-bad (list 1 2 3) even?) 1)
(check-expect (first-bad (list 2 4 6) even?) false)

;------------------------------------------------------------------------------
;Exercise 6

;A function Which-Half is:
; takes two numbers, and a function f from numbers to booleans. The first number must
; be smaller than the second. Assumes that the function produces true when applied to
; the first number and false for the second, and that if it produces false when applied
; to some number n, it produces false for every bigger number too. Which-Half should
; produce true if f produces false starting before the mid-point between the two given
; numbers. It should produce false if f produce false starting after the mid-point
; between the two given numbers, or exactly at the mid-point.
; Number Number Function -> Boolean
(define (which-half num1 num2 fn)
  (local
    (;Number Function -> Number
     (define (which-number num fn)
       (cond
         [(fn num) (which-number ((lambda (x) (+ .00001 x)) num) fn)]
         [else num]
         )))
     (if (< (- (which-number num1 fn) num1) (- num2 (which-number num1 fn)))
         true
         false)))

;checking the function
(check-expect (which-half 4 10 (lambda (x) (< x 5))) true)
(check-expect (which-half 4 10 (lambda (x) (< x 9))) false)

;------------------------------------------------------------------------------
;Exercise 7

;A function Smallest-bad is:
; takes the same unputs as Which-Half, but produces the smallest number where f
; produces false. However, if the two numbers are less than 0.0001 apart,
; Smallest-Bad should just produce the larger of the two numbers it is given.
; Number Number Function -> Number
(define (smallest-bad num1 num2 fn)
  (cond
    [(> .0001 (- num2 num1)) num2]
    [else
     (local
       (
        (define (which-number num fn)
          (cond
            [(fn num) (which-number ((lambda (x) (+ .00001 x)) num) fn)]
            [else num]
            ))
        )(which-number num1 fn))]
    ))

;checking the function
(check-expect (smallest-bad 4 8 (lambda (x) (< x 5))) 5)
(check-expect (smallest-bad 4 16 (lambda (x) (< x 10))) 10)

;------------------------------------------------------------------------------
;Exercise 8

;A function Smallest-Bad-A is:
; same as ex7 but four argument which included the difference
; Number Number Function Number -> Number
(define (smallest-bad-a num1 num2 fn diff)
  (cond
    [(> diff (- num2 num1)) num2]
    [else
     (local
       (
        (define (which-number num fn)
          (cond
            [(fn num) (which-number ((lambda (x) (+ .00001 x)) num) fn)]
            [else num]
            ))
        )(which-number num1 fn))]
    ))

;A function Smallest-Bad2 is:
; takes small difference as an input instead of always using 0.0001.
; Number Number Function -> Number
(define (smallest-bad2 num1 num2 fn)
  (smallest-bad-a num1 num2 fn .0001))

;checking the function
(check-expect (smallest-bad2 4 8 (lambda (x) (< x 5))) 5)
(check-expect (smallest-bad2 4 16 (lambda (x) (< x 10))) 10)
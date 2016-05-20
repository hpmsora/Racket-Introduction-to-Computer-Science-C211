;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab_12_Accumulators) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Lab 12: Accumulators

;--------------------------------------------------------------------------------------
;Getting Started

;--------------------------------------------------------------------------
;Exercise 0

;0. sum the numbers in a list of numbers

;[ListOf Number] -> Number
; sums the numbers in the given list

(define (sum lon)
  (local (
          ; [ListOf Number] Number -> Number
          ; adds the numbers in the list one by one to the second argument
          ;
          ; accumulator: remaining-numbers, numbers still left to be processed
          ; accumulator: sum-thus-far, the answer being built iteration by iteration
          ;
          ; example:
          ;   remaining-numbers     sum-thus-far
          ;-------------------------------------------------------------
          ;  (1 2 3 4 5)                 0
          ;    (2 3 4 5)             0 + 1 = 1
          ;      (3 4 5)             1 + 2 = 3
          ;        (4 5)             3 + 3 = 6
          ;          (5)             6 + 4 = 10
          ;        empty            10 + 5 = 15
          ;
          (define (helper remaining-numbers sum-thus-far)
            (cond ((empty? remaining-numbers) ; termination condition
                   sum-thus-far) ; sum-thus-far becomes the final answer
                  (else (helper (rest remaining-numbers) ; fewer numbers remain
                                (+ (first remaining-numbers) ; update sum-thus-far
                                   sum-thus-far)))))
          
          )
    (helper lon 0))) ; initial call to the helper function

(check-expect (sum '(1 2 3 4 5)) (+ 0 1 2 3 4 5))

;--------------------------------------------------------------------------------------
;Prime Factors

;--------------------------------------------------------------------------
;Exercise 1

; example:
;     number       divisor        factors
;----------------------------------------------
;      150           2              empty
;       75           2              (cons 2 empty)
;       75           3              (cons 2 empty)
;       25           3              (cons 3 (cons 2 empty))
;       25           4              (cons 3 (cons 2 empty))
;       25           5              (cons 3 (cons 2 empty))
;       5            5              (cons 5 (cons 3 (cons 2 empty)))
;       1            5              (cons 5 (cons 5 (cons 3 (cons 2 empty))))

;A function Factors-Of is:
; receives an integer and returns all of its prime factors as a list.
; Number -> LoN(ListOfNumber)
(define (factors-of num)
  (local
    (;Number List -> List
     (define (factoring num div lon)
       (cond
         [(= 1 num) lon]
         [else
          (cond
            [(= 0 (remainder num div))
             (factoring (/ num div) div (cons div lon))]
            [else (factoring num (+ 1 div) lon)]
            )]
         )))
    (reverse (factoring num 2 empty)))
  )

;checking the function
(check-expect (factors-of 150) (list 2 3 5 5))
(check-expect (factors-of 41) (list 41))

;--------------------------------------------------------------------------------------
;Prime Numbers

; Number -> Boolean
; is the number prime or not?
(define (prime? number)
  (local ((define factors (factors-of number ))) ; examine number
    (or (= 1 number) ; if it's 1 it is prime
        (equal? (list number) ; if it's its only factor it's prime
                factors))))

;checking the function
(check-expect (map prime? '(1 2 3 4 5 6 7 13 17 19 23))
              '(#t #t #t #f #t #f #t #t #t #t #t))

;--------------------------------------------------------------------------
;Exercise 2

; example:
;      candidate      primes       how-many
;-------------------------------------------------
;                     empty            7
;       1             '(1)
;       2             '(2 1)
;       3             '(3 2 1)
;       4             '(3 2 1)
;       ~             ~

;A function Prime-First is:
; takes a natural number and produces a list of the first that many primes,
; starting with 1.
; Number -> LoN(ListOfNumber)
(define (primes-first num)
  (local
    (;Number LoN Number -> LoN
     (define (priming cand primes how-many)
       (cond
         [(= how-many (length primes)) primes]
         [else
          (cond
            [(prime? cand) (priming (+ 1 cand) (cons cand primes) how-many)]
            [else (priming (+ 1 cand) primes how-many)]
            )]
         ))
     )(reverse (priming 1 empty (+ 1 num)))
    ))

;checking the function
(check-expect (primes-first 0) (list 1))
(check-expect (primes-first 7) (list 1 2 3 5 7 11 13 17))

;--------------------------------------------------------------------------------------
;Buffon Needle Experiment

;--------------------------------------------------------------------------
;Exercise 3

;A function Buffon-Experiment is:
; simulates the Buffon experiment as indicated above and determineds an experimental
; value for pi using this method.
; Number -> Number
(define (buffon-experiment num)
  (local
    (;Number Number Number -> Number
     (define (buffoning hits tries num)
       (cond
         [(= tries num) (/ num hits)]
         [else
          (cond
            [(< 2 (+ (/ (random 2000000000) 1000000000) (sin (* pi (/ (random 1000000000) 1000000000)))))
             (buffoning (+ 1 hits) (+ 1 tries) num)]
            [else (buffoning hits (+ 1 tries) num)]
            )]
         ))
     )(buffoning 0 0 num)
    ))

;running the function
(buffon-experiment 100000)

;--------------------------------------------------------------------------------------
;Binary Digits

;--------------------------------------------------------------------------
;Exercise 4

;A function To-Binary is:
; receives a number and produces all of its binary digits: start with the number,
; calculate the remainder (remainder number 2) and collect it, then replace the
; number with (quotient number 2) and repeat. Keep going until the number becomes 0.
; Number -> String
(define (to-binary num)
  (local
    (;Number String Number
     (define (binaring leftover answer number)
       (cond
         [(= 0 number) "0"]
         [(= 0 leftover) answer]
         [else
          (cond
            [(= 0 (remainder leftover 2))
             (binaring (/ leftover 2) (string-append "0" answer) number)]
            [else
             (binaring (/ (- leftover 1) 2) (string-append "1" answer) number)]
            )]
         ))
     )(binaring num "" num)
    ))

;checking the function
(check-expect (to-binary 0) "0")
(check-expect (to-binary 13) "1101")
(check-expect (to-binary 2543) "100111101111")

;--------------------------------------------------------------------------------------
;Alternating Sum

;--------------------------------------------------------------------------
;Exercise 5

;A function Alternating-Sum is:
; compute the alternating sum of all elements in an array (a list of number).
; List -> Number
(define (alternating-sum lon)
  (local
    (;LoN Number Number
     (define (adding lon current-align sum)
       (cond
         [(empty? lon) sum]
         [else
          (adding (rest lon) (* -1 current-align) (+ (* current-align (first lon)) sum))]
         ))
     )(adding lon 1 0)
    ))

;checking the function
(check-expect (alternating-sum (list 1 4 9 16 9 7 4 9 11)) -2)

;--------------------------------------------------------------------------------------
;Same Elements

;--------------------------------------------------------------------------
;Exercise 6

; [ListOf Number] [ListOf Number] -> Boolean
; determines if the two lists have same elements ignoring duplicates.
(define (same-set lon1 lon2)
  (and (andmap (lambda (x) (> (count-occurrences-in lon1 x) 0)) lon2)
       (andmap (lambda (x) (> (count-occurrences-in lon2 x) 0)) lon1)))

; [ListOf Number] Number -> Natural
; counts occurrences of the number in the list
(define (count-occurrences-in lon number)
  (local
    (;LoN Number Number -> Number
     (define (counting lon count number)
       (cond
         [(empty? lon) count]
         [else
          (cond
            [(equal? (first lon) number) (counting (rest lon) (+ 1 count) number)]
            [else (counting (rest lon) count number)]
            )]
         ))
     )(counting lon 0 number)
    ))

;checking the function
(check-expect (count-occurrences-in '(1 3 2 3 4 5 3 6 3) 3) 4)

;checking the function
(check-expect (same-set '(1 4 9 16 9 7 4 9 11) '(11 11 7 9 16 4 1)) true)

;--------------------------------------------------------------------------------------
;Fibonacci Numbers

;--------------------------------------------------------------------------
;Exercise 7

;A function Fibonacci is:
; receives an integer n and returns the n-th Fibonacci number using the above algorithm.
;Number -> Number
(define (fibonacci number)
  (local
    (;Number Number Number Number -> Number
     (define (calculating fold1 fold2 count number)
       (cond
         [(= count number) (+ fold1 fold2)]
         [else (calculating fold2 (+ fold1 fold2) (+ 1 count) number)]
         ))
     )(calculating 1 1 3 number)
    ))

;checking the function
(check-expect (fibonacci 10) 55)
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 12) 144)
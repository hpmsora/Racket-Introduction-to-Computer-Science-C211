;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Adolf+_+Ha_+Assignment_10_Simultaneous_Processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment 10: Simultaneous Processing

;Kellen Adolf & Wonyong Ha

;-----------------------------------------------------------------------------------
;1. Book Exercise

;------------------------------------------------------------
;Exercise 321

;A function Cross is:
; consumes a list of Strings and a list of Numbers and produces
; all possible ordered paris of symbols and numbers.
; LoS(List) LoN(List) -> List
(define (cross los lon)
  (cond
    [(empty? los) empty]
    [else
     (append (input (first los) lon) (cross (rest los) lon))]
    ))
        
;String LoN -> List
(define (input string lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (cons string (cons (first lon) empty)) (input string (rest lon)))]
    ))

;checking the function
(check-expect (cross empty (list 1 2)) empty)
(check-expect (cross (list "a" "b" "c") empty) empty)
(check-expect (cross (list "a" "b" "c") (list 1 2))
              (list (list "a" 1) (list "a" 2) (list "b" 1) (list "b" 2) (list "c" 1) (list "c" 2)))

;------------------------------------------------------------
;Exercise 323

(define-struct phone-record (name number))
; A Phone-Record is (make-phone-record String String)

;A function Zip is:
; consumes a list of names, represented as strings, and a list
; phone numbers, also strings. It conbines those equally long
; into a list of ohone records.
; LoS LoS -> LoS
(define (zip losn losp)
  (cond
    [(or (empty? losn) (empty? losp)) empty]
    [else
     (cons (make-phone-record (first losn) (first losp)) (zip (rest losn) (rest losp)))]
    ))

;checking the function
(check-expect (zip (list "John" "Tom" "Lee") empty) empty)
(check-expect (zip empty (list "301-295-2353" "454-353-3456" "354-767-2343")) empty)
(check-expect (zip (list "John" "Tom" "Lee") (list "301-295-2353" "454-353-3456" "354-767-2343"))
              (list (make-phone-record "John" "301-295-2353")
                    (make-phone-record "Tom" "454-353-3456")
                    (make-phone-record "Lee" "354-767-2343")))

;------------------------------------------------------------
;Exercise 327

;A function Merge is:
; consumes two lists of Numbers, sorted in ascending order. It
; produces a single sorted list of numbers that contains all the
; numbers on both inputs list. A number occurs in the output as
; many times as it occurs on the two input lists together.
; LoN LoN -> LoN
(define (merge lon1 lon2)
  (sort (append (sort lon1 <) (sort lon2 <)) <))

;checking the function
(check-expect (merge empty (list 34 6 4 53 26)) (list 4 6 26 34 53))
(check-expect (merge (list 3 5 2 5 3 21) empty) (list 2 3 3 5 5 21))
(check-expect (merge (list 3 5 2 5 3 21) (list 34 6 4 53 26))
              (list 2 3 3 4 5 5 6 21 26 34 53))

;------------------------------------------------------------
;Exercise 331

;A function Value is:
; consumes two equally long list: a linear combination and a list
; of variable values. It produces the value of the combination for
; these values
; LoN LoN -> Number
(define (value lon1 lon2)
  (cond
    [(or (empty? lon1) (empty? lon2)) 0]
    [else (+ (* (first lon1) (first lon2)) (value (rest lon1) (rest lon2)))]
    ))

;checking the function
(check-expect (value empty empty) 0)
(check-expect (value (list 1 2 3 4) (list 5 6 7 8)) 70)

;-----------------------------------------------------------------------------------
;2. DNA Prefixes

;------------------------------------------------------------
;Exercise 1

;Nucleotide is a String
; Nucleotide is one of:
; - A
; - C
; - G
; - T

;DNA-Strand is a List
; DNA-Strand is one of:
; - empty
; - (cons Nucleoride DNA-Strand)

;------------------------------------------------------------
;Exercise 2

;A function DNAPrefix is:
; takes two Strands and determines if the initial part of the first
; Strand is identical to the second Strand.
; DNA-Strand DNA-Strand -> Boolean
(define (dnaprefix dnas1 dnas2)
  (cond
    [(empty? dnas2) true]
    [(empty? dnas1) false]
    [(string=? (first dnas1) (first dnas2)) (dnaprefix (rest dnas1) (rest dnas2))]
    [else false]
    ))

;checking the function
(check-expect (dnaprefix empty (list "a" "g" "a")) false)
(check-expect (dnaprefix (list "a" "g" "a" "c") empty) true)
(check-expect (dnaprefix (list "a" "g" "a" "c") (list "a" "g" "a")) true)
(check-expect (dnaprefix (list "a" "g" "t") (list "a" "g" "a" "c" "a" "t")) false)

;------------------------------------------------------------
;Exercise 3

;A function DNAPrefix-overlap is:
; takes two Strands and returns the Strand representing the shared
; prefix of hte given Strands.
; DNA-Strand DNA-Strand -> DNA-Strand
(define (dnaprefix-overlap dnas1 dnas2)
  (cond
    [(or (empty? dnas1) (empty? dnas2)) empty]
    [(string=? (first dnas1) (first dnas2))
     (cons (first dnas1) (dnaprefix-overlap (rest dnas1) (rest dnas2)))]
    [else empty]
    ))

;checking the function
(check-expect (dnaprefix-overlap empty (list "a" "g" "c" "a" "t")) empty)
(check-expect (dnaprefix-overlap (list "a" "g" "a") empty) empty)
(check-expect (dnaprefix-overlap (list "a" "g" "a") (list "a" "g" "c" "a" "t"))
              (list "a" "g"))

;------------------------------------------------------------
;Exercise 4

;A function DNASearch is:
; takes two Strands and returns true if the first Strand appears anywhere
; in the second Strand (not just at the beginning).
; DNA-Strand DNA-Strand -> Boolean
(define (dnasearch dnas1 dnas2)
  (cond
    [(or (empty? dnas1) (empty? dnas2)) false]
    [(not (dnaprefix dnas2 dnas1)) (dnasearch dnas1 (rest dnas2))]
    [else true]
    ))

;checking function
(check-expect (dnasearch empty (list "g" "c" "a" "g" "t")) false)
(check-expect (dnasearch (list "a" "g") empty) false)
(check-expect (dnasearch (list "a" "g") (list "g" "c" "a" "g" "t")) true)
(check-expect (dnasearch (list "a" "g") (list "g" "c" "c" "g" "t")) false)

;-----------------------------------------------------------------------------------
;3. Commutative Tree

(define-struct bnode [left right])
(define-struct leaf [value])
;A [Commutative-Binary-Tree-of X] ([CBTo X] is one of:
; - (make-leaf X)
; - (make-bnode [CBTo X] [CBTo Y])
;Interpretation: Two commutative binary trees are considered equal if
; they have equal subtrees (regardless of order) and the leaves have
; equal values (according to equal?)

;------------------------------------------------------------
;Exercise 5

;First Picture
(define example1 (make-bnode (make-bnode (make-leaf 1)
                                         (make-bnode (make-leaf 2) (make-leaf 3)))
                             (make-leaf 4)))
;Second Picture
(define example2 (make-bnode (make-leaf 4)
                             (make-bnode (make-leaf 1)
                                         (make-bnode (make-leaf 3) (make-leaf 2)))))
;Third Picture
(define example3 (make-bnode (make-leaf 1)
                             (make-bnode (make-leaf 2)
                                         (make-bnode (make-leaf 3) (make-leaf 4)))))

;------------------------------------------------------------
;Exercise 6

;A function CBT=? is:
; determines whether two examples of a [Commutative-Binary-Tree-Of X]
; are equal or not.
; [Commutative-Binary-Tree-Of X] -> Boolean
(define (cbt=? tree1 tree2)
  (cond
    [(and (leaf? tree1) (leaf? tree2)) (= (leaf-value tree1) (leaf-value tree2))]
    [(and (bnode? tree1) (bnode? tree2))
     (or (and (cbt=? (bnode-left tree1) (bnode-left tree2))
              (cbt=? (bnode-right tree1) (bnode-right tree2)))
         (and (cbt=? (bnode-left tree1) (bnode-right tree2))
              (cbt=? (bnode-right tree1) (bnode-left tree2))))]
    [else false]
    ))

;checking the function
(check-expect (cbt=? example1 example2) true)
(check-expect (cbt=? example2 example3) false)
(check-expect (cbt=? example1 example3) false)

;------------------------------------------------------------
;Exercise 7

;A Branch is a List
; Branch is one of:
; - empty
; - Leaf
; - Branch
; Branch cannot be just empty list.

;------------------------------------------------------------
;Exercise 8

(define example1-a (list (list (make-leaf 1)
                               (make-leaf 2)
                               (list (make-leaf 4) (make-leaf 5) (make-leaf 6)))
                         (list (make-leaf 7) (make-leaf 8) (make-leaf 9))
                         (list (make-leaf 10) (make-leaf 11) (make-leaf 12) (make-leaf 13))))
(define example2-a (list (list (make-leaf 8) (make-leaf 9) (make-leaf 7))
                         (list (make-leaf 2)
                               (list (make-leaf 5) (make-leaf 6) (make-leaf 4))
                               (make-leaf 1))
                         (list (make-leaf 10) (make-leaf 12) (make-leaf 13) (make-leaf 11))))
(define example3-a (list (list (make-leaf 2) (make-leaf 5))
                         (list (list (make-leaf 4)) (make-leaf 9))
                         (list (make-leaf 2))))
(define example4-a (list (list (make-leaf 2) (make-leaf 5))
                         (list (list (make-leaf 4)) (make-leaf 2))
                         (list (make-leaf 2))))
(define example5-a (list (list (make-leaf 5) (make-leaf 2))
                         (list (list (make-leaf 4)) (make-leaf 2))
                         (list (make-leaf 2))))


;------------------------------------------------------------
;Exercise 9

;A function CT=? is:
; determines whether two examples of a CT-X are equal or not.
; CT-X CT-X -> Boolean
(define (ct=? tree1 tree2)
  (cond
    [(and (leaf? tree1) (leaf? tree2)) (= (leaf-value tree1) (leaf-value tree2))]
    [(and (list? tree1) (list? tree2))
     (local
       (;standard normal tree2
        (define standard tree2)
         ; CT CT -> Boolean
        (define (same? tr1 tr2)
          (cond
            [(ct=? (first tr1) (first tr2))
             (cond
               [(empty? (rest tr1)) true]
               [else (same? (rest tr1) standard)]
               )]
            [(empty? (rest tr2)) false]
            [else (same? tr1 (rest tr2))]
            )))
       (same? tree1 tree2))]
    [else false]
    ))

;checking the function
(check-expect (ct=? example1-a example2-a) true)
(check-expect (ct=? example2-a example1-a) true)
(check-expect (ct=? example1-a example3-a) false)
(check-expect (ct=? example2-a example3-a) false)
(check-expect (ct=? example3-a example4-a) false)
(check-expect (ct=? example4-a example5-a) true)
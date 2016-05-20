;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment_7_Abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Assignment 8: Abstraction

;1. Book Exercises

;--------------------------------------------------------------------
;Exercise 201

;A function Contains? is:
; determines whether ListOfString(LoS) contains the string s
; String LoS -> Boolean
(define (contains? str los)
  (cond
    [(empty? los) false]
    [else (or (string=? (first los) str)
              (contains? str (rest los)))]
    ))

;A function contains-atom? is:
; does LoS contain "atom"
; LoS -> Boolean
(define (contains-atom? los)
  (contains? "atom" los))

;checking the function
(check-expect (contains-atom? (list "dog" "cat" "atom")) true)
(check-expect (contains-atom? (list "zoo" "basic")) false)

;A function contains-basic? is:
; does LoS contain "basic"
; LoS -> Boolean
(define (contains-basic? los)
  (contains? "basic" los))

;checking the function
(check-expect (contains-basic? (list "dog" "basic" "atom")) true)
(check-expect (contains-basic? (list "zoo" "atom")) false)

;A function contains-zoo? is:
; does LoS contain "zoo"
; LoS -> Boolean
(define (contains-zoo? los)
  (contains? "zoo" los))

;chekcing the function
(check-expect (contains-zoo? (list "cat" "atom" "zoo")) true)
(check-expect (contains-zoo? (list "atom" "dog")) false)

;--------------------------------------------------------------------
;Exercise 202

;A function Add1* is:
; add 1 to each number on ListOfNumber(LoN)
; LoN -> LoN
(define (add1* lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (add1 (first lon))
           (add1* (rest lon)))]
    ))

;checking the funtion
(check-expect (add1* empty) empty)
(check-expect (add1* (list 4 2 5 2 4)) (list 5 3 6 3 5))
(check-expect (add1* (list 2 5 3 5)) (list 3 6 4 6))

;A function Plus5 is:
; adds 5 to each number on lon
; LoN -> LoN
(define (plus5 lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (+ (first lon) 5)
           (plus5 (rest lon)))]
    ))

;checking the function
(check-expect (plus5 empty) empty)
(check-expect (plus5 (list 3 5 3 2 5)) (list 8 10 8 7 10))
(check-expect (plus5 (list 2 7 2)) (list 7 12 7))

;--------------------------------------------------------------------
;Exercise 206

;A function Inf is:
; determines the smallest
; LoN -> Number on LoN
(define (inf lon)
  (cond
    [(empty? (rest lon))
     (first lon)]
    [else
     (cond
       [(< (first lon) (inf (rest lon)))
        (first lon)]
       [else
        (inf (rest lon))]
       )]
    ))

;checking the function
(check-expect (inf (list 3 4 7 6 4 8)) 3)
(check-expect (inf (list 3 5 9 1 4 6)) 1)

;A fuction Sup is:
; determines the largest
; LoN -> Number on LoN
(define (sup lon)
  (cond
    [(empty? (rest lon))
     (first lon)]
    [else
     (cond
       [(> (first lon) (sup (rest lon)))
        (first lon)]
       [else
        (sup (rest lon))]
       )]
    ))

;checking the function
(check-expect (sup (list 3 4 7 6 4 8)) 8)
(check-expect (sup (list 3 5 9 1 4 6)) 9)

;A function Abs-Finding is:
; abstracting the following functions into a single function.
; [x -> y] LoN -> Number on LoN
(define (abs-finding fn lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else
     (cond
       [(fn (first lon) (abs-finding fn (rest lon))) (first lon)]
       [else (abs-finding fn (rest lon))]
       )]
    ))

;re-define the Inf and Sup function
(define (inf-1 lon)
  (abs-finding < lon))

(define (sup-1 lon)
  (abs-finding > lon))

;checking the function
(check-expect (inf-1 (list 3 4 7 6 4 8)) 3)
(check-expect (inf-1 (list 3 5 9 1 4 6)) 1)
(check-expect (sup-1 (list 3 4 7 6 4 8)) 8)
(check-expect (sup-1 (list 3 5 9 1 4 6)) 9)

;--------------------------------------------------------------------
;Exercise 214

;A function Sum is:
; computes the sum of the numbers on LoN(ListOfNumber)
; LoN -> Number
(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else
      (+ (first lon) (sum (rest lon)))]
    ))

;checking the function
(check-expect (sum (list 5 3 6 3 5)) 22)
(check-expect (sum (list 3 5 4)) 12)

;A function Product is:
; computes the product of the numbers on LoN(ListOfNumber)
; LoN -> Number
(define (product lon)
  (cond
    [(empty? lon) 1]
    [else
      (* (first lon) (product (rest lon)))]
    ))

;checking the function
(check-expect (product (list 5 3 6 3 5)) 1350)
(check-expect (product (list 3 5 4)) 60)

;A function Fold1 is:
; abstracting the Sum and Product functions.
; LoN -> Number
(define (fold1 fn N lon)
  (cond
    [(empty? lon) N]
    [else
     (fn (first lon) (fold1 fn N (rest lon)))]
    ))

;re-define the Sum and Product functions
(define (re-sum lon)
  (fold1 + 0 lon))

(define (re-product lon)
  (fold1 * 1 lon))

;checking the function
(check-expect (re-sum (list 5 3 6 3 5)) 22)
(check-expect (re-sum (list 3 5 4)) 12)
(check-expect (re-product (list 5 3 6 3 5)) 1350)
(check-expect (re-product (list 3 5 4)) 60)

;--------------------------------------------------------------------
;Exercise 215

(require 2htdp/image)

;Fixed Variable
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

;Example of List of Posn(LoS)
(define los1 (list (make-posn 43 53) (make-posn 35 63)))
(define los2 (list (make-posn 5 23)))

;A function Product is:
; computes the product of the numbers on LoN(ListOfNumber)
; LoN -> Number
; defined Exercise 214

;A function Image* is:
; getting the Posn(position) and make a image
; LoS -> Image
(define (image* los)
  (cond
    [(empty? los) emt]
    [else
     (place-dot (first los)
                (image* (rest los)))]
    ))

;A function Place-Dot is:
; getting Posn(position) and Image and make a image that contains the
; dot that its assigned position.
; Posn Image -> Image
(define (place-dot p img)
  (place-image dot
               (posn-x p) (posn-y p)
               img))

;checking the function
(check-expect (image* los1) (place-image dot 43 53
                                         (place-image dot 35 63
                                                      (empty-scene 100 100))))
(check-expect (image* los2) (place-image dot 5 23
                                         (empty-scene 100 100)))

;A function Fold2 is:
; abstracting the Product and Image* functions
; LoN LoS -> Number or Image
(define (fold2 fn N listing)
  (cond
    [(empty? listing) N]
    [else
     (fn (first listing)
         (fold2 fn N (rest listing)))]
    ))

;re-define the Product and Image* functions
(define (fold2-product lon)
  (fold2 * 1 lon))

(define (fold2-image* los)
  (fold2 place-dot emt los))

;checking the function
(check-expect (fold2-product (list 5 3 6 3 5)) 1350)
(check-expect (fold2-product (list 3 5 4)) 60)
(check-expect (fold2-image* los1) (place-image dot 43 53
                                         (place-image dot 35 63
                                                      (empty-scene 100 100))))
(check-expect (fold2-image* los2) (place-image dot 5 23
                                         (empty-scene 100 100)))

;--------------------------------------------------------------------
;2. Abstracting Trees

;A NumBT in one of:
; - (make-leaf Number)
(define-struct leaf (num))
; - (make-node NumBT Number Number)
(define-struct node (numbt1 num numbt2))

(define tree1 (make-node (make-leaf 5) 4 (make-leaf 3)))
(define tree2 (make-node (make-node (make-leaf 2) 3 (make-leaf 5)) 2 (make-leaf 5)))

;--------------------------------------------------------------------
;Exercise 1

;A function Sum-Tree is:
; adding up all the elements of a NumBT.
; NumBT -> Number
(define (sum-tree numbt)
  (cond
    [(empty? numbt) 0]
    [(leaf? numbt) (leaf-num numbt)]
    [(node? numbt)
     (+ (node-num numbt) (sum-tree (node-numbt1 numbt)) (sum-tree (node-numbt2 numbt)))]
    ))

;checking the function
(check-expect (sum-tree empty) 0)
(check-expect (sum-tree tree1) 12)
(check-expect (sum-tree tree2) 17) 

;A function Prod-Tree is:
; multipilying together allof the elements
; NumBT -> Number
(define (prod-tree numbt)
  (cond
    [(empty? numbt) 0]
    [(leaf? numbt) (leaf-num numbt)]
    [(node? numbt)
     (* (node-num numbt) (prod-tree (node-numbt1 numbt)) (prod-tree (node-numbt2 numbt)))]
    ))

(check-expect (prod-tree empty) 0)
(check-expect (prod-tree tree1) 60)
(check-expect (prod-tree tree2) 300)

;--------------------------------------------------------------------
;Exercise 2

;A function OP-Tree is:
; abstracting Sum-Tree and Prod-Tree into a single function
; [X -> Y] NumBT -> Number
(define (op-function fn numbt)
  (cond
    [(empty? numbt) 0]
    [(leaf? numbt) (leaf-num numbt)]
    [(node? numbt)
     (fn (node-num numbt) (op-function fn (node-numbt1 numbt)) (op-function fn (node-numbt2 numbt)))]
    ))

;Redefine the Sum-Tree and Pord-Tree
(define (re-sum-tree numbt)
  (op-function + numbt))

(define (re-prod-tree numbt)
  (op-function * numbt))

;checking the function
(check-expect (re-sum-tree empty) 0)
(check-expect (re-sum-tree tree1) 12)
(check-expect (re-sum-tree tree2) 17)
(check-expect (re-prod-tree empty) 0)
(check-expect (re-prod-tree tree1) 60)
(check-expect (re-prod-tree tree2) 300)

;--------------------------------------------------------------------
;Exercise 3

;A function Count-Tree is:
; counting how many leaves there are in the tree.
; NumBT -> Number
(define (count-tree numbt)
  (cond
    [(empty? numbt) 0]
    [(leaf? numbt) (leaf-num numbt)]
    [(node? numbt)
     (+ (count-tree (node-numbt1 numbt)) (count-tree (node-numbt2 numbt)))]
    ))

;checking the function
(check-expect (count-tree empty) 0)
(check-expect (count-tree tree1) 8)
(check-expect (count-tree tree2) 12)

;--------------------------------------------------------------------
;Exercise 4

;A function Process-Tree is:
; abstracting Sum-Tree, Prod-Tree, and Count-Tree into a single function.
; [x -> y] [x -> y] NumBT -> Number
(define (process-tree fn N numbt)
  (cond
    [(empty? numbt) 0]
    [(leaf? numbt) (leaf-num numbt)]
    [(node? numbt)
     (fn (N 0  (node-num numbt)) (process-tree fn N (node-numbt1 numbt)) (process-tree fn N (node-numbt2 numbt)))]
    ))

;redefine the Sum-Tree, Prod-Tree, and Count-Tree
(define (re-re-sum-tree numbt)
  (process-tree + + numbt))

(define (re-re-prod-tree numbt)
  (process-tree * + numbt))

(define (re-re-count-tree numbt)
  (process-tree + * numbt))

;checking the function
(check-expect (re-re-sum-tree empty) 0)
(check-expect (re-re-sum-tree tree1) 12)
(check-expect (re-re-sum-tree tree2) 17)
(check-expect (re-re-prod-tree empty) 0)
(check-expect (re-re-prod-tree tree1) 60)
(check-expect (re-re-prod-tree tree2) 300)
(check-expect (re-re-count-tree empty) 0)
(check-expect (re-re-count-tree tree1) 8)
(check-expect (re-re-count-tree tree2) 12)

;--------------------------------------------------------------------
;Exercise 5

;A [BT X] is one of:
; - (make-leaf Number)
; - (make-node [BT X] Number [BT X]

;--------------------------------------------------------------------
;Exercise 6

;No Dont need
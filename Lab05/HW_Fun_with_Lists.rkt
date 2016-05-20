;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Fun_with_Lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; A ManyPosn is one of:
; - 0
; - (make-many Posn ManyPosn)
(define-struct many (posn many))

; A Posn is
; (make-posn Number Number)
; Note: the posn structure is defined in Beginning Student, and looks like:
; (define-struct posn (x y))

;Examples of ManyPosn
; -0
(define example1 (make-many 0 empty))
; -one inside
(define example2 (make-many (make-posn 12 142) empty))
; -two inside
(define example3 (make-many (make-posn 102 45) (make-many (make-posn 43 111) empty)))
; -one false (posn-x = negative value) inside
(define example4 (make-many (make-posn -25 1) empty))
; -two false (posn-x = negative value) inside
(define example5 (make-many (make-posn 123 155) (make-many (make-posn -122 53) empty)))

; A function Many-Positive? is
; consumes a ManyPson and produces "true" if the x-coordinates of all of the Posns in the
; ManyPosn are positive. If there aren't any Posns in the ManyPosn, then they are all positive.
; Structure(Field) -> Boolean
(define (many-positive? field)
  (cond
    [(posn? field) (< 0 (posn-x field))]
    [(posn? (many-posn field))
     (cond
       [(empty? (many-many field)) (many-positive? (many-posn field))]
       [(many? (many-many field))
        (cond
          [(many-positive? (many-posn field)) (many-positive? (many-many field))]
          [else false]
          )])]
    [(number? (many-posn field)) true]
    [else false]))

;chekcing functions
(check-expect (many-positive? example1) true)
(check-expect (many-positive? example2) true)
(check-expect (many-positive? example3) true)
(check-expect (many-positive? example4) false)
(check-expect (many-positive? example5) false)

; A list ListOfPosn (LoP) is:
; revised the data definition from ManyPosn to use lists.
; A ListOfPosn (LoP) is one of:
; - empty
; - (cons Number LoP)

;Examples of ListOfPosn
; -0
(define example_list1 (list 0))
; -one inside
(define example_list2 (list (make-posn 12 124)))
; -two inside
(define example_list3 (list (make-posn 12 124) (make-posn 42 199)))
; -three inside
(define example_list4 (list (make-posn 44 34) (make-posn 59 111) (make-posn 134 155)))
; -one false (posn-x = negative value) inside
(define example_list5 (list (make-posn -23 12)))
; -two false (posn-x = negative value) inside
(define example_list6 (list (make-posn 155 53) (make-posn -2 45)))

; A function All-Positive? is
; consumes a ListOfPosn and produces "true" if the x-coordinates of all of the Posns in the
; ManyPosn are positive. If there aren't any Posns in the ManyPosn, then they are all positive.
; List -> Boolean
(define (all-positive? a-list)
  (cond
    [(posn? a-list) (< 0 (posn-x a-list))]
    [(posn? (car a-list))
     (cond
       [(empty? (cdr a-list)) (all-positive? (first a-list))]
       [(cons? (cdr a-list))
        (cond
          [(all-positive? (first a-list)) (all-positive? (rest a-list))]
          [else false]
          )]
       [else false]
       )]
    [(number? (car a-list)) true]
    ))

;checking the function
(check-expect (all-positive? example_list1) true)
(check-expect (all-positive? example_list2) true)
(check-expect (all-positive? example_list3) true)
(check-expect (all-positive? example_list4) true)
(check-expect (all-positive? example_list5) false)
(check-expect (all-positive? example_list6) false)

; A function Draw is:
; consuming a ListOfPosn and draws all the Posns on an empty scene.
; List -> Images
(define (draw a-list)
  (cond
    [(posn? a-list) (place-image(circle 10 "solid" "black")
                                (posn-x a-list) (posn-y a-list)
                                (empty-scene 200 200))]
    [(posn? (car a-list))
     (cond
       [(empty? (cdr a-list)) (draw (car a-list))]
       [(cons? (cdr a-list))
                    (place-image(circle 10 "solid" "black")
                                (posn-x (car a-list)) (posn-y (car a-list))
                                (draw (rest a-list)))]
       )]
    [(or (empty? a-list) (number? (car a-list))) (empty-scene 200 200)]
    ))

;chekcing the function
(check-expect (draw example_list1) (empty-scene 200 200))
(check-expect (draw example_list2) (place-image(circle 10 "solid" "black")
                                               12 124 (empty-scene 200 200)))

; A function Move is:
; consuming a ListOfPosn and produces a ListOfPosn where all of the y-coordinates
; have increased by 1
; List -> Images
(define (move current-dots)
  (cond
    [(empty? current-dots) empty]
    [(cons? current-dots)
     (cons (move-dot (first current-dots))
     (move (rest current-dots)))]
    ))

; A sub-function for Move is:
; adding 1 to each circles' y-coordinates.
; List -> List
(define (move-dot a-dot)
  (make-posn (posn-x a-dot) (+ (posn-y a-dot) 1)))


; A function Add is:
; consuming a ListOfPosn and a KeyEvent and produces a ListOfPosn with one more Posn added.
; List, String -> List
(define (add s ks)
  (cons (make-posn (random 201) (random 201)) s)
    )

; A function Big-Bang is:
; using Draw, Move and Add function to create an animation.
(big-bang (list (make-posn 44 34) (make-posn 59 111) (make-posn 134 155))
          ;upper list is example_list4
          (on-key add)
          (to-draw draw)
          (on-tick move)
          )
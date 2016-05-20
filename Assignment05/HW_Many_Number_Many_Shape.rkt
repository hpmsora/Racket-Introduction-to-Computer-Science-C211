;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Many_Number_Many_Shape) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;A data definition ManyNumbers can store arbitrarily many numbers
;A ListOfNumber (LoN) is one of:
; - empty
; - (cons Number LoN)
(define ManyNumbers (list 23 143 53 124 124 26 134 164))

;A data and structure definition for storing pairs of numbers
; representing x and y coordinates
;(define-struct posn [x y])
;An Posn is a stucture:
; (make-posn Number Number)

;A data definition ManyPairs can store arbitrarily many pairs of
;numbers using the data definition from the previous exercise
(define ManyPair (list (make-posn (first ManyNumbers) (second ManyNumbers))
                       (make-posn (third ManyNumbers) (fourth ManyNumbers))
                       (make-posn (fifth ManyNumbers) (sixth ManyNumbers))
                       (make-posn (seventh ManyNumbers) (eighth ManyNumbers))))

;ListOfPosns (LoP) is one of:
; - empty
; - (cons LoP)
(define (add-dot current-dots mouse-x mouse-y me)
  (cond
    [(string=? "button-down" me) (cons (make-posn mouse-x mouse-y) current-dots)]
    [else current-dots]
    ))

;chekcing the function
(check-expect (add-dot empty 50 75 "button-down") (cons (make-posn 50 75) empty))
(check-expect (add-dot empty 50 75 "move") empty)
(check-expect (add-dot (cons (make-posn 50 75) empty) 120 66 "button-down")
              (cons (make-posn 120 66) (cons (make-posn 50 75) empty)))

;function draws all of the points as circles with radius 10 on the same empty
;scene, producig the resulting image
; LoP -> Image
;empty-scene 200x200 pixels
(define (draw-dots lop)
  (cond
    [(empty? lop) (empty-scene 200 200)]
    [(cons? lop) (place-image (circle 10 "solid" "black") (posn-x (first lop)) (posn-y (first lop))
                              (place-image (circle 10 "solid" "black") (posn-x (second lop)) (posn-y (second lop))
                                           (place-image (circle 10 "solid" "black") (posn-x (third lop)) (posn-y (third lop))
                                                        (place-image (circle 10 "solid" "black") (posn-x (fourth lop)) (posn-y (fourth lop)) (empty-scene 200 200)))))]
    ))

(define (draw-dots_point lop)
  (cond
    [(empty? lop) (empty-scene 200 200)]
    [(cons? lop) (place-image (circle 10 "solid" "black") (posn-x (first lop)) (posn-y (first lop)) (empty-scene 200 200))]
    ))

;checking the function
(check-expect (draw-dots empty) (empty-scene 200 200))

;function creates a larger ManyPairs with all of the coordinates plus
;the new pair of coordinates
; posn -> LoP
(define (add_pair a-list a-posn)
  (append a-list (list (posn-x a-posn) (posn-y a-posn))))

;checking the function
(check-expect (add_pair (list 2 3 4 5) (make-posn 25 42)) (list 2 3 4 5 25 42))

;on-mouse function definition

(big-bang (list (make-posn 100 100))
          (on-mouse add-dot)
          (to-draw draw-dots_point)
          )
  
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab_3_Structures_and_Enumeration) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Exercise #1

;A photo is a structure
(define-struct photo (image tag))
; (make-photo Image String)
; To determine the variable is a photo:
;  (photo? variable) -> Boolean
; (photo-image variable) -> Image
; (photo-tage variable) -> String

;A 3D is a structure
(define-struct 3d (x y z))
; (make-3d Number Number Number)
; To determine the variable is a 3d:
;  (3d? variable) -> Boolean
; (3d-x variable) -> Number
; (3d-y variable) -> Number
; (3d-z variable) -> Number

;Exercise #2 #3 #4 #5 #6

(define-struct item (tag price))
;An Item is a structure:
; (make-item String PositiveNumber)

(define iex1 (make-item "coke" 2))
(define iex2 (make-item "cheese burger" 4))
;structure item example

;field definition
(define-struct a-field (field))
;An a-field is a structure
; (make-a-field field)

(define-struct AI (name field pay-rate))
;An AI is (represented by)a structure:
; (make-AI String Field PositiveNumber)

(define Aex1 (make-AI "biology" a-field 2))
(define Aex2 (make-AI "english" a-field 4))
(define Aex3 (make-AI "computer science" a-field 6))
(define Aex4 (make-AI "business" a-field 1))
;A Field is one of:
; - "biology"
; - "english"
; - "computer science"
; - "business"

;field definition
;field -> Number
;field-multiplier returns the multiplier for a bonus appropriate to the given field
(define (field-multiplier a-field)
  (cond
    [(or (string=? "biology" a-field) (string=? "english" a-field)) 2]
    [(string=? "computer science" a-field) 4]
    [(string=? "business" a-field) 3]
    ))

;check the function of field
(check-expect (field-multiplier "biology") 2)
(check-expect (field-multiplier "english") 2)
(check-expect (field-multiplier "computer science") 4)
(check-expect (field-multiplier "business") 3)

(define-struct cat [name species age feeding_time])
;A cat is a structure:
; (make-cat String String PositiveNumber PositiveNumber)

(define ex1 (make-cat "male_tigers" "tiger" 4 4))
(define ex2 (make-cat "female_tigers" "tiger" 5 4))
(define ex3 (make-cat "male_lion" "lion" 2 4))
(define ex4 (make-cat "female_lion" "tiger" 6 4))
(define ex5 (make-cat "male_leopard" "leopard" 2 5))
(define ex6 (make-cat "female_leopard" "leopard" 1 5))
;name: String, species: String, age: Numbers, feeding_time: Numbers
;structure cat examples

;Exercise #7

;A function Pay-Raise is:
; consuming two pieces of data(an AI and a number) and producing a new AI whose Pay-Raise is
; multiplied by the given number.
; AI(structure) Number -> AI(structure)
(define (pay-raise an-AI a-number)
  (make-AI (AI-name an-AI) (AI-field an-AI) (* a-number (AI-pay-rate an-AI))))

;checking the pay-raise function
(check-expect (pay-raise (make-AI "Sam" "english" 5) 10) (make-AI "Sam" "english" 50))
(check-expect (pay-raise (make-AI "John" "business" 4) 20) (make-AI "John" "business" 80))
(check-expect (pay-raise (make-AI "Bob" "computer science" 3) 40) (make-AI "Bob" "computer science" 120))

;Exercise #8

;A function Bonus is:
; consuming one piece of data(an AI) and producing a number which is twice the pay rate if the
; AI is in English or Biology, three times the pay rate if the AI is in Business, and four times
; the pay rate if the AI is in compuer science.
; AI(structure) -> Number
(define (bonus an-AI)
  (* (field-multiplier (AI-field an-AI)) (AI-pay-rate an-AI)))

;chekcing the bonus function
(check-expect (bonus (make-AI "Erik" "computer science" 10)) 40)
(check-expect (bonus (make-AI "Jackson" "english" 20)) 40)

;Moving Shapes

(require 2htdp/image)
(require 2htdp/universe)

;Exercise 9

;Data definition of ball structure:
; (make-ball PositiveNumber PositiveNumber PositiveNumber)
(define-struct ball (radius x-pos y-pos color))

;Data definition of box structure:
;(make-box PositiveNumber PositiveNumber PositiveNumber PositiveNumber)
(define-struct box (width height x-pos y-pos color))

;Data definition of tiangle structure:
;(make-triangle PositiveNumber PositiveNumber PositiveNumber)
(define-struct pointy (side x-pos y-pos color))

; Draw shape-draws the shape, a-shape as a solid turquoise shape on a 200 x 200 scene
(place-image (triangle 20 "solid" "red")
             40 40
             (empty-scene 200 200))

;Exercise #10

;drawing shape
(define (draw-shape a-shape)
  (cond
    [(ball? a-shape) (place-image (circle (ball-radius a-shape) "solid" (ball-color a-shape))
                                 (ball-x-pos a-shape) (ball-y-pos a-shape) (empty-scene 200 200))]
    [(box? a-shape) (place-image (rectangle (box-width a-shape) (box-height a-shape) "solid" (box-color a-shape))
                                 (box-x-pos a-shape) (box-y-pos a-shape) (empty-scene 200 200))]
    [(pointy? a-shape) (place-image (triangle (pointy-side a-shape) "solid" (pointy-color a-shape))
                                 (pointy-x-pos a-shape) (pointy-y-pos a-shape) (empty-scene 200 200))]
    ))

;checking the function
(check-expect (draw-shape (make-ball 20 50 100 "red"))
              (place-image (circle 20 "solid" "red") 50 100 (empty-scene 200 200)))
(check-expect (draw-shape (make-box 20 50 100 24 "blue"))
              (place-image (rectangle 20 50 "solid" "blue") 100 24 (empty-scene 200 200)))
(check-expect (draw-shape (make-pointy 20 30 100 "black"))
              (place-image (triangle 20 "solid" "black") 30 100 (empty-scene 200 200)))

;Exercise #11

; This function makes either a box or a ball that is one unit bigger, it does draw anything.
(define (move-shape s)
  (cond
    [(ball? s) (make-ball (ball-radius s) (+ (ball-x-pos s) 1) (+ (ball-y-pos s) 1) (ball-color s))]
    [(box? s) (make-box (box-width s) (box-height s) (+ (box-x-pos s) 1) (+ (box-y-pos s) 1) (box-color s))]
    [(pointy? s) (make-pointy (pointy-side s) (+ (pointy-x-pos s) 1) (+ (pointy-y-pos s) 1) (pointy-color s))]
    ))

;checking the function
(check-expect (move-shape (make-ball 15 22 77 "red"))
              (make-ball 15 23 78 "red"))
(check-expect (move-shape (make-box 12 3 100 101 "blue"))
              (make-box 12 3 101 102 "blue"))
(check-expect (move-shape (make-pointy 18 33 98 "black"))
              (make-pointy 18 34 99 "black"))

;Exercise #12

;Define function that will take a shape and key as input and will reset the world state. It will output the initial state.
(define (reset s ks)
  (make-pointy 20 40 40 "black"))

;checking the function
(check-expect (reset (make-pointy 50 22 77 "black") "q") (make-pointy 20 40 40 "black"))

;Define the function that stop the shape
;An ending is a function
;field -> boolean
(define (ending pos)
  (>= (pointy-x-pos pos) 160))

;big-bang define
;initialization
;to-draw; draw the shape
;on-tick; moving the shape - cross moving
;stop-when; stop the shape
;on-key; the special function - "q" -> reset
(big-bang (make-pointy 20 40 40 "black")
          (to-draw draw-shape)
          (on-tick move-shape)
          (stop-when ending)
          (on-key reset)
          )
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab_4_Unions_and_Recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Lab 4: Unions and Recursion

;--------------------------------------------------------------------------------------
;From Unions to Recursive Unions

;--------------------------------------------------------------
;Exercise 1
; A Frosting is one of:
; - "chocolate"
; - "vanilla"

;The structure of Desert
(define-struct desert (food))
; A Desert is one of:
; (make-cupcake Frosting)
; (make-pie String Number)

(define-struct cupcake (frosting))
;A Frosting is a structure:
; (make-cupcake Field)

(define-struct pie (filling slices))
;a Pie is a structure:
; (make-pie String Number)

;A function Processing is:
; processing the following data and structure definition.
; Variable -> Boolean
;(define (processing name)
;  (cond
;    [(desert? name) ...(desert-food person)...]
;    [(cupcake? name) ...(cupcake-frosting person)...]
;    [(pie? name) ...(pie-slices person)]
;    [(string=? name "chocolate") ...]
;    [(string=? name "vanilla") ...]
;    [else ...]
;    ))

;--------------------------------------------------------------
;Exercise 2

;Example of Frosting
(define sam-favorite (make-desert (make-cupcake "chocolate")))
(define john-favorite (make-desert (make-cupcake "vanilla")))
(define bob-favorite (make-desert (make-pie "apple" 4)))

;--------------------------------------------------------------
;Exercise 3

;The function of calculating calories
; Structure -> Number
(define (calories person)
  (cond
    [(desert? person) (calories (desert-food person))]
    [(cupcake? person) (calories (cupcake-frosting person))]
    [(pie? person) (* 175 (pie-slices person))]
    [(string=? person "chocolate") 150]
    [(string=? person "vanilla") 125]
    [else 0]
    ))

;checking the cal_cal function
(check-expect (calories sam-favorite) 150)
(check-expect (calories john-favorite) 125)
(check-expect (calories bob-favorite) 700)

;--------------------------------------------------------------
;Exercise 4

;A_Case_Of_Deserts(cod) is a structure
(define-struct case_of_deserts (desert cod))
; A Desert is one of:
; (make-cupcake Frosting)
; (make-pie String Number)

;--------------------------------------------------------------
;Exercise 5

(define sam-favorite_2 (make-case_of_deserts (make-desert (make-cupcake "chocolate"))
                                              (make-case_of_deserts (make-desert (make-pie "apple" 3)) empty)))
(define john-favorite_2 empty)
(define bob-favorite_2 (make-case_of_deserts (make-desert (make-pie "apple" 4)) empty))

;--------------------------------------------------------------
;Exercise 6

;The function of calculating calories
; Structure -> Number
;(define (total-calories a-cod)
;  (cond
;    [(empty? a-cod) ...]
;    [(case_of_deserts? a-cod) ... (case_of_deserts-desert cod) ... (case_of_deserts-cod a-cod)...]
;    [(desert? name) ...(desert-food person)...]
;    [(cupcake? name) ...(cupcake-frosting person)...]
;    [(pie? name) ...(pie-slices person)]
;    [(string=? name "chocolate") ...]
;    [(string=? name "vanilla") ...]
;    [else ...]
;    ))

;--------------------------------------------------------------
;Exercise 7

;The function of calculating calories
; Structure -> Number
(define (total-calories a-cod)
  (cond
    [(empty? a-cod) 0]
    [(case_of_deserts? a-cod) (+ (total-calories (case_of_deserts-desert a-cod))
                                 (total-calories (case_of_deserts-cod a-cod)))]
    [(desert? a-cod) (total-calories (desert-food a-cod))]
    [(cupcake? a-cod) (total-calories (cupcake-frosting a-cod))]
    [(pie? a-cod) (* 175 (pie-slices a-cod))]
    [(string=? a-cod "chocolate") 150]
    [(string=? a-cod "vanilla") 125]
    [else 0]
    ))

;checking the function
(check-expect (total-calories sam-favorite_2) 675)
(check-expect (total-calories john-favorite_2) 0)
(check-expect (total-calories bob-favorite_2) 700)


;--------------------------------------------------------------------------------------
;Extending the World

(require 2htdp/image)
(require 2htdp/universe)

;--------------------------------------------------------------
;Exercise 8

;The function of circling is a circle that show up on the blank screen
(define-struct ball (radius x-pos y-pos))
;Chose "radius" and not diameter, because the built in circle structure
; uses radius. Ignore colors in this structure.
;Chose "x-pos" and "y-pox" instead of "x" and "y", becaues want to be
; super specific about what those two fields do.

;Data desfinition of the ball structure:
;A Ball is a structure:
; (make-ball PositiveNumber PositiveNumber PositiveNumber)

;--------------------------------------------------------------
;Exercise 9

;The function of draw-shape is a appearance of the circle in the World
;(define (draw-shape a-shape)
;  (place-image (....(ball-radius a-shape)...)
;               (ball-x-pos a-shape)... (ball-y-pos a-shape)....
;               ...))

;The function of add-ball is getting the position of the mouse
;(define (add-ball current-dots mouse-x mouse-y me)
;  (cond
;    [(string=? "button-down" ...) (make-ball ... mouse-x mouse-y)]
;    [else ...]
;    ))

;--------------------------------------------------------------
;Exercise 10

;The function of draw-shape is a appearance of the circle in the World
(define (draw-shape a-shape)
  (place-image (circle (ball-radius a-shape) "outline" "red")
               (ball-x-pos a-shape) (ball-y-pos a-shape)
               (empty-scene 400 400)))
;The description of function
; Field -> World

;checking the function
(check-expect (draw-shape (make-ball 40 30 100))
              (place-image (circle 40 "outline" "red") 30 100 (empty-scene 400 400)))
(check-expect (draw-shape (make-ball 40 50 120))
              (place-image (circle 40 "outline" "red") 50 120 (empty-scene 400 400)))

;The function of add-ball is getting the position of the mouse
;Number -> Images
(define (add-ball current-dots mouse-x mouse-y me)
  (cond
    [(string=? "button-down" me) (make-ball 40 mouse-x mouse-y)]
    [else current-dots]
    ))

;The function of Big-Bang is a main operating part
(big-bang (make-ball 40 200 200)
          (on-mouse add-ball)
          (to-draw draw-shape)
          )
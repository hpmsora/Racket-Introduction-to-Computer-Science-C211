;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab_8_Snakes_on_a_Plane) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Lab 8: Snakes on a Plane

(require 2htdp/image)

;-------------------------------------------------------------------------
; Data Representation:

;A World is:
; -- (make-world Snake Food)
(define-struct world (snake food))

;A Snake is:
; -- (make-snake Direction Head Body)
(define-struct snake (direction head body))

;A Direction is one of:
; -- "north"
; -- "east"
; -- "south"
; -- "west"

;A Food is one of:
; -- empty
; -- (cons Point Food)

;A Head is:
; -- (make-point Number Number)
(define-struct point (x y))

;A Body is one of
; -- empty
; -- (cons Point Body)

;-------------------------------------------------------------------------
; Fixed Variable

;Diameters
; Snake Segment and Food
(define WIDTH 10)

;Radius
; Snake Segment and Food
(define RADIUS (/ WIDTH 2))

;World Size
(define WORLD-WIDTH 400)
(define WORLD-HEIGHT 400)

;Snake Image
;Head
(define SNAKE-HEAD-IMAGE
  (circle RADIUS "solid" "red"))
;Body
(define SNAKE-BODY-IMAGE
  (circle RADIUS "solid" "orange"))

;Food Image
(define FOOD-IMAGE
  (circle RADIUS "solid" "blue"))

;-------------------------------------------------------------------------
; Fixed Values

;Example of Snake number 1
(define s1
  (make-snake "north"
               (make-point 200 200)
               empty))

;Example of Food number 1
(define f1
  (cons (make-point 300 300)
        (cons (make-point 151 327)
              empty)))

;Example of World number 1
(define w1
  (make-world s1 f1))

;Example of Snake number 2
(define s2
  (make-snake "north"
               (make-point 200 200)
               (cons (make-point 200 210)
                     (cons (make-point 200 220)
                           (cons (make-point 200 230)
                                 (cons (make-point 200 240)
                                       (cons (make-point 200 250)
                                             empty)))))))

;Example of Food number 2
(define f2
  (cons (make-point 123 21)
        (cons (make-point 178 239)
              (cons (make-point 98 51)
                    (cons (make-point 259 371)
                          (cons (make-point 309 108)
                                empty))))))

;Example of World number 2
(define w2
  (make-world s2 f2))

;Example of Snake number 3
(define s3
  (make-snake "east"
              (make-point 200 200)
              (cons (make-point 200 210)
                    (cons (make-point 200 220)
                          (cons (make-point 210 220)
                                (cons (make-point 210 230)
                                      (cons (make-point 220 230)
                                            (cons (make-point 220 240)
                                                  (cons (make-point 230 240)
                                                        empty)))))))))

;Example of World number 3
(define w3
  (make-world s3 f2))

;-------------------------------------------------------------------------
;Redering Functions

;A function Render-World is:
; renders the current World
; World -> Image
(define (render-world world)
  (render-snake (world-snake world)
                (render-food (world-food world)
                             (empty-scene WORLD-WIDTH WORLD-HEIGHT))))

;A function Render-Snake is:
; renders the snake on some background image
; Snake Image -> Image
(define (render-snake snake image)
  (render-head (snake-head snake)
               (render-body (snake-body snake)
                            image)))

;checking the function
(check-expect (render-snake s1 (empty-scene 400 400))
              (place-image SNAKE-HEAD-IMAGE 200 200 (empty-scene 400 400)))
(check-expect (render-snake s2 (empty-scene 400 400))
              (place-image SNAKE-HEAD-IMAGE 200 200
                           (place-image SNAKE-BODY-IMAGE 200 210
                                        (place-image SNAKE-BODY-IMAGE 200 220
                                                     (place-image SNAKE-BODY-IMAGE 200 230
                                                                  (place-image SNAKE-BODY-IMAGE 200 240
                                                                               (place-image SNAKE-BODY-IMAGE 200 250
                                                                                            (empty-scene 400 400))))))))

;Both functions have implement function which imaging each elements.
; Render-World has implement functions imaging snake and food.
; Render-Snake has implement functions imaging head and body.

;A function Render-Head is:
; renders the head on some background image
; Head Image -> Image
(define (render-head head image)
  (place-image SNAKE-HEAD-IMAGE (point-x head) (point-y head) image))

;checking the function
(check-expect (render-head (make-point 53 43) (empty-scene 400 400))
              (place-image SNAKE-HEAD-IMAGE 53 43 (empty-scene 400 400)))
(check-expect (render-head (make-point 23 61) (empty-scene 200 200))
              (place-image SNAKE-HEAD-IMAGE 23 61 (empty-scene 200 200)))

;A function Render-Body is:
; renders the body on some background image
; Points Image -> Image
(define (render-body body image)
  (cond
    [(empty? body) image]
    [else
     (place-image SNAKE-BODY-IMAGE (point-x (first body)) (point-y (first body)) (render-body (rest body) image))]
    ))

;checking the function
(check-expect (render-body (snake-body s1) (empty-scene 400 400))
              (empty-scene 400 400))
(check-expect (render-body (snake-body s2) (empty-scene 400 400))
              (place-image SNAKE-BODY-IMAGE 200 210
                           (place-image SNAKE-BODY-IMAGE 200 220
                                        (place-image SNAKE-BODY-IMAGE 200 230
                                                     (place-image SNAKE-BODY-IMAGE 200 240
                                                                  (place-image SNAKE-BODY-IMAGE 200 250
                                                                               (empty-scene 400 400)))))))

;A function Render-Food is:
; renders the food set onto some background image
; Food(Point) Image -> Image
(define (render-food food image)
  (cond
   [(empty? food) image]
   [else
    (place-image FOOD-IMAGE (point-x (first food)) (point-y (first food)) (render-food (rest food) image))]
   ))

;checking the function
(check-expect (render-food f1 (empty-scene 400 400))
              (place-image FOOD-IMAGE 300 300
                           (place-image FOOD-IMAGE 151 327
                                        (empty-scene 400 400))))
(check-expect (render-food f2 (empty-scene 400 400))
              (place-image FOOD-IMAGE 123 21
                           (place-image FOOD-IMAGE 178 239
                                        (place-image FOOD-IMAGE 98 51
                                                     (place-image FOOD-IMAGE 259 371
                                                                  (place-image FOOD-IMAGE 309 108
                                                                               (empty-scene 400 400)))))))

;-------------------------------------------------------------------------
;World Updates

;A fuction Update-World is:
; updates world every tick
; World -> World
(define (update-world world)
  (make-world (move-snake (world-snake world))
              (process-food (world-food world))))

;A function Snake-Direction is:
; creates a new snake, in the new position
; Snake -> Snake
(define (move-snake snake)
  (make-snake (snake-direction snake)
              (move-head (snake-direction snake) (snake-head snake))
              (move-body (snake-head snake) (snake-body snake))))

;A function Process-Food is:
; does nothing, will be used later
; Food -> Food
(define (process-food food)
  food)

;A function Move-Head is:
; generates a new segment, at the new lovation of the head
; String Point -> Point
(define (move-head direction head)
  (cond
    [(string=? direction "north")
     (make-point (point-x head) (- (point-y head) WIDTH))]
    [(string=? direction "south")
     (make-point (point-x head) (+ (point-y head) WIDTH))]
    [(string=? direction "west")
     (make-point (- (point-x head) WIDTH) (point-y head))]
    [(string=? direction "east")
     (make-point (+ (point-x head) WIDTH) (point-y head))]
    ))

;checking the function
(check-expect (move-head "north" (make-point 53 32)) (make-point 53 22))
(check-expect (move-head "south" (make-point 53 32)) (make-point 53 42))
(check-expect (move-head "west" (make-point 53 32)) (make-point 43 32))
(check-expect (move-head "east" (make-point 53 32)) (make-point 63 32))

;A function Move-body is:
; movement of the snake as a whole, food is ignored
; Head Body -> Body
(define (move-body head body)
  (cond
    [(empty? body) empty]
    [else
     (cons head (reverse (rest (reverse body))))]
    ))

;checking the function
(check-expect (move-body (make-point 34 53) empty) empty)
(check-expect (move-body (snake-head s2) (snake-body s2))
              (cons (make-point 200 200)
                    (cons (make-point 200 210)
                          (cons (make-point 200 220)
                                (cons (make-point 200 230)
                                      (cons (make-point 200 240)
                                            empty))))))

;-------------------------------------------------------------------------
;Whistles and Bells

(require 2htdp/universe)

;A function Change-World is:
; modifies the world depending on what key is pressed
; World Key -> World
(define (change-world-accordingly world key)
  (cond
    [(string=? key "up") (point-snake "north" world)]
    [(string=? key "down") (point-snake "south" world)]
    [(string=? key "left") (point-snake "west" world)]
    [(string=? key "right") (point-snake "east" world)]
    [else world]
    ))

;A function Point-Snake is:
; changes the direction of the snake in the world
; World -> World
(define (point-snake direction world)
  (make-world (make-snake direction (snake-head (world-snake world)) (snake-body (world-snake world)))
              (world-food world)))

;A function Game-Ends is:
; determines if the head of the snake went out of the world's bounds
; World -> Boolean
(define (game-ends world)
  (not (and (and (< 0 (point-x (snake-head (world-snake world))))
                 (> 400 (point-x (snake-head (world-snake world)))))
            (and (< 0 (point-y (snake-head (world-snake world))))
                 (> 400 (point-x (snake-head (world-snake world))))))))

;A function And-Say-Goodbye is:
; places one last good bye message on an image (the world, rendered at that time)
; World -> Image
(define (and-say-goodbye world)
  (place-image (text "The End" 48 "olive") 200 200 (render-world world)))

(define (main initial)
  (big-bang initial
            (on-tick update-world 0.4)
            (to-draw render-world)
            (on-key change-world-accordingly)
            (stop-when game-ends and-say-goodbye)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment_7_Feeding_Worms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment 7: Feeding Worms

;Worms

(require 2htdp/image)
(require 2htdp/universe)

;----------------------------------------------------------------------
;Datas

;A World is a Structure:
; assign the worm and food are in one structure.
; - (make-world Worm Food)
(define-struct world (worm food))

;A Food is a Posistion
; - (make-posn Number Number)

;A ListOfSegments(LoS) is a List:
; - empty
; - (cons Segments(Posn) LoS)

;A Segment is a Position:
; - (make-posn Number Number)

;A RestListOfSegments(RLoS) is a List:
; After the function happen, the segments that except the last segment
; - empty
; - (cons Seg RLoS)

;A Worm is a structure:
; - (make-worm LoS direction)
(define-struct worm (los direct))

;A Direction(direct) is:
; - 'up
; - 'down
; - 'left
; - 'right

;One-segment Worm
; - (make-1worm posn direction)
;(define-struct 1worm (posn direct))

;----------------------------------------------------------------------
;Fixed-Variables

; World boundary
(define boundary-x 30)
(define boundary-y 30)

; Sizing
; Size of the one cell
(define standard-size 10)

; Imaging
(define worm-image (circle (/ standard-size 2) "solid" "red"))
(define food-image (square (/ standard-size 2) "solid" "green"))

;Background of Game Image
(define background (empty-scene
                    (* standard-size boundary-x) (* standard-size boundary-x)))

;Starting World
(define starting
  (make-world
   (make-worm (list (make-posn 2 6)) 'right)
   (make-posn 15 15)))

;Boundary Hits World
(define boundary-hits-world
  (make-world
   (make-worm (list (make-posn 2 6)) 'right) (make-posn 100 0)))

;Itself Hits World
(define itself-hits-world
  (make-world
   (make-worm (list (make-posn 2 6)) 'right) (make-posn 0 100)))

;----------------------------------------------------------------------
;Imaging Functions

;Celling
;A function celling is:
; consuming a Image, its position and background image and producing
; a significent image that fit with the cell.
; Image Number(x-posn) Number(y-posn) Image(background) -> Image
(define (celling img px py back)
  (place-image img
               (* standard-size (+ px 0.5))
               (* standard-size (- 30 (+ py 0.5)))
               back))

;chekcing the function
(check-expect (celling food-image 4 2 background)
              (place-image food-image 45 275 background))

;Worm-Imaging
;A function worm-imaging is:
; consuming a worm and background image and producing a one image have
; both worm and background
; Worm(Structure) Image -> Image
(define (worm-imaging worm back)
  (imp-worm-imaging (worm-los worm) back))

;Implement Worm-Imaging
;A function Imp-Worm-Imaging is:
; consuming a LoS(ListOfSegments) and image and producing a one image that
; constain all segments.
; LoS Imgage -> Image
(define (imp-worm-imaging los img)
  (cond
    [(empty? los) img]
    [else (celling
                  worm-image (posn-x (first los)) (posn-y (first los))
                  (imp-worm-imaging (rest los) img))]
    ))

;Food-Imaging
;A function food-imaging is:
; consuming a posn and background image and producing one image that
; constain all together.
; Posn Image -> Image
(define (food-imaging food back)
  (cond
    [(= 100 (posn-x food))
     (place-image (text "Worm Hits Boundary!" 24 "red") 150 150 (empty-scene 300 300))]
    [(= 100 (posn-y food))
     (place-image (text "Worm Hits Itself!" 24 "red") 150 150 (empty-scene 300 300))]
    [else
     (celling food-image (posn-x food) (posn-y food) back)]
    ))

;checking the function
(check-expect (food-imaging (make-posn 2 3) background)
              (celling food-image 2 3 background))

;Final-Imaging
;A function final-imaging is:
; consuming a World and make a Image
; World -> Image
(define (final-imaging world)
  (worm-imaging
   (world-worm world) (food-imaging
                       (world-food world) background)))

;----------------------------------------------------------------------
;Normal Functions

;Apply only Exercise 181 and Exercise 182

;Assigning-Direction-Moving
;A function Assign-Direct-Moving is:
; consuming a Segment and Direction and producing a new Worm that
; switched the 1 unit of the position to the assigned direction
; Direct Segment -> Worm
(define (assign-direct-moving direct seg)
  (cond
   [(symbol=? direct 'up) (make-posn (posn-x seg) (+ (posn-y seg) 1))]
    [(symbol=? direct 'down) (make-posn (posn-x seg) (- (posn-y seg) 1))]
    [(symbol=? direct 'right) (make-posn (+ (posn-x seg) 1) (posn-y seg))]
    [(symbol=? direct 'left) (make-posn (- (posn-x seg) 1) (posn-y seg))]
    ))

;checking the function
(check-expect (assign-direct-moving 'up (make-posn 44 2)) (make-posn 44 3))
(check-expect (assign-direct-moving 'down (make-posn 24 32)) (make-posn 24 31))
(check-expect (assign-direct-moving 'right (make-posn 54 32)) (make-posn 55 32))
(check-expect (assign-direct-moving 'left (make-posn 4 52)) (make-posn 3 52))

;Worm-Moving (Multi-Segment Worm)
;A function Worm-Moving is:
; cosuming Worm and producing a new Worm that move to the current
; aasigned direction one unit
; Worm(Structure) -> Worm (Structure)
(define (worm-moving worm)
  (make-worm
   (cons (assign-direct-moving (worm-direct worm) (first (worm-los worm)))
         (rest-cutting (worm-los worm)))
         (worm-direct worm)))

;checking the funtion
(check-expect (worm-moving (make-worm (list (make-posn 44 23) (make-posn 2 42) (make-posn 4 2))
                         'up))
              (make-worm (list (make-posn 44 24) (make-posn 44 23) (make-posn 2 42))
                         'up))

;Rest-Cutting
;A function Rest-Cutting is:
; consuming LoS and producing a new LoS does not have last segment.
; LoS(ListOfSegment) -> LoS(ListOfSegment)
(define (rest-cutting los)
  (cond
    [(empty? (rest los)) empty]
    [else (cons (first los) (rest-cutting (rest los)))]
    ))

;checking the function
(check-expect (rest-cutting
               (list (make-posn 44 23) (make-posn 2 42) (make-posn 4 2)))
              (list (make-posn 44 23) (make-posn 2 42)))

;Worm-Moving (One-Segment Worm)
;A function Worm-Moving is:
; consuming Worm and producing a new Worm that move to the current
; assigned direction one unit
; Worm(Struture) -> Worm(Structure)
;(define (worm-moving worm)
;  (make-1worm
;   (assign-direct-moving (1worm-direct worm) (1worm-posn worm))
;   (1worm-direct worm)))
;
;checking the function
;(check-expect (worm-moving (make-1worm (make-posn 42 24) 'down))
;                           (make-1worm (make-posn 42 23) 'down))

;----------------------------------------------------------------------
;Game Over Functions

;A function Inside-Border? is:
; consuming a segment and determining that the segment is inside of the
; boundary. Also each value must be more than 0 too.
; Segment(posn) -> Boolean
(define (inside-boundary? seg)
  (and (and (<= 0 (posn-x seg)) (< (posn-x seg) boundary-x))
       (and (<= 0 (posn-y seg)) (< (posn-y seg) boundary-y))))

;checking the function
(check-expect (inside-boundary? (make-posn 24 -54)) false)
(check-expect (inside-boundary? (make-posn 420 54)) false)
(check-expect (inside-boundary? (make-posn 0 14)) true)
(check-expect (inside-boundary? (make-posn 23 14)) true)

;A function Stop-Boundary? is:
; consuming worm and determine the worm is out of boundary.
;  - false: Continue
;  - true: Game Over
; worm -> String or Boolean
(define (stop-boundary? world)
  (cond
    [(not (inside-boundary? (first (worm-los (world-worm world))))) true]
    [else false]
    ))

;A function Stop-Boundary?(One-Segment Worm) is:
; consuming worm and determine the worm is out of boundary.
;  - false: Continue
;  - true: Game Over
; worm -> String or Boolean
;(define (stop-boundary? worm)
;  (cond
;    [(not (inside-boundary? (1worm-posn worm))) true]
;    [else false]
;    ))

;checking the function
;(check-expect (stop-boundary? (make-1worm (make-posn 25 23) 'down)) false)
;(check-expect (stop-boundary? (make-1worm (make-posn 3 22) 'right)) false)
;(check-expect (stop-boundary? (make-1worm (make-posn -3 22) 'right)) true)

;A function Self-Hit? is:
; consuming a assigned position and LoS and determine the assigned position
; is on the worm or not.
; LoS Posn -> Boolean
(define (self-hit? los point)
  (cond
    [(empty? los) false]
    [else
     (or (and (= (posn-x point) (posn-x (first los)))
              (= (posn-y point) (posn-y (first los))))
         (self-hit? (rest los) point))]
    ))

;checking the function
(check-expect (self-hit?
               (list (make-posn 45 23) (make-posn 3 22) (make-posn 2 22))
               (make-posn 41 23)) false)
(check-expect (self-hit?
               (list (make-posn 45 23) (make-posn 3 22) (make-posn 2 22))
               (make-posn 2 22)) true)

;A function Stop-Itself? is:
; consuming world(structure) and determine the worm hit its tail.
;  - false: Continue
;  - true: Game Over
; World(especially using worm here) -> String or Boolean
(define (stop-itself? world)
  (cond
    [(self-hit? (rest (worm-los (world-worm world)))
                (first (worm-los (world-worm world)))) true]
    [else false]
    ))

;checking the function
(check-expect (stop-itself? (make-world 
                               (make-worm (list (make-posn 1 22) (make-posn 2 22) (make-posn 1 22)) 'right)
                               empty)) true)
(check-expect (stop-itself? (make-world 
                               (make-worm (list (make-posn 3 22) (make-posn 2 22) (make-posn 1 22)) 'right)
                               empty)) false)

;----------------------------------------------------------------------
;Growing the Worm Functions

;A function Eating? is:
; consuming a World(Worm and Food) and determing the worm is on the
; same position with food.
; World(Structure) -> Boolean
(define (eating? world)
  (and (= (posn-x (world-food world)) (posn-x (first (worm-los (world-worm world)))))
       (= (posn-y (world-food world)) (posn-y (first (worm-los (world-worm world)))))))

;checking the function
(check-expect (eating? (make-world
                        (make-worm (list (make-posn 1 22) (make-posn 2 22) (make-posn 1 22)) 'right)
                        (make-posn 4 22))) false)
(check-expect (eating? (make-world
                        (make-worm (list (make-posn 1 22) (make-posn 2 22) (make-posn 1 22)) 'right)
                        (make-posn 1 22))) true)

;A function Growing-Worm is:
; using the Eating? function and World(Worm and Food) and producing new
; World(New Worm New Food) if the Eating? function returns true.
; World(Structure) -> World(Structure)
(define (growing-worm world)
  (cond
    [(eating? world)
     (make-world
      (make-worm (cons (assign-direct-moving
                        (worm-direct (world-worm world))
                        (first (worm-los (world-worm world))))
                       (worm-los (world-worm world)))
                 (worm-direct (world-worm world)))
      (make-posn (random boundary-x) (random boundary-x)))]
    [else world]
    ))

;A function Stop-All is:
; using hits functions and returns boolean it is ture or false.
; World -> Boolean
(define (stop-all world)
  (or (= 100 (posn-x (world-food world)))
      (= 100 (posn-y (world-food world)))))

;----------------------------------------------------------------------
;Operating the Game Functions

;A function ConsWorlds is:
; consuming Wrold with determining the situation and make a new world a clock tick
; World -> World
(define (consworlds world)
  (cond
    [(stop-boundary? world) boundary-hits-world]
    [(stop-itself? world) itself-hits-world]
    [(eating? world)
     (growing-worm world)]
    [else (make-world (worm-moving (world-worm world)) (world-food world))]
    ))

;A function Key-Pressing is:
; consuming a state and make new world
; World Key -> World
(define (key-pressing world key)
  (cond
    [(or (key=? "up" key) (key=? "down" key) (key=? "left" key) (key=? "right" key))
     (make-world (make-worm (worm-los (world-worm world))
                            (string->symbol key))
                 (world-food world))]
    [(key=? "n" key) starting]
    [else world]
    ))

(big-bang starting
          (on-draw final-imaging)
          (on-tick consworlds 0.2)
          (on-key key-pressing)
          (stop-when stop-all)
          )
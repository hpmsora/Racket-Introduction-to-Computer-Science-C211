;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |assignment 13|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")))))
;Assignment 13: Mazes

;--------------------------------------------------------------------------------------------
; 1. Rooms

;------------------------------------------------------------------------------
;Exercise 1

;A Direction is one of:
; - E (East)
; - W (West)
; - S (South)
; - N (North)

;------------------------------------------------------------------------------
;Exercise 2

;Importing the library
(require 2htdp/image)

;A Room is one of:
; - empty
; - (cons direction room)

(define ROOM-SIZE 40)
(define ROOM-BG (overlay (square (- ROOM-SIZE 2)
                                 "solid" "white")
                         (square ROOM-SIZE "solid" "black")))
(define N-DOOR
  (overlay/xy
   (beside (polygon
            (list (make-posn 0 0)
                  (make-posn (/ ROOM-SIZE 3) 0)
                  (make-posn (/ ROOM-SIZE 3) 5)
                  (make-posn (- (/ ROOM-SIZE 3) 1) 5)
                  (make-posn (- (/ ROOM-SIZE 3) 1) 1)
                  (make-posn 0 1)) "solid" "black")
           (rectangle (/ ROOM-SIZE 3) 5 "solid" "white")
           (polygon
            (list (make-posn ROOM-SIZE 0)
                  (make-posn (* 2 (/ ROOM-SIZE 3)) 0)
                  (make-posn (* 2 (/ ROOM-SIZE 3)) 5)
                  (make-posn (+ (* 2 (/ ROOM-SIZE 3)) 1) 5)
                  (make-posn (+ (* 2 (/ ROOM-SIZE 3)) 1) 1)
                  (make-posn ROOM-SIZE 1)) "solid" "black")) 
   0 0
   (square ROOM-SIZE "solid" "transparent")))

(define E-DOOR (rotate -90 N-DOOR))
(define S-DOOR (rotate 180 N-DOOR))
(define W-DOOR (rotate 90 N-DOOR))

;------------------------------------------------------------------------------
;Exercise 3

;A function Draw-Door is:
; takes a direction and a background image and places the corresponding door images
; on the given background.
; Direction(String) Image -> Image
(define (draw-door direct img)
  (cond
    [(string=? "N" direct) (overlay N-DOOR img)]
    [(string=? "S" direct) (overlay S-DOOR img)]
    [(string=? "E" direct) (overlay E-DOOR img)]
    [(string=? "W" direct) (overlay W-DOOR img)]
    ))

;checking the function
(check-expect (draw-door "W" (draw-door "S" ROOM-BG)) (overlay W-DOOR S-DOOR ROOM-BG))

;------------------------------------------------------------------------------
;Exercise 4

;A function Draw-Room is:
; takes a room and returns an image representing that room. Recursive Version.
; Room(ListOfDirection) -> Image
(define (draw-room room)
  (cond
    [(empty? room) ROOM-BG]
    [(empty? (rest room)) (draw-door (first room) ROOM-BG)]
    [else (draw-door (first room) (draw-room (rest room)))]
    ))

;checking the function
(check-expect (draw-room (list "N" "E")) (overlay N-DOOR E-DOOR ROOM-BG))

;A function Draw-Room_fold is:
; takes a room and returns an image representing that room. Fold Version.
; Room(ListOfDirection) -> Image
(define (draw-room_fold room)
  (foldr draw-door ROOM-BG room))

;checking the function
(check-expect (draw-room_fold (list "N" "E")) (overlay N-DOOR E-DOOR ROOM-BG))

;--------------------------------------------------------------------------------------------
; 2. Mazes

;------------------------------------------------------------------------------
;Exercise 5

; A Maze is a [List-of [List-of Room]] 
  
(define simple-maze 
  (list (list (list "E" "S") (list "S" "W")) 
        (list (list "N") (list "N")))) 
  
(define fig8 
  (list 
   (list (list "E" "S") (list "E" "W") (list "S" "W")) 
   (list (list "N" "S") empty (list "N" "S")) 
   (list (list "N" "E" "S") (list "E" "W") (list "N" "S" "W")) 
   (list (list "N" "S") empty (list "N" "S")) 
   (list (list "N" "E") (list "E" "W") (list "N" "W")))) 

;A function Draw-Row is:
; takes a ListOfRooms and draws them next to each other.
; ListOfRoom(LoR) -> Image
(define (draw-row lor)
  (foldr (lambda (x y) (beside (draw-room x) y)) empty-image lor))

;checking the function
(check-expect (draw-row (list (list "E" "S") (list "S" "W")))
              (beside (draw-room (list "E" "S"))
                      (draw-room (list "S" "W"))))

;------------------------------------------------------------------------------
;Exercise 6

;A function Draw-Maze is:
; takes a Maze and returns an image of the maze.
; Maze(ListOf (ListOfRoom)) -> Image
(define (draw-maze maze)
  (foldr (lambda (x y) (above (draw-row x) y)) empty-image maze))

;checking the function
(check-expect (draw-maze simple-maze)
              (above (draw-row (list (list "E" "S") (list "S" "W")))
                     (draw-row (list (list "N") (list "N")))))

;--------------------------------------------------------------------------------------------
; 3. Maze Positions

; A Natural-Number (Nat) is one of: 
;  - 0 
;  - (add1 Nat) 
  
; A Maze-Posn is: 
;  (make-posn Nat Nat) 
; interpretation: indicates a particular room in a Maze, starting 
;  with (make-posn 0 0) as the upper-left room 

;------------------------------------------------------------------------------
;Exercise 7

;A function Get-Room is:
; returns the Room corresponding to the given Maze-Posn in the given Maze.
; Make-Posn Maze -> Room
(define (get-room posn maze)
  (local
    (;Number Maze -> ListOfRoom
     (define (columning val1 val2 maze)
       (cond
         [(= 0 val2) (rowing val1 (first maze))]
         [else (columning val1 (- val2 1) (rest maze))]
         ))
     ;Number LoR -> Room(ListOfString)
     (define (rowing val lor)
       (cond
         [(= 0 val) (first lor)]
         [else (rowing (- val 1) (rest lor))]
         ))
     )(columning (posn-x posn) (posn-y posn) maze)
    ))

;chekcing the function
(check-expect (get-room (make-posn 1 1) fig8) empty)

;------------------------------------------------------------------------------
;Exercise 8

;A function Try-Move is:
; takes a current Maze-Pose, an intended Direction of travel, and a Maze and moves
; to a new Maze-Posn in the intended Direction, if movement in that Direction is
; possible from the current position in the Maze. If movement in that Direction is
; impossible, it should returns the original Make-Posn.
; Maze-Posn Direction Maze -> Maze-Posn
(define (try-move posn dir maze)
  (local
    (;Maze-Posn Direction Room -> Maze-Posn
     (define (finalizing posn dir room)
       (cond
         [(empty? room) posn]
         [(string=? dir (first room))
          (cond
            [(string=? dir "N") (make-posn (posn-x posn) (- (posn-y posn) 1))]
            [(string=? dir "S") (make-posn (posn-x posn) (+ (posn-y posn) 1))]
            [(string=? dir "E") (make-posn (+ (posn-x posn) 1) (posn-y posn))]
            [(string=? dir "W") (make-posn (- (posn-x posn) 1) (posn-y posn))]
            )]
         [else (finalizing posn dir (rest room))]
         ))
     )(finalizing posn dir (get-room posn maze))
    ))

;checking the function
(check-expect (try-move (make-posn 1 0) "E" fig8)
              (make-posn 2 0))
(check-expect (try-move (make-posn 1 0) "S" fig8)
              (make-posn 1 0))

;------------------------------------------------------------------------------
;Exercise 9

;A function Remove-Dups is:
; takes a ListOfItem and returns a version of that list with all the duplicates
; removed.
; List -> List
(define (remove-dups list)
  (local
    (;Item List -> List
     (define (remover item list)
       (cond
         [(empty? list) empty]
         [(equal? item (first list)) (remover item (rest list))]
         [else (cons (first list) (remover item (rest list)))]
         ))
     ;List -> List
     (define (removing list)
       (cond
         [(empty? list) empty]
         [else (cons (first list) (removing (remover (first list) list)))]
         ))
     )(removing list)
    ))

;checking the function
(check-expect (remove-dups '(1 2 3 3 3 4 5 6)) (list 1 2 3 4 5 6))

;------------------------------------------------------------------------------
;Exercise 10

;A function Neighbors is:
; takes a Maze-Posn and a Maze and returns ListOfMaze-Posn(LoMP) that are accessible
; from the current position. The resulting list should not contain any duplicated
; positions.
;Maze-Posn Maze -> LoMP
(define (neighbors posn maze)
  (reverse (cons posn (map (lambda (x) (try-move posn x maze)) (get-room posn maze)))))

;checking the function
(check-expect (neighbors (make-posn 0 1) simple-maze) 
              (list (make-posn 0 0) (make-posn 0 1))) 
(check-expect (neighbors (make-posn 1 1) fig8) 
              (list (make-posn 1 1))) 

;------------------------------------------------------------------------------
;Exercise 11

;A function Expand-Posns is:
; takes LoMP and a maze and adds to that list all Maze-Posns that are neighbors
; of positions in the list. the resulting list should not contains any duplicated
; positions.
(define (expand-posns lomp maze)
  (local
    (;
     (define (expanding lomp maze)
       (cond
         [(empty? lomp) empty]
         [else (append (neighbors (first lomp) maze) (expand-posns (rest lomp) maze))]
         ))
     )(remove-dups (expanding lomp maze))
    ))

;checking the function
(check-expect (expand-posns (list (make-posn 0 0) (make-posn 1 0)) fig8)
              (list (make-posn 0 1) (make-posn 1 0) (make-posn 0 0) (make-posn 2 0)))

;------------------------------------------------------------------------------
;Exercise 12

;A function Accessible-Posns is:
; takes a Mazes-Posn and a Maze and returns a list of all positions in the maze
; can be reached from the given position.
; Maze-Posn Maze -> ListOfPosn(LoP)
(define (accessible-posns posn maze)
  (local
    (;ListOfPosn Maze -> ListOfPosn
     (define (listing result maze)
       (cond
         [(equal? (length result) (length (expand-posns result maze))) result]
         [else (listing (expand-posns result maze) maze)]
         ))
     )(listing (list posn) maze)
    ))

;checking the function
(check-expect (accessible-posns (make-posn 0 0) fig8) (list (make-posn 0 4) (make-posn 2 4)
                                                            (make-posn 1 4) (make-posn 0 2)
                                                            (make-posn 0 3) (make-posn 2 2)
                                                            (make-posn 1 2) (make-posn 0 0)
                                                            (make-posn 0 1) (make-posn 2 3)
                                                            (make-posn 2 0) (make-posn 2 1)
                                                            (make-posn 1 0)))
(check-expect (accessible-posns (make-posn 1 1) fig8) (list (make-posn 1 1)))

;--------------------------------------------------------------------------------------------
; 4. Putting It All Together

;------------------------------------------------------------------------------
;Exercise 13

; A world is a structure
(define-struct world (maze location end))
; (make-world [List-of [List-of Room]] posn posn)

;------------------------------------------------------------------------------
; Exercise 14

;A function Solvable? is:
; returns true if the goal can be reached from the player's position.
; Maze-Posn Maze-Posn Maze -> Boolean
(define (solvable? begin end maze)
  (local
    (;Maze-Posn ListOfMaze-Posn -> Boolean
     (define (avail end lomp)
       (cond
         [(empty? lomp) false]
         [(equal? end (first lomp)) true]
         [else (avail end (rest lomp))]
         ))
     )(avail end (accessible-posns begin maze))
    ))

;checking the function
(check-expect (solvable? (make-posn 0 0) (make-posn 0 3) fig8) true)
(check-expect (solvable? (make-posn 0 0) (make-posn 1 1) fig8) false)

;------------------------------------------------------------------------------
; Exercise 15

;Importing Library
(require 2htdp/universe)

;Define the example of Fig8
(define example (make-world fig8 (make-posn 0 0) (make-posn 2 4)))

;A function Draw is:
; takes the 
(define (draw world)
  (place-image (circle 5 "solid" "red") (+ 20 (* 40 (posn-x (world-location world)))) (+ 20 (* 40 (posn-y (world-location world)))) 
               (place-image (circle 5 "solid" "green") (+ 20 (* 40 (posn-x (world-end world)))) (+ 20 (* 40 (posn-y (world-end world))))  
                            (draw-maze (world-maze world)))))


(define (key world ke)
  (cond
    [(string=? ke "up")
     (make-world (world-maze world) (try-move (world-location world) "N" (world-maze world)) (world-end world))]
    [(string=? ke "down")
     (make-world (world-maze world) (try-move (world-location world) "S" (world-maze world)) (world-end world))]
    [(string=? ke "right")
     (make-world (world-maze world) (try-move (world-location world) "E" (world-maze world)) (world-end world))]
    [(string=? ke "left")
     (make-world (world-maze world) (try-move (world-location world) "W" (world-maze world)) (world-end world))]
    ))

(define (ending world)
  (equal? (world-location world) (world-end world)))

(big-bang example
          (to-draw draw)
          (on-key key)
          (stop-when ending))
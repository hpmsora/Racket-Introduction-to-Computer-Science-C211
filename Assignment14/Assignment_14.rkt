;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment_14_Final) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))

(require 2htdp/image)
(require 2htdp/universe)
;Assignment 14

;-------------------------------------------------------------------------------------------------------------------------------------------
;Fixed Variable

(define HEIGHT 400)
(define WIDTH 500)
(define BALL_SIZE 10)
(define BAR_SIZE (/ HEIGHT 5))
(define BAR_SIZE_HALF (/ BAR_SIZE 2))
(define BAR_WIDTH 4)
(define BALL (circle BALL_SIZE "solid" "green"))
(define STICK (rectangle BAR_WIDTH BAR_SIZE "solid" "black"))
(define CENTER_HORIZONTAL_LINE (rectangle 6 HEIGHT "solid" "black"))
(define CENTER_VERTICAL_LINE (rectangle WIDTH 2 "solid" "black"))

;-------------------------------------------------------------------------------------------------------------------------------------------
;Client Side

;A World is structure:
(define-struct world [posn direction bar1 bar2 score1 score2])
; - (make-world Posn Posn Posn String Number Number)

;A function Draw-World is:
; draws a image the given world.
; World -> Image
(define (draw-world world)
  (place-image BALL (posn-x (world-posn world)) (posn-y (world-posn world))
               (place-image STICK BAR_WIDTH (posn-y (world-bar1 world))
                            (place-image STICK (- WIDTH BAR_WIDTH) (posn-y (world-bar2 world))
                                         (place-image CENTER_HORIZONTAL_LINE (/ WIDTH 2) (/ HEIGHT 2)
                                                      (place-image CENTER_VERTICAL_LINE (/ WIDTH 2) (/ HEIGHT 2)
                                                                   (place-image (text (number->string (world-score1 world)) 30 "black") (- (/ WIDTH 2) 20) 20
                                                                                (place-image (text (number->string (world-score2 world)) 30 "black") (+ (/ WIDTH 2) 20) 20
                                                                                             (empty-scene WIDTH HEIGHT)))))))))

;A function Tock is:
; receives the world and produces a new world which is next moment.
; World -> World
(define (tock world)
  (cond
    [(> 0 (posn-x (world-posn world)))
     (make-world (make-posn 250 190) "stop" (make-posn BAR_WIDTH (/ HEIGHT 2)) (make-posn (- WIDTH BAR_WIDTH) (/ HEIGHT 2)) (world-score1 world) (+ 1 (world-score2 world)))]
    [(< WIDTH (posn-x (world-posn world)))
     (make-world (make-posn 250 190) "stop" (make-posn BAR_WIDTH (/ HEIGHT 2)) (make-posn (- WIDTH BAR_WIDTH) (/ HEIGHT 2)) (+ 1 (world-score1 world)) (world-score2 world))]
    [(string=? (world-direction world) "nw")
     (wall (make-world (make-posn (- (posn-x (world-posn world)) 2) (- (posn-y (world-posn world)) 2)) "nw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world)))]
    [(string=? (world-direction world) "ne")
     (wall (make-world (make-posn (+ (posn-x (world-posn world)) 2) (- (posn-y (world-posn world)) 2)) "ne" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world)))]
    [(string=? (world-direction world) "sw")
     (wall (make-world (make-posn (- (posn-x (world-posn world)) 2) (+ (posn-y (world-posn world)) 2)) "sw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world)))]
    [(string=? (world-direction world) "se")
     (wall (make-world (make-posn (+ (posn-x (world-posn world)) 2) (+ (posn-y (world-posn world)) 2)) "se" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world)))]
    [(string=? (world-direction world) "stop")
     (make-world (world-posn world) "ne" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
   ))

;A function Wall is:
; receives the world and determines whether the world's ball will be bounce or not.
; World -> Boolean
(define (wall world)
    (cond
      [(or (> BALL_SIZE (posn-y (world-posn world))) (< (- HEIGHT BALL_SIZE) (posn-y (world-posn world))))
       (cond
         [(string=? (world-direction world) "nw") (make-world (world-posn world) "sw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         [(string=? (world-direction world) "ne") (make-world (world-posn world) "se" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         [(string=? (world-direction world) "sw") (make-world (world-posn world) "nw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         [(string=? (world-direction world) "se") (make-world (world-posn world) "ne" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         )]
      [(and (< (posn-x (world-posn world)) (+ BALL_SIZE (/ BAR_WIDTH 2)))
            (and (> (posn-y (world-posn world)) (- (posn-y (world-bar1 world)) BAR_SIZE_HALF))
                 (< (posn-y (world-posn world)) (+ (posn-y (world-bar1 world)) BAR_SIZE_HALF)))
            (or (string=? "nw" (world-direction world)) (string=? "sw" (world-direction world))))
        (cond
         [(string=? (world-direction world) "nw") (make-world (world-posn world) "ne" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         [(string=? (world-direction world) "sw") (make-world (world-posn world) "se" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         )]
      [(and (> (posn-x (world-posn world)) (- WIDTH (+ BALL_SIZE (/ BAR_WIDTH 2))))
            (and (> (posn-y (world-posn world)) (- (posn-y (world-bar2 world)) BAR_SIZE_HALF))
                 (< (posn-y (world-posn world)) (+ (posn-y (world-bar2 world)) BAR_SIZE_HALF)))
            (or (string=? "ne" (world-direction world)) (string=? "se" (world-direction world))))
       (cond
         [(string=? (world-direction world) "ne") (make-world (world-posn world) "nw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         [(string=? (world-direction world) "se") (make-world (world-posn world) "sw" (world-bar1 world) (world-bar2 world) (world-score1 world) (world-score2 world))]
         )]
      [else world]
      ))

;-------------------------------------------------------------------------------------------------------------------------------------------
;Server Code

;A WorldMsg is (list (list Number) (list Number))

;Updates the world based on the given message
;World WorldMsg -> World
(define (receive w msg)
  (make-world (world-posn w) (world-direction w) (make-posn BAR_WIDTH (first (first msg))) (make-posn (- WIDTH BAR_WIDTH) (first (second msg))) (world-score1 w) (world-score2 w)))


;A KeyMsg is one of:
; - "up"
; - "down"

;A Package is: (make-package World KeyMsg)

;World KeyEvent -> Package
(define (key w ke)
  (make-package w ke))

;A PlayerState (PS) is structure:
(define-struct ps [world y])
; - (make-ps IWorld Number)

;An IWorld is a structure that stores info about a specific client

;A ServerState (SS) is [List-of PlayerState]

;A function Move-PS is:
; updates the PS according to the given message
; PS(Sturcture) KeyMsg -> PS(Sturcture)
(define (move-ps ps msg)
  (cond
    [(string=? "up" msg) (if (= (ps-y ps) BAR_SIZE_HALF) ps (make-ps (ps-world ps) (- (ps-y ps) 10)))]
    [(string=? "down" msg) (if (= (ps-y ps) (- HEIGHT BAR_SIZE_HALF)) ps (make-ps (ps-world ps) (+ (ps-y ps) 10)))]
    [else ps]
    ))

;A function SS->Msg is:
; converts a ss to a worldmsg
; ServerState(List) -> WorldMsg(List)
(define (ss->msg ss)
  (list (list (ps-y (first ss)))
        (list (ps-y (second ss)))))

;on-new: what to do when a new client connects
;on-msg: what to do when the server gets a message from the client
;A Bundle is: (make-bundle ServerState [List-of Mail] [List-of IWorld]
;A Mail is: (make-mail IWorld WorldMsg)

;A function Message is:
; updates the universe and sends messages according to the given message from the given IWorld
; ServerState IWorld KeyMsg -> Bundle(Structure)
(define (message ss iw msg)
  (cond
    [(iworld=? iw (ps-world (first ss))) 
     (local
       [(define new-ss
          (list (move-ps (first ss) msg)
                (second ss)))]
       (make-bundle
        new-ss
        (list 
         (make-mail (ps-world (first ss))
                    (ss->msg new-ss))
         (make-mail (ps-world (second ss))
                    (ss->msg new-ss)))
        empty))]
    [(iworld=? iw (ps-world (second ss)))
     (local
       [(define new-ss
          (list (first ss)
                (move-ps (second ss) msg)))]
       (make-bundle
        new-ss
        (list 
         (make-mail (ps-world (first ss))
                    (ss->msg new-ss))
         (make-mail (ps-world (second ss))
                    (ss->msg new-ss)))
        empty))]))

;A function New is:
; adds a new PlayerState to the ServerState for the new client
; ServerState IWorld -> Bundle(Structure)
(define (new ss iw)
  (make-bundle (cons (make-ps iw (/ HEIGHT 2)) ss)
               empty empty))

;-------------------------------------------------------------------------------------------------------------------------------------------
;Big-Bang and Universe Function

;Launch two Big-Bang Function and one Universe Function
(launch-many-worlds
 (big-bang
  (make-world  (make-posn 250 190) "ne" (make-posn BAR_WIDTH (/ HEIGHT 2)) (make-posn (- WIDTH BAR_WIDTH) (/ HEIGHT 2)) 0 0)
  [register LOCALHOST]
  [on-tick tock]
  [to-draw draw-world]
  [on-receive receive]
  [on-key key])
 (big-bang
  (make-world (make-posn 250 190) "ne" (make-posn BAR_WIDTH (/ HEIGHT 2)) (make-posn (- WIDTH BAR_WIDTH) (/ HEIGHT 2)) 0 0)
  [register LOCALHOST]
  [on-tick tock]
  [to-draw draw-world]
  [on-receive receive]
  [on-key key])
 (universe
  empty
  [on-msg message]
  [on-new new]))
 
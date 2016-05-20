;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_ex140&ex141) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Russina Doll

;The doll constains color
(define-struct layer [color doll])
;An RD(Russian Doll) is one of:
; -String
; -(make-layer String RD)

;An RD constant "yellow, green, red"
;(make-layer "yellow" (make-layer "green" "red"))

;Make statement to show what is in side of RD
; RD -> String
; What color?
(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (string-append (layer-color an-rd)
                                   ", "
                                   (colors (layer-doll an-rd)))]
    ))

;checking the function
(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")
(check-expect (colors "red") "red")

;The function Inner consumes an RD and produces the (color of the)
; innermost doll.
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? (layer-doll an-rd)) (inner (layer-doll an-rd))]
    [else (layer-doll an-rd)]
    ))

;checking the function
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "yellow" (make-layer "green" "red")))
              "red")
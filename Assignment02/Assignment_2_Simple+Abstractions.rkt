;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment_2_Simple Abstractions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Scalable Tree

(require 2htdp/image)

;A function Tree is:
; taking one argument (the size of the picture) and draws a picture
; of a tree of that size.
; Number -> Image
(define (tree size)
  (place-image (triangle (/ size 2) "solid" "green") (/ size 2) (/ size 4)
               (place-image (triangle (/ size 2) "solid" "green") (/ size 2) (/ size 2)
                            (place-image (rectangle (/ size 8) (/ size 3) "solid" "brown") (/ size 2) (/ (* 3 size) 4)
                                         (empty-scene size size)))))

;Hourly Pay

(define (payment containers)
  (+ 10 (* containers 4)))

(check-expect (payment 0) 10)

(check-expect (payment 1) 14)

(check-expect (payment 2) 18)

(check-expect (payment 3) 22)

(check-expect (payment 4) 26)
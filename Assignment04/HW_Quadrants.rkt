;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Quadrants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;image-place
;circle: top-right
;ellipse: bottom-right
;square: top-left
;triangle: bottom-right
(place-image
 
;circle
;red
;radius: 30
(circle 30 "solid" "red")
30 30

(place-image
;ellipse
;green
;radius:60, 30
(ellipse 60 30 "solid" "green")
30 90

(place-image
;square
;blue
;height & wide: 60
(square 60 "solid" "blue")
90 30

(place-image
;triangle
;purple
;height & wide: 60
(triangle 60 "solid" "purple")
90 90
(square 120 "solid" "white")))))
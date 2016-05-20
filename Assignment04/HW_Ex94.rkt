;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Ex94) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Define the Zoo structure
(define-struct zoo (animal_name space))
;A Zoo is a structure:
; (make-zoo String PositiveNumber)

;Example of animals
; spider
; elephant
; boa constrictor
; armadillo
(define spider (make-zoo "spider" 5))
(define elephant (make-zoo "elephant" 4000))
(define boa_constrictor (make-zoo "boa constrictor" 100))
(define armadillo (make-zoo "armadillo" 300))

;The function of Fits is determine the cage is large enough for the animal.
;PositiveNumber -> String
(define (fits space a-animal)
  (cond
    [(> space (zoo-space a-animal)) (string-append "The space is fit for " (zoo-animal_name a-animal) ".")]
    [else (string-append "The space is not fit for " (zoo-animal_name a-animal) ".")]
))

;checking the function
(check-expect (fits 40 spider) "The space is fit for spider.")
(check-expect (fits 3000 elephant) "The space is not fit for elephant.")
(check-expect (fits 200 boa_constrictor) "The space is fit for boa constrictor.")
(check-expect (fits 50 armadillo) "The space is not fit for armadillo.")
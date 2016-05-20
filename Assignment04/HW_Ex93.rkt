;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Ex93) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;define student [first_name last_name gpa]
; String String PositiveNumber
(define-struct student [first_name last_name gpa])

;define professor [first_name last_name tenure_status]
; String String String
(define-struct professor [first_name last_name tenure_status])

;define staff [first_name last_name salary_group]
; String String Boolean(high 1, Low 0)
(define-struct staff [first_name last_name salary_group])

;representing function
;string -> string
(define (representing a-person)
  (string-append "Professor " (professor-first_name a-person) ", " (professor-last_name a-person) " is teaching " (professor-tenure_status a-person) "."))

(check-expect (representing (make-professor "John" "Izzo" "mathematics")) "Professor John, Izzo is teaching mathematics.")
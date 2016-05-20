;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment_3_Composing_Functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; 1. time Between
; 
; String -> String
; calculates the difference between t1 and t2 (in hour and minutes)
; given: "09:12" for t1 and "09:41" for t2
; expected: "0 hours and 29 minutes."
(define (time-between t1 t2)
  (minutes->time (- (time->minutes t2) (time->minutes t1))))
(check-expect (time-between "09:12" "09:41") "0 hours and 29 minutes.")
(check-expect (time-between "09:12" "12:05") "2 hours and 53 minutes.")

; 2. extract-hours
;
; String -> Number
; extracts the number of hours in the time representation time
; given: "12:03" expected: 12
(define (extract-hours time)
  (string->number (substring time 0 2)))
(check-expect(extract-hours "12:03") 12)
(check-expect(extract-hours "09:12") 9)

; 3. extract-minutes
;
; String -> Number
; extracts the number of minutes in the time representation time
; given: "12:03" expected: 12
(define (extract-minutes time)
  (string->number (substring time 3 5)))
(check-expect(extract-minutes "12:03") 3)
(check-expect(extract-minutes "09:12") 12)

; 4. hours-and-minutes->minutes
;
; Number Number -> Number
;converts a pair of numbers (hours, minutes) into a total duration (in minutes)
;given: 2 12 expected: (+ 12 (* 2 60))
;given: 1 59 expected: 119
(define (hours-and-minutes->minutes hours minutes)
  (+ minutes (* 60 hours)))
(check-expect (hours-and-minutes->minutes 2 12) 132)
(check-expect (hours-and-minutes->minutes 1 59) 119)

; 5. time->mintues
;
; String -> Number
; converts a time representation into an equivalent number of minutes
; given: "09:12" expected: (+ 12(* 9 60))
; given: "12:03" expected: (+ 3(* 3 60))
(define (time->minutes time)
  (hours-and-minutes->minutes (extract-hours time) (extract-minutes time)))
(check-expect (time->minutes "09:12") (+ 12(* 9 60)))
(check-expect (time->minutes "12:03") (+ 3 (* 12 60)))

; 6. minutes->time
;
; Number -> String
; converts a number of minutes into a time representation
; given: 3 expected: "0 hours and 3 minutes."
; given: (+ 44 (* 23 60)) expected: "23 hours and 44 minutes."
(define (minutes->time mins)
  (string-append (number->string (quotient mins 60)) " hours and " (number->string (remainder mins 60)) " minutes."))
(check-expect (minutes->time 3) "0 hours and 3 minutes.")
(check-expect (minutes->time 63) "1 hours and 3 minutes.")
(check-expect (minutes->time (+ 44 (* 23 60))) "23 hours and 44 minutes.")

; 7. less-than
;
; String String -> Boolean
;
; determines if the first time representation is earlier in the day than the second
; given: "09:12" "18:23" expected: true
; given: "22:01" "21:59" expected: false
(define (less-than t1 t2)
  (if(< (time->minutes t1) (time->minutes t2)) true false))
(check-expect (less-than "09:12" "18:23") true)
(check-expect (less-than "22:01" "21:59") false)

; 8. time-betwene
;
; String String -> String
; calculates the difference between t1 and t2 (in hours and minutes)
; if t2 earlier in day than t1 time computed's from today's t1 to tomorrow's t2
; given: "09:12" for t1 and "09:41" for t2 expected: "0 hours and 29 minutes."
; given: "09:41" for t1 and "09:12" for t2 expected: "23 hours and 31 munutes."
(define (time-betwene t1 t2)
  (if (less-than t1 t2) (time-between t1 t2) (minutes->time (- (+ (time->minutes t2) 1440) (time->minutes t1)))))

(check-expect (time-betwene "09:12" "09:41") "0 hours and 29 minutes.")
(check-expect (time-betwene "09:41" "09:125") "23 hours and 31 minutes.")
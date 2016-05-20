;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Seasons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;determine season
;Number -> String
;checking the function
;(check-expect (seasons 355) "The days 355 is Winter. And the length of current season is 90.")
;(check-expect (seasons 233) "The days 233 is Summer. And the length of current season is 92.")
(define (seasons day)
  (string-append "The days " (number->string day) " is " (season day) ". And the length of current season is " (number->string (length-of-season (season day))) "."))

(check-expect (seasons 355) "The days 355 is Winter. And the length of current season is 90.")
(check-expect (seasons 233) "The days 233 is Summer. And the length of current season is 92.")

;determine season
;Number -> String
;checking the function
;(check-expect (season 234) "Spring")
;(check-expect (season 34) "Winter")
(define (season day)
  (cond
    [(< day 80) "Winter"]
    [(< day 172) "Spring"]
    [(< day 264) "Summer"]
    [(< day 355) "Fall"]
    [(< day 365) "Winter"]))

(check-expect (season 234) "Summer")
(check-expect (season 34) "Winter")

;determine how many days in the season.
;String -> Number
;checking the function
;(check-expect (length-of-season "Winter") 90)
;(check-expect (length-of-season "Summer") 92)
(define (length-of-season current_season)
  (cond
    [(string=? current_season "Winter") 90]
    [(string=? current_season "Spring") 93]
    [(string=? current_season "Summer") 92]
    [(string=? current_season "Fall") 91]))

(check-expect (length-of-season "Winter") 90)
(check-expect (length-of-season "Spring") 93)
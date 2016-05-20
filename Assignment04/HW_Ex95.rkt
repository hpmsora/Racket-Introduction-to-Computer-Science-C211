;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW_Ex95) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;The function of Vehicles is represent five other vehicles
(define-struct vehicles (ve_name num_pass lic_plate fuel_cons))
;A Vehicles is a structure:
; (make-vehicles String PositiveNumber String PositiveNumber)

;Example of vehicles
;automobiles
;vans
;buses
;SUVs
;trucks
(define automobiles (make-vehicles "automobiles" 4 "general" 12))
(define vans (make-vehicles "vans" 8 "general" 10))
(define buses (make-vehicles "buses" 42 "large" 9))
(define SUVs (make-vehicles "SUVs" 6 "general" 11))
(define trucks (make-vehicles "trucks" 4 "special" 8))

;The function of representing is a appreance of vehicles
; Field -> String
(define (representing a-vehicle)
  (string-append "The " (vehicles-ve_name a-vehicle) " is for " (number->string (vehicles-num_pass a-vehicle)) " people and it is require " (vehicles-lic_plate a-vehicle) " license. Also the fuel efficient is " (number->string (vehicles-fuel_cons a-vehicle)) " miles per gallon."))

;checking the function
(check-expect (representing automobiles) "The automobiles is for 4 people and it is require general license. Also the fuel efficient is 12 miles per gallon.")
(check-expect (representing vans) "The vans is for 8 people and it is require general license. Also the fuel efficient is 10 miles per gallon.")
(check-expect (representing buses) "The buses is for 42 people and it is require large license. Also the fuel efficient is 9 miles per gallon.")
(check-expect (representing SUVs) "The SUVs is for 6 people and it is require general license. Also the fuel efficient is 11 miles per gallon.")
(check-expect (representing trucks) "The trucks is for 4 people and it is require special license. Also the fuel efficient is 8 miles per gallon.")
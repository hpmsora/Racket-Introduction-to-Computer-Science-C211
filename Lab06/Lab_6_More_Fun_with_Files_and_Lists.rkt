;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab_6_More_Fun_with_Files_and_Lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;In Search Of...

(require 2htdp/batch-io)

;A ListOfStrings(LoS) is a list
; LoS is one of
;  - empty
;  - (cons String LoS)

;A function Make_ListOfStrings(M_LoF) is:
; consuming a text fils and produce a list of the words in the text files.
; txtFiles -> M_LoS(List)
(define (make_LoF textFile)
  (read-words textFile))

;A function Has-Word? is:
; consuming a ListOfStrings and a String, returning true if the string is
; one of the strings in the list and false otherwise.
; LoS(List) String -> Boolean
(define (has-word? s-list str)
  (cond
    [(empty? s-list) false]
    [(string=? str (first s-list)) true]
    [else (has-word? (rest s-list) str)]
    ))

;checking the function
(check-expect (has-word? (cons "recall"
                               (cons "data"
                                     (cons "definitions"
                                           (cons "for"
                                                 (cons "lists"
                                                       (cons "from"
                                                             (cons "class" empty)))))))
                         "for")
              true)
(check-expect (has-word? (cons "recall"
                               (cons "data"
                                     (cons "definitions"
                                           (cons "for"
                                                 (cons "lists"
                                                       (cons "from"
                                                             (cons "class" empty)))))))
                         "on")
              false)

;Define a ListOfStrings(LoS) called FileList:
; containing the filenames of all of the files.
(define filelist
  (cons "thefly.txt"
        (cons "thegerm.txt"
              (cons "theoctopus.txt"
                    (cons "theostrich.txt"
                          (cons "thetermite.txt" empty))))))

;A function File-Has-Word? is:
; taking a String (representing a filename) and a String (representing a
; word to be searched for), and then return true if the corresponding file
; contains the fiven word.
; String (representing a filename) String (representing a word to be searched for) -> Boolean
(define (file-has-word? n_str w_str)
  (has-word? (make_LoF n_str) w_str))

;chekcing the function
(check-expect (file-has-word? "thefly.txt" "wisdom") true)
(check-expect (file-has-word? "thegerm.txt" "cool") false)
(check-expect (file-has-word? "theoctopus.txt" "marvel") true)
(check-expect (file-has-word? "theostrich.txt" "same") false)
(check-expect (file-has-word? "thetermite.txt" "Cousin") true)

;A function Search-Files is:
; taking a ListOfStrings (representing filenames) and a String, producing
; a ListOfStrings that contains only those filenames corresponding to files
; which contain the String in question.
; LoS (represending filenames)(List) String -> LoS
(define (search-files f-list str)
  (cond
    [(empty? f-list) empty]
    [(file-has-word? (first f-list) str)
     (cons (first f-list) (search-files (rest f-list) str))]
    [else (search-files (rest f-list) str)]
    ))

;checking the function
(check-expect (search-files filelist "cool") empty)
(check-expect (search-files filelist "wisdom") (cons "thefly.txt" empty))
(check-expect (search-files filelist "it") (cons "theostrich.txt" (cons "thetermite.txt" empty)))
(check-expect (search-files filelist "and") (cons "theostrich.txt" (cons "thetermite.txt" empty)))

;Use Seach-files to compute which of the files in filelist contain the word "one".
;Result: empty (none of them)

; Making Ripples

(require 2htdp/image)
(require 2htdp/universe)

; A sCircle is a structure
;  (make-scircle (Number(posn-x) Number(posn-y) Number(radius))
(define-struct scircle(posn-x posn-y radius))

; A ListOfsCricle(LosC) is a list
;  - empty
;  - (cons make-scricle(posn-x posn-y radius) LosC)

;A function Add-Circles is:
; When me is "button-down", add a dot at the mouse's coordinates
; to current-dots
; LosC Number Number MouseEvent -> LoPR
(define (add-circles current-circles mouse-x mouse-y me)
  (cond
    [(string=? "button-down" me) (cons (make-scircle mouse-x mouse-y 1) current-circles)]
    [else current-circles]
    ))

;A function Draw-Circle is:
; consuming a ListOfsCircle(LosC) and produce a circle at the point.
; LosC(List) -> Image
(define (draw-circle LosC)
  (cond
    [(empty? LosC)
     (empty-scene 400 400)]
    [(cons? LosC)
     (place-image (circle (scircle-radius (first LosC)) "outline" "black")
                  (scircle-posn-x (first LosC)) (scircle-posn-y (first LosC))
                  (draw-circle (rest LosC)))]
    ))

;A function Move-Circles is:
; consuming a ListOfsCircle(LosC) and change the radius of circle by 1
(define (bigger-circles current-circles)
  (cond
    [(empty? current-circles) empty]
    [(cons? current-circles)
     (cons (bigger-circle (first current-circles))
     (bigger-circles (rest current-circles)))]
    ))
;Semi function actual increast the radius.
(define (bigger-circle a-circle)
  (make-scircle (scircle-posn-x a-circle) (scircle-posn-y a-circle) (+ 1 (scircle-radius a-circle))))

;Big-Bang clause
; use "to-draw" "on-mouse" and "on-tick"
(big-bang empty
          (to-draw draw-circle)
          (on-mouse add-circles)
          (on-tick bigger-circles)
          )
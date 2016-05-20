;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment_6_Lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Book Exercise #119, 120, 123, 127, 128

;A ListOfString(LoS) is a list
; LoS is one of
;  - empty
;  - (cons String LoS)

;Example of ListOfString(LoS)
(define ex_LoS1 (cons "The"
                      (cons "great"
                            (cons "depression"
                                  (cons "and"
                                        (cons "the"
                                              (cons "New"
                                                    (cons "Deal" empty))))))))

;Eexercise #119
;A ListOfName is a list
; LoN is one of
;  - empty
;  - (cons String LoName)

;Example of ListOfName(LoName)
(define ex_LoName (cons "John"
                     (cons "Tom"
                           (cons "Mary"
                                 (cons "Ketharine"
                                       (cons "Jackson" empty))))))
;Exercise #120
;A ListOfBoolean is a list
; LoB is one of
;  - empty
;  - (cons Boolean LoB)

;Example of ListOfBoolean(LoB)
(define ex_LoB1 (cons true
                     (cons false
                           (cons true
                                 (cons true
                                       (cons true
                                             (cons false empty)))))))
(define ex_LoB2 (cons true
                     (cons true
                           (cons true
                                 (cons true
                                       (cons true
                                             (cons true empty)))))))

;Exercise #123
;A Contains? is a function:
; consuming a String and ListOfString(Los) to determines whether some given
; string occurs on a list of strings.
; String LoS(List) -> Boolean
(define (constains? s-list str)
  (cond
    [(empty? s-list) false]
    [(cons? s-list)
     (cond
       [(string=? str (first s-list)) true]
       [else (constains? (rest s-list) str)]
       )]
    ))

;Checking the function
(check-expect (constains? ex_LoName "John") true)
(check-expect (constains? ex_LoName "Jack") false)

;Exercise #127
;A All-True? is a function:
; consuming a list of Bollean values and determines whether all of the true.
; In other words, if there is any false on the list, the functio produces false;
; otherwise it produces true.
; LoB(List) -> Boolean
(define (all-true? b-list)
  (cond
    [(empty? b-list) true]
    [(cons? b-list)
     (cond
       [(first b-list) (all-true? (rest b-list))]
       [else false]
       )]
    ))

;Checking the function
(check-expect (all-true? ex_LoB1) false)
(check-expect (all-true? ex_LoB2) true)

;A One-True? is a function:
; consuming a list of Boolean values and determines whether at least one item on the
; list is true.
; LoB(List) -> Boolean
(define (one-true? lob)
  (cond
    [(empty? lob) true]
    [(empty? (rest lob)) (not (first lob))]
    [(cons? lob)
     (cond
       [(not (first lob)) (one-true? (rest lob))]
       [else true]
       )]
    ))

;Checking the function
(check-expect (one-true? ex_LoB1) true)
(check-expect (one-true? ex_LoB2) true)

;Exercise #128
;A Juxtapose is a function:
; consuming a list of strings and appends them all into one long string.
; LoS(List) -> String
(define (juxtapose s-list)
  (cond
    [(empty? s-list) ""]
    [(cons? s-list)
     (string-append (first s-list) " " (juxtapose (rest s-list)))]
    ))

;Checking the function
(check-expect (juxtapose ex_LoS1) "The great depression and the New Deal ")

; Counting Literature

(require 2htdp/batch-io)

;A Frequency is a structure:
; (make-frequency (String Number))
(define-struct frequency (str num))

;A ListOfStrings is a list
; LoS is one of
;  - empty
;  - (cons String LoS)

;A ListOfFrequency is a list
; LoF is one of
;  - empty
;  - (cons (make-frequency String Number) LoF)

;A function Count-Word is:
; comsuming a ListOfFrequency and a Sring and adds 1 to the frequency for that string
; producing a new ListOfFrequency.
; LoF(list) String -> LoF(List)
(define (count-word f-list strr)
  (cond
    [(empty? f-list) (cons (make-frequency strr 1) empty)]
    [(cons? f-list)
     (cond
       [(string=? strr (frequency-str (first f-list)))
        (cons (make-frequency strr (+ 1 (frequency-num (first f-list)))) (rest f-list))]
       [else 
        (cons (make-frequency (frequency-str (first f-list)) (frequency-num (first f-list)))
              (count-word (rest f-list) strr))]
       )]
    ))

;checking the function
(check-expect (count-word (cons (make-frequency "is" 1) empty) "is")
              (cons (make-frequency "is" 2) empty))
(check-expect (count-word (cons (make-frequency "is" 3) (cons (make-frequency "are" 2) empty)) "are")
              (cons (make-frequency "is" 3) (cons (make-frequency "are" 3) empty)))
(check-expect (count-word (cons (make-frequency "is" 1) empty) "are")
              (cons (make-frequency "is" 1) (cons (make-frequency "are" 1) empty)))

;A function Count-All-Words is:
; taking a ListOfString and produces a ListOfFrequecy with the appropriate frequencies 
; counted from the entire list of Strings
; LoS(List) -> LoF(List)
(define (count-all-words LoS)
  (cond
    [(cons? LoS) (count-word (count-all-words (rest LoS)) (first LoS))]
    [(empty? LoS) empty]
    ))

;checking the function
(check-expect (count-all-words (cons "is" (cons "is" (cons "are" empty))))
              (cons (make-frequency "are" 1) (cons (make-frequency "is" 2) empty)))

;Create a list of words from the file, Macbeth.
(define macbeth (read-words "Macbeth.txt"))

;Create a list of Frequency from the file, Macbeth.
(define f_macbeth (count-all-words macbeth))

;A function More_Than_100 is:
; consuming a ListOfFrequency and produces a ListOfFrequency that contains only
; the Frequencys from the original list where the number is more than 100.
; LoF(List) -> LoF(List)
(define (more_than_100 LoF)
  (cond
    [(empty? LoF) empty]
    [(cons? LoF)
     (cond
       [(< 100 (frequency-num (first LoF)))
        (cons (make-frequency
               (frequency-str (first LoF))
               (frequency-num (first LoF)))
              (more_than_100 (rest LoF)))]
       [else
        (more_than_100 (rest LoF))]
       )]
    ))

;checking the function
(check-expect (more_than_100 (cons (make-frequency "is" 120)
                                   (cons (make-frequency "are" 29)
                                         (cons (make-frequency "was" 203) empty))))
              (cons (make-frequency "is" 120)
                    (cons (make-frequency "was" 203) empty)))

;The result of more_than_100
;(cons
; (make-frequency "to" 367)
; (cons
;  (make-frequency "and" 427)
;  (cons
;   (make-frequency "in" 190)
;   (cons
;    (make-frequency "of" 396)
;    (cons
;     (make-frequency "the" 620)
;     (cons
;      (make-frequency "his" 127)
;      (cons
;       (make-frequency "this" 108)
;       (cons
;        (make-frequency "our" 117)
;        (cons
;         (make-frequency "with" 141)
;         (cons
;          (make-frequency "be" 133)
;          (cons
;           (make-frequency "that" 158)
;           (cons
;            (make-frequency "And" 169)
;            (cons
;             (make-frequency "your" 122)
;             (cons
;              (make-frequency "a" 256)
;              (cons
;               (make-frequency "not" 142)
;               (cons
;                (make-frequency "I" 326)
;                (cons
;                 (make-frequency "my" 170)
;                 (cons
;                  (make-frequency "is" 185)
;                  (cons
;                   (make-frequency "haue" 114)
;                   (cons
;                    (make-frequency "The" 131)
;                    (cons
;                     (make-frequency "Macb." 137)
;                     (cons
;                      (make-frequency "you" 193)
;                      (cons
;                       (make-frequency "it" 128)
;                       empty)))))))))))))))))))))))
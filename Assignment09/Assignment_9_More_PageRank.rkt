;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment_9_More_PageRank) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment 9: More PageRank

;1. Book Exercise

;---------------------------------------------------------------------------
;Exercise 221

(define-struct ir [name price])
; An IR is 
;   (make-ir String Number)

;A function Extract1 is:
; creates an Inventory from an-inv for all
; those items that cost less than $1
; Inventory -> Inventory
(define (extract1 an-inv)
  (local
    (;Inventory -> Inventory
     (define (extract an-inv1)
       (cond
         [(empty? an-inv1) empty]
         [else
          (insert (first an-inv1) (extract (rest an-inv1)))]
         ))
     ;IR Inventory -> Inventory
     (define (insert fir res)
       (cond
         [(<= (ir-price fir) 1.0)
          (cons fir res)]
         [else res]
         ))
     )
    (extract an-inv)
    ))

;checking the function
(check-expect (extract1 empty) empty)
(check-expect (extract1 (list (make-ir "A" 0.3) (make-ir "B" 1.5) (make-ir "C" 1.4) (make-ir "D" 0.3)))
              (list (make-ir "A" 0.3) (make-ir "D" 0.3)))
(check-expect (extract1 (list (make-ir "A" 1.3) (make-ir "B" 2.4) (make-ir "C" 0.4) (make-ir "D" 0.3)))
              (list (make-ir "C" 0.4) (make-ir "D" 0.3)))

;---------------------------------------------------------------------------
;Exercise 222

;A function Sort> is:
; constructs a list from the items in i in descending order
; LoN -> LoN
(define (sort> io)
  (local (;LoN -> LoN
          (define (sort l)
            (cond
              [(empty? l) empty]
              [else (insert (first l) (sort (rest l)))]
              ))
          ;Number LoN -> LoN
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(> an (first l)) (cons an l)]
                 [else
                  (cons (first l) (insert an (rest l)))]
                 )]
              ))
          )
    (sort io)
    ))

;checking the function
(check-expect (sort> empty) empty)
(check-expect (sort> (list 1)) (list 1))
(check-expect (sort> (list 5 3 6 9 1)) (list 9 6 5 3 1))

;A function Sort< is:
; constructs a list form the items in i in ascending order
; LoN -> LoN
(define (sort-< io)
  (local (;LoN -> LoN
          (define (sort l)
            (cond
              [(empty? l) empty]
              [else (insert (first l) (sort (rest l)))]
              ))
          ;Number LoN -> LoN
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(< an (first l)) (cons an l)]
                 [else
                  (cons (first l) (insert an (rest l)))]
                 )]
              ))
          )
    (sort io)
    ))

;checking the function
(check-expect (sort-< empty) empty)
(check-expect (sort-< (list 1)) (list 1))
(check-expect (sort-< (list 5 3 6 9 1)) (list 1 3 5 6 9))

;A function Sort-a is:
; abstracts sort> and sort-<.
; fn LoN -> LoN
(define (sort-a fn fn2 io)
  (local (;LoN -> LoN
          (define (sort fn fn2 l)
            (cond
              [(empty? l) empty]
              [else (insert fn fn2 (first l) (sort fn fn2 (rest l)))]
              ))
          ;Number LoN -> LoN
          (define (insert fn fn2 an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(fn (fn2 an) (fn2 (first l))) (cons an l)]
                 [else
                  (cons (first l) (insert fn fn2 an (rest l)))]
                 )]
              ))
          )
    (sort fn fn2 io)
    ))

;Re-define the function Sort> and Sort-< in terms of Sort-a
(define (re-sort> io)
  (sort-a > add1 io))
(define (re-sort-< io)
  (sort-a < add1 io))

(check-expect (re-sort> empty) empty)
(check-expect (re-sort> (list 1)) (list 1))
(check-expect (re-sort> (list 5 3 6 9 1)) (list 9 6 5 3 1))
(check-expect (re-sort-< empty) empty)
(check-expect (re-sort-< (list 1)) (list 1))
(check-expect (re-sort-< (list 5 3 6 9 1)) (list 1 3 5 6 9))

;A function String-Length-Sort-Descending is:
; constructs a ListOfString from the items in i in descending order by string length
; ListOfString(LoS) -> ListOfString(LoS)
(define (string-length-sort-descending los)
  (sort-a > string-length los))

;checking the function
(check-expect (string-length-sort-descending
               (list "APPLE" "BANANA" "COKE" "HAMBURGER" "EGG"))
              (list "HAMBURGER" "BANANA" "APPLE" "COKE" "EGG"))

;A function String-Length-Sort-Ascending is:
; constructs a ListOfString from the items in i in ascending order by string length
; ListOfString(LoS) -> ListOfString(LoS)
(define (string-length-sort-ascending los)
  (sort-a < string-length los))

;checking the function
(check-expect (string-length-sort-ascending
               (list "APPLE" "BANANA" "COKE" "HAMBURGER" "EGG"))
              (list "EGG" "COKE" "APPLE" "BANANA" "HAMBURGER"))

;---------------------------------------------------------------------------
;Exercise 224

;A Toy is structure
(define-struct toy (acq-price reco-price))
;(make-toy (Number Number))

;A ListOfToy(LoT) is List
; - empty
; - (cons toy lot)

;A function Sort is:
; consumes the LoT and produce the new LoT by the difference between the two
; prices as descending
; LoT -> LoT
(define (sort-toy lot)
  (local
     (;LoT -> LoT
     (define (sorting l)
       (cond
         [(empty? l) empty]
         [else (insert (first l) (sorting (rest l)))]
         ))
     ;Number LoN -> LoN
     (define (insert an l)
       (cond
         [(empty? l) (list an)]
         [else
          (cond
            [(> (difference an) (difference (first l))) (cons an l)]
            [else
             (cons (first l) (insert an (rest l)))]
            )]
         ))
    ;Number Number -> Number
    (define (difference toy)
      (abs (- (toy-acq-price toy) (toy-reco-price toy))))
    )
    (sorting lot)
    ))

;checking the function
(check-expect (sort-toy empty) empty)
(check-expect (sort-toy (list (make-toy 44 13))) (list (make-toy 44 13)))
(check-expect (sort-toy (list (make-toy 34 52) (make-toy 50 23) (make-toy 34 40)))
              (list (make-toy 50 23) (make-toy 34 52) (make-toy 34 40)))

;---------------------------------------------------------------------------
;Exercise 265

;A Dir.v1 (short for directory) is one of:
; - empty
; - (cons file.v1 dir.v1)
; - (cons dir.v1 dir.v1)

;A File.v1 is a Symbol

;Figure 68 Translating
(define example_dir.v1
  (list (list (list 'part1 'part2 'part3) 'read! (list (list 'hang 'draw) (list 'read!)))))

;---------------------------------------------------------------------------
;Exercise 266

;A function How-Many is:
; determines how many files a given Dir.v1 contains.
; List -> Number
(define (how-many.v1 lox)
  (local
    (;Dir.v1 -> Number
    (define (counting lox)
       (cond
         [(empty? lox) 0]
         [else (+ 0 (count-in (first lox) (counting (rest lox))))]
         ))
     ;Dir.v1 Dir.v1 -> Number
     (define (count-in dir1 dir2)
       (cond
         [(empty? dir1) dir2]
         [(symbol? dir1) (+ 1 dir2)]
         [(list? dir1) (count-in (first dir1) (count-in (rest dir1) dir2))]
        ))
     )
     (counting lox)
    ))

(check-expect (how-many.v1 example_dir.v1) 7)

;---------------------------------------------------------------------------
;Exercise 267

(define-struct dir [name content])

;A Dir.v2 is a structure
; - (make-dir Symbol LoFD)

;A LoFD(ListOfFilesAndDirectories) is one of:
; - empty
; - (cons File.v2 LoFD)
; - (cons Dir.v2 LoFD)

;A File.v2 is  a Symbol

;Figure 68 Translating
(define example_dir.v2
  (list (make-dir 'TS (list (make-dir 'text
                                      (list 'part1 'part2 'part3))
                            'read!
                            (make-dir 'libs (list (make-dir 'code (list 'hang 'draw))
                                                  (make-dir 'docs (list 'read!))))
                            ))))

;---------------------------------------------------------------------------
;Exercise 268

;A function How-Many.v2 is:
; determines how many files a given Dir.v2 contains.
; List -> Number
(define (how-many.v2 lox)
  (local
    (;Dir.v2 -> Number
    (define (counting lox)
       (cond
         [(empty? lox) 0]
         [else (+ 0 (count-in (first lox) (counting (rest lox))))]
         ))
    ;Dir.v2 Dir.v2 -> Number
    (define (count-in dir1 dir2)
       (cond
         [(empty? dir1) dir2]
         [(symbol? dir1) (+ 1 dir2)]
         [(dir? dir1) (+ (counting (dir-content dir1)) dir2)]
        ))
     )
     (counting lox)
    ))

(check-expect (how-many.v2 example_dir.v2) 7)

;----------------------------------------------------------------------------------------------------
;Page Rank

;A Simple-Element is:
; The string tag represents the type of html tag and content represents the list
; of HTML-elements that occur between the tags. 
(define-struct elem [tag content]) 
; - (make-elem String LoH) 

;A Link-Element is:
; The string url  represents the web address of the page being linked to, and the
; content represents the content between the <a></a> tags. 
(define-struct link [url content])
; - (make-link String List-of-HTML)
  
;An HTML-Element (HTML) is one of: 
; - String 
; - (make-elem String List-of-HTML)  [Simple-Element] 
; - (make-link String List-of-HTML)  [Link-Element] 
  
;A List-of-HTML (LoH) is one of: 
; - empty 
; - (cons HTML List-of-HTML)

;A Webpage is a Structure
(define-struct webpage (URL content))
; - (make-webpage string List-of-HTML-elements)

(define (list-out-links elem)
 (cond 
    [(string? elem) empty]
    [(elem? elem )(list-out-links-list (elem-content elem))]
    [(link? elem)(cons(link-url elem)(list-out-links-list (link-content elem)))]))

(define (list-out-links-list elem)
  (cond 
    [(empty? elem) empty]
    [else 
     (append (list-out-links (first elem)) (list-out-links-list (rest elem)))]))

(define (page-outlinks webpage)
  (list-out-links-list (webpage-content webpage)))

(define (get-page url list web)
  (cond 
    [(empty? list) web]
    [else
     (if
      (equal? url (webpage-URL (first list)))
      (first list)
      (get-page url (rest list) web))]))

(define (list-reference i list)
  (position i list 0)) 
  
(define (position number lon i)
  (cond 
    [(eq? i number) (first lon)]
    [else (position number (rest lon) (add1 i))]))

(define (randomize list)
  (list-reference (random (length list)) list))
 
(define (random-surf webpage list)
  (cond 
    [(empty? (page-outlinks webpage))(randomize list)]
    [else
  (get-page (page-outlinks webpage) list(randomize list))])) 

;Example of Webpage
(define webpage1 (make-webpage "web.com/index.html" (list (make-elem "p" (list "go" (make-link "web.com/links.html" (list "here")) "for" "my" (make-elem "em" (list "favorite")) "links")))))
(define webpage2 (make-webpage "web.com/links.html" (list (make-elem "p" (list "this is my" (make-link "web.com/links.html" (list "links" "page.")) "back to the" (make-link "web.com/index.html" (list "main page")))))))
(define listweb3 (list webpage1 webpage2))

;-----------------------------------------------------------------------------
; Exercise 1 

;A function Ramdom-Surf-For-N is:
; randomly surfs the list of webapges for the given number of steps. It then
; produces the list of webpages visited
; Webpage LoW Number -> LoW
(define (random-surf-for-n webpage list number)
  (cond
    [(= 0 number) empty]
    [else (cons (random-surf webpage list) (random-surf-for-n webpage list (- number 1)))]))

;checking the function
(random-surf-for-n webpage1 listweb3 3)

;-----------------------------------------------------------------------------
; Exercise 2

;A function Random-Surf-For-N/Restart is:
; chooses a random link, but with probability [0 < number < 1] restarts and
; choose a new page randomly from the [Non-EmptyListOf Webpage]
; webpage LoW Number Number -> LoW
(define (random-surf-for-n/restart webpage list number chance)
  (cond
    [(= 0 number) empty]
    [else
     (if (> (* 100 chance) (random 100))
         (cons (randomize list) (random-surf-for-n/restart webpage list (sub1 number) chance))
         (cons (random-surf webpage list) (random-surf-for-n/restart webpage list (sub1 number) chance)))]))

;checking the function
(random-surf-for-n/restart webpage1 listweb3 4 .57)

;-----------------------------------------------------------------------------
; Exercise 4

;A Visits is a structure
(define-struct visits (visits webpage))
; - (make-visits number webpage)

;-----------------------------------------------------------------------------
; Exercise 5

;A function Total is:
; this function calculates how many times a webpage has been visited during surfing
; ListOfWebpages(LoW) -> ListOfVisits(LoV)
(define (total list master-page)
  (cond
    [(empty? master-page) empty]
    [else
     (cons (make-visits (how-many list (first master-page)) (first master-page)) (total (remove-dupe list (first master-page)) (rest master-page)))]))

;A function How-Many is:
; determines how often the given webpage appears in the list of webpages
; LoW Webpage -> Number
(define (how-many list webpage)
  (cond
    [(empty? list) 0]
    [else
     (if
      (string=? (webpage-URL webpage) (webpage-URL (first list)))
      (+ 1 (how-many (rest list) webpage))
      (+ 0 (how-many (rest list) webpage)))]))

;Implicit function of How-Many
;LoW Webpage -> LoW
(define (remove-dupe list webpage)
  (cond
    [(empty? list) empty]
    [else
     (if
      (string=? (webpage-URL webpage) (webpage-URL (first list)))
      (remove-dupe (rest list) webpage)
      (cons (first list) (remove-dupe (rest list) webpage)))]))


;checking the function
(check-expect
 (total (list (make-webpage "web.com/links.html" (list (make-elem "p" (list "this is my"
                                                                            (make-link "web.com/links.html" (list "links" "page."))
                                                                            "back to the"
                                                                            (make-link "web.com/index.html" (list "main page"))))))
              (make-webpage "web.com/links.html" (list (make-elem "p" (list "this is my"
                                                                            (make-link "web.com/links.html" (list "links" "page."))
                                                                            "back to the"
                                                                            (make-link "web.com/index.html" (list "main page"))))))) listweb3)
 (list (make-visits 0 (make-webpage "web.com/index.html" (list (make-elem "p" (list "go" (make-link "web.com/links.html" (list "here"))
                                                                                    "for" "my"
                                                                                    (make-elem "em" (list "favorite")) "links")))))
       (make-visits 2 (make-webpage "web.com/links.html" (list (make-elem "p" (list "this is my"
                                                                                    (make-link "web.com/links.html"(list "links" "page."))
                                                                                    "back to the"
                                                                                    (make-link "web.com/index.html" (list "main page")))))))))
(check-expect (total empty listweb3)
              (list (make-visits 0 (make-webpage "web.com/index.html" (list (make-elem "p"(list "go"(make-link "web.com/links.html" (list "here"))
                                                                                                "for"
                                                                                                "my"
                                                                                                (make-elem "em" (list "favorite"))
                                                                                                "links")))))
                    (make-visits 0 (make-webpage "web.com/links.html" (list (make-elem "p" (list "this is my"
                                                                                                 (make-link "web.com/links.html"(list "links" "page."))
                                                                                                 "back to the"
                                                                                                 (make-link "web.com/index.html" (list "main page")))))))))

;-----------------------------------------------------------------------------
; Exercise 6

;A function Rank is:
; makes a list of visits of the given length with the given chance
; LoW Number Number -> LoV
(define (rank list number chance)
  (sorting (total (random-surf-for-n/restart (randomize list) list number chance) list)))

;A function Sorted? is
; checks to see if a list of webpages is ordered by amount of visits
; LoW -> Boolean
(define (sorted? low)
  (cond
    [(or (empty? low) (empty? (rest low))) true]
    [else
     (cond
       [(> (visits-visits (first low)) (visits-visits (second low)))
        (sorted? (rest low))]
       [else false]
       )]
    ))

;A function Passing is:
; swaps two elements in a list of webpages if they are not sorted
; LoW Funtion -> LoW
(define (passing low)
  (cond
    [(empty? low) empty]
    [(empty? (rest low)) low]
    [else
     (cond
       [(or (> (visits-visits (first low)) (visits-visits (second low))) 
            (= (visits-visits (first low)) (visits-visits (second low))))
        (cons (first low) (passing (rest low)))]
       [else
        (passing (cons (first (rest low))
                      (cons (first low)
                            (rest (rest low)))))]
       )]
    ))

;A function Sorting is:
; resorts the list if the list is still not sorted after passing
; LoW Funtion -> LoW
(define (sorting low)
  (cond
    [(sorted? low) low]
    [else
     (sorting (passing low))]
    ))

;checking the function
(rank listweb3 4 .5)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab_9_PageRank) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Lab 9 PageRnak

(define-struct elem [tag content])
;A Simple-Element is: (make-elem String Lost-Of-HTML(LoH)
; Interpretation: The string tag represents the type of html tag (e.g.. "h1", "p",
; "em", ...), and content represets the list ofHTML -elements that occcur between
; the tags.

(define-struct link [url content])
;_A Link-Element is: (make-link String Lost-Of-HTML(LoH)
; Interpretation: The string url represents the web address of the page being
; linked to. and the content represents the content between the <a></a> tags.

;An HTML-Element (HTML) is one of:
; - String
; - (make-elem String LoH) [Simple-Element]
; - (make-link String LoH) [Link-Element]

;A List-Of-HTML(LoH) is one of:
; - empty
; - (cons HTML LoH)

;------------------------------------------------------------------------------------
;Exercise 1
; process-html 
; will take an element and process it ...
; element -> ...
;(define (process-html elem)
;  (cond 
;    [(string? elem )... ]
;    [(elem? elem )..(elem-tag elem) ...(process-html-list (elem-content elem))]
;    (else 
;    [(link? elem)... (link-url elem)...(process-html-list (elem-content elem))])))

; process-html-list 
; will take a list of leement and process it ..
; listofelement -> ...
;    
;(define (process-html-list lem)
;     (cond 
;       [(empty? lem)...]
;       [else (process-html(first lem)) ... (process-html-list (rest lem))]))

;------------------------------------------------------------------------------------
;Exercise 2

;A webpage is a structure
(define-struct webpage [URL content])
; - (make-webpage string List-of-HTML-elements)

;------------------------------------------------------------------------------------
;Exercise 3

;A example of first html is:
(define IndexEx_1
  (make-webpage "web.com/index.html"
                (list (make-elem "h1" (list "My Page"))
                      (make-elem "p" (list (make-elem "a" (list (make-link "web.com/index.html" (list "Go "))))
                                           (make-elem "em" (list "favorite"))
                                           "links.")))))

(define ex_html_1
  (list (make-elem "h1" (list "My Page"))
        (make-elem "p" (list (make-elem "a" (list (make-link "web.com/index.html" (list "Go "))))
                             (make-elem "em" (list "favorite"))
                             "links."))))

;------------------------------------------------------------------------------------
;Exercise 4

;A example of second html is:
(define IndexEx_2
  (make-webpage "web.com/links.html"
                (list (make-elem "p" (list "This is my "
                             (make-elem "a" (list (make-link "web.com/links.html" (list "links page."))))
                             " Back go the "
                             (make-elem "a" (list (make-link "web.com/index.html" (list "main page?")))))))))

(define ex_html_2
  (list (make-elem "p" (list "This is my "
                             (make-elem "a" (list (make-link "web.com/links.html" (list "links page."))))
                             " Back go the "
                             (make-elem "a" (list (make-link "web.com/index.html" (list "main page?"))))))))

;------------------------------------------------------------------------------------
;Exercise 5

;A function List-Outlinks is:
; takes a ListOfHTML(LoH) and returns a list of all the URLs(LoS) that are linked to within
; given ListOfHTML(LoH).
; ListOfHTML(LoH) -> ListOfString(LoS)
(define (list-outlinks loh)
  (local
    (;LoH -> LoH
     (define (list-outlinking loh)
       (cond
         [(empty? loh) empty]
         [else
          (find-urls (first loh) (list-outlinking (rest loh)))]
         ))
     (define (find-urls elem r-loh)
       (cond
         [(string? elem) r-loh]
         [(cons? elem) (append (list-outlinking elem) r-loh)]
         [(link? elem) (cons (link-url elem) (find-urls (link-content elem) r-loh))]
         [(elem? elem) (find-urls (elem-content elem) r-loh)]
         ))
     )
    (list-outlinking loh))
  )

;checking the function
(check-expect (list-outlinks ex_html_1) (list "web.com/index.html"))
(check-expect (list-outlinks ex_html_2) (list "web.com/links.html" "web.com/index.html"))

;------------------------------------------------------------------------------------
;Exercise 6

;A function Page-Out is:
; takes a Webpage and returns a list of all the URLs that are linked to within the
; content of that webpage.
; Webpage(structure) -> LoS
(define (page-outlinks webpage)
  (list-outlinks (webpage-content webpage)))

;checking the function
(check-expect (page-outlinks IndexEx_1) (list "web.com/index.html"))
(check-expect (page-outlinks IndexEx_2) (list "web.com/links.html" "web.com/index.html"))
;------------------------------------------------------------------------------------
;Exercise 7

;A ListOfWebpage(LoW) is one of:
; - empty 
; - (cons webpage LoW)

;A function Get-Page is:
; takes a String representing a URL, a List of Webpages(LoW), and a default Webpage.
; String LoW(List) Webpage(structure) -> LoW(List)
(define (get-page url list web)
  (cond 
    [(empty? list) web]
    [else
     (cond
       [(equal? url (webpage-URL (first list))) (first list)]
       [else
       (get-page url (rest list) web)]
       )]
    ))


;example of LoW
(define webpage_list_example1 (list IndexEx_1 IndexEx_2))

;checking the function
(check-expect (get-page "web.com/links.html" webpage_list_example1 IndexEx_1) IndexEx_2)
(check-expect (get-page  "web.com/index.html" webpage_list_example1 IndexEx_1)
              (make-webpage "web.com/index.html" (list (make-elem "h1" (list "My Page"))(make-elem "p" (list (make-elem "a" (list (make-link "web.com/index.html" (list "Go ")))) (make-elem "em" (list "favorite")) "links.")))))

;------------------------------------------------------------------------------------
;Exercise 8

;A function List-Reference is:
; takes a Non-Empty-ListOfX(NeLoX) and a Natural Number i (which must be smaller than the
; lenth of the list) and returns the element in the list that is in the ith position.
; NeLoX Natural-Number -> One Element of NeLoX
(define (list-reference nelox i)
  (cond
    [(= i 0) (first nelox)]
    [else
     (list-reference (rest nelox) (- i 1))]
    ))

;checking the function
(check-expect (list-reference (list 1 4 9 16 25) 3) 16)
(check-expect (list-reference (list "one") 0) "one")

;------------------------------------------------------------------------------------
;Exercise 9

;A function Random-List-Reference is:
; takes a Non-Empty-ListOfX(NeLox) and returns the element in the list that is in the
; ith position.
; NeLoX -> One Element of NeLoX
(define (random-list-reference nelox)
  (list-reference nelox (random (length nelox))))

;------------------------------------------------------------------------------------
;Exercise 10

;A function Random-Surf is:
; take a webpage and a non-empty list of webpage and will return another webpage randomly
; selected from the webpages that are linked to in the given webpage. If there are no links
; contained in the given webpage than it should return a webpage randomly selected from
; the master list.
; Webpage(Structure) LoW(List) -> Webpage(Structure)
(define (random-surf webpage loh)
  (cond 
    [(empty? (page-outlinks webpage))(random-list-reference loh)]
    [else
     (get-page (page-outlinks webpage) loh (random-list-reference loh))]
    )) 
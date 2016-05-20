;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Adolf_and_Ha_Assignment_12_Graphs_and_Magic_Squares) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment 12: Graphs and Magic Squares

;-------------------------------------------------------------------------------------------------------
;Graphs

;A DirectedGraph is a:
; - [ListOf Pair]

;A Pair is a:
; - (list Node [ListOf Neighbot])

;A Node is a:
; - String

;A Neighbor is a:
; - String

;Such that for every node a in the graph there's a node c in the corresponding
; list of neighbors if and only if there is a vertex in the graph from a to c.

;Define Ticket-To-Ride is:
(define 
  ticket-to-ride
  '(
    ("vancouver" ("calgary" "seattle")) 
    ("calgary" ("vancouver" "seattle" "helena" "winnipeg")) 
    ("winnipeg" ("calgary" "helena" "duluth" "sault ste. marie")) 
    ("sault ste. marie" ("winnipeg" "duluth" "toronto" "montreal")) 
    ("montreal" ("boston" "new york" "toronto" "sault ste. marie")) 
    ("boston" ("montreal" "new york")) 
    ("new york" ("washington" "pittsburgh" "montreal" "boston")) 
    ("toronto" ("sault ste. marie" "montreal" "pittsburgh" "duluth" "chicago")) 
    ("pittsburgh" ("toronto" "new york" "washington" "raleigh" "nashville" "saint louis" "chicago"))  
    ("washington" ("new york" "pittsburgh" "raleigh"))
    ("raleigh" ("charleston" "atlanta" "nashville" "pittsburgh" "washington")) 
    ("charleston" ("raleigh" "atlanta" "miami")) 
    ("miami" ("charleston" "atlanta" "new orleans")) 
    ("atlanta" ("raleigh" "charleston" "miami" "new orleans" "nashville")) 
    ("nashville" ("saint louis" "little rock" "atlanta" "raleigh" "pittsburgh")) 
    ("chicago" ("pittsburgh" "saint louis" "toronto" "duluth" "omaha")) 
    ("saint louis" ("chicago" "pittsburgh" "nashville" "little rock" "kansas city"))
    ("little rock" ("nashville" "new orleans" "dallas" "oklahoma city" "saint louis")) 
    ("new orleans" ("houston" "little rock" "atlanta" "miami")) 
    ("houston" ("el paso" "dallas" "new orleans")) 
    ("dallas" ("little rock" "houston" "el paso" "oklahoma city")) 
    ("oklahoma city" ("denver" "kansas city" "little rock" "dallas" "el paso" "santa fe")) 
    ("kansas city" ("omaha" "saint louis" "oklahoma city" "denver")) 
    ("omaha" ("helena" "duluth" "chicago" "kansas city" "denver")) 
    ("duluth" ("winnipeg" "sault ste. marie"  "toronto" "chicago" "omaha" "helena")) 
    ("helena" ("seattle" "calgary" "winnipeg" "duluth" "omaha" "denver" "salt lake city")) 
    ("salt lake city" ("portland" "helena" "denver" "las vegas" "san francisco")) 
    ("denver" ("salt lake city" "helena" "omaha" "kansas city" "oklahoma city" "santa fe" "phoenix"))
    ("santa fe" ("denver" "oklahoma city" "el paso" "phoenix")) 
    ("el paso" ("los angeles" "phoenix" "santa fe" "oklahoma city" "dallas" "houston")) 
    ("phoenix" ("los angeles" "denver" "santa fe" "el paso")) 
    ("las vegas" ("salt lake city" "los angeles")) 
    ("san francisco" ("portland" "salt lake city" "los angeles")) 
    ("los angeles" ("san francisco" "las vegas" "phoenix" "el paso")) 
    ("portland" ("seattle" "salt lake city" "san francisco")) 
    ("seattle" ("vancouver" "calgary" "helena" "portland"))
    ))

;--------------------------------------------------------------------------------
;Exercise 1

;A function Neighbors-Of is:
; takes a node and a graph and returns the list of neighbors of that node in the graph.
; String LoP -> ListOfString
(define (neighbors-of str dgraph)
  (cond
    [(empty? dgraph) empty]
    [(string=? (first (first dgraph)) str) (first (rest (first dgraph)))]
    [else (neighbors-of str (rest dgraph))]
    ))

;checking the function
(check-expect (neighbors-of "phoenix" ticket-to-ride)
              (list "los angeles" "denver" "santa fe" "el paso"))

;--------------------------------------------------------------------------------
;Exercise 2

; InversePath Graph -> [ListOf InversePath]
; collects all lists (cons a path) where a is neighbor of (first path)
(define (extend path graph)
  (map (lambda (city) (cons city path))
       (neighbors-of (first path) graph)))

;checking the function
(check-expect (extend (list "new york") ticket-to-ride)
              (list (list "washington" "new york")
                    (list "pittsburgh" "new york")
                    (list "montreal" "new york")
                    (list "boston" "new york")))

;Define Simple-Graph is:
(define simple-graph
  '(("A" ("B"))
    ("B" ("A" "C" "D"))
    ("C" ("B" "E"))
    ("D" ("B"))
    ("E" ("C"))
    ))

; [ListOf InversePath] graph -> [ListOf InversePath]
; extends every path in every possible way while avoiding cycles
(define (one-step paths graph)
  (cond [(empty? paths) empty]
        [else (clean (apply append (map (lambda (path)
                                          (extend path graph))
                                        paths)))]))

;A function Clean is:
; takes a list of Paths(LoP) and removes those where the first element
; of the list representing the inverse path appears elsewhere in the
; inverse path.
; LoP -> LoP
(define (clean lop)
  (cond
    [(empty? lop) empty]
    [(empty? (rest (rest (first lop)))) (cons (first lop) (clean (rest lop)))]
    [(string=? (first (first lop)) (first (rest (rest (first lop))))) (clean (rest lop))]
    [else (cons (first lop) (clean (rest lop)))]
    ))

;checking the function
(check-expect (one-step (list (list "A")) simple-graph)
              (list (list "B" "A")))
(check-expect (one-step (one-step (list (list "A")) simple-graph) simple-graph)
              (list (list "C" "B" "A") (list "D" "B" "A")))

;--------------------------------------------------------------------------------
;Exercise 3

;A function Find-Paths is:
; takes three arguments: a starting node, an end (or target) node, and a graph and
; returns the shortest path (where the length of the path is expressed as the number
; of node in it) between the start node and the target node, or empty if no such
; path exists. If there are more than one path of the same length this function
; returns them all.
;String String LoP -> LoP
(define (find-paths start end graph)
  (local
     (;String String LoP LoP -> LoP
     (define (finding sta en gra)
       (cond
         [(not (and (searching (first (first sta)) gra) (searching en gra))) empty]
         [(matching en sta) (collecting en sta)]
         [else (finding (one-step sta gra) en gra)]
         ))
     ;String LoP -> Boolean
     (define (searching str gra)
       (cond
         [(empty? gra) false]
         [(string=? str (first (first gra))) true]
         [else (searching str (rest gra))]
         ))
     ;String LoP -> Boolean
     (define (matching str lop)
       (cond
         [(empty? lop) false]
         [(string=? str (first (first lop))) true]
         [else (matching str (rest lop))]
         ))
     ;String LoP -> LoP
     (define (collecting str lop)
       (cond
         [(empty? lop) empty]
         [(string=? str (first (first lop))) (cons (reverse (first lop)) (collecting str (rest lop)))]
         [else (collecting str (rest lop))]
         ))
     )(finding (list (list start)) end graph)
    ))

;checking the function
(check-expect
 (find-paths "vancouver" "montreal" ticket-to-ride)
 (list (list "vancouver" "calgary" "winnipeg" "sault ste. marie" "montreal")))
(check-expect (find-paths "atlanta" "new york" ticket-to-ride) (list
                                                                (list "atlanta" "raleigh" "pittsburgh" "new york")
                                                                (list "atlanta" "raleigh" "washington" "new york")
                                                                (list "atlanta" "nashville" "pittsburgh" "new york")))
(check-expect (find-paths "madrid" "berlin" ticket-to-ride) empty)
(check-expect (find-paths "A" "Z" simple-graph) empty)
(check-expect (find-paths "indianapolis" "chicago" ticket-to-ride) empty)

;--------------------------------------------------------------------------------
;Exercise 4

;Define Weight is:
(define weights 
  (list
   (list "atlanta" "nashville" 1)
   (list "atlanta" "new orleans" 4)
   (list "atlanta" "miami" 5)
   (list "atlanta" "charleston" 2)
   (list "atlanta" "raleigh" 2)
   (list "boston" "new york" 2)
   (list "boston" "montreal" 2)
   (list "calgary" "winnipeg" 6)
   (list "calgary" "helena" 4)
   (list "calgary" "seattle" 4)
   (list "calgary" "vancouver" 3)
   (list "charleston" "miami" 4)
   (list "charleston" "atlanta" 2)
   (list "charleston" "raleigh" 2)
   (list "chicago" "omaha" 4)
   (list "chicago" "duluth" 3)
   (list "chicago" "toronto" 4)
   (list "chicago" "saint louis" 2)
   (list "chicago" "pittsburgh" 3)
   (list "dallas" "oklahoma city" 2)
   (list "dallas" "el paso" 4)
   (list "dallas" "houston" 1)
   (list "dallas" "little rock" 2)
   (list "denver" "phoenix" 5)
   (list "denver" "santa fe" 2)
   (list "denver" "oklahoma city" 4)
   (list "denver" "kansas city" 4)
   (list "denver" "omaha" 4)
   (list "denver" "helena" 4)
   (list "denver" "salt lake city" 3)
   (list "duluth" "helena" 6)
   (list "duluth" "omaha" 2)
   (list "duluth" "chicago" 3)
   (list "duluth" "toronto" 6)
   (list "duluth" "sault ste. marie" 3)
   (list "duluth" "winnipeg" 4)
   (list "el paso" "houston" 6)
   (list "el paso" "dallas" 4)
   (list "el paso" "oklahoma city" 5)
   (list "el paso" "santa fe" 2)
   (list "el paso" "phoenix" 3)
   (list "el paso" "los angeles" 6)
   (list "helena" "salt lake city" 3)
   (list "helena" "denver" 4)
   (list "helena" "omaha" 5)
   (list "helena" "duluth" 6)
   (list "helena" "winnipeg" 4)
   (list "helena" "calgary" 4)
   (list "helena" "seattle" 6)
   (list "houston" "new orleans" 2)
   (list "houston" "dallas" 1)
   (list "houston" "el paso" 6)
   (list "kansas city" "denver" 4)
   (list "kansas city" "oklahoma city" 2)
   (list "kansas city" "saint louis" 2)
   (list "kansas city" "omaha" 1)
   (list "las vegas" "los angeles" 2)
   (list "las vegas" "salt lake city" 3)
   (list "little rock" "saint louis" 2)
   (list "little rock" "oklahoma city" 2)
   (list "little rock" "dallas" 2)
   (list "little rock" "new orleans" 3)
   (list "little rock" "nashville" 3)
   (list "los angeles" "el paso" 6)
   (list "los angeles" "phoenix" 3)
   (list "los angeles" "las vegas" 2)
   (list "los angeles" "san francisco" 3)
   (list "miami" "new orleans" 6)
   (list "miami" "atlanta" 5)
   (list "miami" "charleston" 4)
   (list "montreal" "sault ste. marie" 5)
   (list "montreal" "toronto" 3)
   (list "montreal" "new york" 3)
   (list "montreal" "boston" 2)
   (list "nashville" "pittsburgh" 4)
   (list "nashville" "raleigh" 3)
   (list "nashville" "atlanta" 1)
   (list "nashville" "little rock" 3)
   (list "nashville" "saint louis" 2)
   (list "new orleans" "miami" 6)
   (list "new orleans" "atlanta" 4)
   (list "new orleans" "little rock" 3)
   (list "new orleans" "houston" 2)
   (list "new york" "boston" 2)
   (list "new york" "montreal" 3)
   (list "new york" "pittsburgh" 2)
   (list "new york" "washington" 2)
   (list "oklahoma city" "santa fe" 3)
   (list "oklahoma city" "el paso" 5)
   (list "oklahoma city" "dallas" 2)
   (list "oklahoma city" "little rock" 2)
   (list "oklahoma city" "kansas city" 2)
   (list "oklahoma city" "denver" 4)
   (list "omaha" "denver" 4)
   (list "omaha" "kansas city" 1)
   (list "omaha" "chicago" 4)
   (list "omaha" "duluth" 2)
   (list "omaha" "helena" 5)
   (list "phoenix" "el paso" 3)
   (list "phoenix" "santa fe" 3)
   (list "phoenix" "denver" 5)
   (list "phoenix" "los angeles" 3)
   (list "pittsburgh" "chicago" 3)
   (list "pittsburgh" "saint louis" 5)
   (list "pittsburgh" "nashville" 4)
   (list "pittsburgh" "raleigh" 2)
   (list "pittsburgh" "washington" 2)
   (list "pittsburgh" "new york" 2)
   (list "pittsburgh" "toronto" 2)
   (list "portland" "san francisco" 5)
   (list "portland" "salt lake city" 6)
   (list "portland" "seattle" 1)
   (list "raleigh" "washington" 2)
   (list "raleigh" "pittsburgh" 2)
   (list "raleigh" "nashville" 3)
   (list "raleigh" "atlanta" 2)
   (list "raleigh" "charleston" 2)
   (list "saint louis" "kansas city" 2)
   (list "saint louis" "little rock" 2)
   (list "saint louis" "nashville" 2)
   (list "saint louis" "pittsburgh" 5)
   (list "saint louis" "chicago" 2)
   (list "salt lake city" "san francisco" 5)
   (list "salt lake city" "las vegas" 3)
   (list "salt lake city" "denver" 3)
   (list "salt lake city" "helena" 3)
   (list "salt lake city" "portland" 6)
   (list "san francisco" "los angeles" 3)
   (list "san francisco" "salt lake city" 5)
   (list "san francisco" "portland" 5)
   (list "santa fe" "phoenix" 3)
   (list "santa fe" "el paso" 2)
   (list "santa fe" "oklahoma city" 3)
   (list "santa fe" "denver" 2)
   (list "sault ste. marie" "montreal" 5)
   (list "sault ste. marie" "toronto" 2)
   (list "sault ste. marie" "duluth" 3)
   (list "sault ste. marie" "winnipeg" 6)
   (list "seattle" "portland" 1)
   (list "seattle" "helena" 6)
   (list "seattle" "calgary" 4)
   (list "seattle" "vancouver" 1)
   (list "toronto" "chicago" 4)
   (list "toronto" "duluth" 6)
   (list "toronto" "pittsburgh" 2)
   (list "toronto" "montreal" 3)
   (list "toronto" "sault ste. marie" 2)
   (list "vancouver" "seattle" 1)
   (list "vancouver" "calgary" 3)
   (list "washington" "raleigh" 2)
   (list "washington" "pittsburgh" 2)
   (list "washington" "new york" 2)
   (list "winnipeg" "sault ste. marie" 6)
   (list "winnipeg" "duluth" 4)
   (list "winnipeg" "helena" 4)
   (list "winnipeg" "calgary" 6)))

; Node Node [ListOf (list Node Node Number)] -> Number
; finds associated weight of the given arc/edge in the graph
(define (find-weight node neighbor weights)
  (cond ((empty? weights) false)
        ((and (equal? node (first (first weights)))
              (equal? neighbor (second (first weights))))
         (third (first weights)))
        (else (find-weight node neighbor (rest weights)))))

;checking the function
(check-expect (find-weight "toronto" "montreal" weights) 3)
(check-expect (find-weight "sault ste. marie" "winnipeg" weights) 6)
(check-expect (find-weight "chicago" "indianapolis" weights) false)

;A function Measure is:
; takes a path and a set of weights and calculates the weight of the entire
; path (as the sum of the weights on its arcs).
; ListOfString LoP -> Number
(define (measure los graph)
  (cond
    [(or (empty? los) (empty? (rest los))) 0]
    [else
     (+ (find-weight (first los) (second los) weights) (measure (rest los) graph))]
    ))

;checking the function
(check-expect (measure (list "atlanta" "raleigh" "washington" "new york") weights) 6)
(check-expect
 (sort (map (lambda (path) (list path (measure path weights)))
            (find-paths "los angeles" "helena" ticket-to-ride))
       (lambda (pair1 pair2)
         (< (second pair1)
            (second pair2))))
 (list (list (list "los angeles" "las vegas" "salt lake city" "helena") 8)
       (list (list "los angeles" "san francisco" "salt lake city" "helena") 11)
       (list (list "los angeles" "phoenix" "denver" "helena") 12)))

;-------------------------------------------------------------------------------------------------------
;Magic Squares

; A Matrix is:
; -- [ListOf [ListOf Number]]
; additional requirement: all lists of same length
(define magic '((16  3  2 13)
                ( 5 10 11  8)
                ( 9  6  7 12)
                ( 4 15 14  1)))

;example function
(define row-sums (map (lambda (x) (apply + x)) magic))
(check-expect row-sums (list 34 34 34 34))

;--------------------------------------------------------------------------------
;Exercise 5

;A function Extract-Column is:
; receives two arguments: a matrix and an integers. The function should extract the
; indicated column from the matrix and return it as a list of numbers.
; Number Matrix -> ListOfNumber
(define (extract-column num squ)
  (local
    (;Number Square Number -> ListOfNumber
     (define (extracting row squ count)
       (cond
         [(empty? squ) empty]
         [(= row count) (cons (first (first squ)) (extracting row (rest squ) 1))]
         [else (extracting row (cons (rest (first squ)) (rest squ)) (+ 1 count))]
         ))
     ;Number Matrix Number -> Number
     (define (finding_row num squ count)
       (cond
         [(empty? (first squ)) (finding_row num (rest squ) 1)]
         [(= num (first (first squ))) count]
         [else (finding_row num (cons (rest (first squ)) (rest squ)) (+ 1 count))]
         ))
     )(extracting (finding_row num squ 1) squ 1)
    ))

;checking the function
(check-expect (extract-column 2 magic) '(2 11 7 14))

;--------------------------------------------------------------------------------
;Exercise 6

;A function Transpose is:
; the transpose of a matrix matrix is another matrix (transpose matrix) created
; by any one of the following equivalent actions: reflect matrix over its main
; diagonal, write the rows of matrix as the columns of the transpose, or write the
; columns of matrix as the rows of the transpose. In this exercise you are to write
; transpose using the function designed in the previous exercise (extract-column) and
; the third of the approaches listed above.
; Matrix -> Matrix
(define (transpose matrix)
  (local
    (;Matrix Number -> Matrix
     (define (transposing mat count)
       (cond
         [(> count (length mat)) empty]
         [else (cons (collecting count mat 1) (transposing mat (+ 1 count)))]
         ))
     ;Number Matrix Number -> ListOfNumber
     (define (collecting row squ count)
       (cond
         [(empty? squ) empty]
         [(= row count) (cons (first (first squ)) (collecting row (rest squ) 1))]
         [else (collecting row (cons (rest (first squ)) (rest squ)) (+ 1 count))]
         ))
     )(transposing matrix 1)
    ))

;checking the function
(check-expect (transpose magic)
              (list (list 16  5 9  4)
                    (list  3 10 6 15)
                    (list  2 11 7 14)
                    (list 13 8 12  1)))

;--------------------------------------------------------------------------------
;Exercise 7

;A function Diagonals is:
; extracts the diagonals in a matrix received as input.
; Matrix -> [ListOf (ListOfNumber)]
(define (diagonals matrix)
  (local
    (;Matrix Number -> ListOfNumber
     (define (s-left gra count)
       (cond
         [(> count (length gra)) empty]
         [else (cons (collecting_num_l count (collecting_row count gra 1) 1) (s-left gra (+ 1 count)))]
         ))
     ;Matrix Number Number -> ListOfNumber
     (define (s-right gra count1 count2)
       (cond
         [(> count1 (length gra)) empty]
         [else (cons (collecting_num_r count2 (collecting_row count1 gra 1) 1) (s-right gra (+ 1 count1) (- count2 1)))]
         ))
     ;Number Matrix Number -> ListOfNumber
     (define (collecting_row row squ count)
       (cond
         [(empty? squ) empty]
         [(= row count) (cons (first (first squ)) (collecting_row row (rest squ) 1))]
         [else (collecting_row row (cons (rest (first squ)) (rest squ)) (+ 1 count))]
         ))
     ;Number ListOfNumber Number -> Number
     (define (collecting_num_l ord lon count)
       (cond
         [(= ord count) (first lon)]
         [else (collecting_num_l ord (rest lon) (+ 1 count))]
         ))
     ;Number ListOfNumber Number -> Number
     (define (collecting_num_r ord lon count)
       (cond
         [(= ord count) (first lon)]
         [else (collecting_num_r ord (rest lon) (+ 1 count))]
         ))
     )(list (s-left matrix 1) (reverse (s-right matrix 1 (length matrix))))
    ))

;checking the function
(check-expect (diagonals '((16  3  2 13)
                           ( 5 10 11  8)
                           ( 9  6  7 12)
                           ( 4 15 14  1)))
              '((16 10  7  1) (13 11  6  4)))
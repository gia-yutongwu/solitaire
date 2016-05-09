;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solitaire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "a10lib.rkt")

;;  
;;***************************************************   
;;Yutong Wu (20553361)  
;;CS 135 Fall 2014  
;;Assignment 10, Problem 2 (solitaire)  
;;***************************************************  
;;

;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg

(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (a)

;; (build-board d) consumes a Dimension (d) and produces a (listof (listof Peg)) corresponding to 
;;   a list of rows, with each row containing a list of Peg positions in that row
;; build-board: Dimension -> (listof (listof Peg))
;; examples: 
(check-expect (build-board 3) '((11 12 13) (21 22 23) (31 32 33)))
(check-expect (build-board 2) '((11 12) (21 22)))

(define (build-board d)
  (build-list d (lambda (row) (build-list d (lambda (column) (+ (* 10 (add1 row)) (add1 column)))))))

;; tests:
(check-expect (build-board 1) '((11)))
(check-expect (build-board 4) '((11 12 13 14) (21 22 23 24) (31 32 33 34) (41 42 43 44)))

;; (b)

;; (state->los board state) consumes a Board and a State and 
;;   produces a (listof Str) corresponding to one string per row
;; state->los: Board State -> (listof Str)
;; examples:
(check-expect (state->los '(2 (21 22)) '(11)) '("O." "  "))
(check-expect (state->los '(3 ()) '(13 22 23 31 33)) '("..O" ".OO" "O.O"))

(define (state->los board state)
  (map (lambda (lst) (foldr string-append "" lst))
       (map (lambda (y) (map (lambda (x) (cond [(member? x state) "O"]
                                               [(member? x (second board)) " "]
                                               [else "."])) y)) (build-board (first board)))))

;; tests:
(check-expect (state->los '(4 (41 42 43 44)) '(22 23)) '("...." ".OO." "...." "    "))
(check-expect (state->los '(4 (11 14 41 44)) '(12 13 21 32)) '(" OO " "O..." ".O.." " .. "))

;; (c)

;; (make-solved? soln) consumes a Solution and generates a new predicate function that consumes 
;;   a State and produces true if the state is a solution, and false otherwise.
;; make-solved?: Solution -> (State -> Bool)
;; examples:
(check-expect ((make-solved? 'any) '(34)) true)
(check-expect ((make-solved? 24) '(22 23)) false)

(define (make-solved? soln) 
  (lambda (state) (cond [(and (equal? soln 'any) (empty? (rest state))) true]
                        [(and (equal? (first state) soln) (empty? (rest state))) true]
                        [else false])))

;; tests:
(check-expect ((make-solved? 67) '(11 12)) false)
(check-expect ((make-solved? 'any) '(56 58 99)) false)
(check-expect ((make-solved? 99) '(22 23)) false)
(check-expect ((make-solved? 58) '(58)) true)

;; (d)

;; (four-direction peg) produces the two consecutive pegs of peg in four directions
;; four-direction: Peg -> (listof (listof Peg))
;; requires: Two consecutive pegs of Peg in four directions must exist
;; examples:
(check-expect (four-direction 34) '((24 14) (33 32) (35 36) (44 54)))
(check-expect (four-direction 66) '((56 46) (65 64) (67 68) (76 86))) 

(define (four-direction peg)
  (list (list (- peg 10) (- peg 20))
        (list (- peg 1) (- peg 2))
        (list (+ peg 1) (+ peg 2))
        (list (+ peg 10) (+ peg 20))))

;; tests:
(check-expect (four-direction 68) '((58 48) (67 66) (69 70) (78 88)))
(check-expect (four-direction 55) '((45 35) (54 53) (56 57) (65 75)))

;; (flat lst) creates a list by flatting every list in lst
;; flat: (listof Any) -> (listof Any)
;; examples:
(check-expect (flat '(((2) (3))(4) (5))) '((2) (3) 4 5))
(check-expect (flat '(("cs") ("135") ("rocks"))) '("cs" "135" "rocks"))

(define (flat lst)
  (cond [(empty? lst) empty]
        [(empty? (rest (first lst))) (cons (first (first lst)) (flat (rest lst)))]
        [else (append (cons (first (first lst)) (rest (first lst))) (flat (rest lst)))]))

;; tests:
(check-expect (flat '((Emma) (Stone) (Andrew) (Garfield))) '(Emma Stone Andrew Garfield))
(check-expect (flat '()) '())

;; (neighbours board state) produces a list of states after one valid move on board
;; neighbours: Board State -> (listof States)
;; examples:
(check-expect (neighbours '(3 ()) '(13 22 23 31 33)) '((13 21 31 33)))
(check-expect (neighbours '(3 (31 32 33)) '()) '())

(define (neighbours board state)
  (local [;; (possi-state-single peg state) creates a list of pairs that are in the state 
          ;;   and are also in the four directions of peg
          ;; possi-state-single: Peg State -> (listof (list Peg Peg))
          (define (possi-state-single peg state board)
            (map (lambda (y) (cons (second y) (filter (lambda (x) (and (not (= x peg))
                                                                       (not (= x (first y))))) state)))
                 (filter (lambda (direction) (and (member? (first direction) state)
                                                  (not (member? (second direction) state))
                                                  (not (member? (second direction) (second board))) 
                                                  (ormap (lambda (x) (member? (second direction) x)) 
                                                         (build-board (first board))))) 
                         (four-direction peg))))
          ;; (possi-state-lop lop state) creates a list of states representing 
          ;;   all possibilities after every peg in lop is occupied
          ;; possi-state-lop: (listof Peg) State -> (listof State)
          (define (possi-state-lop lop state board)
            (cond [(empty? lop) empty]
                  [else (cons (possi-state-single (first lop) state board) 
                              (possi-state-lop (rest lop) state board))]))]          
    (map (lambda (lst) (quicksort lst <)) 
         (flat (filter (lambda (lst) (not (equal? empty lst))) (possi-state-lop state state board))))))

;; tests:
(check-expect (neighbours '(7 ()) '(24 25 34 35))
              '((26 34 35) (25 35 44) (23 34 35) (24 34 45) (14 25 35) (24 25 36) (15 24 34) (24 25 33)))
(check-expect (neighbours '(4 (41 42 43 44)) '(22 23)) '((24) (21)))
(check-expect (neighbours '(4 (11 14 41 44)) '(12 13 21 32)) '())

;; (e)

;; (solitaire board state soln) consumes a Board(board), a State(state) and a Solution(soln) and produces 
;;   a list of states representing the route to the final answer or false if no solution exists
;; solitaire: Board State Solution -> (anyof false (listof State))
;; examples: 
(check-expect (solitaire '(7 ()) '(24 25 34 35) 'any) '((24 25 34 35) (26 34 35) (26 36) (46)))
(check-expect (solitaire '(3 ()) '(11 12 22 23 31) 'any) false)

(define (solitaire board state soln)
  (local [;; (make-neighbours board) consumes a Board(board) and produces a function which
          ;;   consumes a State and produces it neighbours
          ;; make-neighbours: Board -> (State -> (listof State))
          (define (make-neighbours board) (lambda (state) (neighbours board state)))]
    (find-route state (make-neighbours board) (make-solved? soln))))

;; tests:
(check-expect (solitaire '(4 (41 42 43 44)) '(22 23) 21) '((22 23) (21)))
(check-expect (solitaire '(4 (41 42 43 44)) '(22 23) 24) '((22 23) (24)))

;; (f)

;; (result->text board result) creates a list of lists of strings, visualizing the route of 
;;   finding an answer by consumeing a Board(board) and a result from solitaire 
;;   if no solution exists, produces a constant, no-solution-text
;; result->text: Board (anyof false (listof State)) -> (listof (listof Str))
;; examples: 
(check-expect (result->text '(3 ()) '((11 12) (13))) '(("OO." "..." "...") ("..O" "..." "...")))
(check-expect (result->text '(4 (41 42 43 44)) '((22 23) (24))) 
              '(("...." ".OO." "...." "    ") ("...." "...O" "...." "    ")))

(define (result->text board result)
  (cond [(boolean? result) no-solution-text]
        [else (map (lambda (state) (state->los board state)) result)]))

;; tests:
(check-expect (result->text '(3 ()) false) '(("No Solution Found")))
(check-expect (result->text '(7 ()) '((24 25 34 35) (26 34 35) (26 36) (46)))
              '(("......." "...OO.." "...OO.." "......." "......." "......." ".......")
                ("......." ".....O." "...OO.." "......." "......." "......." ".......")
                ("......." ".....O." ".....O." "......." "......." "......." ".......")
                ("......." "......." "......." ".....O." "......." "......." ".......")))

(show (result->text cross (solitaire cross cross/big-diamond 'any)))

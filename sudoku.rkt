#lang racket

;testing data
(define n
  3)

(define sudoku1
  '((1 0 0 0 0 0 0 8 0)
    (0 8 0 9 0 3 1 0 5)
    (0 5 9 0 0 0 0 2 7)
    (0 0 0 3 9 0 5 0 0)
    (0 0 0 2 5 0 0 0 0)
    (0 0 0 0 0 7 0 0 9)
    (0 0 0 0 0 0 0 0 0)
    (7 0 3 0 0 2 6 0 1)
    (8 0 6 1 0 0 2 0 0)))

(define sudoku2
   '((0 0 0 0 0 4 1 5 0)
     (0 7 2 8 6 1 3 9 0)
     (1 3 4 5 7 0 6 2 0)
     (9 1 0 6 0 0 0 0 2)
     (0 0 0 0 0 8 7 0 0)
     (0 5 7 0 0 0 4 0 6)
     (0 0 9 0 0 0 0 0 1)
     (7 4 0 0 8 2 0 0 0)
     (0 0 0 7 9 0 2 0 0)))

(define sudokuEvil
  '((7 0 0 0 0 0 0 0 9)
    (0 9 2 0 0 8 0 5 0)
    (3 0 0 2 0 0 0 0 0)
    (9 0 0 0 0 0 0 0 0)
    (0 0 0 0 6 0 0 8 0)
    (0 1 4 7 0 0 0 0 5)
    (0 0 0 0 0 7 4 0 0)
    (0 0 3 0 0 0 0 0 0)
    (0 2 5 4 0 0 0 0 1)))

(define sudokuBad
  '((0 0 0 0 0 4 1 5 0)
     (0 7 2 8 6 1 3 9 0)
     (1 3 4 5 7 0 6 2 0)
     (9 1 0 6 0 0 0 2)
     (0 0 0 0 0 8 7 0 0)
     (0 5 7 0 0 0 4 0 6)
     (0 0 9 0 0 0 0 0 1)
     (7 4 0 0 8 2 0 0 0)
     (0 0 0 7 9 0 2 0 0)))

(define n2
  4)

(define sudokuBig
  '((4 8 7 15 16 14 0 12 5 0 0 2 10 13 3 0)
    (0 0 10 14 13 0 3 0 9 0 16 4 0 0 7 15)
    (0 16 9 0 0 10 15 1 0 3 13 6 0 14 12 0)
    (0 6 0 13 11 0 7 0 15 14 0 8 0 16 0 0)
    (0 0 15 0 0 7 12 0 13 0 0 0 4 0 14 16)
    (12 0 0 16 2 15 0 6 14 0 0 7 11 0 0 0)
    (0 2 0 9 0 0 16 0 4 5 0 0 3 12 8 7)
    (11 7 0 0 5 0 14 0 1 16 3 12 15 2 0 0)
    (5 4 0 0 9 0 10 15 16 0 14 13 8 3 0 0)
    (0 0 0 10 0 0 13 0 8 2 0 0 12 7 16 0)
    (8 0 0 0 6 16 0 11 0 4 0 9 2 15 0 0)
    (0 0 16 0 7 4 0 0 0 0 0 0 9 0 13 0)
    (0 14 0 5 10 0 2 0 0 8 0 3 0 0 15 0)
    (0 12 11 0 15 0 5 7 2 13 4 10 0 8 9 0)
    (0 0 1 2 3 0 0 0 12 0 7 14 0 0 0 5)
    (0 15 4 0 0 0 0 0 6 0 5 16 7 10 2 0)))


(define n3
  2)

(define sudokuSmall
  '((0 0 0 0)
    (4 2 0 0)
    (0 0 1 0)
    (1 0 0 3)))

;gets sudoku represented as 2D list of cells, with empty cells represented as zeroes
(define (sudoku-solver lst n)
  (if (checkInput lst n 0)
  (let
      [(result (sudoku-solver-aux lst 1 0 0 n))]
    (if (car result)
        (cdr result)
        '(n e m a _ r i e s e n i e)))
  '(z l y _ r o z m e r _ v s t u p u))
)

(define (checkInput lst n aux)
  (if (null? lst)
      (equal? aux (* n n))
      (if (checkInputRow (car lst) n 0)
          (checkInput (cdr lst) n (+ 1 aux))
          #f)))
      
(define (checkInputRow lst n aux)
  (if (null? lst)
      (equal? aux (* n n))
      (checkInputRow (cdr lst) n (+ 1 aux))
      ))


;recursive function that iterates through every cell of the sudoku grid, implements sudoku brute force algorythm
(define (sudoku-solver-aux lst aux x y n)
  (if (checkIfEmpty lst x y)
      (if (> aux (* n n))
          (cons #f lst)
          (let
              [(sudoku (buildCols lst x y aux 0))]
            (if (> (checkRow (get2row sudoku y) aux n 0) 1)
                (sudoku-solver-aux lst (+ aux 1) x y n)
                (if (> (checkRow (getCols sudoku x 0) aux n 0) 1)
                    (sudoku-solver-aux lst (+ aux 1) x y n)
                    (if (> (checkRow (checkCel sudoku (- x (remainder x n)) (- y (remainder y n)) n) aux n 0) 1)
                        (sudoku-solver-aux lst (+ aux 1) x y n)
                        (if (equal? x (- (* n n) 1))
                            (if (equal? y (- (* n n) 1))
                                (cons #t sudoku)
                                (let
                                    [(result (sudoku-solver-aux sudoku 1 0 (+ y 1) n))]
                                  (if (car result)
                                      result
                                      (sudoku-solver-aux lst (+ aux 1) x y n)
                                      )))
                            (let    
                                [(result (sudoku-solver-aux sudoku 1 (+ x 1) y n))]
                              (if (car result)
                                  result
                                  (sudoku-solver-aux lst (+ aux 1) x y n)
                                  )
                              )))))))
      (if (equal? x (- (* n n) 1))
          (if (equal? y (- (* n n) 1))
              (cons #t lst)
              (sudoku-solver-aux lst 1 0 (+ y 1) n))
          (sudoku-solver-aux lst 1 (+ x 1) y n))
      ))

;checks if cell at given coords is empty or nor
(define (checkIfEmpty lst x y)
  (if (equal? 0 (get2Col (get2row lst y) x 0))
      #t
      #f))
  

;pseudo assignment operator
(define (buildCols lst x y new auxY)
  (if (equal? y auxY)
      (cons (buildRow (car lst) x new 0) (cdr lst))
      (cons (car lst) (buildCols (cdr lst) x y new (+ auxY 1)))))
;row part of the pseudo assignment operator
(define (buildRow lst x new auxX)
  (if (equal? x auxX)
      (if (equal? (car lst) 0)
          (cons new (cdr lst))
          lst)
      (cons (car lst) (buildRow (cdr lst) x new (+ 1 auxX)))))

;return row at given y coord
(define (get2row lst y)
  (if (zero? y)
      (car lst)
      (get2row (cdr lst) (- y 1))
      ))

;counts occurences of 'a' in a given 1D list 
(define (checkRow lst a n aux)
  (if (null? lst)
         aux
         (if (equal? a (car lst))
                 (checkRow (cdr lst) a n (+ aux 1))
                 (checkRow (cdr lst) a n aux))))

;returns value of column x at given row
(define (get2Col lst x aux)
  (if (equal? x aux)
      (car lst)
      (get2Col (cdr lst) x (+ aux 1))))

;return list consisting of column elements
(define (getCols lst x aux)
  (if (null? lst)
      lst
      (cons (get2Col (car lst) x 0) (getCols (cdr lst) x (+ aux 1)))))

;return list consisting of 1/n^2 part of sudoku elements
(define (checkCel lst x y n)
  (if (zero? y)
      (buildCellList lst x 0 '() n)
      (checkCel (cdr lst) x (- y 1) n))
)

;helping functions for putting the list together from now on
(define (buildCellList lst x aux outputList n)
  (if (< aux n)
      (buildCellList (cdr lst) x (+ 1 aux) (buildRowSublist (car lst) x outputList n) n)
      outputList))

(define (buildRowSublist lst x outputList n)
  (if (zero? x)
      (buildSubList lst 0 outputList n)
      (buildRowSublist (cdr lst) (- x 1) outputList n)))

(define (buildSubList lst aux outputList n)
  (if (< aux n)
      (cons (car lst) (buildSubList (cdr lst) (+ 1 aux) outputList n))
      outputList))
;till here

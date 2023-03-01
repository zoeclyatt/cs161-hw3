;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     )
	   )
	)
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 (list x row)
		 (getKeeperPosition (cdr s) (+ row 1))
		 )
	       )
	 )
	)
  )

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;helper function for goal-test-- does the same thing as member
;but we're not allowed to use that so i made my own
(defun myMem (x l)
   (cond ((null l) nil)
         ((equal x (car l)) t)
         (t (myMem x (cdr l)))
   )
)

(defun goal-test (s)
   (cond ((null s) t)
         ;if car s has a box not on a star, return nil
         ((myMem '2 (car s)) nil)
         (t (goal-test (cdr s)))
   )
);end defun

;test cases for goal-test
;(print (goal-test `((0 0 0) (0 0 0) (0 0 0))))
;(print (goal-test `((0 0 2) (0 0 0) (0 0 0))))
;(print (goal-test `((0 0 0) (0 0 3) (0 0 0))))
;(print (goal-test `((0 0 0) (0 0 0) (2 0 0))))

;test cases for myMem
;(print (myMem '1 `(1 2 3)))
;(print (myMem '1 `(2 3 4)))
;(print (myMem '1 `()))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

;helper function for moveRow
(defun moveCol (r thing c)
   (cond ((null r) nil)
         ;if it's reached the goal column, replace square value with thing and append the rest of the row
         ((equal c 0) (append (list thing) (cdr r)))
         ;else, append the results of checking and replacing the rest of the list to the beginning
         (t (append (list (car r)) (moveCol (cdr r) thing (- c 1))))
   )
)

;test cases for moveCol
;(print (moveCol `(0 0 0 1) 1 2))
;(print (moveCol (moveCol `(0 0 0 1) 1 2) 0 3))

;helper function for try-move
;same concept as moveCol but recursively going through rows instead of columns and replacing the target
;row with the results of replacing the target column of that row using moveCol
(defun moveRow (s thing c r)
   (cond ((null s) nil)
         ((equal r 0) (append (list (moveCol (car s) thing c)) (cdr s)))
         (t (append (list (car s)) (moveRow (cdr s) thing c (- r 1))))
   )
)

;test case for moveRow
;(print (moveRow (moveRow `((0 0 0 0) (0 0 0 0) (0 0 1 0) (0 0 0 0)) 1 0 0) 0 2 2))

;helper function for getting length of list to check if trying to move out of bounds
(defun myLen (l)
   (cond ((null l) 0)
         ((null (cdr l)) 1)
         (t (+ 1 (myLen (cdr l))))
   )
) 

;test cases for myLen
;(print (myLen `()))
;(print (myLen `(0)))
;(print (myLen `(0 0 0)))
;(print (myLen `((0 0 0) (0 0 0) (0 0 0) (0 0 0))))

;helper function for next-states
;tried to use variable names to make my code more readable but it kept breaking my code when i did so:
;(nth c (nth r s)) is current keeper position
;(nth c (nth (- r 1) s)) is the spot in the row above the keeper
;(nth c (nth (- r 2) s)) is the spot two rows above the keeper
;(nth c (nth (+ r 1) s)) is the spot in the row below the keeper
;(nth c (nth (+ r 1) s)) is the spot two rows below the keeper
;(nth (- c 1) (nth r s)) is the spot to the left of the keeper
;(nth (- c 2) (nth r s)) is the spot two cols to the left of the keeper
;(nth (+ c 1) (nth r s)) is the spot to the right of the keeper
;(nth (+ c 2) (nth r s)) is the spot two cols to the right of the keeper
(defun try-move (s dir c r)
         ;trying to move up
   (cond ((equal dir 'UP)
                  ;keeper is not on a star
            (cond ((isKeeper (nth c (nth r s)))
                        ;if keeper is in the top row or the spot above the keeper is a wall, it's not possible to move 
                  (cond ((or (equal r 0) (isWall (nth c (nth (- r 1) s)))) nil)
                     ;if the spot above the keeper is a box
                     ((isBox ( nth c (nth (- r 1) s))) 
                              ;if the box is in the top row, or the spot above the box is occupied by something else, it's not possible to move
                        (cond ((or (or (or (equal r 1) (isWall (nth c (nth (- r 2) s)))) (isBox (nth c (nth (- r 2) s)))) (isBoxStar (nth c (nth (- r 2) s)))) nil)
                           ;if the spot above the box is blank, replace row above box with box, replace box with keeper, and replace keeper with blank
                           ((isBlank (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeper c (- r 1)) box c (- r 2)))
                           ;if the spot above the box is a star, replace row above box with boxstar, replace box with keeper, and replace keeper with blank
                           ((isStar (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeper c (- r 1)) boxstar c (- r 2)))
                        )
                     )
                     ;if the spot above the keeper is a boxstar
                     ((isBoxStar (nth c (nth (- r 1) s)))
                              ;if the boxstar is in the top row, or the spot above the boxstar is occupied by something else, it's not possible to move
                        (cond ((or (or (or (equal r 1) (isWall (nth c (nth (- r 2) s)))) (isBox (nth c (nth (- r 2) s)))) (isBoxStar (nth c (nth (- r 2) s)))) nil)
                           ;if the spot above the boxstar is blank, replace row above boxstar with box, replace boxstar with keeperstar, and replace keeper with blank
                           ((isBlank (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeperstar c (- r 1)) box c (- r 2)))
                           ;if the spot above the boxstar is a star, replace row above boxstar with boxstar, replace boxstar with keeperstar, and replace keeper with blank
                           ((isStar (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeperstar c (- r 1)) boxstar c (- r 2)))
                        )
                     )
                     ;if spot above keeper is blank, replace blank with keeper and keeper with blank
                     ((isBlank (nth c (nth (- r 1) s))) (moveRow (moveRow s blank c r) keeper c (- r 1)))
                     ;if spot above keeper is a star, replace star with keeperstar and keeper with blank 
                     ((isStar (nth c (nth (- r 1) s))) (moveRow (moveRow s blank c r) keeperstar c (- r 1)))   
                  ))
               ;if keeper is on a star, do exactly the same thing as above but replace old keeper position with star insteaad of blank
               ((isKeeperStar (nth c (nth r s)))
                  (cond ((or (equal r 0) (isWall (nth c (nth (- r 1) s)))) nil)
                     ((isBox ( nth c (nth (- r 1) s)))
                        (cond ((or (or (or (equal r 1) (isWall (nth c (nth (- r 2) s)))) (isBox (nth c (nth (- r 2) s)))) (isBoxStar (nth c (nth (- r 2) s)))) nil)
                           ((isBlank (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s star c r) keeper c (- r 1)) box c (- r 2)))
                           ((isStar (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s star c r) keeper c (- r 1)) boxstar c (- r 2)))
                        )
                     )
                     ((isBoxStar (nth c (nth (- r 1) s)))
                        (cond ((or (or (or (equal r 1) (isWall (nth c (nth (- r 2) s)))) (isBox (nth c (nth (- r 2) s)))) (isBoxStar (nth c (nth (- r 2) s)))) nil)
                           ((isBlank (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s star c r) keeperstar c (- r 1)) box c (- r 2)))
                           ((isStar (nth c (nth (- r 2) s))) (moveRow (moveRow (moveRow s star c r) keeperstar c (- r 1)) boxstar c (- r 2)))
                        )
                     )
                     ((isBlank (nth c (nth (- r 1) s))) (moveRow (moveRow s star c r) keeper c (- r 1)))
                     ((isStar (nth c (nth (- r 1) s))) (moveRow (moveRow s star c r) keeperstar c (- r 1)))
                  )
               )
            )
         )
         ;moving down is exactly the same thing as moving up, but adding to the row index instead of subtracting from it
         ;to get the index of the last and second-to-last rows, i used my myLen helper function to get the length of the 
         ;overall list and subtracted from that
         ((equal dir 'DOWN)
            (cond ((isKeeper (nth c (nth r s)))
                  (cond ((or (equal r (- (myLen s) 1)) (isWall (nth c (nth (+ r 1) s)))) nil)
                     ((isBox ( nth c (nth (+ r 1) s)))
                        (cond ((or (or (or (equal r (- (myLen s) 2)) (isWall (nth c (nth (+ r 2) s)))) (isBox (nth c (nth (+ r 2) s)))) (isBoxStar (nth c (nth (+ r 2) s)))) nil)
                           ((isBlank (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeper c (+ r 1)) box c (+ r 2)))
                           ((isStar (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeper c (+ r 1)) boxstar c (+ r 2)))
                        )
                     )
                     ((isBoxStar (nth c (nth (+ r 1) s)))
                        (cond ((or (or (or (equal r (- (myLen s) 2)) (isWall (nth c (nth (+ r 2) s)))) (isBox (nth c (nth (+ r 2) s)))) (isBoxStar (nth c (nth (+ r 2) s)))) nil)
                           ((isBlank (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeperstar c (+ r 1)) box c (+ r 2)))
                           ((isStar (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s blank c r) keeperstar c (+ r 1)) boxstar c (+ r 2)))
                        )
                     )
                     ((isBlank (nth c (nth (+ r 1) s))) (moveRow (moveRow s blank c r) keeper c (+ r 1)))
                     ((isStar (nth c (nth (+ r 1) s))) (moveRow (moveRow s blank c r) keeperstar c (+ r 1)))
                  ))
               ((isKeeperStar (nth c (nth r s)))
                  (cond ((or (equal r (- (myLen s) 1)) (isWall (nth c (nth (+ r 1) s)))) nil)
                     ((isBox ( nth c (nth (+ r 1) s)))
                        (cond ((or (or (or (equal r (- (myLen s) 2)) (isWall (nth c (nth (+ r 2) s)))) (isBox (nth c (nth (+ r 2) s)))) (isBoxStar (nth c (nth (+ r 2) s)))) nil)
                           ((isBlank (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s star c r) keeper c (+ r 1)) box c (+ r 2)))
                           ((isStar (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s star c r) keeper c (+ r 1)) boxstar c (+ r 2)))
                        )
                     )
                     ((isBoxStar (nth c (nth (+ r 1) s)))
                        (cond ((or (or (or (equal r (- (myLen s) 2)) (isWall (nth c (nth (+ r 2) s)))) (isBox (nth c (nth (+ r 2) s)))) (isBoxStar (nth c (nth (+ r 2) s)))) nil)
                           ((isBlank (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s star c r) keeperstar c (+ r 1)) box c (+ r 2)))
                           ((isStar (nth c (nth (+ r 2) s))) (moveRow (moveRow (moveRow s star c r) keeperstar c (+ r 1)) boxstar c (+ r 2)))
                        )
                     )
                     ((isBlank (nth c (nth (+ r 1) s))) (moveRow (moveRow s star c r) keeper c (+ r 1)))
                     ((isStar (nth c (nth (+ r 1) s))) (moveRow (moveRow s star c r) keeperstar c (+ r 1)))
                  )
               )
            )
         )
         ;moving left has the same overall format as moving up but instead of subtracting from the row index, i subtract from the column index
         ((equal dir 'LEFT)
            (cond ((isKeeper (nth c (nth r s)))
                  (cond ((or (equal c 0) (isWall (nth (- c 1) (nth r s)))) nil)
                     ((isBox ( nth (- c 1) (nth r s)))
                        (cond ((or (or (or (equal c 1) (isWall (nth (- c 2) (nth r s)))) (isBox (nth (- c 2) (nth r s)))) (isBoxStar (nth (- c 2) (nth r s)))) nil)
                           ((isBlank (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeper (- c 1) r) box (- c 2) r))
                           ((isStar (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeper (- c 1) r) boxstar (- c 2) r))
                        )
                     )
                     ((isBoxStar (nth (- c 1) (nth r s)))
                        (cond ((or (or (or (equal c 1) (isWall (nth (- c 2) (nth r s)))) (isBox (nth (- c 2) (nth r s)))) (isBoxStar (nth (- c 2) (nth r s)))) nil)
                           ((isBlank (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeperstar (- c 1) r) box (- c 2) r))
                           ((isStar (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeperstar (- c 1) r) boxstar (- c 2) r))
                        )
                     )
                     ((isBlank (nth (- c 1) (nth r s))) (moveRow (moveRow s blank c r) keeper (- c 1) r))
                     ((isStar (nth (- c 1) (nth r s))) (moveRow (moveRow s blank c r) keeperstar (- c 1) r))
                  ))
               ((isKeeperStar (nth c (nth r s)))
                  (cond ((or (equal c 0) (isWall (nth (- c 1) (nth r s)))) nil)
                     ((isBox ( nth (- c 1) (nth r s)))
                        (cond ((or (or (or (equal c 1) (isWall (nth (- c 2) (nth r s)))) (isBox (nth (- c 2) (nth r s)))) (isBoxStar (nth (- c 2) (nth r s)))) nil)
                           ((isBlank (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeper (- c 1) r) box (- c 2) r))
                           ((isStar (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeper (- c 1) r) boxstar (- c 2) r))
                        )
                     )
                     ((isBoxStar (nth (- c 1) (nth r s)))
                        (cond ((or (or (or (equal c 1) (isWall (nth (- c 2) (nth r s)))) (isBox (nth (- c 2) (nth r s)))) (isBoxStar (nth (- c 2) (nth r s)))) nil)
                           ((isBlank (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeperstar (- c 1) r) box (- c 2) r))
                           ((isStar (nth (- c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeperstar (- c 1) r) boxstar (- c 2) r))
                        )
                     )
                     ((isBlank (nth (- c 1) (nth r s))) (moveRow (moveRow s star c r) keeper (- c 1) r))
                     ((isStar (nth (- c 1) (nth r s))) (moveRow (moveRow s star c r) keeperstar (- c 1) r))
                  )
               )
            )
         )
         ;moving right is the same as moving left but instead of subtracting from the column index, i add, and
         ;i used my myLen helper function to get the length of the sublists to know how many columns there are
         ((equal dir 'RIGHT)
            (cond ((isKeeper (nth c (nth r s)))
                  (cond ((or (equal c (- (myLen (car s)) 1)) (isWall (nth (+ c 1) (nth r s)))) nil)
                     ((isBox ( nth (+ c 1) (nth r s)))
                        (cond ((or (or (or (equal c (- (myLen (car s)) 2)) (isWall (nth (+ c 2) (nth r s)))) (isBox (nth (+ c 2) (nth r s)))) (isBoxStar (nth (+ c 2) (nth r s)))) nil)
                           ((isBlank (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeper (+ c 1) r) box (+ c 2) r))
                           ((isStar (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeper (+ c 1) r) boxstar (+ c 2) r))
                        )
                     )
                     ((isBoxStar (nth (+ c 1) (nth r s)))
                        (cond ((or (or (or (equal c (- (myLen (car s)) 2)) (isWall (nth (+ c 2) (nth r s)))) (isBox (nth (+ c 2) (nth r s)))) (isBoxStar (nth (+ c 2) (nth r s)))) nil)
                           ((isBlank (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeperstar (+ c 1) r) box (+ c 2) r))
                           ((isStar (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s blank c r) keeperstar (+ c 1) r) boxstar (+ c 2) r))
                        )
                     )
                     ((isBlank (nth (+ c 1) (nth r s))) (moveRow (moveRow s blank c r) keeper (+ c 1) r))
                     ((isStar (nth (+ c 1) (nth r s))) (moveRow (moveRow s blank c r) keeperstar (+ c 1) r))
                  ))
               ((isKeeperStar (nth c (nth r s)))
                  (cond ((or (equal c (- (myLen (car s)) 1)) (isWall (nth (+ c 1) (nth r s)))) nil)
                     ((isBox ( nth (+ c 1) (nth r s)))
                        (cond ((or (or (or (equal c (- (myLen (car s)) 2)) (isWall (nth (+ c 2) (nth r s)))) (isBox (nth (+ c 2) (nth r s)))) (isBoxStar (nth (+ c 2) (nth r s)))) nil)
                           ((isBlank (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeper (+ c 1) r) box (+ c 2) r))
                           ((isStar (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeper (+ c 1) r) boxstar (+ c 2) r))
                        )
                     )
                     ((isBoxStar (nth (+ c 1) (nth r s)))
                        (cond ((or (or (or (equal c (- (myLen (car s)) 2)) (isWall (nth (+ c 2) (nth r s)))) (isBox (nth (+ c 2) (nth r s)))) (isBoxStar (nth (+ c 2) (nth r s)))) nil)
                           ((isBlank (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeperstar (+ c 1) r) box (+ c 2) r))
                           ((isStar (nth (+ c 2) (nth r s))) (moveRow (moveRow (moveRow s star c r) keeperstar (+ c 1) r) boxstar (+ c 2) r))
                        )
                     )
                     ((isBlank (nth (+ c 1) (nth r s))) (moveRow (moveRow s star c r) keeper (+ c 1) r))
                     ((isStar (nth (+ c 1) (nth r s))) (moveRow (moveRow s star c r) keeperstar (+ c 1) r))
                  )
               )
            )
         )
   )
)

;test cases for try-move UP with keeperstar
;(print (try-move `((0 0 6 0 0) 
;                   (0 0 0 0 0) 
;                   (0 0 0 0 0) 
;                   (0 0 0 0 0)) 'UP 2 0))
;(print (try-move `((0 0 1 0 0) 
;                   (0 0 6 0 0) 
;                   (0 0 0 0 0) 
;                   (0 0 0 0 0)) 'UP 2 1))
;(print (try-move `((0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'UP 2 1))
;(print (try-move `((0 0 1 0 0)
;                   (0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 2 0 0)
;                   (0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 5 0 0)
;                   (0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 4 0 0)
;                   (0 0 2 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'UP 2 1))
;(print (try-move `((0 0 1 0 0)
;                   (0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 2 0 0)
;                   (0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 5 0 0)
;                   (0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 4 0 0)
;                   (0 0 5 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)) 'UP 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'UP 2 1))
;(print (try-move `((0 0 4 0 0)
;                   (0 0 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'UP 2 1))

;test cases for try-move DOWN with keeper
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)) 'DOWN 2 3))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 1 0 0)) 'DOWN 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)) 'DOWN 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)
;                   (0 0 1 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)
;                   (0 0 2 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)
;                   (0 0 5 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)
;                   (0 0 0 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 2 0 0)
;                   (0 0 4 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)) 'DOWN 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)
;                   (0 0 1 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)
;                   (0 0 2 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)
;                   (0 0 5 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)
;                   (0 0 0 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 5 0 0)
;                   (0 0 4 0 0)) 'DOWN 2 1))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 0 0 0)) 'DOWN 2 2))
;(print (try-move `((0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 3 0 0)
;                   (0 0 4 0 0)) 'DOWN 2 2))

;test cases for try-move LEFT with keeperstar
;(print (try-move `((6 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 0 0))
;(print (try-move `((1 6 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 1 0))
;(print (try-move `((2 6 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 1 0))
;(print (try-move `((1 2 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((2 2 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((5 2 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((0 2 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((4 2 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((5 6 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 1 0))
;(print (try-move `((1 5 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((2 5 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((5 5 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((0 5 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((4 5 6 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 2 0))
;(print (try-move `((0 6 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 1 0))
;(print (try-move `((4 6 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'LEFT 1 0))

;test cases for try-move RIGHT with keeperstar
;(print (try-move `((0 0 0 0 6)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 4 0))
;(print (try-move `((0 0 0 6 1)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 3 0))
;(print (try-move `((0 0 0 6 2)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 3 0))
;(print (try-move `((0 0 6 2 1)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 2 2)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 2 5)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 2 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 2 4)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 0 6 5)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 3 0))
;(print (try-move `((0 0 6 5 1)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 5 2)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 5 5)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 5 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 6 5 4)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 2 0))
;(print (try-move `((0 0 0 6 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 3 0))
;(print (try-move `((0 0 0 6 4)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)
;                   (0 0 0 0 0)) 'RIGHT 3 0))

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (c (car pos))
	 (r (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP c r) (try-move s 'DOWN c r) (try-move s 'LEFT c r) (try-move s 'RIGHT c r)))
	 )
    (cleanUpList result);end
  );end let
);

;test cases for next-states
;(setq s1 `((1 1 1 1 1)
;           (1 0 0 4 1)
;           (1 0 2 0 1)
;           (1 0 3 0 1)
;           (1 0 0 0 1)
;           (1 1 1 1 1)
; ))
;(setq r1 `(((1 1 1 1 1) (1 0 2 4 1) (1 0 3 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 3 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 3 0 0 1) (1 0 0 0 1) (1 1 1 1 1)) 
;           ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 0 1) (1 1 1 1 1))))
;(setq s2 `((1 1 1 1 1)
;           (1 0 0 4 1)
;           (1 0 2 3 1)
;           (1 0 0 0 1)
;           (1 0 0 0 1)
;           (1 1 1 1 1)
; ))
;(setq r2 `(((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 0 0 4 1) (1 2 3 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))))
;(setq s3 `((1 1 1 1 1)
;           (1 0 0 6 1)
;           (1 0 2 0 1)
;           (1 0 0 0 1)
;           (1 0 0 0 1)
;           (1 1 1 1 1)
; ))
;(setq r3 `(((1 1 1 1 1) (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 0 3 4 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))))
;(setq s4 `((1 1 1 1 1)
;           (1 4 2 0 1)
;           (1 0 0 0 1)
;           (1 0 0 0 1)
;           (1 0 5 3 1)
;           (1 1 1 1 1)
; ))
;(setq r4 `(((1 1 1 1 1) (1 4 2 0 1) (1 0 0 0 1) (1 0 0 3 1) (1 0 5 0 1) (1 1 1 1 1))
;           ((1 1 1 1 1) (1 4 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 2 6 0 1) (1 1 1 1 1))))
;(print (equal r1 (next-states s1)))
;(print (equal r2 (next-states s2)))
;(print (equal r3 (next-states s3)))
;(print (equal r4 (next-states s4)))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

;test cases for h0
;(print (h0 nil))
;(print (h0 `(0 0 0 0)))

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; this heuristic is admissible
(defun h1 (s)
   (cond ((null s) 0)
      (t (+ (count box (car s)) (h1 (cdr s))))
   )
)

;test cases for h1 heuristic
;(print (h1 p1))
;(print (h1 p2))
;(print (h1 p3))
;(print (h1 p4))
;(print (h1 p5))
;(print (h1 p6))
;(print (h1 p7))
;(print (h1 p8))
;(print (h1 p9))
;(print (h1 p10))
;(print (h1 p11))
;(print (h1 p12))
;(print (h1 p13))
;(print (h1 p14))
;(print (h1 p15))
;(print (h1 p16))
;(print (h1 p17))
;(print (h1 p18))
;(print (h1 p19))
;(print (h1 p20))
;(print (h1 p21))
;(print (h1 p22))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;helper functions to get coordinates of boxes and stars
;getCols creates a list containing the index of each box in the row
(defun getCols (r c thing)
         ;if it's reached the end of the row, return nil
   (cond ((equal c (myLen r)) nil)
         ;if column c of row r is what it's looking for
         ((equal thing (nth c r))
                  ;if it's the last column of the row, return a list containing that index
            (cond ((equal c (- (myLen r) 1)) (list c))
                  ;else, append the results of checking the rest of the columns to that index
                  (t (append (list c) (getCols r (+ c 1) thing)))
            )
         )
         ;if it's not a match, check the rest of the row
         (t (getCols r (+ c 1) thing))
   )
)

;test cases for getCols
;(print (getCols `(0 0 0 0) 0 box)) 
;(print (getCols `(0 2 0 0) 0 box))
;(print (getCols `(0 0 2 2) 0 box))
;(print (getCols `(2 2 2 2) 0 box))

;distribute takes a row and a list of columns and makes a list of coordinates
;for each specified column of that row
(defun distribute (row cols)
   (cond ((null cols) nil)
         (t (append (list (list row (car cols))) (distribute row (cdr cols))))
   )
)

;test cases for distribute
;(print (distribute 0 `()))
;(print (distribute 0 `(0)))
;(print (distribute 0 `(0 1)))
;(print (distribute 0 `(0 1 2)))

;getCoords takes a board, a start row, a count for how many things you're looking for, 
;and what kind of thing you're looking for, and compiles a list of all the coordinates
;of things on your board
(defun getCoords (s row thing)
         ;if you've reached the end of the board, stop
   (cond ((equal row (myLen s)) nil)
         ;if there is a thing in the row, append the results of checking the rest of the board 
         ;(and subtracting the number of things found from myCount) to the coordinates of each 
         ;thing in that row 
         ((myMem thing (nth row s)) 
           (append (distribute row (getCols (nth row s) row thing)) (getCoords s (+ row 1) thing))
         )
   )
)

;test cases for getCoords
;(print (getCoords `((2 0 0 0) (0 2 0 0) (0 0 2 0) (0 0 0 2)) 0 4 box))

;getDist gets the distance from one coordinate to the other by taking the difference
;in rows and columns and adding those together
(defun getDist (coord1 coord2)
   (+ (- (max (car coord1) (car coord2)) (min (car coord1) (car coord2))) 
      (- (max (cadr coord1) (cadr coord2)) (min (cadr coord1) (cadr coord2)))
   )
)

;test cases for getDist
;(print (getDist `(0 0) `(0 1)))
;(print (getDist `(0 0) `(0 2)))
;(print (getDist `(0 0) `(1 0)))
;(print (getDist `(0 0) `(2 0)))

;pcHelper makes a list that pairs one box with each star
(defun pcHelper (myBox stars)
   (cond ((null stars) nil)
         (t (append (list (list myBox (car stars))) (pcHelper myBox (cdr stars))))
   )
)

;test cases for pcHelper
;(print (pcHelper `(0 0) `((1 1) (2 2) (3 3))))

;pairCoords makes a list that pairs each box coordinate with each star coordinate
(defun pairCoords (boxes stars)
   (cond ((null boxes) nil)
         (t (append (list (pcHelper (car boxes) stars)) (pairCoords (cdr boxes) stars)))
   )
)

;test cases for pairCoords
;(print (pairCoords `((0 0) (1 1) (2 2)) `((3 3) (4 4) (5 5))))

;mdlHelper makes a list of lists containing the box coord, star coord, and 
;distance between the two for one box and every star on the board
(defun mdlHelper (pairedCoords)
   (cond ((null pairedCoords) nil)
         (t (append 
               (list 
                  (append (car pairedCoords) 
                     (list (getDist (caar pairedCoords) (cadar pairedCoords))
                     )
                  )
               ) 
               (mdlHelper (cdr pairedCoords))
            )
         )
   )
)

;makeDistList compiles the lists of distances for each box
(defun makeDistList (pairedCoords)
   (cond ((null pairedCoords) nil)
         (t (append (list (mdlHelper (car pairedCoords))) (makeDistList (cdr pairedCoords))))
   )
)

;test cases for makeDistList
;(print (makeDistList (pairCoords `((0 0) (0 1) (0 2)) `((1 0) (1 1) (1 2)))))

;mmHelper compares 2 distLists and returns the one with the shortest distance
(defun mmHelper (dl1 dl2)
         ;if the distances are equal, return the first distList
   (cond ((equal (caddr dl1) (caddr dl2)) dl1)
         ;if the distance of dl1 is greater than that of dl2, return dl1
         ((< (caddr dl1) (caddr dl2)) dl1)
         ;else return dl2
         (t dl2)
   )
)

;test case for mmHelper
;(print (mmHelper `((0 0) (0 0) 0) `((0 0) (0 1) 1)))

;myRmv removes specified distList from overall list
(defun myRmv (dl ol)
   (cond ((null ol) nil)
         ((equal dl (car ol)) (cdr ol))
         (t (append (list (car ol)) (myRmv dl (cdr ol))))
   )
)

;test cases for myRmv
;(print (myRmv `((0 0) (0 0) 0) `(((0 0) (0 1) 1) ((0 0) (0 0) 0) ((0 0) (0 2) 2))))

;myMin gets the sublist with the min distance from the list of distances
(defun myMin (distList)
   (cond ((equal 1 (myLen distList)) (car distList))
         ((equal 2 (myLen distList)) (mmHelper (car distList) (cadr distList)))
         (t (mmHelper (mmHelper (car distList) (cadr distList)) (myMin (cddr distList))))
   )
)

;test case for myMin
;(print (myMin `(((0 0) (0 1) 1) ((0 0) (0 0) 0) ((0 0) (0 2) 2))))

;odlHelper orders the list of distances for one box
(defun odlHelper (distList)
   (cond ((null distList) nil)
         (t (append (list (myMin distList)) (odlHelper (myRmv (myMin distList) distList))))
   )
)

;test case for odlHelper
;(print (odlHelper `(((0 0) (0 1) 1) ((0 0) (0 0) 0) ((0 0) (0 2) 2))))

;orderDistLists orders each list of distances from min to max
(defun orderDistLists (distLists)
   (cond ((null distLists) nil)
         (t (append (list (odlHelper (car distLists))) (orderDistLists (cdr distLists))))
   )
)

;test case for orderDistLists
;(print (orderDistLists (makeDistList (pairCoords `((0 0) (0 1) (0 2)) `((1 0) (1 1) (1 2))))))

;myMax compares two distLists and returns the one with the greater distance from the star
(defun myMax (dl1 dl2)
   (cond ((equal (caddr dl1) (caddr dl2)) dl2)
         ((> (caddr dl1) (caddr dl2)) dl1)
         (t dl2)
   )
)

;test case for myMax
;(print (myMax `((0 0) (0 0) 0) `((0 0) (0 1) 1)))

;extractDist gets the dist from a distList
;(defun extractDist (distList)
;   (caddr distList)
;)

;test cases for extractDist
;(print (extractDist `((0 0) (0 0) 0)))

;rmvdHelper removes all the distLists for the same star that are the closest to a box 
;except for the one with the shortest distance 
(defun rmvdHelper (shortest myRest)
         ;if you've checked all the lists, return the list with the the shortest distance to the star
   (cond ((null myRest) (list shortest))
         ;if the star closest to the box of shortest is the same as the star closest to the first
         ;box of myRest
         ((equal (cadar shortest) (cadaar myRest))
                  ;if the max distance between the two is the one for shortest
            (cond ((equal (myMax (car shortest) (caar myRest)) (car shortest))
                     ;remove that star from the list of shortest and appaend it to the result of checking the rest 
                     (append (list (myRmv (car shortest) shortest)) (rmvdHelper (car myRest) (cdr myRest)))
                  )
                  ;else, do the above with the first element of myRest
                  (t (append (list (myRmv (caar myRest) (car myRest))) (rmvdHelper shortest (cdr myRest))))
            )
         )
         ;if they're not the same, check the rest of the list
         (t (append (list (car myRest)) (rmvdHelper shortest (cdr myRest))))
   )
)

;test case for rmvdHelper
;(setq dlists (orderDistLists (makeDistList (pairCoords `((0 0) (0 2) (0 4)) `((1 0) (1 4) (1 5))))))
;(print (rmvdHelper (car dlists) (cdr dlists)))

;rmvDupls removes duplicate stars from the distLists for each box.
;for example, if one star is the closest star to two boxes, it will
;be removed from the distList for the further box, or if the boxes are
;at the same distance, it will be removed from the second box's distList
(defun rmvDupls (i distLists)
   (cond ((null distLists) nil)
         ((equal i (- (myLen distLists) 1)) distLists)
         (t (rmvDupls (+ i 1) (rmvdHelper (car distLists) (cdr distLists))))
   )
)

;test case for rmvDupls
;(print (rmvDupls 0 dlists))

;addMinDists adds the minimum distances from each box to the closest star
(defun addMinDists (distLists)
   (cond ((null distLists) 0)
         (t (+ (addMinDists (cdr distLists)) (car (cdr (cdr (car (car distLists)))))))
   )
)

;(setq dlists (rmvDupls 0 (orderDistLists (makeDistList (pairCoords `((0 0) (0 2) (0 4)) `((1 0) (1 4) (1 5)))))))
;test case for addMinDists
;(print (addMinDists dlists))

;something went wrong but i ran out of time :/
(defun h505020340 (s) nil
   ;x get coordinates of all boxes (r c)
   ;x get coordinates of all stars (r c)
   ;x calculate distance from each box to each star ((rb cb) (rs cs) dist)
   ;x order distances from min to max (((rb1 cb1) (rs1 cs1) dist1) ((rb2 cb2) (rs2 cs2) dist2) etc...)
   ;x if one star is the closest to multiple boxes, compare the distance and chose the box closest to the star
      ;x make the other boxes choose the next closest star
   ;x add min distances to get result
   (addMinDists (rmvDupls 0 (orderDistLists (makeDistList (pairCoords (getCoords s 0 box) (getCoords s 0 star))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(print (getCoords p1 0 box))
;(print (addMinDists (rmvDupls 0 (orderDistLists (makeDistList (pairCoords (getCoords p1 0 (h1 p1) box) (getCoords p1 0 (h1 p1) star)))))))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

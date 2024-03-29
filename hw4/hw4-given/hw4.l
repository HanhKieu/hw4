(defun cycle ( cycles cycleList )   ;cycles = number of times to cycle
    (cond                           ;cycleList = list to cycle through
        ((null cycleList) cycleList) ;if cycleList is null, just return it 
    
        ((= cycles 0) cycleList) ;if 0 cycles, return the unaltered list


        ((= cycles 1) (cdr (append cycleList (list (car cycleList))))) 
            ;do it once
    
        (t( cycle (1- cycles) (cdr (append cycleList (list (car cycleList)))))) 
            ;else recurse
    )
)

(defun split-list ( delim myList )   ;delim = delimeter
	(setq tempList '())
	(setq newList '())
	(setq myListLength (list-length myList))
	(setq i 1)
	(setq ifDelimiterPresent 0)
	(dotimes (i myListLength)
		(setf var (nth i myList))
		(cond
			( (and (= i (- myListLength 1)) (eq var delim))
				(if (not (eq tempList NIL)) 
					(setf newList (append newList (list tempList)))
				)
				(setf tempList '())
				(setf var (nth i myList))
				(if (eq var delim)
					(setf var NIL)
				)
				(setf ifDelimiterPresent 1)

			) ;if(i = 8) and var===delim)
			( (and (= i 0) (eq var delim))
				(setf tempList '())
				(if (not (eq i (- myListLength 1)))
					(setf i (+ i 1))
					)
				(setf var (nth i myList))
				(if (eq var delim)
					(setf var NIL)
				)
				(setf ifDelimiterPresent 1)
			) ;if((i = 0) and var===delim)
		
			((eq var delim)
				(if (not (eq tempList NIL)) 
					(setf newList (append newList (list tempList))) ;append our temporary list to our newLIst
				)
				(setf tempList '())
				(setf i (+ i 1))
				(setf var (nth i myList))
				(setf ifDelimiterPresent 1)
				(if (eq var delim)
					(setf var NIL)
				)
			)
			
		)
		;(if (and (not (eq var delim)) (not (eq tempList NIL)))
		(if (not (eq var NIL))
			(setf tempList (append tempList (list var)))
		)
		; (print "start")
		; (print i)
		; (print newList)
		; (print tempList)
	)

	(if (not (eq tempList NIL))
		(setf newList (append newList (list tempList))) ;when you're out of the function append whats remaining in our tempList
	)
	; (cond
	; 	((= ifDelimiterPresent 0)
	; 		(setf newList myList)
	; 	)
	; );if no delimter in myList then return original list
	
	newList
	;(print newList)
)


(defun range(myList)
(cond 


	((null myList)
		myList
	)

	(t
		(setq smallest (car myList))
		(setq largest (car mylist))
		(setq counter 0)
		(setq mySum 0)
		(setq newList '())


		(doList (element myList)

			(if (< element smallest)
				(setf smallest element)

			);if element smaller, make that new smallest

			(if (> element largest)
				(setf largest element)

			); if element larger make that new largest
			(setf counter (+ counter 1));increment our counter after everytime
			(setf mySum (+ mySum element))
		)

		(setq myFraction (/ mySum counter))
		(setf newList (append newList (list smallest) (list myFraction) (list largest)))

		;print newList)

		newList

	)

)
)

(defun pivot(n myList)

	(setq firstList '())
	(setq secondList '())
	(setq newList '())
	(doList (element myList)

		(if (< element n )
			(setf firstList (append firstList (list element)))
		); if less than
		(if (or (> element n) (eq element n))
			(setf secondList (append secondList (list element)))
		); if greater than or equal to


	)
	(setf newList (append newList (list firstList) (list secondList)))
	newList
)


(defun quicksort (myList)

	(cond
		((or (null myList) (eq (list-length myList) 1) )
			; (if (eq (list-length myList) 1)
			; (print myList)
			; )
			myList
		) ;if null, return

		(t ;else do quicksort
			(let ( ;avoids scoping errors compared to setq
				(pivot (car myList))
				(myList (cdr myList))
				(firstList '())
				(secondList '())
				(newList '())
				)


			;(setq pivot (car myList))
			;(setf myList (cdr myList))
			;(setq firstList '())
			;(setq secondList '())
			;(setq newList '())


			(doList (element myList)

				(if (< element pivot )
					(setf firstList (append firstList (list element)))
				); if less than
				(if (or (> element pivot) (eq element pivot))
					(setf secondList (append secondList (list element)))
				); if greater than or equal to

			)
			; (print "list start")
			; (print secondList)
			; (print "list end")

			(setf newList (append newList (quicksort firstList) (list pivot) (quicksort secondList)))
			;(setf newList (append newList (list firstList) (list secondList)))
			; (print "my New List")\
			; (print " print start ")
			; (print firstList)
			; (print pivot)
			; (print secondList)
			; (print " end start")
			; (print newList)
			newList
			)
			
		)
	)
)


(defun match(firstList secondList)


	(cond

		((or (null firstList) (null secondList))

			(eq firstList secondList)
		);if its null

		((eq (car firstList) '? )
			(match (cdr firstList) (cdr secondList))
		); if first is question mark

		((and(eq (car firstList) '!) (match (cdr firstList) (cdr secondList)))

			(eq firstList firstList)
		)

		((eq (car firstList) '!)

			(match firstList (cdr secondList))
		)

		(t
			(and (eq (car firstList) (car secondList)) (match (cdr firstList) (cdr secondList)))
		)

	

	);followed pseudocode from lecture / TA

)



(defun matrix-add(mat1 mat2)

	(setq currentSum 0)
	(setq currentList '())
	(setq finalMatrix '())
	(setq colSize (list-length (car mat1)))
	(setq rowSize (list-length mat1))

	(dotimes (i rowSize)

		(dotimes (j colSize)

			(setf currentSum  (+ (nth j (nth i mat1) ) (nth j (nth i mat2) ) ))

			(setf currentlist (append currentlist (list currentSum)))
		)

		(setf finalMatrix (append finalMatrix (list currentlist) ))
		(setf currentlist '())
	);for each row and size, calcuate sum and append to new matrix called final MAtrix

	finalMatrix ;return final matrix
)

(defun matrix-multiply(mat1 mat2)

	(setq colSize (list-length (car mat1)))
	(setq rowSize (list-length mat1))
	(setq colSize2 (list-length (car mat2)))
	(setq rowSize2 (list-length mat2))
	(setq currentSum 0)
	(setq currentProduct 1)
	(setq productmMatrix '())
	(setq currentInitList '())

	(dotimes (i rowSize)

		(dotimes (j colSize2)
			(setf currentInitList (append currentInitList'(0)))
		)

		(setf productmMatrix (append productmMatrix (list currentInitList)))
		(setf currentInitList '())
	);initiliaze our product matrix, so no out of bound errors

	(dotimes (i rowSize)
		(dotimes (k colSize2)
			(setf currentSum 0)
			(dotimes (j colSize)
				(setf currentProduct (* (nth j (nth i mat1) ) (nth k (nth j mat2) ) ))
				(setf currentSum (+ currentSum currentProduct))
			)
			(setf productmMatrix (set-nth i k productmMatrix currentSum)) ;add to productMatrix
		)
	)


	productmMatrix

);followed same procedure we did for HW3



(defun set-nth(ith jth myList replacement)

	(setq myElement 0)
	(setq colSize3 (list-length (car myList)))
	(setq rowSize3 (list-length myList))
	(setq finalMatrix '())
	(setq currentlist '())
	(dotimes (i rowSize3)

		(dotimes (j colSize3)
			(setf myElement (nth j (nth i myList)) )
			(if (and (eq i ith) (eq j jth))
				(setf myElement replacement )
			)
			(setf currentlist (append currentlist (list myElement)))
		)
		(setf finalMatrix (append finalMatrix (list currentlist) ))
		(setf currentlist '())
	)

	finalMatrix
  ); this is a helper function that allows us to  set the nth element of a matrix

(defun search-count ( item searchList )
    (defvar count 0)
    (setq count 0)
    (loop for x in searchList
        if(eq x item) ;found instance of the item
            do (setf count (1+ count)) ;increase count
    )

    count
)

(defun matrix-transpose (mat)
    (setq rowSize (list-length (car mat)))
    (setq colSize (list-length mat))
    (setq newList '())
    (setq newMat '())
    
    (dotimes(i rowSize)
    
        (dotimes(j colSize)
        
            (setf newList (append newList (list (nth i (nth j mat))))) 
            ;goes through jth position of ith row, adds to new list

        )
        (setf newMat (append newMat (list newList))) ;append list to new matrix
        (setf newList '()) ;reset list 
    )    
    newMat ;return new Matrix
)
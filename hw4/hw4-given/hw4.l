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
	(cond
		((= ifDelimiterPresent 0)
			(setf newList myList)
		)
	);if no delimter in myList then return original list

	(print newList)
)

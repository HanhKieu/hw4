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
	(setq currentList '())
	(setq newList '())
	(setq myListLength (list-length myList))
	(setq i 1)
	(dotimes (i myListLength)
		(setf var (nth i myList))
		(cond
			((eq var delim) 
				(setf newList (append newList currentList))
				(setf currentList '())
				(setf i (+ i 1))
				(setf var (nth i myList))

			)

			(t(print "NOO"))
			
		)
		(setf currentList (append currentList (list var)))
	)

	(print newList)
)

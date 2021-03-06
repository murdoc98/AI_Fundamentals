;;;======================================================================================
;;; Rules
;;; Toads and Frogs is played on a 1 × n strip of squares. At any time, each square is 
;;; either empty or occupied by a single toad or frog. Although the game may start at any 
;;; configuration, it is customary to begin with toads occupying consecutive squares on 
;;: the leftmost end and frogs occupying consecutive squares on the rightmost end of the strip.
;;;
;;; When it is the Left player's turn to move, they may either move a toad one square to 
;;; the right, into an empty square, or "hop" a toad two squares to the right, over a frog, 
;;; into an empty square. Hops over an empty square, a toad, or more than one square are not 
;;; allowed. Analogous rules apply for Right: on a turn, the Right player may move a frog left 
;;; into a neighboring empty space, or hop a frog over a single toad into an empty square 
;;; immediately to the toad's left. Under the normal play rule conventional for combinatorial 
;;; game theory, the first player to be unable to move on his turn loses.
;;;
;;; frogsAndToads.lisp
;;;     Solve frogs and toads problem using blind search, breath first and depth first.
;;;     States representation:
;;;         A list of frogs represented by a's, toads represented by b's, non-original anurans 
;;;			by c's and the available spaces by 0's
;;;             'a' Fogs, 'b' Toads, '0' Space
;;;                 Initial state:             Target state:
;;;                  F F F S T T T             T T T S F F F
;;;                 (a a a 0 b b b)           (a a a 0 b b b)
;;;
;;;     Oscar Martinez Vazquez
;;;     Artificial Intelligence Fundamentals
;;;     Centro de Investigacion en Computo
;;;  March, 2021
;;;======================================================================================
(defparameter  *open* '()) ;; Search frontier                                             
(defparameter  *memory* '()) ;; Past attempts memory

(defparameter  *ops*  '( 
	(:Frog-jump		    ('a 0))
	(:Frog-double-jump!	('a 'x 0))
    (:Toad-jump        (0 'b))
    (:Toad-double-jump! (0 'x 'b))))

(defparameter  *id*  -1) ;; Last created node id
(defparameter  *current-ancestor*  nil) ;; Root node id of all generated children
(defparameter  *solution*  nil) ;; List of the solution (generated by backtracking)
(defparameter *maxLengthOfOpen* 0) ;; Length of *open* list

;;;=======================================================================================
;;  CREATE-NODE (state  op)  
;;      state - An state of the problem (system)
;;         op - Operator wich application generates the [state]...
;;;=======================================================================================
(defun  create-node 
	(state  op)
    "Creates new node with the state and operator as parameters"
    (incf  *id*)
    (list  *id*  state  *current-ancestor*  (first op)))  ;; The nodes generated are children of *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN and GET-FROM-OPEN  
;;        Insert-to-open  receives a list and key - the key map the function to append the state
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open 
	(state  op  method)
    "Allow to insert nodes in the search frontier *open* depending on method parameter (breath-first or depth-first)"
    (let (
        (node  (create-node  state  op)))
        (cond 
			((eql  method  :depth-first)
	        	(push  node  *open*))
	    	((eql  method  :breath-first)
				(setq *open*  (append  *open*  (list node))))
	   	   	(T  Nil))
		)
	)

(defun get-from-open 
	()
    "Recover the next element to check in the search frontier *open*"
    (pop  *open*))

;;;=======================================================================================
;;  find-rock (state)
;;      Return the list of free rocks
;;;=======================================================================================
(defun find-rocks
	(state)
    "Return a list with available spaces in the map"
    (let 
        ((rocks '())
        (i 0))
        (dolist 
            (n state)
            (if
                (equal n 0)
                (push i rocks))
            (incf i)
            )
        rocks
        )
    )

;;;=======================================================================================
;;  VALID-OPERATOR [op, state]
;;        Indicate if is possible to apply the [op] to [state]
;;;=======================================================================================
(defun  valid-operator? 
	(op  state anuraPos)
	"Validate the application of the operator to the state
	State structure:  (a a a ... 0 0 0 ... b b b ... C),
	Operator: [<human-label> <list operator(<free rock position><><anura target>)>]"  
	(let* (
		(operator (first op)))
        (case operator
	    	(:Frog-jump
				(if
					(equal (nth anuraPos state) 'a)
					NIL
					T)
				)
	    	(:Frog-double-jump!
				(if
					(equal (nth anuraPos state) 'a)
					NIL
					T)
				)
	    	(:Toad-Jump
				(if
					(equal (nth anuraPos state) 'b)
					NIL
					T)
				)
	    	(:Toad-double-jump!
				(if
					(equal (nth anuraPos state) 'b)
					NIL
					T)
				)
	    	(T "error"))
	)
)

;;;=======================================================================================
;;  VALID-STATE (state)
;;      Indicate if [state] is valid according to the restrictions of the problem
;;      If in any of the riverbanks there is not goat-cabbage or goat-wolf combination
;;;=======================================================================================
(defun flip 
	(bit)
	(boole BOOLE-XOR  bit  1))

(defun  valid-state? 
	(state)
	"Validate the state according to problem restrictions
	State structure:  (a a a ... 0 0 0 ... b b b ... C)"
    (if 
		(member 'c state) 
		NIL 
		T)
	)

;;;=======================================================================================
;;  APPLY-OPERATOR (op, state, rock-position)
;;        Change the state of the system
;;;=======================================================================================
(defun  apply-operator 
	(op  state rockPosition) 
	"Get the [states]'s child applying the [op] WITHOUT RESTRICTIONS"
    (let* (
        (newState (copy-list state))
		(stateLength (- (length newState) 1))
		(i rockPosition)
		(j nil)
		(operator (first op)))
	 	(case operator
	    	(:Frog-jump
				(if
					(> (+ i 1) stateLength)
					(setf (nth i newState) 'c)
					(progn
                		(setf j (+ i 1))
						(rotatef (nth i newState) (nth j newState))))
                newState
				)
	    	(:Frog-double-jump!
				(if
					(> (+ i 2) stateLength)
					(setf (nth i newState) 'c)
					(progn 
						(setf j (+ i 2))
						(rotatef (nth i newState) (nth j newState))))
                newState
				)
	    	(:Toad-Jump
				(if
					(< (- i 1) 0)
					(setf (nth i newState) 'c)
					(progn
						(setf j (- i 1))
						(rotatef (nth i newState) (nth j newState))))
                newState
				)
	    	(:Toad-double-jump!
				(if
					(< (- i 2) 0)
					(setf (nth i newState) 'c)
					(progn 
						(setf j (- i 2))
						(rotatef (nth i newState) (nth j newState))))
                newState
				)
	    	(T "error"))
		)
	)


;;;=======================================================================================
;;  EXPAND (state)
;;      Build and return a list with all valid parents of [state]
;;;=======================================================================================
(defun expand 
	(state)
    "Get all valid children of state, aplying the operators *ops* in the same order"
    (let (
		(children  nil)
		(new-state  nil)
        (free-rocks (find-rocks state)))
        (dolist
            (rockPosition free-rocks)
            (dolist
                (op *Ops*)
                    (setq new-state (apply-operator op state rockPosition))
                    (when
                        (and 
							(valid-state? new-state)
							(valid-operator? op new-state rockPosition))
                        (setq children (cons (list new-state op) children)))
                )
			)
        children
		)
	)


;;;=======================================================================================
;;  REMEMBER-STATE? and FILTER-MEMORIES
;;      Allow to manage the previous attempts memory
;;;=======================================================================================
(defun remember-state? 
	(state  memory-list)
    "Search an state in the list, this list works as previous attemps memory
    State structure: (a a a ... 0 0 0 ... b b b ... C)
    Node structure: [<Id> <state> <id-parent> <operator>]"
    (cond 
		((null  memory-list)  
			NIL)
	    ((equal  state  (second (first  memory-list)))  
			T) ;; The state is equal than the node state?
		(T  
			(remember-state?  state  (rest  memory-list))))
	)


(defun  filter-memories 
    (states-and-ops)
    "Filter a list of states-and-operations removing the elements that are in the memory *memory*
    states-and-ops structure:[(<state> <op>) (<state> <op>) ... ]"
    (cond 
	 	((null  states-and-ops)  
			Nil)
	    ((remember-state? (first (first  states-and-ops)) *memory*) ;; If remember the first element of the list filter it...
		    (filter-memories  (rest  states-and-ops)))
		(T
			(cons  (first states-and-ops) 
			(filter-memories  (rest  states-and-ops)))) ;; in other case, added it to the answer
		) 
	)

;;;=======================================================================================
;;  EXTRACT-SOLUTION and DISPLAY-SOLUTION
;;       Recover and show the solution sequence of the problem...
;;       extract-solution Receives a node (with the target state) that is in memory and backtrack the parents until the initial state
;;       display-solution Show on screen the global list *solution* where already is and in correct order the solution of the problem
;;;=======================================================================================
(defun extract-solution 
	(node)
    "Backtrack in *memory* all the parents of [node] until the initial state"
    (labels (
		(locate-node (id list)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		(cond 
			((null list) Nil)
		    	((eql  id  (first (first list))) (first list))
		    (T  
				(locate-node  id (rest list)))))
		)
	  	(let (
			(current (locate-node (first  node)  *memory*)))
			(loop  while  (not (null  current))  do                        
				(push current *solution*) ;; Add the current node to the solution
				(setq current (locate-node (third current) *memory*))) ;; then change the parent...
			)
	    *solution*)
	)

(defun  display-solution 
	(nodes-list last-node method)
    "Display in a formatted and numerated way the solution"
	(format T "Search method: ~d~%" method)
	(format T "Total nodes: ~d~%" last-node)
	(format T "Total proccessed nodes: ~d~%" (length *memory*))
	(format T "Max lenght of frontier search : ~d~%" *maxLengthOfOpen*)
    (format T "Solution with ~A  steps:~%~%" (1- (length  nodes-list)))
    (let (
		(node  nil))
        (dotimes 
			(i (length  nodes-list))
			(setq  node  (nth  i  nodes-list))
	      	(if  
				(= i 0)
				(format t "Starts on: ~A~%" (second node)) ;; From this initial states
		   		(format t "\( ~2A\)  applying ~20A go to ~A~%"  i (fourth  node)  (second  node))) ;; print the step, operator and state
			)
		)
	)

;;;=======================================================================================
;;  RESET-ALL and BLIND-SEARCH
;;      Recall and show the solution sequence of the problem
;;          reset-all Reset all  global variables for new executions
;;          bind-search Main function, executes the search from initial state to target state
;;;=======================================================================================
(defun reset-all 
	() 
	"Reset all global variables to execute a new blind-search..."
	(setq  *open*  NIL)
	(setq  *memory*  NIL)
	(setq  *id*  0)
	(setq  *current-ancestor*  NIL)
	(setq  *solution*  NIL))

(defun  blind-search 
	(initial-state target-state method)
	"Executes a blind-search with the specified method from initial to target state
    available methods:  :depth-first
                        :breath-first"
  	(reset-all)
  	(let (
		(node nil)
	  	(state nil)
	  	(children  '())
	  	(operator  nil)
	  	(target-found  nil))
      	(insert-to-open initial-state nil method)
      	(loop until 
			(or 
				target-found
    	    	(null *open*))  
			do
			(setq 
				node (get-from-open) ;; Get the next node in the search frontier
				state (second  node) ;; Identify the state and operator
				operator (third  node))
	   		(push  node  *memory*) ;; Save it if something happend
			(if
				(> (length *open*) *maxLengthOfOpen*)
				(setf *maxLengthOfOpen* (length *open*))
			)
	   		(cond 
			   	((equal target-state state)
		            (display-solution  (extract-solution node) (first node) method)
		            (setq  target-found  T))
		        (t
					(setq *current-ancestor*  (first node)) 
			    	(setq children (expand state))
			     	(setq children (filter-memories children)) ;; Filter states that already have been proccesed
			      	(loop for  element  in  children  do
				    	(insert-to-open  (first element)  (second element)  method)))
				)
			)
		)  
	)
  
;;;=======================================================================================
;;;=======================================================================================

;;; Execution sentence: sbcl --script frogsAndToads.lisp
;;; Traditional cases
(time (blind-search '(a a a 0 b b b) '(b b b 0 a a a) :breath-first))
(time (blind-search '(a a a 0 b b b) '(b b b 0 a a a) :depth-first))
;;; Custom case
(time (blind-search '(a a a a 0 0 b b b b) '(b b b b 0 0 a a a a) :depth-first))
(time (blind-search '(a a a a 0 0 b b b b) '(b b b b 0 0 a a a a) :breath-first))
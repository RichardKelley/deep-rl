;;;; Utility functions 
(defmacro while (condition &body body)
  `(do ()
       ((not ,condition))
     (progn
       ,@body)))

(defun random-choice (choices)
  "Return a random choice according to the probabilities in list of choices."
  (let* ((z (random 1.0))
	 (probs (mapcar #'car choices))
	 (cdf (loop for x in probs sum x into y collect y))
	 (n 0))
    (progn
      (while (> z (nth n cdf))
	(incf n))
      (cdr (nth n choices)))))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun flatten (ls)
  (let ((result '()))
    (dolist (l ls)
      (setf result (append result l)))
    (remove-duplicates result)))

;;;; Code for list-based MDPs
(defun source-states (m) (remove-duplicates (mapcar (lambda (x) (first x)) m)))

(defun target-states (m)
  (let ((transitions (mapcar (lambda (x) (third x)) m)))
    (remove-duplicates
     (flatten
      (loop for trs in transitions collect
	   (loop for edge in trs collect
		(second edge)))))))

(defun mdp-states (m)
  (union (source-states m) (target-states m)))

;(defun mdp-actions (m &optional (state '()))
;  (remove-duplicates (mapcan (lambda (x) (list (second x))) m)))

(defun mdp-actions (m &optional (state nil))
  "Return the actions for MDP m. If state is not null, return actions available from state."
  (if (null state)
      (remove-duplicates (mapcan (lambda (x) (list (second x))) m))
      (remove-duplicates
       (mapcar #'second
	       (remove-if-not (lambda (x) (equal (car x) state)) m)))))

(defun mdp-get-choices (m state action)
  (let ((result '()))
    (dolist (tr m)
      (when (equal (list state action) (list (first tr) (second tr)))
	(setf result (third tr))))
    result))

(defun step-mdp (m state action)
  (when (and (member state (mdp-states m)) (member action (mdp-actions m)))
      (random-choice (mdp-get-choices m state action))))

(defun simulate-mdp (m initial-state policy &optional (steps 10))
  (when (member initial-state (mdp-states m))
      (do* ((i 1 (incf i))
	    (action (funcall policy initial-state) (funcall policy state))
	    (rs (step-mdp m initial-state action) (step-mdp m state action))
	    (reward (cadr rs) (incf reward (cadr rs)))
	    (history (list initial-state action (cadr rs))
		     (append history (list state action (cadr rs))))
	    (state initial-state (car rs)))
	   ((>= i steps) (list reward history)))))

;;;; Example MDP & Policies
(defun test-policy (state)
  (cond ((equal state 'a) 'to-b)
	((equal state 'b) 'to-a)))

(defun human-policy (state)
  (progn
    (format t "You are in state ~a. What do you do?~%" state)
    (read)))

(defun random-policy (mdp state)
  (random-elt (mdp-actions mdp state)))

(setf test-mdp
      '((a to-b ((0.9 b 1) (0.1 a -1)))
	(a to-a ((1.0 a 1)))
	(b to-a ((0.6 a 3) (0.4 b -3)))))

(setf simple-mdp
      '((a to-b ((1.0 b -10)))
	(a to-a ((1.0 a 1)))
	(b to-a ((1.0 a 1)))
	(b to-b ((1.0 b -10)))))
      
;; Simulate test-mdp and test-policy for a few iterations
(simulate-mdp test-mdp 'a #'test-policy)

;;;; Q-Learning
(defun mdp-max (m q-mat state)
  (let ((actions (mdp-actions m state))
	(current-max most-negative-double-float))
    (dolist (a actions)
      (when (> (gethash (list state a) q-mat 0.0) current-max)
	(setf current-max (gethash (list state a) q-mat 0.0))))
    current-max))

(defun mdp-episode (m q-mat gamma alpha policy steps)
  (let ((previous-state nil)
	(current-state (random-elt (mdp-states m)))
	(i 1))
    (while (< i steps)
      (progn
	(setf action (funcall policy current-state))
	(setf previous-state current-state)
	(setf sr (step-mdp m current-state action))
	(setf current-state (car sr))
	(setf new-q-val (+ (gethash (list previous-state action) q-mat 0.0)
			   (* alpha (+ (cadr sr) (* gamma (mdp-max m q-mat current-state))
				       (- (gethash (list previous-state action) q-mat 0.0))))))
	(setf (gethash (list previous-state action) q-mat) new-q-val)
	(incf i)))))

(defun q-learn (m gamma alpha policy &optional (episodes 100) (steps 100))
  (let ((q-matrix (make-hash-table :test #'equal)))
    (do ((i 0 (incf i)))
	((> i episodes) q-matrix)
      (mdp-episode m q-matrix gamma alpha policy steps))))

;;;; Test our Q-Learning implementation for a simple deterministic MDP:
(setf table (q-learn simple-mdp 0.9 1.0 (lambda (s) (random-policy simple-mdp s))))

(loop for value being the hash-values of table
        using (hash-key key)
        do (format t "~&~A -> ~A" key value))

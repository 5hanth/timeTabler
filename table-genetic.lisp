(defpackage :tk.shanth.time-tabler
  (:use :common-lisp))

(in-package :tk.shanth.time-tabler)

(defconstant *input-file* "input.md")
(defconstant *map-file* "map.md")
(defconstant *subjects-per-day* 8)

(defun my-split-sequence (string &key (delimiterp #'delimiterp))
  (loop for beg = (position-if-not delimiterp string)
     then (position-if-not delimiterp string start (1+ end))
     for end = (and beg (position-if delimiterp string start beg))
     when beg collect (subseq string beg end)
     while end))

(defun delimiterp (c) (or (char= c #\,)))

(defun parse-markdown (markdown-file)
  "Input file is parsed into a hash with each batch as key and a list of assignments as value"
  (with-open-file (input-stream markdown-file)
    (loop
       with parsed = (make-hash-table :test #'equal)
       for  line   = (read-line input-stream nil nil)
       while line do
	 (let ((tokens (my-split-sequence line)))
	   (setf (gethash (car tokens) parsed)
		 (cdr tokens)))
       finally (return parsed))))

(defun random-init-subjects (remainder assignments)
  (loop for elements = (loop for subject
			  from 1 to remainder
			  collect (elt assignments
				       (random (1- (length assignments)))))
     while (/= (length elements)
	       (length (remove-duplicates elements)))
     finally (return elements)))

(defun init-population (parsed-hash)
  "Initial Population is generated using parsed-hash"
  (let ((population (make-hash-table :test #'equal)))
    (loop for batch
       being the hash-keys of parsed-hash
       using (hash-value assignments) do 
	 (setf (gethash batch population)
	       (append assignments
		       (random-init-subjects
			(- *subjects-per-day* (length assignments))
			assignments))))
    population))

(defun collisions-with-other (chromosome other)
  "Compare two chromosomes and return collisions list"
  (mapcar #'string= chromosome other))

(defun collisions-with-population (batch population)
  "Compare a batch with others and return a hash-table with values as lists (collisions to others with given batch)"
  (let ((collisions (make-hash-table :test #'equal)))
    (loop for other-batch
       being the hash-keys of population
       using (hash-value other-assignments)
       unless (string= batch other-batch) do
	 (setf (gethash other-batch collisions)
	       (collisions-with-other (gethash batch population)
				      other-assignments)))
    collisions))

(defun collisions-in (population)
  "Compare every batch with other chromosomes and return hash-table with values as hash-tables (collisions to other batches)"
  (let ((collisions (make-hash-table :test #'equal)))
    (loop for batch
       being the hash-keys of population do
	 (setf (gethash batch collisions)
	       (collisions-with-population batch population)))
    collisions))

(defun total-collisions-in (population)
  "Count total collisions in population"
  (loop for collisions-to-a-batch
     being the hash-values of (collisions-in population)
     sum (loop for individual-collisions
	    being the hash-values of collisions-to-a-batch
	    sum (count-if #'identity individual-collisions))))

(defun non-elites-of (population)
  "Eliminate Chromosomes with no collision"
  (let ((collisions-in-population (collisions-in population)))
    (loop for collisions-to-a-batch
       being the hash-values of collisions-in-population do
	 (loop for other-batch
	    being the hash-keys of collisions-to-a-batch
	    using (hash-value individual-collisions)
	    when (zerop (loop for collision
			   in individual-collisions
			   count collision)) do
	      (remhash other-batch collisions-to-a-batch)))
    collisions-in-population))

(defun lovers-in (population)
  (let ((lovers (make-hash-table :test #'equal)))
    (loop for batch
       being the hash-keys of (non-elites-of population)
       using (hash-value collisions-to-a-batch) do
	 (setf (gethash batch lovers)
	       (loop for other-batch
		  being the hash-keys of collisions-to-a-batch
		  using (hash-value collisions)
		  collect `(,other-batch
			    ,(+ (random 1.0)
				(- 1
				   (/ (loop for collision
					 in collisions
					 count collision)
				      (total-collisions-in population))))))))
    lovers))

(defun do-breakups-for (lovers-in-population)
  (loop for lovers-to-an-individual
     being the hash-keys of lovers-in-population
     using (hash-value lovers) do
       (setf (gethash lovers-to-an-individual lovers-in-population)
	     (loop for (lover love%)
		in lovers
		unless (< love% 0.5)
		collect `(,lover ,love%)))
     finally (return lovers-in-population)))

(defun have-common-elements (sequence-1 sequence-2)
  "Return T if atleast 1 element is common to both sequences"
  (loop for element in sequence-1
     when (member element sequence-2 :test #'string=) do
       (return t)))

(defun complications-in (after-breakups)
  (remove-duplicates
   (mapcar #'cdr
	   (sort
	    (apply #'append
		   (loop for lover
		      being the hash-keys of after-breakups
		      using (hash-value complications)
		      collect (loop for (other love%)
				 in complications
				 collect `(,love% ,lover ,other))))
	    #'(lambda (selfie other)
		(> (car selfie) (car other)))))
   :test #'have-common-elements))

(defun all-true-positions-of (haystack)
  (loop for element in haystack 
     and position from 0
     when element
     collect position))

(defun random-position-not-near-to (true-position lst)
  (loop for position = (random (length lst))
     then (random (length lst))
     while (or
	    (if (= true-position position) t
		(if (zerop position)
		    (or (string= (elt lst position)
				 (elt lst true-position))
			(string= (elt lst 1)
				 (elt lst true-position)))
		    (if (= position (1- (length lst)))
			(or (string= (elt lst position)
				     (elt lst true-position))
			    (string= (elt lst (1- position))
				     (elt lst true-position)))
			(or (string= (elt lst position)
				     (elt lst true-position))
			    (string= (elt lst (1- position))
				     (elt lst true-position))
			    (string= (elt lst (1+ position))
				     (elt lst true-position))))))
	    (if (zerop true-position)
		(or (string= (elt lst true-position)
			     (elt lst position))
		    (string= (elt lst 1)
			     (elt lst true-position)))
		(if (= true-position (1- (length lst)))
		    (or (string= (elt lst true-position)
				 (elt lst position))
			(string= (elt lst (1- true-position))
				 (elt lst position)))
		    (or (string= (elt lst true-position)
				 (elt lst position))
			(string= (elt lst (1- true-position))
				 (elt lst position))
			(string= (elt lst (1+ true-position))
				 (elt lst position))))))
       
     finally (return position)))

(defun cross-over (y-chromosome x-chromosome)
  (loop for true-position
     in (all-true-positions-of
	 (collisions-with-other y-chromosome x-chromosome)) do
       (rotatef (elt y-chromosome true-position)
		(elt y-chromosome
		     (random-position-not-near-to true-position
						  y-chromosome)))))

(defun do-mating-in (complications-for population)
  (loop for (x-y x-x) in complications-for do
       (cross-over (gethash x-y population)
		   (gethash x-x population))))

(defun evolve (population)
  (do-mating-in
      (complications-in
       (do-breakups-for
	   (lovers-in population)))
    population)
  population)

(defun there-are-lovers-in (population)
  (loop for individual
     being the hash-keys of (lovers-in population)
     using (hash-value lovers)
     when lovers do (return t)))

(defun print-map-to-subjects (parsed population)
  (loop
     with mapped = (parse-markdown *map-file*)
     for batch
     being the hash-keys of population
     using (hash-value assignments) do
       (progn
	 (format t "~A~A" batch #\tab)
	 (loop for assignment in assignments do 
	      (format t "~A~A"
		      (elt (gethash batch mapped)
			   (position assignment
				     (gethash batch parsed)))
		      #\tab))
	 (format t "~%"))))

(defun init-genetic ()
  (loop
     with parsed = (parse-markdown *input-file*)
     as population = (init-population parsed)
     then (evolve population)
     while (there-are-lovers-in population)
     finally (print-map-to-subjects parsed population)))

(defun init-genetic-dbg ()
  (loop
     with parsed = (parse-markdown *input-file*)
     as population = (init-population parsed)
     then (evolve population)
     for i = 1 then (1+ i)
     while (there-are-lovers-in population) do
       (format t "Generation [~A] :~%" i)
       (dbg-units population)
       (print-map-to-subjects parsed population)
     finally (progn (format t "Final Generation :~%")
		    (dbg-units population)
		    (print-map-to-subjects parsed population)
		    (return population))))

(defun dbg (str o)
  (format t "[~A] :  ~A~%" str o))

(defun test ()
  (init-genetic))

(defun dbg-units (p)
  (dbg "Population" p)
  (setq l (lovers-in p))
  (setq cc (total-collisions-in p))
  (dbg "Collisions Count" cc)
  (dbg "Lovers" l)
  (setq b (do-breakups-for l))
  (dbg "Breakups" b)
  (setq c (complications-in b))
  (dbg "Complications" c)
  (setq m (do-mating-in c p))
  (dbg "Mating" p)
  (setq cc (total-collisions-in p))
  (dbg "Collisions Count" cc))

(test)

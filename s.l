(defun cross1-recursive(x y)
	(cond
		((and (atom x) (or (atom x) (atom y))) nil)
		((and (listp x) (listp y)) (append(list(list (car x) y)) (cross1-recursive(cdr x) y))) 	
	)
)

(defun cross1-iterative(x y)
	(cond
		((and (atom x) (or (atom x) (atom y))) nil)
		((and (listp x) (listp y))
			(do ((l x (cdr l)) (iter nil (append iter (list(list (car l) y)))))
				((null l) iter)
			) 
		)	
	)
)

(defun cross1-mapcar(x y)
	(cond
		((and (atom x) (or (atom x) (atom y))) nil)
		((and (listp x) (listp y))
			(mapcar #'(lambda (a) (cons a (list y))) x)
		)	
	)
)

(defun cross2-recursive(x y)
	(cond
		((or (atom x) (atom y)) nil)
		((and (listp x) (listp y))  (append 
										(cross2-help x y)
										(if (not(null x)) (cross2-recursive (cdr x) y))
									)
		)
	)
)

(defun cross2-help(x y)
	(append 
		(list(list (car x) (car y)))
		(if (not(null (cdr y))) (cross2-help x (cdr y)))
	)
)

(defun cross2-iterative(x y)
	(cond
		((and (atom x) (or (atom x) (atom y))) nil)
		((and (listp x) (listp y))
			(do ((l x) (iter nil))
				((null l) iter)
				(do ((q y) (iiter nil))
					((null q) (setq iter (append iter iiter) ))
					(setq iiter (append iiter (list(list (car l) (car q)))))
					(setq q (cdr q))
					)
				(setq l (cdr l))
			) 
		)	
	)
)

(defun cross2-mapcar(x y)
	(cond
		((or (atom x) (atom y)) nil)
		((and (listp x) (listp y))
			(apply 'append(mapcar #'(lambda(a) 
										(mapcar #'(lambda(b) 
														(apply 'append (list a) (list(list b)))
													) 
										y)
									) 
							x)
			)
		)	
	)
)

(defun my-every(fun q)
	(cond 
		((null q) T)
		((not(null q)) (and (funcall fun (car q))
			 (cond
				 ((not(null (cdr q))) (my-every fun (cdr q)))
				 ((null (cdr q)) T)
			 )
		))
	)
)

(defun flatp(x)
	(cond 
		((equal (length x) 1) (if (not(listp (car x))) T))
		((/= (length x) 1) 
			(my-every #'(lambda(a) (and (atom a) (not(null a)))) x)
		)
	)
)

(defun lenLFL(x)
	(cond
		((flatp x) (length x))
		((not(flatp x))
			(lenLFL-help x)
		)
	)
)

(defun lenLFL-help(x)
	(cond 
		((atom(car x)) (setq a '0))
		((and (listp(car x)) (flatp (car x))) (setq a (length (car x))))
		((and (listp(car x)) (not(flatp (car x)))) (setq a (lenLFL (car x))))
	)
	(cond 
		((not(null(cdr x))) (max a (lenLFL-help (cdr x))))
		((null(cdr x))  a) 
	)
)

(defun legalcondp(x)
	(cond 
		((atom x) T)
		((not(equal (car x) 'cond)) (and (legalcondp (car x)) (legalcondp (cdr x))))
		((and (equal (car x) 'cond) (legalcondp (cadr x)) (legalcondp (caddr x)) (legalArm (cdr x))))
	)
)

(defun legalArm(x)
	(cond 
		((atom x) nil)
		((and (listp(car x)) (< (length(car x)) 3) (not(null(car x)))
			(cond
				((not(null(cdr x))) (legalArm (cdr x)))
				((null(cdr x)) T)
			)
		))
	)
)

(defun rewrite(x)
	(cond 
		((not(legalcondp x)) nil)
		((legalcondp x) 
			(cond
				((atom x) x)
				((not(equal (car x) 'cond)) (append (list(rewrite(car x))) (rewrite (cdr x)))) ;for all exp not include cond
				((equal (car x) 'cond) (reCondList (cdr x)))
			)
		)
	)
)

(defun reCondList(x)
	;(prin1 x) (prin1 'OK)
	(append (list 'if) (rewrite (car x))
		(cond 
			((null (cdr(car x))) (rewrite (car x)))
		)
		(if (not(null(cdr x))) (list(reCondList (cdr x))))
	)
)

(defun check(x)
	(cond
		((not(legalcondp x)) nil)
		((legalcondp x) (setq a (eval x)) (setq b (eval (rewrite x)))
			(list (equal a b) a b)
		)
	)
)
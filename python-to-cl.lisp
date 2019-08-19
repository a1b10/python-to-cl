;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's import variants and their equivalent in lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; import xyz
(asdf:load-system 'xyz)

;; install/import
;; in sbcl:
(ql:quickload 'xyz)

;; in clisp:
(load #P"/home/josephus/quicklisp/setup.lisp")
(ql:quickload 'xyz)


;; import a file:
(load "file.lisp")

(defpackage :package
  (:use :cl "file))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's split
;; s.split(sep)
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :cl-ppcre)
(cl-ppcre:split " " "a b c")
;; ("a" "b" "c")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple string split function
;; https://lispcookbook.github.io/cl-cookbook/strings.html

;; (defun split-by-one-space (string)
;;   "Returns list of 'words' of sentence."
;;   (loop for i = 0 then (1+ j)
;;      as j = (position #\Space string :start i)
;;      collect (subseq string i j)
;;      while j)) ; works
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;; split by regex preserving matched delimiter
;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-ppcre:split "(<your regex pattern>)" <string-to-be-splitted> :with-registers-p t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove empty string from list of strings
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-string-p (s)
  (string= s ""))

(remove-if #'empty-string-p *list*)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's join
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :cl-strings)
(cl-strings:join '("a" "b" "c") :separator "-")
;; "a-b-c" ;; so works!


(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
          do (princ element out)
          when more
            do (princ separator out))))

(defun join (delim list)
  (format nil "~{~A~}" (reduce #'(lambda (a b) (cons a (cons delim b)))
          list :initial-value '() :from-end t))) ;; at the end one too much

;; (defmacr join (sep l)
;;   (format nil (concatenate 'string "~{~A^/(princ sep)/~}") l))


(join " " '("a" "b" "c"))
;; "a b c"

(format nil "~{~A~^ ~}" '("a" "b" "c"))
;; "a b c"

;; separator can be #\tab characters, too


(format nil "~{~A~}" `("\~\{\~\~A\~\^" ,sep "\~\}"))

(defmacro join (sep l)
      `(format nil ,(format nil "~{~A~}" `("\~\{\~A\~\^" ,(write-to-string sep) "\~\}")) l))


;; https://groups.google.com/forum/#!msg/comp.lang.lisp/Ac_FqxHKdnA/-hQf4-UJ8GAJ
(defun cl-user::printtab (stream &rest args)
  (declare (ignore args))
  (write-char #\Tab stream))

(format nil "~{~a~^~:*~/printtab/~}" '(1 2 3))

;; @Sylwester's Antwort: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join (l &key (sep ", "))
  (format nil (format nil "~a~a~a" "~{~a~^" sep "~}") l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(join '(1 2 3))
(join '(1 2 3) :sep #\Tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's string +
;; "a" + "bcd" + "e"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(concatenate 'string "a" "b" "c")
;; "abc"

(format nil "~{~A~}" (list "a" "b" "c"))
(format nil "~A~A~A" "a" "b" "c")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's string .replace()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :cl-ppcre)
(cl-ppcre:regex-replace-all "  " "This is a  string." " " :preserve-case t)
;; "This is a string."

;; only once
(cl-ppcre:regex-replace "  " "This is a  string  ." " ")
;; "This is a string  ."



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's .strip()
;; python's .lstrip()
;; python's .rstrip()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(string-trim '(#\Space #\Linefeed) "   This is a sequence.    ")
;; "This is a sequence."
(string-left-trim "([" "([foo])")
;; "foo])"
(string-right-trim ")]" *)
;; "foo")

;; can be a copied string or the original string if nothing was to string!




;; python's string .match()
;; python's string .find()
;; python's string .findall()
;; python's string .group()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's string slicing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(subseq s start end)
(subseq s start)

;; (subseq s :end ..) not possible do:
;; (subseq s 0 end)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's list slicing
;; l[start:end]
;; l[start:]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(subseq l start end)
(subseq l start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; l[0]
(first l)
(car l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; l[1:] ;; in python O(n)
(cdr l)
(rest l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; l[-1]
(last l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for faster access, do tail-wagging
(defparamter l '(1 2 3 4))
(defparameter *tail* (last l))
(tailp *tail* l) ;; t
(setf (cdr *tail*) '(5 6)
           *tail* (caddr *tail*))
l ;; (1 2 3 4 5 6)
*tail* ;; 6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's list access l[i]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elt l i)
(nth i l)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign l[i] = val

(setf (elt l i) val)
(setf (nth i l) val)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's list .push()
;; python's list .pop()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *l* '(a b c d e))
(push '0 *l*)
(pop *l*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's list .append()
;; python's list .extend()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; since lisp's append is python's extend ...
(append '(a b c d) (list (quote e)))
;; (a b c d e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp's append is python's extend
(append '(a b c d) '(e))
;; (a b c d e)


;;;;;;;;;;;;;;;;;;;;;;;
;; python's assignment
(defparameter   )                ;; with direct effect
(defvar  )                       ;; only first time effect else not
                                 ;; overwritten! only by `setf` change
                                 ;; able! avoids accidental re-definition! `defvar` - `setf` pair...

;; mutating assignment
(setf   )                        ;; accessing already existing!

(setq   )                        ;; less general forms
(set    )                        ;; always replacable by `setf`!

;; simple variable
(setf x 10)

;; array value
(setf (aref a 0) 10)

;; hash table value
(setf (gethash 'key hash) 10)

;; slot named 'field'
(setf (field o) 10)


(setf x (- x 1)) = (decf x)
(setf x (+ x 1)) = (incf x)
(setf x (- x N)) = (decf x N)
(setf x (+ x N)) = (incf x N)
(incf (aref *array* (random (length *array*))))  ;; increases random
;; element's value by 1
(setf (aref *array* (random (length *array*)))
      (1+ (aref *array* (random (length *array*))))) ;; wrong! random!
(let ((tmp (random (length *array*))))
  (setf (aref *array* tmp) (1+ (aref *array* tmp)))) ;; correct what incf does!

POP
PUSHNEW
ROTATEF
SHIFTF

(rotatef a b) ;; swaps values
(let ((tmp a)) (setf a b b tmp)  nil)

(shiftf a b 10) ;; shifts
(let ((tmp a)) (setf a b b 10) tmp) ;; a's original value returned!

;; rotatef and shiftf can be applied to any number




;; local variables with controlled scopes
(let ((var val)
      (var1 val1))
  ...)

(let* ((var val)
       (var1 val1))
  ...)
  
;; constants
(defconstant <name> <initial-value-form> [ documentation-string ])
;; you can redefine constant - but other functions referring to it
;; have to be re-evaluated
;; rather use `defparameter` and only really constant things
;; like `pi` use









;;;;;;;;;;;;;;;
;; python's x=y
;; norvig says:
(setq x y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's multiple assignment x, y = 1, 2

;; norvig: x, y = 1, 2
(psetq x 1 y 2)
(multiple-value-setq (x y) (values 1 2))

;; python's (1, 2, 3) uses memory in heap
;; lisp's   (values 1 2 3) on stack

;;;;;;;;;;;;;;;
;; x, y = y, x
(rotatef x y)

;;;;;;;;;;;;;;;
;; x.slot = y
(setf (slot x) y)


;;;;;;;;;;;;;;;;;;;;;;;
;; python's for-loops
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; for x in l:
;;    ...sth with x ...

(loop for x in l
      do ( ... ))  ;; do (progn () () ()))

(dolist (x l)
  ...)

;; 0 ... n times
(dotimes (i (length l)) ;; or (i n)
  ...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's list-expressions
;;
;; [ ...sth with x... for x in l]
;; [ ... if ... else for x in l]
;; [ ... for x in l if ... ]
;; [ ...wth with i,x... for i,x in enumerate(l)]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop for x in l
      collect ( ))

(loop for x in l
      when (cond)   ;; = 'if' ;;  unless (cond-neg) ;; = 'if not'
      collect ( ))

(loop for x in l
      collect (if cond alt1 alt2))

;; enumerate not necessary, since it is possible to loop several in
;; parallel
(loop for x in l1
      for y in l2
      collect ())

;; lisp is even better, with several 'for var = (expr)' statements
;; which can communicate each other like 'let*'
;; several things can be stated


;;;;;;;;;;;;;;;;;;;;;;
;; zipping in python 
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; using loop in lisp:

(loop for name in '(fred sue alic joe june)
      for kids in '((bob ken) () () (kris sunshine) ())
      collect name
      append kids)
;; (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using `mapcar` and `list` in lisp:

(mapcar #'list '(1 2 3) '(4 5 6) '(7 8 9))
;; ((1 4 7) 2 5 8) (3 6 9))
;; shortest defines length or number of outcome!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; producing pair-list
(pairlis '(1 2 3 4) '(a b c d))
;; ((4 . D) (3 . C) (2 . B) (1 . A))
(let ((alist '((hello  . world) (foo . bar))))
     (pairlis '(1 2) '(a b) alist))
;; ((2 . B) (1 . A) (HELLO . WORLD) (FOO . BAR))


;; python's generators

;; ( ...sth with x... for x in l)
;; ( ... if ... else ... for x in l)
;; ( ... for x in l if ... )
;; ( ...wth with i,x... for i,x in enumerate(l))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's dictionaries
;;
;; {x: ... for x in l}
;; {x: ... if ... else ... for x in l}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h = {}
(setq h (make-hash-table :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h[key1] = val1
(setf (gethash key1 h) val1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h[key1]
(gethash key1 h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h.get(k, 0) ;; default value access
(gethash k h 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h = {"one": 1, "two": 2} 
(let ((h (make-hash-table :test #'equal)))
  (loop for k in '("one", "two")
        for v in '(1, 2)
        do (setf (gethash k h) v))
  h)


(loop for k in key-list
      for v in val-list
      do (setf (gethash k h) 
               (if <cond> v1 v2)))

;; hint: any time abbreviations creatable!

(defun two-lists-2-dict (kl vl &key (test #'eql))
  "Returns a dict from two lists."
  (let ((helper-dictionary (make-hash-table :test test)))
    (loop for k in kl
          for v in vl
          do (setf (gethash k helper-dictionary) v))
    helper-dictionary))

(two-lists-2-dict '(1 2 3 4 5) '(a b c d e))
;; #S(HASH-TABLE :TEST FASTHASH-EQL (5 . E) (4 . D) (3 . C) (2 . B) (1 . A))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's file i/o
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading a whole file at once

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to one string

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to list of lines

(defun file-get-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; both: http://sodaware.sdf.org/notes/cl-read-file-into-string/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; by Edi Weitz

(defun file-at-once (filespec &rest open-args)
  (with-open-stream (stram (apply #'open flespec open-args))
    (let* ((buffer (make-array (file-length stream)
                               :elment-type (stream-element-type stream)
                               :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))

(defun number-of-users ()
  (count #\Newline
         (file-at-once "/etc/passwd"
                       :element-type 'character)))

(number-of-users)

;; LispWorks-specific:
(subseq (hcl:file-string "/etc/passwd") 0 4) ;; "root"
;; for bonus: implement (SETF FILE-AT-once) ;; Recipe 10-8

;; A LLEGRO CL has EXCL:FILE-CONTENTS

;; ALEXANDRIA library (Recipe 18-4) 
;; READ-FILE-INTO-BYTE-VECTOR
;; READ-FiLE-INTO-STRING


;; READ-LINE
;; READ-CHARACTER

;; READ-SEQUENCE ;; chunk by chunk


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with open(infilepath, 'wra') as fin:
;;   for line in fin:
;;     ... sth with line ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-open-file (out "/tmp/foo.txt"
                     :direction :output)
  (write-line "First line" out)
  (write-line "Second line" out)
  (write-line "The third line" out :start 4 :end 9) ;; write only part
  (write-string "Last line, without Newline" out))
;; "Last line, without Newline"

(with-open-file (in "/tmp/foo.txt")
  (loop for (line no-nl-p)
          = (multiple-value-list (read-line in nil nil))
        while line
        do (format t "~S~:[ <newline at end>~;~]~%" 
                   line no-l-p)))

"First line" <newline at end>
"Second line" <newline at end>
"third" <newline t end>
"Last line, wihtout Newline"
NIL





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with open(infilepath) as fin,
;;      open(outfilepath,'wa') as fount:
;;   for line in fin:
;;     print fout >> ... sth with line ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-open-file (in "~/foo.txt")
  (with-open-file (out "~/foo.out.txt"
                       :direction :output)
    (loop for (line no-nl-p)
            = (multiple-value-list (read-line in nil nil))
          while line
          do (progn 
               (format t "~S~%" line)
               (format out "~S~%" line)
               (write-line line out)))))
;; it worked and "~S" had '" "' with it
;; while write-line printed as visible. cool!
;; So one can always use format with "~%" at end to write lines!





;; python's decorator

;; use clojures!

;; there are ways to use the condition system to mimic decorators.
;; however, the condition system is global, so you can run only
;; one decorator at a time.

;; in fact, decorators are nothing else than functions with memory
;; (which remember which was the last result).
;; Or you could say they are functions with states - which continue to live on
;; after the function call.

;; this you can mimic by createing classes - which have class attributes
;; which concitute the "memory".

;; but the more lispy way is to use clojures - which are function with their
;; own evaluation environment - which live on. They are more lightweight
;; than classes - but support memory for each function.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python's range

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looping with DO (from PCL - P. Seibel)

do
dolist             ;; looping over list
dotimes            ;; counting loops
loop

;; all lisp's looping control constructs are macros
;; built on top of:
;; TAGBODY and GO (Ch20)

(dolist (<var> <list-form>)   ;; Python's `for` and Perl's `foreach`
  <body-form*>)

(dotimes (<var> <count-form>) ;; must evaluate to an integer
  <body-form*>)               ;; <var> holds 0 to 1- integer

;; nestable
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;; dolist and dotimes only one looping variable


(do (<variable-definition*>)
    (<end-test-form> <result-form*>)
  <statement*>)

;; <variable-definition*>
(<var> <init-form> <step-form>) ;; stepform always before next var
;; if no <step-form> given, var stays from loop to loop the same

;; if <init-form> left out like `let` var bound to nil

;; after vars got values
;; <end-test-form> evaluated, if evaluates to NIL, operation proceeds
;; meaning <statements> executed
;; but when T, result-forms evauated
;; and value of last result form returned as value of entire DO expr

;; in step-forms you can refer to any of the other values
(DO*;; assigns each var its value before evaluating the step form for 
;; subsequent variables

(do ((nums nil) 
     (i 1 (1+ i)))
    ((> i 10) 
     (nreverse nums))
  (push i nums)) ;; → (1 2 3 4 5 6 7 8 9 10)

;; 11th fibonacci number
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
  finally (return a))



















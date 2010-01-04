;;====================================================================
;; strips.lisp
;;
;;
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================
;;(declaim (optimize (debug 3) (safety 3)))
;;(declaim (optimize (debug 0) (safety 0) (compilation-speed 0) (speed 3) (space 3)))
(declaim (optimize (debug 0) (safety 0) (compilation-speed 0) (speed 3) (space 0)))

(in-package :tengu)

(defclass* <strips-problem>
    (<graph>)
  ((name nil)
   (actions nil)
   (predicates nil)
   (constant-predicates nil)
   (initial-state nil)
   (class-obj-alist nil)
   (constant-state nil)
   (type-alist nil)))

;; うーん, びみょう...
(defclass* <strips-node>
    (<node>)
  ((value nil)))

(defclass* <strips-action>
    ()
  ((precondition nil)
   (effect nil)
   (parameters nil)
   (parameters-type nil)
   (name nil)
   (pre-check nil)
   ))

(defclass* <strips-predicate>
    ()
  ((parameters nil)
   (parameters-type nil)
   (name nil)))

;; function: satisfy-condition-p
;; state = list, condition = s式
;; stateがconditionを満たしていればtを返す
(defun satisfy-condition-p (state condition)
  (declare (type list state condition))
  (cond ((null condition)
         ;; 最後まで行けた
         t)
        ((eq (car condition) 'and)
         (let ((flag t))
           (dolist (con (cdr condition))
             (let ((result (satisfy-condition-p state con)))
               (if result
                   nil
                   (progn (setf flag nil)
                          (return nil)))
               ))
           flag))
        ((eq (car condition) 'or)
         (dolist (con (cdr condition))
           (let ((result (satisfy-condition-p state con)))
             (if result
                 (return t)
                 nil)
             )))
        ((eq (car condition) 'not)
         (not (satisfy-condition-p state (cadr condition))))
        (t
         (cdr (assoc condition state :test #'equal)))
        ))

(defgeneric goal-test (g gs &key debug))
(defmethod goal-test ((g <strips-problem>) (gs <strips-node>) &key (debug nil))
  (when debug
    (debug-print-variable gs goal-test)
    (debug-print-variable (goal-state-of g) goal-test))
  (satisfy-condition-p (value-of gs) (goal-state-of g)))

(defgeneric objects-of (problem))
(defmethod objects-of ((problem <strips-problem>))
  (mapcar #'cdr (class-obj-alist-of problem)))

(defgeneric all-object-satisfy-type (problem type &key debug))
(defmethod all-object-satisfy-type ((problem <strips-problem>) (type symbol) &key (debug nil))
  (declare (type symbol type)
           (ignore debug))
  (when debug
    (debug-print-variable type all-object-satisfy-type))
  (remove-if
   #'null
   (mapcar
    #'(lambda (obj)
        (declare (type symbol obj))
        (when debug
          (debug-print-variable obj all-object-satisfy-type))
        (let ((type-tree (object-type-tree-of problem obj)))
          (declare (type list type-tree))
          (when debug
            (debug-print-variable type-tree all-object-satisfy-type))
          (if (find type type-tree) obj nil)))
    (objects-of problem))))

;; method: all-object-combination
;; 型をもらって, その方を満たすようなobjectの組み合わせを返す
;; つまり, <hoge> -- hoge-a, hoge-b, <fuga> -- fuga-a, fuga-bのとき,
;; (<hoge> <fuga>)をもらうと, ((hoge-a fuga-a) (hoge-a fuga-b) (hoge-b fuga-a) (hoge-b fuga-b))
;; を返す.
(defgeneric all-object-combination (g type action state &key debug))
(defmethod all-object-combination
    ((g <strips-problem>) (types cons) action state &key (debug nil))
  (declare (ignore debug))
  (let ((types-objects
         (mapcar #'(lambda (x) (all-object-satisfy-type g x :debug debug)) types)))
    (declare (type list types-objects))
    (if action
        (let ((tmp (%all-combination types-objects g action state)))
          (declare (type list tmp))
          (if tmp
              (all-combination tmp)
              nil))
        (all-combination types-objects))))

;; とりあえず適当
#|
types-objects
'((|2X2-A|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|)
  (|4-5| |4-4| |4-3| |4-2| |4-1| |3-5| |3-4| |3-3| |3-2|
   |3-1| |2-5| |2-4| |2-3| |2-2| |2-1| |1-5| |1-4| |1-3|
   |1-2| |1-1|))
|#
(defun find-number-in-list (lst)
  (declare (type list lst))
  (find-if #'numberp (flatten lst)))

;; 2引数も出来るようにする
(defun %all-combination (types-objects problem action state)
  (declare (type list types-objects))
  ;;(format t "precondition check~%")
  ;; types-objects ->
  (let ((pre-checks (pre-check-of action)))
    (declare (type list pre-checks))
    (block out
      (dolist (pre-check pre-checks)
        (declare (type list pre-check))
        ;; pre-check -> (empty ?a)
        (let* ((indexed-pre-check
                (replace-list (if (eq (car pre-check) 'not)
                                  (cadr pre-check)
                                  pre-check)
                              (parameters-of action)
                              (loop for i from 0 to (length (parameters-of action)) collect i)))
               (param-num (length (cdr indexed-pre-check))))
          (declare (type list indexed-pre-check)
                   (type fixnum param-num))
          ;; indexed-pre-check -> (at 0 1)
          ;; or非対応
          (let ((candidate-arguments
                 (chimi:all-combination
                  (mapcar #'(lambda (i)
                              (elt types-objects i))
                          (cdr indexed-pre-check)))))
            (declare (type list candidate-arguments))
            ;;(debug-print-variable candidate-arguments %all-combination)
            (let ((satisfy-arg-sets
                   (remove-if #'null
                              (mapcar #'(lambda (arg-set)
                                          (let ((pre-check-with-arg
                                                 (replace-list indexed-pre-check
                                                               (cdr indexed-pre-check)
                                                               arg-set)))
                                            (if (eq (car pre-check) 'not)
                                                (if (not (cdr (assoc pre-check-with-arg
                                                                     (append (constant-state-of problem)
                                                                             (value-of state))
                                                                     :test #'equal)))
                                                    (cdr pre-check-with-arg)
                                                    nil)
                                                (if (cdr (assoc pre-check-with-arg
                                                                (append (constant-state-of problem)
                                                                        (value-of state))
                                                                :test #'equal))
                                                    (cdr pre-check-with-arg)
                                                    nil))))
                                      candidate-arguments))))
              (declare (type list satisfy-arg-sets))
              ;;(debug-print-variable satisfy-arg-sets %all-combination)
              (dolist (i (cdr indexed-pre-check))
                (declare (type fixnum i))
                (let ((i-all-objects
                       (remove-duplicates
                        (mapcar #'(lambda (args)
                                    (elt args (position i (cdr indexed-pre-check))))
                                satisfy-arg-sets) :test #'equal)))
                  (declare (type list i-all-objects))
                  ;;(debug-print-variable i)
                  ;;(debug-print-variable i-all-objects)
                  (setf (elt types-objects i)
                        i-all-objects)
              ;; (dolist (args satisfy-arg-sets)
;;                 (debug-print-variable args)
                  ))))))
      ;;(format t "new types-objects -> ~s~%" types-objects)
      types-objects)))


(defgeneric equal-state-p (a b))
(defmethod equal-state-p ((a <strips-node>) (b cons))
  (equal (value-of a) b))
(defmethod equal-state-p ((a <strips-node>) (b <strips-node>))
  (equal (value-of a) (value-of b)))

(defun equal-strips-state-p (a b)
  (declare (type list a b))
  (equal a b))

(defmethod successors-of
    ((g <strips-problem>) (state <strips-node>) &key (debug nil))
  (mapcan
   #'(lambda (action)
       ;; 考えられうる引数を全て考えてから, preconditionで切っている
       ;; それだとメモリバカ食いらしい
       (let ((arg-list (all-object-combination
                        g (parameters-type-of action) action state
                        :debug debug)))
         (remove-if
          #'null
          (mapcar #'(lambda (arg)
                      (when debug
                        (debug-print-variable arg successors-of))
                      (when (satisfy-precondition-p g action state arg)
                        ;;:exec -> next-state
                        (let ((next-state (next-state action state arg)))
                          (if (find-if
                               #'(lambda (x)
                                   (equal-strips-state-p (value-of x)
                                                         (value-of next-state)))
                                       (nodes-of g))
                              nil
                              (progn
                                (add-node g next-state)
                                (cons (action-name-of action arg)
                                      next-state))))))
                  arg-list))))
   (actions-of g)))

(defgeneric create-state (g &key debug))
(defmethod create-state ((g <strips-problem>) &key (debug nil))
  (mapcan
   #'(lambda (pred)
       (let* ((obj-types (parameters-type-of pred))
              (arg-list (all-object-combination g obj-types nil nil :debug debug)))
         (when debug
           (debug-print-variable obj-types create-state)
           (debug-print-variable arg-list create-state)
           )
         (mapcar #'(lambda (args)
                     (cons (cons (name-of pred) args) nil))
                 arg-list)))
   (predicates-of g)))

(defgeneric create-constant-state (g &key debug))
(defmethod create-constant-state ((g <strips-problem>) &key (debug nil))
  (mapcan
   #'(lambda (pred)
       (let* ((obj-types (parameters-type-of pred))
              (arg-list (all-object-combination g obj-types nil nil :debug debug)))
         (when debug
           (debug-print-variable obj-types create-state)
           (debug-print-variable arg-list create-state)
           )
         (mapcar #'(lambda (args)
                     (cons (cons (name-of pred) args) nil))
                 arg-list)))
   (constant-predicates-of g)))

(defgeneric set-initial-state (g istates))
(defmethod set-initial-state ((g <strips-problem>) (istates cons))
  ;;:initial-state -> set-initial-state

  (setf (start-state-of g)
        (make-instance '<strips-node> :value (create-state g)))
  (setf (constant-state-of g) (create-constant-state g))
  (dolist (stt istates)
    (if (find (car stt) (constant-predicates-of g) :key #'name-of)
        (setf (cdr (assoc stt (constant-state-of g) :test #'equal)) t)
      (setf (cdr (assoc stt (value-of (start-state-of g)) :test #'equal)) t)))
  (add-node g (start-state-of g))       ;here?
  (start-state-of g))

(defgeneric strips-type-symbol-p (sym))
(defun strips-type-symbol-p (sym)
  (declare (type symbol sym))
  (let ((str (string sym)))
    (declare (type string str))
    (and (equal (elt str 0) #\<)
         (equal (elt str (1- (length str))) #\>))))

(defgeneric add-type-alist (g super-sub-dot))
(defmethod add-type-alist ((g <strips-problem>) (super-sub-dot cons))
  (push super-sub-dot (type-alist-of g)))

(defgeneric add-class-obj-alist (g type-obj-dot))
(defmethod add-class-obj-alist ((g <strips-problem>) (type-obj-dot cons))
  (push type-obj-dot (class-obj-alist-of g)))

(defgeneric add-objects (g args))
(defmethod add-objects ((g <strips-problem>) args)
  (dolist (arg args)
    (if (listp arg)
        ;; argがlistだったら
        ;; (type obj)もしくは (super-type sub-type)
        (if (strips-type-symbol-p (cadr arg)) ;cadrがstripsの型か調べる
            (add-type-alist g (cons (car arg) (cadr arg))) ;(super-type . sub-type)
            (add-class-obj-alist g (cons (car arg) (cadr arg)))) ;(type . obj)
        ;; listじゃなかったら型の定義
        (add-type-alist g (cons '<top> arg)))
    t))

(defgeneric object-direct-type-of (problem obj))
(defmethod object-direct-type-of ((problem <strips-problem>) obj)
  (declare (type symbol obj))
  (car (find obj (class-obj-alist-of problem) :key #'cdr)))

(defgeneric type-tree-of (problem type))
(defmethod type-tree-of ((problem <strips-problem>) type)
  (declare (type symbol type))
  (let ((cpa (list type)))
    (declare (type list cpa))
    (while (find (car cpa) (type-alist-of problem) :key #'cdr)
      (push (car (find (car cpa) (type-alist-of problem) :key #'cdr))
            cpa))
    cpa))

(defgeneric object-type-tree-of (problem obj))
(defmethod object-type-tree-of ((problem <strips-problem>) obj)
  (let ((direct-type (object-direct-type-of problem obj)))
    (type-tree-of problem direct-type)))

(defgeneric add-predicate (problem pre))
(defmethod add-predicate ((problem <strips-problem>) pre)
  ;; 重複チェック
  (setf (predicates-of problem)
        (delete (car pre) (predicates-of problem) :key #'name-of :count 1))
  (push (make-instance '<strips-predicate>
                       :name (car pre)
                       :parameters (mapcar #'cadr (cdr pre))
                       :parameters-type (mapcar #'car (cdr pre)))
        (predicates-of problem)))

(defgeneric add-constant-predicate (problem pre))
(defmethod add-constant-predicate ((problem <strips-problem>) pre)
  ;; 重複チェック
  (setf (constant-predicates-of problem)
        (delete (car pre) (constant-predicates-of problem)
                :key #'name-of :count 1))
  (push (make-instance '<strips-predicate>
                       :name (car pre)
                       :parameters (mapcar #'cadr (cdr pre))
                       :parameters-type (mapcar #'car (cdr pre)))
        (constant-predicates-of problem)))

(defmethod add-action ((problem <strips-problem>) ac)
  ;; 重複チェック
  (setf (actions-of problem)
        (delete (name-of ac) (actions-of problem) :key #'name-of :count 1))
  (push ac (actions-of problem)))

(defmethod find-predicate ((problem <strips-problem>) n)
  (declare (type symbol n))
  (find n (predicates-of problem) :key #'name-of))

(defmethod satisfy-precondition-p ((problem <strips-problem>)
                                   (action <strips-action>)
                                   (state <strips-node>) args)
  (declare (type list args))
  (let ((condition (replace-list (precondition-of action) ;(and (on a b) ...)
                                 (parameters-of action)   ;(a b ...)
                                 args)))                  ;(pet desk ...)
    ;;(satisfy-condition-p (value-of state) condition)))
    (satisfy-condition-p (append (constant-state-of problem) (value-of state)) condition)))

(defun change-to-satisfy-condition (state condition)
  (declare (type list condition))
  (setf (value-of state)
        (%change-to-satisfy-condition (value-of state) condition))
  state)


(defun %change-to-satisfy-condition (state condition)
  (declare (type list state))
  (cond ((null condition)
         state)
        ((equal (car condition) 'and)
         (dolist (c (cdr condition))
           (setq state (%change-to-satisfy-condition state c)))
         state)
        ((equal (car condition) 'not)
         (setf (cdr (assoc (cadr condition) state :test #'equal)) nil)
         state)
        (t
         (setf (cdr (assoc condition state :test #'equal)) t)
         state)))

(defun copy-list2 (lst)
  (cond ((null lst)
         nil)
        ((atom lst)
         lst)
        (t
         (cons (copy-list2 (car lst)) (copy-list2 (cdr lst))))))

(defun copy-strips-node (state)
  (let ((ret (make-instance '<strips-node>)))
    (setf (value-of ret) (copy-list2 (value-of state)))
    ret))

(defun next-state (action state arg)
  (declare (type list arg))
  (let ((condition (replace-list (effect-of action) (parameters-of action) arg)))
    (declare (type list condition))
    (change-to-satisfy-condition (copy-strips-node state) condition)))

(defmethod action-name-of ((action <strips-action>) arg)
  (declare (type list arg))
  (cons (name-of action) arg))

;; User APIs
(defmacro defobjects (problem sexp)
  `(add-objects ,problem ',sexp))

(defmacro defpredicate (problem sexp &key (constant nil))
  `(if ,constant
       (add-constant-predicate ,problem ',sexp)
       (add-predicate ,problem ',sexp)))

(defmacro definitial-state (problem state)
  `(progn
     (set-initial-state ,problem ',state)))

(defmacro defgoal-state (problem goal-condition)
  `(setf (goal-state-of ,problem)
         ',goal-condition))

(defmacro defaction (problem &key (name nil)
                                  (parameters nil)
                                  (parameters-type nil)
                                  (precondition nil)
                                  (pre-check nil)
                                  (effect nil))
  (let ((action (gensym)))
    `(let ((,action (make-instance '<strips-action>
                                   :name ',name
                                   :parameters ',parameters
                                   :parameters-type ',parameters-type
                                   :precondition ',precondition
                                   :pre-check ',pre-check
                                   :effect ',effect)))
           (add-action ,problem ,action))))

(defun solve-strips (problem &key (heuristic #'(lambda (x p) (declare (ignore x p)) 0)))
  "problemをa*で解く."
  (let ((solver (make-instance '<a*-graph-search-solver>
                               :problem problem
                               :heuristic heuristic)))
    (solve solver problem)))


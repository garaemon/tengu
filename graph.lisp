;;====================================================================
;;                             graph.lisp
;;
;;
;;                               written by R.Ueda(garaemon@gmail.net)
;;====================================================================
;;(declaim (optimize (debug 0) (safety 0) (compilation-speed 0) (speed 3) (space 3)))
(declaim (optimize (debug 0) (safety 0) (compilation-speed 0) (speed 3) (space 0)))

(in-package :tengu)

;; classes ....
;; class: <graph>
;; いわゆるproblem
(defclass* <graph>
    ()
  ((start-state nil)
   (goal-state nil)
   (nodes nil)))

;; class: <solver-node>
;; AIMA的にはnode
(defclass* <solver-node>
    ()
  ((state nil)
   (cost 0)
   (priority-value nil)                 ;for best first search
   (parent nil)
   (action nil)))

;; class: <solver>
;; 各種アルゴリズムに対して特殊化していくための規定仮想クラス
(defclass* <solver>
    ()
  ())

;; class: <graph-search-solver>
;; graph探索のためのsolver
(defclass* <graph-search-solver>
    (<solver>)
  ((open-list nil)
   (close-list nil)))

;; class: <breadth-first-graph-search-solver>
;; 幅優先探索アルゴリズム
(defclass* <breadth-first-graph-search-solver>
    (<graph-search-solver>)
  ())

;; class: <depth-first-graph-search-solver>
;; 深さ優先探索アルゴリズム
(defclass* <depth-first-graph-search-solver>
    (<graph-search-solver>)
  ())

;; class: <best-first-graph-search-solver>
;; 最良優先探索アルゴリズム
(defclass* <best-first-graph-search-solver>
    (<graph-search-solver>)
  ((key-func)
   (problem)))

;; class: <a*-graph-search-solver>
;; A*探索アルゴリズム
(defclass* <a*-graph-search-solver>
    (<best-first-graph-search-solver>)
  ((heuristic)))

;; class: <node>
;; ノードクラス. AIMA的にはstate
(defclass* <node>
    ()
  ((name nil)
   (neighbors nil)))

;; class: <arced-node>
;; arc付きノード. つまりコストが付いている
(defclass* <arced-node>
    (<node>)
  ((neighbor-arc-alist nil)))

;; class: <costed-arc>
;; cost付きarc
(defclass* <costed-arc>
    ()
  ((cost 0)))

;; class: <costed-graph>
;; cost付きarcを使うgraph
(defclass* <costed-graph>
    (<graph>)
  ())

;; method: goal-test
;; ゴールに達していたらtを返す
(defgeneric goal-test (g gs &key debug))
(defmethod goal-test ((g <graph>) (gs <node>) &key (debug nil))
  (declare (ignore debug))
  (eq (goal-state-of g) gs))

;; method: path-cost
;; グラフgにおいてfromからtoにactionで移動した際にかかるコストを返す.
;; コストはstart-stateからの積算.
(defgeneric path-cost (g from action to))
(defmethod path-cost ((g <graph>) (from <solver-node>) action (to <node>))
  (declare (ignore action to g))
    (+ 1 (cost-of from)))

;; method: add-node
;; ノードをグラフへ追加する
(defgeneric add-node (g n))
(defmethod add-node ((g <graph>) (n <node>))
  (push n (nodes-of g)))

;; method: add-neighbor
;; nの隣接ノードとしてtargetを追加する
(defgeneric add-neighbor (n target))
(defmethod add-neighbor ((n <node>) (target <node>))
  (push target (neighbors-of n))
  (neighbors-of n))

;; method: add-arc-from-to
;; add-arcで内部的に呼ばれる.
(defgeneric add-arc-from-to (g from to &key both))
(defmethod add-arc-from-to ((g <graph>) (from <node>) (to <node>) &key (both nil))
  (declare (ignore g))
  (add-neighbor from to)
  (if both
      (add-neighbor to from))
  t)

;; method: add-arc
;; グラフgにおいて, fromとtoをつなぐ.
;; じっさいにarcが作られるわけではない.
(defgeneric add-arc (g from to &key both))
(defmethod add-arc ((g <graph>) (from <node>) (to <node>) &key (both nil))
  (if (listp to)
      (dolist (l to)
        (add-arc-from-to g from l :both both))
      (add-arc-from-to g from to :both both)))

;; method: successord-of
;; グラフgにおいて, stateからたどりうるstateのリストを返す.
;; それぞれの要素は, (action . state)のドットリスト.
(defgeneric successors-of (g state &key debug))
(defmethod successors-of ((g <graph>) (state <node>) &key (debug nil))
  (declare (ignore debug))
  (mapcar #'(lambda (n)
              (cons :go-to n))
          (neighbors-of state)))

;; method: clear-nodes
;; グラフgのnodesをクリアする
(defgeneric clear-nodes (g))
(defmethod clear-nodes ((g <graph>))
  (setf (nodes-of g) nil))

;; method: find-node
;; nameをもつnodeをかえす
(defgeneric find-node (g name))
(defmethod find-node ((g <graph>) name)
  (find name (nodes-of g) :key #'name-of))

;; method: path-of
;; solver-nodeの親を順にたどって, rootから
;; sまでのリストを返す
(defgeneric path-of (s &optional prev))
(defmethod path-of ((s <solver-node>) &optional (prev nil))
  (if (parent-of s)
      (path-of (parent-of s) (cons s prev))
      (cons s prev)))

;; method: expand
;; グラフgにおいて, sからたどりうるsolver-nodeのリストを返す
(defgeneric expand (s g &key debug))
(defmethod expand ((s <solver-node>) (g <graph>) &key (debug nil))
  (declare (ignore debug))
  (let ((successors (successors-of g (state-of s))))
    (declare (type list successors))
    (mapcar
     #'(lambda (successor)
         (declare (type list successor))
         (make-instance '<solver-node>
                        :state (cdr successor)
                        :parent s
                        :action (car successor)
                        :cost (path-cost g s (car successor) (cdr successor))))
     successors)))

;; <solver>

;; method: solve
;; アルゴリズムsを用いてグラフgを解く
(defgeneric solve (s g &key debug))
(defmethod solve ((s <solver>) (g <graph>) &key (debug nil))
  (declare (ignore s g debug))
  (error "need to override this method"))

;; method: solve-by-name
;; startとgoalの名前を指定してからsolveを呼ぶ.
(defgeneric solve-by-name (s graph start goal))
(defmethod solve-by-name ((s <solver>) (graph <graph>) start goal)
  (setf (start-state-of graph) (find-node graph start))
  (setf (goal-state-of graph) (find-node graph goal))
  (solve s graph))

;; <graph-search-solver>

;; method: solve-init
;; solveの初めに呼び出す初期化メソッド
(defgeneric solve-init (s graph))
(defmethod solve-init ((s <graph-search-solver>) (graph <graph>))
  (setf (close-list-of s) nil)
  (add-to-open-list s (make-instance '<solver-node> :state (start-state-of graph)
                                                    :cost 0))
  t)

(defmethod solve ((solver <graph-search-solver>) (graph <graph>) &key (debug nil))
  (declare (type symbol debug))
  (block solve
    (solve-init solver graph)
    (while (not (null-open-list solver))
      (if debug
          (format t "current open-list -> ~A -- SOLVE --~%"
                  (mapcar #'state-of (open-list-of solver))))
      (let ((target-node (pop-from-open-list solver)))
        (if debug
            (debug-print-variable target-node solve))
        (cond ((goal-test graph (state-of target-node))
               (if debug
                   (format t "arrived at goal! -- SOLVE --~%"))
               (return-from solve (path-of target-node)))
            ((not (find (state-of target-node) (close-list-of solver)))
             (push (state-of target-node) (close-list-of solver))
             (format t "current cost is ~A~%" (cost-of target-node))
             (let ((expanded-nodes (expand target-node graph :debug debug)))
               (declare (type list expanded-nodes))
               (format t "~A nodes~%" (length expanded-nodes))
               (if expanded-nodes
                   (add-to-open-list solver expanded-nodes))) ;
             )
            )))
    (warn "open-list is nil -- SOLVE --~%")
    (warn "search was missed!! -- SOLVE --~%")
    (clear-open-list solver)
    (setf (close-list-of solver) nil)
    nil))


;; method: add-to-open-list
;; sのopen-listに追加する.
;; nodesは<solver-node>のリスト
(defgeneric add-to-open-list (s nodes))
(defmethod add-to-open-list ((s <graph-search-solver>) (nodes cons))
  (dolist (n nodes)
    (add-to-open-list s n))
  t)

;; method: null-open-list
;; sのopen-listがnilかどうか調べる
(defgeneric null-open-list (s))
(defmethod null-open-list ((s <graph-search-solver>))
  (null (open-list-of s)))

;; virtual methods
;; method: add-to-open-list
;; sのopen-list<solver-node>をに追加する.
(defmethod add-to-open-list ((s <graph-search-solver>) (node <solver-node>))
    (declare (ignore s node))
  (error "need to override this method"))

;; method: clear-open-list
;; sのopen-listをリセットする
(defgeneric clear-open-list (s))
(defmethod clear-open-list ((s <graph-search-solver>))
  (declare (ignore s))
  (error "need to override this method"))

;; method: pop-from-open-list
;; sのopen-listから一つ要素を取り出す
(defgeneric pop-from-open-list (s &key debug))
(defmethod pop-from-open-list ((s <graph-search-solver>) &key (debug nil))
  (declare (ignore s debug))
  (error "need to override this method"))

;; <breadth-first-graph-search-solver>
(defmethod clear-open-list ((s <breadth-first-graph-search-solver>))
  (setf (open-list-of s) nil))

(defmethod add-to-open-list ((s <breadth-first-graph-search-solver>) (node <solver-node>))
  (setf (open-list-of s) (nconc (open-list-of s) (list node)))
  (open-list-of s))

(defmethod add-to-open-list ((s <breadth-first-graph-search-solver>) (nodes cons))
  (setf (open-list-of s) (nconc (open-list-of s) nodes))
  (open-list-of s))

(defmethod pop-from-open-list ((s <breadth-first-graph-search-solver>) &key (debug nil))
  (declare (ignore debug))
  (pop (open-list-of s)))

;; <depth-first-graph-search-solver>
(defmethod clear-open-list ((s <depth-first-graph-search-solver>))
  (setf (open-list-of s) nil))

(defmethod add-to-open-list ((s <depth-first-graph-search-solver>) (node <solver-node>))
  (push node (open-list-of s))
  (open-list-of s))

(defmethod add-to-open-list ((s <depth-first-graph-search-solver>) (nodes cons))
  (setf (open-list-of s) (nconc nodes (open-list-of s)))
  (open-list-of s))

(defmethod pop-from-open-list ((s <depth-first-graph-search-solver>) &key (debug nil))
  (declare (ignore debug))
  (pop (open-list-of s)))

;; informed search

;; <best-first-graph-search-solver>
(defmethod clear-open-list ((s <best-first-graph-search-solver>))
  (setf (open-list-of s) nil))

(defmethod add-to-open-list ((s <depth-first-graph-search-solver>) (node <solver-node>))
  (push node (open-list-of s))
  (open-list-of s))

(defmethod add-to-open-list ((s <breadth-first-graph-search-solver>) (nodes cons))
  (setf (open-list-of s) (nconc nodes (open-list-of s)))
  (open-list-of s))

(defmethod pop-from-open-list ((s <best-first-graph-search-solver>) &key (debug nil))
  (let ((min-x (car (open-list-of s))))
    ;;(let ((min-value (keyfuncall s min-x (problem-of s))))
    (let ((min-value (or (priority-value-of min-x)
                         (setf (priority-value-of min-x) (keyfuncall s min-x (problem-of s))))))
      (dolist (x (cdr (open-list-of s)))
        (let ((v (or (priority-value-of x)
                     (setf (priority-value-of x) (keyfuncall s x (problem-of s))))))
          (if (< v min-value)
              (progn
                (setf min-value v)
                (setf min-x x)))))
      (when debug
        (format t "POP-FROM-OPEN-LIST result~%")
        (format t "min-value -> ~A~%" min-value)
        (format t "min-x -> ~A~%" min-x)
        )
      (setf (open-list-of s) (delete min-x (open-list-of s) :count 1))
      min-x)))

(defmethod add-to-open-list ((s <best-first-graph-search-solver>) (node <solver-node>))
  (push node (open-list-of s))
  (open-list-of s))

(defmethod add-to-open-list ((s <best-first-graph-search-solver>) (nodes cons))
  (setf (open-list-of s) (nconc (open-list-of s) nodes))
  (open-list-of s))

;; method: keyfuncall
;; 最良優先のキー関数を呼び出す
(defgeneric keyfuncall (s n p))
(defmethod keyfuncall ((s <best-first-graph-search-solver>) (n <solver-node>) (p <graph>))
  (funcall (key-func-of s) n p))

;; <a*-graph-search-solver>
(defmethod keyfuncall ((s <a*-graph-search-solver>) (n <solver-node>) (p <graph>))
  (+ (cost-of n) (funcall (heuristic-of s) n p)))

;; <arced-node>

;; method: find-arc
;; nに移るためのarcを返す
(defgeneric find-arc (node n))
(defmethod find-arc ((node <arced-node>) (n <arced-node>))
  (cdr (assoc n (neighbor-arc-alist-of node))))

;; method: add-neighbor
;; targetは(arc . node)のドットリスト
(defmethod add-neighbor ((node <node>) (target cons))
  "target = (arc . node)"
  (push target (neighbor-arc-alist-of node))
  (call-next-method node (cdr target))
  )

;; <costed-graph>

;; method: add-costed-arc
;; <costed-graph>のノードをコストを考慮して繋ぐ
(defgeneric add-costed-arc (g from to act-cost &key both))
(defmethod add-costed-arc ((g <costed-graph>) (from <arced-node>) (to <arced-node>) act-cost
                           &key (both nil))
  (add-neighbor from (cons (make-instance '<costed-arc> :cost act-cost) to))
  (if both
      (add-neighbor to (cons (make-instance '<costed-arc> :cost act-cost) from)))
  t)

(defmethod successors-of ((g <costed-graph>) (state <node>) &key (debug nil))
  (declare (ignore debug))
  (neighbor-arc-alist-of state))

(defmethod path-cost ((g <costed-graph>) (from <solver-node>) (action <costed-arc>) (to <node>))
  (declare (ignore to))
  (+ (cost-of from) (cost-of action)))

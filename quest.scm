(define-module (quest)
	       #:export (new-builder
			  register-task
			  fetch
			  mark-as-dirty))

(use-modules (rnrs base)
	     (rnrs hashtables)
	     (rnrs lists)
	     (rnrs io ports))

;; Creates a new default builder.
(define (new-builder)
  (list
    ;; Tasks that the builder knows how to execute.
    ;; Association list from `query` to `executor`.
    (cons 'tasks (list))
    ;; Stack of currently executing tasks in the form '(query key).
    (cons 'task-stack (list))
    ;; Acyclic graph of dependencies between the tasks.
    ;; Each node has an ordered list of children and a list of parents.
    ;;
    ;; It has the following form
    ;;
    ;; ```
    ;; AssocList[query, AssocList[key, DepGraphInfo]];
    ;;
    ;; DepGraphInfo = AssocList[
    ;; 		Pair[deps, Dependencies],
    ;; 		Pair[parents, Parents],
    ;; 		Pair[is-dirty?, IsDirty],
    ;; 		Pair[hash, Hash],
    ;; 		Pair[value, Value],
    ;; 		Pair[init, Init],
    ;; ];
    ;;
    ;; Dependencies = AssocList[Pair[query, key], ChildHash];
    ;; 	* Ordered list of tasks the current task depends on.
    ;; ChildHash: Hash of the value of the dependency.
    ;;
    ;; Parents = List[Triple[query, key]];
    ;;	* List of tasks that depend on the current task.
    ;;
    ;; IsDirty: boolean -> The node is dirty and must be recomputed.
    ;; Hash: number | #f - EqualHash of the value stored.
    ;; Result: v | #f - Memoized result of the current task.
    ;; Init boolean - The node has been initialized.
    ;; ```
    (cons 'dep-graph (list))))

;; Registers task `query` to `task`.
;;
;; A `task` is a pure function which takes the builder context and a `key` as parameter
;; and returns a value by either fetching its dependencies from the builder context,
;; and calculating a result, or returning a result immediately.
;;
;; (: register-task (forall [a Eq]
;;			    [k Eq]
;;			    [v]
;;			    (-> Builder
;;				a
;;				(-> Builder
;;				    k
;;				    v)
;;				())))
(define (register-task bctx query task)
  (append-element-assl bctx 'tasks query task))

;; Runs the task `query` with `key`
(define (fetch bctx query key)
  ; Find the task related to `query`.
  (let ([task (find-task bctx query)]
	[c-task (current-task bctx)])
    (when (not task)
      (assertion-violation 'fetch
			   "task not found: ~a"
			   query))

    (cond
      [(get-value (get-or-insert-node bctx query key))
       ; The value is cached, return it as-is.
       => (lambda (cached-value)
	    (when c-task
	      (register-dependency bctx
				   c-task
				   (cons query key)
				   (get-hash (get-node bctx query key))))
	    cached-value)]
      [else
	; We form a stack of executing tasks to trace and build the dependency graph between
	; the queries.
	(push-task bctx query key)
	(let* ([v (task bctx key)]
	       [hash (equal-hash v)])
	  (if (eqv? (get-hash (get-node bctx query key))
		    hash)
	    (mark-unchanged bctx query key)
	    (begin
	      (when c-task
		(register-dependency bctx
				     c-task
				     (cons query key)
				     hash))
	      (update-query-info bctx query key v hash)))
	  (pop-task bctx)
	  v)])))

;; Marks the node '(query key) and its parents transitively as dirty.
;;
;; The task for '(query key) **must** have been executed at least once.
;;
;; # Exceptions
;; * Throws an `assertion-violation` if the node doesn't exist.
(define (mark-as-dirty bctx query key)
  ; If the node is already dirty, skip it and its dependencies.
  (let ([key-node (get-node bctx query key)])
    (when (not (is-dirty? key-node))
      ; Mark the node as dirty, but don't touch anything else.
      ; Maybe it won't have to be recomputed.
      (set-cdr! (assq 'is-dirty? (cdr key-node)) #t)
      (for-each (lambda (dep) (mark-as-dirty bctx (car dep) (cdr dep)))
		(cdr (assq 'parents (cdr key-node)))))))

;; Appends the element `(key value)` to the association list `asslk`.
(define (append-element-assl bctx asslk key value)
  (let ([item (assq asslk bctx)])
    (set-cdr! item
	      (cons (list key value)
		    (cdr item)))))

;; Returns the item in the association list `asslk` associated to `key` or false.
(define (find-item-assl bctx asslk key)
  (let* ([assl (cdr (assq asslk bctx))]
	 [value (assq key assl)])
    (if value
      (cadr value)
      value)))

;; Pushes '(query . key) to the stack of currently executing tasks.
(define (push-task bctx query key)
  (let ([item (assq 'task-stack bctx)])
    (set-cdr! item (cons (cons query key)
			 (cdr item)))))

;; Pops the last executed task from the stack.
;;
;; # Exceptions
;; * Throws an `assertion-violation` if the stack is empty.
(define (pop-task bctx)
  (let ([item (assq 'task-stack bctx)])
    (when (null? (cdr item))
      (assertion-violation 'pop-task
			   "tried to pop from an empty task stask"))
    (set-cdr! item (cddr item))))

;; Returns the currently executing task '(query . key), or #f if none.
(define (current-task bctx)
  (let ([stack (cdr (assq 'task-stack bctx))])
    (if (null? stack)
      #f
      (car stack))))

;; Returns a new key node for `key` to be inserted into a `query` node.
(define (new-key-node key)
  (cons key
	(list
	  (cons 'deps '())
	  (cons 'parents '())
	  (cons 'is-dirty? #t)
	  (cons 'hash #f)
	  (cons 'value #f)
	  (cons 'init #f))))

;; Returns a new `query` node with a node for `key` present already.
(define (new-query-key-node query key)
  (cons query
	(list (new-key-node key))))

;; Returns the key-node for '(query key).
;;
;; # Exceptions
;; * Throws an `assertion-violation` if the node doesn't exist.
(define (get-node bctx query key)
  (let* ([assl (assq 'dep-graph bctx)]
	 [querynode (assq query (cdr assl))])
    (if querynode
      (let ([keynode (assq key (cdr querynode))])
	(if keynode
	  ; The key node already exists, return it.
	  keynode
	  ; Create a new key node, update the query node and return the newly created node.
	  (assertion-violation 'get-node
			       "couldn't find node ~a, query present, key missing"
			       (cons query key))))
      ; The query node doesn't exist, create a new one and return the inner key node.
      (assertion-violation 'get-node
			   "couldn't find node ~a, query missing"
			   (cons query key)))))

;; Returns or inserts a new node for '(query key) and returns the key node.
(define (get-or-insert-node bctx query key)
  (let* ([assl (assq 'dep-graph bctx)]
	 [querynode (assq query (cdr assl))])
    (if querynode
      (let ([keynode (assq key (cdr querynode))])
	(if keynode
	  ; The key node already exists, return it.
	  keynode
	  ; Create a new key node, update the query node and return the newly created node.
	  (let ([keynode (new-key-node key)])
	    (set-cdr! querynode (cons keynode
				      (cdr querynode)))
	    keynode)))
      ; The query node doesn't exist, create a new one and return the inner key node.
      (let ([querynode (new-query-key-node query key)])
	(set-cdr! assl (cons querynode
			     (cdr assl)))
	(cadr querynode)))))

;; Registers a dependency from `from (query key)` to `to (query key)`.
(define (register-dependency bctx from to hash)
  ; inserts `elem` at the end of `lst`. `lst` **must** not be '().
  (define (insert-end lst elem)
    (if (null? (cdr lst))
      (set-cdr! lst (list elem))
      (insert-end (cdr lst) elem)))

  ; Inserts `elem` of type '(query . key) into the list `assel` of type
  ; '(key . [list-of (query . key)]) if not exists.
  ; The element is inserted at the end of the list to preserve the dependency order.
  (define (insert-if-not-exists assel elem)
    (when (not (member elem (cdr assel)))
      (if (null? (cdr assel))
	(set-cdr! assel (cons elem
			     (cdr assel)))
	(insert-end (cdr assel) elem))))

  (let ([parent-node (get-or-insert-node bctx (car from) (cdr from))]
	[child-node (get-or-insert-node bctx (car to) (cdr to))])
    (let ([parent-deps (assq 'deps (cdr parent-node))]
	  [child-parents (assq 'parents (cdr child-node))])
      ; Try to insert a dependency '((query . key) . hash)
      ; from the parent to the child
      (cond
	; The dependency already exists, update the hash if outdated.
	[(assoc to (cdr parent-deps)) =>
				      (lambda (p)
					(when (not (= (cdr p)
						      hash))
					  (set-cdr! p hash)))]
	; The dependency list is empty, simply add it.
	[(null? (cdr parent-deps))
	 (set-cdr! parent-deps (cons (cons to hash)
				     (cdr parent-deps)))]
	; Append it at the end of the list of dependencies.
	[else (insert-end (cdr parent-deps)
			  (cons to hash))])

      (insert-if-not-exists child-parents from))))

;; Updates the key-node with the given value, stores its hash, and marks it as clean, and initialzed.
(define (update-query-info bctx query key value hash)
  (let ([keynode (get-or-insert-node bctx query key)])
    ; Mark the node as not dirty anymore.
    (set-cdr! (assq 'is-dirty? (cdr keynode)) #f)
    ; Store the hash of the value
    (set-cdr! (assq 'hash (cdr keynode)) hash)
    ; Store the value as well.
    (set-cdr! (assq 'value (cdr keynode)) value)
    ; Mark the node as initialized as well.
    (set-cdr! (assq 'init (cdr keynode)) #t)))

;; Returns if the key-node is dirty.
(define (is-dirty? key-node)
  (cdr (assq 'is-dirty? (cdr key-node))))

(define-syntax is-clean?
  (syntax-rules ()
    [(_ x) (not (is-dirty? x))]))

;; Returns the value from the key-node if not marked as dirty.
(define (get-value key-node)
  (if (and (is-clean? key-node)
	   (is-init? key-node))
    (cdr (assq 'value (cdr key-node)))
    #f))

;; Returns the hash from the key-node, even if marked as dirty.
(define (get-hash key-node)
  (cdr (assq 'hash (cdr key-node))))

;; Returns if the key-node has been initialized.
(define (is-init? key-node)
  (cdr (assq 'init (cdr key-node))))

;; Returns the task associated with `query`
(define (find-task bctx query)
  (find-item-assl bctx 'tasks query))

;; Marks the node as unchanged. It was marked as dirty, but its value - after being recomputed -
;; remained the same.
;; 
;; Tries to mark its parents as clean as well.
(define (mark-unchanged bctx query key)
  (let* ([key-node (get-node bctx query key)]
	 [parents (cdr (assq 'parents (cdr key-node)))])
    (set-cdr! (assq 'is-dirty? (cdr key-node))
	      #f)

    ; Now try to mark its parents as unchanged as well if none of their other dependencies are
    ; marked as dirty.
    (for-each (lambda (parent)
		(let* ([key-node (get-node bctx (car parent) (cdr parent))]
		       [deps (cdr (assq 'deps (cdr key-node)))])
		  ; If the parent node has been initialized
		  (when (and (is-init? key-node)
			     (for-all (lambda (dep)
					(let ([dep (get-node bctx (caar dep) (cdar dep))]
					      [saved-dep-hash (cdr dep)])
					  ; And all its dependencies are clean (not dirty).
					  (and (is-clean? dep)
					       ; And the current hash matches the saved hash.
					       (eqv? saved-dep-hash
						     (get-hash dep)))))
				      deps))
		    ; Then mark the parent as unchanged as well, and its parents transitively.
		    (mark-unchanged bctx (car parent) (cdr parent)))))
	      parents)))

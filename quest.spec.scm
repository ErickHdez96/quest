(import (srfi srfi-64)
	(quest))

(test-runner-current (test-runner-simple))

(define (maybe-apply fn . vals)
  (if (memq #f vals)
    #f
    (apply fn vals)))

(test-group
  "no dependencies tasks"
  (let ([bctx (make-builder)])
    (register-task bctx
                   'sprsh1
                   (lambda (bctx key)
                     (case key
                       [(A1) 10]
                       [(A2) 20]
                       [(B1) 15]
                       [(B2) 25]
                       [else #f])))
    (test-eqv 10 (fetch bctx 'sprsh1 'A1))
    (test-eqv 20 (fetch bctx 'sprsh1 'A2))
    (test-eqv 15 (fetch bctx 'sprsh1 'B1))
    (test-eqv 25 (fetch bctx 'sprsh1 'B2))
    (test-eq #f (fetch bctx 'sprsh1 'C2))))

(test-group
  "linear dependencies tasks"
  (let ([bctx (make-builder)])
    (register-task bctx
                   'sprsh1
                   (lambda (bctx key)
                     (case key
                       [(A1) 10]
                       [(A2) (maybe-apply
                              *
                              (fetch bctx 'sprsh1 'A1)
                              2)]
                       [else #f])))
    (register-task bctx
                   'sprsh2
                   (lambda (bctx key)
                     (case key
                       [(A1) (fetch bctx 'sprsh1 'A2)]
                       [(A2) (maybe-apply
                              +
                              (fetch bctx 'sprsh1 'A2)
                              2)]
                       [(B1) 15]
                       [(B2) (maybe-apply
                              +
                              (fetch bctx 'sprsh2 'B1)
                              (fetch bctx 'sprsh2 'B1))]
                       [else #f])))
    (test-eqv 20 (fetch bctx 'sprsh1 'A2))
    (test-eqv 30 (fetch bctx 'sprsh2 'B2))
    (test-eqv 22 (fetch bctx 'sprsh2 'A2))))

(test-group
  "tree-like dependencies tasks"
  (let ([bctx (make-builder)])
    (register-task bctx
                   'sprsh1
                   (lambda (bctx key)
                     (case key
                       [(A1) 10]
                       [(A2) 20]
                       [(A3) 30]
                       [(A4) 40]
                       [(B1) (maybe-apply
                              +
                              (fetch bctx 'sprsh1 'A1)
                              (fetch bctx 'sprsh1 'A2))]
                       [(C1) (maybe-apply
                              *
                              (fetch bctx 'sprsh1 'B1)
                              (fetch bctx 'sprsh1 'A3))]
                       [else #f])))
    (register-task bctx
                   'sprsh2
                   (lambda (bctx key)
                     (case key
                       [(D1) (maybe-apply
                               +
                               (fetch bctx 'sprsh1 'C1)
                               (fetch bctx 'sprsh1 'A4))]
                       [else #f])))
    (test-eqv 30 (fetch bctx 'sprsh1 'B1))
    (test-eqv 900 (fetch bctx 'sprsh1 'C1))
    (test-eqv 940 (fetch bctx 'sprsh2 'D1))))

(test-group
  "recompute tasks"
  (let ([bctx (make-builder)])
    (define calls-a1 0)
    (define a1-value 10)
    (define calls-a2 0)
    (define a2-value 100)
    (define calls-b1 0)

    (register-task bctx
                   'sprsh1
                   (lambda (bctx key)
                     (case key
                       [(A1) (set! calls-a1 (+ calls-a1 1))
                             a1-value]
                       [(A2) (set! calls-a2 (+ calls-a2 1))
                             a2-value]
                       [(B1) (set! calls-b1 (+ calls-b1 1))
                             (maybe-apply
                               +
                               (fetch bctx 'sprsh1 'A1)
                               (fetch bctx 'sprsh1 'A2))]
                       [else #f])))

    ; Fetching 'sprsh1 'A1 only calls a1
    (test-eqv 10 (fetch bctx 'sprsh1 'A1))
    (test-eqv 1 calls-a1)
    (test-eqv 0 calls-a2)
    (test-eqv 0 calls-b1)

    ; Fetching 'sprsh1 'A2 only calls a2
    (test-eqv 100 (fetch bctx 'sprsh1 'A2))
    (test-eqv 1 calls-a1)
    (test-eqv 1 calls-a2)
    (test-eqv 0 calls-b1)

    ; Fetching 'sprsh1 'B1 calls a1, a2, and b1
    (test-eqv 110 (fetch bctx 'sprsh1 'B1))
    ; a1 and a2 should have been memoized
    (test-eqv 1 calls-a1)
    (test-eqv 1 calls-a2)
    (test-eqv 1 calls-b1)

    ; Marking B1 as dirty doesn't mark A1 or A2 as dirty
    (mark-as-dirty bctx 'sprsh1 'B1)
    (test-eqv 10 (fetch bctx 'sprsh1 'A1))
    (test-eqv 100 (fetch bctx 'sprsh1 'A2))
    (test-eqv 1 calls-a1)
    (test-eqv 1 calls-a2)

    (test-eqv 110 (fetch bctx 'sprsh1 'B1))
    (test-eqv 1 calls-a1)
    (test-eqv 1 calls-a2)
    ; B2 must be recalculated
    (test-eqv 2 calls-b1)

    (set! a1-value 20)
    (set! a2-value 200)
    ; a1 wasn't marked as dirty, so A1 hasn't seen the new value
    (test-eqv 10 (fetch bctx 'sprsh1 'A1))
    ; a2 wasn't marked as dirty either, so A2 hasn't seen the new value
    (test-eqv 100 (fetch bctx 'sprsh1 'A2))
    ; nor has B1
    (test-eqv 110 (fetch bctx 'sprsh1 'B1))

    ; Marking A1 as dirty causes the task to fetch the new value.
    (mark-as-dirty bctx 'sprsh1 'A1)
    (test-eqv 20 (fetch bctx 'sprsh1 'A1))
    (test-eqv 2 calls-a1)
    ; A2 wasn't marked as dirty, so no new calls
    (test-eqv 1 calls-a2)
    (test-eqv 2 calls-b1)

    ; B1 was marked transitively as dirty as well.
    ; Only A1 and B1 need to be recomputed, therefore A2
    ; doesn't see the new value.
    (test-eqv 120 (fetch bctx 'sprsh1 'B1))
    (test-eqv 2 calls-a1)
    (test-eqv 1 calls-a2)
    (test-eqv 3 calls-b1)))

(test-group
  "shared child"
  ; B1     B2
  ; |       |
  ; |> A1  <|
  (let ([bctx (make-builder)])
    (define a1-value 10)

    (register-task bctx
                   'sprsh1
                   (lambda (bctx key)
                     (case key
                       [(A1) a1-value]
                       [(B1) (fetch bctx 'sprsh1 'A1)]
                       [(B2) (fetch bctx 'sprsh1 'A1)]
                       [else #f])))

    ; Memoize both B1 and B2
    (test-eqv 10 (fetch bctx 'sprsh1 'B1))
    (test-eqv 10 (fetch bctx 'sprsh1 'B2))

    ; Change input A1 and recalculate B1, but not B2
    (set! a1-value 20)
    (mark-as-dirty bctx 'sprsh1 'A1)

    ; B1*    B2*
    ; |       |
    ; |> A1* <|
    ; * - dirty

    (test-eqv 20 (fetch bctx 'sprsh1 'B1))

    ; B1     B2*
    ; |       |
    ; |> A1  <|
    ; * - dirty

    ; Mark A1 as dirty again without actually changing its value
    (mark-as-dirty bctx 'sprsh1 'A1)

    ; B1*    B2*
    ; |       |
    ; |> A1* <|
    ; * - dirty

    (test-eqv 20 (fetch bctx 'sprsh1 'B1))

    ; B1     B2*
    ; |       |
    ; |> A1  <|
    ; * - dirty

    ; B2 should be recalculated as the value of its dependency changed.
    (test-eqv 20 (fetch bctx 'sprsh1 'B2))
    ; B1     B2
    ; |       |
    ; |> A1  <|
    ; * - dirty
    ))

(test-group
  "strings as keys"
  (let ([bctx (make-builder)])
    (define call-fa 0)
    (define fa-contents "a")
    (define call-fb 0)
    (define fb-contents "b")

    (register-task
      bctx
      'file-contents
      (lambda (bctx key)
	(cond
	  [(string=? key "a") (set! call-fa (+ call-fa 1))
			      fa-contents]
	  [(string=? key "b") (set! call-fb (+ call-fb 1))
			      fb-contents]
	  [else #f])))

    (register-task
      bctx
      'concat
      (lambda (bctx key)
	(apply
	  string-append
	  (map (lambda (k) (fetch bctx 'file-contents (string k)))
	       (string->list key)))))

    (test-equal "ab" (fetch bctx 'concat "ab"))

    (set! fa-contents "aa")
    (test-equal "ab" (fetch bctx 'concat "ab"))

    (mark-as-dirty bctx 'file-contents "a")
    (test-equal "aab" (fetch bctx 'concat "ab"))
    (test-eq 2 call-fa)

    (set! fb-contents "bb")
    (mark-as-dirty bctx 'file-contents "b")
    (test-equal "aabb" (fetch bctx 'concat "ab"))
    (test-eq 2 call-fa)
    (test-eq 2 call-fb)))

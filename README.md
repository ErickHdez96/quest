# Quest

Build system based on "Build Systems à la Carte".

Given a ruleset of tasks, when fetching a `(query . key)` pair, builds its dependencies and returns the built value.
It memoizes every `(query. key)` pair to prevent rebuilding the same dependency multiple times.

## Example

```scheme
; Create a new Builder context to accummulate new tasks.
(define bctx (make-builder))

; Input value that may be changed from an outer entity.
(define value-a1 10)

; Registers a new task for a spreadsheet with rules for each cell (key),
; returning eiter a primitive or a formula that depends on other cells.
(register-task
  bctx
  'spreadsheet-1
  (lambda (bctx key)
    (case key
      ; Cell referencing an outside value that may be changed from an outer entity.
      [(A1) value-a1]
      ; Cell containing a primitive value
      [(A2) 20]
      [(A3) 30]
      ; Cell with formula `= A1 + A2`.
      [(B1) (+ (fetch bctx 'spreadsheet-1 'A1)
	       (fetch bctx 'spreadsheet-1 'A2))]
      [else #f])))

(register-task
  bctx
  'spreadsheet-2
  (lambda (bctx key)
    (case key
      ; Cell containing a reference to another spreadsheet.
      ; `=if(sprsh1:A1 = 10, sprsh1:A2 * 2, sprsh1:A3 * sprsh1:A3)
      [(C1) (if (eqv? (fetch bctx 'spreadsheet-1 'A1)
		      10)
	      (* (fetch bctx 'spreadsheet-1 'A2)
		 2)
	      (* (fetch bctx 'spreadsheet-1 'A3)
		 (fetch bctx 'spreadsheet-1 'A3)))]
      [else #f])))

; Fetch the value for 'spreadsheet-1 'A1
(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
; Evaluate the formula on 'spreadsheet-1 'B1
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
; Evaluate the formula on 'spreadsheet-2 'C1
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; 10
; 30
; 40

; The value was changed from outside the build system. However, as the pair
; '(spreadsheet-1 . A1) was memoized, it returns the old value.
(set! value-a1 20)

(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; 10
; 30
; 40

; The pair '(spreadsheet-1 A1) needs to be marked as dirty, to tell the build system
; that the task needs to be recalculated next time its fetched.
(mark-as-dirty bctx 'spreadsheet-1 'A1)

(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; 20
; 40
; 900
```

## API

### Types

```text
type Builder;

; Name of a task, ideally a symbol.
type Query = q => [q Eq?];

; Key given to a task to calculate a value for the query.
type Key = k => [k Equal?];

; Value returned from a task.
type Value = v => [v Hashable];

; Function that calculates a Value for a given Key.
; Input tasks may cause side effects (e.g. reading a file).
; Intermediate tasks **must** be pure.
type Task = (-> Builder Key Value);
```

### Public functions

* make-builder: `(-> Builder)`

Returns a new builder, takes no parameters.

```scheme
(define bctx (make-builder))
```
* register-task: `(-> Builder Query (-> Builder Key Value) ())`

Registers a new Task under the name `query`, to be used later by a `fetch`.

```scheme
(register-task
  bctx
  'file-contents
  (lambda (bctx filepath)
    (call-with-input-file filepath get-string-all)))
```

* fetch: `(-> Builder Query Key Value)`

Runs the Task associated with `Query` with `Key` to calculate `Value`, running dependent tasks along the way.
`Value` is memoized for the pair `(Query . Key)`, and will be returned immediately if not marked dirty.

```scheme
(fetch bctx 'file-contents "quest.scm")
```

* mark-as-dirty: `(-> Builder Query Key ())`

Marks the Task associated with `(Query . Key)` and all the other Tasks that depend on it as dirty. Forcing
the Tasks to be re-evaluated when called again.

```scheme
(mark-as-dirty bctx 'file-contents "quest.scm")
```

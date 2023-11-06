# Quest

Build system based on "Build Systems à la Carte".

## Example

```scheme
(define bctx (new-builder))
(define value-a1 10)

(define (maybe-apply fn . vals)
  (if (for-all (lambda (x) x) vals)
    (apply fn vals)
    #f))

(register-task
  bctx
  'spreadsheet-1
  (lambda (bctx key)
    (case key
      [(A1) value-a1]
      [(A2) 20]
      [(A3) 30]
      [(B1) (maybe-apply +
			 (fetch bctx 'spreadsheet-1 'A1)
			 (fetch bctx 'spreadsheet-1 'A2))]
      [else #f])))

(register-task
  bctx
  'spreadsheet-2
  (lambda (bctx key)
    (case key
      [(C1) (if (eqv? (fetch bctx 'spreadsheet-1 'A1)
		      10)
	      (maybe-apply *
			   (fetch bctx 'spreadsheet-1 'A2)
			   2)
	      (let ([a3 (fetch bctx 'spreadsheet-1 'A3)])
		(maybe-apply *
			     a3
			     a3)))]
      [else #f])))

(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; 10
; 30
; 40

(set! value-a1 20)

(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; A1 needs to be marked as dirty to get the new input.
; 10
; 30
; 40

(mark-as-dirty bctx 'spreadsheet-1 'A1)

(display (fetch bctx 'spreadsheet-1 'A1)) (newline)
(display (fetch bctx 'spreadsheet-1 'B1)) (newline)
(display (fetch bctx 'spreadsheet-2 'C1)) (newline)

; 20
; 40
; 900
```

(define lambda-char (string->symbol "\u03BB"))

(define (get-type expr) (if [not (pair? expr)]
	;; If not a pair, a "base" expression
	'base
	(let ([head (car expr)]) (cond
		;; If a quote, treat like a base expression
		([equal? head 'quote] 'base)
		;; We know it's some list-like type, let's check if it's an if-statement
		([equal? head 'if] 'if)
		;; If neither of above, check if lambda expression
		([and (pair? head) 
			(or (equal? (car head) 'lambda) (equal? (car head) lambda-char))] 'lambda)
		;; None of above, just a generic list expression
		(else 'list)
	))
))

(define (expr-compare a b) (let ([type-a (get-type a)] [type-b (get-type b)]) (cond
	; If different types, not meaningful to compare
	([not (equal? type-a type-b)] (list 'if '% a b))
	; If both base (note we already established a and b are the same type)
	([equal? type-a 'base] (expr-compare-base a b))
	; If both quotes, treat like base
	([equal? type-a 'quote] (expr-compare-base a b))
	; Now we know these are definitely lists check same length
	([not (equal? (length a) (length b))] (list 'if '% a b))
	; If both lambdas
	([equal? type-a 'lambda]
		(cons (expr-compare-lambda-wrapper (car a) (car b)) (expr-compare (cdr a) (cdr b)))
	)
	; Else both list-like or both if-statements, treat normally
	(else (cons (expr-compare (car a) (car b)) (expr-compare (cdr a) (cdr b))))
)))

; Compares "base" expressions, ie symbols, numbers, booleans, and quotes
(define (expr-compare-base a b) (cond
	; If equal, just return
	([equal? a b] a)

	; Boolean special cases 
	([and (equal? a #t) (equal? b #f)] '%)
	([and (equal? a #f) (equal? b #t)] '(not %))

	; Are different
	(else (list 'if '% a b))
))

(define (expr-compare-lambda-wrapper a b)
	(let ([c (cdr a)] [d (cdr b)] [lambda_new (fix-lambda (car a) (car b))])
		(let* ([ret (expr-compare-lambda base-replace base-replace (car c) (car d))] 
						[r (take-right ret 2)]
						[res (drop-right ret 2)]
						[a_impl (replace-all (first r) (cdr c))]
						[b_impl (replace-all (second r) (cdr d))])
			(cons* lambda_new (cons* res (expr-compare a_impl b_impl)))
		)
	)
)

(define (fix-lambda lambda_a lambda_b)
	(if [not (equal? lambda_a lambda_b)]
		lambda-char
		lambda_a
	)
)

; Takes in the lambda declarations, matches them up and also returns the resulting replacement functions
(define (expr-compare-lambda ra rb a_decl b_decl) (cond
	([not (empty? a_decl)] ; If still going through the declaration
		(if (equal? (car a_decl) (car b_decl)) 

			; If next symbol in declaration match, then cleanly matched
			(cons
				(car a_decl) 
				(expr-compare-lambda ra rb (cdr a_decl) (cdr b_decl))
			)

			; Not equal, join symbols and need to create a new replacement function to join future symbols 
			(let 
				([new_ra (lambda (x) (generic-replace x (car a_decl) (join-symbols (car a_decl) (car b_decl)) ra))]
					[new_rb (lambda (y) (generic-replace y (car b_decl) (join-symbols (car a_decl) (car b_decl)) rb))])
				(cons
					(join-symbols (car a_decl) (car b_decl))
					(expr-compare-lambda new_ra new_rb (cdr a_decl) (cdr b_decl)) 
				)
			)
		)
	)

	(else ; Hacky as hell, tack the replacement functions as the last element of our return value
		(list ra rb)
	)
))

; Maps a and b to a!b
(define (join-symbols a b)
	(string->symbol
		(string-append
			(symbol->string a) "!" (symbol->string b)
		)
	)
)

; Used to construct replacement function
(define (generic-replace x tar repl callback) (cond
	; If x matches tar_1 or tar_2, replace with repl
	([equal? x tar] repl)
	; Otherwise apply our callback on x
	(else (callback x))
))
(define (base-replace x) x)

; Used to apply replacement function on the lambda implementation, skipping over 
; nested lambda declarations themselves 
(define (replace-all r l) (cond
	; Empty terminates
	([empty? l] '())
	; If head not a pair, just apply replacement function to head
	([not (pair? (car l))] 
		(cons (r (car l)) (replace-all r (cdr l))))
	; If head is a pair, but it's a lambda, skip over definition and implementation
	([equal? (get-type l) 'lambda] 
		(cons (car l) (replace-all r (cdr l))))
	; If head is any other generic pair, recur down on it
	(else (cons (replace-all r (car l)) (replace-all r (cdr l))))
))

(define (test-expr-compare x y) 
	(let* ([res (expr-compare x y)] 
			[res_x (list 'let '([% #t]) res)]
			[res_y (list 'let '([% #f]) res)])
		(and
			(equal? (eval x) (eval res_x))
			(equal? (eval y) (eval res_y))
		)
	)
)

(require racket/trace) ; TODO: Delete this
(define (cons* a b) (cons a b))
; (trace expr-compare-lambda-wrapper)


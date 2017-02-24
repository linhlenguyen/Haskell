;;; self-interpreter.scm -*- Petite -*-
;;; a self-interpreter for (a subset of) Scheme in direct style
;;; Olivier Danvy, dProgSprog'08
;;; Version 1.0

;;; Use: start Petite Chez Scheme and load "self-interpreter.scm"
;;; then type (self-interpreter ">> ")
;;
;;; This interpreter is totally meta-circular.
;;; It can be loaded in a session with
;;;       (load "self-interpreter.scm")
;;; and started with
;;;       (self-interpreter ">>> ")
;;; at the price of a measurable slowness.
;;; NB. The prompts grow to reflect the growing layers of interpretation.

;;; Syntax:
;;; e ::= <integer>
;;;     | <string>
;;;     | <boolean>
;;;     | <identifier>
;;;     | (quote <representable-value>)
;;;     | (lambda (<identifier>*) e)
;;;     | (if e e e)
;;;     | (cond [e e]* [else e])
;;;     | (define <identifier> e)
;;;     | (set! <identifier> e)
;;;     | (case e [(<representable-value>*) e]* [else e])
;;;     | (and e*)
;;;     | (or e*)
;;;     | (begin e+)
;;;     | (let ([<identifier> e]*) e)
;;;     | (let* ([<identifier> e]*) e)
;;;     | (letrec ([<identifier> (lambda (<identifier>*) e)]*) e)
;;;     | (time e)
;;;     | (e e*)

;;; predefined procedures:
;;;     car cdr
;;;     caar cadr
;;;     cdar cddr
;;;     caddr cdddr
;;;     list-tail
;;;     last-pair
;;;     null? pair?
;;;     number? string? symbol?
;;;     zero? add1 sub1
;;;     + - *
;;;     cons equal?
;;;     = boolean?
;;;     negative? positive?
;;;     procedure?
;;;     list
;;;     set-car! set-cdr!
;;;     display
;;;     pretty-print newline
;;;     not length
;;;     load read
;;;     open-input-file eof-object?
;;;     close-input-port
;;;     member
;;;     error

;;; Semantics:
;;;
;;; Values:
;;; Val = Num + String + Ide + Pair + Proc + Subr + Fsubr
;;;
;;; Environments -- lexical extensions, then global, then pre-defined:
;;; Env = (Ide* x Val*)*
;;;
;;; Procedures, primitive functions and special forms:
;;; Proc =  Val* -> val
;;; Subr = Val* -> Val
;;; Fsubr = Expr* x Env -> Val

;;; ------ auxiliaries ------------------------------------------------------

(define _constant?
  (lambda (x)
    (or (number? x)
        (string? x)
        (boolean? x))))

(define _identifier?
  symbol?)

(define _applicable?
  (lambda (x)
    (and (pair? x)
         (case (car x)
           [(subr fsubr proc)
            (and (= 3 (length x))
                 (number? (cadr x))
                 (procedure? (caddr x)))]
           [else
	    #f]))))

(define _index
  (lambda (i is)
    (letrec ([loop
	      (lambda (n is)
		(cond
		  [(null? is)
		   -1]
		  [(equal? i (car is))
		   n]
		  [else
		   (loop (+ n 1) (cdr is))]))])
      (loop 0 is))))

(define _fetch-ftype car)
(define _fetch-arity cadr)
(define _fetch-value caddr)

(define _inProc
  (lambda (n a)
    (list 'proc n a)))

;;; Basic lexical environment extension:
(define _extend_env
  (lambda (xs vs env)
    (cons (cons xs vs) env)))

(define _access
    car)

(define _update
    set-car!)

;;; ----- the core -----------------------------------------------------------

;;; Expr * Env -> Val * {Something else}
(define _eval
  (lambda (e r)
    (cond
      [(_constant? e)
       e]
      [(_identifier? e)
       (_lookup e r)]
      [(pair? e)
       (_apply (_eval (car e) r) (cdr e) r)]
      [else
       (_error '_eval "unknown form: ~s" e)])))

; Ide * Env -> Val
(define _lookup
  (lambda (i r)
    (if (null? r)
        (let ([pos (_index i table-common-identifiers)])
          (if (not (negative? pos))
              (_access (list-tail table-common-values pos))
              (_error '_lookup "unbound identifier: ~s" i)))
        (let ([pos (_index i (caar r))])
          (if (not (negative? pos))
              (_access (list-tail (cdar r) pos))
              (_lookup i (cdr r)))))))

; Fun * List-of-Expr * Env -> Val
(define _apply
  (lambda (fo es r)
    (if (_applicable? fo)
        (case (_fetch-ftype fo)
          ((subr)
           (_apply_subr fo es r))
          ((fsubr)
           (_apply_fsubr fo es r))
          ((proc)
           (_apply_procedure fo es r))
          (else
            (_error '_apply "unknown functional object: ~s" (car fo))))
        (_error '_apply "unapplicable value: ~s" fo))))

; Subr * List-of-Expr * Env -> Val
(define _apply_subr
  (lambda (f es r)
    (if (not (= (length es) (_fetch-arity f)))
        (_error '_apply_subr "arity mismatch: ~s" es)
        (case (_fetch-arity f)
          [(0)
           ((_fetch-value f))]
          [(1)
           ((_fetch-value f) (_eval (car es) r))]
          [(2)
           ((_fetch-value f) (_eval (car es) r)
                             (_eval (cadr es) r))]
          [(3)
           ((_fetch-value f) (_eval (car es) r)
                             (_eval (cadr es) r)
                             (_eval (caddr es) r))]
          [else
           (_error '_apply_subr "arity: ~s" f)]))))

; Fsubr * List-of-Expr * Env -> Val
(define _apply_fsubr
  (lambda (fv es r)
    (if (or (= (length es) (_fetch-arity fv))
            (zero? (_fetch-arity fv))) ; arbitrary number of arguments
        ((_fetch-value fv) es r)
        (_error '_apply_fsubr "arity mismatch: ~s" es))))

; Proc * List-of-Expr * Env * Cont * Meta-Cont -> Val
(define _apply_procedure
  (lambda (p es r)
    (if (not (= (length es) (_fetch-arity p)))
        (_error '_apply_procedure "arity mismatch: ~s" es)
        ((_fetch-value p) (_evlis es r)))))

; List-of-Expr * Env -> Val
(define _evlis
  (lambda (es r)
    (if (null? es)
        '()
        ;;; left-to-right evaluation:
        (let ([v (_eval (car es) r)])
          (cons v (_evlis (cdr es) r))))))

;;; ----- the special forms: -------------------------------------------------

(define _quote
  (lambda (es r)
    (car es)))

(define _if
  (lambda (es r)
    (case (_eval (car es) r)
      [(#f)
       (_eval (caddr es) r)]
      [else
       (_eval (cadr es) r)])))

;;; simpler definition:
(define _if
  (lambda (es r)
    (if (_eval (car es) r)
        (_eval (cadr es) r)
        (_eval (caddr es) r))))

(define _cond
  (lambda (cs r)
    (if (null? cs)
        (_error '_cond "out of clauses: ~s" cs)
        (let ([c (car cs)])
          (cond
            [(equal? (car c) 'else)
	     (_eval (cadr c) r)]
	    [(equal? (cadr c) '=>)
	     (let ([c1 (_eval (car c) r)])
	       (if c1
		   (_eval ((caddr c) c1) r)
		   (_cond (cdr cs) r)))] 
            [(_eval (car c) r) 
             (_eval (cadr c) r)]
            [else
             (_cond (cdr cs) r)])))))

; (cond [(member 2 '(1 2 3 2)) => car] [else 'no]) ... (_eval (caddr cs) ...)

(define _lambda
  (lambda (es r)
    (let ([xs (car es)]
          [e (cadr es)])
      (_inProc (length xs)
               (lambda (vs)
                 (_eval e (_extend_env xs vs r)))))))

(define _define
  (lambda (es r)
    (let ([x (car es)]
          [e (cadr es)])
      (if (not (_identifier? x))
          (_error '_define "undefinable: ~s" x)
          (let ([v (_eval e r)])
            (let* ([global-env (car (last-pair r))]
                   [pos (_index x (car global-env))])
              (if (not (negative? pos))
                  (begin
                    (_update (list-tail (cdr global-env) pos) v)
                    x)
                  (begin
                    (set-car! global-env
                              (cons x (car global-env)))
                    (set-cdr! global-env
                              (cons v (cdr global-env)))
                    x))))))))

(define _set!
  (lambda (es r)
    (let ([x (car es)]
          [e (cadr es)])
      (if (not (_identifier? x))
          (_error '_set! "undefinable: ~s" x)
          (_L_set! x (_eval e r) r)))))

(define _L_set!
  (lambda (i v r)
    (let ((pos (_index i (caar r))))
      (cond
	[(not (negative? pos))
	 (let* ([location (list-tail (cdar r) pos)]
		[previous-value (_access location)])
	   (begin
	     (_update location v)
	     previous-value))]
	[(null? (cdr r))
	 (let ([pos (_index i table-common-identifiers)])
	   (if (not (negative? pos))
	       (begin
		 (set-car! (car r) (cons i (caar r)))
		 (set-cdr! (car r) (cons v (cdar r)))
		 (_access (list-tail table-common-values pos)))
	       (_error '_L_set! "undefined variable: ~s" i)))]
	[else
	 (_L_set! i v (cdr r))]))))

(define _case 
  (lambda (es r)
    (let ([v (_eval (car es) r)]) 
      (letrec ([loop
		(lambda (es)
		  (cond
		    [(null? es)
		     (_error '_case "unmatched: ~s" v)]
		    [(equal? (caar es) 'else)
		     (_eval (cadr (car es)) r)]
		    [(member v (caar es))
		     (_eval (cadr (car es)) r)]
		    [else
		     (loop (cdr es))]))])
	(loop (cdr es))))))

(define _and
  (lambda (es r)
    (letrec ([visit (lambda (e es)
                      (if (null? es)
                          (_eval e r)
                          (case (_eval e r)
                            [(#f)
                             #f]
                            [else
                             (visit (car es) (cdr es))])))])
      (if (null? es)
          #t
          (visit (car es) (cdr es))))))

(define _or
  (lambda (es r)
    (letrec ([visit (lambda (e es)
                      (if (null? es)
                          (_eval e r)
                          (let ([v (_eval e r)])
                            (case v
                              [(#f)
                               (visit (car es) (cdr es))]
                              [else
                               v]))))])
      (if (null? es)
          #f
          (visit (car es) (cdr es))))))

(define _begin
  (lambda (es r)
    (if (null? (cdr es))
        (_eval (car es) r)
        (begin
          (_eval (car es) r)
          (_begin (cdr es) r)))))

(define _let                  ; assumes a well-formed let construction
  (lambda (es r)
    (let ([bs (car es)]
          [e (cadr es)])
      (if (null? bs)
          (_eval e r)
          (_eval e (_extend_env (_let_idlis bs)
                                (_let_evlis bs r)
				r))))))

(define _let_evlis
  (lambda (bs r)
    (if (null? bs)
        '()
        (cons (_eval (cadr (car bs)) r)
              (_let_evlis (cdr bs) r)))))

(define _let_idlis
  (lambda (bs)
    (if (null? bs)
        '()
        (cons (car (car bs)) (_let_idlis (cdr bs))))))

(define _letrec            ; assumes a well-formed letrec construction
  (lambda (es r)
    (let ([bs (car es)]
          [e (cadr es)])
      (if (null? bs)
          (_eval e r)
          (let* ([r (_extend_env (_let_idlis bs) '() r)]
                 [vs (_let_evlis bs r)])
            (begin
              (set-cdr! (car r) vs)
              (_eval e r)))))))

(define _let*                   ; assumes a well-formed let* construction
    (lambda (es r)
      (_let*_evlis (car es) (cadr es) r)))

(define _let*_evlis
  (lambda (bs e r)
    (if (null? bs)
	(_eval e r)
	(let ([b (car bs)])
	  (_let*_evlis (cdr bs)
		       e
		       (_extend_env (list (car b))
				    (list (_eval (cadr b) r))
				    r))))))

(define _time
  (lambda (es r)
    (time (_eval (car es) r))))

(define _error
  (lambda (e msg es)
    (display (format "Error in ~a: ~a\n" e (format msg es))))) 
    

;;; ----- the predefined procedures ------------------------------------------

(define _read
  (lambda (es r)
    (cond
      [(null? es)
       (read)]
      [(null? (cdr es))
       (read (_eval (car es) r))]
      [else
       (_error '_read "arity mismatch: ~s" es)])))

(define _load
  (lambda (es r)
    (let ([file (_eval (car es) r)])
      (letrec ([loop
		(lambda (port)
		  (let ([e (read port)])
		    (if (eof-object? e)
			(begin
			  (close-input-port port)
			  file)
			(begin
			  (_eval e r)
			  (loop port)))))])
	(loop (open-input-file file))))))

(define _load-verbose
  (lambda (es r)
    (let ([file (_eval (car es) r)])
      (letrec ([loop
		(lambda (port)
		  (let ([e (read port)])
		    (if (eof-object? e)
			(begin
			  (newline)
			  (close-input-port port)
			  file)
			(let ([v (_eval e r)])
			  (begin
			    (display v)
			    (display " ")
			    (loop port))))))])
	(loop (open-input-file file))))))

(define _fac
  (lambda (n)
    (if (= n 0)
	1
	(* n (_fac (- n 1))))))

(define _fib
  (lambda (n)
    (if (<= n 1)
	n
	(+ (_fib (- n 1)) (_fib (- n 2))))))

; ----- the initial environment -----------------------------------------------

(define table-common-identifiers
      '(car cdr
        caar cadr
        cdar cddr
        caddr cdddr
        list-tail
        last-pair
        null? pair?
        number? string? symbol?
        zero? add1 sub1
        + - *
        cons equal?
        = boolean?
        negative? positive?
        procedure?
        quote
        lambda
        if
        cond
        define
        set!
        case
        and or
        list
        set-car! set-cdr!
        begin
        display print
        pretty-print newline
        not length
        load load-verbose
	read
        open-input-file eof-object?
        close-input-port
        let letrec
        let*
	member
	time
	error
	fac
	fib
        ))

(define table-common-values
  (list (list 'subr 1 car) (list 'subr 1 cdr)
        (list 'subr 1 caar) (list 'subr 1 cadr)
        (list 'subr 1 cdar) (list 'subr 1 cddr)
        (list 'subr 1 caddr) (list 'subr 1 cdddr)
        (list 'subr 2 list-tail)
        (list 'subr 1 last-pair)
        (list 'subr 1 null?) (list 'subr 1 pair?)
        (list 'subr 1 number?) (list 'subr 1 string?) (list 'subr 1 symbol?)
        (list 'subr 1 zero?) (list 'subr 1 add1) (list 'subr 1 sub1)
        (list 'subr 2 +) (list 'subr 2 -) (list 'subr 2 *)
        (list 'subr 2 cons) (list 'subr 2 equal?)
        (list 'subr 2 =) (list 'subr 1 boolean?)
        (list 'subr 1 negative?) (list 'subr 1 positive?)
        (list 'subr 1 _applicable?)
        (list 'fsubr 1 _quote)
        (list 'fsubr 2 _lambda)
        (list 'fsubr 3 _if)
        (list 'fsubr 0 _cond)
        (list 'fsubr 2 _define)
        (list 'fsubr 2 _set!)
        (list 'fsubr 0 _case)
        (list 'fsubr 0 _and) (list 'fsubr 0 _or)
        (list 'fsubr 0 _evlis)
        (list 'subr 2 set-car!) (list 'subr 2 set-cdr!)
        (list 'fsubr 0 _begin)
        (list 'subr 1 display) (list 'subr 1 pretty-print)
        (list 'subr 1 pretty-print) (list 'subr 0 newline)
        (list 'subr 1 not) (list 'subr 1 length)
        (list 'fsubr 1 _load) (list 'fsubr 1 _load-verbose)
	(list 'fsubr 0 _read)
        (list 'subr 1 open-input-file) (list 'subr 1 eof-object?)
        (list 'subr 1 close-input-port)
        (list 'fsubr 2 _let) (list 'fsubr 2 _letrec)
        (list 'fsubr 2 _let*)
	(list 'subr 2 member)
        (list 'fsubr 1 _time)
	(list 'subr 3 _error)
	(list 'subr 1 _fac)
	(list 'subr 1 _fib)
	))

;;; ----- the syntax checker--------------------------------------------------

;;; circularities in the source syntax are not checked,
;;; only well-formedness

(define check-syntax
  (lambda (e)
    (letrec ([check-expression
	      (lambda (e)
		(cond
		  [(constant? e)
		   #t]
		  [(identifier? e)
		   #t]
		  [(pair? e)
		   (check-application (car e) (cdr e))]
		  [else
		   #f]))]
	     [check-application
	      (lambda (e es)
		(case e
		  [(quote)
		   (and (pair? es)
			(null? (cdr es)))]
		  [lambda
		   (and (pair? es)
			(let ([xs (car es)]
			      [es (cdr es)])
			  (and (check-formals xs)
			       (pair? es)
			       (null? (cdr es))
			       (check-expression (car es)))))]
                  ;;; incomplete
		  [else
		   #t]))]
	     [check-formals
	      (lambda (xs)
		(or (null? xs)
		    (and (pair? xs)
			 (identifier? (car xs))
			 (check-formals (cdr xs)))))])
      (check-expression e))))      

;;; ----- the read-eval-print toplevel loop ----------------------------------

(define _banner
  "Petite Chez Scheme Version 7.0 (self-interpreter).\nmodified June 2008 (26/06-2008)")

(define self-interpreter
  (lambda (prompt)
    (let ([initial-environment (list (cons '() '()))])
      (letrec ([read-eval-print-loop
		(lambda ()
		  (begin
		    (display prompt)
		    (let ([e (read)])
		      (if (eof-object? e)
			  (begin
			    (newline)
			    "So long.")
			  (begin
			    (pretty-print
			      (_eval e initial-environment))
			    (read-eval-print-loop))))))])
	(begin
	  (display _banner)
	  (newline)
	  (read-eval-print-loop))))))

;;; ----- end of "self-interpreter.scm" --------------------------------------

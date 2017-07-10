(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; expressions 表达式的格式，即对表达式格式的封装。
(define (self-evaluating? exp)
  (cond ([number? exp] true)
        ([string? exp] true)
        (else false))))

(define (variable? exp) (symbol? exp))

;; quote表达式 - 具有(quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;; assignment表达式 - 具有(set! <var> <val>)格式
(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; define表达式 - 具有(define <var> <value>) 和
;; (define (<var> <parameter_1> ... <parameter_n>) <body>) 两种形式
;; 第二种define表达式形式可以转化为 (define <var> (lambda (<parameter_1> ... <parameter_n>) <body>))
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)                        ;第一种形式
      (caadr exp)))                     ;第二种形式

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)                       ;第一种形式
      (make-lambda                      ;第二种形式
       (cdadr exp)                      ;formal parameters
       (cddr exp))))                    ;body


;; lambda表达式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

;; if表达式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (caddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ([null? seq] seq)
        ([last-exp? seq] (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;; procedure
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


;; cond 表达式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond->clauses exp)))

(define (cond->clauses clauses)
  (if (null? clauses)
      'false
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELASE clause isn't last -- COND->IF" clauses))
            (make-if (cond->predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; and or
(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))

(define (and-actions exp) (cdr exp))

(define (or-actions exp) (cdr exp))


;; chapter-4 The Metacircular Evaluator
(import "expressions.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 基础表达式
;; 自求值表达式 - 数字等
;; 标识符 - 在环境指针中查找对应的值

;; 特殊表达式
;; 索引表达式 - (quote xxx) 或者 'xxx
;; 赋值表达式 - define 和 set!
;; 条件表达式 - if 和 cond
;; lambda表达式 - 将lambda的参数和代码块结合在一起，构造成复合代码块
;; begin表达式
(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env
    )
  'ok)
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ([last-exp? exps] (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-and seq env)
  (cond ([last-exp? seq] (eval (first-exp seq)))
        ([eval (first-exp seq)] (eval-and (rest-exps seq) env))
        (else false)))

(define (eval-or seq env)
  (cond ([last-exp? seq] (eval (first-exp seq) env))
        ([eval (first-exp seq) env] true)
        (else (eval-or (rest-exps seq) env))))
;; 复合表达式 - 函数调用
;; 函数调用，eval需要迭代的对操作符和操作对象进行求值，并将操作符和操作对象都送到 apply 中调用


;; 函数参数
;; list-of-values 对函数的参数进行求值，返回最终结果
;; 该函数会影响函数调用时，参数的求值顺序

;; 求值顺序与scheme内置的cons一致
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 从左至右的求值顺序
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ([first-value (eval (first-operand exps) env)])
;;         (cons first-value (list-of-values (rest-operands exps) env)))))

;; 从右至左的求值顺序
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ([rest-values (list-of-values (rest-operands exps) env)])
;;         (cons (eval (first-operand exps) env)
;;               rest-values))))


;; eval - 输入表达式参数和当前环境指针。通过分析语法类型进行分析
(define (eval exp env)
  (cond ([self-evaluating? exp] exp)
        ([variable? exp] (lookup-variable-value exp env))
        ([quoted? exp] (text-of-quotation exp))
        ([assignment? exp] (eval-assignment exp env))
        ([definition? exp] (eval-definition exp env))
        ([if? exp] (eval-if exp env))
        ([lambda? exp]
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ([begin? exp] (eval-sequence (begin-actions exp) env))
        ([cond? exp]  (eval (cond->if exp) env))
        ([and? exp]   (eval-and (and-actions exp) env))
        ([or? exp]    (eval-or (or-actions exp) env))
        ([application? exp]
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


;; Apply - 接受操作符和操作对象，并且根据操作符的类型运行对应的操作，得到运行结果
;; apply-primitive-procedure - 执行基础运算
;; compound-primitive-procedure - 执行复合的运算，通过依次执行复合代码；并且需要进行环境扩展，在新的环境下执行对应复合代码
(define (apply procedure arguments)
  (cond ([primitive-procedure? procedure]
         (apply-primitive-procedure procedure arguments))
        ([compound-procedure? procedure]
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))


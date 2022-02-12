;; Adapted from obj.scm version 4.0 - https://github.com/theurere/berkeley_cs61a_spring-2011_archive/blob/master/code/src/ucb/bscheme/obj.scm
#lang racket

;; ASK: send a message to an object
(define (ask object message . args)
  (let ((method (object message)))
    (if (method? method)
	(apply method args)
	(error "No method " message " in class " (cadr method)))))

;; INSTANTIATE and INSTANTIATE-PARENT: Create a new instance of a class
;; The difference is that only INSTANTIATE initializes the new object 
(define (instantiate class . arguments)
  (let ((new-instance (apply (class 'instantiate) arguments)))
    (ask new-instance 'initialize new-instance)
    new-instance))

(define (instantiate-parent class . arguments)
  (apply (class 'instantiate) arguments))

(define (no-method name)
  (list 'no-method name))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      #f))

(define (method? x)
  (not (no-method? x)))

(define (get-method give-up-name message . objects)
  (if (null? objects)
      (no-method give-up-name)
      (let ((method ((car objects) message)))
	(if (method? method)
	    method
	    (apply get-method (cons give-up-name
				    (cons message (cdr objects)) ))))))

;; DEFINE-CLASS: Create a new class.
;; The macro expands out to the following structure:
;; (begin
;;   (define class-name ....)
;;   (define class-name-DEFINITION ....)
;;   'class-name)
(define-syntax (define-class stx)
  (let ((definition (translate (cdr (syntax->datum stx))))
        (class-name (class-name (cdr (syntax->datum stx)))))  
    (let ((output
           ;; Include the definition of the class itself, and the code
           (list 'begin
                  definition
                  `(define ,(maknam class-name '-definition) ',definition)
                  (list 'quote class-name))))
      (datum->syntax stx output))))

;; SHOW-CLASS: Print the definition of a class, previously defined by define-class
(define-syntax (show-class stx)
  (datum->syntax stx (maknam (cadr (syntax->datum stx)) '-definition)))

;; A bunch of utility functions for define-class
;; These have to be defined inside begin-for-syntax, as we need them available at syntax
;; expansion level.
;; This is copied from obj.scm
(begin-for-syntax
  (define (maknam . symbols)
    (string->symbol (apply string-append (map symbol->string symbols))))

  (define (translate form)
    (apply-usual-translate (translate-without-usual form)))
  
  ;; (usual ARGS) inside a define-class definition means "send a message to my parent"
  ;; I could have defined usual as syntax, with the following:
  ;;
  ;; (define-syntax (usual stx)
  ;;   (datum->syntax stx (append '(ask dispatch 'send-usual-to-parent) (cdr (syntax->datum stx)))))
  ;;
  ;; This works, but Racket parses syntax "outside in", so the call to usual wouldn't be expanded
  ;; inside the definition of the class. Instead, I've parsed it by hand here, so
  ;; (show-class CLASS-NAME)
  ;; will show the call to usual expanded.
  (define (apply-usual-translate class-definition)
    (cond ((not (list? class-definition))
           class-definition)
          ((null? class-definition) class-definition)
          ((equal? (car class-definition) 'usual)
           (append '(ask self 'send-usual-to-parent) (cdr class-definition)))
          (else
           (map apply-usual-translate class-definition))))
  
  (define (translate-without-usual form)
    (cond ((null? form) (error "Define-class: empty body"))
          ((not (null? (obj-filter form (lambda (x) (not (pair? x))))))
           (error "Each argument to define-class must be a list"))
          ((not (null? (extra-clauses form)))
           (error "Unrecognized clause in define-class:" (extra-clauses form)))
          (else 
           `(define ,(class-name form)
              (let ,(class-var-bindings form)
                (lambda (class-message)
                  (cond
                    ,@(class-variable-methods form)
                    ((eq? class-message 'instantiate)
                     (lambda ,(instantiation-vars form)
                       (let ((self '())
                             ,@(parent-let-list form)
                             ,@(instance-vars-let-list form))
                         (define (dispatch message)
                           (cond
                             ,(init-clause form)
                             ,(usual-clause form)
                             ,@(method-clauses form)
                             ,@(local-variable-methods form)
                             ,(else-clause form) ))
                         dispatch )))
                    (else (error "Bad message to class" class-message)))))))))

  (define *legal-clauses*
    '(instance-vars class-vars method default-method parent initialize))
  
  (define (extra-clauses form)
    (obj-filter (cdr form)
                (lambda (x) (null? (member (car x) *legal-clauses*)))))

  (define class-name caar)

  (define (class-var-bindings form)
    (let ((classvar-clause (find-a-clause 'class-vars form)))
      (if (null? classvar-clause)
          '()
          (cdr classvar-clause) )))

  (define instantiation-vars cdar)

  (define (parent-let-list form)
    (let ((parent-clause (find-a-clause 'parent form)))
      (if (null? parent-clause)
          '()
          (map (lambda (parent-and-args)
                 (list (maknam 'my- (car parent-and-args))
                       (cons 'instantiate-parent parent-and-args)))
               (cdr parent-clause)))))

  (define (instance-vars-let-list form)
    (let ((instance-vars-clause (find-a-clause 'instance-vars form)))
      (if (null? instance-vars-clause)
          '()
          (cdr instance-vars-clause))))

  (define (init-clause form)
    (define (parent-initialization form)
      (let ((parent-clause (find-a-clause 'parent form)))
        (if (null? parent-clause)
            '()
            (map
             (lambda (parent-and-args)
               `(ask ,(maknam 'my- (car parent-and-args)) 'initialize self) )
             (cdr parent-clause) ))))
    (define (my-initialization form)
      (let ((init-clause (find-a-clause 'initialize form)))
        (if (null? init-clause) '()
            (cdr init-clause))))
    (define (init-body form)
      (append (parent-initialization form)
              (my-initialization form) ))

    `((eq? message 'initialize)
      (lambda (value-for-self)
        (set! self value-for-self)
        ,@(init-body form) )))

  (define (variable-list var-type form)
    (let ((clause (find-a-clause var-type form)))
      (if (null? clause)
          '()
          (map car (cdr clause)) )))

  (define (class-variable-methods form)
    (cons `((eq? class-message 'class-name) (lambda () ',(class-name form)))
          (map (lambda (variable)
                 `((eq? class-message ',variable) (lambda () ,variable)))
               (variable-list 'class-vars form))))

  (define (local-variable-methods form)
    (cons `((eq? message 'class-name) (lambda () ',(class-name form)))
          (map (lambda (variable)
                 `((eq? message ',variable) (lambda () ,variable)))
               (append (cdr (car form))
                       (variable-list 'instance-vars form)
                       (variable-list 'class-vars form)))))

  (define (method-clauses form)
    (map
     (lambda (method-defn)
       (let ((this-message (car (cadr method-defn)))
             (args (cdr (cadr method-defn)))
             (body (cddr method-defn)))
         `((eq? message ',this-message)
           (lambda ,args ,@body))))
     (obj-filter (cdr form) (lambda (x) (eq? (car x) 'method))) ))

  (define (parent-list form)
    (let ((parent-clause (find-a-clause 'parent form)))
      (if (null? parent-clause)
          '()
          (map (lambda (class) (maknam 'my- class))
               (map car (cdr parent-clause))))))

  (define (usual-clause form)
    (let ((parent-clause (find-a-clause 'parent form)))
      (if (null? parent-clause)
          `((eq? message 'send-usual-to-parent)
            (error "Can't use USUAL without a parent." ',(class-name form)))
          `((eq? message 'send-usual-to-parent)
            (lambda (message . args)
              (let ((method (get-method ',(class-name form)
                                        message
                                        ,@(parent-list form))))
                (if (method? method)
                    (apply method args)
                    (error "No USUAL method" message ',(class-name form)) )))))))

  (define (else-clause form)
    (let ((parent-clause (find-a-clause 'parent form))
          (default-method (find-a-clause 'default-method form)))
      (cond
        ((and (null? parent-clause) (null? default-method))
         `(else (no-method ',(class-name form))))
        ((null? parent-clause)
         `(else (lambda args ,@(cdr default-method))))
        ((null? default-method)
         `(else (get-method ',(class-name form) message ,@(parent-list form))) )
        (else
         `(else (let ((method (get-method ',(class-name form)
                                          message
                                          ,@(parent-list form))))
                  (if (method? method)
                      method
                      (lambda args ,@(cdr default-method)) )))))))

  (define (find-a-clause clause-name form)
    (let ((clauses (obj-filter (cdr form)
                               (lambda (x) (eq? (car x) clause-name)))))
      (cond ((null? clauses) '())
            ((null? (cdr clauses)) (car clauses))
            (else (error "Error in define-class: too many "
                         clause-name "clauses.")) )))

  (define (obj-filter l pred)
    (cond ((null? l) '())
          ((pred (car l))
           (cons (car l) (obj-filter (cdr l) pred)))
          (else (obj-filter (cdr l) pred)))))

;; Exports
(provide ask instantiate define-class show-class)


(provide instantiate-parent no-method method? get-method)
#lang racket
(require "obj.rkt")

(define-class (account balance)
  (method (deposit amount)
          (set! balance (+ amount balance))
          balance)
  (method (withdraw amount)
          (if (< balance amount)
              "Insufficient funds"
              (begin
                (set! balance (- balance amount))
                balance))))

(define-class (checking-account balance)
  (parent (account balance))
  (instance-vars (check-fee 0.01))
  ; A method that overrides the parent method, and uses usual to call back to it
  (method (withdraw amount)
          (begin
            (display "withdrawing!")
            (newline)
            (usual 'withdraw amount)))
  ; A method that isn't present in the parent, that uses an instance variable
  (method (write-check account)
          (ask self 'withdraw (+ account check-fee)))
  ; Demonstrating that we can set the instance variable
  (method (set-fee! value)
          (set! check-fee value)))
(show-class checking-account)

(define My-account (instantiate checking-account 100))
(ask My-account 'deposit 10)
(ask My-account 'write-check 50)
(ask My-account 'set-fee! 0.5)
(ask My-account 'write-check 10)
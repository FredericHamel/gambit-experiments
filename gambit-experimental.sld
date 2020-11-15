;; Author: Frédéric Hamel
;; Date: Sun Nov 15 09:59:25 AM EST 2020
;; Description: Simple implementation of the erlang pipe operator in Gambit

(define-library (gambit-expiremental)

  (namespace "gambit-experimental#")

  (export pipe)
  (import gambit)

  (begin
    (define-macro (pipe  conn1 op1 . ops)
       (define (prepend-args arg op)
         `(,(car op) ,arg ,@(cdr op)))

       (let ((conn (gensym)))
        `(let ((,conn ,(prepend-args conn1 op1)))
           ,(if (null? ops)
                conn
               `(gambit-experimental#pipe ,conn ,@ops)))))))


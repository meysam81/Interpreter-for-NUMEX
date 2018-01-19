;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones => added with a slight bit of care

(struct var  (string)    #:transparent)  ;; variable
(struct mult  (e1 e2)    #:transparent)  ;; multiplication
(struct neg  (e)    #:transparent)  ;;  negation
(struct islthan  (e1 e2)    #:transparent)  ;;  comparison
(struct ifzero  (e1 e2 e3)    #:transparent)  ;;  
(struct ifgthan  (e1 e2 e3 e4)    #:transparent)  ;;  
(struct mlet  (s e1 e2)    #:transparent)  ;;  
(struct apair  (e1 e2)    #:transparent)  ;;  pair constructor
(struct first  (e)    #:transparent)  ;;
(struct second  (e)    #:transparent)  ;;  


(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions


(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1 => solved proudly :D

(define (racketlist->numexlist xs) (cond
                                     ((null? xs) (munit))
                                     (#t (apair
                                          (car xs)
                                          (racketlist->numexlist (cdr xs))))))
(define (numexlist->racketlist xs) (cond
                                     ((munit? xs) '())
                                     (#t (cons
                                          (apair-e1 xs) (numexlist->racketlist (apair-e2 xs))))))

;; Problem 2 => I think I got it => I'm sure I got it!!! pooooooof!!!

;; lookup a variable in an environment
;; Complete this function => done beautifully, as I myself enjoyed
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? str (car (car env))) (cdr (car env))]
        [#t (envlookup (cdr env) str)]
        ))
; =============== adding my own append list ====================
(define (append-args x y)
  (if (null? x)
      (if (null? y) null (cons (car y) (append-args x (cdr y))))
      (cons (car x) (append-args (cdr x) y))))
; ==============================================================
;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
    [(int? e)
     (let
         [(v (int-num e))]
       (cond
         ((integer? v) e) ;; terminal i.e. base case of evel-under-env
         (#t (error "NUMEX int applied to non-number"))))]
    
    ;; add closure here if you understand what it means
    ;; trying to understand what it means
    [(closure? e)
     (let
         [(v1 (closure-fun e))
          (v2 (closure-env e))]
      (if (fun? v1) (if (list? env) e
                       (error "NUMEX closure's env not a racket-list"))  ; return the closure itself
         (#t (error "NUMEX closure applied to non-function"))))]

    ; functions => I think I got it!!!
    [(fun? e)
     (let [(v1 {fun-formal e})]
       (if (string? v1) ; a variable?
           (closure env e) ; then
           (error "NUMEX function argument not a string"); else
           ))]
    [(call? e)
     (let {[v1 (eval-under-env (call-funexp e) env)]
           [v2 (eval-under-env (call-actual e) env)]}
       (if (closure? v1)
           {if (int? v2)
               ; to be honest, I don't even know what I did below,
               ; I just did and bang, magic happens
               (eval-under-env (fun-body (closure-fun v1)) ; give the closure to it recursively ...
                               ; ... with the closure's own invironment
                               (append-args (list (cons (fun-nameopt (call-funexp e)) v1))
                               (append-args (list (cons (fun-formal (call-funexp e)) v2))
                                            (closure-env v1)))
                               )
               (error "NUMEX call-actual not an int")}
           (error "NUMEX call-funexp not a closure")))
     ]
    [(mlet? e)
     (let [{v1 (eval-under-env (mlet-e1 e) env)}]
       (eval-under-env (mlet-e2 e) (append-args (list (cons (mlet-s e) v1)) env)))]
    
    [(var? e) ; A variable evaluates to the value associated with it in the given environment
     (envlookup env (var-string e))]
    
    [(add? e) 
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1) 
                   (int-num v2)))
           (error "NUMEX add applied to non-number")))]
    [(mult? e)
     (let ([v1 (eval-under-env (mult-e1 e) env)]
           [v2 (eval-under-env (mult-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (* (int-num v1) 
                   (int-num v2)))
           (error "NUMEX mult applied to non-number")))]
    [(neg? e) 
     (let {[tmp (eval-under-env (neg-e e) env)]}
       (cond
         [(int? tmp) (int (- (int-num tmp)))]
         [#t (error "NUMEX neg applied to non-number")]))]
    
    [(islthan? e)
     (let ({v1 [eval-under-env (islthan-e1 e) env]}
           {v2 [eval-under-env (islthan-e2 e) env]})
       (cond
         ([and (int? v1) (int? v2)]
          [cond
            ((< (int-num v1) (int-num v2)) (int 1))
            (#t (int 0))])
         (#t (error "NUMEX islthan applied to non-number"))))]
    [(ifzero? e)
     (let [{v1 (eval-under-env (ifzero-e1 e) env)}]
       (cond
         {(int? v1) ;; Error handling - Type THREE ;;
          (cond 
            [(zero? (int-num v1)) (eval-under-env (ifzero-e2 e) env)] ;; IS zero
            [#t (eval-under-env (ifzero-e3 e) env)] ;; is NOT zero
            )}
         {#t (error "NUMEX iszero applied to non-number")}))]
    
    [(ifgthan? e)
     (let {[v1 (eval-under-env (ifgthan-e1 e) env)]
           [v2 (eval-under-env (ifgthan-e2 e) env)]}
       ;; body of let
       (cond
         ((and (int? v1) (int? v2))
          {cond
            [(> (int-num v1) (int-num v2)) (eval-under-env (ifgthan-e3 e) env)] ;; e1 > e2
            [#t (eval-under-env (ifgthan-e4 e) env)] ;; otherwise
            })
         {#t (error "NUMEX isgthan applied to non-number")})
       )]
    [(apair? e)
     (let ([v1 (eval-under-env (apair-e1 e) env)]
           [v2 (eval-under-env (apair-e2 e) env)])
       ; body
       {cond
         ((and (int? v1) (int? v2)) (apair v1 v2))
         {#t (error "NUMEX apair applied to non-number")}})]
    [(first? e)
     (let [(v {first-e e})]
       ; body
       (cond
         ((apair? v) (apair-e1 v))
         {#t (error "NUMEX first applied to non-number")}))]
    
    [(second? e)
     (let [(v (second-e e) )]
       ; body
       (cond
         ((apair? v) (apair-e2 v))
         {#t (error "NUMEX second applied to non-number")}))]
    [(ismunit? e)
     (let [(v (ismunit-e e))]
       ; body
       (cond
         ((munit? v) (int 1))
         {#t (int 0)}))]
    
    ;; CHANGE add more cases here => of course
    [#t (error (format "bad NUMEX expression: ~v" e))])) ;; Error handling - Type ONE ;;

;; Do NOT change => of course
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (mlet* bs e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-map "CHANGE")

(define numex-mapAddN
  (mlet "map" numex-map
        "CHANGE (notice map is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

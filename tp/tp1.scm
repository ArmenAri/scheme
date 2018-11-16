;;square function
(define carre (lambda(x)
                (* x x)))
(carre 2)
(carre 4)

;;quad function
(define quad(lambda(x)
              (let ( (y (carre x)))
                (* y y))))

(quad 5)

;;constant definition
(define pi 3.14159)

;;circle surface and permieter
(define cercle(lambda(r)
                 (list (* 2 pi r)
                       (* pi (carre r)))))

(cercle 4)

;;list definition
(define LA
'(1 (2 (6)
(7))
(3 (8 (12))
(9))
(4 (10 (13)
(14 (17)))
(11 (15 (16)))
(18))
(5)))

;;head
(car LA)

;;tail
(cdr LA)

;;ss-a de r 1
(cadr LA)

;;s-a de r 3
(caddr LA)

;;s-a de r 4
(cadddr LA)

;;leaf n°12
(caadr (cadr (caddr LA)))

;;s-a d r 10
(cddr (cadr (cadddr LA)))

;;tests
(list (cadr LA) (caddr LA))
(cadr (cadddr LA))

;;n!
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact(- n 1))))))

(define fac
;; calcul la fact d'un nombre
  ;; erreur sinon
  (lambda (n)
    (if (and (integer? n)
             (>= n 0))
        (fact n)
        'erreur)))


;;n!
(define factorielle
;;n est ici un entier positif
  (lambda (n)
        (if (zero? n)
             1
             (* n (fact(- n 1))))
        'erreur))

;; reccursive sum of first n integers
(define s_n_int
  (lambda (n)
    (if (= 1 n)
        1
        (+ n (s_n_int(- n 1))))))

(s_n_int 10)
(s_n_int 325)

;;sum integers until n
(define s_n_int2
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

;;get list length
(define longl
  (lambda (L)
    (if (null? L)
        0
        (+ 1 (longl (cdr L))))))

(longl LA)
(longl '(1 2 (f g 6) k0))
(longl '(1 2 f g 6 k0))

;;reverse a list
(define reverseL
  (lambda (L)
    (if (null? L)
        ()
        (append (reverseL (cdr L)) (list (car L))))))


;;mirroir
(define miroir
  (lambda (L)
    (if (null? L)
        '()
        (append (miroir (cdr  L)) (if (list? (car L))
                                      (list (miroir (car L)))
                                      (list (car L)))))))


;;ajouter en fin de liste sans append
(define (ajouter_fin x L)
  (if (null? L)
    (list x)
    (cons (car L) (ajouter_fin x (cdr L)))
  )
)
(miroir '(1 ((2 3) 4) 3))
(miroir '(1 (2 3) 3))

(define carreL
  (lambda (L)
    (if (null? L)
        '()
        (append (list (carre (car L))) (carreL (cdr L))))))

(carreL '(1 2 3))

(define nbpos
  (lambda (L)
    (if (null? L)
        0
        (if (>= (car L) 0)
            (+ 1 (nbpos (cdr L)))
            (nbpos (cdr L))))))

(nbpos '(1 -2 3))

(define membre
  (lambda (x L)
    (if (null? L)
        'erreur'
        (if (= (car L) x)
            #t
            (membre x (cdr L))))))

(membre 9 '(1 -2 3))


;;epure
(define epure
  (lambda (L)
    (if (null? L)
        '()
        (if (! (membre (car L) (cdr L)))
	      (cons (list (car L)) (epure (cdr L)))
        (epure (cdr L))))))

;;epure correction
(define epurec
  (lambda (L)
    (if (null? L)
      ()
      (let ((res (epure (cdr L))))
        (if (membre (car L) res)
        res
        (cons (car L) res))))))

;;nieme
(define nieme
  (lambda (n L)
    (if (= 0 n)
	  (car L)
	  (nieme (- n 1) (cdr L)))))

;;insere
(define insere
  (lambda (n x L)
    (if (null? L)
	  '()
	  (if (< 0 n)
	    (cons (car L) (insere (- n 1) x (cdr L)))
	    (cons (list x) L)))))

;;union
(define union
  (lambda (L1 L2)
    (epurec (cons L1 L2))))

;;inter
(define inter
  (lambda (L1 L2)
    (if (null? L1)
	  '()
	  (if (membre (car L1) L2)
	    (cons (car L1) (inter (cdr L1) L2))
	    (inter (cdr L1) L2)))))

(miroir '(1 2 3))

(define epureter
  (lambda (L Lres)
    (if (null? L)
      Lres
      (if (membre (car L) Lres)
        (epureter (cdr L) Lres)
        (epureter (cdr L) (cons (car L) Lres))
      )
    )
  )  
)

;;creer une liste avec les couples de L1 et L2
(define zip
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
    ()
    (cons (list (car L1) (car L2))
      (zip (cdr L1) (cdr L2))))))


(define map (lambda (f L)
  (if (null? L)
    ()
    (cons (f (car L)) (map f (cdr L)))
  )
))
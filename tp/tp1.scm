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


(define mapkar (lambda (f L)
  (if (null? L)
    ()
    (cons (f (car L)) (map f (cdr L)))
  )
))

(define L1 '(1 2 3 4))
(define L2 '(5 6 7 5))
(define L3 '("armen" "loxol" "alex" "mathieu"))

;;produitsc : 
;; - (somme de i = 0 jusqu'a n) de xi * yi = (PS * y)

(define produitsc (lambda (L1 L2)
    (if (null? L1)
        0
        (+ (* (car L1)
              (car L2))
           (produitsc (cdr L1)
                      (cdr L2))))))

;; Les Tris
;; - tri insertion : inserer un objet dans une liste triée "à la bonne place"
;; une collection vide est triée
;; une collection d'un objet est triée

(define ins (lambda(x L)
  ;; si L vide -> (x)
  ;; sinon si L = [e | L'] alors si e < x alors [e | (ins x L')]
  ;;                             sinon (cons x L)
  (if (null? L)
    (list x)
    (if (< (car L) x)
      (cons (car L)
        (ins x (cdr L))
      )
      (cons x L)
    )
  )
))

(define triins(lambda (L)
  ;; si L vide -> ()
  ;; sinon inserer e (triins L')
  (if (null? L)
    ()
    (ins (car L) (triins (cdr L)))
  )
))

;; - tri par selection
;; 1 - séléctionner le plus petit
;; 2 - inserer cet objet en tête de "la collection privée de cet objet" triée

(define min (lambda (x L)
  (if (null? L)
    x
    (if (< x (car L))
      (min x (cdr L))
      (min (car L) (cdr l))
    )
  )
))

(define minimum (lambda (L)
  (if (null? L)
    "erreur"
    (min (car L) (cdr L))
  )
))

(define retirerelt (lambda (x L)
  (if (= x (car L))
    (cdr L)
    (cons (car L) (retirerelt x (cdr L)))
  )
))

;;+ 3
(map (lambda(x) (+ 3 x)) L1)

;; integer
(map (lambda(x) (if (integer? x) #t #f)) L1)
(map integer? L1)

;; compare string
(string<? "armen" "loxol")



(define ins_par_type (lambda(x L type)
  ;; si L vide -> (x)
  ;; sinon si L = [e | L'] alors si e < x alors [e | (ins x L')]
  ;;                             sinon (cons x L)
  (if (null? L)
    (list x)
    (if (type (car L) x)
      (cons (car L)
        (ins_par_type x (cdr L) type)
      )
      (cons x L)
    )
  )
))

(define triins_par_type(lambda (L type)
  ;; si L vide -> ()
  ;; sinon inserer e (triins L')
  (if (null? L)
    ()
    ( ins_par_type (car L) (triins_par_type (cdr L) type) type)
  )
))

(triins_par_type L3 string<?)

(define (comp_personne_taille<? t1 t2)
  (< (caddr t1) (caddr t2))
)

(define (comp_personne_taille>? t1 t2)
  (> (caddr t1) (caddr t2))
)

(define (comp_personne_age<? a1 a2)
  (< (cadr a1) (cadr a2))
)

(define (comp_personne_age>? a1 a2)
  (> (cadr a1) (cadr a2))
)

(define (comp_personne_nom<? n1 n2)
  (string<? (car n1) (car n2))
)

(define (comp_personne_nom>? n1 n2)
  (string>? (car n1) (car n2))
)

(define L4 '( ("armen" 36 160) ("alex" 43 100) ("loxol" 12 150) ))

(triins_par_type L4 comp_personne_age<?)

(define h 0.0001)

(define (diff f)
  (lambda (x) ( / (- (f (+ x h)) (f (- x h))) (* 2 h)))
)

(define (fog f g)
  (lambda (x) 
    (f (g x))  
  )
)

(define mapkan
  (lambda (f L)
    (if (null? L)
      ()
      (append (f (car L)) (mapkan f (cdr L)))
    )
  )  
)

(define (remove_not_int x)
    (if (integer? x)
    (list x)
    ()
    )
)

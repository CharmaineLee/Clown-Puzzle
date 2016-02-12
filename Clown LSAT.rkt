;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Clown LSAT|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Clown is String
;; interp.  The name of a clown
(define M "Marny")
(define N "Nifty")
(define O "Oscar")
(define P "Porky")
(define R "Rascal")
(define S "Sunny")
(define V "Vinny")
#;
(define (fn-for-clown c)
  (... c))


(define-struct chst (picked options))
;; ChoiceState is (make-chst (listof Clown) (listof Clown))
;; interp. (make-chst picked options) represents a moment during selection when:
;; - we have selected all of the clowns in picked
;; - we are still deciding whether to pick or skip the clowns in options
;; INVARIANT: all of the clowns in picks and options are unique.
(define CH0 (make-chst (list)       (list M N O P R S V))) ; no selections yet
(define CH1 (make-chst (list M)     (list O P R S V)))     ; M picked, N skipped
(define CH2 (make-chst (list M P V) (list)))               ; picking is complete

;; PRELAB
(define CH3 (make-chst (list)       (list V R O)))         ; picking has not began
(define CH4 (make-chst (list R V)   (list O)))             ; R & V picked, O skipped

#;
(define (fn-for-chst c)
  (... (fn-for-loc (chst-picked c))
       (fn-for-loc (chst-options c))))



;; ========================================================
;; Functions

;; ChoiceState -> (listof Clown) or false
;; produces list of clowns that satisfy the criteria based from our choice state
;;<template blended from BST & generative recursion & backtracking search>

(check-expect (solve--chst CH0) (list "Sunny" "Rascal" "Porky" "Oscar" "Marny"))
(define (solve--chst chst)
  (if (empty? (chst-options chst))
      (if (all-crits? (chst-picked chst))
          (chst-picked chst)
          false)
      (local [(define try (solve--chst (choose-first chst)))]
                     (if (not (false? try))                     
                         try                                    
                         (solve--chst (skip-first chst))))))

;; ChoiceState -> ChoiceState
;; produces the choice state with first option added to pick
(check-expect (choose-first CH0) (make-chst (list M) (list N O P R S V)))

(define (choose-first chst)
  (make-chst (cons (first (chst-options chst)) (chst-picked chst)) (rest (chst-options chst))))

;; ChoiceState -> ChoiceState
;; produces the choice state with the first pick omitted
(check-expect (skip-first CH0) (make-chst (list) (list N O P R S V)))

(define (skip-first chst)
  (make-chst (chst-picked chst) (rest (chst-options chst))))


;; (listof Clown) -> Boolean
;; produce true if loc satisfies all the clown crits, otherwise false
(check-expect (all-crits? empty) false)
(check-expect (all-crits? (list "Vinny" "Rascal" "Oscar" "Marny")) true)

(define (all-crits? loc) 
  (andmap (λ (crit?) (crit? loc))
          (list crit-0? crit-1? crit-2? crit-3? crit-4? crit-5?)))


;; (listof Clown) -> Boolean
;; check Criteria 0: There must be at least four clowns
(check-expect (crit-0? empty) false)
(check-expect (crit-0? (list V R)) false)
(check-expect (crit-0? (list V R S O)) true)
(check-expect (crit-0? (list M V R S O)) true)

(define (crit-0? loc)
  (>= (length loc) 4))



;; (listof Clown) -> Boolean
;; check Criteria 1: Exactly two of Rascal, Sunny, and Vinny are selected.
(check-expect (crit-1? empty) false)
(check-expect (crit-1? (list R S V)) false)
(check-expect (crit-1? (list V)) false)
(check-expect (crit-1? (list V R)) true)

(define (crit-1? loc)
  (or (and (member? R loc)
           (member? S loc)
           (not (member? V loc)))
      (and (member? R loc)
           (member? V loc)
           (not (member? S loc)))
      (and (member? S loc)
           (member? V loc)
           (not (member? R loc)))))



;; (listof Clown) -> Boolean
;; check Criteria 2: Either Nifty is selected, Oscar is selected, or both
(check-expect (crit-2? empty) false)
(check-expect (crit-2? (list N)) true)
(check-expect (crit-2? (list N O)) true)
(check-expect (crit-2? (list M O)) true)

(define (crit-2? loc)
  (if (not (member? N loc))
      (member? O loc)
      true))



;; (listof Clown) -> Boolean
;; check Criteria 3: Oscar cannot be selected unless Rascal is selected.
(check-expect (crit-3? empty) true)
(check-expect (crit-3? (list O)) false)
(check-expect (crit-3? (list O R)) true)

(define (crit-3? loc)
  (if (member? O loc)
      (member? R loc)
      true))


;; (listof Clown) -> Boolean
;; check Criteria 4: Porky cannot be selected unless Sunny is selected.
(check-expect (crit-4? empty) true)
(check-expect (crit-4? (list P)) false)
(check-expect (crit-4? (list P S)) true)

(define (crit-4? loc)
  (if (member? P loc)
      (member? S loc)
      true))


;; (listof Clown) -> Boolean
;; check Criteria 5: If Marny is selected, then Nifty cannot be selected.
(check-expect (crit-5? empty) true)
(check-expect (crit-5? (list M)) true)
(check-expect (crit-5? (list N)) true)
(check-expect (crit-5? (list M N)) false)

(define (crit-5? loc)
  (if (member? M loc)
      (not (member? N loc))
      true))
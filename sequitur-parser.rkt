#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(require parser-tools/cfg-parser)
(require racket/hash)
(require "aqinas-physics-lexer-2.rkt")

;; Parse stream of tokens into (Listof (number (Listof Stmt-1)))
(struct Section (number
                 stmt-list)
  #:transparent)

(struct Stmt-1 (name
                prop
                premises
                note)
  #:transparent)
               
(define parse
  (parser
   (tokens value-tokens syntax-tokens)
   (start <section-list>)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value)
            (error (if tok-value (format "Unexpected ~a \"~a\"" tok-name tok-value)
                       (format "Unexpected ~a" tok-name)))))
   (grammar
    (<section-list> ((<section>) (list $1))
                    ((<section> <section-list>) (cons $1 $2)))
    (<section> ((SECT STR <stmt-list>) (Section (string->number $2) $3)))

    (<stmt-list> ((<stmt>) (list $1))
                 ((<stmt> <stmt-list>) (cons $1 $2)))
    
    (<stmt> ((VAR GETS STR <opt-premises-list> <opt-note>) (Stmt-1 $1 $3 $4 $5)))
    
    (<opt-premises-list> ((LCUR <premises-list> RCUR) $2)
                         (() '()))
    (<premises-list> ((<var>) (list $1))
                     ((<var> <premises-list>) (cons $1 $2)))
    (<var> ((VAR) $1)
           ((STR VAR) (string-append $1 $2)))
    (<opt-note> ((LAB STR RAB) $2)
                (() #f)))))

(struct Stmt (name
              prop
              premises
              note
              section)
  #:transparent)

;; (Listof Section) -> (HashMap name Stmt)
(define (to-struct sect-list)
  (define ret (hash))
  (for/fold ([ret (hash)])
            ([section sect-list])
    (define sect-number (Section-number section))
    (hash-union ret (for/fold ([hash-tbl (hash)])
                              ([stmt-1 (Section-stmt-list section)])
                      (define name (Stmt-1-name stmt-1))
                      (define stmt (Stmt name
                                         (Stmt-1-prop stmt-1)
                                         (Stmt-1-premises stmt-1)
                                         (Stmt-1-note stmt-1)
                                         sect-number))
                      (hash-set hash-tbl name stmt)))))

(define (to-abs-struct sect-list)
  (define ret (hash))
  (for/fold ([ret (hash)])
            ([section sect-list])
    (define sect-number (Section-number section))
    (define sect-n-str (number->string sect-number))
    (hash-union ret (for/fold ([hash-tbl (hash)])
                              ([stmt-1 (Section-stmt-list section)])
                      (define abs-name (string-append sect-n-str
                                                      (Stmt-1-name stmt-1)))
                      (define stmt (Stmt abs-name
                                         (Stmt-1-prop stmt-1)
                                         (map (λ (premise)
                                                (if (equal? "$" (substring premise 0 1))
                                                    (string-append sect-n-str premise)
                                                    premise))
                                              (Stmt-1-premises stmt-1))
                                         (Stmt-1-note stmt-1)
                                         sect-number))
                      (hash-set hash-tbl abs-name stmt)))))

(define (string-prettify str)
  ; Listof String -> Listof Listof String
  (define (inner word-lst)
    (cond
      [(empty? word-lst) '()]
      [(< (length word-lst) 5) (list word-lst)]
      [else (cons (take word-lst 5)
                  (inner (drop word-lst 5)))]))
  (string-join (map (λ(wlst) (string-join wlst " "))
                    (inner (string-split str)))
               "\\n"))

;; Renders to graphviz
(define (render-graphviz stmt-map)
  (printf "Digraph G {\n")
  (for ([(name stmt) stmt-map])
    (if (Stmt-note stmt)
        (printf "~s [label = \"~a\n<note: ~a>\"]\n"
                name
                (string-prettify (Stmt-prop stmt))
                (string-prettify (Stmt-note stmt)))
        (printf "~s [label = \"~a\"]\n"
                name
                (string-prettify (Stmt-prop stmt))))
    (for ([premise (Stmt-premises stmt)])
      (printf "~s -> ~s\n" premise name)))
  (printf "}"))


;; Tests
(define str (open-input-string
             ":: Lectio 19 He Shows with Proper Proofs That Circular Motion Can be Continuous and That Circular Motion Is the First Motion ::

:section 1131
$proof2 := Proof 2: circular motion is not in the same things, but straight motion is repeatedly in the same things. {$2d}

    $2c := To be moved from the same thing to the same thing involves no opposition with respect to circular motion, but involves opposition with respect to reflex motion.  {$2a $2b}"))

(define lectio-19 (open-input-file "lectio-19-outline.txt"))
(render-graphviz(to-abs-struct (parse (lambda () (lex-once lectio-19)))))
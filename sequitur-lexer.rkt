#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(provide lex-once
         lex-all
         value-tokens
         syntax-tokens)

(define-tokens value-tokens (STR VAR))
(define-empty-tokens syntax-tokens (SECT LAB RAB LCUR RCUR GETS EOF))

(define lex-once
  (lexer
   [":section" 'SECT]
   [":=" 'GETS]
   [(:: "::"
        (complement (:: any-string "::" any-string))
        "::")
      (lex-once input-port)]
   ["<" 'LAB]
   [">" 'RAB]
   ["{" 'LCUR]
   ["}" 'RCUR]
   [(:: "$" (:*(:- any-char
                   (:+ whitespace)
                   "}" "{" "<" ">")))
    (token-VAR lexeme)]
   [(:+ whitespace) (lex-once input-port)]
   [(eof) 'EOF]
   [any-char (token-STR (string-trim (~a lexeme (str-lexer input-port))))]))

(define str-lexer
  (lexer
   [(:& (complement (:: any-string (:or "::" "$" "<" ">" "{" "}")
                        any-string))
        (complement (:: any-string ":")))
    lexeme]))

;; Tests
(define (lex-all port)
  (match (lex-once port)
    ['EOF '()]
    [something (cons something (lex-all port))]))

(define str (open-input-string
             ":: Lectio 19 He Shows with Proper Proofs That Circular Motion Can be Continuous and That Circular Motion Is the First Motion ::

:section 1131
$proof2 := Proof 2: circular motion is not in the same things, but straight motion is repeatedly in the same things. {$2d}

    $2c := To be moved from the same thing to the same thing involves no opposition with respect to circular motion, but involves opposition with respect to reflex motion.  {$2a $2b} <this is a note>"))

(define lectio-19 (open-input-file "lectio-19-outline.txt"))

(lex-all lectio-19)
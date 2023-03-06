#lang racket
(require racket/string)
(require racket/file)

;;Source Note: To get this code below i ask my instructor help, i use a lot of resources online like chatGPT, stackoverflow,https://riptutorial.com/parsing and the racket library website

;; read the input file into a list of strings
(define input-file "file02.txt")
(define input-lines (file->lines input-file))

;; split each line into a list of substrings
(define input-strings
  (for/list ([line input-lines])
    (string-split line)))

;; display the resulting list of lists of strings
(displayln input-strings)

;;Reading grammar



(define (digit-star)
  (regexp #px"[0-9]*"))


(define (program input-strings)
  (let
    [(result (linelist(first input-strings)))]
     (if (equal?(first(result)))
         ("Accept"(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )

(define (linelist input-strings)
  (let
    [(result (line (first input-strings)))]
     (if (equal?(first(result))#t)
         (linelist(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
(define (line input-strings)
  (let
    [(result (line (first input-strings)))]
     (if (idx(first(input-strings list))#t)
         (stmt(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
(define (idx input-strings)
  (let
    [(result (idx (first string->list string)))]
     (if (nonzero_digit(first(input-strings list))#t)
         (digit-star(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
(define (linetail input-strings)
  (let
    [(result (linetail (first input-strings)))]
     (if (equal?(first(input-strings list))":")
         (stmt(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
(define (stmt input-strings)
  (let
    [(result1 (id (first input-strings)))]
     (if (equal?(first(result1))#t)
         (expr(rest input-strings))
   (let
    [(result2 (expr (first input-strings)))]
     (if (equal?(first(result2))#t)
         (stmt(rest input-strings))
   (let
    [(result3 (expr (first input-strings)))]
     (if (equal?(first(result3))"read")
         (id(rest input-strings))
   (let
    [(result4 (expr (first input-strings)))]
     (if (equal?(first(result4))"write")
         (expr(rest input-strings))
   (let
    [(result5 (expr (first input-strings)))]
     (if (equal?(first(result5))"goto")
         (idx(rest input-strings))
   (let
    [(result6 (expr (first input-strings)))]
     (if (equal?(first(result6))"gosub")
         (idx(rest input-strings))
   (let
    [(result7 (num(first input-strings)))]
     (if (equal?(first(result7))#t)
         (etail(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
)
  )
     )
   )
     )
   )
     )
   )
     )
   )
    )
  )

(define (expr input-strings)
  (let
    [(result1 (id (first input-strings)))]
     (if (equal?(first(result1))#t)
         (etail(rest input-strings))
   (let
    [(result2 (num(first input-strings)))]
     (if (equal?(first(result2))#t)
         (etail(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
)
  )
(define (etail input-strings)
  (let
    [(result (expr (first string->list string)))]
     (if (equal?(first(input-strings list))"+")
         (expr(rest input-strings))
     (if (equal?(first(input-strings list))"-")
         (expr(rest input-strings))
     (if (equal?(first(input-strings list))"=")
         (expr(rest input-strings))
     ("error"(rest input-strings))
     
    )
  )
  )
  )
)
(define (chars)
  (regexp #px"[a-z]"))

(define (id input-strings)
  ((chars (string->list (first input-strings)))
    (if (andmap char-alphabetic? chars)
        (list (list 'id (list->string chars)))
     ("error"(rest input-strings)))
)
  )


(define (num input-strings)
  (let
    [(result (num (first string->list string)))]
     (if (equal?(first(input-strings list))"+")
         (digit(rest input-strings))
      (if (equal?(first(input-strings list))"-")
         (digit(rest input-strings))
     ("error"(rest input-strings))
     
  )
  )
  )
)

(define (numsign input-strings)
  (let
    [(result (numsign(first input-strings)))]
     (if (equal?(first(result))"+" )
         (numsign(rest input-strings))
     (if (equal?(first(result))"-")
         (numsign(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
  )
;Continue till 9
(define (nonzero_digit input-strings)
  (let
    [(result (nonzero_digit(first input-strings)))]
     (if (equal?(first(result))"1" )
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"2")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"3")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"4")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"5")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"6")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"7")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"8")
         (nonzero_digit(rest input-strings))
     (if (equal?(first(result))"9")
         (nonzero_digit(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
    )
  )
     )
     )
     )
     )
    )
  )
(define (digit input-strings)
  (let
    [(result (digit (first input-strings)))]
     (if (equal?(first(result))"0")
         (digit(rest input-strings))
     (if (equal?(first(result))nonzero_digit)
         (digit(rest input-strings))
     ("error"(rest input-strings))
    )
  )
  )
  )
;;Parse function
(define (parse filename)
  (with-input-from-file filename
    (lambda ()
      (let ((program (read)))
        (if (syntax? program)
            "accept"
            "syntax error")))))
#lang racket

(define filename "hello.txt")

(define (read-next-line-iter file)
	   (let ((line (read-line file)))
	     (if (eof-object? line) '()
	       (cons line (read-next-line-iter file))
               )))

(define (print-each-element lst)
  (if (null? lst) null
      ((display (car lst))
      (newline)
      (print-each-element (cdr lst)))))

(define (get-char-value ch)
  (char->integer ch))

(define chosungs #("ㄱ" "ㄲ" "ㄴ" "ㄷ" "ㄸ" "ㄹ" "ㅁ" "ㅂ" "ㅃ" "ㅅ" "ㅆ" "ㅇ" "ㅈ" "ㅉ" "ㅊ" "ㅋ" "ㅌ" "ㅍ" "ㅎ"))
(define joongsungs #("ㅏ" "ㅐ" "ㅑ" "ㅒ" "ㅓ" "ㅔ" "ㅕ" "ㅖ" "ㅗ" "ㅘ" "ㅛ" "ㅙ" "ㅚ" "ㅜ" "ㅝ" "ㅞ" "ㅟ" "ㅠ" "ㅡ" "ㅢ" "ㅣ"))
(define jongsungs #(" " "ㄱ" "ㄲ" "ㄳ" "ㄴ" "ㄵ" "ㄶ" "ㄷ" "ㄹ" "ㄺ" "ㄻ" "ㄼ" "ㄽ" "ㄾ" "ㄿ" "ㅀ" "ㅁ" "ㅂ" "ㅄ" "ㅅ" "ㅆ" "ㅇ" "ㅈ" "ㅊ" "ㅋ" "ㅌ" "ㅍ" "ㅎ"))
(define (associate-index i)
  (if (= i (vector-length jongsungs)) '()
      (cons (cons (vector-ref jongsungs i) i) (associate-index (+ i 1)))))
(define jongsung-index (make-hash (associate-index 0)))
(define lines (make-hash (list  (cons " " 0) (cons "ㄱ" 2) (cons "ㄴ" 2) (cons "ㄷ" 3) (cons "ㄹ" 5) (cons "ㅁ" 4) (cons "ㅂ" 4) (cons "ㅅ" 2) (cons "ㅈ" 3) (cons "ㅊ" 4) (cons "ㅋ" 3) (cons "ㅌ" 4) (cons "ㅍ" 4)
                                     (cons "ㄲ" 4) (cons "ㄳ" 4) (cons "ㄵ" 5) (cons "ㄶ" 5) (cons "ㄺ" 7) (cons "ㄻ" 9) (cons "ㄼ" 9) (cons "ㄽ" 7) (cons "ㄾ" 9) (cons "ㄿ" 9) (cons "ㅀ" 8)
                                     (cons "ㅄ" 6) (cons "ㅆ" 4))))


(define (extract-char ch)
  (define ch-value (get-char-value ch))
  (vector
   (vector-ref chosungs (quotient (- ch-value 44032) (* (vector-length joongsungs) (vector-length jongsungs))))
   (vector-ref joongsungs (quotient (remainder (- ch-value 44032) (* (vector-length joongsungs) (vector-length jongsungs))) (vector-length jongsungs)))
   (vector-ref jongsungs (remainder (- ch-value 44032) (vector-length jongsungs)))))


(define (run-aheui cmds pos-x pos-y prev-x prev-y direction storage-no storage)
  (unless (>= pos-y 0) (set! pos-y (+ (vector-length cmds) pos-y)))
  (unless (>= pos-x 0) (set! pos-x (+ (string-length (vector-ref cmds pos-y)) pos-x)))
  (define extracted (extract-char (string-ref (vector-ref cmds pos-y) pos-x)))
  (define cmd0 (vector-ref extracted 0))
  (define cmd1 (vector-ref extracted 1))
  (define cmd2 (vector-ref extracted 2))
  (define cur-storage (vector-ref storage storage-no))
  (define new-storage-no storage-no)
  (define decision 1)
  
  ;for debugging
;  (newline)
;  (display (string-ref (vector-ref cmds pos-y) pos-x))
;  (display pos-x)
;  (display pos-y)
;  (display storage-no)
;  (display storage)
;  (newline)
  
  ((cond ((equal? cmd0 "ㅇ") (void))
        ;exit with error code?
        ((equal? cmd0 "ㅎ") (if (= (length cur-storage) 0) (exit 0)
                               (exit (car cur-storage))))
        ((equal? cmd0 "ㄷ") (vector-set! storage storage-no (cons (+ (cadr cur-storage) (car cur-storage)) (cddr cur-storage))))
        ((equal? cmd0 "ㄸ") (vector-set! storage storage-no (cons (* (cadr cur-storage) (car cur-storage)) (cddr cur-storage))))
        ((equal? cmd0 "ㅌ") (vector-set! storage storage-no (cons (- (cadr cur-storage) (car cur-storage)) (cddr cur-storage))))
        ((equal? cmd0 "ㄴ") (vector-set! storage storage-no (cons (/ (cadr cur-storage) (car cur-storage)) (cddr cur-storage))))
        ((equal? cmd0 "ㄹ") (vector-set! storage storage-no (cons (remainder (cadr cur-storage) (car cur-storage)) (cddr cur-storage))))
        ((equal? cmd0 "ㅁ")
         (define to-display
           (cond ((= storage-no 21) (car (reverse cur-storage)))
                 (else (car cur-storage))))
         (begin 
           (cond ((equal? cmd2 "ㅇ") (display to-display))
                 ((equal? cmd2 "ㅎ") (display (integer->char to-display))))
           (cond ((= storage-no 21) (vector-set! storage storage-no (reverse (cdr (reverse cur-storage)))))
                 (else (vector-set! storage storage-no (cdr cur-storage))))))
        ((equal? cmd0 "ㅂ") (vector-set! storage storage-no (cons (hash-ref lines cmd2) cur-storage)))
        ((equal? cmd0 "ㅃ") (vector-set! storage storage-no (cons (car cur-storage) cur-storage)))
        ((equal? cmd0 "ㅍ") (vector-set! storage storage-no (cons (cadr cur-storage) (cons (car cur-storage) (cddr cur-storage)))))
        ((equal? cmd0 "ㅅ") (set! new-storage-no (hash-ref jongsung-index cmd2)) (void))
        ;what if the storage is empty?
        ((equal? cmd0 "ㅆ") (unless (= (length cur-storage) 0)
                             (begin
                               (vector-set! storage (hash-ref jongsung-index cmd2) (cons (car cur-storage) (vector-ref storage (hash-ref jongsung-index cmd2))))
                               (vector-set! storage storage-no (cdr cur-storage)))))
        ((equal? cmd0 "ㅈ") (if (>= (cadr cur-storage) (car cur-storage)) (vector-set! storage storage-no (cons 1 (cddr cur-storage)))
                               (vector-set! storage storage-no (cons 0 (cddr cur-storage)))))
        ((equal? cmd0 "ㅊ") (if (= (car cur-storage) 0) (set! decision -1)
                                (set! decision 1))
                           (vector-set! storage storage-no (cdr cur-storage))))
                           
  
  (cond ((equal? cmd1 "ㅏ") (run-aheui cmds (+ pos-x (* 1 decision)) pos-y pos-x pos-y (* 1 decision) new-storage-no storage))
        ((equal? cmd1 "ㅓ") (run-aheui cmds (- pos-x (* 1 decision)) pos-y pos-x pos-y (* -1 decision) new-storage-no storage))
        ((equal? cmd1 "ㅗ") (run-aheui cmds pos-x (- pos-y (* 1 decision)) pos-x pos-y (* -2 decision) new-storage-no storage))
        ((equal? cmd1 "ㅜ") (run-aheui cmds pos-x (+ pos-y (* 1 decision)) pos-x pos-y (* 2 decision) new-storage-no storage))
        ((equal? cmd1 "ㅑ") (run-aheui cmds (+ pos-x (* 2 decision)) pos-y pos-x pos-y (* 1 decision) new-storage-no storage))
        ((equal? cmd1 "ㅕ") (run-aheui cmds (- pos-x (* 2 decision)) pos-y pos-x pos-y (* -1 decision) new-storage-no storage))
        ((equal? cmd1 "ㅛ") (run-aheui cmds pos-x (- pos-y (* 2 decision)) pos-x pos-y (* -2 decision) new-storage-no storage))
        ((equal? cmd1 "ㅠ") (run-aheui cmds pos-x (+ pos-y (* 2 decision)) pos-x pos-y (* 2 decision) new-storage-no storage))
        ;decision with ㅊ not implemented below here
        ((equal? cmd1 "ㅡ")
         (if (= (remainder direction 2) 1) (run-aheui cmds (+ pos-x direction) pos-y pos-x pos-y direction new-storage-no storage)
             (run-aheui cmds prev-x prev-y pos-x pos-y (* direction -1) new-storage-no storage)))
        ((equal? cmd1 "ㅣ")
         (if (= (remainder direction 2) 0) (run-aheui cmds pos-x (+ pos-y (/ direction 2)) pos-x pos-y direction new-storage-no storage)
             (run-aheui cmds prev-x prev-y pos-x pos-y (* direction -1) new-storage-no storage)))
        ((equal? cmd1 "ㅢ") (run-aheui cmds prev-x prev-y pos-x pos-y (* direction -1) new-storage-no storage))
        (else (run-aheui cmds prev-x prev-y pos-x pos-y direction new-storage-no storage)))))


(define aheui-vector (list->vector (call-with-input-file filename read-next-line-iter)))
                     
(define new-storage (make-vector 28))
(vector-fill! new-storage '())

(run-aheui aheui-vector 0 0 0 0 1 0 new-storage)
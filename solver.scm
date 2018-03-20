; start is a list of pairs representing the move history of whatever branch you're on
; end is a pair representing the final place you need to arive at
; maze is a n membered list filled with lists that themselves have n members, these members should be either 0 or 1

(define (maze-solver start end maze)
  (display 'top) (display start) (display '--) (display maze) (newline )
  (let* (
        (start-x (caar start))
        (start-y (cdar start))
        (end-x (car end))
        (end-y (cdr end))
        (side-length (length maze))
        (finished? (and (= start-x end-x) (= start-y end-y )))
        (out-of-bounds? (lambda (x) (or (< x 0) (> x side-length))))
        (left-board? (or (out-of-bounds? start-x) (out-of-bounds? start-y)))
        (hit-wall? (if (left-board?)
          '()
          (= 1 (list-ref (list-ref maze start-x) start-y))))
        )
    (display finished?) (newline ) (display hit-wall?) (newline ) (display left-board?) (newline )
    (cond
      ( finished? (cons #t start))
      ( hit-wall? (cons #f start))
      ( left-board? (cons #f start))
      (else (let* (
                  (up-one (cons (cons start-x (+ 1 start-y)) start))
                  (right-one (cons (cons (+ 1 start-x) start-y) start))
                  (down-one (cons (cons start-x (- start-y 1)) start))
                  (left-one (cons (cons (- start-x 1) start-y) start))
                  (up-worked? (maze-solver up-one end maze))
                  (right-worked? (maze-solver right-one end maze))
                  (down-worked? (maze-solver down-one end maze))
                  (left-worked? (maze-solver left-one end maze))
                  )
              (display 'made-it-here)
              (display up-worked?) (newline ) (display right-worked?) (newline ) (display down-worked?) (newline ) (display left-worked?)
              (cond
                ((car up-worked?) up-worked?)
                ((car right-worked?) right-worked?)
                ((car down-worked?) down-worked?)
                (else left-worked?)))))))
                
(define starter (list (cons 4 3)))

(define ender (cons 0 4))

(define test-maze (list (list 0 0 0 0 0)(list 0 1 1 1 0)(list 0 1 1 1 0)(list 0 1 1 1 0)(list 0 0 0 0 0)))

(maze-solver starter ender test-maze)
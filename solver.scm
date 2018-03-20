; start is a list of pairs representing the move history of whatever branch you're on
; end is a pair representing the final place you need to arive at
; maze is a n membered list filled with lists that themselves have n members, these members should be either 0 or 1

; some thoughts on this project: weirdly, this method is only sort of recursive, it does have deferred logic (the logic that decides
; node by node, which move history to return to the move above it) but it has state variables and detailed termination conditions
; like an iterative process, maybe worth rewriting to get it completely iterative or completely recursive

(define (maze-solver start end maze)
  (let* (
        (start-x (caar start))
        (start-y (cdar start))
        (end-x (car end))
        (end-y (cdr end))
        (side-length (length maze))
        ; here we check to see if the latest member of start matches end, if so then this branch is done
        (finished? (and (= start-x end-x) (= start-y end-y )))
        ; here we check to see if we've gone out of bounds, if so this branch is done
        (out-of-bounds? (lambda (x) (or (< x 0) (> x 4))))
        (left-board? (or (out-of-bounds? start-x) (out-of-bounds? start-y)))
        ; here we check to see if we've gone out of bounds, if we have then we're done, this is because if we allowed out
        ; of bounds then you end up with branches that endlessly toggle between two squares
        (step-back? (and (> (length start) 2) (and (= start-x (car (third start))) (= start-y (cdr (third start))))))
        ; here we check to see if we've hit a wall, if we have then this branch is done
        (hit-wall?  (and (not left-board?) (= 1 (list-ref (list-ref maze start-x) start-y))))
        )
    (cond
      ; if any of the conditions above have been met then cap this branch with a boolean, yes for succesful completions,
      ; no for failures, the reason we do this is because at each node that is not a leaf we will return the return value of
      ; only one of that nodes branches, if there is a succesful completion we will return that one, otherwise we return one
      ; of the failed ones, if there is any way to succesfully traverse the maze it will end up as the return value of the original
      ; call
      ( finished? (cons #t start))
      ( left-board? (cons #f start))
      ( step-back? (cons #f start))
      ( hit-wall? (cons #f start))
      (else (let* (
                  ; here we generate the move histories of the four possible moves that
                  ; could be made from this point
                  (up-one (cons (cons start-x (+ 1 start-y)) start))
                  (right-one (cons (cons (+ 1 start-x) start-y) start))
                  (left-one (cons (cons (- start-x 1) start-y) start))
                  (down-one (cons (cons start-x (- start-y 1)) start))
                  ; and call them
                  (up-worked? (maze-solver up-one end maze))
                  (right-worked? (maze-solver right-one end maze))
                  (down-worked? (maze-solver down-one end maze))
                  (left-worked? (maze-solver left-one end maze))
                  )
              (cond
                ; if any of those branches worked then their first element will be a true boolean
                ; and they will be the return value from this node
                ; otherwise we simply return the left call
                ((car up-worked?) up-worked?)
                ((car right-worked?) right-worked?)
                ((car down-worked?) down-worked?)
                (else left-worked?)))))))
                
(define starter (list (cons 4 0)))

(define ender (cons 0 0))

(define test-maze (list (list 0 0 0 0 0)(list 0 1 1 1 0)(list 0 1 1 1 0)(list 0 1 1 1 0)(list 0 0 0 0 0)))

(maze-solver starter ender test-maze)
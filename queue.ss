(library (chez-speedy-queue queue)
  (export make-queue
	  empty-queue?
	  full-queue?
	  enqueue
	  dequeue)
  (import (chezscheme))

  (define queue-sentinel (gensym))

  (define make-queue
    (lambda (length)
      (let ([queue (make-vector (+ 2 length))])
	(vector-set! queue 0 2)
	(vector-set! queue 1 2)
	(vector-set! queue 2 queue-sentinel)
	queue)))
  
  (define queue-out
    (lambda (queue)
      (vector-ref queue 0)))
  
  (define queue-in
    (lambda (queue)
      (vector-ref queue 1)))
  
  (define next-index
    (lambda (current-index queue-real-length)
      (let ([new-index (+ current-index 1)])
	(if (fx= new-index queue-real-length) 2 new-index))))

  (define enqueue
    (lambda (queue object)
      (let ([in (queue-in queue)])
	(if (or (not (fx= in (queue-out queue)))
		(eq? (vector-ref queue in) queue-sentinel))
	    (begin (vector-set! queue in object)
		   (vector-set! queue 1 (next-index in (vector-length queue)))
		   object)
	    (error 'queue-overflow-error "queue is full, and can't be enqueued anymore")))))
  
  (define dequeue
    (lambda (queue)
      (let* ([out (queue-out queue)]
	     [out-object (vector-ref queue out)])
	(if (eq? out-object queue-sentinel)
	    (error 'queue-underflow-error "queue is empty, and can't be dequeued anymore")
	    (begin (vector-set! queue 0
				(if (fx= (incf out) (vector-length queue))
				    (begin (set! out 2) out) out))
		   (when (fx= (queue-in queue) out)
			 (vector-set! queue out queue-sentinel))
		   out-object)))))

  (define empty-queue?
    (lambda (queue)
      (eq? (vector-ref queue (queue-out queue)) queue-sentinel)))

  (define full-queue?
    (lambda (queue)
      (let ([out (queue-out queue)])
	(when (fx= out (queue-in queue))
	      (not (eq? (vector-ref queue out) queue-sentinel))))))

  (define-syntax incf
    (syntax-rules ()
      ((_ x) (begin (set! x (+ x 1)) x))
      ((_ x i) (begin (set! x (+ x i)) x))))
  )

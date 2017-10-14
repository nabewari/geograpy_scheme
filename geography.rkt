#lang racket
(require (only-in 2htdp/batch-io read-csv-file)
         plot
         xml
         xml/path)
(define g (read-csv-file "dem.csv"))
(define z (map string->number (car g)))
(define x
  (flatten
   (map (lambda (y) (build-list 257 (lambda (x) y)))
        (build-list 257 values))))
(define y
  (letrec ((func (lambda (x a)
                   (if (= x 257)
                       a
                       (func (+ x 1) (append (build-list 257 values) a))))))
    (func 0 '())))
          
(define t
  (xml->xexpr (document-element
               (call-with-input-file
                   "20161228.kml" read-xml))))
(define traj
     (map (lambda (x) (map string->number x))     
       (map (lambda (x) (string-split x ","))
            (call-with-input-string
             (se-path* '(coordinates) t)
             port->lines))))
(define scale 10)
(define traj-s
  (letrec ((func (lambda (l a)
                   (let* ((s (car l))
                          (e (if (= (length l) 1)
                                 #f
                                 (cadr l)))
                          (color (if (false? e)
                                     #f
                                     (* scale (dist s e)))))
                   (if (false? e)
                       a
                       (func (cdr l)
                        (cons (lines3d (map list->vector
                                           (list (car l) (cadr l)))
                                      #:color (list color 0 100)
                                      #:width 10) a))))))
           (dist (lambda (s e)
                   (sqrt
                    (apply +
                           (map sqr
                                (map - s e)))))))
    (func traj '())))
(define nabewari
  (plot3d (list (points3d (map vector x y z)
                        #:sym 'dot
                        #:alpha 0.8)
              (lines3d (map list->vector traj)
                       #:color '(0 255 255)))
              #:altitude 25))

(define bokka
  (plot3d traj-s
          #:altitude 25))
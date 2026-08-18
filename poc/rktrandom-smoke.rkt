#lang racket/base

;; rktrandom cross-platform determinism smoke.
;; Emits a canonical text transcript of deterministic draws; the same
;; script must produce byte-identical output on every platform.

(require '#%linklet
         racket/random/generator
         racket/flonum
         racket/list)

(unless rktrandom-available?
  (error 'smoke "rktrandom-available? is #f"))
(printf "rktrandom-available? #t\n")

;; C-level known-answer selftest (reference-checked vs upstream impls)
(define selftest
  (hash-ref (primitive-table '#%rktrandom) 'rktrandom_selftest))
(unless (= 1 (selftest))
  (error 'smoke "rktrandom_selftest failed"))
(printf "selftest 1\n")

(define (hex bs)
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (if (< b 16) (format "0~x" b) (format "~x" b)))))

(define algs '(xoshiro256++ xoshiro256** xoroshiro128++ sfc64 pcg64-dxsm philox4x64))

(for ([alg (in-list algs)])
  (printf "== ~a\n" alg)
  ;; small-integer seed path (splitmix64 expansion)
  (define g (make-rgen 42 #:algorithm alg))
  (printf "u64 ~a\n" (for/list ([_ (in-range 8)]) (rgen-u64 g)))
  (printf "real ~a\n" (for/list ([_ (in-range 4)]) (rgen-real g)))
  (printf "normal ~a\n" (for/list ([_ (in-range 4)]) (rgen-normal g)))
  (printf "exp ~a\n" (for/list ([_ (in-range 4)]) (rgen-exponential g)))
  (printf "int1e6 ~a\n" (for/list ([_ (in-range 8)]) (rgen-integer g 1000000)))
  (printf "bytes ~a\n" (hex (rgen-bytes g 32)))
  (printf "flvector ~a\n" (for/list ([x (in-flvector (rgen-flvector g 4))]) x))
  (printf "shuffle ~a\n" (rgen-shuffle g (range 16)))
  (printf "weighted ~a\n"
          (for/list ([_ (in-range 8)])
            (rgen-weighted-index g (vector 1.0 2.0 3.0 4.0))))
  ;; jump / long-jump / fork (not every algorithm has a jump)
  (with-handlers ([exn:fail? (lambda (e) (printf "jump none\n"))])
    (define gj (make-rgen 42 #:algorithm alg))
    (rgen-jump! gj)
    (printf "post-jump ~a\n" (for/list ([_ (in-range 2)]) (rgen-u64 gj))))
  (with-handlers ([exn:fail? (lambda (e) (printf "long-jump none\n"))])
    (define gl (make-rgen 42 #:algorithm alg))
    (rgen-long-jump! gl)
    (printf "post-long-jump ~a\n" (for/list ([_ (in-range 2)]) (rgen-u64 gl))))
  (with-handlers ([exn:fail? (lambda (e) (printf "fork none\n"))])
    (define gp (make-rgen 42 #:algorithm alg))
    (define gc (rgen-fork gp))
    (printf "fork-parent ~a\n" (for/list ([_ (in-range 2)]) (rgen-u64 gp)))
    (printf "fork-child ~a\n" (for/list ([_ (in-range 2)]) (rgen-u64 gc))))
  ;; 32-byte seed + wide stream (exercises rgen-init-with paths)
  (define g2 (make-rgen (make-bytes 32 7) #:algorithm alg
                        #:stream (expt 2 70)))
  (printf "wide-stream ~a\n" (for/list ([_ (in-range 4)]) (rgen-u64 g2))))

;; distribution sanity: deterministic values, loose semantic bounds
(let* ([g (make-rgen 2026 #:algorithm 'xoshiro256++)]
       [n 100000]
       [xs (rgen-flvector g n)]
       [mean (/ (for/sum ([x (in-flvector xs)]) x) n)]
       [gn (make-rgen 2026 #:algorithm 'pcg64-dxsm)]
       [ys (build-list n (lambda (_) (rgen-normal gn)))]
       [nmean (/ (for/sum ([y (in-list ys)]) y) n)]
       [nvar (/ (for/sum ([y (in-list ys)]) (* (- y nmean) (- y nmean))) n)])
  (unless (< 0.49 mean 0.51) (error 'smoke "uniform mean out of bounds: ~a" mean))
  (unless (< -0.02 nmean 0.02) (error 'smoke "normal mean out of bounds: ~a" nmean))
  (unless (< 0.97 nvar 1.03) (error 'smoke "normal variance out of bounds: ~a" nvar))
  (printf "dist-uniform-mean ~a\n" mean)
  (printf "dist-normal-mean ~a\n" nmean)
  (printf "dist-normal-var ~a\n" nvar))

(displayln "rktrandom-smoke-ok")

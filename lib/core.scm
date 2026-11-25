;;; lib/core.scm - Headless pure logic for oil.hx

(provide path-join
         basename
         parent-dir
         entry-dir?
         entry-raw-name
         entry-display-name
         hidden-entry?
         entry-extension
         unique-copy-path
         format-file-size
         format-permissions
         format-metadata-hint
         pad-left
         pad-right
         list-max-length
         sort-entries
         unquote-git-path
         parse-git-line
         git-relative-prefix
         git-ignored-in?
         entry-git-status-from
         strip-cr
         parse-buffer-entries
         find-duplicate-entries
         name-similarity
         plan-wdired-changes
         glob-match?
         format-save-preview-lines)

(define (path-join base name)
  (cond
    [(or (not base) (string=? base "")) name]
    [(ends-with? base (path-separator)) (string-append base name)]
    [else (string-append base (path-separator) name)]))

(define (basename full-path)
  (let* ([parts (split-many full-path (path-separator))]
         [nonempty (filter (lambda (s) (> (string-length s) 0)) parts)])
    (if (null? nonempty)
        full-path
        (list-ref nonempty (- (length nonempty) 1)))))

(define (parent-dir full-path)
  (let ([p (parent-name full-path)])
    (if (or (not p) (string=? p "")) "/" p)))

(define (entry-dir? name)
  (ends-with? name "/"))

(define (entry-raw-name entry)
  (if (entry-dir? entry)
      (trim-end-matches entry "/")
      entry))

(define (entry-display-name full-path is-dir-flag?)
  (let ([name (basename full-path)])
    (if is-dir-flag?
        (if (ends-with? name "/") name (string-append name "/"))
        name)))

(define (hidden-entry? name)
  (let ([raw (entry-raw-name name)])
    (and (> (string-length raw) 0)
         (not (string=? raw ".."))
         (char=? (string-ref raw 0) #\.))))

(define (entry-extension name)
  (let* ([raw (entry-raw-name name)]
         [base (basename raw)]
         [len (string-length base)])
    (let loop ([i (- len 1)])
      (cond
        [(<= i 0) ""]
        [(char=? (string-ref base i) #\.)
         (string-downcase (substring base (+ i 1) len))]
        [else (loop (- i 1))]))))

(define (unique-copy-path base-path is-dir? exists?)
  (let* ([clean (if is-dir? (trim-end-matches base-path "/") base-path)]
         [first-try (string-append clean " (copy)")])
    (if (not (exists? first-try))
        first-try
        (let loop ([n 2])
          (let ([candidate (string-append clean " (copy " (number->string n) ")")])
            (if (not (exists? candidate))
                candidate
                (loop (+ n 1))))))))

(define (format-file-size bytes-str)
  (let ([n (if (number? bytes-str) bytes-str (string->number bytes-str))])
    (cond
      [(not n) "?"]
      [(< n 1024) (string-append (number->string n) "B")]
      [(< n (* 1024 1024)) (string-append (number->string (quotient n 1024)) "K")]
      [(< n (* 1024 1024 1024)) (string-append (number->string (quotient n (* 1024 1024))) "M")]
      [else (string-append (number->string (quotient n (* 1024 1024 1024))) "G")])))

(define (format-permissions perms)
  (if (and (> (string-length perms) 1)
           (char=? (string-ref perms 0) #\-))
      (substring perms 1 (string-length perms))
      perms))

(define (pad-left s width)
  (let ([n (string-length s)])
    (if (>= n width)
        s
        (string-append (make-string (- width n) #\space) s))))

(define (pad-right s width)
  (let ([n (string-length s)])
    (if (>= n width)
        s
        (string-append s (make-string (- width n) #\space)))))

(define (list-max-length strs)
  (let loop ([lst strs] [m 0])
    (if (null? lst)
        m
        (loop (cdr lst) (max m (string-length (car lst)))))))

;; meta-vals: (list perms links size owner mtime symlink-target)
(define (format-metadata-hint meta-vals)
  (let* ([perms (list-ref meta-vals 0)]
         [links (list-ref meta-vals 1)]
         [size  (list-ref meta-vals 2)]
         [owner (if (>= (length meta-vals) 4) (list-ref meta-vals 3) "")]
         [mtime (if (>= (length meta-vals) 5) (list-ref meta-vals 4) "")]
         [link-target (if (>= (length meta-vals) 6) (list-ref meta-vals 5) #false)]
         [base (string-append
                 (pad-right (format-permissions perms) 10)
                 "  " (pad-left links 3)
                 (if (string=? owner "") "" (string-append "  " (pad-right owner 8)))
                 "  " (pad-left (format-file-size size) 5)
                 (if (string=? mtime "") "" (string-append "  " mtime)))])
    (if (and link-target (not (string=? link-target "")))
        (string-append "-> " link-target "  " base)
        base)))

(define (meta-size-num meta-alist name)
  (let ([m (assoc (entry-raw-name name) meta-alist)])
    (if (and m (>= (length (cdr m)) 3))
        (or (string->number (list-ref (cdr m) 2)) 0)
        0)))

(define (meta-mtime-str meta-alist name)
  (let ([m (assoc (entry-raw-name name) meta-alist)])
    (if (and m (>= (length (cdr m)) 5))
        (list-ref (cdr m) 4)
        "")))

(define (make-entry-cmp sort-by reverse? meta-alist)
  (lambda (a b)
    (let ([res
           (cond
             [(eq? sort-by 'size)
              (let ([sa (meta-size-num meta-alist a)]
                    [sb (meta-size-num meta-alist b)])
                (if (= sa sb)
                    (string<? (string-downcase a) (string-downcase b))
                    (> sa sb)))]
             [(eq? sort-by 'mtime)
              (let ([ma (meta-mtime-str meta-alist a)]
                    [mb (meta-mtime-str meta-alist b)])
                (if (string=? ma mb)
                    (string<? (string-downcase a) (string-downcase b))
                    (string>? ma mb)))]
             [(eq? sort-by 'ext)
              (let ([ea (entry-extension a)]
                    [eb (entry-extension b)])
                (if (string=? ea eb)
                    (string<? (string-downcase a) (string-downcase b))
                    (string<? ea eb)))]
             [else
              (string<? (string-downcase a) (string-downcase b))])])
      (if reverse? (not res) res))))

(define (sort-entries entries sort-by reverse? meta-alist)
  (let* ([has-parent? (and (not (null? entries)) (member "../" entries))]
         [rest (filter (lambda (e) (not (string=? e "../"))) entries)]
         [dirs (filter entry-dir? rest)]
         [files (filter (lambda (e) (not (entry-dir? e))) rest)]
         [cmp (make-entry-cmp sort-by reverse? meta-alist)]
         [sorted-dirs (sort dirs cmp)]
         [sorted-files (sort files cmp)]
         [body (append sorted-dirs sorted-files)])
    (if has-parent? (cons "../" body) body)))

;; Unquote git porcelain C-quoted path like "\"foo \\\"bar\\\".txt\""
(define (unquote-git-path s)
  (let ([len (string-length s)])
    (if (and (>= len 2)
             (char=? (string-ref s 0) #\")
             (char=? (string-ref s (- len 1)) #\"))
        (let loop ([i 1] [acc '()])
          (if (>= i (- len 1))
              (apply string (reverse acc))
              (let ([c (string-ref s i)])
                (if (and (char=? c #\\) (< (+ i 1) (- len 1)))
                    (let ([nxt (string-ref s (+ i 1))])
                      (cond
                        [(char=? nxt #\\) (loop (+ i 2) (cons #\\ acc))]
                        [(char=? nxt #\") (loop (+ i 2) (cons #\" acc))]
                        [(char=? nxt #\t) (loop (+ i 2) (cons #\tab acc))]
                        [(char=? nxt #\n) (loop (+ i 2) (cons #\newline acc))]
                        [else (loop (+ i 2) (cons nxt acc))]))
                    (loop (+ i 1) (cons c acc))))))
        s)))

(define (parse-git-line line)
  (if (< (string-length line) 4)
      #false
      (let* ([x (substring line 0 1)]
             [y (substring line 1 2)]
             [raw-path (trim (substring line 3 (string-length line)))]
             [path (if (or (string=? x "R") (string=? x "C"))
                       (let ([parts (split-many raw-path " -> ")])
                         (if (>= (length parts) 2) (list-ref parts 1) raw-path))
                       raw-path)]
             [clean-path (unquote-git-path path)]
             [label (cond
                      [(string=? x "?") " ?"]
                      [(string=? x "!") " !"]
                      [(string=? x "R") " →"]
                      [(string=? x "A") " +"]
                      [(or (string=? x "M") (string=? y "M")) " ~"]
                      [else #false])])
        (if label (cons label clean-path) #false))))

(define (git-relative-prefix dir root)
  (if (or (not root) (string=? dir root))
      ""
      (let ([root-with-sep (if (ends-with? root "/") root (string-append root "/"))])
        (if (starts-with? dir root-with-sep)
            (let ([sub (substring dir (string-length root-with-sep))])
              (if (or (string=? sub "") (ends-with? sub "/"))
                  sub
                  (string-append sub "/")))
            ""))))

(define (git-ignored-in? entry git-status)
  (let ([name (entry-raw-name entry)])
    (let loop ([ps git-status])
      (if (null? ps)
          #false
          (let ([label (car (car ps))]
                [gpath (trim-end-matches (cdr (car ps)) "/")])
            (if (and (string=? label " !")
                     (or (string=? gpath name)
                         (starts-with? (string-append name "/") (string-append gpath "/"))))
                #true
                (loop (cdr ps))))))))

(define (entry-git-status-from entry git-status)
  (let ([name (entry-raw-name entry)])
    (let loop ([ps git-status])
      (if (null? ps)
          #false
          (let ([git-path (trim-end-matches (cdr (car ps)) "/")]
                [label (car (car ps))])
            (if (or (string=? git-path name)
                    (starts-with? git-path (string-append name "/")))
                label
                (loop (cdr ps))))))))

;; Strip trailing CR without destroying intentional spaces in filenames
(define (strip-cr line)
  (trim-end-matches line "\r"))

(define (parse-buffer-entries text)
  (let* ([lines (split-many text "\n")]
         [data (if (null? lines) '() (cdr lines))])
    (filter (lambda (e) (> (string-length e) 0))
            (map strip-cr data))))

(define (find-duplicate-entries entries)
  (let loop ([rest entries] [seen '()] [dups '()])
    (if (null? rest)
        (reverse dups)
        (let* ([cur (car rest)]
               [raw (entry-raw-name cur)])
          (if (member raw seen)
              (loop (cdr rest) seen (if (member cur dups) dups (cons cur dups)))
              (loop (cdr rest) (cons raw seen) dups))))))

;; Similarity metric in [0.0, 1.0] for matching renamed files
(define (common-prefix-len a b)
  (let ([limit (min (string-length a) (string-length b))])
    (let loop ([i 0])
      (if (or (>= i limit) (not (char=? (string-ref a i) (string-ref b i))))
          i
          (loop (+ i 1))))))

(define (common-suffix-len a b)
  (let ([la (string-length a)]
        [lb (string-length b)])
    (let ([limit (min la lb)])
      (let loop ([i 0])
        (if (or (>= i limit)
                (not (char=? (string-ref a (- la 1 i))
                             (string-ref b (- lb 1 i)))))
            i
            (loop (+ i 1)))))))

(define (split-stem-ext raw-name)
  (let* ([base (basename (entry-raw-name raw-name))]
         [len (string-length base)])
    (let loop ([i (- len 1)])
      (cond
        [(<= i 0) (cons base "")]
        [(char=? (string-ref base i) #\.)
         (cons (substring base 0 i) (string-downcase (substring base (+ i 1) len)))]
        [else (loop (- i 1))]))))

(define (string-sim a b)
  (let* ([la (string-length a)]
         [lb (string-length b)]
         [max-l (max la lb)])
    (if (= max-l 0)
        0.0
        (let* ([pref (common-prefix-len a b)]
               [suff (min (- max-l pref) (common-suffix-len a b))])
          (/ (+ pref suff) (* 1.0 max-l))))))

(define (name-similarity a b)
  (let* ([ra (entry-raw-name a)]
         [rb (entry-raw-name b)]
         [ba (basename ra)]
         [bb (basename rb)])
    (cond
      [(string=? ra rb) 1.0]
      ;; Cross-directory move with identical filename
      [(string=? ba bb) 0.95]
      [else
       (let* ([sa (split-stem-ext ra)]
              [sb (split-stem-ext rb)]
              [stem-a (car sa)]
              [ext-a (cdr sa)]
              [stem-b (car sb)]
              [ext-b (cdr sb)]
              [stem-s (string-sim stem-a stem-b)]
              [same-ext? (string=? ext-a ext-b)])
         (cond
           [(string=? stem-a stem-b) 0.75]
           [(<= stem-s 0.0) 0.0]
           [same-ext? (min 0.95 (+ stem-s 0.15))]
           [else (* 0.85 stem-s)]))])))

;; LCS index pairs between orig and curr lists of strings
(define (lcs-anchors orig curr)
  (let* ([n (length orig)]
         [m (length curr)]
         [ov (list->vector orig)]
         [cv (list->vector curr)]
         [cols (+ m 1)]
         [dp (make-vector (* (+ n 1) cols) 0)])
    (define (dp-get r c) (vector-ref dp (+ (* r cols) c)))
    (define (dp-set! r c v) (vector-set! dp (+ (* r cols) c) v))
    (let rloop ([i (- n 1)])
      (when (>= i 0)
        (let cloop ([j (- m 1)])
          (when (>= j 0)
            (if (string=? (vector-ref ov i) (vector-ref cv j))
                (dp-set! i j (+ 1 (dp-get (+ i 1) (+ j 1))))
                (dp-set! i j (max (dp-get (+ i 1) j) (dp-get i (+ j 1)))))
            (cloop (- j 1))))
        (rloop (- i 1))))
    (let trace ([i 0] [j 0] [acc '()])
      (cond
        [(or (>= i n) (>= j m)) (reverse acc)]
        [(string=? (vector-ref ov i) (vector-ref cv j))
         (trace (+ i 1) (+ j 1) (cons (cons i j) acc))]
        [(>= (dp-get (+ i 1) j) (dp-get i (+ j 1)))
         (trace (+ i 1) j acc)]
        [else
         (trace i (+ j 1) acc)]))))

(define (slice-range lst start end)
  (let loop ([l lst] [idx 0] [acc '()])
    (cond
      [(or (null? l) (>= idx end)) (reverse acc)]
      [(>= idx start) (loop (cdr l) (+ idx 1) (cons (car l) acc))]
      [else (loop (cdr l) (+ idx 1) acc)])))

(define (same-entry-type? a b)
  (equal? (entry-dir? a) (entry-dir? b)))

(define (best-global-pair rem-list add-list min-score)
  (let rloop ([rs rem-list] [best-r #false] [best-a #false] [best-sc min-score])
    (if (null? rs)
        (if best-r (list best-r best-a) #false)
        (let ([r (car rs)])
          (let aloop ([as add-list] [br best-r] [ba best-a] [bsc best-sc])
            (if (null? as)
                (rloop (cdr rs) br ba bsc)
                (let ([a (car as)])
                  (if (same-entry-type? r a)
                      (let ([sc (name-similarity r a)])
                        (if (> sc bsc)
                            (aloop (cdr as) r a sc)
                            (aloop (cdr as) br ba bsc)))
                      (aloop (cdr as) br ba bsc)))))))))

(define (remove-first x lst)
  (cond
    [(null? lst) '()]
    [(string=? (car lst) x) (cdr lst)]
    [else (cons (car lst) (remove-first x (cdr lst)))]))

(define (greedy-pair-by-similarity rem-list add-list min-score)
  (let loop ([rem rem-list] [add add-list] [renames '()])
    (let ([best (best-global-pair rem add min-score)])
      (if (not best)
          (list (reverse renames) rem add)
          (let ([r (list-ref best 0)]
                [a (list-ref best 1)])
            (loop (remove-first r rem)
                  (remove-first a add)
                  (cons (cons r a) renames)))))))

;; Pair items within one aligned gap between LCS anchors
(define (pair-gap orig-gap curr-gap)
  (let ([same-type-1to1? (and (= (length orig-gap) 1)
                               (= (length curr-gap) 1)
                               (same-entry-type? (car orig-gap) (car curr-gap)))])
    (if same-type-1to1?
        (list (list (cons (car orig-gap) (car curr-gap))) '() '())
        (greedy-pair-by-similarity orig-gap curr-gap 0.34))))

;; Global cross-gap matching only for high-confidence moves/renames
(define (pair-cross-gap rem-left add-left)
  (greedy-pair-by-similarity rem-left add-left 0.64))

(define (plan-wdired-changes orig-raw curr-raw)
  (let* ([orig (filter (lambda (e) (not (string=? e "../"))) orig-raw)]
         [curr (filter (lambda (e) (not (string=? e "../"))) curr-raw)]
         [dups (find-duplicate-entries curr)])
    (if (not (null? dups))
        (list 'error (string-append "Duplicate entries in buffer: " (string-join dups ", ")))
        (let* ([anchors (lcs-anchors orig curr)]
               [sentinels (append anchors (list (cons (length orig) (length curr))))])
          (let loop ([ancs sentinels]
                     [oi 0]
                     [ci 0]
                     [renames '()]
                     [rem-left '()]
                     [add-left '()])
            (if (null? ancs)
                (let* ([cross (pair-cross-gap rem-left add-left)]
                       [all-renames (append renames (list-ref cross 0))]
                       [actual-renames (filter (lambda (p) (not (string=? (car p) (cdr p)))) all-renames)]
                       [to-delete (list-ref cross 1)]
                       [to-create (list-ref cross 2)])
                  (list 'ok actual-renames to-delete to-create))
                (let* ([a (car ancs)]
                       [next-oi (car a)]
                       [next-ci (cdr a)]
                       [og (slice-range orig oi next-oi)]
                       [cg (slice-range curr ci next-ci)]
                       [gap-res (pair-gap og cg)])
                  (loop (cdr ancs)
                        (+ next-oi 1)
                        (+ next-ci 1)
                        (append renames (list-ref gap-res 0))
                        (append rem-left (list-ref gap-res 1))
                        (append add-left (list-ref gap-res 2))))))))))

(define (format-save-preview-lines renames to-delete to-create)
  (append
    (map (lambda (p) (string-append "  RENAME  " (car p) " -> " (cdr p))) renames)
    (map (lambda (n) (string-append "  CREATE  " n)) to-create)
    (map (lambda (n) (string-append "  DELETE  " n)) to-delete)))

;; Wildcard (*, ?) or substring matcher (case-insensitive)
(define (glob-match? pattern text)
  (let ([pat (string-downcase pattern)]
        [txt (string-downcase text)])
    (if (and (not (string-contains? pat "*"))
             (not (string-contains? pat "?")))
        (string-contains? txt pat)
        (let ([plen (string-length pat)]
              [tlen (string-length txt)])
          (let loop ([pi 0] [ti 0] [star -1] [match 0])
            (cond
              [(>= ti tlen)
               (let tail ([k pi])
                 (cond
                   [(and (< k plen) (char=? (string-ref pat k) #\*)) (tail (+ k 1))]
                   [else (= k plen)]))]
              [(and (< pi plen)
                    (or (char=? (string-ref pat pi) #\?)
                        (char=? (string-ref pat pi) (string-ref txt ti))))
               (loop (+ pi 1) (+ ti 1) star match)]
              [(and (< pi plen) (char=? (string-ref pat pi) #\*))
               (loop (+ pi 1) ti pi ti)]
              [(not (= star -1))
               (let ([next-match (+ match 1)])
                 (loop (+ star 1) next-match star next-match))]
              [else #false]))))))

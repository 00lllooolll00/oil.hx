;;; lib/ui.scm - Modal input, confirmation/diff preview, and help popup for oil.hx

(require "helix/components.scm")
(require "helix/misc.scm")

(provide oil-show-input-modal!
         oil-show-confirm-modal!
         oil-show-help-modal!)

(struct OilModalState ())
(struct OilHelpState ())

(define *modal-open?* #false)
(define *modal-mode* 'input) ; 'input | 'confirm
(define *modal-title* "")
(define *modal-buffer* "")
(define *modal-cursor* 0)
(define *modal-lines* '())
(define *modal-scroll* 0)
(define *modal-callback* #false)

(define *help-open?* #false)
(define *help-scroll* 0)

(define (safe-theme-scope scope fallback)
  (with-handler
    (lambda (_) (theme-scope-ref fallback))
    (theme-scope-ref scope)))

(define (truncate-str s max-len)
  (if (<= max-len 0)
      ""
      (let ([n (string-length s)])
        (if (<= n max-len)
            s
            (if (> max-len 1)
                (string-append (substring s 0 (- max-len 1)) "…")
                (substring s 0 max-len))))))

(define (clamp v lo hi)
  (max lo (min hi v)))

(define (modal-dimensions rect)
  (let* ([term-w (area-width rect)]
         [term-h (area-height rect)]
         [max-line (let loop ([ls (cons *modal-title* *modal-lines*)] [m 0])
                     (if (null? ls)
                         m
                         (loop (cdr ls) (max m (string-length (car ls))))))]
         [content-w (if (eq? *modal-mode* 'input)
                        (+ (string-length *modal-title*) (string-length *modal-buffer*) 4)
                        (+ max-line 4))]
         [w (clamp content-w (min 44 (- term-w 2)) (max 44 (- term-w 4)))]
         [detail-count (length *modal-lines*)]
         [max-detail-h (max 3 (- term-h 8))]
         [visible-details (min detail-count max-detail-h)]
         [h (if (eq? *modal-mode* 'input)
                3
                (+ 4 visible-details (if (> detail-count 0) 1 0)))]
         [x (max 0 (quotient (- term-w w) 2))]
         [y (max 0 (quotient (- term-h h) 2))])
    (list x y w h visible-details)))

(define (confirm-line-style line)
  (cond
    [(starts-with? line "  DELETE") (safe-theme-scope "diff.minus" "error")]
    [(starts-with? line "  CREATE") (safe-theme-scope "diff.plus" "info")]
    [(starts-with? line "  RENAME") (safe-theme-scope "diff.delta" "warning")]
    [else (theme-scope-ref "ui.text")]))

(define (render-modal _state rect frame)
  (let* ([dims (modal-dimensions rect)]
         [x (list-ref dims 0)]
         [y (list-ref dims 1)]
         [w (list-ref dims 2)]
         [h (list-ref dims 3)]
         [visible-details (list-ref dims 4)]
         [bg-style (safe-theme-scope "ui.popup" "ui.background")]
         [border-style (safe-theme-scope "ui.window" "ui.text")]
         [text-style (theme-scope-ref "ui.text")]
         [title-style (style-with-bold (safe-theme-scope "ui.text.focus" "ui.text"))]
         [modal-area (area x y w h)]
         [inner-w (max 1 (- w 2))])
    (buffer/clear-with frame modal-area bg-style)
    (block/render frame modal-area (make-block bg-style border-style "all" "rounded"))
    (if (eq? *modal-mode* 'input)
        (let* ([prefix-len (string-length *modal-title*)]
               [avail (max 1 (- inner-w prefix-len))]
               [scroll-x (max 0 (- *modal-cursor* (- avail 1)))]
               [vis-buf (let* ([end (min (string-length *modal-buffer*) (+ scroll-x avail))])
                          (if (< scroll-x end)
                              (substring *modal-buffer* scroll-x end)
                              ""))])
          (frame-set-string! frame (+ x 1) (+ y 1) (truncate-str *modal-title* inner-w) title-style)
          (when (> avail 0)
            (frame-set-string! frame (+ x 1 prefix-len) (+ y 1) vis-buf text-style)))
        (begin
          (frame-set-string! frame (+ x 1) (+ y 1) (truncate-str *modal-title* inner-w) title-style)
          (let ([detail-start-y (+ y 2)]
                [total (length *modal-lines*)])
            (let loop ([i 0])
              (when (< i visible-details)
                (let ([idx (+ *modal-scroll* i)])
                  (when (< idx total)
                    (let ([line (list-ref *modal-lines* idx)])
                      (frame-set-string! frame
                                         (+ x 1)
                                         (+ detail-start-y i)
                                         (truncate-str line inner-w)
                                         (confirm-line-style line)))))
                (loop (+ i 1))))
            (let* ([footer-y (+ y h -2)]
                   [scroll-hint (if (> total visible-details)
                                    (string-append " [j/k scroll "
                                                   (number->string (+ *modal-scroll* 1))
                                                   "-"
                                                   (number->string (min total (+ *modal-scroll* visible-details)))
                                                   "/"
                                                   (number->string total)
                                                   "]")
                                    "")]
                   [footer (string-append "Confirm? (y = yes, n/Esc = cancel)" scroll-hint)])
              (frame-set-string! frame (+ x 1) footer-y (truncate-str footer inner-w) title-style)))))))

(define (modal-cursor _state rect)
  (if (eq? *modal-mode* 'confirm)
      #false
      (let* ([dims (modal-dimensions rect)]
             [x (list-ref dims 0)]
             [y (list-ref dims 1)]
             [w (list-ref dims 2)]
             [inner-w (max 1 (- w 2))]
             [prefix-len (string-length *modal-title*)]
             [avail (max 1 (- inner-w prefix-len))]
             [scroll-x (max 0 (- *modal-cursor* (- avail 1)))]
             [col (+ x 1 prefix-len (- *modal-cursor* scroll-x))])
        (position (+ y 1) (min (+ x w -2) col)))))

(define (close-modal!)
  (set! *modal-open?* #false)
  (pop-last-component-by-name! "oil-modal"))

(define (handle-modal-event _state event)
  (let ([ch (key-event-char event)])
    (cond
      [(mouse-event? event) event-result/consume]
      [(eq? *modal-mode* 'confirm)
       (cond
         [(or (key-event-down? event) (and (char? ch) (char=? ch #\j)))
          (let ([max-s (max 0 (- (length *modal-lines*) 3))])
            (set! *modal-scroll* (min max-s (+ *modal-scroll* 1))))
          event-result/consume]
         [(or (key-event-up? event) (and (char? ch) (char=? ch #\k)))
          (set! *modal-scroll* (max 0 (- *modal-scroll* 1)))
          event-result/consume]
         [(and (char? ch) (or (char=? ch #\y) (char=? ch #\Y)))
          (let ([cb *modal-callback*])
            (set! *modal-callback* #false)
            (close-modal!)
            (when cb (enqueue-thread-local-callback (lambda () (cb #true))))
            event-result/consume)]
         [(or (key-event-escape? event)
              (and (char? ch) (or (char=? ch #\n) (char=? ch #\N) (char=? ch #\q))))
          (let ([cb *modal-callback*])
            (set! *modal-callback* #false)
            (close-modal!)
            (when cb (enqueue-thread-local-callback (lambda () (cb #false))))
            event-result/consume)]
         [else event-result/consume])]
      [(key-event-enter? event)
       (let ([result *modal-buffer*]
             [cb *modal-callback*])
         (set! *modal-callback* #false)
         (close-modal!)
         (when cb (enqueue-thread-local-callback (lambda () (cb result))))
         event-result/consume)]
      [(key-event-escape? event)
       (set! *modal-callback* #false)
       (close-modal!)
       event-result/consume]
      [(key-event-left? event)
       (set! *modal-cursor* (max 0 (- *modal-cursor* 1)))
       event-result/consume]
      [(key-event-right? event)
       (set! *modal-cursor* (min (string-length *modal-buffer*) (+ *modal-cursor* 1)))
       event-result/consume]
      [(key-event-home? event)
       (set! *modal-cursor* 0)
       event-result/consume]
      [(key-event-end? event)
       (set! *modal-cursor* (string-length *modal-buffer*))
       event-result/consume]
      [(key-event-backspace? event)
       (when (> *modal-cursor* 0)
         (let ([before (substring *modal-buffer* 0 (- *modal-cursor* 1))]
               [after (substring *modal-buffer* *modal-cursor* (string-length *modal-buffer*))])
           (set! *modal-buffer* (string-append before after))
           (set! *modal-cursor* (- *modal-cursor* 1))))
       event-result/consume]
      [(key-event-delete? event)
       (let ([len (string-length *modal-buffer*)])
         (when (< *modal-cursor* len)
           (let ([before (substring *modal-buffer* 0 *modal-cursor*)]
                 [after (substring *modal-buffer* (+ *modal-cursor* 1) len)])
             (set! *modal-buffer* (string-append before after)))))
       event-result/consume]
      [(char? ch)
       (let ([before (substring *modal-buffer* 0 *modal-cursor*)]
             [after (substring *modal-buffer* *modal-cursor* (string-length *modal-buffer*))])
         (set! *modal-buffer* (string-append before (string ch) after))
         (set! *modal-cursor* (+ *modal-cursor* 1)))
       event-result/consume]
      [else event-result/consume])))

(define (oil-show-input-modal! title initial-value callback)
  (when *modal-open?*
    (close-modal!))
  (set! *modal-open?* #true)
  (set! *modal-mode* 'input)
  (set! *modal-title* title)
  (set! *modal-buffer* (or initial-value ""))
  (set! *modal-cursor* (string-length *modal-buffer*))
  (set! *modal-lines* '())
  (set! *modal-scroll* 0)
  (set! *modal-callback* callback)
  (push-component!
    (new-component! "oil-modal"
                    (OilModalState)
                    render-modal
                    (hash "handle_event" handle-modal-event
                          "cursor" modal-cursor))))

(define (oil-show-confirm-modal! title detail-lines callback)
  (when *modal-open?*
    (close-modal!))
  (set! *modal-open?* #true)
  (set! *modal-mode* 'confirm)
  (set! *modal-title* title)
  (set! *modal-buffer* "")
  (set! *modal-cursor* 0)
  (set! *modal-lines* (or detail-lines '()))
  (set! *modal-scroll* 0)
  (set! *modal-callback* callback)
  (push-component!
    (new-component! "oil-modal"
                    (OilModalState)
                    render-modal
                    (hash "handle_event" handle-modal-event
                          "cursor" modal-cursor))))

(define *help-lines*
  (list
    "─── Native Buffer Editing (Default) ───────────────────"
    "  Use normal Helix keys (i, a, o, c, dd, yp, s, u...)"
    "  Add line 'dir/' or 'sub/file.txt' -> create on save"
    "  Edit filename (supports multi-cursor) -> rename on save"
    "  Delete line -> trash/delete on save"
    ""
    "─── Navigation & Actions ──────────────────────────────"
    "  ret            Enter dir / open file (creates new dir)"
    "  -              Parent directory (lands on prev subdir)"
    "  gr             Git repository root (or cwd)"
    "  C-s / :oil-save    Apply pending buffer edits"
    "  C-r / :oil-refresh Reload directory"
    "  C-q / :oil-close   Close oil buffer"
    ""
    "─── Optional Commands ─────────────────────────────────"
    "  :oil-open-vsplit / :oil-open-hsplit / :oil-preview"
    "  :oil-toggle-hidden / :oil-toggle-git-ignored / :oil-toggle-metadata"
    "  :oil-cycle-sort / :oil-toggle-sort-reverse / :oil-toggle-subtree"
    "  :oil-toggle-wdired   Switch between native edit & Dired lock mode"))

(define (render-help _state rect frame)
  (let* ([term-w (area-width rect)]
         [term-h (area-height rect)]
         [w (clamp 64 (min 40 (- term-w 2)) (- term-w 4))]
         [max-vis (max 5 (- term-h 6))]
         [vis (min (length *help-lines*) max-vis)]
         [h (+ vis 4)]
         [x (max 0 (quotient (- term-w w) 2))]
         [y (max 0 (quotient (- term-h h) 2))]
         [bg-style (safe-theme-scope "ui.popup" "ui.background")]
         [border-style (safe-theme-scope "ui.window" "ui.text")]
         [title-style (style-with-bold (safe-theme-scope "ui.text.focus" "ui.text"))]
         [text-style (theme-scope-ref "ui.text")]
         [box-area (area x y w h)]
         [inner-w (max 1 (- w 2))])
    (buffer/clear-with frame box-area bg-style)
    (block/render frame box-area (make-block bg-style border-style "all" "rounded"))
    (frame-set-string! frame (+ x 1) (+ y 1) (truncate-str "oil.hx Keybindings (Dired / WDired)" inner-w) title-style)
    (let loop ([i 0])
      (when (< i vis)
        (let ([idx (+ *help-scroll* i)])
          (when (< idx (length *help-lines*))
            (let* ([line (list-ref *help-lines* idx)]
                   [st (if (starts-with? line "───") title-style text-style)])
              (frame-set-string! frame (+ x 1) (+ y 2 i) (truncate-str line inner-w) st))))
        (loop (+ i 1))))
    (frame-set-string! frame
                       (+ x 1)
                       (+ y h -2)
                       (truncate-str "j/k: scroll   q / Esc / ?: close" inner-w)
                       title-style)))

(define (close-help!)
  (set! *help-open?* #false)
  (pop-last-component-by-name! "oil-help"))

(define (handle-help-event _state event)
  (let ([ch (key-event-char event)])
    (cond
      [(mouse-event? event) event-result/consume]
      [(or (key-event-escape? event)
           (and (char? ch) (or (char=? ch #\q) (char=? ch #\?))))
       (close-help!)
       event-result/consume]
      [(or (key-event-down? event) (and (char? ch) (char=? ch #\j)))
       (set! *help-scroll* (min (max 0 (- (length *help-lines*) 8)) (+ *help-scroll* 1)))
       event-result/consume]
      [(or (key-event-up? event) (and (char? ch) (char=? ch #\k)))
       (set! *help-scroll* (max 0 (- *help-scroll* 1)))
       event-result/consume]
      [else event-result/consume])))

(define (oil-show-help-modal!)
  (if *help-open?*
      (close-help!)
      (begin
        (set! *help-open?* #true)
        (set! *help-scroll* 0)
        (push-component!
          (new-component! "oil-help"
                          (OilHelpState)
                          render-help
                          (hash "handle_event" handle-help-event
                                "cursor" (lambda (_s _r) #false)))))))

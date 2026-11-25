;;; oil.scm - Dired & WDired-inspired file manager for Helix + Steel

(require-builtin helix/core/keymaps as helix.keymaps.)

(require "helix/editor.scm")
(require "helix/keymaps.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require-builtin helix/core/text as text.)
(require (prefix-in helix. "helix/commands.scm"))
(require "notify/notify.scm")
(require "lib/core.scm")
(require "lib/ui.scm")

(provide oil
         oil-enter
         oil-open-vsplit
         oil-open-hsplit
         oil-preview
         oil-up
         oil-root
         oil-home
         oil-history-back
         oil-history-forward
         oil-refresh
         oil-save
         oil-save-close
         oil-close
         ow
         owq
         oq
         oil-toggle-hidden
         oil-toggle-git-ignored
         oil-toggle-metadata
         oil-cycle-sort
         oil-toggle-sort-reverse
         oil-toggle-subtree
         oil-mark
         oil-unmark
         oil-unmark-all
         oil-toggle-marks
         oil-mark-pattern
         oil-mkdir
         oil-create-file
         oil-rename
         oil-delete
         oil-wdired-enter
         oil-wdired-abort
         oil-toggle-wdired
         oil-help
         oil-configure!
         oil-install-keymaps!
         oil-yank
         oil-cut
         oil-paste
         oil-clipboard-clear
         oil-info
         oil-error
         OIL-BUFFER-NAME)

(define OIL-BUFFER-NAME "*oil*")

(struct OilEntry (rel display is-dir? depth) #:transparent)

;; Core buffer & view state
(define *oil-dir* #false)
(define *oil-doc-id* #false)
(define *oil-preview-doc-id* #false)
(define *oil-mode* 'wdired)         ; 'wdired (native editable buffer) | 'dired (read-only browse)
(define *oil-default-mode* 'wdired) ; 'wdired | 'dired
(define *oil-entries* '())         ; list of OilEntry
(define *oil-original* '())        ; list of rel-path strings
(define *oil-git-status* '())
(define *oil-metadata* '())
(define *oil-hint-ids* '())
(define *oil-rendering?* #false)
(define *oil-clamping?* #false)

;; Navigation & subtree state
(define *oil-expanded-dirs* '())
(define *oil-history-back* '())
(define *oil-history-forward* '())

;; Marks & clipboard state
(define *oil-marked-paths* '())
(define *oil-clipboard-op* #false) ; 'copy | 'move | #false
(define *oil-clipboard-paths* '())

;; Display & behavior configuration
(define *oil-show-hidden* #false)
(define *oil-show-git-ignored* #false)
(define *oil-show-metadata* #false)
(define *oil-sort-by* 'name)       ; 'name | 'mtime | 'size | 'ext
(define *oil-sort-reverse* #false)
(define *oil-use-trash* #true)
(define *oil-confirm-save* 'destructive) ; 'destructive | 'always | 'never
(define *oil-default-keymaps* #true)

;; Notifications
(define (oil-info msg)
  (notify msg #:title "oil.hx"))

(define (oil-error msg)
  (notify msg #:severity 'error #:title "oil.hx"))

;; Subprocess execution helpers
(define (run-cmd prog args)
  (let ([proc (~> (command prog args)
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let* ([child (Ok->value proc)]
               [stdout (read-port-to-string (child-stdout child))]
               [stderr (read-port-to-string (child-stderr child))]
               [status (wait child)]
               [code (if (Ok? status) (Ok->value status) -1)])
          (list (= code 0) code stdout stderr))
        (list #false -1 "" (string-append prog ": could not spawn process")))))

(define (run-cmd! prog args)
  (let* ([res (run-cmd prog args)]
         [ok? (list-ref res 0)]
         [code (list-ref res 1)]
         [stderr (trim (list-ref res 3))])
    (unless ok?
      (error (if (string=? stderr "")
                 (string-append prog " exited with status " (number->string code))
                 stderr)))
    (list-ref res 2)))

(define (command-exists? name)
  (list-ref (run-cmd "which" (list name)) 0))

(define (run-mv! from-path to-path)
  (run-cmd! "mv" (list "--" from-path to-path)))

(define (run-mkdir-p! path)
  (run-cmd! "mkdir" (list "-p" "--" path)))

(define (run-cp-r! src dest)
  (run-cmd! "cp" (list "-r" "--" src dest)))

;; Path & buffer helpers
(define (normalize-dir dir)
  (with-handler
    (lambda (_) dir)
    (let ([canonical (canonicalize-path dir)])
      (if (and (> (string-length canonical) 1)
               (ends-with? canonical (path-separator)))
          (trim-end-matches canonical (path-separator))
          canonical))))

(define (oil-buffer-alive?)
  (and *oil-doc-id* (editor-doc-exists? *oil-doc-id*)))

(define (oil-in-oil-buffer?)
  (and (oil-buffer-alive?)
       (equal? (editor->doc-id (editor-focus)) *oil-doc-id*)))

(define (full-path-for rel-entry)
  (path-join *oil-dir* (entry-raw-name rel-entry)))

(define (buffer-has-unsaved-edits?)
  (and (oil-buffer-alive?)
       (eq? *oil-mode* 'wdired)
       (let* ([text (text.rope->string (editor->text *oil-doc-id*))]
              [curr (filter (lambda (e) (not (string=? e "../")))
                            (parse-buffer-entries text))]
              [orig (filter (lambda (e) (not (string=? e "../")))
                            *oil-original*)])
         (not (equal? curr orig)))))

(define (guard-unsaved-edits! thunk)
  (if (buffer-has-unsaved-edits?)
      (oil-error "unsaved WDired edits: run :oil-save (C-s) or :oil-wdired-abort (C-q)")
      (thunk)))

;; Keymap management (Dired browse mode <-> WDired editable mode)
(define (build-dired-keymap)
  (let ([base (deep-copy-global-keybindings)]
        [dired-map
         (hash
           "normal"
           (hash
             ;; Navigation
             "ret" ":oil-enter"
             "l" ":oil-enter"
             "-" ":oil-up"
             "^" ":oil-up"
             "backspace" ":oil-up"
             "H" ":oil-history-back"
             "L" ":oil-history-forward"
             "~" ":oil-home"
             "g" (hash "r" ":oil-root")
             ;; Splits & preview
             "o" ":oil-open-vsplit"
             "O" ":oil-open-hsplit"
             "v" ":oil-preview"
             "tab" ":oil-toggle-subtree"
             ;; Marks & clipboard
             "m" ":oil-mark"
             "u" ":oil-unmark"
             "U" ":oil-unmark-all"
             "t" ":oil-toggle-marks"
             "%" ":oil-mark-pattern"
             "y" ":oil-yank"
             "C" ":oil-yank"
             "x" ":oil-cut"
             "p" ":oil-paste"
             "c" ":oil-clipboard-clear"
             ;; Direct file operations
             "+" ":oil-mkdir"
             "a" ":oil-create-file"
             "R" ":oil-rename"
             "D" ":oil-delete"
             "d" ":oil-delete"
             ;; View, sorting & modes
             "s" ":oil-cycle-sort"
             "S" ":oil-toggle-sort-reverse"
             "." ":oil-toggle-hidden"
             "I" ":oil-toggle-git-ignored"
             "(" ":oil-toggle-metadata"
             "M" ":oil-toggle-metadata"
             "r" ":oil-refresh"
             "E" ":oil-wdired-enter"
             "q" ":oil-close"
             "?" ":oil-help"
             ;; Block remaining mutating keys in browse mode
             "i" "no_op"
             "A" "no_op"
             "X" "no_op"
             "P" "no_op"
             "Y" "no_op"
             ">" "no_op"
             "<" "no_op"
             "=" "no_op"
             "J" "no_op"
             "&" "no_op"))])
    (merge-keybindings base dired-map)))

(define (build-wdired-keymap)
  (let ([base (deep-copy-global-keybindings)]
        [wdired-map
         (hash
           "normal"
           (hash
             "ret" ":oil-enter"
             "-" ":oil-up"
             "g" (hash "r" ":oil-root")
             "Z" (hash "Z" ":oil-save-close" "Q" ":oil-close")
             "C-s" ":oil-save"
             "C-r" ":oil-refresh"
             "C-q" ":oil-close"
             "?" ":oil-help"))])
    (merge-keybindings base wdired-map)))

(define (apply-mode-keymap! mode)
  (when *oil-default-keymaps*
    (set-global-buffer-or-extension-keymap
      (hash OIL-BUFFER-NAME
            (if (eq? mode 'wdired)
                (build-wdired-keymap)
                (build-dired-keymap))))))

;;@doc
;; Install keymaps for the current oil mode
(define (oil-install-keymaps!)
  (set! *oil-default-keymaps* #true)
  (apply-mode-keymap! *oil-mode*))

;; Git integration
(define (git-repo? dir)
  (let ([res (run-cmd "git" (list "-C" dir "rev-parse" "--is-inside-work-tree"))])
    (and (list-ref res 0)
         (string=? (trim (list-ref res 2)) "true"))))

(define (git-repo-root dir)
  (let ([res (run-cmd "git" (list "-C" dir "rev-parse" "--show-toplevel"))])
    (if (list-ref res 0)
        (let ([out (trim (list-ref res 2))])
          (if (string=? out "") #false out))
        #false)))

(define (parse-git-status-pairs dir)
  (let* ([root (git-repo-root dir)]
         [res (run-cmd "git" (list "-C" dir "status" "--porcelain" "--ignored=matching"))])
    (if (list-ref res 0)
        (let* ([output (list-ref res 2)]
               [lines (filter (lambda (l) (> (string-length l) 0))
                              (split-many output "\n"))]
               [prefix (git-relative-prefix dir root)])
          (filter (lambda (x) x)
                  (map (lambda (line)
                         (let ([pair (parse-git-line line)])
                           (and pair
                                (let* ([label (car pair)]
                                       [git-path (cdr pair)]
                                       [rel (cond
                                              [(string=? prefix "") git-path]
                                              [(starts-with? git-path prefix)
                                               (substring git-path (string-length prefix))]
                                              [else #false])])
                                  (and rel (cons label rel))))))
                       lines)))
        '())))

;; Cross-platform metadata (GNU stat -> BSD stat fallback)
(define (parse-gnu-stat-target n-field perms)
  (if (and (> (string-length perms) 0)
           (char=? (string-ref perms 0) #\l))
      (let ([parts (split-many n-field " -> ")])
        (if (>= (length parts) 2)
            (trim-end-matches (trim-start-matches (list-ref parts 1) "'") "'")
            #false))
      #false))

(define (short-mtime mtime-str)
  (if (>= (string-length mtime-str) 16)
      (substring mtime-str 0 16)
      mtime-str))

(define (read-entry-metadata! dir rel-entries)
  (let* ([rels (filter (lambda (e) (not (string=? e "../"))) rel-entries)]
         [paths (map (lambda (e) (path-join dir (entry-raw-name e))) rels)])
    (if (null? paths)
        (set! *oil-metadata* '())
        (let* ([gnu-args (cons "--printf=%n\t%A\t%h\t%U\t%s\t%y\t%N\n" (cons "--" paths))]
               [gnu-res (run-cmd "stat" gnu-args)])
          (if (list-ref gnu-res 0)
              (set! *oil-metadata* (parse-metadata-lines dir (list-ref gnu-res 2) #true))
              (let* ([bsd-args (cons "-f" (cons "%N\t%Sp\t%l\t%Su\t%z\t%Sm\t%Y" (cons "-t" (cons "%Y-%m-%d %H:%M" (cons "--" paths)))))]
                     [bsd-res (run-cmd "stat" bsd-args)])
                (if (list-ref bsd-res 0)
                    (set! *oil-metadata* (parse-metadata-lines dir (list-ref bsd-res 2) #false))
                    (set! *oil-metadata* '()))))))))

(define (rel-from-full dir full-path)
  (let ([prefix (if (ends-with? dir "/") dir (string-append dir "/"))])
    (if (starts-with? full-path prefix)
        (substring full-path (string-length prefix))
        (basename full-path))))

(define (parse-metadata-lines dir output gnu?)
  (let ([lines (filter (lambda (l) (> (string-length l) 0))
                       (split-many output "\n"))])
    (filter (lambda (x) x)
            (map (lambda (line)
                   (let ([parts (split-many line "\t")])
                     (if (>= (length parts) 6)
                         (let* ([full (list-ref parts 0)]
                                [rel (rel-from-full dir full)]
                                [perms (list-ref parts 1)]
                                [links (list-ref parts 2)]
                                [owner (list-ref parts 3)]
                                [size (list-ref parts 4)]
                                [mtime (short-mtime (list-ref parts 5))]
                                [n-field (if (>= (length parts) 7) (list-ref parts 6) "")]
                                [target (if gnu?
                                            (parse-gnu-stat-target n-field perms)
                                            #false)])
                           (cons rel (list perms links size owner mtime target)))
                         #false)))
                 lines))))

;; Directory reading with sorting & subtree expansion
(define (read-dir-names dir git-status include-parent?)
  (let* ([raw (with-handler
                (lambda (err)
                  (error (string-append "Cannot read directory: "
                                        (error-object-message err))))
                (read-dir dir))]
         [entries (map (lambda (p) (entry-display-name p (is-dir? p))) raw)]
         [vis-hidden (if *oil-show-hidden*
                         entries
                         (filter (lambda (e) (not (hidden-entry? e))) entries))]
         [visible (if *oil-show-git-ignored*
                      vis-hidden
                      (filter (lambda (e) (not (git-ignored-in? e git-status))) vis-hidden))])
    (if include-parent?
        (cons "../" visible)
        visible)))

(define (collect-entries-recursive dir rel-prefix depth git-status include-parent?)
  (let* ([names (read-dir-names dir git-status include-parent?)]
         [rel-names (map (lambda (n)
                           (if (string=? n "../")
                               "../"
                               (if (string=? rel-prefix "") n (string-append rel-prefix n))))
                         names)]
         ;; Fetch metadata before sorting when sorting by mtime/size
         [_ (when (and (or *oil-show-metadata*
                           (eq? *oil-sort-by* 'mtime)
                           (eq? *oil-sort-by* 'size))
                       (not (null? rel-names)))
              (read-entry-metadata! *oil-dir* rel-names))]
         [sorted-rels (sort-entries rel-names *oil-sort-by* *oil-sort-reverse* *oil-metadata*)])
    (let loop ([rest sorted-rels] [acc '()])
      (if (null? rest)
          (reverse acc)
          (let* ([rel (car rest)]
                 [is-parent? (string=? rel "../")]
                 [is-d? (entry-dir? rel)]
                 [disp-name (if is-parent? "../" (entry-display-name rel is-d?))]
                 [indent (make-string (* 2 depth) #\space)]
                 [display-str (if (eq? *oil-mode* 'wdired)
                                  rel
                                  (string-append indent disp-name))]
                 [item (OilEntry rel display-str is-d? depth)]
                 [raw-rel (entry-raw-name rel)]
                 [expanded? (and (not is-parent?)
                                 is-d?
                                 (not (eq? *oil-mode* 'wdired))
                                 (member raw-rel *oil-expanded-dirs*))])
            (if expanded?
                (let ([children (with-handler
                                  (lambda (_) '())
                                  (collect-entries-recursive
                                    (path-join *oil-dir* raw-rel)
                                    (string-append raw-rel "/")
                                    (+ depth 1)
                                    git-status
                                    #false))])
                  (loop (cdr rest) (append (reverse children) (cons item acc))))
                (loop (cdr rest) (cons item acc))))))))

;; Buffer rendering & hints
(define (make-header-line dir)
  (let* ([mode-tag (if (eq? *oil-mode* 'dired) " [Dired]" "")]
         [sort-tag (if (eq? *oil-sort-by* 'name)
                       (if *oil-sort-reverse* " [sort:name↓]" "")
                       (string-append " [sort:" (symbol->string *oil-sort-by*)
                                      (if *oil-sort-reverse* "↓" "") "]"))]
         [flags (string-append
                  (if *oil-show-hidden* " [+h]" "")
                  (if *oil-show-git-ignored* " [+i]" "")
                  (if *oil-show-metadata* " [+m]" ""))]
         [mark-tag (if (null? *oil-marked-paths*)
                       ""
                       (string-append " [marked:" (number->string (length *oil-marked-paths*)) "]"))]
         [clip-tag (if (and *oil-clipboard-op* (not (null? *oil-clipboard-paths*)))
                       (string-append " ["
                                      (if (eq? *oil-clipboard-op* 'copy) "copy:" "cut:")
                                      (number->string (length *oil-clipboard-paths*))
                                      "]")
                       "")])
    (string-append dir mode-tag sort-tag flags mark-tag clip-tag)))

(define (make-buffer-text dir entries)
  (let* ([header (make-header-line dir)]
         [lines (map OilEntry-display entries)])
    (string-join (cons header lines) "\n")))

(define (entry-line-number entries target-rel)
  (if (not target-rel)
      2
      (let loop ([es entries] [idx 0])
        (cond
          [(null? es) 2]
          [(string=? (OilEntry-rel (car es)) target-rel) (+ idx 2)]
          [else (loop (cdr es) (+ idx 1))]))))

(define (populate-oil-buffer! dir entries select-rel)
  (let ([content (make-buffer-text dir entries)]
        [target-line (entry-line-number entries select-rel)])
    (set! *oil-rendering?* #true)
    (with-handler
      (lambda (err)
        (set! *oil-rendering?* #false)
        (error err))
      (begin
        (if (oil-buffer-alive?)
            (begin
              (select_all)
              (replace-selection-with content)
              (collapse_selection))
            (insert_string content))
        (helix.goto-line target-line)))
    (set! *oil-rendering?* #false)))

(define (build-entry-hint-for rel)
  (if (string=? rel "../")
      #false
      (let* ([full (full-path-for rel)]
             [marked? (and (member full *oil-marked-paths*) #true)]
             [mark-str (if marked? "*" #false)]
             [git (entry-git-status-from rel *oil-git-status*)]
             [meta-pair (and *oil-show-metadata* (assoc (entry-raw-name rel) *oil-metadata*))]
             [meta (and meta-pair (format-metadata-hint (cdr meta-pair)))]
             [parts (filter (lambda (x) x) (list mark-str meta git))])
        (if (null? parts)
            #false
            (string-join parts "  ")))))

(define (line-end-char-index lines n)
  (let loop ([i 0] [pos 0])
    (if (= i n)
        (+ pos (string-length (list-ref lines i)))
        (loop (+ i 1) (+ pos (string-length (list-ref lines i)) 1)))))

(define (clear-oil-hints!)
  (for-each
    (lambda (id) (remove-inlay-hint-by-id (list-ref id 0) (list-ref id 1)))
    *oil-hint-ids*)
  (set! *oil-hint-ids* '()))

(define (apply-hints-with-resolver! resolver)
  (when (oil-buffer-alive?)
    (clear-oil-hints!)
    (let* ([text (text.rope->string (editor->text *oil-doc-id*))]
           [lines (split-many text "\n")]
           [max-len (list-max-length (if (> (length lines) 1) (cdr lines) '()))])
      (let loop ([i 1])
        (when (< i (length lines))
          (let* ([rel (resolver i (strip-cr (list-ref lines i)))]
                 [hint (and rel (build-entry-hint-for rel))])
            (when hint
              (let* ([line-len (string-length (list-ref lines i))]
                     [pad (make-string (max 0 (+ (- max-len line-len) 2)) #\space)]
                     [id (add-inlay-hint (line-end-char-index lines i)
                                         (string-append pad hint))])
                (set! *oil-hint-ids* (cons id *oil-hint-ids*)))))
          (loop (+ i 1)))))))

(define (apply-oil-hints! entries)
  (apply-hints-with-resolver!
    (lambda (line-idx _line-text)
      (let ([idx (- line-idx 1)])
        (if (and (>= idx 0) (< idx (length entries)))
            (OilEntry-rel (list-ref entries idx))
            #false)))))

(define (reapply-oil-hints!)
  (if (eq? *oil-mode* 'wdired)
      (apply-hints-with-resolver!
        (lambda (_line-idx line-text)
          (if (> (string-length line-text) 0) line-text #false)))
      (apply-oil-hints! *oil-entries*)))

;; Current line & entry resolution
(define (current-oil-entry)
  (and (oil-buffer-alive?)
       (let ([line-n (get-current-line-number)])
         (if (eq? *oil-mode* 'wdired)
             (let* ([text (text.rope->string (editor->text *oil-doc-id*))]
                    [lines (split-many text "\n")]
                    [raw (and (> line-n 0) (< line-n (length lines)) (strip-cr (list-ref lines line-n)))])
               (and raw
                    (not (string=? raw ""))
                    (not (string=? raw "../"))
                    (OilEntry raw raw (entry-dir? raw) 0)))
             (let ([idx (- line-n 1)])
               (and (>= idx 0)
                    (< idx (length *oil-entries*))
                    (let ([ent (list-ref *oil-entries* idx)])
                      (and (not (string=? (OilEntry-rel ent) "../"))
                           ent))))))))

(define (current-oil-line-raw)
  (and (oil-buffer-alive?)
       (let* ([line-n (get-current-line-number)]
              [text (text.rope->string (editor->text *oil-doc-id*))]
              [lines (split-many text "\n")])
         (and (> line-n 0)
              (< line-n (length lines))
              (strip-cr (list-ref lines line-n))))))

(define (current-selected-rel-for-restore)
  (let ([ent (current-oil-entry)])
    (and ent (OilEntry-rel ent))))

;; Pin *oil* to the first tab when moka.hx bufferline is active
(define (pin-oil-tab-first!)
  (with-handler
    (lambda (_) #false)
    (let ([n (length (editor-all-documents))])
      (let loop ([i 0])
        (when (< i n)
          (eval-string "(moka-bufferline-move-left!)")
          (loop (+ i 1)))))))

;; Core directory open/refresh
(define (open-oil-for-dir dir [select-rel #false] [record-history? #true])
  (let* ([canonical (normalize-dir dir)]
         [dir-changed? (and *oil-dir* (not (string=? *oil-dir* canonical)))])
    (when (and record-history? dir-changed?)
      (set! *oil-history-back* (cons *oil-dir* *oil-history-back*))
      (set! *oil-history-forward* '()))
    (when dir-changed?
      (set! *oil-expanded-dirs* '())
      (set! *oil-marked-paths* '()))
    (let* ([git-status (if (git-repo? canonical)
                           (parse-git-status-pairs canonical)
                           '())]
           [_ (set! *oil-dir* canonical)]
           [_ (set! *oil-git-status* git-status)]
           [entries (with-handler
                      (lambda (err)
                        (oil-error (string-append "oil: " (error-object-message err)))
                        (list (OilEntry "../" "../" #true 0)))
                      (collect-entries-recursive canonical "" 0 git-status #true))]
           [orig-rels (map OilEntry-rel entries)])
      (set! *oil-entries* entries)
      (set! *oil-original* orig-rels)
      (when (and *oil-show-metadata* (not (null? orig-rels)))
        (read-entry-metadata! canonical orig-rels))
      (apply-mode-keymap! *oil-mode*)
      (if (oil-buffer-alive?)
          (begin
            (editor-switch-action! *oil-doc-id* (Action/Replace))
            (populate-oil-buffer! canonical entries select-rel)
            (enqueue-thread-local-callback
              (lambda ()
                (pin-oil-tab-first!)
                (apply-oil-hints! entries))))
          (begin
            (helix.new)
            (enqueue-thread-local-callback
              (lambda ()
                (let* ([view-id (editor-focus)]
                       [doc-id (editor->doc-id view-id)])
                  (set! *oil-doc-id* doc-id)
                  (set-scratch-buffer-name! OIL-BUFFER-NAME)
                  (helix.keymaps.#%add-reverse-mapping (doc-id->usize doc-id) OIL-BUFFER-NAME)
                  (populate-oil-buffer! canonical entries select-rel)
                  (pin-oil-tab-first!)
                  (apply-oil-hints! entries)))))))))

;;@doc
;; Open oil file manager in optional directory (or current buffer's directory)
(define (oil . args)
  (let* ([explicit-dir (and (not (null? args)) (car args))]
         [doc-id (editor->doc-id (editor-focus))]
         [path (editor-document->path doc-id)]
         [select-name (and (not explicit-dir) path (basename path))]
         [dir (cond
                [explicit-dir explicit-dir]
                [path (parent-dir path)]
                [else (get-helix-cwd)])])
    (set! *oil-mode* *oil-default-mode*)
    (open-oil-for-dir dir select-name #false)))

;; File & directory operations
(define (move-to-trash! path)
  (cond
    [(command-exists? "trash-put")
     (run-cmd! "trash-put" (list "--" path))]
    [(command-exists? "gio")
     (run-cmd! "gio" (list "trash" "--" path))]
    [else
     (let* ([home (or (env-var "HOME") "/tmp")]
            [trash-files (path-join home ".local/share/Trash/files")]
            [_ (run-mkdir-p! trash-files)]
            [dest (let ([base (path-join trash-files (basename path))])
                    (if (path-exists? base)
                        (unique-copy-path base (is-dir? path) path-exists?)
                        base))])
       (run-mv! path dest))]))

(define (do-delete-path! path)
  (if *oil-use-trash*
      (move-to-trash! path)
      (if (is-dir? path)
          (with-handler
            (lambda (_) (run-cmd! "rm" (list "-rf" "--" path)))
            (delete-directory! path))
          (delete-file! path))))

(define (do-create-rel! rel)
  (let ([path (full-path-for rel)])
    (if (entry-dir? rel)
        (run-mkdir-p! path)
        (begin
          (run-mkdir-p! (parent-dir path))
          (call-with-output-file path (lambda (_p) #t))))))

;; Navigation & Opening commands
(define (open-current-with open-fn)
  (unless (oil-buffer-alive?)
    (oil-error "no active oil buffer"))
  (when (oil-buffer-alive?)
    (let ([line-n (get-current-line-number)])
      (when (> line-n 0)
        (let ([raw (current-oil-line-raw)])
          (cond
            [(not raw) #t]
            [(string=? raw "../")
             (guard-unsaved-edits!
               (lambda ()
                 (let ([child-name (string-append (basename *oil-dir*) "/")])
                   (open-oil-for-dir (parent-dir *oil-dir*) child-name))))]
            [else
             (let* ([ent (current-oil-entry)]
                    [rel (if ent (OilEntry-rel ent) raw)]
                    [path (full-path-for rel)])
               (cond
                 [(is-dir? path)
                  (guard-unsaved-edits!
                    (lambda () (open-oil-for-dir path)))]
                 [(and (entry-dir? rel)
                       (not (path-exists? path))
                       (not (member rel *oil-original*)))
                  (with-handler
                    (lambda (err)
                      (oil-error (string-append "cannot create directory: "
                                                (error-object-message err))))
                    (begin
                      (run-mkdir-p! path)
                      (open-oil-for-dir path)))]
                 [(path-exists? path)
                  (open-fn path)]
                 [else
                  (oil-error (string-append "cannot open missing entry: " path))]))]))))))

;;@doc
;; Enter directory or open file under cursor
(define (oil-enter)
  (open-current-with (lambda (p) (helix.open p))))

;;@doc
;; Open file under cursor in a vertical split (or enter directory)
(define (oil-open-vsplit)
  (open-current-with (lambda (p) (helix.vsplit p))))

;;@doc
;; Open file under cursor in a horizontal split (or enter directory)
(define (oil-open-hsplit)
  (open-current-with (lambda (p) (helix.hsplit p))))

;;@doc
;; Preview file in another split while keeping focus in oil buffer
(define (oil-preview)
  (unless (oil-buffer-alive?)
    (oil-error "no active oil buffer"))
  (when (oil-buffer-alive?)
    (let ([ent (current-oil-entry)])
      (if (not ent)
          (oil-error "no entry under cursor")
          (let ([path (full-path-for (OilEntry-rel ent))])
            (if (is-dir? path)
                (oil-toggle-subtree)
                (if (path-exists? path)
                    (let ([oil-view (editor-focus)]
                          [existing-view (and *oil-preview-doc-id*
                                              (editor-doc-exists? *oil-preview-doc-id*)
                                              (editor-doc-in-view? *oil-preview-doc-id*))])
                      (if existing-view
                          (begin
                            (editor-set-focus! existing-view)
                            (helix.open path))
                          (helix.vsplit path))
                      (set! *oil-preview-doc-id* (editor->doc-id (editor-focus)))
                      (editor-set-focus! oil-view))
                    (oil-error (string-append "missing file: " path)))))))))

;;@doc
;; Go to parent directory (cursor lands on previous subdirectory)
(define (oil-up)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (let ([child-name (string-append (basename *oil-dir*) "/")])
            (open-oil-for-dir (parent-dir *oil-dir*) child-name))))
      (oil-error "no active oil buffer")))

;;@doc
;; Go back in directory navigation history
(define (oil-history-back)
  (if (null? *oil-history-back*)
      (oil-info "history: at oldest directory")
      (guard-unsaved-edits!
        (lambda ()
          (let ([prev (car *oil-history-back*)])
            (set! *oil-history-back* (cdr *oil-history-back*))
            (when *oil-dir*
              (set! *oil-history-forward* (cons *oil-dir* *oil-history-forward*)))
            (open-oil-for-dir prev #false #false))))))

;;@doc
;; Go forward in directory navigation history
(define (oil-history-forward)
  (if (null? *oil-history-forward*)
      (oil-info "history: at newest directory")
      (guard-unsaved-edits!
        (lambda ()
          (let ([nxt (car *oil-history-forward*)])
            (set! *oil-history-forward* (cdr *oil-history-forward*))
            (when *oil-dir*
              (set! *oil-history-back* (cons *oil-dir* *oil-history-back*)))
            (open-oil-for-dir nxt #false #false))))))

;;@doc
;; Refresh oil buffer, preserving cursor line
(define (oil-refresh)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)))
      (oil-error "no active oil buffer")))

;;@doc
;; Jump to the git repository root, or helix cwd if not in a repo
(define (oil-root)
  (guard-unsaved-edits!
    (lambda ()
      (let ([root (and *oil-dir* (git-repo-root *oil-dir*))])
        (open-oil-for-dir (or root (get-helix-cwd)))))))

;;@doc
;; Jump to user's HOME directory
(define (oil-home)
  (guard-unsaved-edits!
    (lambda ()
      (let ([home (env-var "HOME")])
        (if home
            (open-oil-for-dir home)
            (oil-error "HOME environment variable is not set"))))))

;; Subtree expand/collapse
;;@doc
;; Toggle inline subtree expansion for directory under cursor
(define (oil-toggle-subtree)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (if (eq? *oil-mode* 'wdired)
          (oil-error "subtree toggle is disabled in WDired mode")
          (let ([ent (current-oil-entry)])
            (if (and ent (OilEntry-is-dir? ent))
                (let* ([raw-rel (entry-raw-name (OilEntry-rel ent))]
                       [was-expanded? (member raw-rel *oil-expanded-dirs*)])
                  (if was-expanded?
                      (set! *oil-expanded-dirs*
                            (filter (lambda (d)
                                      (and (not (string=? d raw-rel))
                                           (not (starts-with? d (string-append raw-rel "/")))))
                                    *oil-expanded-dirs*))
                      (set! *oil-expanded-dirs* (cons raw-rel *oil-expanded-dirs*)))
                  (open-oil-for-dir *oil-dir* (OilEntry-rel ent) #false))
                (oil-error "not a directory under cursor"))))))

;; Visibility & sorting toggles
;;@doc
;; Toggle visibility of hidden (dot) files and directories
(define (oil-toggle-hidden)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (set! *oil-show-hidden* (not *oil-show-hidden*))
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
          (oil-info (if *oil-show-hidden* "oil: showing dotfiles" "oil: hiding dotfiles"))))
      (oil-error "no active oil buffer")))

;;@doc
;; Toggle visibility of git-ignored files and directories
(define (oil-toggle-git-ignored)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (set! *oil-show-git-ignored* (not *oil-show-git-ignored*))
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
          (oil-info (if *oil-show-git-ignored*
                        "oil: showing git-ignored files"
                        "oil: hiding git-ignored files"))))
      (oil-error "no active oil buffer")))

;;@doc
;; Toggle display of file permissions, links, owner, size, and mtime as inlay hints
(define (oil-toggle-metadata)
  (if (oil-buffer-alive?)
      (begin
        (set! *oil-show-metadata* (not *oil-show-metadata*))
        (when *oil-show-metadata*
          (read-entry-metadata! *oil-dir* *oil-original*))
        (populate-oil-buffer! *oil-dir* *oil-entries* (current-selected-rel-for-restore))
        (enqueue-thread-local-callback reapply-oil-hints!)
        (oil-info (if *oil-show-metadata*
                      "oil: showing metadata"
                      "oil: hiding metadata")))
      (oil-error "no active oil buffer")))

;;@doc
;; Cycle sort order: name -> mtime -> size -> ext
(define (oil-cycle-sort)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (set! *oil-sort-by*
                (cond
                  [(eq? *oil-sort-by* 'name) 'mtime]
                  [(eq? *oil-sort-by* 'mtime) 'size]
                  [(eq? *oil-sort-by* 'size) 'ext]
                  [else 'name]))
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
          (oil-info (string-append "sort: " (symbol->string *oil-sort-by*)))))
      (oil-error "no active oil buffer")))

;;@doc
;; Toggle reverse sort order
(define (oil-toggle-sort-reverse)
  (if *oil-dir*
      (guard-unsaved-edits!
        (lambda ()
          (set! *oil-sort-reverse* (not *oil-sort-reverse*))
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
          (oil-info (if *oil-sort-reverse* "sort: descending" "sort: ascending"))))
      (oil-error "no active oil buffer")))

;; Marks (Dired style)
(define (advance-cursor-line!)
  (let ([cur (get-current-line-number)]
        [max-line (length *oil-entries*)])
    (when (< cur max-line)
      (helix.goto-line (+ cur 2)))))

(define (refresh-marks-ui!)
  (when (oil-buffer-alive?)
    (let ([sel (current-selected-rel-for-restore)])
      (populate-oil-buffer! *oil-dir* *oil-entries* sel)
      (enqueue-thread-local-callback reapply-oil-hints!))))

;;@doc
;; Mark entry under cursor and move down
(define (oil-mark)
  (let ([ent (current-oil-entry)])
    (if ent
        (let ([full (full-path-for (OilEntry-rel ent))])
          (unless (member full *oil-marked-paths*)
            (set! *oil-marked-paths* (cons full *oil-marked-paths*)))
          (refresh-marks-ui!)
          (advance-cursor-line!))
        (advance-cursor-line!))))

;;@doc
;; Unmark entry under cursor and move down
(define (oil-unmark)
  (let ([ent (current-oil-entry)])
    (if ent
        (let ([full (full-path-for (OilEntry-rel ent))])
          (set! *oil-marked-paths* (filter (lambda (p) (not (string=? p full))) *oil-marked-paths*))
          (refresh-marks-ui!)
          (advance-cursor-line!))
        (advance-cursor-line!))))

;;@doc
;; Unmark all marked entries
(define (oil-unmark-all)
  (set! *oil-marked-paths* '())
  (refresh-marks-ui!)
  (oil-info "all marks cleared"))

;;@doc
;; Invert marks across all entries in current buffer
(define (oil-toggle-marks)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (let* ([candidates (filter (lambda (e) (not (string=? (OilEntry-rel e) "../"))) *oil-entries*)]
             [new-marks
              (filter (lambda (x) x)
                      (map (lambda (e)
                             (let ([full (full-path-for (OilEntry-rel e))])
                               (if (member full *oil-marked-paths*) #false full)))
                           candidates))])
        (set! *oil-marked-paths* new-marks)
        (refresh-marks-ui!)
        (oil-info (string-append "marked " (number->string (length new-marks)) " entries")))))

;;@doc
;; Prompt for a glob/substring pattern to mark matching entries (e.g. *.scm)
(define (oil-mark-pattern)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (oil-show-input-modal!
        "Mark pattern (e.g. *.c): "
        ""
        (lambda (pat)
          (when (and pat (> (string-length pat) 0))
            (let* ([matches
                    (filter (lambda (e)
                              (and (not (string=? (OilEntry-rel e) "../"))
                                   (glob-match? pat (basename (entry-raw-name (OilEntry-rel e))))))
                            *oil-entries*)]
                   [paths (map (lambda (e) (full-path-for (OilEntry-rel e))) matches)])
              (for-each (lambda (p)
                          (unless (member p *oil-marked-paths*)
                            (set! *oil-marked-paths* (cons p *oil-marked-paths*))))
                        paths)
              (refresh-marks-ui!)
              (oil-info (string-append "marked " (number->string (length paths)) " matching '" pat "'"))))))))

;; Target paths helper: uses marks if any, else entry under cursor
(define (target-paths-or-current)
  (if (not (null? *oil-marked-paths*))
      (reverse *oil-marked-paths*)
      (let ([ent (current-oil-entry)])
        (if ent (list (full-path-for (OilEntry-rel ent))) '()))))

;; Clipboard operations (multi-file aware)
;;@doc
;; Mark selected/current entries for copying
(define (oil-yank)
  (let ([paths (target-paths-or-current)])
    (if (null? paths)
        (oil-error "no entry marked or under cursor")
        (begin
          (set! *oil-clipboard-op* 'copy)
          (set! *oil-clipboard-paths* paths)
          (refresh-marks-ui!)
          (oil-info (string-append "yanked "
                                   (if (= (length paths) 1)
                                       (basename (car paths))
                                       (string-append (number->string (length paths)) " items"))))))))

;;@doc
;; Mark selected/current entries for moving
(define (oil-cut)
  (let ([paths (target-paths-or-current)])
    (if (null? paths)
        (oil-error "no entry marked or under cursor")
        (begin
          (set! *oil-clipboard-op* 'move)
          (set! *oil-clipboard-paths* paths)
          (refresh-marks-ui!)
          (oil-info (string-append "cut "
                                   (if (= (length paths) 1)
                                       (basename (car paths))
                                       (string-append (number->string (length paths)) " items"))))))))

;;@doc
;; Paste clipboard items into the current oil directory with conflict protection
(define (oil-paste)
  (cond
    [(not (oil-buffer-alive?))
     (oil-error "no active oil buffer")]
    [(null? *oil-clipboard-paths*)
     (oil-error "clipboard is empty")]
    [else
     (let ([op *oil-clipboard-op*]
           [errors '()]
           [count 0])
       (for-each
         (lambda (src)
           (with-handler
             (lambda (err)
               (set! errors (cons (string-append (basename src) ": " (error-object-message err))
                                  errors)))
             (let* ([name (basename src)]
                    [dest (path-join *oil-dir* name)])
               (cond
                 [(eq? op 'copy)
                  (let ([final-dest (if (path-exists? dest)
                                        (unique-copy-path dest (is-dir? src) path-exists?)
                                        dest)])
                    (run-cp-r! src final-dest)
                    (set! count (+ count 1)))]
                 [(eq? op 'move)
                  (if (string=? src dest)
                      (error "source and destination are identical")
                      (if (path-exists? dest)
                          (error (string-append "destination already exists: " name))
                          (begin
                            (run-mv! src dest)
                            (set! count (+ count 1)))))]))))
         *oil-clipboard-paths*)
       (when (and (eq? op 'move) (null? errors))
         (set! *oil-clipboard-op* #false)
         (set! *oil-clipboard-paths* '()))
       (set! *oil-marked-paths* '())
       (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
       (if (null? errors)
           (oil-info (string-append (if (eq? op 'copy) "copied " "moved ")
                                    (number->string count) " item(s) -> " *oil-dir*))
           (oil-error (string-join (reverse errors) " | "))))]))

;;@doc
;; Clear the clipboard
(define (oil-clipboard-clear)
  (set! *oil-clipboard-op* #false)
  (set! *oil-clipboard-paths* '())
  (refresh-marks-ui!)
  (oil-info "clipboard cleared"))

;; Direct Dired file actions (modals)
;;@doc
;; Prompt to create a new directory
(define (oil-mkdir)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (oil-show-input-modal!
        "New directory: "
        ""
        (lambda (name)
          (let ([clean (trim-end-matches (trim name) "/")])
            (when (> (string-length clean) 0)
              (with-handler
                (lambda (err) (oil-error (string-append "mkdir failed: " (error-object-message err))))
                (let ([target (path-join *oil-dir* clean)])
                  (run-mkdir-p! target)
                  (open-oil-for-dir *oil-dir* (string-append clean "/") #false)
                  (oil-info (string-append "created directory " clean "/"))))))))))

;;@doc
;; Prompt to create a new file and open it (paths ending in '/' create a directory)
(define (oil-create-file)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (oil-show-input-modal!
        "New file (end with / for dir): "
        ""
        (lambda (name)
          (when (> (string-length name) 0)
            (with-handler
              (lambda (err) (oil-error (string-append "create failed: " (error-object-message err))))
              (begin
                (do-create-rel! name)
                (if (entry-dir? name)
                    (open-oil-for-dir *oil-dir* name #false)
                    (helix.open (full-path-for name)))
                (oil-info (string-append "created " name)))))))))

;;@doc
;; Prompt to rename the entry under the cursor
(define (oil-rename)
  (let ([ent (current-oil-entry)])
    (if (not ent)
        (oil-error "no entry under cursor")
        (let* ([old-rel (OilEntry-rel ent)]
               [old-raw (entry-raw-name old-rel)]
               [old-full (full-path-for old-rel)])
          (oil-show-input-modal!
            "Rename to: "
            old-raw
            (lambda (new-raw)
              (let ([clean-new (trim-end-matches new-raw "/")])
                (when (and (> (string-length clean-new) 0)
                           (not (string=? clean-new old-raw)))
                  (let ([new-full (path-join *oil-dir* clean-new)])
                    (if (path-exists? new-full)
                        (oil-error (string-append "destination already exists: " clean-new))
                        (with-handler
                          (lambda (err) (oil-error (string-append "rename failed: " (error-object-message err))))
                          (begin
                            (run-mkdir-p! (parent-dir new-full))
                            (run-mv! old-full new-full)
                            (let ([new-rel (if (OilEntry-is-dir? ent) (string-append clean-new "/") clean-new)])
                              (open-oil-for-dir *oil-dir* new-rel #false))
                            (oil-info (string-append "renamed " old-raw " -> " clean-new))))))))))))))

;;@doc
;; Delete marked entries or the entry under cursor (with confirmation)
(define (oil-delete)
  (let ([paths (target-paths-or-current)])
    (if (null? paths)
        (oil-error "no entry marked or under cursor")
        (let* ([verb (if *oil-use-trash* "Trash" "Permanently delete")]
               [title (string-append verb " " (number->string (length paths)) " item(s)?")]
               [details (map (lambda (p) (string-append "  DELETE  " (rel-from-full *oil-dir* p))) paths)])
          (oil-show-confirm-modal!
            title
            details
            (lambda (confirmed?)
              (when confirmed?
                (let ([errors '()]
                      [deleted 0])
                  (for-each
                    (lambda (p)
                      (with-handler
                        (lambda (err)
                          (set! errors (cons (string-append (basename p) ": " (error-object-message err))
                                             errors)))
                        (begin
                          (do-delete-path! p)
                          (set! deleted (+ deleted 1)))))
                    paths)
                  (set! *oil-marked-paths* '())
                  (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
                  (if (null? errors)
                      (oil-info (string-append (if *oil-use-trash* "trashed " "deleted ")
                                               (number->string deleted) " item(s)"))
                      (oil-error (string-join (reverse errors) " | ")))))))))))

;; WDired mode (editable filesystem buffer)
;;@doc
;; Enter WDired mode to edit filenames directly in the buffer
(define (oil-wdired-enter)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (if (eq? *oil-mode* 'wdired)
          (oil-info "already in WDired mode")
          (let ([sel (current-selected-rel-for-restore)])
            (set! *oil-mode* 'wdired)
            (open-oil-for-dir *oil-dir* sel #false)
            (oil-info "WDired mode: edit buffer, then :oil-save (C-s) or :oil-wdired-abort (C-q)")))))

;;@doc
;; Abort WDired edits and restore the Dired browse view
(define (oil-wdired-abort)
  (if (not (oil-buffer-alive?))
      (oil-error "no active oil buffer")
      (let ([sel (current-selected-rel-for-restore)])
        (set! *oil-mode* *oil-default-mode*)
        (open-oil-for-dir *oil-dir* sel #false)
        (oil-info "WDired edits discarded"))))

;;@doc
;; Toggle between Dired browse mode and WDired editable mode
(define (oil-toggle-wdired)
  (if (eq? *oil-mode* 'wdired)
      (if (buffer-has-unsaved-edits?)
          (oil-show-confirm-modal!
            "Discard unsaved WDired edits?"
            '()
            (lambda (ok?) (when ok? (oil-wdired-abort))))
          (oil-wdired-abort))
      (oil-wdired-enter)))

;; Executing WDired save plan
(define (build-save-summary renames to-delete to-create)
  (let* ([rename-lines (map (lambda (p) (string-append "Renamed " (car p) " -> " (cdr p))) renames)]
         [delete-lines (map (lambda (name) (string-append "Deleted " name)) to-delete)]
         [create-lines (map (lambda (name) (string-append "Created " name)) to-create)])
    (string-join (append rename-lines delete-lines create-lines) "\n")))

(define (execute-wdired-plan! renames to-delete to-create [close-after? #false])
  (let ([errors '()]
        [staged-renames '()])
    (define (try! label thunk)
      (with-handler
        (lambda (err)
          (set! errors (cons (string-append label ": " (error-object-message err)) errors))
          #false)
        (begin (thunk) #true)))

    ;; Validate destination collisions against unrenamed disk entries (e.g. hidden files)
    (let* ([rename-sources (map car renames)]
           [valid-renames
            (filter
              (lambda (pair)
                (let* ([dst-rel (cdr pair)]
                       [dst-path (full-path-for dst-rel)])
                  (if (and (path-exists? dst-path) (not (member dst-rel rename-sources)))
                      (begin
                        (set! errors (cons (string-append "rename " (car pair) " -> " dst-rel ": destination exists")
                                           errors))
                        #false)
                      #true)))
              renames)])
      ;; Phase 1: move sources to unique temp names
      (for-each
        (lambda (pair)
          (let* ([src-path (full-path-for (car pair))]
                 [tmp-path (string-append src-path ".~oil-tmp~")])
            (when (try! (string-append "stage " (car pair))
                        (lambda () (run-mv! src-path tmp-path)))
              (set! staged-renames (cons (list (car pair) (cdr pair) tmp-path) staged-renames)))))
        valid-renames)

      ;; Phase 2: move successfully staged temp names to final destinations
      (for-each
        (lambda (item)
          (let* ([src-rel (list-ref item 0)]
                 [dst-rel (list-ref item 1)]
                 [tmp-path (list-ref item 2)]
                 [dst-path (full-path-for dst-rel)])
            (try! (string-append "rename " src-rel " -> " dst-rel)
                  (lambda ()
                    (run-mkdir-p! (parent-dir dst-path))
                    (run-mv! tmp-path dst-path)))))
        (reverse staged-renames)))

    ;; Delete
    (for-each
      (lambda (name)
        (try! (string-append "delete " name)
              (lambda () (do-delete-path! (full-path-for name)))))
      to-delete)

    ;; Create
    (for-each
      (lambda (name)
        (try! (string-append "create " name)
              (lambda () (do-create-rel! name))))
      to-create)

    (set! *oil-mode* *oil-default-mode*)
    (if (and close-after? (null? errors))
        (begin
          (oil-info (build-save-summary renames to-delete to-create))
          (helix.buffer-close!))
        (begin
          (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
          (if (null? errors)
              (oil-info (build-save-summary renames to-delete to-create))
              (oil-error (string-append "errors: " (string-join (reverse errors) " | "))))))))

(define (needs-save-confirmation? renames to-delete to-create)
  (cond
    [(eq? *oil-confirm-save* 'never) #false]
    [(eq? *oil-confirm-save* 'always)
     (> (+ (length renames) (length to-delete) (length to-create)) 0)]
    [else
     (not (null? to-delete))]))

(define (save-oil-impl close-after?)
  (unless (oil-buffer-alive?)
    (oil-error "no active oil buffer; run :oil first"))
  (when (oil-buffer-alive?)
    (let* ([text (text.rope->string (editor->text *oil-doc-id*))]
           [curr (parse-buffer-entries text)]
           [plan (plan-wdired-changes *oil-original* curr)])
      (if (eq? (car plan) 'error)
          (oil-error (list-ref plan 1))
          (let ([renames (list-ref plan 1)]
                [to-delete (list-ref plan 2)]
                [to-create (list-ref plan 3)])
            (if (= (+ (length renames) (length to-delete) (length to-create)) 0)
                (begin
                  (set! *oil-mode* *oil-default-mode*)
                  (if close-after?
                      (helix.buffer-close!)
                      (begin
                        (open-oil-for-dir *oil-dir* (current-selected-rel-for-restore) #false)
                        (oil-info "nothing to do"))))
                (if (needs-save-confirmation? renames to-delete to-create)
                    (oil-show-confirm-modal!
                      (string-append "Apply "
                                     (number->string (+ (length renames) (length to-delete) (length to-create)))
                                     " filesystem change(s)?")
                      (format-save-preview-lines renames to-delete to-create)
                      (lambda (ok?)
                        (when ok?
                          (execute-wdired-plan! renames to-delete to-create close-after?))))
                    (execute-wdired-plan! renames to-delete to-create close-after?))))))))

;;@doc
;; Save pending WDired buffer changes to the filesystem
(define (oil-save)
  (save-oil-impl #false))

;;@doc
;; Save pending changes and close the oil buffer
(define (oil-save-close)
  (save-oil-impl #true))

;;@doc
;; Short alias for :oil-save
(define (ow)
  (oil-save))

;;@doc
;; Short alias for :oil-save-close
(define (owq)
  (oil-save-close))

;;@doc
;; Short alias for :oil-close
(define (oq)
  (oil-close))

;;@doc
;; Close the oil buffer
(define (oil-close)
  (guard-unsaved-edits!
    (lambda ()
      (helix.buffer-close!))))

;;@doc
;; Show keybinding help popup
(define (oil-help)
  (oil-show-help-modal!))

(define (oil-keyword? x)
  (and (symbol? x) (starts-with? (symbol->string x) "#:")))

;;@doc
;; Configure oil.hx. Supports both legacy positional (show-hidden show-git-ignored)
;; and keyword options: #:show-hidden #:show-git-ignored #:show-metadata #:mode
;; #:sort-by #:sort-reverse #:use-trash #:confirm-save #:default-keymaps
(define (oil-configure! . args)
  (if (and (not (null? args)) (not (oil-keyword? (car args))))
      (begin
        (when (>= (length args) 1) (set! *oil-show-hidden* (list-ref args 0)))
        (when (>= (length args) 2) (set! *oil-show-git-ignored* (list-ref args 1))))
      (let loop ([rest args])
        (when (>= (length rest) 2)
          (let ([k (list-ref rest 0)]
                [v (list-ref rest 1)])
            (cond
              [(equal? k '#:show-hidden) (set! *oil-show-hidden* v)]
              [(equal? k '#:show-git-ignored) (set! *oil-show-git-ignored* v)]
              [(equal? k '#:show-metadata) (set! *oil-show-metadata* v)]
              [(equal? k '#:mode)
               (set! *oil-default-mode* v)
               (set! *oil-mode* v)]
              [(equal? k '#:sort-by) (set! *oil-sort-by* v)]
              [(equal? k '#:sort-reverse) (set! *oil-sort-reverse* v)]
              [(equal? k '#:use-trash) (set! *oil-use-trash* v)]
              [(equal? k '#:confirm-save) (set! *oil-confirm-save* v)]
              [(equal? k '#:default-keymaps) (set! *oil-default-keymaps* v)]
              [else (oil-error "unknown oil-configure! option")])
            (loop (cddr rest))))))
  (when *oil-default-keymaps*
    (apply-mode-keymap! *oil-mode*)))

;; Reapply inlay hints when buffer edits occur in WDired mode or metadata/git/marks are active
(register-hook 'document-changed
  (lambda (doc-id _old-text)
    (when (and (not *oil-rendering?*)
               (oil-buffer-alive?)
               (equal? doc-id *oil-doc-id*)
               (or *oil-show-metadata*
                   (not (null? *oil-git-status*))
                   (not (null? *oil-marked-paths*))))
      (enqueue-thread-local-callback reapply-oil-hints!))))

;; Keep normal-mode collapsed cursor before end-of-line inlay hints (metadata/git)
(register-hook 'selection-did-change
  (lambda (view-id)
    (when (and (not *oil-rendering?*)
               (not *oil-clamping?*)
               (oil-buffer-alive?)
               (equal? (editor->doc-id view-id) *oil-doc-id*)
               (equal? (editor-mode) (string->editor-mode "normal")))
      (let* ([sel (current-selection-object)]
             [ranges (selection->ranges sel)])
        (when (= (length ranges) 1)
          (let ([r (car ranges)])
            (when (= (range-anchor r) (range-head r))
              (let* ([line-n (get-current-line-number)]
                     [col (get-current-column-number)]
                     [text (text.rope->string (editor->text *oil-doc-id*))]
                     [lines (split-many text "\n")])
                (when (and (> line-n 0) (< line-n (length lines)))
                  (let ([line-len (string-length (strip-cr (list-ref lines line-n)))])
                    (when (and (> line-len 0) (>= col line-len))
                      (set! *oil-clamping?* #true)
                      (with-handler
                        (lambda (_) #false)
                        (move_char_left))
                      (set! *oil-clamping?* #false))))))))))))

;; Install default keymaps at load time
(when *oil-default-keymaps*
  (apply-mode-keymap! *oil-mode*))

;;; tests/test-core.scm - Unit tests for lib/core.scm

(require "../lib/core.scm")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (assert-eq label expected actual)
  (if (equal? expected actual)
      (set! *tests-passed* (+ *tests-passed* 1))
      (begin
        (set! *tests-failed* (+ *tests-failed* 1))
        (displayln (string-append "FAIL [" label "]: expected "
                                  (to-string expected) ", got " (to-string actual))))))

(define (assert-true label val)
  (assert-eq label #true (if val #true #false)))

(define (assert-false label val)
  (assert-eq label #false (if val #true #false)))

;; Path and entry helpers
(assert-eq "path-join regular" "/a/b/c.txt" (path-join "/a/b" "c.txt"))
(assert-eq "path-join root" "/etc" (path-join "/" "etc"))
(assert-eq "basename file" "c.txt" (basename "/a/b/c.txt"))
(assert-eq "basename dir" "b" (basename "/a/b/"))
(assert-eq "entry-display-name dir" "sub/" (entry-display-name "/a/sub" #true))
(assert-eq "entry-display-name broken-link" "broken" (entry-display-name "/a/broken" #false))
(assert-true "hidden dotfile" (hidden-entry? ".gitignore"))
(assert-false "parent not hidden" (hidden-entry? "../"))
(assert-false "normal file not hidden" (hidden-entry? "README.md"))
(assert-eq "entry-extension" "scm" (entry-extension "oil.SCM"))
(assert-eq "entry-extension none" "" (entry-extension "Makefile"))

;; Unique copy naming
(let ([existing '("/d/foo.txt" "/d/foo.txt (copy)" "/d/foo.txt (copy 2)")])
  (assert-eq "unique-copy-path increments"
             "/d/foo.txt (copy 3)"
             (unique-copy-path "/d/foo.txt" #false (lambda (p) (and (member p existing) #true)))))
(assert-eq "unique-copy-path dir"
           "/d/sub (copy)"
           (unique-copy-path "/d/sub/" #true (lambda (_) #false)))

;; Buffer parsing (preserves intentional spaces, drops empty lines and CR)
(assert-eq "parse-buffer-entries keeps spaces"
           '("../" " spaced.txt" "dir/")
           (parse-buffer-entries "/tmp [Dired]\r\n../\r\n spaced.txt\r\n\r\ndir/\r\n"))

;; Sorting
(let ([entries '("../" "b.txt" "a.txt" "zdir/" "adir/")]
      [meta '(("a.txt" "rw-r--r--" "1" "100" "u" "2026-01-01 10:00" #f)
              ("b.txt" "rw-r--r--" "1" "500" "u" "2026-02-01 10:00" #f))])
  (assert-eq "sort name dirs first"
             '("../" "adir/" "zdir/" "a.txt" "b.txt")
             (sort-entries entries 'name #false meta))
  (assert-eq "sort size descending"
             '("../" "adir/" "zdir/" "b.txt" "a.txt")
             (sort-entries entries 'size #false meta))
  (assert-eq "sort mtime descending"
             '("../" "adir/" "zdir/" "b.txt" "a.txt")
             (sort-entries entries 'mtime #false meta))
  (assert-eq "sort reverse"
             '("../" "zdir/" "adir/" "b.txt" "a.txt")
             (sort-entries entries 'name #true meta)))

;; Git porcelain parsing & prefix
(assert-eq "parse-git-line quoted path"
           '(" ?" . "dir/hello \"world\".txt")
           (parse-git-line "?? \"dir/hello \\\"world\\\".txt\""))
(assert-eq "parse-git-line rename"
           '(" →" . "new.scm")
           (parse-git-line "R  old.scm -> new.scm"))
(assert-eq "git-relative-prefix same" "" (git-relative-prefix "/repo" "/repo"))
(assert-eq "git-relative-prefix subdir" "sub/a/" (git-relative-prefix "/repo/sub/a" "/repo"))
(assert-eq "git-relative-prefix root slash" "etc/" (git-relative-prefix "/etc" "/"))

(let ([gs '((" !" . "build") (" ~" . "src/main.c"))])
  (assert-true "git-ignored exact dir" (git-ignored-in? "build/" gs))
  (assert-false "git-ignored nonmatch" (git-ignored-in? "src/" gs))
  (assert-eq "entry-git-status dir propagation" " ~" (entry-git-status-from "src/" gs)))

;; WDired diff planner (no false delete+create -> rename!)
(assert-eq "delete a + create d never renames a->d"
           '(ok () ("a.txt") ("d.txt"))
           (plan-wdired-changes '("../" "a.txt" "b.txt" "c.txt")
                                '("../" "b.txt" "c.txt" "d.txt")))

(assert-eq "in-place edit pairs as rename"
           '(ok (("b.txt" . "b_new.txt")) () ())
           (plan-wdired-changes '("../" "a.txt" "b.txt" "c.txt")
                                '("../" "a.txt" "b_new.txt" "c.txt")))

(assert-eq "delete a + edit b in same gap pairs b->b_v2 and deletes a"
           '(ok (("b.txt" . "b_v2.txt")) ("a.txt") ())
           (plan-wdired-changes '("../" "a.txt" "b.txt" "c.txt")
                                '("../" "b_v2.txt" "c.txt")))

(assert-eq "cross-dir move pairs by identical basename"
           '(ok (("foo.c" . "sub/foo.c")) () ())
           (plan-wdired-changes '("../" "foo.c" "bar.c")
                                '("../" "bar.c" "sub/foo.c")))

(assert-eq "duplicate detection catches dir/file name collision"
           'error
           (car (plan-wdired-changes '("../" "foo") '("../" "foo" "foo/"))))

;; Glob matching
(assert-true "glob *.scm" (glob-match? "*.scm" "oil.scm"))
(assert-true "glob foo*" (glob-match? "foo*" "foo"))
(assert-true "glob *a" (glob-match? "*a" "aba"))
(assert-false "glob a*b miss" (glob-match? "a*b" "ax"))
(assert-true "substring fallback" (glob-match? "core" "test-core.scm"))

(displayln (string-append "Tests passed: " (number->string *tests-passed*)
                          ", failed: " (number->string *tests-failed*)))
(when (> *tests-failed* 0)
  (error "Unit test failure"))

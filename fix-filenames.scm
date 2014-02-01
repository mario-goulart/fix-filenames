(module fix-filenames ()

(import chicken scheme)
(use data-structures files posix srfi-1 srfi-13 extras)
(use (rename accents-substitute-latin1 (accents-substitute accents-substitute-latin1))
     (rename accents-substitute-utf8 (accents-substitute accents-substitute-utf8)))

(define *dry-run?* #f)

(define chars-ok
  (string->list "~+_-.,0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (debug fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

(define directory-separators
  (if (eq? (software-type) 'windows)
      '(#\\ #\/)
      '(#\/)))

(define (fix-filename filename)
  (let ((chars
         (string->list
          (accents-substitute-latin1
           (accents-substitute-utf8
            (string-trim-both filename))))))
    (list->string
     (filter-map
      (lambda (c)
        (cond ((memq c chars-ok)
               c)
              ((memq c '(#\' #\( #\[ #\{ #\) #\] #\}))
               #f)
              (else #\-)))
      chars))))

(define (maybe-rename-file file-path)
  (let* ((filename (pathname-strip-directory file-path))
         (fixed-filename (fix-filename filename)))
    (unless (equal? filename fixed-filename)
      (let ((fixed-file-path (make-pathname (pathname-directory file-path)
                                            fixed-filename)))
        (debug "~S -> ~S" file-path fixed-file-path)
        (unless *dry-run?*
          (rename-file file-path fixed-file-path))))))

(define (path-depth path)
  (string-count path (lambda (c)
                       (memq c directory-separators))))

(define (sort-by-depth paths)
  (sort paths
        (lambda (a b)
          (> (path-depth a) (path-depth b)))))

(define (rename-files dir)
  (let ((files (find-files dir test: (lambda (f) (not (symbolic-link? f))))))
    (for-each maybe-rename-file (sort-by-depth files))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (fprintf port "Usage: ~a [ --dry-run ] <dir>\n"
             (pathname-strip-directory (program-name)))
    (when exit-code
      (exit exit-code))))


(let* ((args (command-line-arguments))
       (options (filter (lambda (arg) (string-prefix? "--" arg)) args))
       (non-options (remove (lambda (arg) (string-prefix? "--" arg)) args)))
  (when (null? non-options)
    (usage 1))

  (when (member "--dry-run" options)
    (set! *dry-run?* #t))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (for-each rename-files non-options))

) ;; end module

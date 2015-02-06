(provide 'belden-uselines)
(require 'sort)
(require 'belden-shell-funcs)

;; sort uselines
(defun belden-sort-use-lines ()
  "Sort lines in region by length, remove dupes"
  (interactive)
  ; set beg to beginning of line
  (let (beg end enduse-marker)
    (if mark-active
        (progn
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (point) end (mark))

          (goto-char beg)
          (beginning-of-line)
          (setq beg (point))

          ; set end to end of line
          (goto-char end)
          (if (not (bolp)) (forward-line 1))
          (setq end (point))
          )
      (progn  ; determine region based on text
        ; find beg
        (beginning-of-line)
        (while (and (not (bobp)) (looking-at "use ")) (forward-line -1))
        (if (not (looking-at "use ")) (forward-line 1))
        (setq beg (point))

        ; find end
        (while (looking-at "use ") (forward-line 1))
        (setq end (point))
        )
      )
    (setq enduse-marker (make-marker))
    (set-marker enduse-marker (+ end 1))
    (save-excursion
      (belden-sort-uselines-region-by-length beg end)
      (belden-uniq-uselines-in-region beg end))
    (goto-char (- enduse-marker 1))
    (set-marker enduse-marker nil)))

(defun belden-sort-uselines-region-by-length (start end)
  "Sort the given region by the second field
   i.e. use <Shutterstock::Thingy>.

Sorts by length then by `string<'
"
  (let ((tbl (syntax-table)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (set-syntax-table sort-fields-syntax-table)
            (sort-subr nil
                       'forward-line 'end-of-line
                       (function (lambda ()
                                   (sort-skip-fields 2)
                                   nil))
                       (function (lambda () (skip-chars-forward "^ ;\t\n")))
                       (function (lambda (a b)
                                   (let ((len-a (- (cdr a) (car a)))
                                         (len-b (- (cdr b) (car b))))
                                   (cond
                                    ((= len-a len-b)
                                     (> 0 (compare-buffer-substrings
                                           nil (car a) (cdr a)
                                           nil (car b) (cdr b))))
                                    (t (< len-a len-b)))))))))
          (set-syntax-table tbl))))

(defun belden-uniq-uselines-in-region (start end)
  "Like running uniq on a uselines region"
  (let ((query-replace-highlight nil))
    (replace-regexp "\\(^use .*$\\)\n\\(\\1\n\\)+" "\\1\n" nil start end)))

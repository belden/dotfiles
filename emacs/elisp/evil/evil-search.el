;;;; Search

(require 'evil-common)
(require 'evil-ex)

(defun evil-select-search-module (option module)
  "Change the search module according to MODULE.
If MODULE is `isearch', then Emacs' isearch module is used.
If MODULE is `evil-search', then Evil's own interactive
search module is used."
  (let ((search-functions
         '(forward
           backward
           symbol-forward
           symbol-backward
           unbounded-symbol-forward
           unbounded-symbol-backward
           next
           previous)))
    (dolist (fun search-functions)
      (let ((isearch (intern (format "evil-search-%s" fun)))
            (evil-search (intern (format "evil-ex-search-%s" fun))))
        (if (eq module 'isearch)
            (substitute-key-definition
             evil-search isearch evil-motion-state-map)
          (substitute-key-definition
           isearch evil-search evil-motion-state-map)))))
  (set-default option module))

;; this customization is here because it requires
;; the knowledge of `evil-select-search-mode'
(defcustom evil-search-module 'isearch
  "The search module to be used."
  :type '(radio (const :tag "Emacs built-in isearch." :value isearch)
                (const :tag "Evil interactive search." :value evil-search))
  :group 'evil
  :set 'evil-select-search-module)

(defun evil-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((evil-search-prompt (evil-search-prompt forward))
        (isearch-search-fun-function 'evil-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (evil-save-echo-area
      (if forward
          (isearch-forward regexp-p)
        (isearch-backward regexp-p))
      (when isearch-success
        ;; always position point at the beginning of the match
        (when (and forward isearch-other-end)
          (goto-char isearch-other-end))
        (when (and (eq point (point))
                   (not (string= isearch-string "")))
          (if forward
              (isearch-repeat-forward)
            (isearch-repeat-backward))
          (isearch-exit)
          (when (and forward isearch-other-end)
            (goto-char isearch-other-end)))
        (evil-flash-search-pattern
         (evil-search-message isearch-string forward))))))

(defun evil-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `evil-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a message
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'evil-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable #'(lambda (&optional arg) (evil-flash-hook t))))
    (when evil-flash-timer
      (cancel-timer evil-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (evil-echo-area-save)
      (evil-echo string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook #'evil-flash-hook nil t)
      (add-hook 'evil-operator-state-exit-hook #'evil-flash-hook nil t)
      (add-hook 'pre-command-hook #'evil-clean-isearch-overlays nil t)
      (setq evil-flash-timer
            (run-at-time evil-flash-delay nil disable)))))

(defun evil-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook #'evil-clean-isearch-overlays t)
  (unless (memq this-command
                '(evil-search-backward
                  evil-search-forward
                  evil-search-next
                  evil-search-previous
                  evil-search-symbol-backward
                  evil-search-symbol-forward))
    (isearch-clean-overlays)))
(put 'evil-clean-isearch-overlays 'permanent-local-hook t)

(defun evil-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(evil-search-backward
                         evil-search-forward
                         evil-search-next
                         evil-search-previous
                         evil-search-symbol-backward
                         evil-search-symbol-forward))))
    (evil-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when evil-flash-timer
      (cancel-timer evil-flash-timer)))
  (remove-hook 'pre-command-hook #'evil-flash-hook t)
  (remove-hook 'evil-operator-state-exit-hook #'evil-flash-hook t))
(put 'evil-flash-hook 'permanent-local-hook t)

(defun evil-search-function (&optional forward regexp-p wrap)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (funcall search-fun string bound
                             ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (funcall search-fun string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun evil-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (evil-search-function isearch-forward evil-regexp-search evil-search-wrap))

(defun evil-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (search-func (evil-search-function
                         forward regexp-p evil-search-wrap)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (error "\"%s\": %s not found"
                string (if regexp-p "pattern" "string"))))
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      ;; handle opening and closing of invisible area
      (when (boundp 'isearch-filter-predicate)
        (funcall isearch-filter-predicate
                 (match-beginning 0) (match-end 0)))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (evil-search-message string forward))))
      (evil-flash-search-pattern string t))))

(defun evil-search-symbol (forward)
  "Search for symbol near point.
If FORWARD is nil, search backward, otherwise forward."
  (let ((string (car-safe regexp-search-ring))
        (move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(evil-search-symbol-forward
                   evil-search-symbol-backward))
           (stringp string)
           (not (string= string "")))
      (evil-search string forward t))
     (t
      (setq string (evil-find-symbol forward))
      (if (null string)
          (error "No symbol under point")
        (setq string (format "\\_<%s\\_>" (regexp-quote string))))
      (evil-search string forward t)))))

(defun evil-find-symbol (forward)
  "Return symbol near point as a string.
If FORWARD is nil, search backward, otherwise forward.
Returns nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point 'symbol))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point 'symbol)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        string))))

(defun evil-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun evil-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (evil-search-prompt forward) string))

(defadvice isearch-message-prefix (around evil activate)
  "Use `evil-search-prompt'."
  (if evil-search-prompt
      (setq ad-return-value evil-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around evil activate)
  "Exit search if no search string."
  (if (and evil-search-prompt
           (string= isearch-string ""))
      (isearch-exit)
    ad-do-it))

(defadvice isearch-lazy-highlight-search (around evil activate)
  "Never wrap the search in this context."
  (let (evil-search-wrap)
    ad-do-it))

;;; Ex search

;; a pattern
(defun evil-ex-make-pattern (regexp casefold whole-line)
  "Create a new search pattern.
REGEXP is the regular expression to be searched for.
CASEFOLD is the case-fold property of the search,
which can be either `sensitive', `insensitive' or `smart'.
Here `smart' means the pattern is case sensitive if and only if
it contains a capital character. If WHOLE-LINE is non-nil,
all occurrences of the pattern on a line will be highlighted,
otherwise only the first one."
  (list (evil-ex-regex-without-case regexp)
        (evil-ex-regex-case regexp casefold)
        whole-line))

(defun evil-ex-pattern-regex (pattern)
  "Return the regular expression of a search PATTERN."
  (car pattern))

(defun evil-ex-pattern-case-fold (pattern)
  "Return the case-fold property of a search PATTERN."
  (cadr pattern))

(defun evil-ex-pattern-whole-line (pattern)
  "Return the whole-line property of a search PATTERN."
  (nth 2 pattern))

(defun evil-ex-regex-without-case (re)
  "Return the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string
   "\\(\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\)\\\\[cC]" "\\1" re))

(defun evil-ex-regex-case (re default-case)
  "Return the case as implied by \\c or \\C in regular expression RE.
If \\c appears anywhere in the pattern, the pattern is case
insensitive. If \\C appears, the pattern is case sensitive.
Only the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the case
specified by DEFAULT-CASE is used. DEFAULT-CASE should be either
`sensitive', `insensitive' or `smart'. In the latter case, the pattern
will be case-sensitive if and only if it contains an upper-case
letter, otherwise it will be case-insensitive."
  (cond
   ((string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\\\([cC]\\)" re)
    (if (eq (aref (match-string 1 re) 0) ?c) 'insensitive 'sensitive))
   ((eq default-case 'smart)
    (if (isearch-no-upper-case-p re t)
        'insensitive
      'sensitive))
   (t default-case)))

(defun evil-ex-make-hl (name &rest args)
  "Create a new highlight object with name NAME and properties ARGS.
The following properties are supported:
:face The face to be used for the highlighting overlays.
:win The window in which the highlighting should be shown.
     Note that the highlight will be visible in all windows showing
     the corresponding buffer, but only the matches visible in the
     specified window will actually be highlighted. If :win is nil,
     the matches in all windows will be highlighted.
:min The minimal buffer position for highlighted matches.
:max The maximal buffer position for highlighted matches.
:match-hook A hook to be called once for each highlight.
            The hook must take two arguments, the highlight and
            the overlay for that highlight.
:update-hook A hook called once after updating the highlighting
             with two arguments, the highlight and a message string
             describing the current match status."
  (unless (symbolp name)
    (error "Expected symbol as name of highlight"))
  (let ((face 'evil-ex-lazy-highlight)
        (win (selected-window))
        min max match-hook update-hook)
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (cond
         ((eq key :face) (setq face val))
         ((eq key :win)  (setq win val))
         ((eq key :min)  (setq min val))
         ((eq key :max)  (setq max val))
         ((eq key :match-hook) (setq match-hook val))
         ((eq key :update-hook) (setq update-hook val))
         (t (error "Unexpected keyword: %s" key)))))
    (when (assoc name evil-ex-active-highlights-alist)
      (evil-ex-delete-hl name))
    (when (null evil-ex-active-highlights-alist)
      (add-hook 'window-scroll-functions
                #'evil-ex-hl-update-highlights-scroll nil t)
      (add-hook 'window-size-change-functions
                #'evil-ex-hl-update-highlights-resize nil))
    (push (cons name (vector name
                             nil
                             face
                             win
                             min
                             max
                             match-hook
                             update-hook
                             nil))
          evil-ex-active-highlights-alist)))

(defun evil-ex-hl-name (hl)
  "Return the name of the highlight HL."
  (aref hl 0))

(defun evil-ex-hl-pattern (hl)
  "Return the pattern of the highlight HL."
  (aref hl 1))

(defun evil-ex-hl-set-pattern (hl pattern)
  "Set the pattern of the highlight HL to PATTERN."
  (aset hl 1 pattern))

(defun evil-ex-hl-face (hl)
  "Return the face of the highlight HL."
  (aref hl 2))

(defun evil-ex-hl-window (hl)
  "Return the window of the highlight HL."
  (aref hl 3))

(defun evil-ex-hl-min (hl)
  "Return the minimal buffer position of the highlight HL."
  (aref hl 4))

(defun evil-ex-hl-set-min (hl min)
  "Set the minimal buffer position of the highlight HL to MIN."
  (aset hl 4 min))

(defun evil-ex-hl-max (hl)
  "Return the maximal buffer position of the highlight HL."
  (aref hl 5))

(defun evil-ex-hl-set-max (hl max)
  "Set the minimal buffer position of the highlight HL to MAX."
  (aset hl 5 max))

(defun evil-ex-hl-match-hook (hl)
  "Return the match-hook of the highlight HL."
  (aref hl 6))

(defun evil-ex-hl-update-hook (hl)
  "Return the update-hook of the highlight HL."
  (aref hl 7))

(defun evil-ex-hl-overlays (hl)
  "Return the list of active overlays of the highlight HL."
  (aref hl 8))

(defun evil-ex-hl-set-overlays (hl overlays)
  "Set the list of active overlays of the highlight HL to OVERLAYS."
  (aset hl 8 overlays))

(defun evil-ex-delete-hl (name)
  "Remove the highlighting object with a certain NAME."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (evil-ex-hl-overlays hl))
      (setq evil-ex-active-highlights-alist
            (assq-delete-all name evil-ex-active-highlights-alist))
      (evil-ex-hl-update-highlights))
    (when (null evil-ex-active-highlights-alist)
      (remove-hook 'window-scroll-functions
                   #'evil-ex-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions
                   #'evil-ex-hl-update-highlights-resize))))

(defun evil-ex-hl-active-p (name)
  "Whether the highlight with a certain NAME is active."
  (and (assoc name evil-ex-active-highlights-alist) t))

(defun evil-ex-hl-change (name pattern)
  "Set the regular expression of highlight NAME to PATTERN."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (evil-ex-hl-set-pattern hl
                              (if (zerop (length pattern))
                                  nil
                                pattern))
      (evil-ex-hl-idle-update))))

(defun evil-ex-hl-set-region (name beg end &optional type)
  "Set minimal and maximal position of highlight NAME to BEG and END."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (evil-ex-hl-set-min hl beg)
      (evil-ex-hl-set-max hl end)
      (evil-ex-hl-idle-update))))

(defun evil-ex-hl-get-max (name)
  "Return the maximal position of the highlight with name NAME."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (and hl (evil-ex-hl-max hl))))

(defun evil-ex-hl-update-highlights ()
  "Update the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr evil-ex-active-highlights-alist))
    (let ((old-ovs (evil-ex-hl-overlays hl))
          new-ovs
          (pattern (evil-ex-hl-pattern hl))
          (face (evil-ex-hl-face hl))
          (match-hook (evil-ex-hl-match-hook hl))
          result)
      (condition-case lossage
          (save-match-data
            (when pattern
              (dolist (win (if (eq evil-ex-interactive-search-highlight
                                   'all-windows)
                               (get-buffer-window-list (current-buffer) nil t)
                             (list (evil-ex-hl-window hl))))
                (let ((beg (max (window-start win)
                                (or (evil-ex-hl-min hl) (point-min))))
                      (end (min (window-end win)
                                (or (evil-ex-hl-max hl) (point-max)))))
                  (when (< beg end)
                    (save-excursion
                      (goto-char beg)
                      ;; set the overlays for the current highlight,
                      ;; reusing old overlays (if possible)
                      (while (and (evil-ex-search-find-next-pattern pattern)
                                  (< (match-beginning 0) (match-end 0))
                                  (<= (match-end 0) end))
                        (let ((ov (or (pop old-ovs) (make-overlay 0 0))))
                          (move-overlay ov (match-beginning 0) (match-end 0))
                          (overlay-put ov 'face face)
                          (overlay-put ov 'evil-ex-hl (evil-ex-hl-name hl))
                          (overlay-put ov 'priority 1000)
                          (push ov new-ovs)
                          (when match-hook (funcall match-hook hl ov)))
                        (unless (evil-ex-pattern-whole-line pattern)
                          (forward-line))))))))

            (mapc #'delete-overlay old-ovs)
            (evil-ex-hl-set-overlays hl new-ovs)
            (if (or (null pattern) new-ovs)
                (setq result t)
              ;; Maybe the match could just not be found somewhere else?
              (save-excursion
                (goto-char (or (evil-ex-hl-min hl) (point-min)))
                (if (and (evil-ex-search-find-next-pattern pattern)
                         (< (match-end 0) (or (evil-ex-hl-max hl)
                                              (point-max))))
                    (setq result (format "Match in line %d"
                                         (line-number-at-pos
                                          (match-beginning 0))))
                  (setq result "No match")))))

        (invalid-regexp
         (setq result (cadr lossage)))

        (search-failed
         (setq result (nth 2 lossage)))

        (error
         (setq result (format "%s" lossage))))
      (when (evil-ex-hl-update-hook hl)
        (funcall (evil-ex-hl-update-hook hl) hl result)))))

(defun evil-ex-search-find-next-pattern (pattern &optional direction)
  "Look for the next occurrence of PATTERN in a certain DIRECTION.
Note that this function ignores the whole-line property of PATTERN."
  (setq direction (or direction 'forward))
  (let ((case-fold-search (eq (evil-ex-pattern-case-fold pattern)
                              'insensitive)))
    (cond
     ((eq direction 'forward)
      (re-search-forward (evil-ex-pattern-regex pattern) nil t))
     ((eq direction 'backward)
      (re-search-backward (evil-ex-pattern-regex pattern) nil t))
     (t
      (error "Unknown search direction: %s" direction)))))

(defun evil-ex-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and evil-ex-interactive-search-highlight
             evil-ex-active-highlights-alist)
    (when evil-ex-hl-update-timer
      (cancel-timer evil-ex-hl-update-timer))
    (setq evil-ex-hl-update-timer
          (run-at-time 0.1 nil
                       #'evil-ex-hl-do-update-highlight
                       (current-buffer)))))

(defun evil-ex-hl-do-update-highlight (&optional buffer)
  "Timer function for updating the highlights."
  (with-current-buffer buffer
    (evil-ex-hl-update-highlights))
  (setq evil-ex-hl-update-timer nil))

(defun evil-ex-hl-update-highlights-scroll (win beg)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer win)
    (evil-ex-hl-idle-update)))
(put 'evil-ex-hl-update-highlights-scroll 'permanent-local-hook t)

(defun evil-ex-hl-update-highlights-resize (frame)
  "Update highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (evil-ex-hl-idle-update)))))
(put 'evil-ex-hl-update-highlights-resize 'permanent-local-hook t)

;; interactive search
(defun evil-ex-search-activate-highlight (pattern)
  "Activate highlighting of the search pattern set to PATTERN.
This function does nothing if `evil-ex-search-interactive' or
`evil-ex-search-highlight-all' is nil. "
  (when (and evil-ex-search-interactive evil-ex-search-highlight-all)
    (with-current-buffer (or evil-ex-current-buffer (current-buffer))
      (unless (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-make-hl 'evil-ex-search
                         :win (minibuffer-selected-window)))
      (if pattern
          (evil-ex-hl-change 'evil-ex-search pattern)))))

(defun evil-ex-find-next (&optional pattern direction nowrap)
  "Search for the next occurrence of the PATTERN in DIRECTION.
PATTERN must be created using `evil-ex-make-pattern', DIRECTION
is either 'forward or 'backward. If NOWRAP is non nil, the search
does not wrap at buffer boundaries. Furthermore this function
only searches invisible text if `search-invisible' is t. If
PATTERN is not specified the current global pattern
`evil-ex-search-pattern' and if DIRECTION is not specified the
current global direction `evil-ex-search-direction' is used.
This function return t if the search was successful, nil if it
was unsuccessful and 'wrapped if the search was successful but
has been wrapped at the buffer boundaries."
  (setq pattern (or pattern evil-ex-search-pattern)
        direction (or direction evil-ex-search-direction))
  (catch 'done
    (let (wrapped)
      (while t
        (let ((search-result (evil-ex-search-find-next-pattern pattern
                                                               direction)))
          (cond
           ((and search-result
                 (or (eq search-invisible t)
                     (not (isearch-range-invisible
                           (match-beginning 0) (match-end 0)))))
            ;; successful search and not invisible
            (throw 'done (if wrapped 'wrapped t)))
           ((not search-result)
            ;; unsuccessful search
            (if nowrap
                (throw 'done nil)
              (setq nowrap t
                    wrapped t)
              (goto-char (if (eq direction 'forward)
                             (point-min)
                           (point-max)))))))))))

(defun evil-ex-search-update (pattern offset beg end message)
  "Update the highlighting and info-message for the search pattern.
PATTERN is the search pattern and OFFSET the associated offset.
BEG and END specifiy the current match, MESSAGE is the info
message to be shown. This function does nothing if
`evil-ex-search-interactive' is nil."
  (when evil-ex-search-interactive
    (cond
     ((and beg end)
      ;; update overlay
      (if evil-ex-search-overlay
          (move-overlay evil-ex-search-overlay beg end)
        (setq evil-ex-search-overlay
              (make-overlay beg end))
        (overlay-put evil-ex-search-overlay 'priority 1001)
        (overlay-put evil-ex-search-overlay 'face 'evil-ex-search))
      ;; move point
      (goto-char beg)
      (evil-ex-search-goto-offset offset)
      ;; update highlights
      (when evil-ex-search-highlight-all
        (evil-ex-hl-change 'evil-ex-search pattern)))
     (t
      ;; no match
      (when evil-ex-search-overlay
        ;; remove overlay
        (delete-overlay evil-ex-search-overlay)
        (setq evil-ex-search-overlay nil))
      ;; no highlights
      (when evil-ex-search-highlight-all
        (evil-ex-hl-change 'evil-ex-search nil))
      ;; and go to initial position
      (goto-char evil-ex-search-start-point)))
    (evil-ex-echo message)))

(defun evil-ex-search-start-session ()
  "Initialize Ex for interactive search."
  (remove-hook 'minibuffer-setup-hook #'evil-ex-search-start-session)
  (add-hook 'after-change-functions #'evil-ex-search-update-pattern nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session)
  (evil-ex-search-activate-highlight nil))
(put 'evil-ex-search-start-session 'permanent-local-hook t)

(defun evil-ex-search-stop-session ()
  "Stop interactive search."
  (with-current-buffer evil-ex-current-buffer
    ;; TODO: This is a bad fix to remove duplicates. The duplicates
    ;;       exist because `isearch-range-invisible' may add a single
    ;;       overlay multiple times if we are in an unlucky situation
    ;;       of overlapping overlays. This happens in our case because
    ;;       of the overlays that are used for (lazy) highlighting.
    ;;       Perhaps it would be better to disable those overlays
    ;;       temporarily before calling `isearch-range-invisible'.
    (setq isearch-opened-overlays (delete-dups isearch-opened-overlays))
    (isearch-clean-overlays))
  (remove-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session)
  (remove-hook 'after-change-functions #'evil-ex-search-update-pattern t)
  (when evil-ex-search-overlay
    (delete-overlay evil-ex-search-overlay)
    (setq evil-ex-search-overlay nil)))
(put 'evil-ex-search-stop-session 'permanent-local-hook t)

(defun evil-ex-split-search-pattern (pattern direction)
  "Split PATTERN in regexp, offset and next-pattern parts.
Returns a triple (regexp  offset next-search)."
  (save-match-data
    (if (or (and (eq direction 'forward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(/\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern))
            (and (eq direction 'backward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(\\?\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern)))
        (list (substring pattern 0 (match-beginning 1))
              (match-string 2 pattern)
              (match-string 3 pattern))
      (list pattern nil nil))))

(defun evil-ex-search-full-pattern (pattern-string count direction)
  "Search for a full search pattern PATTERN-STRING in DIRECTION.
This function split PATTERN-STRING in
pattern/offset/;next-pattern parts and performs the search in
DIRECTION which must be either 'forward or 'backward. The first
search is repeated COUNT times. If the pattern part of
PATTERN-STRING is empty, the last global pattern stored in
`evil-ex-search-pattern' is used instead if in addition the
offset part is nil (i.e. no pattern/offset separator), the last
global offset stored in `evil-ex-search-offset' is used as
offset. The current match data will correspond to the last
successful match.  This function returns a triple (RESULT PATTERN
OFFSET) where RESULT is

  t              the search has been successful without wrap
  'wrap          the search has been successful with wrap
  'empty-pattern the last pattern has been empty
  nil            the search has not been successful

and PATTERN and OFFSET are the last pattern and offset this
function searched for. Note that this function does not handle
any error conditions."
  (setq count (or count 1))
  (catch 'done
    (while t
      (let* ((res (evil-ex-split-search-pattern pattern-string direction))
             (pat (pop res))
             (offset (pop res))
             (next-pat (pop res)))
        ;; use last pattern of no new pattern has been specified
        (if (not (zerop (length pat)))
            (setq pat (evil-ex-make-pattern pat
                                            evil-ex-search-case
                                            t))
          (setq pat evil-ex-search-pattern
                offset (or offset evil-ex-search-offset)))
        (let (search-result)
          (while (> count 0)
            (let ((result (evil-ex-find-next pat direction)))
              (if (not result) (setq search-result nil count 0)
                (setq search-result
                      (if (or (eq result 'wrap)
                              (eq search-result 'wrap))
                          'wrap t)
                      count (1- count)))))
          (cond
           ;; search failed
           ((not search-result) (throw 'done (list nil pat offset)))
           ;; no next pattern, search complete
           ((zerop (length next-pat))
            (evil-ex-search-goto-offset offset)
            (throw 'done (list search-result pat offset)))
           ;; next pattern but empty
           ((= 1 (length next-pat))
            (evil-ex-search-goto-offset offset)
            (throw 'done (list 'empty-pattern pat offset)))
           ;; next non-empty pattern, next search iteration
           (t
            (evil-ex-search-goto-offset offset)
            (setq count 1
                  pattern-string (substring next-pat 1)
                  direction (if (= (aref next-pat 0) ?/)
                                'forward
                              'backward)))))))))

(defun evil-ex-search-update-pattern (beg end range)
  "Update the current search pattern."
  (let ((pattern-string (minibuffer-contents)))
    (with-current-buffer evil-ex-current-buffer
      (with-selected-window (minibuffer-selected-window)
        (goto-char (1+ evil-ex-search-start-point))
        (condition-case err
            (let* ((result (evil-ex-search-full-pattern pattern-string
                                                        (or evil-ex-search-count 1)
                                                        evil-ex-search-direction))
                   (success (pop result))
                   (pattern (pop result))
                   (offset (pop result)))
              (cond
               ((eq success 'wrap)
                (evil-ex-search-update pattern offset
                                       (match-beginning 0) (match-end 0)
                                       "Wrapped"))
               ((eq success 'empty-string)
                (evil-ex-search-update nil nil nil nil nil))
               (success
                (evil-ex-search-update pattern offset
                                       (match-beginning 0) (match-end 0)
                                       nil))
               (t
                (evil-ex-search-update nil nil
                                       nil nil
                                       "search failed"))))
          (invalid-regexp
           (evil-ex-search-update nil nil nil nil (cadr err)))
          (error
           (evil-ex-search-update nil nil nil nil (format "%s" err))))))))
(put 'evil-ex-search-update-pattern 'permanent-local-hook t)

(defun evil-ex-search-exit ()
  "Exit interactive search, keeping lazy highlighting active."
  (interactive)
  (evil-ex-search-stop-session)
  (exit-minibuffer))

(defun evil-ex-search-abort ()
  "Abort interactive search, disabling lazy highlighting."
  (interactive)
  (evil-ex-search-stop-session)
  (evil-ex-delete-hl 'evil-ex-search)
  (abort-recursive-edit))

(defun evil-ex-search-goto-offset (offset)
  "Move point according to search OFFSET and set `evil-this-type' accordingly.
This function assumes that the current match data represents the
current search result."
  (unless (zerop (length offset))
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (save-match-data
        (unless
            (string-match
             "^\\([esb]\\)?\\(\\([-+]\\)?\\([0-9]*\\)\\)$"
             offset)
          (error "Invalid search offset: %s" offset))
        (let ((count (if (= (match-beginning 4) (match-end 4))
                         (cond
                          ((not (match-beginning 3)) 0)
                          ((= (aref offset (match-beginning 3)) ?+) +1)
                          (t -1))
                       (string-to-number (match-string 2 offset)))))
          (cond
           ((not (match-beginning 1))
            (setq evil-this-type 'line)
            (forward-line count))
           ((= (aref offset (match-beginning 1)) ?e)
            (goto-char (+ end count -1))
            (setq evil-this-type 'inclusive))
           ((memq (aref offset (match-beginning 1)) '(?s ?b))
            (goto-char (+ beg count))
            (setq evil-this-type 'inclusive))))))))

(defun evil-ex-start-search (direction count)
  "Start a new search in a certain DIRECTION."
  ;; store buffer and window where the search started
  (let ((evil-ex-current-buffer (current-buffer)))
    (setq evil-ex-search-count count
          evil-ex-search-direction direction
          evil-ex-search-start-point (point))
    (progn
      ;; ensure minibuffer is initialized accordingly
      (add-hook 'minibuffer-setup-hook #'evil-ex-search-start-session)
      ;; read the search string
      (let* ((minibuffer-local-map evil-ex-search-keymap)
             (search-string
              (condition-case err
                  (read-string (if (eq evil-ex-search-direction 'forward)
                                   "/" "?")
                               nil 'evil-ex-search-history)
                (quit
                 (evil-ex-search-stop-session)
                 (evil-ex-delete-hl 'evil-ex-search)
                 (goto-char evil-ex-search-start-point)
                 (signal (car err) (cdr err))))))
        ;; pattern entered successful
        (goto-char (1+ evil-ex-search-start-point))
        (let* ((result
                (evil-ex-search-full-pattern search-string
                                             evil-ex-search-count
                                             evil-ex-search-direction))
               (success (pop result))
               (pattern (pop result))
               (offset (pop result)))
          (cond
           ((memq success '(t wrap))
            (setq evil-ex-search-pattern pattern
                  evil-ex-search-offset offset)
            (goto-char (match-beginning 0))
            (setq evil-ex-search-match-beg (match-beginning 0)
                  evil-ex-search-match-end (match-end 0))
            (evil-ex-search-goto-offset offset))
           (t
            (signal 'search-failed (list search-string)))))))))

(defun evil-ex-start-symbol-search (unbounded direction count)
  "Search for the symbol under point.
The search matches the COUNT-th occurrence of the word.
If the UNBOUNDED argument is nil, the search matches only
at symbol boundaries, otherwise it matches anywhere.
The DIRECTION argument should be either `forward' or
`backward', determining the search direction."
  (let ((string (evil-find-symbol (eq direction 'forward))))
    (if (null string)
        (error "No symbol under point")
      (setq evil-ex-search-count count
            evil-ex-search-direction direction
            evil-ex-search-pattern
            (evil-ex-make-pattern
             (if unbounded
                 (regexp-quote (match-string 0))
               (format "\\_<%s\\_>" (regexp-quote (match-string 0))))
             (cond
              ((memq evil-ex-search-case '(sensitive smart))
               'sensitive)
              ((eq evil-ex-search-case 'insensitive)
               'insensitive)) t))
      (evil-ex-delete-hl 'evil-ex-search)
      (when (fboundp 'evil-ex-search-next)
        (evil-ex-search-next count)))))

;; substitute
(evil-ex-define-argument-type substitution (flag &rest args)
  (with-selected-window (minibuffer-selected-window)
    (with-current-buffer evil-ex-current-buffer
      (cond
       ((eq flag 'start)
        (evil-ex-make-hl
         'evil-ex-substitute
         :update-hook #'evil-ex-pattern-update-ex-info
         :match-hook (and evil-ex-substitute-interactive-replace
                          #'evil-ex-pattern-update-replacement))
        (setq flag 'update))

       ((eq flag 'stop)
        (evil-ex-delete-hl 'evil-ex-substitute))))

    (when (and (eq flag 'update) evil-ex-substitute-highlight-all)
      (condition-case lossage
          (let* ((result (evil-ex-parse-substitute (car args)))
                 (pattern (pop result))
                 (replacement (or (pop result) ""))
                 (flags (append (pop result) nil)))

            (setq evil-ex-substitute-pattern
                  (and pattern
                       (evil-ex-make-pattern
                        pattern
                        (or (and (memq ?i flags) 'insensitive)
                            (and (memq ?I flags) 'sensitive)
                            evil-ex-substitute-case
                            evil-ex-search-case)
                        (memq ?g flags)))
                  evil-ex-substitute-replacement replacement)
            (apply #'evil-ex-hl-set-region
                   'evil-ex-substitute
                   (or evil-ex-range
                       (evil-range (line-beginning-position)
                                   (line-end-position))))
            (evil-ex-hl-change 'evil-ex-substitute
                               evil-ex-substitute-pattern))
        (end-of-file
         (evil-ex-pattern-update-ex-info nil
                                         "incomplete replacement"))
        (error
         (evil-ex-pattern-update-ex-info nil
                                         (format "%s" lossage)))))))

(defun evil-ex-pattern-update-ex-info (hl result)
  "Update the Ex info string."
  (when (stringp result)
    (evil-ex-echo result)))

(defun evil-ex-pattern-update-replacement (hl overlay)
  "Update the replacement display."
  (when (fboundp 'match-substitute-replacement)
    (let ((fixedcase (not (eq (evil-ex-pattern-case-fold
                               (evil-ex-hl-pattern hl))
                              'insensitive)))
          repl)
      (setq repl (evil-match-substitute-replacement
                  evil-ex-substitute-replacement
                  fixedcase))
      (put-text-property 0 (length repl)
                         'face 'evil-ex-substitute
                         repl)
      (overlay-put overlay 'after-string repl))))

(defun evil-ex-parse-global (string)
  "Parse STRING as a global argument."
  (evil-delimited-arguments string 2))

(defun evil-ex-parse-substitute (string)
  "Parse STRING as a substitution argument."
  (let ((args (evil-delimited-arguments string 3)))
    (list (nth 0 args)
          (evil-compile-replacement (nth 1 args))
          (nth 2 args))))

(defun evil-ex-nohighlight ()
  "Disable the active search highlightings."
  (interactive)
  (evil-ex-delete-hl 'evil-ex-substitute)
  (evil-ex-delete-hl 'evil-ex-search))

(provide 'evil-search)

;;; evil-search.el ends here
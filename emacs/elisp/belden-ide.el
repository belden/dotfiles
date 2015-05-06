(provide 'belden-ide)

;;;;;;;;;;;;
;; debugger customizations:
;;    show line numbers in the file that the debugger is visiting
(defadvice gud-find-file (after show-line-numbers activate compile)
  (with-current-buffer (get-file-buffer file)
    (if (string-match "^perl5db.pl\$" (buffer-name))
        'nil
      (setnu-mode 1))))

;;    turn off line numbers when we're done in the debugger
(defadvice gud-sentinel (after remove-line-numbers activate compile)
  (belden-remove-line-numbers))

;;    run the debugger on the current file
(defun belden-perl-debug (perldebug-file)
	"Run the perl debugger"
	(interactive
	 (list (read-string
					"Run perldb as: "
					(format "perl -d %s" (buffer-file-name)))))
	(perldb perldebug-file))

(defun belden-remove-line-numbers ()
  (interactive)
  (loop for buf in (buffer-list) do
        (with-current-buffer buf (if setnu-mode (setnu-mode nil)))))

(defun belden/perl-current-class ()
  "Insert the name of the current class"
  (interactive)
  (insert (replace-regexp-in-string "/" "::"
          (replace-regexp-in-string ".pm$" ""
          (replace-regexp-in-string "^.*lib/" ""
          (buffer-file-name))))))


(defun belden-open-perl-module (module)
	"Prompt for a Perl module name and go open it"
	(interactive
	 (list (read-string	"Perl module to find: ")))
	(belden-find-module module))

;; inspect the codebase
(defun belden-findcode  (findcode-command)
   "Run a findcode in separate buffer"
   (interactive
    (list (read-string
           "Run findcode as: "
           (format "findcode %s" (belden-current-keyword-or-quoted-active-region)))))
   (let ((compilation-buffer-name-function
          (lambda (mode-name)
            (format "*%s*" findcode-command))))
     (grep findcode-command)))

(defun belden-findcallers (findcallers-command)
	"Find callers of a particular method"
	(interactive
	 (list (read-string
					"Run findcallers on: "
					(format "%s" (belden-current-keyword-or-quoted-active-region)))))
	(let ((compilation-buffer-name-function
				 (lambda (mode-name)
					 (format "*findcallers %s*" findcallers-command))))
		(grep (format "findcode '>(?:SUPER::|NEXT::|super->)?%s'" findcallers-command))))

(defun belden-comparesub (comparesub-command)
   "Run a comparesub in separate buffer"
   (interactive
    (list (read-string "Run comparesub as: "
                       (format "comparesub %s" (belden-current-keyword-or-quoted-active-region)))))
   (let ((compilation-buffer-name-function
          (lambda (mode-name)
            (format "*%s*" comparesub-command))))
     (grep comparesub-command)))

;; does this file compile?
(defun belden-perl-check ()
   "Run a perl check on the current buffer."
   (interactive)
   (save-buffer)
   (belden-push-mark)
   (setq compile-apc-command
      (format
        (concat
          (if (save-excursion
                (goto-char 0)
                (string-match "^#![ ]*/usr/bin/perl" (thing-at-point 'line))
              ) "/usr/bin/perl" "perl")
          " -Mwarnings -M-warnings=redefine -c \'%s\'")
        (buffer-file-name))
   )
   (message "compile-apc-command: %s" compile-apc-command)
   (compile compile-apc-command))

(defvar belden-last-test-executed nil)
(defvar belden-next-to-last-test-executed nil)
(defun belden-run-perl-file ()
   "Run a perl file"
   (interactive)
   (if (not (string-match "^\\*.*\\*$" (buffer-name))) (save-buffer))
   (when (string= (buffer-name) "*compilation*")
     (delete-other-windows)
     (belden-jump-to-last-test))
   (let ((filename (filename-under-point)))
     (if (and filename (string-match "\\.t\$" filename))
         (progn (belden-find-file 'nil filename) (belden-run-perl-file))
       (progn
         (if (not (boundp 'compile-rpf-command))
             (setq compile-rpf-command (buffer-file-name)))
         (if (not (file-exists-p compile-rpf-command))
             (setq compile-rpf-command
                   (read-string "File to run: "
                                (format "%s" compile-rpf-command))))
         (message "compile-rpf-command: %s" compile-rpf-command)
         (when (not (string= belden-last-test-executed compile-rpf-command))
           (setq belden-next-to-last-test-executed belden-last-test-executed))
         (setq belden-last-test-executed compile-rpf-command)
         (compile (format "(cd %s; %s)" (file-name-directory (expand-file-name (buffer-name))) compile-rpf-command))
         (other-window 1)
         (toggle-read-only)
         (other-window 1)))))

(defun belden-run-rock ()
   "Run a file with rock"
   (interactive)
   (if (not (string-match "^\\*.*\\*$" (buffer-name))) (save-buffer))
   (when (string= (buffer-name) "*compilation*")
     (delete-other-windows)
     (belden-jump-to-last-test))
   (let ((default-directory (locate-dominating-file default-directory ".rock.yml"))
         (filename (filename-under-point)))
		 (setq compile-rpf-command (concat "rock run " (buffer-file-name)))
		 (message "compile-rpf-command: %s" compile-rpf-command)
		 (when (not (string= belden-last-test-executed compile-rpf-command))
			 (setq belden-next-to-last-test-executed belden-last-test-executed))
		 (setq belden-last-test-executed compile-rpf-command)
		 (compile compile-rpf-command)
		 (other-window 1)
		 (toggle-read-only)
		 (other-window 1)))

(defun belden/test-this ()
  "Run a file with run_tests"
  (interactive)
	(setq checkout-root (expand-file-name (locate-dominating-file (buffer-file-name) ".git")))
	(setq filename (replace-regexp-in-string checkout-root "" (buffer-file-name)))
	(let ((test-command  (read-string "run test as: " (format "run-modified-tests %s" filename))))
		(compile test-command)
		(other-window 1)
		(toggle-read-only)
		(other-window 1)))

(defun belden-set-test-file ()
   "Set the testfile to be run"
   (interactive)
   (if (not (boundp 'compile-rpf-command))
       (setq compile-rpf-command (buffer-file-name)))
   (setq compile-rpf-command
         (read-string "TestFile for Buffer: "
                      (format "/home/dev/bin/development-tools/run-modified-tests %s" compile-rpf-command))))

(defun belden-make-executable ()
	"Makes this file executable"
	(interactive)
  (if (= (shell-command (concat "chmod a+x " (buffer-file-name))) 0)
      (message (concat "Wrote and made executable " (buffer-file-name)))))

(defun belden-save-and-make-executable ()
	"Save and make this file executable"
  (interactive)
  (save-buffer)
	(belden-make-executable))

(defun belden-toggle-quotes ()
  "Converts double quotes to singles and vice versa"
  (interactive)
  (save-excursion
    (let ((start (point)) 
          (face_at_point (last-element (face-at-point)))
          beg end)
      (while (eq face_at_point (last-element (face-at-point)))
        (forward-char -1))
      (forward-char 1)
      (while (looking-at "[ \t\n]") (forward-char 1))
      (setq beg (point))
      (if (not (looking-at "[\"\']")) (search-forward-regexp "[\"\']" 'nil 't))
      (if (<= (point) start) (setq beg (point)))

      (goto-char start)

      (while (eq face_at_point (last-element (face-at-point)))
        (forward-char 1))
      (forward-char -1)
      (while (looking-at "[ \t\n]") (forward-char -1))
      (setq end (point))
      (if (not (looking-at "[\"\']")) (search-backward-regexp "[\"\']" 'nil 't))
      (if (>= (point) start) (setq end (point)))

      (goto-char beg)
      (if (looking-at "\"") 
          (progn (delete-char 1) (insert-char ?\047 1))
        (if (looking-at "\'") 
            (progn (delete-char 1) (insert-char ?\042 1))
          (insert-char ?\042 1)))
      
      (goto-char end)
      (if (looking-at "\"") 
          (progn (delete-char 1) (insert-char ?\047 1))
        (if (looking-at "\'") 
            (progn (delete-char 1) (insert-char ?\042 1))
          (progn (forward-char 2) (insert-char ?\042 1))))
      )))

;; useful things to insert
(defun belden-insert-dbsingle ()
  "insert DB::single line"
  (interactive)
  (belden-insert-as-new-line
   "local $MY::var = 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (belden-insert-as-new-line
   "$DB::single = 1 if $MY::var; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (save-buffer)
)
(defun belden-insert-die-signal-handler ()
  "drop into debugger mode if a line dies"
  (interactive)
  (belden-insert-as-new-line
   "$SIG{__DIE__} = sub { $MY::var = 1 }; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (belden-insert-as-new-line
   "$DB::single = 1 if $MY::var; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (save-buffer)
)
(defun belden-insert-as-new-line (string)
  (beginning-of-line)
  (insert (concat "  " string "\n"))
  (forward-line -1)
  (beginning-of-line)
  (belden-kill-whole-word)
  (forward-line 1)
)

(defun belden-stack-trace ()
  "insert cluck line"
  (interactive)
  (belden-insert-as-new-line
   "$MY::var = 1; # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (belden-insert-as-new-line
   "use Carp qw(cluck carp); if ($MY::var) { local $Carp::MaxArgLen = local $Carp::MaxArgNums = 0; cluck; } # XXXXXXXXXXXXXXXXXXXXXXXXXXX");
  (save-buffer)
)

(defun belden-insert-end-here ()
  "insert __END__"
  (interactive)
  (belden-insert-as-new-line "__END__"))

(defun belden-deparse-perl ()
   "Deparse and add parenthesis to perl code in region"
   (interactive)
   (shell-command-on-region (mark) (point) "perl -MO=Deparse,-p,-sCi2"
     "*deparse*"))

(defun belden-perl-tidy ()
  "Run perltidy on a region"
  (interactive)
	(belden-shell-replace "perltidy --standard-output --maximum-line-length 120 -nola --tabs --add-whitespace --cuddled-else --paren-tightness 2 -"))

(defun belden-perl-tidy-sub ()
	"Perltidy the current sub"
	(interactive)
	(save-excursion (mark-defun)
									(belden-perl-tidy)))

(defun belden-devtool (tool &rest args)
  (concat (format "%s/bin/development-tools/%s" (getenv "HOME") tool) " " (list-join args)))

(defun belden-make-testfiles (class)
	"Create test files for an existing perl module"
  (interactive (list (read-string "Create .t for: "
                                  (module-on-point-or-current-module))))
  (let ((created-file (shell-command-to-string (belden-devtool "create_module_from_template" "--nomodule" class))))
    (if (file-exists-p created-file)
        (find-file created-file))))

(defun belden-make-perl-module (class)
	"Create a new perl module and associated test files"
  (interactive (list (read-string "Create .pm for: "
                                  (module-on-point-or-current-module))))
  (let ((created-file (shell-command-to-string (belden-devtool "create_module_from_template" class))))
    (if (file-exists-p created-file)
        (find-file created-file))))


;; Emacs Configuration File ;;

;; Load Path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; (normal-top-level-add-subdirs-to-load-path)


(require 'cl)

;; Initialize Color Theme
(add-to-list 'load-path "~/.emacs.d/packages/color-theme")
(require 'color-theme)
(eval-after-load "color-theme" '(progn (color-theme-initialize)))

;; Select Color Theme Wombat
(require 'color-theme-wombat)
(if window-system
   (color-theme-wombat))
(add-hook 'after-make-frame-functions 'color-theme-wombat)

;; Disabling the startup screen
(setq inhibit-startup-message t)

;; Disabling unwanted UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Toggle fullscreen
(defun toggle-fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'toggle-fullscreen)

;; Enable Ido Mode with some attributes
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;; Auto Complete
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(ac-config-default)

;; Open Emacs In Full Screen Mode By Default
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Scroll one line at a time
(setq scroll-step 1)

;; Show line and column numbers on minibuffer
(line-number-mode 1)
(column-number-mode 1)

;; Enable delete selection mode
(delete-selection-mode 1)

;; Avoid backup file creation
(setq make-backup-files nil)

;; Undo
(global-set-key (kbd "C-z") 'undo)

;; Default tab width for C
(setq-default c-basic-offset 8)

;; Default tab width
(setq-default tab-width 2)

;; Fontsize := 10
;; (set-face-attribute 'default nil :height 110)

;; Inserts 'TAB' character instead of spaces
(setq-default indent-tabs-mode t)

;; Uncomment Region
(global-set-key (kbd "C-c C-v") 'uncomment-region)

;; Comment Region
(global-set-key (kbd "C-c C-c") 'comment-region)

;; Displays given message in scratch by default
(setq initial-scratch-message ";; Scratch Buffer [Emacs @ Elixir]\n\n\n")

;; Display Persitent Time, Day and Date
(setq display-time-day-and-date t) (display-time)

;; Enable Copy To X-Clipboard
(setq x-select-enable-clipboard t)

;; Inserts 'SPACES' character instead of TAB
(setq-default indent-tabs-mode nil)

;; Enable Linum Mode globally
(global-linum-mode 1)

;; Show Parenthesis
(show-paren-mode 1)

;; Jump the pointer when it is close to the cursor
(mouse-avoidance-mode 'jump)

;; Answer with just Y/N
(fset 'yes-or-no-p 'y-or-n-p)

;; Switch Buffers
(global-set-key [s-right] 'next-buffer)
(global-set-key [s-left] 'previous-buffer)

;; Switch Frames
(global-set-key [s-tab] 'next-multiframe-window)
(global-set-key [S-s-iso-lefttab] 'previous-multiframe-window)

;; Enable clearing of buffer
(put 'erase-buffer 'disabled nil)

;; Remove WhiteSpace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
		(load
		 (expand-file-name "~/.emacs.d/elpa/package.el"))
	(package-initialize))

;; Rails-reloaded - Support Rails - Option to relaoded
;; Need to fix some issues
(setq load-path (cons (expand-file-name "~/.emacs.d/packages/rails-reloaded") load-path))
(require 'rails-autoload)

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/packages/cucumber")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Duplicate current line
(defun duplicate-current-line ()
	"Duplicate current line"
  (interactive)
  (let ((str (concat
    (buffer-substring (point)
      (save-excursion (end-of-line) (point)))
	      "\n"
	       (buffer-substring (save-excursion (beginning-of-line) (point))
				 (point)))))
    (insert str)))
(global-set-key "\C-cd" 'duplicate-current-line)

;; Duplicate current paragraph
(defun duplicate-paragraph()
  "Duplicate a paragraph"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion (forward-paragraph) (point))))
    (copy-region-as-kill beg end)
    (yank)))
(global-set-key "\C-cf" 'duplicate-paragraph)

;; Move lines up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; Use ibuffer instead of the default annoying one
(global-set-key (kbd "C-x C-b") 'ibuffer)

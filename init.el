;; Emacs Configuration File ;;

;; Load Path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; (normal-top-level-add-subdirs-to-load-path)


(require 'cl)

;; Emacs Package Repositories
(package-initialize)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Initialize Color Theme
;; (add-to-list 'load-path "~/.emacs.d/packages/color-theme")
;; (require 'color-theme)
;; (eval-after-load "color-theme" '(progn (color-theme-initialize)))

;; Select Color Theme Wombat
;; (require 'color-theme-wombat)
;; (if window-system
;; (color-theme-wombat))
;; (add-hook 'after-make-frame-functions 'color-theme-wombat)
(add-to-list 'load-path "~/.emacs.d/elpa/darcula-theme-20150629.235")
(require 'darcula-theme)
;; (set-frame-font "Inconsolata-14")

;; Disabling the startup screen
(setq inhibit-startup-message t)

;; Disabling unwanted UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-battery-mode 1)

;; Changing keybindings for Mac
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)

;; Toggle fullscreen
(defun toggle-fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'toggle-fullscreen)

;; Enable Ido Mode with some attributes
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 5
      confirm-nonexistent-file-or-buffer nil)

;; IdoMenu - Easy toggling between functions of a file
(add-to-list 'load-path "~/.emacs.d/packages/idomenu")
(require 'idomenu)
(autoload 'idomenu "idomenu" nil t)

;; Auto Complete
;; (add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
;; (ac-config-default)

;; Open Emacs In Full Screen Mode By Default
(set-frame-parameter nil 'fullscreen 'fullboth)

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
(setq-default c-basic-offset 4)

;; Default tab width
(setq-default tab-width 4)

;; Fontsize := 10
;; (set-face-attribute 'default nil :height 110)

;; Increasing the line spacing
(setq-default line-spacing 5)

;; Inserts 'TAB' character instead of spaces
(setq-default indent-tabs-mode t)

;; Confirm before exiting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Smarter function to comment-uncomment lines or region
(defun toggle-comment ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;; Toggle Comments of a Region or Line
(global-set-key (kbd "M-/") 'toggle-comment)
(global-set-key (kbd "C-c C-c") 'toggle-comment)

;; Displays given message in scratch by default
(setq initial-scratch-message ";; Scratch Buffer [Emacs @ Elixir]\n\n\n\n\n\n\n\n\n\n")

;; Display Persitent Time, Day and Date
(setq display-time-day-and-date t) (display-time)

;; Enable Copy To X-Clipboard
(setq x-select-enable-clipboard t)

;; Inserts 'SPACES' character instead of TAB
(setq-default indent-tabs-mode nil)

;; Enable Linum Mode globally
(setq linum-format " %3d |")
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
;; (global-set-key [M-right] 'next-buffer)
;; (global-set-key [M-left] 'previous-buffer)

;; Switch Frames
(global-set-key [C-tab] 'next-multiframe-window)

;; Enable clearing of buffer
(put 'erase-buffer 'disabled nil)

;; Remove WhiteSpace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs Package Repositories
(package-initialize)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Copy Current Line
(defun copy-line (arg)
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg))))
(global-set-key (kbd "C-c C-k") 'copy-line)

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
(global-set-key "\C-cr" 'duplicate-paragraph)

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

;; Search anything using 'anything' !! Extra configs for the mode
(add-to-list 'load-path "~/.emacs.d/packages/anything-config")
(require 'anything-config)

;; Configuring anything mode
;; (global-set-key (kbd "C-x b")
;;                 (lambda() (interactive)
;;                   (anything
;;                    :prompt "Switch to: "
;;                    :candidate-number-limit 10                 ;; up to 10 of each
;;                    :sources
;;                    '( anything-c-source-buffers               ;; buffers
;;                       anything-c-source-recentf               ;; recent files
;;                       anything-c-source-bookmarks             ;; bookmarks
;;                       anything-c-source-files-in-current-dir+ ;; current dir
;;                       anything-c-source-locate))))            ;; use 'locate'

;; Scale the font-size
(global-set-key [(C +)] 'text-scale-increase)
(global-set-key [(C -)] 'text-scale-decrease)

;; Custom Variables (Auto-Generated)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode java-mode malabar-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode ts-mode puppet-mode org-mode)))
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-indent-on-enter-key t)
 '(package-selected-packages
   (quote
    (company-go company exec-path-from-shell yaml-mode ruby-mode puppet-mode projectile paredit jump js2-mode ido-vertical-mode haml-mode go-mode go-autocomplete darcula-theme css-mode autopair)))
 '(user-mail-address "piyushmnimbalkar@gmail.com"))

;; Js2 Mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Yaml Mode
(add-to-list 'load-path "~/.emacs.d/elpa/yaml-mode-0.0.5")
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Yasnippet Mode
(require 'yasnippet)
(yas/global-mode 1)
(global-set-key [s-tab] 'yas/expand)


;; Zencoding Mode
(add-to-list 'load-path "~/.emacs.d/packages/zencoding/")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(require 'zencoding-trie)

;; Golden Ratio
;; (add-to-list 'load-path "~/.emacs.d/packages/golden-ratio")
;; (require 'golden-ratio)
;; (golden-ratio-enable)

;; Expand Region Mode
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Smex
(add-to-list 'load-path "~/.emacs.d/packages/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Org Mode
(add-to-list 'load-path "~/.emacs.d/packages/org-mode-crate")
(setq org-directory "~/Dropbox/Notes")
(require 'org-mode-crate-init)
(global-set-key (kbd "C-c t") 'org-set-tags)
(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

;; Magit Mode
;; (add-to-list 'load-path "~/.emacs.d/elpa/magit-1.2.0")
;; (require 'magit)
;; (global-set-key (kbd "C-c C-g") 'magit-status)

;; Paredit Mode
(add-to-list 'load-path "~/.emacs.d/elpa/paredit-22")
(require 'paredit)

;; Browse-Kill-Ring
(add-to-list 'load-path "~/.emacs.d/packages/browse-kill-ring")
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;; Prettify JSON
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; Slime support
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/packages/slime/")
(require 'slime)
(slime-setup '(slime-fancy))

;; Initiate the autopair mode globally
(autopair-global-mode)

;; Avoid scroll-jumping because it's really irritating
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Activate the highlight current line mode by default
(global-hl-line-mode 1)

;; Protobuf Mode
(add-to-list 'load-path "~/.emacs.d/packages/protobuf")
(require 'protobuf-mode)
(defun proto-mode-hook ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.proto\\'" buffer-file-name))
    (protobuf-mode)))
(add-hook 'find-file-hook 'proto-mode-hook)

;; Golang Mode
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; (auto-complete-mode 1))                         ; Enable auto-complete mode
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; (with-eval-after-load 'go-mode
  ;; (require 'go-autocomplete))

(add-to-list 'load-path "~/.emacs.d/packages/go-guru")
(require 'go-guru)

;; Projectile Mode
(projectile-mode)
(defun my-switch-project-hook ()
  (go-set-project))
(add-hook 'projectile-after-switch-project-hook 'my-switch-project-hook)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "M-N") 'projectile-find-file)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(server-start)

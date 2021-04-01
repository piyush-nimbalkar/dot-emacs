;; Emacs Configuration File ;;

;; Load Path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; (normal-top-level-add-subdirs-to-load-path)


(require 'cl)

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

;; Delete word backward
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Default tab width for C
(setq-default c-basic-offset 4)

;; Default tab width
(setq-default tab-width 2)

;; Fontsize := 10
;; (set-face-attribute 'default nil :height 110)

;; Increasing the line spacing
(setq-default line-spacing 5)

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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

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
 '(custom-safe-themes
   (quote
    ("a91f190d93bc5d70003c9fa41282574f54ca602c5ad129d1bf559e63d3db6410" "45d65333e3deca701eb04e8404246e77a7ada931492477f0923c89c68843e2aa" "a53718620df861f3a8061867f9952201fcb27e08d3aad97fded557fe3399e16d" "ea102a31354bb3a348ac14ab24795458c43b9026e3962bb1b1c1ff35b27c101a" "ecda3de644eb6ac58b03eb981b5afed90d398ffe868f59d36c79bd1fe9037c10" "82b67c7e21c3b12be7b569af7c84ec0fb2d62105629a173e2479e1053cff94bd" default)))
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(js-indent-level 4)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-indent-on-enter-key t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet lsp-go company-lsp lsp-mode ruby-mode puppet-mode projectile paredit jump js2-mode ido-vertical-mode idea-darkula-theme haml-mode exec-path-from-shell darcula-theme css-mode company-go)))
 '(user-mail-address "piyushmnimbalkar@gmail.com")
 '(yas-global-mode t))

;; Js2 Mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

(add-to-list 'load-path "~/.emacs.d/packages/go-guru")
(require 'go-guru)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Projectile Mode
(projectile-mode)
(defun my-switch-project-hook ()
  (go-set-project))
(add-hook 'projectile-after-switch-project-hook 'my-switch-project-hook)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "M-N") 'projectile-find-file)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
(setq company-idle-delay .1)
(setq company-minimum-prefix-length 2)

(setq-default fill-column 80)

;; Disables pinging when you try to ido find-file at point
(setq ffap-machine-p-known 'reject)

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

(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:inherit hl-line :foreground "color-243"))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-preview-search ((t (:inherit company-preview :background "#00707d")))))

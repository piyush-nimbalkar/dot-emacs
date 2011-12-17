;; Emacs Configuration File ;;

;; Load Path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(normal-top-level-add-subdirs-to-load-path)

;; Initialize Color Theme
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

;; Switch Buffers
(global-set-key [s-right] 'next-buffer)
(global-set-key [s-left] 'previous-buffer)

;; Enable clearing of buffer
(put 'erase-buffer 'disabled nil)

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
		(load
		 (expand-file-name "~/.emacs.d/elpa/package.el"))
	(package-initialize))

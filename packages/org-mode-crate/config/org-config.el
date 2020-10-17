;;; org-config.el --- Configuration for org-mode
;;; Author: Vedang Manerikar
;;; Created on: 11 Mar 2012
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


;; Setup directory and file paths for org
(setq org-archive-directory (concat org-directory "/archive")
      org-archive-location (concat org-archive-directory "/%s_archive::")
      org-default-notes-file (concat org-directory "/refile.org")
      org-agenda-files (list org-directory))


;; Ido for the win
(setq org-completion-use-ido t)


;; Auto starting org-mode for following file types
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(transient-mark-mode 1)

;; Undefine C-c [ and C-c ] since this breaks my org-agenda files
;; when directories are included
;; It expands the files in the directories individually
(add-hook 'org-mode-hook (lambda ()
                           (turn-on-font-lock)
                           (org-defkey org-mode-map "\C-c[" 'undefined)
                           (org-defkey org-mode-map "\C-c]" 'undefined)))


;; Settings for org-capture
(setq org-capture-templates
      '(("t" "todo" entry
         (file org-default-notes-file)
         "* TODO %?  :refile:\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("d" "today" entry
         (file org-default-notes-file)
         "* TODO %?  :today:\n")
        ("n" "note" entry
         (file org-default-notes-file)
         "* %?  :refile:note:\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("l" "linklog" entry
         (file (concat org-directory "/linklog.org"))
         "* %?  :refile:\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("p" "phone" entry
         (file org-default-notes-file)
         "* DONE %?  :refile:phone:\n%U\n%a\n %i" :clock-in t :clock-resume t)))


;; refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 4)
                           (nil :maxlevel . 4))
      ;; Targets start with the file name - allows creating level 1 tasks
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)


;; org-todo settings
;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "WORKING(w!/!)" "WAITING(a@/!)" "|" "DONE(d!/@)" "|" "SOMEDAY(s!/@)")))


(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("WORKING" :foreground "orange" :weight bold)
        ("WAITING" :foreground "lightblue" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("DONE" :foreground "lightgreen" :weight bold)))


;; Changing State should trigger following Tag changes
(setq org-todo-state-tags-triggers
      '(("SOMEDAY"
         ("waiting" . t) ("next"))
        (done
         ("next") ("waiting"))
        ("WAITING"
         ("next") ("waiting" . t))
        ("TODO"
         ("waiting"))
        ("WORKING"
         ("waiting") ("next" . t))))


(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "waiting")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


;; Other todo related settings
(setq org-use-fast-todo-selection t
      org-fast-tag-selection-single-key 'expert
      ;; Allow me to change state without it being logged
      org-treat-S-cursor-todo-selection-as-state-change nil
      ;; show TODO counts of _all_ subtasks under a heading
      org-hierarchical-todo-statistics nil
      org-hierarchical-checkbox-statistics nil
      org-enforce-todo-dependencies t)


;; (dolist (map (list org-agenda-keymap org-agenda-mode-map))
  ;; (define-prefix-command 'org-todo-state-map)
  ;; (define-key map "x" 'org-todo-state-map)

  ;; (define-key org-todo-state-map "d"
    ;; #'(lambda nil (interactive) (org-agenda-todo "DONE")))
  ;; (define-key org-todo-state-map "x"
    ;; #'(lambda nil (interactive) (org-agenda-todo "CANCELLED")))

  ;; These functions are defined later in the file.
  ;; (define-key org-todo-state-map "D" #'fc/org-agenda-inherit-deadline))


;;org-tags
;; Important Tag list
(setq org-tag-alist '(("next" . ?x)
                      ("note" . ?N)
                      ("study" . ?s)
                      ("goal" . ?g)
                      ("today" . ?t)
                      ("write" . ?w)
                      ("essential" . ?e)
                      ("waiting" . ?a)
                      ("fun" . ?f)))


;; org-priorities
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)


;; Logbook settings
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-reschedule 'note
      org-log-redeadline 'note)


;; settings for org-clock
(org-clock-persistence-insinuate)
(setq org-clock-history-length 10
      org-clock-in-resume t
      org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK")
      org-clock-into-drawer "CLOCK"
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist 'history
      org-clock-persist-file (concat org-directory "/org-clock-save")
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t)


;; List of TODO states to clock-in
(setq vm/todo-list '("TODO" "WAITING"))


(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))
(global-set-key (kbd "<f9> h") 'bh/hide-other)


;; Change task state to WORKING when clocking in
(defun bh/clock-in-to-working (kw)
  "Switch task from TODO to WORKING when clocking in.
Skips capture tasks and tasks with subtasks"
  (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
             (member kw vm/todo-list))
    "WORKING"))

(setq org-clock-in-switch-to-state 'bh/clock-in-to-working)


;; Remove empty drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


(setq bh/keep-clock-running nil)


(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)


(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))
(global-set-key (kbd "<f9> i") 'bh/punch-in)


(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))
(global-set-key (kbd "<f9> o") 'bh/punch-out)


(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))


(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))


(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (when (boundp 'bh/organization-task-id)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16)))))


(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))


(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


;; org-agenda
;; Custom views for Agenda
(setq org-agenda-custom-commands
      (quote (("a" "The Agenda"
               ((tags-todo "+today"
                           ((org-agenda-overriding-header
                             "Complete these tasks today")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (agenda "" ((org-agenda-overriding-header
                             "Deadlines and Scheduled")))
                (tags-todo "+release-future|+next-future|+imp-future"
                           ((org-agenda-overriding-header
                             "Do These Tasks Next")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags-todo "fun"
                           ((org-agenda-overriding-header
                             "Other Fun Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))
                (tags "refile"
                      ((org-agenda-overriding-header
                        "Notes and Tasks to Refile")))
                nil))
              ("c" "Select default clocking task" tags "LEVEL=1-refile"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\* Organization"))
                (org-agenda-overriding-header
                 "Set default clocking task with C-u C-u I")))
              ("d" "Delegated Tasks" todo "DELEGATED"
               ((org-use-tag-inheritance nil)
                (org-agenda-todo-ignore-with-date nil)))
              ("I" "Inheritable Deadlines" todo "TODO|WAITING|IN-REVIEW|WORKING|SOMEDAY"
                    ((org-agenda-overriding-header "Inheritable DEADLINEs")
                     (org-agenda-skip-function 'fc/skip-non-inheritable-deadlines))))))


;;; http://article.gmane.org/gmane.emacs.orgmode/49215
(defun fc/has-inheritable-deadline-p ()
  "Any task (without DEADLINE) that can inherit a DEADLINE"
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (inheritable-deadline (org-entry-get-with-inheritance "DEADLINE")))

    (if (org-not-nil deadline)
        nil
      (if (org-not-nil inheritable-deadline)
          t
        nil))))


(defun fc/skip-non-inheritable-deadlines ()
  "Skip tasks that cannot inherit a DEADLINE"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (fc/has-inheritable-deadline-p)
        nil
      next-headline)))


(defun fc/org-inherit-deadline ()
  "Inherit a DEADLINE."
  (interactive)
  (let* ((deadline (org-entry-get-with-inheritance "DEADLINE")))
    (if (and (org-not-nil deadline)
             (y-or-n-p (format "Inherit DEADLINE: <%s>? " deadline)))
        (org-deadline nil (org-time-string-to-time deadline)))))


(defun fc/org-agenda-inherit-deadline (&optional arg)
  "Inherit a DEADLINE in agenda."
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (hdmarker (or (org-get-at-bol 'org-hd-marker)
                       marker))
         (pos (marker-position marker))
         newhead)
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-show-entry)
        (org-cycle-hide-drawers 'children)
        (fc/org-inherit-deadline)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))


;; Always highlight current agenda line
(add-hook 'org-agenda-mode-hook '(lambda ()
                                   (hl-line-mode 1)))

(setq org-agenda-repeating-timestamp-show-all nil
      org-agenda-show-all-dates t
      org-agenda-sorting-strategy
      '((agenda time-up priority-down effort-up category-up)
       (todo todo-state-up priority-up)
       (tags priority-down))
      org-agenda-start-on-weekday nil
      org-agenda-time-grid
      '(nil "----------------"
           (800 1000 1200 1400 1600 1800 2000))
      org-deadline-warning-days 30
      org-agenda-todo-ignore-with-date t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-text-search-extra-files '(agenda-archives)
      org-agenda-log-mode-items '(clock closed state)
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t
                                                     :compact t :narrow 80)
      org-agenda-span 1
      org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
      org-global-properties
      '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
        ("STYLE_ALL" . "habit"))
      org-agenda-clock-consistency-checks
      '(:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around ("4:00")))


;; settings for Reminder
;; Erase all reminders and rebuild reminders for today from the agenda
(defadvice org-agenda-to-appt (before wickedcool activate)
  "Clear the appt-time-msg-list."
  (setq appt-time-msg-list nil))

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(appt-activate t)

;; If we leave Emacs running overnight -
;; reset the appointments one minute after midnight
(run-at-time "24:01" nil 'org-agenda-to-appt)


;; Settings for org-table
;; Export org table as CSV by default
(setq org-table-export-default-format "orgtbl-to-csv")


(setq org-link-abbrev-alist
      '(("bug" . "http://bugzilla.helpshift.com/show_bug.cgi?id=%s")
        ("google"   . "http://www.google.com/search?q=%s")))


(provide 'org-config)
;; A big thanks to Bernt Hansen for providing an awesome guide to
;; beginners so that we can harness the power of org-mode. Almost all of the
;; customization here, and my complete day-to-day workflow,
;; is based on his document about org-mode which can be
;; found here: http://doc.norang.ca/org-mode.html

;; -*- lexical-binding: t -*-
(require 'jiralib2)
(require 'magit-section)
(require 's)
(require 'popup)

(defvar jiradash-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map "o" 'jiradash-cmd-open-browser)
    (define-key map "a" 'jiradash-cmd-action)
    (define-key map "g" 'jiradash)
    map)
  "Keymap for jiradash mode")

(define-derived-mode jiradash-mode magit-section-mode "JiraDash"
  "Mode for looking at your JIRA dashboard.")

(defcustom jiradash-dashboard-jql
  "resolution = Unresolved and status != closed and assignee = currentUser()"
  "The JIRA JQL query to run to fetch dashboard items")

(defun jiradash-issue-at-point ()
  (alist-get 'jiradash-issue (magit-section-ident (magit-current-section))))

(defun jiradash-cmd-open-browser ()
  (interactive)
  (message "Open in URL: %s" (jiradash-issue-at-point)))
;(browse-url-generic "https://google.com")

(defun jiradash-cmd-action ()
  (interactive)
  (message "Section: %s" (magit-section-ident (magit-current-section))))


(defun jiradash-issues ()
  (jiralib2-jql-search jiradash-dashboard-jql "summary" "description" "status"))

(defmacro with-jiradash-buffer (&rest body)
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*jiradash*")))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (jiradash-mode)
         (magit-insert-section (jiradash-buffer)
           ,@body)))
     (when (not (equal (current-buffer) buffer))
       (switch-to-buffer-other-window buffer))))

(defun jiradash-cleanup-text (text)
  ;; JIRA returns ^M at linefeeds
  (s-replace "" "" (or text "")))

(defun jiradash-api-url->browse-url (url issue)
  ;; This assumes that all JIRA installations have: https://jira.example.com/browse/<ISSUE>
  (format "%s/browse/%s" (cadr (s-match "^\\(.*\\)/rest/api/.*$" url)) issue))

(defun jiradash-fetch-and-insert-issue (key)
  (message "Fetch issue %s" key)
  (let* ((issue (jiralib2-get-issue key))
         (fields (alist-get 'fields issue))
         (actions (jiralib2-get-actions key)))
    (insert-button "Open in browser"
                   'action (lambda (_button)
                             (message "Opening issue %s in browser" key)
                             (browse-url-generic (jiradash-api-url->browse-url
                                                  (alist-get 'self issue)
                                                  key)))
                   'help-echo (format "Open %s in browser" key))
    (insert "\n")
    (when (> (length actions) 0)
      (insert "Actions: ")
      (dolist (action actions)
        (insert-button (cdr action)
                       'action (lambda (_button)
                                 (message "run action %s on %s" action key)
                                 ;; TODO: run action
                                 )
                       'help-echo (format "Transition %s to %s" key action)
                       )
        (insert " "))
      (insert "\n"))

    (magit-insert-section (jiradash-issue-description)
      (magit-insert-heading "Description")
      (magit-insert-section-body
        (jiradash-cleanup-text (alist-get 'description fields))))
    (let ((comments (alist-get 'comments (alist-get 'comment fields))))
      (magit-insert-section (jiradash-issue-comments nil t)
        (magit-insert-heading (format "Comments (%d)" (length comments)))
        (magit-insert-section-body
          (dolist (comment comments)
            (let ((author (alist-get 'displayName (alist-get 'author comment)))
                  (body (jiradash-cleanup-text (alist-get 'body comment))))
              (insert (format "%s: %s\n-------\n" author body)))))))
    (insert "\n")))


(defun jiradash-issue-section (issue)
  (let* ((key (alist-get 'key issue))
         (fields (alist-get 'fields issue))
         (summary (alist-get 'summary fields))
         (status (alist-get 'status fields))
         (status-name (alist-get 'name status)))
    (magit-insert-section (jiradash-issue key t)
      (magit-insert-heading (format "%-10s %-25s  %s\n" key status-name summary))
      (magit-insert-section-body
        (jiradash-fetch-and-insert-issue key)))))

(defun jiradash ()
  (interactive)
  (let ((issues (jiradash-issues)))
    (with-jiradash-buffer
      (magit-insert-section (jiradash-issues)
        (magit-insert-heading
          (format "JIRA dashboard, %d issues" (length issues)))
        (magit-insert-section-body
          (dolist (issue issues)
            (jiradash-issue-section issue)))))))

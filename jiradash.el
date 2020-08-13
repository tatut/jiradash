;; -*- lexical-binding: t -*-
(require 'jiralib2)
(require 'magit-section)
(require 's)
(require 'dash)

(defvar jiradash-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map "o" (lambda ()
                          (interactive)
                          (jiradash-cmd-activate-button "o")))
    (dotimes (i 7)
      (let ((hotkey (format "%d" i)))
        (define-key map hotkey (lambda ()
                                 (interactive)
                                 (jiradash-cmd-activate-button hotkey)))))
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

(defun jiradash-cmd-activate-button (hotkey)
  (message "HOTKEY: %s" hotkey)
  ;; Find the current jiradash-issue section
  (let ((section (magit-current-section)))
    (while (and (not (null section))
                (not (equal (caar (magit-section-ident section)) 'jiradash-issue)))
      (setq section (oref section parent)))

    (let ((buttons-section (-some (lambda (section)
                                    (when (equal 'jiradash-buttons (caar (magit-section-ident section)))
                                      section))
                                  (oref section children))))
      (with-slots (start end) buttons-section
        (let ((pos (marker-position start))
              (end-pos (marker-position end))
              (found-button nil))
          (while (and (null found-button)
                      (< pos end-pos))
            (let ((button (button-at pos)))
              (if (and button
                       (equal (button-get button 'jiradash-hotkey) hotkey))
                  (setq found-button button)
                (setq pos (+ pos 1)))))
          (if found-button
              (button-activate found-button)
              (message "No button found for hotkey: %s" hotkey)))))))

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

    (magit-insert-section (jiradash-buttons)
      (magit-insert-section-body
        (button-put
         (insert-button "(o) Open in browser"
                        'action (lambda (_button)
                                  (message "Opening issue %s in browser" key)
                                  (browse-url-generic (jiradash-api-url->browse-url
                                                       (alist-get 'self issue)
                                                       key)))
                        'help-echo (format "Open %s in browser" key))
         'jiradash-hotkey "o")
        (insert "\n")
        (when (> (length actions) 0)
          (insert "Actions: ")
          (dotimes (i (length actions))
            (let ((action (nth i actions))
                  (hotkey (format "%d" (+ i 1))))
              (button-put (insert-button (format "(%d) %s" (+ i 1) (cdr action))
                                         'action (lambda (_button)
                                                   (message "run action %s on %s" (cdr action) key)
                                                   (jiralib2-do-action key (car action)))
                                         'help-echo (format "Transition %s to %s" key action))
                          'jiradash-hotkey hotkey)
              (insert " ")))
          (insert "\n"))))

    (magit-insert-section (jiradash-issue-description)
      (magit-insert-heading "Description")
      (magit-insert-section-body
        (insert (jiradash-cleanup-text (alist-get 'description fields)))
        (insert "\n")))
    (let ((comments (alist-get 'comments (alist-get 'comment fields))))
      (magit-insert-section (jiradash-issue-comments nil t)
        (magit-insert-heading (format "Comments (%d)" (length comments)))
        (magit-insert-section-body
          (dolist (comment comments)
            (let ((author (alist-get 'displayName (alist-get 'author comment)))
                  (body (jiradash-cleanup-text (alist-get 'body comment))))
              (insert (format "%s: %s\n" author body)))))))
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

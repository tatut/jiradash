;; -*- lexical-binding: t -*-
(require 'jiralib2)
(require 'magit-section)
(require 's)
(require 'dash)

(defconst jiradash-hotkeys '("o" "a" "1" "2" "3" "4" "5" "6"))

(defvar jiradash-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (dolist (h jiradash-hotkeys)
      (define-key map h 'jiradash-cmd-activate-button))
    (define-key map "g" 'jiradash)
    map)
  "Keymap for jiradash mode")

(define-derived-mode jiradash-mode magit-section-mode "JiraDash"
  "Mode for looking at your JIRA dashboard.")

(defcustom jiradash-dashboards
  '(((name . "My open issues")
     (jql . "resolution = Unresolved and status != closed and assignee = currentUser() and Sprint in openSprints()"))

    ((name . "Current sprint issues")
     (jql . "sprint in openSprints()"))

    ((name . "Backlog and future issues")
     (jql . "(sprint = null or sprint in futureSprints()) and status != Closed")))
  "The JIRA dashboards to show"
  :type '(list (alist :key-type string :value-type string)))

(defun jiradash-issue-at-point ()
  (alist-get 'jiradash-issue (magit-section-ident (magit-current-section))))

(defun jiradash-cmd-activate-button ()
  (interactive)
  ;; Find the current jiradash-issue section
  (let ((hotkey (this-command-keys))
        (section (magit-current-section)))
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

(defun jiradash-issues (jql)
  (jiralib2-jql-search jql "summary" "description" "status"))

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

(defun jiradash-get-likely-assignable-users (key)
  (let ((project (car (split-string key "-"))))
    (mapcar (lambda (issue)
              (alist-get 'assignee (alist-get 'fields issue)))
            (jiralib2-jql-search (format "project = %s and assignee is not null" project) "assignee"))))

(defun jiradash-assign-issue (key)
  (let* ((assignable-users (jiradash-get-likely-assignable-users key)) ; PENDING: configurable (jiralib2-get-assignable-users key)
         (assignee (completing-read (format "Assign %s to: " key)
                                    (cl-concatenate 'list
                                                    (list "unassigned")
                                                    (mapcar (lambda (user)
                                                              (alist-get 'displayName user))
                                                            assignable-users)))))
    (message "Assigning %s to %s" key assignee)
    (jiralib2-assign-issue key (if (string= "unassigned" assignee)
                                   nil
                                 (-some (lambda (user)
                                          (let ((display-name (alist-get 'displayName user)))
                                            (when (string= display-name assignee)
                                              (alist-get 'name user))))
                                        assignable-users)))))

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
        (insert " ")
        (button-put (insert-button "(a) Assign"
                                   'action (lambda (_button)
                                             (jiradash-assign-issue key))
                                   'help-echo (format "Set new assignee for %s" key))
                    'jiradash-hotkey "a")
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
  (with-jiradash-buffer
    (dolist (dashboard jiradash-dashboards)
      (let ((dashboard-name (alist-get 'name dashboard))
            (dashboard-jql (alist-get 'jql dashboard)))
        (magit-insert-section (jiradash-issues dashboard-name t)
          (magit-insert-heading dashboard-name)
          (magit-insert-section-body
            (let ((issues (jiradash-issues dashboard-jql)))
              (insert (format "%d issues\n" (length issues)))
              (dolist (issue issues)
                (jiradash-issue-section issue)))))))))

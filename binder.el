;;; binder.el --- major mode for structuring multi-file projects  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Paul W. Rankin

;; Author: Paul W. Rankin <code@paulwrankin.com>
;; Keywords: files, outlines

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 


;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup binder ()
  "Work with a structured project of files."
  :group 'files)


;;; Options

(defcustom binder-mode-lighter " B/"
  "Mode-line indicator for `binder-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'binder)

(defcustom binder-default-file ".binder.el"
  "Default file in which to store Binder project data."
  :type 'string
  :safe 'stringp
  :group 'binder)

(defcustom binder-save-threshold
  15
  "Integer of changes before binder file is automatically saved."
  :type 'integer
  :safe 'integerp
  :group 'binder)


;;; Faces

(defgroup binder-sidebar-faces ()
  "Default faces for `binder-sidebar-mode'."
  :group 'binder-sidebar)

(defface binder-sidebar-marked
  '((t (:inherit (warning))))
  "Default face marked items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-highlight
  '((t (:inherit (secondary-selection))))
  "Default face for highlighted items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-missing
  '((t (:inherit (trailing-whitespace))))
  "Default face for missing items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-status
  '((t (:inherit (font-lock-variable-name-face))))
  "Default face for status labels."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-delete
  '((t (:inherit (error))))
  "Default face for items marked for removal."
  :group 'binder-sidebar-faces)


;;; Core Variables

(defvar binder-file-header
  "\
;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.\n\n"
  "Header information for binder-file.")

(defvar binder--cache nil)
(defvar binder--directory nil)
(defvar binder--modification-time nil)
(defvar binder--modification-count 0)
(defvar binder--marked-fileids nil)


;;; Core Functions

(defun binder-root ()
  "Find the root directory with a binder file."
  (locate-dominating-file default-directory binder-default-file))

(defun binder-init-binder-file ()
  "Initialize a binder file."
  (unless (binder-root)
    (when (y-or-n-p (format "Create %s in %s? " binder-default-file
                            (abbreviate-file-name default-directory)))
      (let ((binder-file
             (expand-file-name binder-default-file default-directory)))
      (with-temp-buffer
        (insert binder-file-header
                (pp-to-string
                 (list (cons 'structure nil)
                       (cons 'default-mode binder-default-multiview-mode)
                       (cons 'default-extension binder-default-file-extention))))
        (write-file binder-file))
      binder-file))))

(defun binder-find-binder-file ()
  "Find current binder file."
  (let ((binder-file (expand-file-name binder-default-file (binder-root))))
    (if (file-readable-p binder-file)
        binder-file
      (binder-init-binder-file))))

(defun binder-read ()
  "Read current binder data.
Reads from `binder--cache' if valid, or from binder file if not."
  (let ((binder-file (binder-find-binder-file)))
    (unless binder-file
      (user-error "No binder file found"))
    (unless (and (string= default-directory (get 'binder--cache 'directory))
                 (time-less-p (file-attribute-modification-time
                               (file-attributes binder-file))
                              (get 'binder--cache 'modification-time)))
      (with-temp-buffer
        (insert-file-contents binder-file)
        (setq binder--cache (read (current-buffer)))))
    (put 'binder--cache 'directory default-directory)
    (put 'binder--cache 'modification-time (current-time)))
  binder--cache)

(defun binder-write ()
  "Write binder data to file."
  (interactive)
  (mapc (lambda (item)
          (rassq-delete-all "" item))
        (binder-get-structure))
  (let ((binder-file (binder-find-binder-file)))
    (with-temp-buffer
      (insert binder-file-header (pp-to-string binder--cache))
      (write-file binder-file))))

(defun binder-write-maybe ()
  (if (< binder--modified-count binder-save-threshold)
      (cl-incf binder--modified-count)
    (binder-write)
    (setq binder--modified-count 0)))

(defun binder-file-relative-to-root (fileid)
  "Return FILEID relative to binder root directory."
  (let ((root (binder-root)))
    (when root
      (string-trim (expand-file-name fileid)
                   (expand-file-name root)))))

(defun binder-get-structure ()
  "Return binder data structure component."
  (alist-get 'structure (binder-read)))

(defun binder-get-prop-list (prop)
  "Return list of values for PROP."
  (delq nil
        (mapcar
         (lambda (item)
           (let ((value (alist-get prop item)))
             (when (and (stringp value) (< 0 (string-width value)))
               value)))
         (binder-get-structure))))

(defun binder-get-item (fileid)
  "Return binder item association list for FILEID."
  (assoc-string fileid (binder-get-structure)))

(defun binder-get-item-prop (fileid prop)
  "Return value of PROP for binder item for FILEID."
  (alist-get prop (cdr (binder-get-item fileid))))

(defun binder-set-item-prop (fileid prop value)
  "Set VALUE of PROP for binder item for FILEID."
  (let ((item (binder-get-item fileid)))
    (if (string-empty-p value)
        (assq-delete-all prop item)
      (let ((prop-elt (assq prop item)))
        (if prop-elt
            (setcdr prop-elt value)
          (push (cons prop value) (cdr item))))))
  (put 'binder--cache 'modification-time (current-time)))

(defun binder-get-item-index (fileid)
  (seq-position (binder-get-structure) (binder-get-item fileid) 'eq))

(defun binder-insert-item (item index)
  "Insert binder ITEM at position INDEX."
  (let ((structure (binder-get-structure)))
    (unless (listp item) (setq item (list item)))
    (setcdr (assq 'structure (binder-read))
            (nconc (seq-take structure index)
                   (cons item (seq-drop structure index)))))
  (put 'binder--cache 'modification-time (current-time))
  (binder-write-maybe))

(defun binder-delete-item (fileid)
  "Delete binder item for FILEID."
  (setcdr (assq 'structure (binder-read))
          (delq (binder-get-item fileid) (binder-get-structure)))
  (put 'binder--cache 'modification-time (current-time))
  (binder-write-maybe))

;; FIXME: unused for flat file structure
(defun binder-get-file-tree (file)
  (setq file (abbreviate-file-name (expand-file-name file)))
  (when (file-exists-p file)
    (let ((last-file (file-name-nondirectory file))
          (root (binder-root))
          tree)
      (while (not (or (file-equal-p file root)
                      (null file)
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq file (file-name-directory (directory-file-name file))
              tree (list (file-name-nondirectory
                          (directory-file-name file))
                         tree)))
      tree)))


;;; Global Minor Mode

(defun binder-next (&optional n)
  "Goto Nth next file in binder.
Or goto Nth previous file if N is negative."
  (interactive "p")
  ;; FIXME: error on dired buffers
  (let ((this-file (or (buffer-file-name) default-directory))
        (struct (binder-get-structure))
        item index next-index next-file)
    (setq this-file (binder-file-relative-to-root this-file)
          item (binder-get-item this-file))
    (if (not item)
        (user-error "%S not in a binder" this-file)
      (setq index (seq-position struct item 'eq)
            next-index (+ index n))
      (if (and (<= 0 next-index)
               (< next-index (length struct)))
          (and
           (setq next-file (expand-file-name (car (nth next-index struct))
                                             (binder-root)))
           (find-file-existing next-file))
        (message "End of binder"))
      (unless overriding-terminal-local-map
        (let ((keys (substring (this-single-command-keys) 0 -1))
              (map (cdr binder-mode-map)))
          (mapc (lambda (k) (setq map (assq k map))) keys)
          (when (consp map) (set-transient-map (cdr map) t)))))))

(defun binder-previous (&optional n)
  "Visit Nth previous file in binder.
Or visit Nth next file if N is negative."
  (interactive "p")
  (binder-next (- n)))

(defun binder-add (fileid)
  (interactive "FAdd file (extension optional): ")
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (< 0 (string-width fileid))
    (user-error "No file name supplied"))
  (unless (or (directory-name-p fileid)
              (and (string-match "\\.[^.]+\\'" fileid)
	               (not (= 0 (match-beginning 0)))))
    (setq fileid
          (concat fileid "." (alist-get 'default-extension (binder-read)))))
  (let ((filepath
         (expand-file-name fileid (binder-root)))
        (this-file-index
         (when (buffer-file-name)
           (binder-get-item-index
            (binder-file-relative-to-root (buffer-file-name)))))
        index)
    (unless (file-exists-p filepath)
      (if (directory-name-p filepath)
          (make-directory filepath)
        (with-current-buffer (find-file-noselect filepath)
          (write-file filepath))))
    (setq index
          (cond (this-file-index
                 (1+ this-file-index))
                ((eq major-mode 'binder-sidebar-mode)
                 (1+ (binder-sidebar-get-index)))
                (t (length (binder-get-structure)))))
    (unless (binder-get-item fileid)
      (binder-insert-item fileid index))
    (with-current-buffer (get-buffer-create binder-sidebar-buffer)
      (binder-sidebar-refresh)
      (binder-write-maybe)
      (binder-sidebar-goto-item fileid))
    (find-file filepath)))

(defvar binder-mode-map (make-sparse-keymap))

(define-key binder-mode-map (kbd "C-c ]") #'binder-next)
(define-key binder-mode-map (kbd "C-c [") #'binder-previous)
(define-key binder-mode-map (kbd "C-c ;") #'binder-toggle-sidebar)
(define-key binder-mode-map (kbd "C-c :") #'binder-add)

;;;###autoload
(define-minor-mode binder-mode
  "Globally interact with `binder'."
  :init-value nil
  :lighter binder-mode-lighter
  :global t)


;;; Sidebar Major Mode

(defgroup binder-sidebar ()
  "Options for `binder-sidebar-mode'."
  :group 'binder)

(defcustom binder-sidebar-buffer "*Binder Sidebar*"
  "Default buffer name for binder sidebar."
  :type 'string
  :safe 'stringp
  :group 'binder-sidebar)

(defcustom binder-sidebar-display-alist
  '((side . left)
    (window-width . 40)
    (slot . -1))
  "Association list used to display binder sidebar buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :safe (lambda (value)
          (and (listp value)
               (seq-every-p 'consp value)))
  :group 'binder-sidebar)

(defcustom binder-sidebar-hide-file-extensions
  nil
  "When non-nil, list items without file extension."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-pop-up-windows
  nil
  "Non-nil means displaying a new buffer should make a new window."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-status-column
  25
  "Integer for column to align status tags."
  :type 'integer
  :safe 'integerp
  :group 'binder-sidebar)

(defcustom binder-sidebar-include-char
  ?x
  "Character to display on items including in joining."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-notes-char
  ?*
  "Character to display on items with notes."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-status-char
  ?#
  "Character to prefix to item status."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-missing-char
  ??
  "Character to display on items with missing files."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-select-window
  t
  "If non-nil, switch to binder sidebar upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-persistent
  t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `binder-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defun binder-sidebar-refresh ()
  "Redraw binder sidebar, reading from cache."
  (interactive)
  (with-silent-modifications
    (let ((x (point)))
      (erase-buffer)
      (dolist (item (binder-get-structure))
        (let ((fileid (car item))
              (display (alist-get 'display item))
              (notes (alist-get 'notes item))
              (status (alist-get 'status item))
              marked missing)
          (when (member fileid binder--marked-fileids)
            (setq marked t))
          (when (not (file-exists-p fileid))
            (setq missing t))
          (insert (cond (marked ">")
                        (include binder-sidebar-include-char)
                        (t " "))
                  " "
                  (cond (missing binder-sidebar-missing-char)
                        ((and notes (not (string-empty-p notes)))
                         binder-sidebar-notes-char)
                        (t " "))
                  " "
                  (or display
                      (if (file-directory-p fileid)
                          (replace-regexp-in-string "/*$" "/" fileid)
                        (if binder-sidebar-hide-file-extensions
                            (replace-regexp-in-string ".+\\(\\..+\\)" ""
                                                      fileid nil nil 1)
                          fileid))))
          (put-text-property (line-beginning-position) (line-end-position)
                             'binder-fileid fileid)
          (put-text-property (line-beginning-position) (line-end-position)
                             'front-sticky '(binder-fileid))
          (when missing
            (put-text-property (line-beginning-position) (line-end-position)
                               'face 'binder-sidebar-missing))
          (when marked
            (put-text-property (line-beginning-position) (line-end-position)
                               'face 'binder-sidebar-marked))
          (when (and status (< 0 (string-width status)))
            (move-to-column binder-sidebar-status-column)
            (indent-to-column binder-sidebar-status-column)
            (let ((x (point)))
              (delete-region (1- x) (line-end-position))
              (insert " " binder-sidebar-status-char status)
              (put-text-property x (line-end-position)
                                 'face 'binder-sidebar-status)))
          (insert "\n")))
      (goto-char x))))

(defun binder-sidebar-get-fileid ()
  "Return fileid for item at point."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'binder-fileid)))

(defun binder-sidebar-create (dir)      ; FIXME: this does not create a buffer
  "Create binder sidebar buffer for DIR."
  (setq default-directory dir)
  (binder-sidebar-refresh)
  (binder-sidebar-mode))

;;;###autoload
(defun binder-toggle-sidebar ()
  "Toggle visibility of binder sidebar side-window."
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory))
    (if (get-buffer-window binder-sidebar-buffer (selected-frame))
        (delete-windows-on binder-sidebar-buffer (selected-frame))
      (with-current-buffer (get-buffer-create binder-sidebar-buffer)
        (binder-sidebar-create dir))
      (display-buffer-in-side-window
       (get-buffer binder-sidebar-buffer)
       (append binder-sidebar-display-alist
               (when binder-sidebar-persistent
                 (list '(window-parameters (no-delete-other-windows . t))))))
      (when binder-sidebar-select-window
        (select-window
         (get-buffer-window binder-sidebar-buffer (selected-frame)))))))

(defun binder-sidebar-find-file (arg)
  "Visit binder item at point.
When ARG is non-nil, visit in new window."
  (interactive "P")
  (let ((pop-up-windows (or arg binder-sidebar-pop-up-windows)))
    (find-file (binder-sidebar-get-fileid))))

(defun binder-sidebar-find-file-other-window ()
  "Visit binder item in other window."
  (interactive)
  (binder-sidebar-find-file t))

(defalias 'binder-sidebar-save 'binder-write)

(defun binder-sidebar-get-index ()
  "Return binder index position at point."
  (if (eobp)
      (length (binder-get-structure))
    (binder-get-item-index (binder-sidebar-get-fileid))))

(defun binder-sidebar-mark ()
  "Mark the binder item at point."
  (interactive)
  (cl-pushnew (binder-sidebar-get-fileid) binder--marked-fileids)
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark ()
  "Unmark the binder item at point."
  (interactive)
  (setq binder--marked-fileids
        (delete (binder-sidebar-get-fileid) binder--marked-fileids))
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark-all ()
  "Unmark all binder items."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid)))
    (goto-char (point-min))
    (while (not (eobp))
      (binder-sidebar-unmark))
    (binder-sidebar-goto-item fileid)))

(defun binder-sidebar-add-file (fileid)
  "Add (possibly non-existent) file to binder as FILEID."
  (interactive "FAdd file: ")
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (binder-get-item fileid)
    (binder-insert-item fileid (1+ (binder-sidebar-get-index))))
  (put 'binder--cache 'modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-add-all-files ()
  "Add all files in current directory to binder."
  (interactive)
  (when (y-or-n-p (format "Add all files in %s" (abbreviate-file-name default-directory)))
    (dolist (file (directory-files default-directory nil "^[^.]"))
      (binder-sidebar-add-file file))))

(defun binder-sidebar-new-file (fileid)
  "Add a new file to binder as FILEID and visit it."
  (interactive "sNew file (extension optional): ")
  (unless (< 0 (string-width fileid))
    (user-error "No file name supplied"))
  (unless (or (directory-name-p fileid)
              (and (string-match "\\.[^.]+\\'" fileid)
	               (not (= 0 (match-beginning 0)))))
    (setq fileid
          (concat fileid "." (alist-get 'default-extension (binder-read)))))
  (unless (binder-get-item fileid)
    (binder-insert-item fileid (1+ (binder-sidebar-get-index))))
  (unwind-protect
      (let ((pop-up-windows binder-sidebar-pop-up-windows)
            (filepath (expand-file-name fileid (binder-root))))
        (if (directory-name-p filepath)
            (make-directory filepath)
        (find-file filepath)
        (write-file filepath)))
    (with-current-buffer (get-buffer-create binder-sidebar-buffer)
      (put 'binder--cache 'modification-time (current-time))
      (binder-sidebar-refresh)
      (binder-write-maybe))))

(defun binder-sidebar-remove (arg)
  "Remove binder item at point.
When ARG is non-nil, do not prompt for confirmation."
  (interactive "P")
  (let ((fileid (binder-sidebar-get-fileid))
        display)
    (setq display (or (binder-get-item-prop fileid 'display)
                      fileid))
    (if arg
        (binder-delete-item fileid)
      (when (y-or-n-p (format "Really remove item %S?" display))
        (binder-delete-item fileid)))
    (put 'binder--cache 'modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-write-maybe)))

(defun binder-sidebar-rename ()
  "Change display name of binder item at point."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid))
        name)
    (setq name
          (read-string "New name: "
                       (or (binder-get-item-prop fileid 'display) fileid)))
    (binder-set-item-prop fileid 'display name)
    (put 'binder--cache 'modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-write-maybe)))

(defun binder-sidebar-relocate (filepath)
  "Change file path of binder item at point to FILEPATH."
  (interactive "fNew file path: ")
  (setq filepath (binder-file-relative-to-root filepath))
  (setcar (binder-get-item (binder-sidebar-get-fileid)) filepath)
  (put 'binder--cache 'modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-set-status (status)
  "Set the status of binder item at point to STATUS."
  (interactive
   (list (completing-read-default
          "Status: " (binder-get-prop-list 'status)
          nil nil
          (binder-get-item-prop (binder-sidebar-get-fileid) 'status))))
  (binder-set-item-prop (binder-sidebar-get-fileid) 'status status)
  (put 'binder--cache 'modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-toggle-file-extensions ()
  "Toggle visibility of binder item file extensions."
  (interactive)
  (customize-set-variable 'binder-sidebar-hide-file-extensions
                          (not binder-sidebar-hide-file-extensions))
  (let ((current-fileid (binder-sidebar-get-fileid)))
    (binder-sidebar-refresh)
    (binder-sidebar-goto-item current-fileid))
  (message "%s file extensions"
           (capitalize
            (if binder-sidebar-hide-file-extensions
                "hiding" "showing"))))

(defun binder-sidebar-goto-item (fileid)
  "Move point to binder item with FILEID."
  (goto-char (point-min))
  (let (found)
    (while (and (< (point) (point-max))
                (not found))
      (if (string= (binder-sidebar-get-fileid) fileid)
          (setq found t)
        (forward-line 1)))))

(defun binder-sidebar-shift-down (&optional n)
  "Shift index position of binder item at point down in list."
  (interactive "p")
  (let ((p (if (<= n 0) -1 1))
        (fileid (binder-sidebar-get-fileid))
        item index)
    (setq item (binder-get-item fileid)
          index (binder-get-item-index fileid))
    (binder-delete-item fileid)
    (binder-insert-item item (+ index p))
    (put 'binder--cache 'modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-sidebar-goto-item fileid)))

(defun binder-sidebar-shift-up (&optional n)
  "Shift index position of binder item at point up in list."
  (interactive "p")
  (binder-sidebar-shift-down (- n)))

;;;###autoload
(define-derived-mode binder-sidebar-mode
  special-mode "Binder Sidebar"
  "Major mode for working with `binder' projects.")

(define-key binder-sidebar-mode-map (kbd "g") #'binder-sidebar-refresh)
(define-key binder-sidebar-mode-map (kbd "n") #'next-line)
(define-key binder-sidebar-mode-map (kbd "p") #'previous-line)
(define-key binder-sidebar-mode-map (kbd "RET") #'binder-sidebar-find-file)
(define-key binder-sidebar-mode-map (kbd "o") #'binder-sidebar-find-file-other-window)
(define-key binder-sidebar-mode-map (kbd "s") #'binder-sidebar-save)
(define-key binder-sidebar-mode-map (kbd "m") #'binder-sidebar-mark)
(define-key binder-sidebar-mode-map (kbd "u") #'binder-sidebar-unmark)
(define-key binder-sidebar-mode-map (kbd "#") #'binder-sidebar-set-status)
(define-key binder-sidebar-mode-map (kbd "U") #'binder-sidebar-unmark-all)
(define-key binder-sidebar-mode-map (kbd "v") #'binder-sidebar-join)
(define-key binder-sidebar-mode-map (kbd "i") #'binder-sidebar-toggle-notes)
(define-key binder-sidebar-mode-map (kbd "z") #'binder-sidebar-open-notes)
(define-key binder-sidebar-mode-map (kbd "M-n") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "M-p") #'binder-sidebar-shift-up)
(define-key binder-sidebar-mode-map (kbd "a") #'binder-sidebar-add-file)
(define-key binder-sidebar-mode-map (kbd "A") #'binder-sidebar-add-all-files)
(define-key binder-sidebar-mode-map (kbd "d") #'binder-sidebar-remove)
(define-key binder-sidebar-mode-map (kbd "r") #'binder-sidebar-rename)
(define-key binder-sidebar-mode-map (kbd "R") #'binder-sidebar-relocate)
(define-key binder-sidebar-mode-map (kbd "E") #'binder-sidebar-toggle-file-extensions)
(define-key binder-sidebar-mode-map (kbd "M-RET") #'binder-sidebar-new-file)


;;; Notes Major Mode

(defcustom binder-notes-display-alist
  '((side . left)
    (slot . 1))
  "Association list used to display binder notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :group 'binder-sidebar)

(defcustom binder-notes-buffer
  "*Binder Notes*"
  "Buffer name for Binder notes."
  :type 'string
  :safe 'stringp
  :group 'binder-sidebar)

(defvar-local binder-notes--fileid nil)
(defvar-local binder-notes--display nil)

(defun binder-sidebar-get-notes (dir fileid)
  (binder-notes-mode)
  (when dir (setq default-directory dir))
  (when fileid
    (unless (string= binder-notes--fileid fileid)
      (setq binder-notes--fileid fileid)
      (setq binder-notes--display (alist-get 'display (binder-get-item binder-notes--fileid)))
      (let ((notes (alist-get 'notes (binder-get-item binder-notes--fileid))))
        (with-silent-modifications
          (erase-buffer)
          (when notes (insert notes)))))))

(defun binder-sidebar-toggle-notes ()
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory)
        (fileid (binder-sidebar-get-fileid))
        (buffer (get-buffer-create binder-notes-buffer)))
    (with-current-buffer buffer
      (binder-sidebar-get-notes dir fileid))
    (if (get-buffer-window buffer (selected-frame))
        (delete-windows-on buffer (selected-frame))
      (display-buffer-in-side-window buffer binder-notes-display-alist))))

(defun binder-sidebar-open-notes ()
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory)
        (fileid (binder-sidebar-get-fileid))
        (buffer (get-buffer-create binder-notes-buffer)))
    (with-current-buffer buffer
      (binder-sidebar-get-notes dir fileid)
    (display-buffer-in-side-window buffer binder-notes-display-alist)
    (select-window (get-buffer-window buffer (selected-frame))))))

(defun binder-notes-commit ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (not (buffer-modified-p))
      (message "(No changes need to be added to binder)")
    (let ((prop-elt
           (assq 'notes (binder-get-item binder-notes--fileid)))
          (notes
           (string-trim (buffer-substring-no-properties
                         (point-min) (point-max)))))
      (if prop-elt
          (setcdr prop-elt notes)
        (push (cons 'notes notes) (cdr (binder-get-item binder-notes--fileid)))))
    (with-current-buffer binder-sidebar-buffer
      (binder-sidebar-refresh))
    (set-buffer-modified-p nil)
    (message "Added notes for %S to binder"
             (or binder-notes--display binder-notes--fileid))))

(defun binder-notes-expand-window ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (window-parameter (selected-window) 'window-side)
      (progn
        (quit-window)
        (pop-to-buffer (get-buffer-create binder-notes-buffer) t))
    (quit-window)
    (binder-sidebar-open-notes)))

(defcustom binder-notes-mode-hook
  '(visual-line-mode)
  "Hook run after entering Binder Notes Mode mode."
  :type 'hook
  :options '(visual-line-mode))

;;;###autoload
(define-derived-mode binder-notes-mode
  text-mode "Binder Notes Mode"
  "Major mode for editing `binder' notes.")

(define-key binder-notes-mode-map (kbd "C-c C-c") #'binder-notes-commit)
(define-key binder-notes-mode-map (kbd "C-c C-l") #'binder-notes-expand-window)
(define-key binder-notes-mode-map (kbd "C-c C-q") #'quit-window)


;;; Join Mode

(defgroup binder-join ()
  "Options for `binder-join-mode'.")

(defcustom binder-join-separator "\n\n"
  "String to insert between files when joining."
  :type 'string
  :group 'binder-join)

(defcustom binder-join-buffer
  "*Binder Join View*"
  "Buffer name for viewing joined files."
  :type 'string
  :safe 'stringp
  :group 'binder-join)

(defun binder-sidebar-join ()
  (interactive)
  (let ((buffer (get-buffer-create "*Binder Multiview*"))
        (mode (alist-get 'default-mode (binder-read)))
        file-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^>" nil t)
        (push (binder-sidebar-get-fileid) file-list)))
    (setq file-list (reverse file-list))
    (with-current-buffer buffer
      (with-silent-modifications
        (erase-buffer)
        (setq major-mode mode)
        (dolist (file file-list)
          (insert-file-contents file)
          (goto-char (point-max))
          (insert binder-file-separator))))
    (pop-to-buffer buffer)))

(define-minor-mode binder-join-mode
  "Minor mode for viewing Binder joined files."
  :init-value nil
  (if binder-join-mode
      (read-only-mode t)))



(provide 'binder)
;;; binder.el ends here

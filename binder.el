;;; binder.el --- Global minor mode to facilitate multi-file writing projects  -*- lexical-binding: t; -*-

;; Copyright (c) 2020  Paul William Rankin

;; Author: William Rankin <code@william.bydasein.com>
;; Keywords: files, outlines, wp, text
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.3"))
;; URL: https://github.com/rnkn/binder

;; This file is not part of GNU Emacs.

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

;; # Binder #

;; **Warning: this is alpha-level software. Although it does not touch files on
;; disk, you should consider all binder data (project structure, notes, statuses)
;; as susceptible to loss. Key bindings, variable and function names, and overall
;; program design are subject to change.**

;; Binder is global minor mode (and associated major modes) to facilitate working
;; on a writing project in multiple files. It is heavily inspired by the binder
;; feature in the [macOS writing app Scrivener][scriv].

;; The rationale behind working this way is to split a large writing project into
;; much smaller pieces.

;; [scriv]: https://www.literatureandlatte.com/scrivener/features?os=macOS

;; ## Features ##

;; Primarily, Binder provides a global minor mode binder-mode. This allows
;; working with files in the current binder data, which is saved in a .binder.el
;; file in the current directory. (You can change the name of this file is
;; binder-default-file option.)

;; At this top-level, the main interaction with your binder will be in navigating
;; back and forth between binder files:

;; - binder-next (bound to C-c ]) visits the next file in the binder, and
;; - binder-previous (bound to C-c [) visits the previous.

;; Calling these commands activates a transient map so that each command can be
;; repeated by repeating only the last key.

;; You probably want some idea of the structure of your binder project...

;; - binder-reveal-in-sidebar (bound to C-c ;) will find the current file in
;;   the binder sidebar (see below) or call binder-init-binder-file if there is
;;   none.
;; - binder-toggle-sidebar (bound to C-c ') toggles the visibility of the
;;   binder sidebar.

;; And when you're writing and want to quickly add something new, you can with...

;; - binder-add-file (bound to C-c :) prompts for a file-name and adds this
;;   possibly non-existent file to the binder after the current file's index. If no
;;   file-name extension is provided, use value of the binder's default-extension
;;   property (set with binder-default-file-extention option).

;; ### Binder Sidebar Mode ###

;; A major mode for displaying the binder sidebar. This is where your main
;; interaction with the binder happens.

;; Binder items are displayed in a linear ordered list. Calling
;; binder-sidebar-find-file (bound to RET) or
;; binder-sidebar-find-file-other-window (bound to o) will visit the
;; corresponding file.

;; Each item in the binder sidebar displays the following information:

;; 1. binder-sidebar-include-char (default x) denotes that this item has a
;;    non-nil value for its include property and therefore is included in
;;    binder-stable-mode (see below).
;; 2. binder-sidebar-notes-char (default *) denotes that this item has a string
;;    value for its notes property, which can be edited in binder-notes-mode
;;    (see below), or
;; 3. binder-sidebar-missing-char (defautl ?) denote that the item's
;;    corresponding file cannot be found, but can be relocated by calling
;;    binder-sidebar-relocate (bound to R).
;; 4. The item name, either the car of the item element or its display
;;    property, which can be set by calling binder-sidebar-rename (bound to r).
;; 5. The item status property value, prefixed with binder-sidebar-status-char
;;    (default #). The display of this value can be set with the
;;    binder-sidebar-status-column option.

;; Add an item with binder-sidebar-add-file (bound to a) or add all files in
;; directory with binder-sidebar-add-all-files (bound to A). Add a new file
;; with binder-sidebar-new-file (bound to M-RET). Remove items with
;; binder-sidebar-remove (bound to d) -- this *does not delete the files*, only
;; removes them from the binder.

;; Items can be reordered with binder-sidebar-shift-up (bound to M-p & M-up)
;; and binder-sidebar-shift-down (bound to M-n & M-down).

;; Each item's include state is toggled with binder-sidebar-toggle-include (bound
;; to x).

;; Each item can be given a status with binder-sidebar-set-status (bound to #).
;; Filter item by including or excluding a status with binder-sidebar-filter-in
;; (bound to /) and binder-sidebar-filter-out (bound to \). To clear a
;; status, just set an empty string. (n.b. Each command filter in/out only a single
;; status, therefore it won't make sense to use them in conjunction.)

;; The notes buffer (see below) can be accessed with either
;; binder-sidebar-open-notes (bound to z) or binder-sidebar-toggle-notes
;; (bound to i).

;; Hide item file extensions by setting the binder-sidebar-hide-file-extensions
;; option. This can be toggled with binder-sidebar-toggle-file-extensions (bound
;; to E).

;; You can customize how the sidebar window is displayed by setting
;; binder-sidebar-display-alist option.

;; (There is a "mark" functionality, but this is yet to be implemented beyond just
;; temporarily making items look marked.)

;; ### Binder Notes Mode ###

;; A major mode for editing binder notes.

;; Notes are only saved to the binder when calling binder-notes-commit (bound to
;; C-c C-c). Calling quit-window (bound to C-c C-q) or binder-toggle-sidebar
;; does not save notes.

;; By default, the notes window will update to the corresponding item notes
;; whenever the cursor moves in the binder sidebar. This may be disconcerting, so
;; you can change it by setting the binder-notes-keep-in-sync option.

;; If the notes side window feels too small, you can pop the buffer out to a
;; regular sized window with binder-notes-expand-window (bound to C-c C-l).

;; You can customize how the notes window is displayed by setting
;; binder-notes-display-alist option.

;; ### Binder Staple Mode ###

;; A minor mode for "stapling" binder files together.

;; When calling binder-sidebar-staple (bound to v), items marked as included in
;; the binder will be concatenated in a new buffer, separated by
;; binder-staple-separator string.

;; In this buffer, calling binder-staple-find-original-file (bound to M-RET)
;; will visit the original file corresponding to the text at point.

;; ## Why not just use Org Mode? ##

;; [Org Mode] is nice, but it's also a very *heavy* tool that almost insists that
;; everything be done within Org Mode. This isn't useful if you want to write in a
;; different format, e.g. [Markdown] or [Fountain].

;; Also, I prefer to keep my writing in a collection of separate text files. It
;; feels nicer to work on something small and self-contained than to organize a
;; large file with headings and use indirect buffers with narrowing.

;; [org mode]: https://orgmode.org
;; [markdown]: http://jblevins.org/projects/markdown-mode/
;; [fountain]: https://github.com/rnkn/fountain-mode

;; ## Requirements ##

;; - Emacs 25.3

;; ## Bugs and Feature Requests ##

;; Report bugs and feature requests at: <https://github.com/rnkn/binder/issues>

;; ## Start Here ##

;; This file is part of a sample binder project. Enabled binder-mode and type C-c ;
;; to reveal this file in the binder siderbar.


;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(require 'seq)

(defgroup binder ()
  "Work with a structured project of files."
  :group 'files)


;;; Core Variables

(defvar binder-file-header
  "\
;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.\n\n"
  "Header information for binder-file.")

(defvar binder--cache nil)
(defvar binder--modification-time nil)
(defvar binder--modification-count 0)
(defvar binder-status-filter-in nil)
(defvar binder-status-filter-out nil)


;;; Options

(defcustom binder-mode-lighter
  " B/"
  "Mode-line indicator for `binder-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'binder)

(defcustom binder-default-file
  ".binder.el"
  "Default file in which to store Binder project data."
  :type 'string
  :safe 'stringp
  :group 'binder)

(defcustom binder-project-directory
  nil
  "Directory containing current `binder-default-file' or nil.

The value set here will be used as default, but may be changed at
any time with `binder-change-directory'."
  :type '(choice (const nil) directory)
  :safe 'stringp
  :group 'binder)

(defcustom binder-default-file-extention
  "txt"
  "Default extension for new binder files."
  :type 'string
  :safe 'stringp
  :group 'binder)

(defcustom binder-save-threshold
  20
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
  '((t (:extend t :inherit (lazy-highlight))))
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


;;; Core Non-interactive Functions

(defun binder-root ()
  "Return the root directory with a binder file, or nil."
  (let ((directory
         (locate-dominating-file default-directory binder-default-file)))
    (when (and directory (file-directory-p directory))
      (expand-file-name directory))))

(defun binder-init-binder-file ()
  "Initialize an empty binder file."
  (let ((directory
         (or binder-project-directory default-directory)))
    (when (y-or-n-p (format "Initialize empty %s in %s? "
                            binder-default-file
                            (abbreviate-file-name directory)))
      (let ((binder-file
             (expand-file-name binder-default-file directory)))
      (with-temp-buffer
        (insert binder-file-header
                (pp-to-string
                 (list (cons 'structure nil)
                       (cons 'default-mode binder-default-stapled-mode)
                       (cons 'default-extension binder-default-file-extention))))
        (write-file binder-file))
      binder-file))))

(defun binder-find-binder-file ()
  "Find or initialize current binder file."
  (let ((binder-file
         (expand-file-name binder-default-file (binder-root))))
    (if (file-exists-p binder-file) binder-file
      (binder-init-binder-file))))

(defun binder-read ()
  "Read current binder data.
Reads from `binder--cache' if valid, or from binder file if not."
  (let ((binder-file (binder-find-binder-file)))
    (unless binder-file
      (user-error "No binder file found"))
    ;; The cache is only valid if binder--cache is non-nil,
    ;; binder-project-directory is the current binder-root, and either we
    ;; haven't modified binder data or the binder file is older than
    ;; binder--modification-time.
    (unless (and binder--cache
                 (string= (expand-file-name binder-project-directory) (binder-root))
                 (or (= binder--modification-count 0)
                     (time-less-p (nth 5 (file-attributes binder-file))
                                  binder--modification-time)))
      (with-temp-buffer
        (insert-file-contents binder-file)
        (goto-char (point-min))
        (setq binder--cache (read (current-buffer))))))
  binder--cache)

(defun binder-write ()
  "Write binder data to file."
  (mapc (lambda (item)
          (setf item (rassq-delete-all "" item)))
        (binder-get-structure))
  (let ((binder-file (binder-find-binder-file)))
    (with-temp-buffer
      (insert binder-file-header (pp-to-string binder--cache))
      (write-file binder-file)))
  (setq binder--modification-count 0))

(defun binder-write-maybe ()
  "Write binder data if passed modified threshold."
  (if (< binder--modification-count binder-save-threshold)
      (cl-incf binder--modification-count)
    (binder-write)))

(defun binder-cd (directory)
  "Set `binder-project-directory' to DIRECTORY and erase cache."
  (setq binder-project-directory (expand-file-name directory)
        binder--notes-fileid nil
        binder--cache nil))

(defun binder-ensure-in-project ()
  "Ensure the current file or directory is in the current project."
  (let ((root (binder-root)))
    (cond
     ;; The binder-project-directory matches root, we're all good.
     ((and (stringp binder-project-directory)
           (stringp root)
           (string= (directory-file-name
                     (expand-file-name binder-project-directory))
                    (directory-file-name root)))
      t)
     ;; The binder-project-directory does not match project root; offer to
     ;; change it to current project root.
     ((and (stringp binder-project-directory)
           (stringp root))
      (when (y-or-n-p (format "Outside of current binder project %s
Change binder directory to %s?"
                              (abbreviate-file-name binder-project-directory)
                              (abbreviate-file-name root)))
        (binder-cd root)))
     ;; The binder-project-directory is set but we're not in a project; this is
     ;; fine.
     ((stringp binder-project-directory)
      t)
     ;; The binder-project-directory is not set, but we're in a project; offer
     ;; to set it to current project root.
     ((stringp root)
      (binder-cd root)
      (message "Set binder directory to %s" (abbreviate-file-name root)))
     ;; A fresh project; offer to set project directory to default-directory.
     (t
      (when (y-or-n-p (format "Set binder directory to %s?"
                              (abbreviate-file-name default-directory)))
        (binder-cd default-directory)
        (binder-init-binder-file))))))

(defun binder-file-relative-to-root (filepath)
  "Return FILEPATH relative to binder root directory."
  (string-trim (expand-file-name filepath)
               (expand-file-name binder-project-directory)))

(defun binder-get-structure ()
  "Return binder data structure component."
  (alist-get 'structure (binder-read)))

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
        (setf item (assq-delete-all prop item))
      (let ((prop-elt (assq prop item)))
        (if prop-elt
            (setcdr prop-elt value)
          (push (cons prop value) (cdr item))))))
  (setq binder--modification-time (current-time)))

(defun binder-get-item-index (fileid)
  "Return index position for binder item for FILEID."
  (seq-position (binder-filter-structure) (binder-get-item fileid)))

(defun binder-insert-item (item index)
  "Insert binder ITEM at position INDEX."
  (unless (listp item) (setq item (list item)))
  (setcdr (assq 'structure (binder-read))
          (let ((structure (binder-get-structure)))
            (append (seq-take structure index)
                    (cons item (seq-drop structure index)))))
  (setq binder--modification-time (current-time))
  (binder-write-maybe))

(defun binder-delete-item (fileid)
  "Delete binder item for FILEID."
  (setcdr (assq 'structure (binder-read))
          (remove (binder-get-item fileid) (binder-get-structure)))
  (setq binder--modification-time (current-time))
  (binder-write-maybe))

(defun binder-get-prop-list (prop)
  "Return list of values for PROP."
  (delq nil
        (mapcar
         (lambda (item)
           (let ((value (alist-get prop item)))
             (when (and (stringp value) (< 0 (string-width value)))
               value)))
         (binder-get-structure))))

(defun binder-get-buffer-fileid ()
  "Return buffer binder fileid."
  (binder-file-relative-to-root
   (or (buffer-file-name) (expand-file-name default-directory))))

(defun binder-filter-structure ()
  "Return binder structure filtered by status.

Filters in `binder-status-filter-in' or filters out
`binder-status-filter-out'."
  (let ((structure (binder-get-structure)))
    (if binder-status-filter-in
        (setq structure
              (seq-filter
               (lambda (item)
                 (string= (alist-get 'status item) binder-status-filter-in))
               structure))
      (when binder-status-filter-out
        (setq structure
              (seq-remove
               (lambda (item)
                 (string= (alist-get 'status item) binder-status-filter-out))
               structure))))
    structure))

(defun binder-exit-hook ()
  "Ensure project data is saved on exit."
  (when binder-mode (binder-save 'prompt)))


;;; Global Minor Mode

(defvar binder-mode-map (make-sparse-keymap))

(defun binder-save (&optional prompt)
  "Save project data (with prompt when PROMPT is non-nil)."
  (interactive)
  (cond ((= 0 binder--modification-count)
         (message "(No changes need to be saved)"))
        ((and prompt
              (y-or-n-p (format "Save binder project %s?"
                                (abbreviate-file-name binder-project-directory))))
         (binder-write))
        (t
         (binder-write))))

(defun binder-change-directory (directory)
  "Change to binder project directory DIRECTORY."
  (interactive "DDirectory: ")
  (binder-save 'prompt)
  (binder-cd directory)
  (binder-sidebar-refresh-window)
  (binder-notes-refresh-window))

(defun binder-next (&optional n)
  "Visit Nth next file in binder.
Or visit Nth previous file if N is negative."
  (interactive "p")
  (binder-ensure-in-project)
  ;; Find the current file/directory fileid, if one.
  (let ((this-fileid (binder-get-buffer-fileid))
        (structure (binder-filter-structure))
        index next-index)
    ;; If current file has an INDEX, get the NEXT-INDEX.
    (setq index (or (binder-get-item-index this-fileid) 0)
          next-index (+ index n))
    ;; If NEXT-INDEX is within the filtered structure length, find the
    ;; Nth next/previous file.
    (if (not (<= 0 next-index (1- (length structure))))
        (message "End of binder")
      (find-file-existing
       (expand-file-name (car (nth next-index structure))
                         binder-project-directory))
      (binder-sidebar-refresh-window))
    ;; Setup the overriding keymap.
    (unless overriding-terminal-local-map
      (let ((keys (substring (this-single-command-keys) 0 -1))
            (map (cdr binder-mode-map)))
        (mapc (lambda (k) (setq map (assq k map))) keys)
        (when (consp map) (set-transient-map (cdr map) t))))))

(defun binder-previous (&optional n)
  "Visit Nth previous file in binder.
Or visit Nth next file if N is negative."
  (interactive "p")
  (binder-next (- n)))

(defun binder-add-file (fileid &optional index)
  "Add a (possibly non-existent) FILEID to the binder at INDEX.
If the current file is in the binder, add at INDEX after that one."
  (interactive "FAdd file (extension optional): ")
  (binder-ensure-in-project)
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (< 0 (string-width fileid))
    (user-error "No file name supplied"))
  ;; If FILEID is not a directory and no file extension was provided,
  ;; add the binder's default file extensions.
  (unless (or (directory-name-p fileid)
              (and (string-match "\\.[^.]+\\'" fileid)
                   (not (= 0 (match-beginning 0)))))
    (setq fileid
          (concat fileid "." (alist-get 'default-extension (binder-read)))))
  ;; If the file/directory does not exist, create it.
  (let ((filepath (expand-file-name fileid binder-project-directory)))
    (unless (file-exists-p filepath)
      (if (directory-name-p filepath)
          (make-directory filepath t)
        (with-current-buffer (find-file-noselect filepath)
          (write-file filepath))))
    ;; Insert FILEID into binder at INDEX, or after current file.
    (unless (binder-get-item fileid)
      (unless index
        (let ((this-file-index
               (binder-get-item-index (binder-get-buffer-fileid))))
          (setq index (if this-file-index
                          (1+ this-file-index)
                        (length (binder-get-structure))))))
      (binder-insert-item fileid index))
    (binder-write-maybe)
    ;; When binder sidebar is active, refresh it.
    (binder-sidebar-refresh-window)
    ;; Finally, visit the file FILEPATH.
    (let ((pop-up-windows binder-sidebar-pop-up-windows))
      (find-file filepath))))

(define-key binder-mode-map (kbd "C-c ]") #'binder-next)
(define-key binder-mode-map (kbd "C-c [") #'binder-previous)
(define-key binder-mode-map (kbd "C-c ;") #'binder-reveal-in-sidebar)
(define-key binder-mode-map (kbd "C-c '") #'binder-toggle-sidebar)
(define-key binder-mode-map (kbd "C-c \"") #'binder-toggle-notes)
(define-key binder-mode-map (kbd "C-c :") #'binder-add-file)

;;;###autoload
(define-minor-mode binder-mode
  "Globally interact with `binder'."
  :init-value nil
  :lighter binder-mode-lighter
  :global t
  (if binder-mode
      (unless noninteractive
        (add-hook 'kill-emacs-hook 'binder-exit-hook)
        (add-hook 'window-configuration-change-hook 'binder-highlight-in-sidebar))
    (remove-hook 'window-configuration-change-hook 'binder-highlight-in-sidebar)))


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
  nil
  "If non-nil, switch to binder sidebar upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-persistent-window
  t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `binder-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defvar binder--current-fileid nil)
(defvar binder--sidebar-marked nil)

(defun binder-sidebar-refresh ()
  "Redraw binder sidebar, reading from cache."
  (interactive)
  (with-silent-modifications
    (setq default-directory binder-project-directory)
    (let ((x (point)))
      (erase-buffer)
      (dolist (item (binder-filter-structure))
        (let ((fileid (car item))
              (include (alist-get 'include item))
              (display (alist-get 'display item))
              (notes (alist-get 'notes item))
              (status (alist-get 'status item))
              marked missing status-overwrite)
          ;; Set whether FILEID is MARKED and MISSING.
          (when (member fileid binder--sidebar-marked)
            (setq marked t))
          (when (not (file-exists-p fileid))
            (setq missing t))
          ;; Insert the item line elements.
          (insert (cond (marked ">")
                        (include binder-sidebar-include-char)
                        (t " "))
                  " "
                  (cond (missing binder-sidebar-missing-char)
                        ((and notes (not (string-empty-p notes)))
                         binder-sidebar-notes-char)
                        (t " "))
                  " "
                  ;; Use either DISPLAY, or if directory ensure a
                  ;; trailing slash, and finally if we're hiding file
                  ;; extensions, do that, otherwise just the FILEID is
                  ;; fine.
                  (or display
                      (if (file-directory-p fileid)
                          (replace-regexp-in-string "/*$" "/" fileid)
                        (if binder-sidebar-hide-file-extensions
                            (replace-regexp-in-string ".+\\(\\..+\\)" ""
                                                      fileid nil nil 1)
                          fileid))))
          ;; Add the face properties. Make them front-sticky since we
          ;; were previously editing the buffer text (but not anymore).
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
          ;; Add the item STATUS with a hashtag, because hashtags are
          ;; cool, right?
          (when (and status (< 0 (string-width status)))
            (move-to-column (1- binder-sidebar-status-column))
            (unless (eolp) (setq status-overwrite t))
            (move-to-column binder-sidebar-status-column)
            (indent-to-column binder-sidebar-status-column)
            (let ((x (point)))
              (delete-region (1- x) (line-end-position))
              (insert (if status-overwrite "~" " ")
                      binder-sidebar-status-char status)
              (put-text-property x (line-end-position)
                                 'face 'binder-sidebar-status)))
          (insert "\n")
          (when (string= fileid binder--current-fileid)
            (put-text-property (line-beginning-position 0) (line-beginning-position)
                               'face 'binder-sidebar-highlight))))
      (goto-char x))))

(defun binder-sidebar-refresh-window ()
  "Call `binder-sidebar-refresh' if sidebar window is live."
  (when (window-live-p (get-buffer-window binder-sidebar-buffer))
    (with-current-buffer binder-sidebar-buffer
      (binder-sidebar-refresh))))

(defun binder-sidebar-force-refresh ()
  "Clear the cache, reread project data from disk and redraw sidebar."
  (interactive)
  (setq binder--cache nil)
  (message "Project cache cleared")
  (binder-sidebar-refresh-window))

(defalias 'binder-sidebar-change-directory 'binder-change-directory)

(defun binder-sidebar-create-buffer ()
  "Return binder sidebar buffer for DIRECTORY."
  (binder-ensure-in-project)
  (with-current-buffer (get-buffer-create binder-sidebar-buffer)
    (binder-sidebar-mode)
    (binder-sidebar-refresh)
    (current-buffer)))

(defun binder-sidebar-create-window ()
  "Return binder sidebar window for DIRECTORY.
Defaults to current directory."
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window
     (binder-sidebar-create-buffer)
     (append binder-sidebar-display-alist
             (when binder-sidebar-persistent-window
               (list '(window-parameters (no-delete-other-windows . t))))))))

(defun binder-sidebar-get-fileid ()
  "Return fileid for item at point."
  (save-excursion
    (if (eobp) (forward-line -1) (beginning-of-line))
    (get-text-property (point) 'binder-fileid)))

(defun binder-sidebar-goto-item (fileid)
  "Move point to binder item with FILEID."
  (goto-char (point-min))
  ;; It would be nice to use find-next-text-property but that isn't
  ;; available until Emacs 27.
  (let (found)
    (while (and (< (point) (point-max))
                (not found))
      (if (string= (binder-sidebar-get-fileid) fileid)
          (setq found t)
        (forward-line 1)))))

(defun binder-sidebar-find-file (arg)
  "Visit binder item at point.
When ARG is non-nil, visit in new window."
  (interactive "P")
  (let ((pop-up-windows (or arg binder-sidebar-pop-up-windows))
        (fileid (binder-sidebar-get-fileid))
        filepath)
    (setq filepath (expand-file-name fileid))
    (when (file-exists-p filepath)
      (setq binder--current-fileid fileid)
      (binder-sidebar-refresh)
      (find-file filepath))))

(defun binder-sidebar-find-file-other-window ()
  "Visit binder item in other window."
  (interactive)
  (binder-sidebar-find-file t))

(defalias 'binder-sidebar-save 'binder-save)

(defun binder-sidebar-get-index ()
  "Return binder index position at point."
  (if (eobp) (1- (length (binder-get-structure)))
    (binder-get-item-index (binder-sidebar-get-fileid))))

(defun binder-sidebar-mark ()
  "Mark the binder item at point."
  (interactive)
  (cl-pushnew (binder-sidebar-get-fileid) binder--sidebar-marked)
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark ()
  "Unmark the binder item at point."
  (interactive)
  (setq binder--sidebar-marked
        (remove (binder-sidebar-get-fileid) binder--sidebar-marked))
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark-all ()
  "Unmark all binder items."
  (interactive)
  (setq binder--sidebar-marked nil)
  (binder-sidebar-refresh))

(defun binder-sidebar-add-file (fileid)
  "Add (possibly non-existent) file to binder as FILEID."
  (interactive "FAdd file: ")
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (binder-get-item fileid)
    (binder-insert-item fileid (1+ (binder-sidebar-get-index))))
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-sidebar-goto-item fileid)
  (binder-write-maybe))

(defun binder-sidebar-add-all-files ()
  "Add all files in current directory to binder."
  (interactive)
  (when (y-or-n-p (format "Add all files in %s"
                          (abbreviate-file-name default-directory)))
    (dolist (file (directory-files default-directory nil "^[^.]"))
      (binder-sidebar-add-file file))))

(defun binder-sidebar-new-file (fileid)
  "Add a new file to binder as FILEID and visit it."
  (interactive "FAdd file (extension optional): ")
  (unless (eq major-mode 'binder-sidebar-mode)
    (user-error "Not in %S" 'binder-sidebar-mode))
  (binder-add-file fileid (1+ (binder-sidebar-get-index)))
  (binder-sidebar-goto-item fileid))

(defun binder-sidebar-remove (arg)
  "Remove marked items or item at point.
When ARG is non-nil, do not prompt for confirmation."
  (interactive "P")
  (let ((fileid-list (or binder--sidebar-marked
                      (list (binder-sidebar-get-fileid)))))
    (when (or arg (y-or-n-p (format "Really remove %s?"
                                    (string-join fileid-list ", "))))
      (mapc #'binder-delete-item fileid-list)
      (setq binder--modification-time (current-time))
      (setq binder--sidebar-marked nil)))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-rename ()
  "Change display name of binder item at point."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid))
        name)
    (setq name
          (read-string "New name: "
                       (or (binder-get-item-prop fileid 'display) fileid)))
    (binder-set-item-prop fileid 'display name)
    (setq binder--modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-write-maybe)))

(defun binder-sidebar-relocate (filepath)
  "Change file path of binder item at point to FILEPATH."
  (interactive "fNew file path: ")
  (setq filepath (binder-file-relative-to-root filepath))
  (setcar (binder-get-item (binder-sidebar-get-fileid)) filepath)
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-toggle-include ()
  "Toggle whether marked items or item at point is included in `binder-sidebar-staple'."
  (interactive)
  (dolist (fileid (or binder--sidebar-marked
                      (list (binder-sidebar-get-fileid))))
    (binder-set-item-prop fileid 'include
                          (not (binder-get-item-prop fileid 'include))))
  (setq binder--sidebar-marked nil)
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-set-status (status)
  "Set the status of marked items or item at point to STATUS."
  (interactive
   (list (completing-read
          "Status: " (binder-get-prop-list 'status)
          nil nil (binder-get-item-prop (or (car binder--sidebar-marked)
                                            (binder-sidebar-get-fileid))
                                        'status))))
  (dolist (fileid (or binder--sidebar-marked
                      (list (binder-sidebar-get-fileid))))
    (binder-set-item-prop fileid 'status status))
  (setq binder--sidebar-marked nil)
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-toggle-file-extensions ()
  "Toggle visibility of binder item file extensions."
  (interactive)
  (customize-set-variable 'binder-sidebar-hide-file-extensions
                          (not binder-sidebar-hide-file-extensions))
  (let ((fileid (binder-sidebar-get-fileid)))
    (binder-sidebar-refresh)
    (binder-sidebar-goto-item fileid))
  (message "%s file extensions"
           (capitalize
            (if binder-sidebar-hide-file-extensions
                "hiding" "showing"))))

(defun binder-sidebar-shift-down (&optional n)
  "Shift index position of item at point down N places in list."
  (interactive "p")
  (let ((p (if (<= n 0) -1 1))
        (fileid (binder-sidebar-get-fileid))
        item index)
    (setq item (binder-get-item fileid)
          index (binder-get-item-index fileid))
    (when (<= 0 (+ index p) (1- (length (binder-get-structure))))
      (binder-delete-item fileid)
      (binder-insert-item item (+ index p))
      (setq binder--modification-time (current-time))
      (binder-sidebar-refresh)
      (binder-sidebar-goto-item fileid))))

(defun binder-sidebar-shift-up (&optional n)
  "Shift index position of item at point up N places in list."
  (interactive "p")
  (binder-sidebar-shift-down (- n)))

(defun binder-sidebar-filter-in (status)
  "Filter items to only include items with STATUS."
  (interactive
   (list (completing-read
          "Filter in status: " (binder-get-prop-list 'status))))
  (setq binder-status-filter-in
        (if (string-empty-p status) nil status))
  (binder-sidebar-refresh))

(defun binder-sidebar-filter-out (status)
  "Filter items to exclude items with STATUS."
  (interactive
   (list (completing-read
          "Filter out status: " (binder-get-prop-list 'status))))
  (setq binder-status-filter-out
        (if (string-empty-p status) nil status))
  (binder-sidebar-refresh))

(defun binder-highlight-in-sidebar ()
  "Highlight the current file in sidebar.

Added to `window-configuration-change-hook'."
  (unless (or (minibuffer-window-active-p (selected-window))
              (eq major-mode 'binder-sidebar-mode))
    (when (window-live-p (get-buffer-window binder-sidebar-buffer))
      (setq binder--current-fileid (binder-get-buffer-fileid))
      (binder-sidebar-refresh-window))))

;;;###autoload
(defun binder-reveal-in-sidebar ()
  "Reveal current file in binder sidebar.

Unconditionally activates `binder-mode'."
  (interactive)
  (binder-mode)
  (let ((filepath (or (buffer-file-name)
                      (expand-file-name default-directory)))
        (root (binder-root)))
    (select-window (binder-sidebar-create-window))
    (if (string= filepath root)
        (binder-sidebar-refresh)
      (let ((fileid (binder-file-relative-to-root filepath)))
        (setq binder--current-fileid fileid)
        (unless (binder-get-item fileid)
          (when (y-or-n-p (format "Add %s to binder? " fileid))
            (binder-sidebar-add-file fileid)))
        (binder-sidebar-refresh)
        (binder-sidebar-goto-item fileid)))))

;;;###autoload
(defun binder-toggle-sidebar ()
  "Toggle visibility of project sidebar window.

Unconditionally activates `binder-mode'."
  (interactive)
  (binder-mode)
  (if (window-live-p (get-buffer-window binder-sidebar-buffer))
      (delete-window (get-buffer-window binder-sidebar-buffer))
    (binder-sidebar-create-window)
    (when binder-sidebar-select-window
      (select-window (get-buffer-window binder-sidebar-buffer)))))

;;;###autoload
(define-derived-mode binder-sidebar-mode
  special-mode "Binder Sidebar"
  "Major mode for working with `binder' projects."
  (add-hook 'post-command-hook 'binder-sidebar-sync-notes t t))

(define-key binder-sidebar-mode-map (kbd "g") #'binder-sidebar-refresh)
(define-key binder-sidebar-mode-map (kbd "G") #'binder-sidebar-force-refresh)
(define-key binder-sidebar-mode-map (kbd "C") #'binder-sidebar-change-directory)
(define-key binder-sidebar-mode-map (kbd "n") #'next-line)
(define-key binder-sidebar-mode-map (kbd "p") #'previous-line)
(define-key binder-sidebar-mode-map (kbd "RET") #'binder-sidebar-find-file)
(define-key binder-sidebar-mode-map (kbd "o") #'binder-sidebar-find-file-other-window)
(define-key binder-sidebar-mode-map (kbd "s") #'binder-sidebar-save)
(define-key binder-sidebar-mode-map [remap save-buffer] #'binder-sidebar-save)
(define-key binder-sidebar-mode-map (kbd "m") #'binder-sidebar-mark)
(define-key binder-sidebar-mode-map (kbd "u") #'binder-sidebar-unmark)
(define-key binder-sidebar-mode-map (kbd "#") #'binder-sidebar-set-status)
(define-key binder-sidebar-mode-map (kbd "U") #'binder-sidebar-unmark-all)
(define-key binder-sidebar-mode-map (kbd "v") #'binder-sidebar-staple)
(define-key binder-sidebar-mode-map (kbd "i") #'binder-sidebar-toggle-notes)
(define-key binder-sidebar-mode-map (kbd "z") #'binder-sidebar-open-notes)
(define-key binder-sidebar-mode-map (kbd "M-n") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "<M-down>") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "M-p") #'binder-sidebar-shift-up)
(define-key binder-sidebar-mode-map (kbd "<M-up>") #'binder-sidebar-shift-up)
(define-key binder-sidebar-mode-map (kbd "a") #'binder-sidebar-add-file)
(define-key binder-sidebar-mode-map (kbd "A") #'binder-sidebar-add-all-files)
(define-key binder-sidebar-mode-map (kbd "d") #'binder-sidebar-remove)
(define-key binder-sidebar-mode-map (kbd "r") #'binder-sidebar-rename)
(define-key binder-sidebar-mode-map (kbd "R") #'binder-sidebar-relocate)
(define-key binder-sidebar-mode-map (kbd "E") #'binder-sidebar-toggle-file-extensions)
(define-key binder-sidebar-mode-map (kbd "x") #'binder-sidebar-toggle-include)
(define-key binder-sidebar-mode-map (kbd "/") #'binder-sidebar-filter-in)
(define-key binder-sidebar-mode-map (kbd "\\") #'binder-sidebar-filter-out)
(define-key binder-sidebar-mode-map (kbd "M-RET") #'binder-sidebar-new-file)


;;; Notes Major Mode

(defcustom binder-notes-buffer
  "*Binder Notes*"
  "Buffer name for Binder notes."
  :type 'string
  :safe 'stringp
  :group 'binder-sidebar)

(defcustom binder-notes-display-alist
  '((side . left)
    (window-width . 50)
    (slot . 1))
  "Association list used to display binder notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :group 'binder-sidebar)

(defcustom binder-notes-persistent-window
  t
  "When non-nil, notes will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `binder-toggle-notes' or `quit-window' to close notes."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-notes-keep-in-sync
  nil
  "If non-nil, moving point in binder sidebar updates notes.

Enabling this can be disconcerting, because notes are not
automatically saved."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-notes-mode-hook
  '(visual-line-mode)
  "Hook run after entering Binder Notes Mode."
  :type 'hook
  :options '(visual-line-mode))

(defvar binder--notes-fileid nil)
(defvar binder--notes-display nil)

(defun binder-notes-refresh ()
  "Redraw the notes buffer."
  (setq default-directory binder-project-directory)
  (with-silent-modifications
    (erase-buffer)
    (insert (or (binder-get-item-prop binder--notes-fileid 'notes)
                ""))
    (setq binder--notes-display
          (binder-get-item-prop binder--notes-fileid 'display))
    (setq header-line-format
          (if binder--notes-fileid
              (list (list :propertize (or binder--notes-display binder--notes-fileid)
                          'face 'bold)
                    "  C-c C-c to commit; C-c C-q to quit")
            "Nothing selected; C-c C-q to quit"))))

(defun binder-notes-refresh-window ()
  "Call `binder-notes-refresh' if notes window is live."
  (when (window-live-p (get-buffer-window binder-notes-buffer))
    (with-current-buffer binder-notes-buffer
      (binder-notes-refresh))))

(defun binder-notes-create-buffer ()
  "Create the notes buffer."
  (binder-ensure-in-project)
  (with-current-buffer (get-buffer-create binder-notes-buffer)
    (binder-notes-mode)
    (binder-notes-refresh)
    (current-buffer)))

(defun binder-notes-create-window ()
  "Create the notes window."
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window
     (binder-notes-create-buffer)
     (append binder-notes-display-alist
             (when binder-sidebar-persistent-window
               (list '(window-parameters (no-delete-other-windows . t))))))))

(defun binder-show-notes (&optional select)
  "Show the notes for the appropriate project item.
If argument SELECT is non-nil, select the notes window."
  (binder-ensure-in-project)
  (if (eq major-mode 'binder-sidebar-mode)
      (setq binder--notes-fileid (binder-sidebar-get-fileid))
    (let ((filepath (or (buffer-file-name)
                        (expand-file-name default-directory)))
          (root (binder-root)))
      (unless (string= filepath root)
        (setq binder--notes-fileid (binder-file-relative-to-root filepath)))))
  (if (window-live-p (get-buffer-window binder-notes-buffer))
      (binder-notes-refresh-window)
    (binder-notes-create-window))
  (when select (select-window (get-buffer-window binder-notes-buffer))))

(defun binder-sidebar-open-notes ()
  "Open notes for item at point and select the notes window."
  (interactive)
  (binder-show-notes t))

(defun binder-toggle-notes ()
  "Toggle visibility of binder notes window."
  (interactive)
  (if (window-live-p (get-buffer-window binder-notes-buffer))
      (delete-window (get-buffer-window binder-notes-buffer))
    (binder-show-notes)))

(defalias 'binder-sidebar-toggle-notes 'binder-toggle-notes)

(defun binder-notes-save ()
  "Save notes buffer content to project.

This command writes project data to disk."
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (not (buffer-modified-p))
      (message "(No changes need to be added to binder)")
    (binder-set-item-prop binder--notes-fileid 'notes
                          (string-trim (buffer-substring-no-properties
                                        (point-min) (point-max))))
    (set-buffer-modified-p nil)
    (binder-write)
    (binder-sidebar-refresh-window)
    (message "Saved notes for %s to binder"
             (or binder--notes-display binder--notes-fileid))))

(defun binder-notes-save-and-quit-window ()
  "Call `binder-notes-save' and quit the notes window."
  (interactive)
  (binder-notes-save)
  (quit-window))

(defun binder-notes-expand-window ()
  "Toggle the notes window from a side window to full window."
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (window-parameter (selected-window) 'window-side)
      (progn
        (quit-window)
        (pop-to-buffer (get-buffer-create binder-notes-buffer)))
    (quit-window)
    (binder-sidebar-open-notes)))

(defun binder-sidebar-sync-notes ()
  "Set the current notes to sidebar item at point."
  (while-no-input
    (redisplay)
    (when binder-notes-keep-in-sync
      (setq binder--notes-fileid (binder-sidebar-get-fileid))
      (binder-notes-refresh-window))))

;;;###autoload
(define-derived-mode binder-notes-mode
  text-mode "Binder Notes Mode"
  "Major mode for editing `binder' notes."
  (binder-notes-refresh))

(define-key binder-notes-mode-map (kbd "C-c C-c") #'binder-notes-save-and-quit-window)
(define-key binder-notes-mode-map [remap save-buffer] #'binder-notes-save)
(define-key binder-notes-mode-map (kbd "C-c C-l") #'binder-notes-expand-window)
(define-key binder-notes-mode-map (kbd "C-c C-q") #'quit-window)
(define-key binder-notes-mode-map (kbd "C-c C-k") #'quit-window)


;;; Staple Mode

(defgroup binder-staple ()
  "Options for `binder-staple-mode'.
This is for \"stapling\" together multiple binder files."
  :group 'binder)

(defcustom binder-default-stapled-mode
  'text-mode
  "Default major mode when stapling together files."
  :type 'function
  :safe 'functionp
  :group 'binder-staple)

(defcustom binder-staple-separator "\n"
  "String to insert between files when stapling together."
  :type 'string
  :group 'binder-staple)

(defcustom binder-staple-buffer
  "*Binder Staple View*"
  "Buffer name for viewing stapled files."
  :type 'string
  :safe 'stringp
  :group 'binder-staple)

(defun binder-staple ()
  "Concatenate all project files marked as included.
Creates `binder-staple-buffer' with each file is separated by
`binder-staple-separator'. Sets destination buffer major mode to
\"default-mode\" project property.

See `binder-sidebar-toggle-include'."
  (interactive)
  (binder-ensure-in-project)
  (let ((item-list
         (seq-filter
          (lambda (item) (alist-get 'include item))
          (binder-filter-structure))))
    (with-current-buffer (get-buffer-create binder-staple-buffer)
      (with-silent-modifications
        (erase-buffer)
        (dolist (item item-list)
          (let ((x (point)))
            (insert-file-contents (expand-file-name (car item) binder-project-directory))
            (goto-char (point-max))
            (insert binder-staple-separator)
            (put-text-property x (point) 'binder-original-file
                               (expand-file-name (car item) binder-project-directory)))))
      (funcall (alist-get 'default-mode (binder-read)))
      (binder-staple-mode t))
    (if (eq major-mode 'binder-sidebar-mode)
        (let ((pop-up-windows binder-sidebar-pop-up-windows))
          (pop-to-buffer binder-staple-buffer))
      (pop-to-buffer binder-staple-buffer))))

(defalias 'binder-sidebar-staple 'binder-staple)

(defun binder-staple-find-original-file ()
  "Find the file containing content at point."
  (interactive)
  (unless binder-staple-mode
    (user-error "Not in %S" 'binder-staple-mode))
  (let ((original-file (or (get-text-property (point) 'binder-original-file)
                           (get-text-property (1- (point)) 'binder-original-file))))
        ;; (position
        ;;  (cond ((bobp) 1)
        ;;        ((eobp)
        ;;         (previous-single-property-change
        ;;          (point) 'binder-original-file))
        ;;        ((string= (get-text-property (point) 'binder-original-file)
        ;;                  (get-text-property (1- (point)) 'binder-original-file))
        ;;         (or (previous-single-property-change
        ;;              (point) 'binder-original-file)
        ;;             1))
        ;;        (t (point))))
    ;; (setq position (1+ (- (point) position)))
    (find-file-existing original-file)))

(defcustom binder-staple-mode-hook
  '(view-mode)
  "Hook run after entering Binder Staple Mode."
  :type 'hook
  :group 'binder-staple)

(defvar binder-staple-mode-map (make-sparse-keymap))

(define-key binder-staple-mode-map (kbd "M-RET") #'binder-staple-find-original-file)

(define-minor-mode binder-staple-mode
  "Minor mode for viewing files \"stapled\" together."
  :init-value nil)



(provide 'binder)
;;; binder.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:

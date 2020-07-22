;;; binder.el --- Global minor mode to facilitate multi-file writing projects  -*- lexical-binding: t; -*-

;; Copyright (c) 2020  Paul William Rankin

;; Author: William Rankin <william@bydasein.com>
;; Keywords: files, outlines, wp, text
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.4") (seq "2.20"))
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

;; Binder is global minor mode (and associated major modes) to facilitate
;; working on a writing project in multiple files. It is heavily inspired
;; by the binder feature in the [macOS writing app Scrivener][scriv].

;; The rationale behind working this way is to split a large writing
;; project into much smaller pieces.

;; [scriv]: https://www.literatureandlatte.com/scrivener/

;; ## Features ##

;; Primarily, Binder provides a global minor mode binder-mode. This
;; allows working with files in the current binder-project-directory.
;; Data concerning these files is saved in a .binder.el file in the
;; project directory. (You can change the name of this file with the
;; binder-default-file option.)

;; ## Navigation ##

;; At the most basic level, you can navigate back and forth through the
;; files in a project:

;; - binder-next (C-c ]) visits the next file in the binder, and
;; - binder-previous (C-c [) visits the previous.

;; Calling these commands activates a transient map so that each command
;; can be repeated by repeating only the last key.

;; ## Sidebar ##

;; You'll mostly interact with the project structure via the sidebar.

;; - binder-toggle-sidebar (C-c ') toggles the visibility of the binder
;;   sidebar.
;; - binder-reveal-in-sidebar (C-c ;) finds the current file in the
;;   sidebar.

;; Each project item is a file, referenced relative to the project
;; directory. Project items are displayed in a linear ordered list. Calling
;; binder-sidebar-find-file (RET) or binder-sidebar-find-file-other-window
;; (o) will visit the corresponding file.

;; Each item in the sidebar displays the following information:

;; 1. binder-sidebar-include-char (default x) denotes that this item is
;;    included when the "joining" the project (see **Concatentate** below).
;; 2. binder-sidebar-notes-char (default *) denotes that this item has
;;    some notes, which can be edited in binder-notes-mode (see below),
;;    or...
;; 3. binder-sidebar-missing-char (default ?) denote that the item's
;;    corresponding file cannot be found, but can be relocated by calling
;;    binder-sidebar-relocate (R). The item will also highlight red.
;; 4. The item name, which is either the file relative to the project
;;    directory or an arbitrary display name, which can be set by calling
;;    binder-sidebar-rename (r).
;; 5. The item tags, each prefixed with binder-sidebar-status-char (default
;;    #). The tags column can be set with the binder-sidebar-tags-column
;;    option.

;; To add an existing file, call binder-sidebar-add-file (a) or add all
;; files in directory with binder-sidebar-add-all-files (A).

;; Add a new file with binder-sidebar-new-file (M-RET). This prompts
;; for a file-name and adds this (possibly non-existent) file to the
;; project after the current file's index. If no file-name extension is
;; provided, use value of the current project's default-extension
;; property (default set with binder-default-file-extention option).

;; *Hint: you can use an alternate default file extension for different
;; projects by setting a directory local variable.*

;; Files can also be added to a project from outside the sidebar with
;; binder-add-file (C-c :).

;; Remove items with binder-sidebar-remove (d) -- this *does not delete
;; the files*, only removes them from the project, but it *does delete* the
;; corresponding notes and tags.

;; Items can be reordered with binder-sidebar-shift-up (M-p or M-up)
;; and binder-sidebar-shift-down (M-n or M-down).

;; Hide item file extensions by setting the
;; binder-sidebar-hide-file-extensions option. This can be toggled with
;; binder-sidebar-toggle-file-extensions (E).

;; The sidebar can be resized with binder-sidebar-shrink-window ({) and
;; binder-sidebar-enlarge-window (}). The window size is changed by the
;; number of columns specified in option binder-sidebar-resize-window-step.

;; You can customize how the sidebar window is displayed by setting
;; binder-sidebar-display-alist option.

;; ### Marking ###

;; Multiple items can be marked to add tags, toggle include state or
;; delete.

;; Call binder-sidebar-mark (m) to mark an item. Call binder-sidebar-unmark
;; (u) to unmark an item or binder-sidebar-unmark-all (U) for all sidebar
;; items.

;; ### Tags ###

;; A project is strictly a linear list. As your project grows, you may find
;; the number of items becomes unweidly. Tags can help organize a project.
;; An item can have any number of tags.

;; Add a tag to an item with binder-sidebar-add-tag (t). Remove a tag from
;; an item with binder-sidebar-remove-tag (T). You can tag/untag multiple
;; items at once by using marks.

;; Items listed in the sidebar can be narrowed to only show items with a
;; certain tag with binder-sidebar-narrow-by-tag (/) and/or only show items
;; without a certain tag with binder-sidebar-exclude-by-tag (\). Each of
;; these commands can be called multiple times with additional tags. Reset
;; the sidebar with binder-sidebar-refresh (g).

;; ## Notes ##

;; Project items can have notes, which are stored within the project file.

;; To open the notes buffer from the sidebar, call either
;; binder-sidebar-open-notes (z) or binder-sidebar-toggle-notes (i). To
;; open a project file's notes when visiting that file, call
;; binder-toggle-notes (C-c ").

;; n.b. *Notes are not automatically saved*.

;; Calling quit-window (C-c C-q) or binder-toggle-sidebar does not save
;; notes. You need to call either binder-notes-save (C-x C-s) or
;; binder-notes-save-and-quit-window (C-c C-c).

;; You can embiggen the notes window, to pop it out from the sidebar and
;; edit like a regular buffer window, with binder-notes-expand-window (C-c
;; C-l).

;; If you want the notes buffer to stay in sync with the item under the
;; cursor in the sidebar, change the option binder-notes-keep-in-sync. This
;; can be disconcerting, and again, notes are not automatically saved!

;; You can customize how the notes window is displayed by setting
;; binder-notes-display-alist option.

;; ## Concatenate ##

;; A writing project written in discrete pieces probably has an end goal of
;; being put together. Each Binder project item has a property of being
;; "included" or not. In the sidebar, an item's include state is toggled
;; with binder-sidebar-toggle-include (x).

;; When calling binder-sidebar-concat (c or v), project items marked as
;; included will be concatenated in a new buffer (separated by
;; binder-concat-separator string.)

;; The default mode of this buffer is set by binder-default-concat-mode.

;; *Hint: you can use an alternate default mode for different projects by
;; setting a directory local variable.*

;; In this buffer, calling binder-concat-find-original (C-c RET) will visit
;; the original file corresponding to the text at point.

;; ## Why not just use Org Mode? ##

;; [Org Mode] is nice, but it's also a very *heavy* tool that almost insists
;; that everything be done within Org Mode. This isn't useful if you want
;; to write in a different format, e.g. [Markdown] or [Fountain].

;; Also, I prefer to keep my writing in a collection of separate text
;; files. It feels nicer to work on something small and self-contained than
;; to organize a large file with headings and use indirect buffers with
;; narrowing.

;; [org mode]: https://orgmode.org
;; [markdown]: http://jblevins.org/projects/markdown-mode/
;; [fountain]: https://github.com/rnkn/fountain-mode

;; ## Requirements ##

;; - Emacs 24.4
;; - seq 2.20

;; ## Bugs and Feature Requests ##

;; Report bugs and feature requests at: <https://github.com/rnkn/binder/issues>

;; ## Start Here ##

;; This file is part of a Binder tutorial project. Enabled binder-mode and
;; type C-c ; to reveal this file in the binder siderbar.


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'lisp-mnt)
  (defconst binder-version
    (lm-version load-file-name)))

(require 'seq)

(defgroup binder ()
  "Work with a structured project of files."
  :group 'files)


;;; Core Variables

(defvar binder-format-version 2)
(defvar binder-file-header
  (format "\
;; -*- coding: utf-8; -*-
;; Binder-Format-Version: %s
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.\n\n" binder-format-version)
  "Header information for project file.")

(defvar binder--cache nil)
(defvar binder--modification-time nil)
(defvar binder--modification-count 0)
(defvar binder-narrow-tags nil)
(defvar binder-exclude-tags nil)
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

(defface binder-sidebar-tags
  '((t (:inherit (font-lock-variable-name-face))))
  "Default face for tags."
  :group 'binder-sidebar-faces)


;;; Core Non-interactive Functions

(defun binder-root ()
  "Return the root directory with a project file, or nil."
  (locate-dominating-file default-directory binder-default-file))

(defun binder-set-modified ()
  "Set the project data as modified."
  (cl-incf binder--modification-count)
  (setq binder--modification-time (current-time)))

(defun binder-set-unmodified ()
  "Set the project data as unmodified."
  (setq binder--modification-count 0
        binder--modification-time (current-time)))

(defun binder-cd (directory)
  "Set `binder-project-directory' to DIRECTORY and erase cache."
  (customize-set-variable 'binder-project-directory directory)
  (setq binder-narrow-tags nil
        binder-exclude-tags nil
        binder--notes-fileid nil
        binder--cache nil)
  (binder-set-unmodified))

(defun binder-init ()
  "Initialize an empty project file in `binder-project-directory'."
  (with-temp-buffer
    (insert binder-file-header
            (pp-to-string nil))
    (write-file (expand-file-name binder-default-file binder-project-directory))))

(defun binder-find-project-file ()
  "Find or initialize current project file."
  (let ((binder-file
         (expand-file-name binder-default-file binder-project-directory)))
    (unless (file-exists-p binder-file) (binder-init))
    binder-file))

(defun binder-write ()
  "Write project data to file."
  (let ((binder-file (binder-find-project-file)))
    (with-temp-buffer
      (insert binder-file-header
              (pp-to-string binder--cache))
      (write-file binder-file)))
  (binder-set-unmodified))

(defun binder-write-maybe ()
  "Call `binder-write' if modified threshold has been reached.
Otherwise call `binder-set-modified'."
  (if (<= binder-save-threshold binder--modification-count)
      (binder-write)
    (binder-set-modified)))

(defun binder-filter (data)
  "Filter project DATA by tags.
Included `binder-narrow-tags' and excluded `binder-exclude-tags'."
  (seq-filter
   (lambda (item)
     (and (seq-every-p
           (lambda (tag)
             (member tag (cdr (assq 'tags item))))
           binder-narrow-tags)
          (seq-every-p
           (lambda (tag)
             (not (member tag (cdr (assq 'tags item)))))
           binder-exclude-tags)))
   data))

(defun binder-upgrade (data version)
  "Upgrade project DATA in VERSION to `binder-format-version'."
  (cond ((not (stringp version))
         (cdr (assq 'structure data)))
        ((= (string-to-number version) binder-format-version)
         data)))

(defun binder-read (&optional filter)
  "Read current project data.
Reads from `binder--cache' if valid, or from project file if not.
With optional argument FILTER, call `binder-filter' on data."
  (let ((binder-file (binder-find-project-file))
        version)
    (when binder--cache
      (cond
       ;; If there's no project file found, signal an error.
       ((null binder-file)
        (user-error "No binder file found"))
       ;; If the cache doesn't refer to the project directory, set the cache to
       ;; nil.
       ((not (file-equal-p binder-project-directory (binder-root)))
        (setq binder--cache nil))
       ;; If the project file is newer than the cache, offer to revert from disk
       ;; (and write binder data), regardless, set the cache to nil.
       ((time-less-p binder--modification-time
                     (elt (file-attributes binder-file) 5))
        (unless (y-or-n-p "Project file changed on disk; revert from disk? ")
          (binder-write))
        (setq binder--cache nil))))
    ;; If the cache survived all that, it's valid, otherwise, read from the
    ;; project file.
    (unless binder--cache
      (with-temp-buffer
        (insert-file-contents binder-file)
        ;; Read Binder-Format-Version header
        (setq version (lm-header "binder-format-version"))
        (goto-char (point-min))
        (setq binder--cache
              (binder-upgrade (read (current-buffer)) version)))
      (binder-set-unmodified)))
  ;; Finally, return the cache.
  (if filter (binder-filter binder--cache) binder--cache))

(defun binder-ensure-in-project ()
  "Ensure the current file or directory is in the project."
  (let ((root (binder-root)))
    (cond
     ;; The project directory matches root, we're all good.
     ((and (stringp binder-project-directory)
           (stringp root)
           (file-equal-p binder-project-directory root))
      t)
     ;; The project directory does not match project root; offer to change it to
     ;; current project root.
     ((and (stringp binder-project-directory)
           (stringp root))
      (when (y-or-n-p (format "Outside of current project %s\nSwitch project directory to %s?"
                              binder-project-directory root))
        (binder-cd root)))
     ;; The project directory is set but we're not in a project; this is fine.
     ((stringp binder-project-directory)
      t)
     ;; The project directory is not set, but we're in a project; offer to set
     ;; it to current project root.
     ((stringp root)
      (binder-cd root)
      (message "Set binder directory to %s" root))
     ;; A fresh project; offer to set project directory to default-directory.
     (t
      (when (y-or-n-p (format "Set binder directory to %s?"
                              (abbreviate-file-name default-directory)))
        (binder-cd default-directory)
        (binder-init))))))

(defun binder-file-relative-to-root (filepath)
  "Return FILEPATH relative to binder root directory."
  (string-remove-prefix (expand-file-name (or binder-project-directory ""))
                        (expand-file-name filepath)))

(defun binder-get-item (fileid)
  "Return project item association list for FILEID."
  (assoc-string fileid (binder-read t)))

(defun binder-get-item-prop (fileid prop)
  "Return value of PROP for project item for FILEID."
  (cdr (assq prop (cdr (binder-get-item fileid)))))

(defun binder-set-item-prop (fileid prop value)
  "Set VALUE of PROP for binder item for FILEID."
  (let ((item (binder-get-item fileid)))
    (if (or (null value) (and (stringp value) (string-empty-p value)))
        (setf item (assq-delete-all prop item))
      (let ((prop-elt (assq prop item)))
        (if prop-elt
            (setcdr prop-elt value)
          (push (cons prop value) (cdr item)))))))

(defun binder-add-to-item-prop (fileid prop value)
  "Add VALUE to PROP for binder item for FILEID."
  (let ((prop-elt (binder-get-item-prop fileid prop)))
    (unless (member value prop-elt)
      (binder-set-item-prop fileid prop (push value prop-elt)))))

(defun binder-remove-from-item-prop (fileid prop value)
  (let ((prop-elt (binder-get-item-prop fileid prop)))
    (when (member value (binder-get-item-prop fileid 'tags))
      (binder-set-item-prop fileid prop (remove value prop-elt)))))

(defun binder-get-item-index (fileid &optional filter)
  "Return index position for project item for FILEID.
With option argument FILTER, return index when items are
filtered."
  (seq-position (binder-read filter) (binder-get-item fileid)))

(defun binder-insert-item (item index)
  "Insert binder ITEM at position INDEX."
  (unless (listp item) (setq item (list item)))
  (setq binder--cache
       (let ((structure (binder-read)))
         (append (seq-take structure index)
                 (cons item (seq-drop structure index))))))

(defun binder-delete-item (fileid)
  "Delete binder item for FILEID."
  (setq binder--cache
        (remove (binder-get-item fileid) (binder-read))))

(defun binder-get-prop-list (prop)
  "Return list of values for PROP."
  (delq nil
        (mapcar
         (lambda (item)
           (let ((value (cdr (assq prop item))))
             (when (and (stringp value) (< 0 (string-width value)))
               value)))
         (binder-read))))

(defun binder-get-tags (&optional filter)
  (let (tags)
    (mapc
     (lambda (item)
       (mapc
        (lambda (tag)
          (push tag tags))
        (cdr (assq 'tags item))))
     (binder-read filter))
    (seq-uniq tags 'string-equal)))

(defun binder-get-buffer-fileid ()
  "Return buffer binder fileid."
   (cond ((eq major-mode 'binder-sidebar-mode)
          binder--current-fileid)
         ((eq major-mode 'binder-notes-mode)
          binder--notes-fileid)
         (t
          (binder-file-relative-to-root
           (or (buffer-file-name) default-directory)))))


;;; Global Minor Mode

(defun binder-init-project (directory)
  (interactive "DInitialize empty project in directory: ")
  (binder-cd directory)
  (binder-init))

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

(defun binder-exit-hook ()
  "Ensure project data is saved on exit."
  (when binder-mode (binder-save 'prompt)))

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
  (when (binder-ensure-in-project)
    ;; Find the current file/directory fileid, if one.
    (let ((this-fileid (binder-get-buffer-fileid))
          (structure (binder-read t))
          index next-index)
      ;; If current file has an INDEX, get the NEXT-INDEX.
      (setq index (or (binder-get-item-index this-fileid t) 0)
            next-index (+ index n))
      ;; If NEXT-INDEX is within the filtered structure length, find the
      ;; Nth next/previous file.
      (if (not (<= 0 next-index (1- (length structure))))
          (message "End of binder")
        (find-file-existing
         (expand-file-name (car (elt structure next-index))
                           binder-project-directory))
        (binder-sidebar-refresh-window))
      ;; Setup the overriding keymap.
      (unless overriding-terminal-local-map
        (let ((keys (substring (this-single-command-keys) 0 -1))
              (map (cdr binder-navigation-map)))
          (mapc (lambda (k) (setq map (assq k map))) keys)
          (when (consp map) (set-transient-map (cdr map) t)))))))

(defun binder-previous (&optional n)
  "Visit Nth previous file in binder.
Or visit Nth next file if N is negative."
  (interactive "p")
  (binder-next (- n)))

(defun binder-add-file (fileid &optional index string)
  "Add a (possibly non-existent) FILEID at INDEX containing STRING.
If the current file is in the project, add at index after that
one, otherwise insert at end."
  (interactive "FAdd file (extension optional): ")
  (binder-ensure-in-project)
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (< 0 (string-width fileid))
    (user-error "No file name supplied"))
  ;; If FILEID is not a directory and no file extension was provided,
  ;; add the binder's default file extensions.
  (unless (or (file-directory-p fileid)
              (and (string-match "\\.[^.]+\\'" fileid)
                   (not (= 0 (match-beginning 0)))))
    (setq fileid
          (concat fileid "." binder-default-file-extention)))
  ;; If the file/directory does not exist, create it.
  (let ((filepath (expand-file-name fileid binder-project-directory)))
    (unless (file-exists-p filepath)
      (if (file-directory-p filepath)
          (make-directory filepath t)
        (with-current-buffer (find-file-noselect filepath)
          (when (stringp string) (insert string))
          (write-file filepath))))
    ;; Insert FILEID into binder at INDEX, or after current file.
    (unless (binder-get-item fileid)
      (unless index
        (let ((this-file-index
               (binder-get-item-index (binder-get-buffer-fileid) t)))
          (setq index (if this-file-index
                          (1+ this-file-index)
                        (length (binder-read))))))
      (binder-insert-item fileid index))
    (binder-write-maybe)
    ;; When binder sidebar is active, refresh it.
    (binder-sidebar-refresh-window)
    ;; Finally, visit the file FILEPATH.
    (let ((pop-up-windows binder-sidebar-pop-up-windows))
      (find-file filepath))))

(defun binder-extract-region-to-new-file (beg end fileid)
  "Extract region between BEG and END into new project file FILEID."
  (interactive "r\nFNew file name (extension optional): ")
  (binder-ensure-in-project)
  (let ((string (delete-and-extract-region beg end)))
    (binder-add-file fileid nil string)))

(defvar binder-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ]") #'binder-next)
    (define-key map (kbd "C-c [") #'binder-previous)
    map)
  "Navigational mode map for `binder-mode'.")

(defvar binder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") #'binder-reveal-in-sidebar)
    (define-key map (kbd "C-c '") #'binder-toggle-sidebar)
    (define-key map (kbd "C-c \"") #'binder-toggle-notes)
    (define-key map (kbd "C-c :") #'binder-add-file)
    map)
  "Main mode map for `binder-mode'.")

(set-keymap-parent binder-mode-map binder-navigation-map)

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
    (binder-save 'prompt)
    (remove-hook 'window-configuration-change-hook 'binder-highlight-in-sidebar)
    (when (window-live-p (get-buffer-window binder-sidebar-buffer))
      (with-selected-window (get-buffer-window binder-sidebar-buffer)
        (quit-window t)))
    (when (window-live-p (get-buffer-window binder-notes-buffer))
      (with-selected-window (get-buffer-window binder-notes-buffer)
        (quit-window t)))
    (setq binder--cache nil)))


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

(defcustom binder-sidebar-hide-tags
  nil
  "When non-nil, list items without tags."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-pop-up-windows
  nil
  "Non-nil means displaying a new buffer should make a new window."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-tags-column
  25
  "Integer for column to align tags."
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

(defcustom binder-sidebar-tags-char
  ?#
  "Character to prefix to item tags."
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

(defcustom binder-sidebar-resize-window-step
  4
  "Integer of columns by which to resize window.
Used by `binder-sidebar-shrink-window' and `binder-sidebar-enlarge-window'."
  :type 'integer
  :safe 'integerp
  :group 'binder-sidebar)

(defvar binder--current-fileid nil)
(defvar binder--sidebar-marked nil)

(defun binder-sidebar-format-header-line ()
  (setq header-line-format
        (list :propertize (abbreviate-file-name binder-project-directory)
              'face 'bold)))

(defun binder-sidebar-refresh (&optional clear-filter)
  "Redraw binder sidebar, reading from cache.
When called interactively (or with optional CLEAR-FILTER) clear
filter by tags."
  (interactive "p")
  (with-silent-modifications
    (setq default-directory binder-project-directory)
    (hack-local-variables)
    (binder-sidebar-format-header-line)
    (when clear-filter (setq binder-narrow-tags nil
                             binder-exclude-tags nil))
    (let ((x (point)))
      (erase-buffer)
      (mapc
       (lambda (item)
         (let ((fileid   (car item))
               (include  (cdr (assq 'include item)))
               (display  (cdr (assq 'display item)))
               (notes    (cdr (assq 'notes item)))
               (tags     (cdr (assq 'tags item)))
               marked missing tags-overwrite)
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
          ;; Add the item TAGS with a hashtag, because hashtags are cool, right?
          (when (and (not binder-sidebar-hide-tags) tags)
            (move-to-column (1- binder-sidebar-tags-column))
            (unless (eolp) (setq tags-overwrite t))
            (move-to-column binder-sidebar-tags-column)
            (indent-to-column binder-sidebar-tags-column)
            (let ((tags-col  (point))
                  (tags-char (char-to-string binder-sidebar-tags-char)))
              (delete-region (1- tags-col) (line-end-position))
              (insert (if tags-overwrite "~" " ")
                      tags-char
                      (string-join tags (concat " " tags-char)))
              (put-text-property tags-col (line-end-position)
                                 'face 'binder-sidebar-tags)))
          (insert "\n")
          (when (string= fileid binder--current-fileid)
            (put-text-property (line-beginning-position 0)
                               (line-beginning-position)
                               'face 'binder-sidebar-highlight))))
       (binder-read t))
      (goto-char x))))

(defun binder-sidebar-refresh-window ()
  "Call `binder-sidebar-refresh' if sidebar window is live."
  (when (window-live-p (get-buffer-window binder-sidebar-buffer))
    (with-current-buffer binder-sidebar-buffer
      (binder-sidebar-refresh))))

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
  (if (eobp)
      (1- (length (binder-read)))
    (binder-get-item-index (binder-sidebar-get-fileid) t)))

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
  (binder-write-maybe)
  (binder-sidebar-refresh)
  (binder-sidebar-goto-item fileid))

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
      (setq binder--sidebar-marked nil)))
  (binder-write)
  (binder-sidebar-refresh))

(defun binder-sidebar-rename ()
  "Change display name of binder item at point."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid))
        name)
    (setq name
          (read-string "New name: "
                       (or (binder-get-item-prop fileid 'display) fileid)))
    (binder-set-item-prop fileid 'display name)
    (binder-write-maybe)
    (binder-sidebar-refresh)))

(defun binder-sidebar-relocate (filepath)
  "Change file path of binder item at point to FILEPATH."
  (interactive "fNew file path: ")
  (setq filepath (binder-file-relative-to-root filepath))
  (setcar (binder-get-item (binder-sidebar-get-fileid)) filepath)
  (binder-write-maybe)
  (binder-sidebar-refresh))

(defun binder-sidebar-toggle-include ()
  "Toggle whether marked items or item at point is included in `binder-sidebar-concat'."
  (interactive)
  (dolist (fileid (or binder--sidebar-marked
                      (list (binder-sidebar-get-fileid))))
    (binder-set-item-prop fileid 'include
                          (not (binder-get-item-prop fileid 'include))))
  (setq binder--sidebar-marked nil)
  (binder-write-maybe)
  (binder-sidebar-refresh))

(defun binder-sidebar-clear-include ()
  "Make no items included in `binder-sidebar-concat'."
  (interactive)
  (dolist (item (binder-read t))
    (binder-set-item-prop (car item) 'include nil))
  (binder-write-maybe)
  (binder-sidebar-refresh))

(defun binder-sidebar-add-tag (tag)
  "Add TAG to marked items or item at point."
  (interactive
   (list (completing-read
          "Add tag: " (binder-get-tags))))
  (mapc
   (lambda (fileid)
     (binder-add-to-item-prop fileid 'tags tag))
   (or binder--sidebar-marked
       (list (binder-sidebar-get-fileid))))
  (setq binder--sidebar-marked nil)
  (binder-write-maybe)
  (binder-sidebar-refresh))

(defun binder-sidebar-remove-tag (tag)
  "Remove TAG to marked items or item at point."
  (interactive
   (list (completing-read
          "Remove tag: "
          (binder-get-item-prop (binder-sidebar-get-fileid) 'tags))))
  (mapc
   (lambda (fileid)
     (binder-remove-from-item-prop fileid 'tags tag))
   (or binder--sidebar-marked
       (list (binder-sidebar-get-fileid))))
  (setq binder--sidebar-marked nil)
  (binder-write-maybe)
  (binder-sidebar-refresh))

(defun binder-sidebar-toggle-file-extensions ()
  "Toggle visibility of item file extensions."
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

(defun binder-sidebar-toggle-tags ()
  "Toggle visibility of tags."
  (interactive)
  (customize-set-variable 'binder-sidebar-hide-tags
                          (not binder-sidebar-hide-tags))
  (let ((fileid (binder-sidebar-get-fileid)))
    (binder-sidebar-refresh)
    (binder-sidebar-goto-item fileid))
  (message "%s tags"
           (capitalize
            (if binder-sidebar-hide-tags
                "hiding" "showing"))))

;; FIXME: fails with filtered structure
(defun binder-sidebar-shift-down (&optional n)
  "Shift index position of item at point down N places in list."
  (interactive "p")
  (let ((p (if (<= n 0) -1 1))
        (fileid (binder-sidebar-get-fileid))
        item fake-index next-index)
    (setq item (binder-get-item fileid)
          fake-index (binder-get-item-index fileid t)
          next-index (seq-position (binder-read)
                                   (elt (binder-read t) (+ fake-index p))))
    (when (<= 0 (+ fake-index p)
              (1- (length (binder-read t))))
      (binder-delete-item fileid)
      (binder-insert-item item next-index)
      (binder-write-maybe)
      (binder-sidebar-refresh)
      (binder-sidebar-goto-item fileid))))

(defun binder-sidebar-shift-up (&optional n)
  "Shift index position of item at point up N places in list."
  (interactive "p")
  (binder-sidebar-shift-down (- n)))

(defun binder-sidebar-jump-to-current ()
  "Jump to current file in sidebar."
  (interactive)
  (binder-sidebar-goto-item binder--current-fileid))

(defun binder-sidebar-narrow-by-tag (tag)
  "Filter sidebar items to include items with TAG."
  (interactive
   (list (completing-read
          "Narrow items by tag: " (binder-get-tags)
          nil t)))
  (unless (or (string-empty-p tag) (member tag binder-narrow-tags))
    (push tag binder-narrow-tags)
    (binder-sidebar-refresh)))

(defun binder-sidebar-exclude-by-tag (tag)
  "Filter sidebar items to exclude items with TAG."
  (interactive
   (list (completing-read
          "Exclude items by tag: " (binder-get-tags)
          nil t)))
  (unless (or (string-empty-p tag) (member tag binder-exclude-tags))
    (push tag binder-exclude-tags)
    (binder-sidebar-refresh)))

(defun binder-highlight-in-sidebar ()
  "Highlight the current file in sidebar.

Added to `window-configuration-change-hook'."
  (unless (or (minibuffer-window-active-p (selected-window))
              (eq major-mode 'binder-sidebar-mode))
    (setq binder--current-fileid (binder-get-buffer-fileid))
    (binder-sidebar-refresh-window)))

(defun binder-sidebar-shrink-window ()
  "Shrink the sidebar window horizontally.

Calls `shrink-window-horizontally' with `binder-sidebar-resize-window-step'."
  (interactive)
  (shrink-window-horizontally binder-sidebar-resize-window-step))

(defun binder-sidebar-enlarge-window ()
  "Enlarge the sidebar window horizontally.

Calls `enlarge-window-horizontally' with `binder-sidebar-resize-window-step'."
  (interactive)
  (enlarge-window-horizontally binder-sidebar-resize-window-step))

(defun binder-sidebar-help (char)
  "Interactively set project-specific properties by CHAR."
  (declare (interactive-only t))
  (interactive
   (list (read-char-choice
          "? = describe-mode, q = quit-window, C-g = cancel: "
          '(?? ?q))))
  (cond ((= char ?q)
         (quit-window))
        ((= char ??)
         (describe-mode))))

;;;###autoload
(defun binder-reveal-in-sidebar ()
  "Reveal current file in binder sidebar.

Unconditionally activates `binder-mode'."
  (interactive)
  (binder-mode)
  (let ((filepath (or (buffer-file-name) default-directory)))
    (select-window (binder-sidebar-create-window))
    (if (file-equal-p filepath (binder-root))
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

(defvar binder-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'binder-sidebar-help)
    (define-key map (kbd "{") #'binder-sidebar-shrink-window)
    (define-key map (kbd "}") #'binder-sidebar-enlarge-window)
    (define-key map (kbd "g") #'binder-sidebar-refresh)
    (define-key map (kbd "j") #'binder-sidebar-jump-to-current)
    (define-key map (kbd "C") #'binder-sidebar-change-directory)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "RET") #'binder-sidebar-find-file)
    (define-key map (kbd "o") #'binder-sidebar-find-file-other-window)
    (define-key map (kbd "s") #'binder-sidebar-save)
    (define-key map [remap save-buffer] #'binder-sidebar-save)
    (define-key map (kbd "m") #'binder-sidebar-mark)
    (define-key map (kbd "u") #'binder-sidebar-unmark)
    (define-key map (kbd "t") #'binder-sidebar-add-tag)
    (define-key map (kbd "T") #'binder-sidebar-remove-tag)
    (define-key map (kbd "#") #'binder-sidebar-toggle-tags)
    (define-key map (kbd "U") #'binder-sidebar-unmark-all)
    (define-key map (kbd "c") #'binder-sidebar-concat)
    (define-key map (kbd "v") #'binder-sidebar-concat)
    (define-key map (kbd "i") #'binder-sidebar-toggle-notes)
    (define-key map (kbd "z") #'binder-sidebar-open-notes)
    (define-key map (kbd "M-n") #'binder-sidebar-shift-down)
    (define-key map (kbd "<M-down>") #'binder-sidebar-shift-down)
    (define-key map (kbd "M-p") #'binder-sidebar-shift-up)
    (define-key map (kbd "<M-up>") #'binder-sidebar-shift-up)
    (define-key map (kbd "a") #'binder-sidebar-add-file)
    (define-key map (kbd "A") #'binder-sidebar-add-all-files)
    (define-key map (kbd "d") #'binder-sidebar-remove)
    (define-key map (kbd "r") #'binder-sidebar-rename)
    (define-key map (kbd "R") #'binder-sidebar-relocate)
    (define-key map (kbd "E") #'binder-sidebar-toggle-file-extensions)
    (define-key map (kbd "x") #'binder-sidebar-toggle-include)
    (define-key map (kbd "X") #'binder-sidebar-clear-include)
    (define-key map (kbd "/") #'binder-sidebar-narrow-by-tag)
    (define-key map (kbd "\\") #'binder-sidebar-exclude-by-tag)
    (define-key map (kbd "M-RET") #'binder-sidebar-new-file)
    map))

;;;###autoload
(define-derived-mode binder-sidebar-mode
  special-mode "Binder Sidebar"
  "Major mode for working with `binder' projects."
  (add-hook 'post-command-hook 'binder-sidebar-sync-notes t t))


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

(defun binder-show-notes (&optional fileid select)
  "Show the notes for FILEID of the appropriate project item.
If argument SELECT is non-nil, select the notes window."
  (binder-ensure-in-project)
  (if fileid
      (setq binder--notes-fileid fileid)
    (let ((filepath (or (buffer-file-name) default-directory)))
      (unless (file-equal-p filepath (binder-root))
        (setq binder--notes-fileid (binder-file-relative-to-root filepath)))))
  (if (window-live-p (get-buffer-window binder-notes-buffer))
      (binder-notes-refresh-window)
    (binder-notes-create-window))
  (when select (select-window (get-buffer-window binder-notes-buffer))))

(defun binder-sidebar-open-notes ()
  "Open notes for item at point and select the notes window."
  (interactive)
  (binder-show-notes (binder-sidebar-get-fileid) t))

(defun binder-toggle-notes ()
  "Toggle visibility of binder notes window."
  (interactive)
  (if (window-live-p (get-buffer-window binder-notes-buffer))
      (delete-window (get-buffer-window binder-notes-buffer))
    (binder-show-notes)))

(defun binder-sidebar-toggle-notes ()
  "Toggle visibility of binder notes window for item at point."
  (interactive)
  (if (window-live-p (get-buffer-window binder-notes-buffer))
      (delete-window (get-buffer-window binder-notes-buffer))
    (binder-show-notes (binder-sidebar-get-fileid))))

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
  (declare (interactive-only t))
  (interactive)
  (binder-notes-save)
  (quit-window))

(defun binder-notes-expand-window ()
  "Toggle the notes window from a side window to full window."
  (declare (interactive-only t))
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (window-parameter (selected-window) 'window-side)
      (progn
        (quit-window)
        (pop-to-buffer (get-buffer-create binder-notes-buffer)))
    (quit-window)
    (binder-show-notes binder--notes-fileid t)))

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


;;; Concat Mode

(defgroup binder-concat ()
  "Options for `binder-concat-mode'."
  :group 'binder)

(defcustom binder-default-concat-mode
  'text-mode
  "Default major mode when concatenating files."
  :type 'function
  :safe 'functionp
  :group 'binder-concat)

(defcustom binder-concat-separator "\n"
  "String to insert between concatenated project files."
  :type 'string
  :group 'binder-concat)

(defcustom binder-concat-buffer
  "*Binder Concat View*"
  "Buffer name for viewing a concatenated project."
  :type 'string
  :safe 'stringp
  :group 'binder-concat)

(defun binder-concat ()
  "Concatenate all project files marked as included.
Creates `binder-concat-buffer' with each file separated by
`binder-concat-separator' and sets the major mode to
`binder-default-concat-mode'.

See `binder-sidebar-toggle-include'."
  (interactive)
  (binder-ensure-in-project)
  (let ((item-list
         (seq-filter
          (lambda (item) (cdr (assq 'include item)))
          (binder-read t)))
        (concat-mode binder-default-concat-mode))
    (with-current-buffer (get-buffer-create binder-concat-buffer)
      (with-silent-modifications
        (erase-buffer)
        (dolist (item item-list)
          (let ((x (point)))
            (insert-file-contents
             (expand-file-name (car item) binder-project-directory))
            (goto-char (point-max))
            (insert binder-concat-separator)
            (put-text-property x (point) 'binder-original-file
                               (expand-file-name (car item) binder-project-directory)))))
      (funcall concat-mode)
      (binder-concat-mode t))
    (if (eq major-mode 'binder-sidebar-mode)
        (let ((pop-up-windows binder-sidebar-pop-up-windows))
          (pop-to-buffer binder-concat-buffer))
      (pop-to-buffer binder-concat-buffer))))

(defalias 'binder-sidebar-concat 'binder-concat)

(defun binder-concat-find-original ()
  "Find the file containing content at point."
  (interactive)
  (unless binder-concat-mode
    (user-error "Not in %S" 'binder-concat-mode))
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

(defcustom binder-concat-mode-on-hook
  '(view-mode)
  "Hook run after entering `binder-concat-mode'."
  :type 'hook
  :group 'binder-concat)

(defvar binder-concat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'binder-concat-find-original)
    map))

(define-minor-mode binder-concat-mode
  "Minor mode for viewing concatenated project files."
  :init-value nil)



(provide 'binder)
;;; binder.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:

;;; doc-tags.el --- Create sqlite database of tagged file-system documents   -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: April 17, 2025
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/doc-tags
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'triples)
(require 'triples-backups)

(defcustom doc-tags-db-file nil
  "Location of doc-tags database file."
  :type '(file)
  :group 'doc-tags)

(defvar doc-tags-db nil
  "Live sqlite database connection.")

(defconst doc-tags-default-num-backups 5
  "The number of backups to set when first using the database.
This can be overwritten by other database users, and will not be
set again.  If you want to change the number of backups in your
database after it has been created, run `triples-backups-setup'.")

(defconst doc-tags-default-backups-strategy 'daily
  "The default database backup strategy when first setting up the database.
This can be overwritten by other database users, and
will not be set again.  If you want to change the number of
backups in your database after it has been created, run
`triples-backups-setup'.")

(defun doc-tags-connect ()
  "Initialize doc-tags."
  (interactive)
  (let ((db (triples-connect doc-tags-db-file)))
    (unless doc-tags-db
      (setq doc-tags-db db))
    (doc-tags-add-schema)
    (unless (triples-backups-configuration doc-tags-db)
      (triples-backups-setup doc-tags-db doc-tags-default-num-backups
                             doc-tags-default-backups-strategy))))

(defun doc-tags-close-db ()
  "Close current `doc-tags-db’ connection."
  (interactive)
  (when doc-tags-db
    (triples-close doc-tags-db)
    (setq doc-tags-db nil)))

(defun doc-tags-add-schema ()
  "Add schema necessary for doc-tags to function."
  (triples-add-schema doc-tags-db 'doc
                      '(tags :base/type string))
  (triples-add-schema doc-tags-db 'tag
                      '(members :base/virtual-reversed doc/tags)))

;;; helper functions

(defun doc-tags-all-docs ()
  "Return all docs in doc-tags."
  (triples-subjects-of-type doc-tags-db 'doc))

(defun doc-tags-untagged-docs ()
  "Return all docs with no tags."
  (let* ((docs (doc-tags-all-docs))
         untagged)
    (dolist (doc docs)
      (unless (doc-tags-get-doc-tags doc)
        (push doc untagged)))
    untagged))

(defun doc-tags-all-tags ()
  "Return list of all tags."
  (triples-subjects-of-type doc-tags-db 'tag))

(defun doc-tags-get-doc-tags (doc)
  "Return tags for DOC."
  (ensure-list (plist-get
                (triples-get-subject doc-tags-db doc)
                :doc/tags)))

(defun doc-tags-get-tag-members (tag)
  "Return docs for TAG."
  (plist-get
   (triples-get-subject doc-tags-db tag)
   :tag/members))

;;; doc functions

(defun doc-tags-add-doc (doc &optional tags)
  "Add DOC to `doc-tags-db’.
Optionals TAGS"
  (interactive "fAdd doc: ")
  (setq doc (expand-file-name doc))
  (doc-tags-connect)
  (when (triples-get-subject doc-tags-db doc) ;; need to skip this when relocating a file
    (user-error "Selected file already in database \“%s\”" doc-tags-db))
  (let* ((doc-name (if (file-directory-p doc)
                       (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory doc)))
                     (file-name-nondirectory doc)))
         (tags (or tags
                   (completing-read-multiple
                    (format "Tags for %s: " doc-name)
                    (doc-tags-all-tags) nil nil))))
    (triples-set-subject doc-tags-db doc
                         `(doc :tags ,tags))
    (dolist (tag tags)
      (triples-set-type doc-tags-db tag 'tag))))

(defun doc-tags-locate-doc (doc)
  "Locate file for missing DOC."
  (let ((tags (doc-tags-get-doc-tags doc))
        (new-doc (progn
                   (file-name-history--add doc)
                   (read-file-name "Find file: "
                                   (file-name-directory doc) nil t))))
    (doc-tags-add-doc new-doc tags)
    (doc-tags-remove-doc doc)))

(defun doc-tags-remove-doc (doc)
  "Remove DOC from database."
  (interactive (list (doc-tags-select-doc)))
  (when (y-or-n-p (format "Remove from database: \“%s\”?" doc))
    (triples-delete-subject doc-tags-db doc)
    (message "Removed \“%s\” from database" doc)
    (doc-tags-delete-empty-tags)))

;;; tag functions

(defun doc-tags-add-tag (doc &optional tags)
  "Add TAGS to DOC."
  (interactive (list (doc-tags-select-doc)))
  (let* ((add-tags (or tags
                       (completing-read-multiple
                        (format "Tags for %s: "
                                (file-name-nondirectory doc))
                        (doc-tags-all-tags) nil nil)))
         (doc-tags (seq-uniq (flatten-tree (list add-tags (doc-tags-get-doc-tags doc))))))
    (triples-set-type doc-tags-db doc 'doc :tags doc-tags)
    (dolist (tag add-tags)
      (triples-set-type doc-tags-db tag 'tag))))

(defun doc-tags-remove-tag (doc &optional tag edit-tag)
  "Remove tag from DOC.
With option TAG and EDIT-TAG."
  (interactive (list (doc-tags-select-doc)))
  (let* ((doc-tags (doc-tags-get-doc-tags doc))
         (del-tags (or tag
                       (doc-tags-select-tag doc-tags "Remove tag: "))))
    (dolist (tag del-tags)
      (setq doc-tags (delete tag doc-tags)))
    (unless (or edit-tag
                (y-or-n-p (format "Remove tags\n%s\nfrom \“%s\”?"
                                  (doc-tags-format-tags del-tags)
                                  (file-name-nondirectory doc))))
      (user-error "Tag removal cancelled"))
    (if doc-tags
        (triples-set-type doc-tags-db doc 'doc :tags doc-tags)
      (triples-set-subject doc-tags-db doc)
      (unless edit-tag
        (if (y-or-n-p "Untagged doc. Delete from database [y]? Add other tag [n]?")
            (doc-tags-remove-doc doc)
          (setq doc-tags (doc-tags-select-tag))
          (doc-tags-add-tag doc doc-tags))))
    (unless edit-tag
      (doc-tags-delete-empty-tags))))

(defun doc-tags-edit-tag (tag)
  "Edit doc-tag TAG."
  (interactive (list (completing-read
                      "Edit doc-tag: "
                      (doc-tags-all-tags)
                      nil t)))
  (let ((docs (doc-tags-get-tag-members tag))
        (new-tag (read-string "Edit doc-tag: " tag)))
    (dolist (doc docs)
      (doc-tags-remove-tag doc (list tag) t)
      (doc-tags-add-tag doc (list new-tag)))
    (triples-delete-subject doc-tags-db tag)
    (message "Doc-tag edited: \“%s\” -> \“%s\”" tag new-tag)))

(defun doc-tags-format-tags (tags)
  "Format list of TAGS into list of string."
  (if (listp tags)
      (mapconcat (lambda (x)
                   (format "\“%s\” " x))
                 tags)
    (format "\“%s\” " tags)))

(defun doc-tags-empty-tags ()
  "Return list of tags with no associated files."
  (delq nil
        (mapcar (lambda (tag)
                  (unless
                      (triples-get-subject doc-tags-db tag)
                    tag))
                (doc-tags-all-tags))))

(defun doc-tags-delete-empty-tags ()
  "Delete all empty tags in doc-tags."
  (interactive)
  (when-let* ((empties (doc-tags-empty-tags)))
    (if (and (> (length empties) 1)
             (y-or-n-p (format "Empty tags: %s\nSelect for deletion?"
                               (doc-tags-format-tags empties))))
        (setq empties (doc-tags-select-tag empties "Delete empty tags: "))
      (unless (y-or-n-p (format "Delete empty tag:\n\“%s\”?" (car empties)))
        (setq empties nil)))
    (when empties
      (mapc
       (lambda (tag)
         (triples-delete-subject doc-tags-db tag))
       empties))))

;;; find files

(defun doc-tags-find-file ()
  "Find `doc-tag’ docs by tag.
Boolean operator AND by default; use prefix arg for OR."
  (interactive)
  (pcase-let* ((`(,bool1 . ,bool2)
                (if (or current-prefix-arg
                        (eq this-command 'doc-tags-find-file-or))
                    (cons " OR " "ANY")
                  (cons " AND " "ALL")))
               (tags (doc-tags-select-tag
                      nil (format "Find docs with %s tags: " bool2)))
               (all-docs (mapcar #'doc-tags-get-tag-members tags))
               (docs (cond ((string= bool1 " OR ")
                            (seq-uniq
                             (flatten-tree
                              (mapcar #'doc-tags-get-tag-members tags))))
                           ((string= bool1 " AND ")
                            (seq-reduce #'seq-intersection
                                        all-docs
                                        (car all-docs))))))
    (if docs
        (doc-tags-open-doc (doc-tags-select-doc docs))
      (error "No docs tagged: %s" (string-join tags bool1)))))

(defun doc-tags-find-file-and ()
  "Find `doc-tag’ docs using AND operator."
  (interactive)
  (doc-tags-find-file))

(defun doc-tags-find-file-or ()
  "Find `doc-tag’ docs using OR operator."
  (interactive)
  (doc-tags-find-file))

(defun doc-tags-open-doc (doc)
  "Find DOC in `doc-tags-db’."
  (interactive (list (doc-tags-select-doc)))
  (if (file-exists-p doc)
      (find-file doc)
    (when (y-or-n-p (format "Doc not found: %s. Locate file?" doc))
      (doc-tags-locate-doc doc))))

;;; completing read functions

(defvar doc-tags-tag-history nil)
(defvar doc-tags-doc-history nil)

(defun doc-tags-select-tag (&optional tags prompt)
  "Completing read function for selecting a tag.
With optional TAGS and PROMPT values."
  (doc-tags-connect)
  (let ((tags (or tags
                  (doc-tags-all-tags))))
    (completing-read-multiple
     (or prompt
         "Select tag: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . doc-tags-tag)
             (annotation-function . doc-tags-annotate-tag))
         (complete-with-action action tags string predicate)))
     nil t nil 'doc-tags-tag-history)))

(defun doc-tags-annotate-tag (tag)
  "Annotation function for TAG candidates."
  (let ((count (length
                (doc-tags-get-tag-members
                 (substring-no-properties tag)))))
    (format " [%s]" count)))

(defun doc-tags-select-doc (&optional docs prompt initial)
  "Completing read function for selecting DOCS.
With optional PROMPT and INITIAL value."
  (doc-tags-connect)
  (let ((docs (or docs (doc-tags-all-docs))))
    (completing-read
     (or prompt "Select doc: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . doc-tags-doc)
             (group-function . doc-tags-group-function)
             (annotation-function . doc-tags-annotate-doc))
         (complete-with-action action docs string predicate)))
     nil t initial 'doc-tags-doc-history)))

(defun doc-tags-group-function (doc transform)
  "TRANSFORM completion candidate DOC."
  (if transform
      (file-name-nondirectory doc)
    (file-name-extension doc t)))

(defun doc-tags-annotate-doc (doc)
  "Annotation function for DOC candidates."
  (let* ((doc (substring-no-properties doc))
         (num (- 60 (length (file-name-nondirectory doc))))
         (spaces (if (wholenump num)
                     (make-string num ? )
                   ""))
         (tags (doc-tags-get-doc-tags doc))
         (format-tags (doc-tags-format-tags tags)))
    (format "%s  tags: %s" spaces format-tags)))

;;; embark integration

(defvar embark-file-map)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

(defvar doc-tags-file-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-file-map)
    (define-key map (kbd "d") #'doc-tags-remove-doc)
    (define-key map (kbd "a") #'doc-tags-add-tag)
    (define-key map (kbd "r") #'doc-tags-remove-tag)
    map)
  "Keymap for Embark doc-tags actions.")

(add-to-list 'embark-keymap-alist '(doc-tags-doc . doc-tags-file-map))

(add-to-list 'embark-default-action-overrides
             '(doc-tags-doc . doc-tags-open-doc))

(defvar doc-tags-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'doc-tags-edit-tag)
    map)
  "Keymap for Embark doc-tags actions.")

(add-to-list 'embark-keymap-alist '(doc-tags-tag . doc-tags-tag-map))

(provide 'doc-tags)
;;; doc-tags.el ends here

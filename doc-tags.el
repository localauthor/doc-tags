;;; doc-tags.el --- Create database of tagged file-system documents   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'triples)
(require 'triples-backups)

;; add doc-tags-db-dir, for switching databases?

(defvar doc-tags-db-file nil)

(defvar doc-tags-db nil
  "Live sqlite database connection.")

(defvar doc-tags-auto-delete-empty-tags t)

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

(defun doc-tags-close ()
  "Close the gr-db connection."
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
  (doc-tags-connect)
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
  (doc-tags-connect)
  (triples-subjects-of-type doc-tags-db 'tag))

(defun doc-tags-get-doc-name (doc)
  "Return name for DOC."
  (plist-get (triples-get-subject doc-tags-db doc)
             :doc/name))

(defun doc-tags-get-doc-tags (doc)
  "Return tags for DOC."
  (ensure-list (plist-get
                (triples-get-subject doc-tags-db doc)
                :tagged/tags)))

;;; add doc to db

(defun doc-tags-add-doc (doc)
  "Add DOC to `doc-tags-db’."
  (interactive "fAdd doc: ")
  (doc-tags-connect)
  (let* ((doc-name (if (file-directory-p doc)
                       (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory doc)))
                     (file-name-nondirectory doc)))
         (tags (completing-read-multiple
                (format "Tags for %s: " doc-name)
                (doc-tags-all-tags) nil nil)))
    (triples-set-subject doc-tags-db doc
                         `(doc :tags ,tags))
    (dolist (tag tags)
      (triples-set-type doc-tags-db tag 'tag))))


;;; remove doc from db

(defun doc-tags-remove-doc (doc)
  "Remove DOC from database."
  (interactive (list (doc-tags-select-doc)))
  (doc-tags-connect)
  (when (y-or-n-p (format "Remove \“%s\” from database?" doc))
    (triples-delete-subject doc-tags-db doc)
    (message "Removed \“%s\” from database" doc)
    (when doc-tags-auto-delete-empty-tags
      (doc-tags-delete-empty-tags))))


;;; tag functions

(defun doc-tags-add-tag (doc)
  "Add tag to DOC."
  (interactive (list (doc-tags-select-doc)))
  (let* ((add-tags (completing-read-multiple
                    (format "Tags for %s: " doc)
                    (doc-tags-all-tags) nil nil))
         (doc-tags (delete-dups (flatten-list (list add-tags (doc-tags-get-doc-tags doc))))))
    (triples-set-type doc-tags-db doc 'tagged :tags doc-tags)
    (dolist (tag add-tags)
      (triples-set-type doc-tags-db tag 'tag))))

(defun doc-tags-delete-doc-tag (doc)
  "Delete tag from DOC."
  (interactive (list (doc-tags-select-doc)))
  (let* ((doc-tags (doc-tags-get-doc-tags doc))
         (del-tags (completing-read-multiple
                    "Delete tags: "
                    doc-tags)))
    (dolist (tag del-tags)
      (setq doc-tags (delete tag doc-tags)))
    (if doc-tags
        (triples-set-type doc-tags-db doc 'tagged :tags doc-tags)
      (if (y-or-n-p "Untagged doc. Delete from database [y]? Add other tag [n]?")
          (doc-tags-remove-doc doc)
        (setq doc-tags (doc-tags-select-tag))
        (triples-set-type doc-tags-db doc 'tagged :tags doc-tags))))
  (doc-tags-delete-empty-tags))

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
                      (triples-search doc-tags-db
                                      'tagged/tags tag)
                    tag))
                (doc-tags-all-tags))))

(defun doc-tags-delete-empty-tags ()
  "Delete all empty tags in doc-tags."
  (interactive)
  (when-let* ((empties (doc-tags-empty-tags)))
    (when (y-or-n-p (format "Delete empty tags:\n%s?"
                            (doc-tags-format-tags empties)))
      (mapc
       (lambda (tag) (triples-delete-subject doc-tags-db tag))
       empties))))

;;; find files

(defun doc-tags-find-file (doc)
  "Find DOC in `doc-tags-db’."
  (interactive (list (doc-tags-select-doc)))
  (if (file-exists-p doc)
      (find-file doc)
    (error "File not found: %s" doc)))

(defun doc-tags-find-file-by-tag ()
  "Find file by tag."
  (interactive)
  (doc-tags-connect)
  (if-let* ((tag (doc-tags-select-tag))
            (docs (mapcar #'car
                          (triples-search doc-tags-db
                                          'tagged/tags tag)))
            (doc (doc-tags-select-doc docs)))
      (doc-tags-find-file doc)
    (error "No files for tag: \“%s\”" tag)))


;;; completing read functions

(defun doc-tags-select-tag ()
  "Completing read function for selecting a tag.
With optional PROMPT value."
  (let ((tags (doc-tags-all-tags)))
    (completing-read
     "Select tag: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (annotation-function . doc-tags-annotate-tag))
         (complete-with-action action tags string predicate)))
     nil t)))

(defun doc-tags-annotate-tag (tag)
  "Annotation function for TAG candidates."
  (let ((count (length
                (triples-search doc-tags-db
                                'tagged/tags tag))))
    (format " [%s]" count)))

(defun doc-tags-select-doc (&optional docs prompt initial)
  "Completing read function for selecting DOCS.
With optional PROMPT and INITIAL value."
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
    (file-name-extension doc)))

(defun doc-tags-annotate-doc (doc)
  "Annotation function for DOC candidates."
  (let* ((doc (substring-no-properties doc))
         (tags (doc-tags-get-doc-tags doc))
         (format-tags (doc-tags-format-tags tags)))
    (format " | tags: %s" format-tags)))


;;; embark integration

(defvar embark-file-map)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

(defvar doc-tags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-file-map)
    (define-key map (kbd "d") #'doc-tags-remove-doc)
    (define-key map (kbd "a") #'doc-tags-add-tag)
    (define-key map (kbd "r") #'doc-tags-remove-tag)
    map)
  "Keymap for Embark doc-tags actions.")

(add-to-list 'embark-keymap-alist '(doc-tags-doc . doc-tags-map))

(add-to-list 'embark-default-action-overrides
             '(doc-tags-doc . doc-tags-open-doc))

(provide 'doc-tags)
;;; doc-tags.el ends here

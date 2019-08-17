;;; lsp-treemacs.el --- LSP treemacs                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ivan Yonchovski

;; Author: Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (treemacs "2.5") (lsp-mode "6.0"))
;; Homepage: https://github.com/emacs-lsp/lsp-treemacs
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `lsp-mode' and `treemacs' integration.

;;; Code:
(require 'treemacs)
(require 's)
(require 'f)
(require 'treemacs-extensions)
(require 'treemacs-icons)

(require 'lsp-mode)

(defconst lsp-treemacs-deps-buffer-name "*Java Dependency List*")


(defgroup lsp-treemacs nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server")

(defvar lsp-treemacs-deps-position-params
  '((side . left)
    (slot . 1)
    (window-width . 35))
  "The params which will be used by `display-buffer-in-side-window'.")

(defface lsp-treemacs-project-root-error
  '((t :inherit font-lock-keyword-face))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-project-root-info
  '((t :inherit font-lock-keyword-face))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-project-root-warn
  '((t :inherit font-lock-keyword-face))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-file-error
  '((t :inherit error))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)


(defface lsp-treemacs-file-info
  '((t :inherit success))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-file-warn
  '((t :inherit warning))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defcustom lsp-treemacs-face-map
  '((1 . lsp-treemacs-project-root-error)
    (2 .  lsp-treemacs-project-root-warn)
    (3 .  lsp-treemacs-project-root-info))
  "Alist diagnostics to face."
  :type 'alist)

(defcustom lsp-treemacs-file-face-map
  '((1 . lsp-treemacs-file-error)
    (2 . lsp-treemacs-file-warn)
    (3 . lsp-treemacs-file-info))
  "Alist diagnostics to face."
  :type 'alist)

(defcustom lsp-treemacs-error-list-severity 3
  "Severity level for `lsp-treemacs-error-list-mode'. 1 (highest) to 3 (lowest)"
  :type 'number)

(defcustom lsp-treemacs-theme "Default"
  "The `lsp-treemacs' theme."
  :type 'string)

(defun lsp-treemacs--match-diagnostic-severity (diagnostic)
  (<= (lsp-diagnostic-severity diagnostic)
      (prefix-numeric-value lsp-treemacs-error-list-severity)))

(defun lsp-treemacs--diagnostics-match-selected-severity (diagnostics)
  (-some #'lsp-treemacs--match-diagnostic-severity diagnostics))

(defun lsp-treemacs--root-folders ()
  "Get root folders containing errors."

  (let ((diagnostics (lsp-diagnostics)))
    (->> (lsp-session)
         lsp-session-folders
         (-filter (lambda (folder-name)
                    (-some (-lambda ((file-name . file-diagnostics))
                             (and
                              (s-starts-with? folder-name file-name)
                              (lsp-treemacs--diagnostics-match-selected-severity file-diagnostics)))
                           (ht->alist diagnostics))))
         (-map
          (lambda (root-folder)
            (cons
             (format (propertize "%s %s %s" 'face 'default)
                     (propertize (f-filename root-folder)
                                 'face (lsp-treemacs--face root-folder diagnostics))
                     (lsp-treemacs--diag-statistics (lsp-treemacs--project-diagnostics
                                                     root-folder
                                                     diagnostics))
                     (propertize (f-dirname root-folder)
                                 'face 'lsp-lens-face))
             root-folder))))))

(defun lsp-treemacs-quick-fix ()
  "Select the element under cursor."
  (interactive)
  (let ((key (button-get (treemacs-node-at-point) :data)))
    (if (and (consp key) (lsp-diagnostic-p (cdr key)))
        (-let (((file . diag) key)
               (session (lsp-session)))
          (with-current-buffer (find-file-noselect file)
            (with-lsp-workspaces (gethash
                                  (lsp-find-session-folder session file)
                                  (lsp-session-folder->servers session))
              (save-excursion
                (goto-char (point-min))
                (forward-line (lsp-diagnostic-line diag))
                (lsp-execute-code-action-by-kind "quickfix")))))
      (user-error "Not no a diagnostic"))))

(defun lsp-treemacs-cycle-severity ()
  "Cycle through the severity levels shown in the errors list"
  (interactive)
  (setq lsp-treemacs-error-list-severity
        (if (= lsp-treemacs-error-list-severity 1)
            3
          (1- lsp-treemacs-error-list-severity)))
  (lsp-treemacs--after-diagnostics))

(defun lsp-treemacs-open-file (&rest _)
  "Open file."
  (interactive)
  (let ((file (button-get (treemacs-node-at-point) :key)))
    (select-window (get-mru-window (selected-frame) nil :not-selected))
    (find-file file)))

(defun lsp-treemacs-open-error (&rest _)
  "Open error."
  (interactive)
  (-let [(file . diag) (button-get (treemacs-node-at-point) :data)]
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (lsp-diagnostic-line diag))
    (forward-char (lsp-diagnostic-column diag))))

(defun lsp-treemacs--face (root-folder diagnostics)
  "Calculate ROOT-FOLDER face based on DIAGNOSTICS."
  (--> diagnostics
       ht->alist
       (-keep (-lambda ((file-name . file-diagnostics))
                (when (s-starts-with? root-folder file-name)
                  (lsp-diagnostic-severity
                   (--min-by (> (lsp-diagnostic-severity it)
                                (lsp-diagnostic-severity other))
                             file-diagnostics))))
              it)
       -min
       (assoc it lsp-treemacs-face-map)
       cl-rest))

(defun lsp-treemacs--project-diagnostics (root-folder diagnostics)
  "Calculate ROOT-FOLDER face based on DIAGNOSTICS."
  (->> diagnostics
       (ht-map (lambda (file-name file-diags)
                 (when (and (s-starts-with? root-folder file-name)
                            (lsp-treemacs--diagnostics-match-selected-severity file-diags))
                   file-diags)))
       (apply #'append)))

(defun lsp-treemacs--diag-statistics (file-diagnostics)
  "Calculate FILE-DIAGNOSTICS statistics."
  (->> file-diagnostics
       (-filter #'lsp-treemacs--match-diagnostic-severity)
       (-group-by 'lsp-diagnostic-severity)
       (-sort (-lambda ((left) (right)) (< left right)))
       (-map (-lambda ((severity . diagnostics))
               (propertize (f-filename (number-to-string (length diagnostics)))
                           'face (cl-rest (assoc severity lsp-treemacs-file-face-map)))))
       (s-join "/")))

(defun lsp-treemacs--get-files (project-root)
  "Get files with errors in PROJECT-ROOT."
  (--> (lsp-diagnostics)
       ht->alist
       (-keep (-lambda ((file-name . file-diagnostics))
                (when (and (s-starts-with? project-root file-name)
                           (lsp-treemacs--diagnostics-match-selected-severity file-diagnostics))
                  (cons file-name
                        (format (propertize "%s %s %s" 'face 'default)
                                (propertize (f-filename file-name)
                                            'face 'default)
                                (lsp-treemacs--diag-statistics file-diagnostics)
                                (propertize (f-dirname (f-relative file-name project-root))
                                            'face 'lsp-lens-face)))))
              it)))

(defun lsp-treemacs--errors (file-name)
  "Get errors for FILE-NAME."
  (->> (lsp-diagnostics)
       (gethash file-name)
       (-filter #'lsp-treemacs--match-diagnostic-severity)
       (--sort (if (= (lsp-diagnostic-line it)
                      (lsp-diagnostic-line other))
                   (< (lsp-diagnostic-column it)
                      (lsp-diagnostic-column other))
                 (< (lsp-diagnostic-line it)
                    (lsp-diagnostic-line other))))
       (--map (cons file-name it))))

(defun lsp-treemacs--diagnostic-icon (diagnostic)
  "Get the icon for DIAGNOSTIC."
  (cl-case (lsp-diagnostic-severity diagnostic)
    (1 treemacs-icon-error)
    (2 treemacs-icon-warning)
    (t treemacs-icon-info)))

(treemacs-define-expandable-node lsp-error
  :icon-open-form (lsp-treemacs--diagnostic-icon (cl-rest (treemacs-button-get node :data)))
  :icon-closed-form (lsp-treemacs--diagnostic-icon (cl-rest (treemacs-button-get node :data)))
  :query-function (lsp-treemacs--errors (treemacs-button-get node :data))
  :ret-action 'lsp-treemacs-open-error
  :render-action
  (treemacs-render-node
   :icon (treemacs-as-icon ". " 'face 'font-lock-string-face)
   :label-form (propertize (lsp-diagnostic-message item) 'face 'default)
   :state treemacs-lsp-error-open-state
   :key-form item))

(treemacs-define-expandable-node lsp-files
  :icon-open-form (treemacs-icon-for-file (treemacs-button-get node :key))
  :icon-closed-form (treemacs-icon-for-file (treemacs-button-get node :key))
  :query-function (lsp-treemacs--errors (treemacs-button-get node :key))
  :ret-action 'lsp-treemacs-open-file
  :render-action
  (let* ((diag (cl-rest item))
         (label (format (propertize "%s %s %s" 'face 'default)
                        (propertize (format "[%s]" (lsp-diagnostic-source diag))
                                    'face 'shadow)
                        (lsp-diagnostic-message diag)
                        (propertize (format "(%s:%s)"
                                            (lsp-diagnostic-line diag)
                                            (lsp-diagnostic-column diag))
                                    'face 'lsp-lens-face))))
    (treemacs-render-node
     :icon (lsp-treemacs--diagnostic-icon (cl-rest item))
     :label-form label
     :state treemacs-lsp-error-open-state
     :key-form label
     :more-properties (:data item))))

(treemacs-define-expandable-node lsp-projects
  :icon-open (treemacs-get-icon-value 'root nil lsp-treemacs-theme)
  :icon-closed (treemacs-get-icon-value 'root nil lsp-treemacs-theme)
  :query-function (lsp-treemacs--get-files (treemacs-button-get node :key))
  :ret-action 'lsp-treemacs-open-file
  :render-action
  (treemacs-render-node
   :icon (treemacs-icon-for-file (cl-first item))
   :label-form (cl-rest item)
   :state treemacs-lsp-files-closed-state
   :key-form (cl-first item)))

(treemacs-define-variadic-node lsp-error-list
  :query-function (lsp-treemacs--root-folders)
  :render-action
  (treemacs-render-node
   :icon (treemacs-get-icon-value 'root nil lsp-treemacs-theme)
   :label-form (cl-first item)
   :state treemacs-lsp-projects-closed-state
   :key-form (cl-rest item))
  :root-key-form 'LSP-Errors)

(defun lsp-treemacs--after-diagnostics ()
  "After diagnostics handler."
  (save-excursion
    (condition-case _err
        (with-current-buffer (get-buffer-create "*LSP Error List*")
          (treemacs-update-node '(:custom LSP-Errors) t))
      (error))))

(defun lsp-treemacs--kill-buffer ()
  "Kill buffer hook."
  (remove-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs--after-diagnostics))

(defvar lsp-treemacs-error-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "x") #'lsp-treemacs-quick-fix)
    (define-key m (kbd "=") #'lsp-treemacs-cycle-severity)
    m)
  "Keymap for `lsp-treemacs-error-list-mode'.")

(define-minor-mode lsp-treemacs-error-list-mode ""
  nil nil nil
  :keymap lsp-treemacs-error-list-mode-map
  :group 'lsp-treeemacs)

;;;###autoload
(defun lsp-treemacs-errors-list ()
  "Display error list."
  (interactive)

  (-if-let (buffer (get-buffer "*LSP Error List*"))
      (progn
        (select-window (display-buffer-in-side-window buffer '((side . bottom))))
        (lsp-treemacs--after-diagnostics))
    (let* ((buffer (get-buffer-create "*LSP Error List*"))
           (window (display-buffer-in-side-window buffer '((side . bottom)))))
      (select-window window)
      (set-window-dedicated-p window t)
      (treemacs-initialize)
      (lsp-treemacs-error-list-mode 1)

      (setq-local treemacs-default-visit-action 'treemacs-RET-action)

      (treemacs-LSP-ERROR-LIST-extension)
      (setq-local mode-line-format (propertize "LSP Errors View" 'face 'shadow))

      (add-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs--after-diagnostics)
      (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-buffer nil t))))

(treemacs-modify-theme "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode")
  :config
  (progn
    (treemacs-create-icon :file "BooleanData.png" :extensions (boolean-data) :fallback "-")
    (treemacs-create-icon :file "Class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "ColorPalette.png" :extensions (color-palette) :fallback "-")
    (treemacs-create-icon :file "Constant.png" :extensions (constant) :fallback "-")
    (treemacs-create-icon :file "Document.png" :extensions (document) :fallback "-")
    (treemacs-create-icon :file "Enumerator.png" :extensions (enumerator) :fallback "-")
    (treemacs-create-icon :file "EnumItem.png" :extensions (enumitem) :fallback "-")
    (treemacs-create-icon :file "Event.png" :extensions (event) :fallback "-")
    (treemacs-create-icon :file "Field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "Indexer.png" :extensions (indexer) :fallback "-")
    (treemacs-create-icon :file "IntelliSenseKeyword.png" :extensions (intellisense-keyword) :fallback "-")
    (treemacs-create-icon :file "Interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "LocalVariable.png" :extensions (localvariable) :fallback "-")
    (treemacs-create-icon :file "Method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "Namespace.png" :extensions (namespace) :fallback "-")
    (treemacs-create-icon :file "Numeric.png" :extensions (numeric) :fallback "-")
    (treemacs-create-icon :file "Operator.png" :extensions (operator) :fallback "-")
    (treemacs-create-icon :file "Property.png" :extensions (property) :fallback "-")
    (treemacs-create-icon :file "Snippet.png" :extensions (snippet) :fallback "-")
    (treemacs-create-icon :file "String.png" :extensions (string) :fallback "-")
    (treemacs-create-icon :file "Structure.png" :extensions (structure) :fallback "-")
    (treemacs-create-icon :file "Template.png" :extensions (template) :fallback "-")
    (treemacs-create-icon :file "collapsed.png" :extensions (collapsed) :fallback "-")
    (treemacs-create-icon :file "expanded.png" :extensions (expanded) :fallback "-")
    (treemacs-create-icon :file "classfile.png" :extensions (classfile) :fallback "-")
    (treemacs-create-icon :file "default_folder_opened.png" :extensions (default-folder-opened) :fallback "-")
    (treemacs-create-icon :file "default_folder.png" :extensions (default-folder) :fallback "-")
    (treemacs-create-icon :file "default_root_folder_opened.png" :extensions (default-root-folder-opened) :fallback "-")
    (treemacs-create-icon :file "default_root_folder.png" :extensions (default-root-folder) :fallback "-")
    (treemacs-create-icon :file "file_type_class.png" :extensions ("class") :fallback "-")
    (treemacs-create-icon :file "file_type_jar.png" :extensions (file-type-jar) :fallback "-")
    (treemacs-create-icon :file "folder-open.png" :extensions (folder-open) :fallback "-")
    (treemacs-create-icon :file "folder.png" :extensions (folder) :fallback "-")
    (treemacs-create-icon :file "folder_type_component_opened.png" :extensions (folder-type-component-opened) :fallback "-")
    (treemacs-create-icon :file "folder_type_component.png" :extensions (folder-type-component) :fallback "-")
    (treemacs-create-icon :file "folder_type_library_opened.png" :extensions (folder-type-library-opened) :fallback "-")
    (treemacs-create-icon :file "folder_type_library.png" :extensions (folder-type-library) :fallback "-")
    (treemacs-create-icon :file "folder_type_maven_opened.png" :extensions (folder-type-maven-opened) :fallback "-")
    (treemacs-create-icon :file "folder_type_maven.png" :extensions (folder-type-maven) :fallback "-")
    (treemacs-create-icon :file "folder_type_package_opened.png" :extensions (folder-type-package-opened) :fallback "-")
    (treemacs-create-icon :file "folder_type_package.png" :extensions (folder-type-package) :fallback "-")
    (treemacs-create-icon :file "icon-create.png" :extensions (icon-create) :fallback "-")
    (treemacs-create-icon :file "icon-flat.png" :extensions (icon-flat) :fallback "-")
    (treemacs-create-icon :file "icon-hierarchical.png" :extensions (icon-hierarchical) :fallback "-")
    (treemacs-create-icon :file "icon-link.png" :extensions (icon-link) :fallback "-")
    (treemacs-create-icon :file "icon-refresh.png" :extensions (icon-refresh) :fallback "-")
    (treemacs-create-icon :file "icon-unlink.png" :extensions (icon-unlink) :fallback "-")
    (treemacs-create-icon :file "jar.png" :extensions (jar) :fallback "-")
    (treemacs-create-icon :file "library.png" :extensions (library) :fallback "-")
    (treemacs-create-icon :file "packagefolder-open.png" :extensions (packagefolder-open) :fallback "-")
    (treemacs-create-icon :file "packagefolder.png" :extensions (packagefolder) :fallback "-")
    (treemacs-create-icon :file "package.png" :extensions (package) :fallback "-")
    (treemacs-create-icon :file "project.png" :extensions (java-project) :fallback "-")))

(defun lsp-treemacs--symbol-icon (symbol expanded)
  "Get the symbol for the the kind."
  (-let [(&hash "kind" "children") symbol]
    (concat
     (if (seq-empty-p children)
         "   "
       (treemacs-get-icon-value
        (if expanded 'expanded 'collapsed)
        nil
        lsp-treemacs-theme))
     (treemacs-get-icon-value
      (cl-case kind
        (1 'document)
        (2  'namespace)
        (3  'namespace)
        (4  'namespace)
        (5  'class)
        (6  'method)
        (7  'property)
        (8  'field)
        (9  'method)
        (10 'enumerator)
        (11 'interface)
        (12 'method )
        (13 'localvariable)
        (14 'constant)
        (15 'string)
        (16 'numeric)
        (17 'boolean-data)
        (18  'boolean-data)
        (19 'namespace)
        (20 'indexer)
        (21 'boolean-data)
        (22 'enumitem)
        (23 'structure)
        (24 'event)
        (25 'operator)
        (26 'template))
      nil
      lsp-treemacs-theme))))

(treemacs-define-expandable-node lsp-symbol
  :icon-open-form (lsp-treemacs--symbol-icon (treemacs-button-get node :symbol) t)
  :icon-closed-form (lsp-treemacs--symbol-icon (treemacs-button-get node :symbol) nil)
  :query-function (append (gethash "children" (treemacs-button-get node :symbol)) nil)
  :ret-action 'lsp-treemacs-goto-symbol
  :render-action
  (treemacs-render-node
   :icon (lsp-treemacs--symbol-icon item nil)
   :label-form (propertize (gethash "name" item) 'face 'default)
   :state treemacs-lsp-symbol-closed-state
   :key-form (gethash "name" item)
   :more-properties (:symbol item)))

(defvar-local lsp-treemacs--symbols nil)
(defvar-local lsp-treemacs--symbols-tick nil)
(defvar-local lsp-treemacs--empty nil)
(defvar-local lsp-treemacs--symbols-state-string nil)
(defvar-local lsp-treemacs--symbols-state-locals nil)
(defvar lsp-treemacs--symbols-current-buffer nil)
(defvar lsp-treemacs--symbols-last-buffer nil)
(defvar lsp-treemacs--symbols-timer nil)

(treemacs-define-variadic-node lsp-symbols-list
  :query-function lsp-treemacs--symbols
  :render-action
  (treemacs-render-node
   :icon (lsp-treemacs--symbol-icon item nil)
   :label-form (propertize (gethash "name" item) 'face 'default)
   :state treemacs-lsp-symbol-closed-state
   :key-form (gethash "name" item)
   :more-properties (:symbol item))
  :root-key-form 'LSP-Symbols)

(defun lsp-treemacs--update-symbols ()
  "After diagnostics handler."
  (condition-case _err
      (let ((inhibit-read-only t))
        (with-current-buffer "*LSP Symbols List*"
          (treemacs-update-node '(:custom LSP-Symbols) t)))
    (error))

  (when (and lsp-treemacs--symbols (> 30 (length lsp-treemacs--symbols)))
    (lsp-treemacs--expand '(:custom LSP-Symbols)))
  (setq-local header-line-format
              (unless lsp-treemacs--symbols
                (propertize "The active buffer cannot provide symbols information."
                            'face 'shadow))))

(defun lsp-treemacs--update ()
  (unless (eq (current-buffer) (get-buffer "*scratch*"))
    (when (with-current-buffer "*LSP Symbols List*" (get-buffer-window))
      (if (lsp--find-workspaces-for "textDocument/documentSymbol")
          (when (or (not lsp-treemacs--symbols-tick)
                    (not (eq lsp-treemacs--symbols-tick (buffer-modified-tick)))
                    (not (eq (current-buffer) lsp-treemacs--symbols-current-buffer)))
            (lsp-request-async "textDocument/documentSymbol"
                               `(:textDocument ,(lsp--text-document-identifier))
                               (lambda (document-symbols)
                                 (save-excursion
                                   (with-current-buffer "*LSP Symbols List*"
                                     (setq-local lsp-treemacs--symbols document-symbols)
                                     (lsp-treemacs--update-symbols))))
                               :mode 'alive)
            (setq-local lsp-treemacs--symbols-tick (buffer-modified-tick))
            (setq lsp-treemacs--symbols-last-buffer (current-buffer)))
        (when (buffer-file-name)
          (with-current-buffer "*LSP Symbols List*"
            (setq-local lsp-treemacs--symbols nil)
            (lsp-treemacs--update-symbols)))))
    (let ((buffer-changed (and lsp-treemacs--symbols-current-buffer
                               (not (eq lsp-treemacs--symbols-current-buffer (current-buffer)))
                               (not (eq (current-buffer) (get-buffer "*LSP Symbols List*"))))))
      ;; (when buffer-changed
      ;;   (with-current-buffer lsp-treemacs--symbols-current-buffer
      ;;     (setq-local lsp-treemacs--symbols-state-string (with-current-buffer "*LSP Symbols List*"
      ;;                                                      (buffer-string)))
      ;;     (setq-local lsp-treemacs--symbols-state-locals (with-current-buffer "*LSP Symbols List*"
      ;;                                                      (buffer-local-variables)))))
      (setq lsp-treemacs--symbols-current-buffer (current-buffer))

      ;; (when (and lsp-treemacs--symbols-state-string buffer-changed)
      ;;   (with-current-buffer "*LSP Symbols List*"
      ;;     (let ((buffer-string lsp-treemacs--symbols-state-string)
      ;;           (locals lsp-treemacs--symbols-state-locals))
      ;;       (let ((inhibit-read-only t))
      ;;         (mapc (lambda (v)
      ;;                 (condition-case ()
      ;;                     (if (symbolp v)
      ;;                         (makunbound v)
      ;;                       (set (make-local-variable (car v)) (cdr v)))
      ;;                   (setting-constant nil)))
      ;;               locals)))))
      )))

(defun lsp-treemacs-goto-symbol (&rest _)
  "Goto the symbol at point."
  (interactive)
  (if-let ((symbol-data (-some-> (treemacs-node-at-point)
                                 (button-get :symbol))))
      (with-current-buffer lsp-treemacs--symbols-last-buffer
        (let ((p (lsp--position-to-point (or (-some->> symbol-data
                                                       (gethash "selectionRange")
                                                       (gethash "start"))
                                             (-some->> symbol-data
                                                       (gethash "location")
                                                       (gethash "range")
                                                       (gethash "start"))
                                             (error "Unable to go to location")))))
          (pop-to-buffer lsp-treemacs--symbols-last-buffer)
          (goto-char p)))
    (user-error "No symbol under point.")))

(with-eval-after-load 'winum
  (when (boundp 'winum-ignored-buffers)
    (add-to-list 'winum-ignored-buffers "*LSP Symbols List*")
    (add-to-list 'winum-ignored-buffers "*LSP Error List*")
    (add-to-list 'winum-ignored-buffers lsp-treemacs-deps-buffer-name)))

(defun lsp-treemacs--expand (root-key)
  (-when-let (root (treemacs-dom-node->position (treemacs-find-in-dom root-key)))
    (treemacs-save-position
     (lsp-treemacs--expand-recursively root))))

(defun lsp-treemacs--kill-symbols-buffer ()
  (and lsp-treemacs--symbols-timer (cancel-timer lsp-treemacs--symbols-timer)))

;;;###autoload
(defun lsp-treemacs-symbols ()
  "Show symbols view."
  (interactive)
  (let ((original-buffer (current-buffer))
        (position '((side . left) (slot . 2))))
    (if-let (buf (get-buffer "*LSP Symbols List*"))
        (select-window (display-buffer-in-side-window buf position))
      (let* ((buf (get-buffer-create "*LSP Symbols List*"))
             (window (display-buffer-in-side-window buf position)))
        (select-window window)
        (set-window-dedicated-p window t)
        (treemacs-initialize)
        (setq-local treemacs-default-visit-action 'treemacs-RET-action)
        (treemacs-LSP-SYMBOLS-LIST-extension)
        (setq lsp-treemacs--symbols-timer (run-at-time 0 1.0 #'lsp-treemacs--update))
        (setq-local mode-line-format (propertize "LSP Symbols View" 'face 'shadow))
        (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-symbols-buffer nil t)))
    (with-current-buffer original-buffer (lsp-treemacs--update))))

(defun lsp-treemacs--expand-recursively (root)
  (-map
   (lambda (btn)
     (unless (treemacs-is-node-expanded? btn)
       (save-excursion
         (goto-char (marker-position btn))
         (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config))))
     (lsp-treemacs--expand-recursively btn))
   (treemacs--get-children-of root)))


(defmacro lsp-treemacs-deps-with-jdtls (&rest body)
  "Helper macro for invoking BODY against WORKSPACE context."
  (declare (debug (form body))
           (indent 0))
  `(if-let (lsp--cur-workspace (lsp-find-workspace 'jdtls nil))
       (progn ,@body)
     (user-error "Java Language Server is not started.")))

(defun lsp-treemacs-deps--goto-element (&rest _args)
  (if-let ((dep (-some-> (treemacs-node-at-point)
                         (button-get :dep))))
      (--doto (find-file-noselect
               (or (-some-> (gethash "uri" dep)
                            (lsp--uri-to-path))
                   (when (f-exists? (gethash "path" dep))
                     (gethash "path" dep))
                   (concat (f-parent (lsp--uri-to-path (gethash "projectUri" dep)))
                           (gethash "path" dep))))
        (select-window (get-mru-window nil nil t))
        (switch-to-buffer it))
    (user-error "No element under point.")))

(defun lsp-treemacs-deps--icon (dep expanded)
  "Get the symbol for the the kind."
  (-let (((&hash "uri" "name" "kind" "entryKind" entry-kind) dep))
    (concat
     (treemacs-get-icon-value
      (if expanded 'expanded 'collapsed)
      nil
      lsp-treemacs-theme)
     (if (or (= kind 8)
             (= kind 6))
         (treemacs-icon-for-file uri)
       (treemacs-get-icon-value
        (cond
         ((eq entry-kind 2) 'jar)
         ((eq kind 5) 'package)
         ((eq kind 7) 'folder)
         ((eq kind 4) 'packagefolder)
         ((eq kind 2) 'java-project)
         ((eq entry-kind 3) 'packagefolder)
         ((eq entry-kind 5) 'library))
        nil
        lsp-treemacs-theme)))))

(defun lsp-treemacs-deps--get-children (dep)
  (lsp-treemacs-deps-with-jdtls
    (-let (((&hash "projectUri" project-uri "rootPath" root-path "path" "kind" "name") dep))
      (unless (or (= kind 6)
                  (= kind 8))
        (->> (lsp-send-execute-command
              "java.getPackageData"
              (vector (ht ("kind"  kind)
                          ("path"  (unless (eq kind 2)
                                     (if (= 5 kind)
                                         name
                                       path)))
                          ("rootPath" (unless (eq kind 2)
                                        (or root-path path)))
                          ("projectUri"  project-uri))))
             (-mapcat (lambda (inner-dep)
                        (puthash "projectUri" project-uri inner-dep)
                        (when (= kind 4)
                          (puthash "rootPath" path inner-dep))
                        (if (eq (gethash "entryKind" inner-dep) 3)
                            (lsp-treemacs-deps--get-children inner-dep)
                          (list inner-dep)))))))))

(defun lsp-treemacs-deps--java-file? (dep)
  (-let [(&hash "kind" "entryKind" entry-kind) dep]
    (and (eq kind 6)
         (or (eq entry-kind 1)
             (eq entry-kind 2)))))

(treemacs-define-expandable-node lsp-treemacs-deps
  :icon-open-form (lsp-treemacs-deps--icon (treemacs-button-get node :dep) t)
  :icon-closed-form (lsp-treemacs-deps--icon (treemacs-button-get node :dep) nil)
  :query-function (-let (((dep &as &hash "uri") (treemacs-button-get node :dep)))
                    (if (lsp-treemacs-deps--java-file? dep)
                        (lsp-treemacs-deps-with-jdtls
                          (lsp-request "textDocument/documentSymbol"
                                       `(:textDocument (:uri ,uri))))
                      (lsp-treemacs-deps--get-children dep)))
  :ret-action 'lsp-treemacs-deps--goto-element
  :render-action (if (lsp-treemacs-deps--java-file? (treemacs-button-get node :dep))
                     (treemacs-render-node
                      :icon (lsp-treemacs--symbol-icon item nil)
                      :label-form (propertize (gethash "name" item) 'face 'default)
                      :state treemacs-lsp-symbol-closed-state
                      :key-form (list (gethash "name" item)
                                      (gethash "uri" item)
                                      (gethash "path" item))
                      :more-properties (:symbol item))
                   (treemacs-render-node
                    :icon (lsp-treemacs-deps--icon item nil)
                    :label-form (propertize (gethash "name" item) 'face 'default)
                    :state treemacs-lsp-treemacs-deps-closed-state
                    :key-form (list (gethash "name" item)
                                    (gethash "uri" item)
                                    (gethash "path" item))
                    :more-properties (:dep item))))

(defun lsp-treemacs-deps--root-folders ()
  (lsp-treemacs-deps-with-jdtls
    (-mapcat (lambda (root-path)
               (let ((project-uri (lsp--path-to-uri root-path)))
                 (->> project-uri
                      (lsp-send-execute-command "java.project.list")
                      (--map (--doto it (puthash "projectUri" project-uri it))))))
             (lsp-session-folders (lsp-session)))))

(treemacs-define-variadic-node lsp-treemacs-deps-list
  :query-function (lsp-treemacs-deps--root-folders)
  :render-action
  (treemacs-render-node
   :icon (lsp-treemacs-deps--icon item nil)
   :label-form (propertize (gethash "name" item) 'face 'default)
   :state treemacs-lsp-treemacs-deps-closed-state
   :key-form (list (gethash "name" item)
                   (gethash "uri" item)
                   (gethash "path" item))
   :more-properties (:dep item))
  :root-key-form 'LSP-Java-Dependency)

(defun lsp-treemacs-java-deps-refresh ()
  "Refresh dependecy list."
  (interactive)
  (condition-case _err
      (let ((inhibit-read-only t))
        (with-current-buffer lsp-treemacs-deps-buffer-name
          (treemacs-update-node '(:custom LSP-Java-Dependency) t)
          (lsp--info "Refresh completed")))
    (error)))

;;;###autoload
(defun lsp-treemacs-java-deps-list ()
  "Display error list."
  (interactive)
  (-if-let (buffer (get-buffer lsp-treemacs-deps-buffer-name))
      (select-window
       (or (get-buffer-window lsp-treemacs-deps-buffer-name)
           (display-buffer-in-side-window buffer lsp-treemacs-deps-position-params)))
    (let* ((buffer (get-buffer-create lsp-treemacs-deps-buffer-name ))
           (window (display-buffer-in-side-window buffer lsp-treemacs-deps-position-params)))
      (select-window window)
      (set-window-dedicated-p window t)
      (treemacs-initialize)
      (lsp-treemacs-deps-list-mode t)
      (setq-local treemacs-default-visit-action 'treemacs-RET-action)
      (setq-local mode-line-format (propertize "Java Dependencies" 'face 'shadow))
      (treemacs-LSP-TREEMACS-DEPS-LIST-extension))))

(defun lsp-treemacs--deps-find-children-for-key (node key)
  (->> node
       treemacs--get-children-of
       (-first (lambda (child)
                 (goto-char (marker-position child))
                 (equal (treemacs-button-get child :key) key)))))

(defvar lsp-treemacs-deps-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") #'lsp-treemacs-java-deps-refresh)
    m)
  "Keymap for `lsp-treemacs-deps-list-mode'.")

(define-minor-mode lsp-treemacs-deps-list-mode ""
  nil nil nil
  :keymap lsp-treemacs-deps-list-mode-map
  :group 'lsp-treeemacs)

;;;###autoload
(defun lsp-treemacs-java-deps-follow ()
  (interactive)
  (lsp-treemacs-deps-with-jdtls
    (let ((paths (lsp-send-execute-command "java.resolvePath"
                                           (lsp--buffer-uri))))
      (select-window
       (with-current-buffer lsp-treemacs-deps-buffer-name
         (set-window-point
          (get-buffer-window)
          (marker-position
           (-reduce-from
            (-lambda (node (&hash "path" "name" "uri"))
              (unless (treemacs-is-node-expanded? node)
                (save-excursion
                  (goto-char (marker-position node))
                  (funcall (alist-get (treemacs-button-get node :state) treemacs-TAB-actions-config))))
              (or (lsp-treemacs--deps-find-children-for-key node (list name uri path))
                  (user-error "Unable to find %s in the dependency tree." (buffer-name))))
            (treemacs-dom-node->position (treemacs-find-in-dom '(:custom LSP-Java-Dependency)))
            paths)))
         (get-buffer-window)))
      (recenter nil))))

(provide 'lsp-treemacs)
;;; lsp-treemacs.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

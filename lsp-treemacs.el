;;; lsp-treemacs.el --- LSP treemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 emacs-lsp maintainers

;; Author: Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (f "0.20.0") (ht "2.0") (treemacs "2.5") (lsp-mode "6.0"))
;; Homepage: https://github.com/emacs-lsp/lsp-treemacs
;; Version: 0.5

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
(require 'treemacs-icons)
(require 'treemacs-treelib)
(require 'lsp-treemacs-generic)

(require 'lsp-treemacs-themes)
(require 'lsp-mode)

(defconst lsp-treemacs-deps-buffer-name "*Java Dependency List*")
(defconst lsp-treemacs-symbols-buffer-name "*LSP Symbols List*")
(defconst lsp-treemacs-errors-buffer-name "*LSP Error List*")

(defgroup lsp-treemacs nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server")

(defvar lsp-treemacs-deps-position-params
  `((side . ,treemacs-position)
    (slot . 1)
    (window-width . ,treemacs-width))
  "The params which will be used by
  `display-buffer-in-side-window' in
  `lsp-treemacs-java-deps-list'.")

(defvar lsp-treemacs-symbols-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The params which will be used by
  `display-buffer-in-side-window' in `lsp-treemacs-symbols'.")

(defvar lsp-treemacs-errors-position-params
  `((side . bottom))
  "The params which will be used by
  `display-buffer-in-side-window' in
  `lsp-treemacs-errors-list'.")

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

(defface lsp-treemacs-file-hint
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
    (3 . lsp-treemacs-file-info)
    (4 . lsp-treemacs-file-hint))
  "Alist diagnostics to face."
  :type 'alist)

(defcustom lsp-treemacs-error-list-severity 3
  "Severity level for `lsp-treemacs-error-list-mode'. 1 (highest) to 3 (lowest)"
  :type 'number)

(defcustom lsp-treemacs-error-list-current-project-only nil
  "List the error list of the current project only if available.
Fallback to list all workspaces if no project root is found."
  :type 'boolean
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-error-list-expand-depth nil
  "Automatic expansion depth for `lsp-treemacs-error-list'."
  :type 'number
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-call-hierarchy-expand-depth nil
  "Automatic expansion depth for `lsp-treemacs-call-hierarchy'."
  :type 'number
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-type-hierarchy-expand-depth nil
  "Automatic expansion depth for `lsp-treemacs-type-hierarchy'."
  :type 'number
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-java-deps-list-expand-depth nil
  "Automatic expansion depth for `lsp-treemacs-java-deps-list'."
  :type 'number
  :group 'lsp-treemacs)

(defun lsp-treemacs--open-file-in-mru (file)
  (select-window (get-mru-window (selected-frame) nil :not-selected))
  (find-file file))

(defun lsp-treemacs-symbol-kind->icon (kind)
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
    (18 'boolean-data)
    (19 'namespace)
    (20 'indexer)
    (21 'boolean-data)
    (22 'enumitem)
    (23 'structure)
    (24 'event)
    (25 'operator)
    (26 'template)
    (t 'misc)))

(defun lsp-treemacs-get-icon (icon-name)
  "Get the treemacs ICON using current theme."
  (treemacs-get-icon-value icon-name nil lsp-treemacs-theme))

(defun lsp-treemacs-symbol-icon (kind)
  "Get icon for `kind'."
  (treemacs-get-icon-value (lsp-treemacs-symbol-kind->icon kind)
                           nil
                           lsp-treemacs-theme))

(defun lsp-treemacs--symbol-icon (symbol expanded)
  "Get the symbol for the the kind."
  (-let [(&DocumentSymbol :kind :children?) symbol]
    (concat
     (if (seq-empty-p children?)
         "  "
       (if expanded  "▾ " "▸ "))
     (lsp-treemacs-symbol-icon kind))))

(defvar-local lsp-treemacs--symbols nil)
(defvar-local lsp-treemacs--symbols-tick nil)
(defvar-local lsp-treemacs--empty nil)
(defvar-local lsp-treemacs--symbols-state-string nil)
(defvar-local lsp-treemacs--symbols-state-locals nil)
(defvar lsp-treemacs--symbols-current-buffer nil)
(defvar lsp-treemacs--symbols-last-buffer nil)
(defvar lsp-treemacs--symbols-timer nil)

(defun lsp-treemacs-sort-by-position (left right)
  (-let (((&plist :position left-position) left)
         ((&plist :position right-position) right))
    (and left-position right-position (lsp--position-compare left-position right-position))))

(defun lsp-treemacs-sort-by-kind (left right)
  (-let (((&plist :kind left-kind) left)
         ((&plist :kind right-kind) right))
    (and left-kind right-kind (> left-kind right-kind))))

(defun lsp-treemacs-sort-by-name (left right)
  (-let* (((&plist :label left-name) left)
          ((&plist :label right-name) right))
    (string> (downcase right-name) (downcase left-name))))

(defcustom lsp-treemacs-symbols-sort-functions '(lsp-treemacs-sort-by-position)
  "Sort functions."
  :type '(repeat
          (choice
           (const :tag "Name" lsp-treemacs-sort-by-name)
           (const :tag "Kind"  lsp-treemacs-sort-by-kind)
           (const :tag "Position" lsp-treemacs-sort-by-position))))

(defcustom lsp-treemacs-detailed-outline t
  "Whether `lsp-treemacs-symbols' should include signatures.
For this to work, the language server must support
DocumentSymbols."
  :group 'lsp-treemacs
  :type 'boolean)

(defcustom lsp-treemacs-after-jump-hook
  (list (lambda () (run-hooks 'xref-after-jump-hook)))
  "List of functions to call after jumping to a symbol.
When pressing RET on a symbol in the `lsp-treemacs-symbols' view,
this hook will be run after having jumped to the target."
  :group 'lsp-treemacs
  :type '(list function))

(defmacro lsp-treemacs-define-action (name keys &rest body)
  (declare (doc-string 3) (indent 2) (debug (&define name sexp lambda-doc def-body)))
  (let* ((docstring (car body)))
    (when (stringp docstring)
      (pop body))
    `(defun ,name (&rest args)
       ,(if (stringp docstring) docstring (format "Code action %s." name))
       (interactive)
       (ignore args)
       (-if-let (node (treemacs-node-at-point))
           (-let [,(cons '&plist keys) (button-get node :item)]
             ,@body)
         (treemacs-pulse-on-failure "No node at point")))))

(lsp-treemacs-define-action lsp-treemacs-symbols-goto-symbol (:location)
  "Goto the symbol node at `point'."
  (pop-to-buffer lsp-treemacs--symbols-last-buffer)
  (goto-char (lsp--position-to-point location))
  (run-hooks 'lsp-treemacs-after-jump-hook))

(lsp-treemacs-define-action lsp-treemacs-go-to (:uri :position)
  "Goto POSITION in URL."
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point position))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-treemacs--symbols->tree (items parent-key)
  "Convert ITEMS and PARENT-KEY to a treemacs tree."
  (-sort (lambda (left right)
           (-first (lambda (fn)
                     (funcall fn left right))
                   lsp-treemacs-symbols-sort-functions))
         (if (-some->> items lsp-seq-first lsp-symbol-information?)
             (-let [(current rest) (-separate (-lambda ((&SymbolInformation :container-name?))
                                                (string= container-name? parent-key))
                                              (append items nil))]
               (seq-map (-lambda ((&SymbolInformation :name :container-name? :kind
                                                      :location (location &as &Location :range (&Range :start start-range))))
                          (when (string= parent-key container-name?)
                            `(:label ,name
                                     :key ,name
                                     :icon ,(lsp-treemacs-symbol-kind->icon kind)
                                     ,@(when (-first (-lambda ((&SymbolInformation :container-name? parent))
                                                       (string= name parent))
                                                     rest)
                                         (list :children (lsp-treemacs--symbols->tree rest name)))
                                     :kind ,kind
                                     :location ,start-range
                                     :ret-action lsp-treemacs-symbols-goto-symbol)))
                        current))
           (seq-map
            (-lambda ((sym &as &DocumentSymbol :name :kind :selection-range
                           (&Range :start start-range) :children?))
              `(:label ,(lsp-render-symbol sym lsp-treemacs-detailed-outline)
                       :key ,name
                       :icon ,(lsp-treemacs-symbol-kind->icon kind)
                       :kind ,kind
                       :location ,start-range
                       ,@(unless (seq-empty-p children?)
                           (list :children (lsp-treemacs--symbols->tree children? name)))
                       :ret-action lsp-treemacs-symbols-goto-symbol))
            items))))

(defun lsp-treemacs--update-symbols ()
  "After diagnostics handler."
  (setq-local header-line-format
              (unless lsp-treemacs--symbols
                (propertize "No symbol information." 'face 'shadow)))
  (lsp-treemacs-render
   (lsp-treemacs--symbols->tree
    lsp-treemacs--symbols
    nil)
   " LSP Symbols "
   (and lsp-treemacs--symbols (> 30 (length lsp-treemacs--symbols)))
   lsp-treemacs-symbols-buffer-name ))

(defun lsp-treemacs--update ()
  (unless (eq (current-buffer) (get-buffer "*scratch*"))
    (when (with-current-buffer lsp-treemacs-symbols-buffer-name (get-buffer-window))
      (if (lsp--find-workspaces-for "textDocument/documentSymbol")
          (when (or (not lsp-treemacs--symbols-tick)
                    (not (eq lsp-treemacs--symbols-tick (buffer-modified-tick)))
                    (not (eq (current-buffer) lsp-treemacs--symbols-current-buffer)))
            (lsp-request-async "textDocument/documentSymbol"
                               (lsp-make-document-symbol-params :text-document (lsp--text-document-identifier))
                               (lambda (document-symbols)
                                 (save-excursion
                                   (with-current-buffer lsp-treemacs-symbols-buffer-name
                                     (setq-local lsp-treemacs--symbols document-symbols)
                                     (lsp-treemacs--update-symbols))))
                               :mode 'alive)
            (setq-local lsp-treemacs--symbols-tick (buffer-modified-tick))
            (setq lsp-treemacs--symbols-last-buffer (current-buffer)))
        (when (buffer-file-name)
          (with-current-buffer lsp-treemacs-symbols-buffer-name
            (setq-local lsp-treemacs--symbols nil)
            (lsp-treemacs--update-symbols)))))
    (setq lsp-treemacs--symbols-current-buffer (current-buffer))))

(defun lsp-treemacs-goto-symbol (&rest _)
  "Goto the symbol at point."
  (interactive)
  (if-let ((symbol-data (-some-> (treemacs-node-at-point)
                          (button-get :symbol))))
      (with-current-buffer lsp-treemacs--symbols-last-buffer
        (let ((p (lsp--position-to-point (or (-some->> symbol-data
                                               lsp:document-symbol-selection-range
                                               lsp:range-start)
                                             (-some->> symbol-data
                                               lsp:symbol-information-location
                                               lsp:location-range
                                               lsp:range-start)
                                             (error "Unable to go to location")))))
          (pop-to-buffer lsp-treemacs--symbols-last-buffer)
          (goto-char p)
          (run-hooks 'xref-after-jump-hook)))
    (user-error "No symbol under point.")))

(with-eval-after-load 'winum
  (when (boundp 'winum-ignored-buffers)
    (add-to-list 'winum-ignored-buffers lsp-treemacs-symbols-buffer-name)
    (add-to-list 'winum-ignored-buffers lsp-treemacs-errors-buffer-name)
    (add-to-list 'winum-ignored-buffers  lsp-treemacs-deps-buffer-name)))

(defun lsp-treemacs--expand (root-key depth)
  (-when-let (root (treemacs-dom-node->position (treemacs-find-in-dom root-key)))
    (treemacs-save-position
     (lsp-treemacs--expand-recursively root depth))))

(defun lsp-treemacs--kill-symbols-buffer ()
  (and lsp-treemacs--symbols-timer (cancel-timer lsp-treemacs--symbols-timer)))

(defcustom lsp-treemacs-symbols-space-between-root-nodes nil
  "Whether there should be empty lines between symbols.
If this is set to t, top-level symbols in `lsp-treemacs-symbols'
will be rendered an empty line between them."
  :group 'lsp-treemacs
  :type 'boolean)

;;;###autoload
(defun lsp-treemacs-symbols ()
  "Show symbols view."
  (interactive)
  (let ((original-buffer (current-buffer)))
    (if-let (buf (get-buffer lsp-treemacs-symbols-buffer-name))
        (select-window (display-buffer-in-side-window buf lsp-treemacs-symbols-position-params))
      (let* ((buf (get-buffer-create lsp-treemacs-symbols-buffer-name))
             (window (display-buffer-in-side-window buf lsp-treemacs-symbols-position-params)))
        (select-window window)
        (set-window-dedicated-p window t)

        (unless lsp-treemacs--symbols-timer
          (setq lsp-treemacs--symbols-timer (run-with-idle-timer 1 t #'lsp-treemacs--update)))
        (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-symbols-buffer nil t)))
    (with-current-buffer original-buffer (lsp-treemacs--update))))

(defun lsp-treemacs--expand-recursively (root depth)
  (when (if (booleanp depth) depth (not (zerop depth)))
    (save-excursion
      (-map
       (lambda (btn)
         (unless (treemacs-is-node-expanded? btn)
           (goto-char (marker-position btn))
           (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config)))
         (lsp-treemacs--expand-recursively btn (if (booleanp depth) depth (1- depth))))
       (treemacs-collect-child-nodes root)))))


;; deps
(eval-and-compile
  (lsp-interface
   (java:Node (:projectUri :rootPath :path :kind :name :uri :entryKind))))

(defmacro lsp-treemacs-deps-with-jdtls (&rest body)
  "Helper macro for invoking BODY against WORKSPACE context."
  (declare (debug (form body))
           (indent 0))
  `(if-let (lsp--cur-workspace (lsp-find-workspace 'jdtls nil))
       (progn ,@body)
     (user-error "Java Language Server is not started.")))

(defun lsp-treemacs-deps--icon (dep)
  "Get the symbol for the the kind."
  (-let (((&java:Node :uri :kind :entry-kind) dep))
    (if (or (= kind 8)
            (= kind 6))
        (f-ext uri)
      (cond
       ((eq entry-kind 2) 'jar)
       ((eq kind 5) 'package)
       ((eq kind 7) 'folder)
       ((eq kind 4) 'packagefolder)
       ((eq kind 2) 'java-project)
       ((eq entry-kind 3) 'packagefolder)
       ((eq entry-kind 5) 'library)))))

(defun lsp-treemacs-deps--get-children (dep)
  (lsp-treemacs-deps-with-jdtls
    (-let* (((&java:Node :project-uri :root-path :path :kind :name :uri) dep)
            (project-uri (if (eq kind 2) uri project-uri)))
      (unless (or (= kind 6)
                  (= kind 8))
        (->> (lsp-send-execute-command
              "java.getPackageData"
              (vector (ht ("kind" kind)
                          ("path" (unless (eq kind 2)
                                    (if (= 5 kind)
                                        name
                                      path)))
                          ("rootPath" (unless (eq kind 2)
                                        (or root-path path)))
                          ("projectUri" project-uri))))
             (-mapcat (lambda (inner-dep)
                        (lsp-put inner-dep :projectUri project-uri)
                        (when (= kind 4)
                          (lsp-put inner-dep :rootPath path))
                        (if (eq (lsp-get inner-dep :entryKind) 3)
                            (lsp-treemacs-deps--get-children inner-dep)
                          (list inner-dep)))))))))

(defun lsp-treemacs-deps--java-file? (dep)
  (-let [(&java:Node :kind :entry-kind) dep]
    (and (eq kind 6)
         (or (eq entry-kind 1)
             (eq entry-kind 2)))))

(defun lsp-treemacs-deps--root-folders ()
  (lsp-treemacs-deps-with-jdtls
    (-mapcat (lambda (root-path)
               (let ((project-uri (lsp--path-to-uri root-path)))
                 (->> project-uri
                      (lsp-send-execute-command "java.project.list")
                      (--map (--doto it (lsp-put it :projectUri project-uri))))))
             (lsp-session-folders (lsp-session)))))

(lsp-defun lsp-treemacs-deps--process-dep ((item &as &java:Node :name :uri :path :project-uri))
  `(:label ,name
           :icon ,(lsp-treemacs-deps--icon item)
           :key ,(list name uri path)
           :ret-action ,(lambda (&rest _)
                          (lsp-treemacs--open-file-in-mru
                           (or (when uri (lsp--uri-to-path uri))
                               (when (f-exists? path) path)
                               (concat (f-parent (lsp--uri-to-path project-uri))
                                       path))))
           ,@(unless (lsp-treemacs-deps--java-file? item)
               (list :children
                     (lambda (_)
                       (-map #'lsp-treemacs-deps--process-dep
                             (lsp-treemacs-deps--get-children item)))))))
;;;###autoload
(defun lsp-treemacs-java-deps-list ()
  "Display java dependencies."
  (interactive)
  (select-window
   (display-buffer-in-side-window
    (lsp-treemacs-render
     (-map 'lsp-treemacs-deps--process-dep (lsp-treemacs-deps--root-folders))
     "*Java Deps*" lsp-treemacs-java-deps-list-expand-depth)
    lsp-treemacs-deps-position-params)))

(defun lsp-treemacs--deps-find-children-for-key (node key)
  (->> node
       treemacs-collect-child-nodes
       (-first (lambda (child)
                 (goto-char (marker-position child))
                 (equal (treemacs-button-get child :key) key)))))

(defvar lsp-treemacs-deps-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") #'lsp-treemacs-generic-refresh)
    m)
  "Keymap for `lsp-treemacs-deps-list-mode'.")

(define-minor-mode lsp-treemacs-deps-list-mode "LSP Treemacs mode for listing dependencies."
  :keymap lsp-treemacs-deps-list-mode-map
  :group 'lsp-treemacs)

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
            (-lambda (node (&java:Node :path :name :uri))
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


;; treemacs synchronization

(defun lsp-treemacs--on-folder-remove (project)
  (lsp-workspace-folders-remove (treemacs-project->path project)))

(defun lsp-treemacs--on-folder-added (project)
  (lsp-workspace-folders-add (treemacs-project->path project)))

(defun lsp-treemacs--treemacs->lsp ()
  (let ((lsp-folders (lsp-session-folders (lsp-session)))
        (treemacs-folders (->> (treemacs-current-workspace)
                               (treemacs-workspace->projects)
                               (-map #'treemacs-project->path)
                               (-map #'lsp-canonical-file-name))))
    (seq-do #'lsp-workspace-folders-remove (-difference lsp-folders treemacs-folders))
    (seq-do #'lsp-workspace-folders-add (-difference treemacs-folders lsp-folders))))

(defun lsp-treemacs--sync-folders (added removed)
  (when-let (treemacs-workspace (treemacs-current-workspace))
    (let ((treemacs-create-project-functions (remove #'lsp-treemacs--on-folder-added
                                                     treemacs-create-project-functions))
          (treemacs-delete-project-functions (remove #'lsp-treemacs--on-folder-remove
                                                     treemacs-delete-project-functions))
          (added (seq-map #'treemacs--canonical-path added))
          (removed (seq-map #'treemacs--canonical-path removed)))
      (dolist (added-path added)
        (unless (treemacs-is-path added-path :in-workspace treemacs-workspace)
          (let* ((name (file-name-nondirectory (directory-file-name added-path)))
                 (result (treemacs-do-add-project-to-workspace added-path name)))
            (unless (eq 'success result)
              (lsp-log "Failed to add path '%s' to treemacs' workspace: %s" added-path result)))))
      (dolist (removed-path removed)
        (when (treemacs-is-path removed-path :in-workspace treemacs-workspace)
          (let ((result (treemacs-do-remove-project-from-workspace removed-path)))
            (unless (eq 'success result)
              (lsp-log "Failed to remove path '%s' from treemacs' workspace: %s" removed-path result))))))))

;;;###autoload
(define-minor-mode lsp-treemacs-sync-mode
  "Global minor mode for synchronizing lsp-mode workspace folders and
treemacs projects."
  :init-value nil
  :group 'lsp-treemacs
  :global t
  (cond
   (lsp-treemacs-sync-mode
    (add-hook 'treemacs-create-project-functions #'lsp-treemacs--on-folder-added)
    (add-hook 'treemacs-delete-project-functions #'lsp-treemacs--on-folder-remove)
    (add-hook 'lsp-workspace-folders-changed-functions #'lsp-treemacs--sync-folders)
    (add-hook 'treemacs-workspace-edit-hook #'lsp-treemacs--treemacs->lsp)
    (add-hook 'treemacs-switch-workspace-hook #'lsp-treemacs--treemacs->lsp))
   (t
    (remove-hook 'treemacs-create-project-functions #'lsp-treemacs--on-folder-added)
    (remove-hook 'treemacs-delete-project-functions #'lsp-treemacs--on-folder-remove)
    (remove-hook 'lsp-workspace-folders-changed-functions #'lsp-treemacs--sync-folders)
    (remove-hook 'treemacs-workspace-edit-hook #'lsp-treemacs--treemacs->lsp)
    (remove-hook 'treemacs-switch-workspace-hook #'lsp-treemacs--treemacs->lsp))))



(defun lsp-treemacs--java-get-class-file (file)
  (-let (((_ _package _class jar-file) (s-match "jdt://contents/.*\/\\(.*\\)\/\\(.*\\).class\\?=.*?/\\(.*?\\)=\/" file)))
    (symbol-name (read (url-unhex-string jar-file )))))

(defun lsp-treemacs--get-xrefs-in-file (file-locs location-link)
  (-let (((filename . links) file-locs))
    (list :key filename
          :label (format (propertize "%s %s" 'face 'default)
                         (propertize (f-filename filename) 'face 'default)
                         (propertize (format "%s references" (length links)) 'face 'lsp-details-face))
          :icon (f-ext filename)
          :children (lambda (_item)
                      (condition-case err
                          (let ((buf (find-buffer-visiting filename))
                                (fn (lambda ()
                                      (seq-map (lambda (loc)
                                                 (lsp-treemacs--make-ref-item
                                                  (if location-link
                                                      (or (lsp:location-link-target-selection-range loc)
                                                          (lsp:location-link-target-range loc))
                                                    (lsp:location-range loc))
                                                  filename))
                                               links))))
                            (if buf
                                (with-current-buffer buf
                                  (funcall fn))
                              (when (file-readable-p filename)
                                (with-temp-buffer
                                  (insert-file-contents-literally filename)
                                  (funcall fn)))))
                        (error (ignore (lsp-warn
                                        "Failed to process xref entry for filename '%s': %s"
                                        filename
                                        (error-message-string err))))
                        (file-error (ignore (lsp-warn
                                             "Failed to process xref entry, file-error, '%s': %s"
                                             filename
                                             (error-message-string err))))))
          :ret-action (lambda (&rest _)
                        (interactive)
                        (lsp-treemacs--open-file-in-mru filename)))))

(defun lsp-treemacs--extract-line (point)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring (line-beginning-position)
                        (line-end-position)))))

(lsp-defun lsp-treemacs--make-ref-item ((&Range :start (start &as &Position :line start-line :character start-character)
                                                :end (&Position :character end-character))
                                        filename)
  "Return a xref-item from a RANGE in FILENAME."
  (-let* ((start-point (lsp--position-to-point start))
          (line (lsp-treemacs--extract-line start-point))
          (len (length line)))
    (add-face-text-property (max (min start-character len) 0)
                            (max (min end-character len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (list :label (s-trim (format "%s %s"
                                 line
                                 (propertize(format "%s line"
                                                    (1+ start-line))
                                            'face 'lsp-details-face)))
          :key line
          :point start-point
          :icon-literal ""
          :ret-action (lambda (&rest _)
                        (interactive)
                        (lsp-treemacs--open-file-in-mru filename)
                        (goto-char start-point)
                        (run-hooks 'xref-after-jump-hook)))))

(defun lsp-treemacs--handle-references (refs)
  (->> refs
       (-group-by (-lambda ((&Location :uri))
                    (let ((type (url-type (url-generic-parse-url (url-unhex-string uri)))))
                      (if (string= type "jdt")
                          (lsp-treemacs--java-get-class-file uri)
                        (lsp-workspace-root (lsp--uri-to-path uri))))))
       (-map (-lambda ((path . rst))
               (list :key path
                     :label (format
                             "%s %s"
                             (f-filename path)
                             (propertize (format "%s references" (length rst)) 'face 'lsp-details-face))
                     :icon (if (f-file? path)
                               (f-ext path)
                             'dir-open)
                     :children (lambda (_item)
                                 (-map (lambda (it)
                                         (lsp-treemacs--get-xrefs-in-file it nil))
                                       (-group-by (-lambda ((&Location :uri))
                                                    (lsp--uri-to-path uri))
                                                  rst)))
                     :ret-action (lambda (&rest _)
                                   (interactive)
                                   (lsp-treemacs--open-file-in-mru path)))))))

(defun lsp-treemacs--set-mode-line-format (buffer title)
  "Set the mode line format of BUFFER to TITLE.
This function sets the `mode-name' or `mode-line-format'
depending on if a custom mode line is detected."
  (with-current-buffer buffer
    (cond ((or (fboundp 'spaceline-install)
               (memq 'moody-mode-line-buffer-identification
                     (default-value 'mode-line-format))
               (and (fboundp 'doom-modeline)
                    (fboundp 'doom-modeline-def-modeline)))
           (setq mode-name title))
          (t
           (setq mode-line-format title)))))

(defun lsp-treemacs--do-search (method params title prefix-args)
  (let ((search-buffer (get-buffer-create "*LSP Lookup*"))
        (window (display-buffer (get-buffer-create "*LSP Lookup*"))))
    (lsp-request-async
     method
     params
     (lambda (refs)
       (lsp-treemacs--set-mode-line-format search-buffer " Rendering results... ")
       (lsp-with-cached-filetrue-name
        (let ((lsp-file-truename-cache (ht)))
          (lsp-treemacs-render (lsp-treemacs--handle-references refs)
                               (format title (length refs))
                               (and prefix-args (not (equal prefix-args 0))))))
       (lsp--info "Refresh completed!"))
     :mode 'detached
     :cancel-token :treemacs-lookup)

    (unless (equal prefix-args 0)
      (select-window window)
      ;; (set-window-dedicated-p window t)
      )

    (with-current-buffer search-buffer
      (lsp-treemacs--set-mode-line-format search-buffer " Loading... ")
      (setq-local lsp-treemacs-tree nil)
      (lsp-treemacs-generic-refresh))))

;;;###autoload
(defun lsp-treemacs-references (arg)
  "Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of
references automatically."
  (interactive "P")
  (lsp-treemacs--do-search
   "textDocument/references"
   `(:context (:includeDeclaration t) ,@(lsp--text-document-position-params))
   " Found %s references "
   arg))

;;;###autoload
(defun lsp-treemacs-implementations (arg)
  "Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of
implementations automatically."
  (interactive "P")
  (lsp-treemacs--do-search "textDocument/implementation"
                           (lsp--text-document-position-params)
                           " Found %s implementations "
                           arg))


;; Call hierarchy.

(lsp-defun lsp-treemacs--call-hierarchy-ret-action ((&CallHierarchyItem :uri :selection-range (&Range :start)))
  "Build the ret action for a call hierarchy item using URI and START range."
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point start))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-treemacs--call-hierarchy-children (buffer method outgoing node callback)
  (-let [item (plist-get node :item)]
    (with-current-buffer buffer
      (lsp-request-async
       method
       (list :item item)
       (lambda (result)
         (funcall
          callback
          (seq-map
           (-lambda (node)
             (-let* (((child-item &as &CallHierarchyItem :_name :kind :_detail? :_uri :selection-range (&Range :_start))
                      (if outgoing
                          (lsp:call-hierarchy-outgoing-call-to node)
                        (lsp:call-hierarchy-incoming-call-from node)))
                     (label (lsp-render-symbol child-item t)))
               (list :label label
                     :key label
                     :icon (lsp-treemacs-symbol-kind->icon kind)
                     :children-async (-partial #'lsp-treemacs--call-hierarchy-children buffer method outgoing)
                     :ret-action (lambda (&rest _)
                                   (interactive)
                                   (lsp-treemacs--call-hierarchy-ret-action child-item))
                     :item child-item)))
           result)))
       :mode 'detached))))

;;;###autoload
(defun lsp-treemacs-call-hierarchy (outgoing)
  "Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy."
  (interactive "P")
  (unless (lsp-feature? "textDocument/prepareCallHierarchy")
    (user-error "Call hierarchy not supported by the current servers: %s"
                (-map #'lsp--workspace-print (lsp-workspaces))))
  (let ((buffer (current-buffer)))
    (select-window
     (display-buffer-in-side-window
      (lsp-treemacs-render
       (seq-map
        (-lambda ((item &as &CallHierarchyItem :kind :name))
          (list :label (lsp-render-symbol item t)
                :key name
                :icon (lsp-treemacs-symbol-kind->icon kind)
                :children-async (-partial
                                 #'lsp-treemacs--call-hierarchy-children
                                 buffer
                                 (if outgoing
                                     "callHierarchy/outgoingCalls"
                                   "callHierarchy/incomingCalls")
                                 outgoing)
                :ret-action (lambda (&rest _)
                              (interactive)
                              (lsp-treemacs--call-hierarchy-ret-action item))
                :item item))
        (lsp-request "textDocument/prepareCallHierarchy"
                     (lsp--text-document-position-params)))
       (concat (if outgoing "Outgoing" "Incoming") " Call Hierarchy")
       lsp-treemacs-call-hierarchy-expand-depth "*Call Hierarchy*" nil t) nil))))



;; Type hierarchy.

(defconst lsp-treemacs--hierarchy-sub 0)
(defconst lsp-treemacs--hierarchy-super 1)
(defconst lsp-treemacs--hierarchy-both 2)

(defun lsp-treemacs--type-hierarchy-render-nodes (result loaded? &optional direction)
  (-map (-lambda ((it &as &TypeHierarchyItem :name :kind :uri :range (&Range :start) :children? :parents?))
          `(:label ,(concat name (cond
                                  ((eq lsp-treemacs--hierarchy-sub direction) (propertize " ↓" 'face 'shadow))
                                  ((eq lsp-treemacs--hierarchy-super direction) (propertize " ↑" 'face 'shadow))))
                   :key ,name
                   :icon ,(lsp-treemacs-symbol-kind->icon kind)
                   ,@(if loaded?
                         (list :children (append
                                          (lsp-treemacs--type-hierarchy-render-nodes children? nil lsp-treemacs--hierarchy-sub)
                                          (lsp-treemacs--type-hierarchy-render-nodes parents? nil lsp-treemacs--hierarchy-super)))
                       (list :children-async (-partial #'lsp-treemacs--type-hierarchy-render
                                                       it
                                                       direction)))
                   :ret-action ,(lambda (&rest _)
                                  (interactive)
                                  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
                                  (goto-char (lsp--position-to-point start))
                                  (run-hooks 'xref-after-jump-hook))))
        result))

(lsp-defun lsp-treemacs--type-hierarchy-render ((&TypeHierarchyItem :uri :range (&Range :start)) direction _ callback)
  (lsp-request-async
   "textDocument/typeHierarchy"
   (lsp-make-type-hierarchy-params :text-document (lsp-make-text-document-item :uri uri)
                                   :position start
                                   :direction direction
                                   :resolve 1)
   (-lambda ((&TypeHierarchyItem :children? :parents?))
     (funcall callback (lsp-treemacs--type-hierarchy-render-nodes
                        (if (eq direction lsp-treemacs--hierarchy-sub)
                            children?
                          parents?)
                        nil direction)))))

;;;###autoload
(defun lsp-treemacs-type-hierarchy (direction)
  "Show the type hierarchy for the symbol at point.
With prefix 0 show sub-types.
With prefix 1 show super-types.
With prefix 2 show both."
  (interactive "P")
  (unless (lsp--find-workspaces-for "textDocument/typeHierarchy")
    (user-error "Type hierarchy not supported by the current servers: %s"
                (-map #'lsp--workspace-print (lsp-workspaces))))
  (setq direction (or direction 0))
  (let ((workspaces (lsp-workspaces))
        (result (lsp-request
                 "textDocument/typeHierarchy"
                 (-> (lsp--text-document-position-params)
                     (plist-put :direction direction)
                     (plist-put :resolve 1)))))
    (if result
        (pop-to-buffer
         (lsp-treemacs-render
          (lsp-treemacs--type-hierarchy-render-nodes (vector result) t)
          (concat (cond
                   ((eq lsp-treemacs--hierarchy-sub direction) "Sub")
                   ((eq lsp-treemacs--hierarchy-super direction) "Super")
                   ((eq lsp-treemacs--hierarchy-both direction) "Sub/Super"))
                  " Type Hierarchy")
          lsp-treemacs-type-hierarchy-expand-depth
          "*lsp-treemacs-call-hierarchy*"))
      (user-error "No class under point."))
    (setq lsp--buffer-workspaces workspaces)))


;; errors

(defun lsp-treemacs--error-list-diags (_folder file &rest _)
  (->> (lsp-diagnostics)
       (gethash file)
       (-filter #'lsp-treemacs--match-diagnostic-severity)
       (-sort (-lambda ((&Diagnostic :range (&Range :start (&Position :character char-a
                                                                      :line line-a)))
                        (&Diagnostic :range (&Range :start (&Position :character char-b
                                                                      :line line-b))))
                (if (= line-a line-b)
                    (< char-a char-b)
                  (< line-a line-b))))
       (-map (-lambda ((diag &as &Diagnostic
                             :severity?
                             :message
                             :range (&Range :start (start &as &Position :line :character))
                             :source?))
               (list :id message
                     :label (format (propertize "%s %s %s" 'face 'default)
                                    (if source?
                                        (propertize (format "[%s]" source?)
                                                    'face 'shadow)
                                      "")
                                    message
                                    (propertize (format "(%s:%s)" line character)
                                                'face 'lsp-details-face))
                     :icon-literal (lsp-treemacs--diagnostic-icon severity?)
                     :ret-action (lambda (&rest _)
                                   (lsp-treemacs--open-file-in-mru file)
                                   (->> start
                                        lsp--position-to-point
                                        goto-char)
                                   (run-hooks 'xref-after-jump-hook))
                     :file file
                     :diag diag
                     :actions `(["Quick fix..." lsp-treemacs-list-errors-quick-fix ]
                                ["Go to..." lsp-treemacs-list-errors-quick-fix ]))))))

(defun lsp-treemacs-errors--list-files (folder &rest _)
  (->> (lsp-diagnostics)
       (ht-keys)
       (-keep
        (lambda (file)
          (when (and (lsp-f-ancestor-of? folder file)
                     (lsp-treemacs-errors--diags? (lsp-diagnostics-stats-for file)))
            (list :id file
                  :label (format "%s %s %s"
                                 (f-filename file)
                                 (->> (append (lsp-diagnostics-stats-for file) ())
                                      (-map-indexed
                                       (lambda (index count)
                                         (unless (zerop count)
                                           (propertize
                                            (number-to-string count)
                                            'face (alist-get index lsp-treemacs-file-face-map)))))
                                      (-filter #'identity)
                                      (s-join "/"))
                                 (propertize (f-dirname (f-relative file folder))
                                             'face 'lsp-details-face))
                  :icon (if (f-directory? file) 'dir-closed (f-ext file))
                  :children (-partial #'lsp-treemacs--error-list-diags folder file)
                  :ret-action (lambda (&rest _)
                                (interactive)
                                (lsp-treemacs--open-file-in-mru file))))))))

(lsp-treemacs-define-action lsp-treemacs-quick-fix (:file :diag)
  "Select the element under cursor."
  (lsp-treemacs--open-file-in-mru file)
  (-let [(&Diagnostic :range (&RangeToPoint :start)) diag]
    (goto-char start)
    (call-interactively #'lsp-execute-code-action)))

(defun lsp-treemacs-cycle-severity  ()
  "Cycle through the severity levels shown in the errors list"
  (interactive)
  (setq lsp-treemacs-error-list-severity
        (if (= lsp-treemacs-error-list-severity lsp/diagnostic-severity-error)
            lsp/diagnostic-severity-hint
          (1- lsp-treemacs-error-list-severity)))
  (lsp-treemacs-errors-list--refresh))

(defun lsp-treemacs-errors--diags? (diags)
  (->> diags
       (seq-map-indexed (lambda (count index)
                          (when (and (not (zerop count))
                                     (<= index lsp-treemacs-error-list-severity))
                            count)))
       (seq-some #'identity)))

(defun lsp-treemacs--build-error-list (folder)
  (when-let ((diags (append (lsp-diagnostics-stats-for folder) ())))
    (when (lsp-treemacs-errors--diags? diags)
      (list :label (format
                    (propertize "%s %s %s" 'face 'default)
                    (f-filename folder)
                    (->> diags
                         (-map-indexed
                          (lambda (index count)
                            (when (and (not (zerop count))
                                       (<= index lsp-treemacs-error-list-severity))
                              (propertize
                               (number-to-string count)
                               'face (alist-get index lsp-treemacs-file-face-map)))))
                         (-filter #'identity)
                         (s-join "/"))
                    (propertize (f-dirname folder)
                                'face 'lsp-lens-face))
            :id folder
            :icon 'root
            :children (-partial #'lsp-treemacs-errors--list-files folder)
            :ret-action (lambda (&rest _)
                          (interactive)
                          (lsp-treemacs--open-file-in-mru folder))))))

(defvar lsp-treemacs--current-workspaces nil)

(defun lsp-treemacs-errors-list--refresh ()
  (lsp-treemacs-render
   (if (and lsp-treemacs-error-list-current-project-only
            lsp-treemacs--current-workspaces)
       (->> lsp-treemacs--current-workspaces
            (-map #'lsp-workspace-folders)
            (-flatten)
            (-keep #'lsp-treemacs--build-error-list))
     (->> (lsp-session)
          (lsp-session-folders)
          (-keep #'lsp-treemacs--build-error-list)))
   "Errors List"
   lsp-treemacs-error-list-expand-depth
   lsp-treemacs-errors-buffer-name
   `(["Cycle Severity" lsp-treemacs-cycle-severity])))

;;;###autoload
(defun lsp-treemacs-errors-list ()
  (interactive)
  (setq lsp-treemacs--current-workspaces (lsp-workspaces))
  (-if-let (buffer (get-buffer lsp-treemacs-errors-buffer-name))
      (progn
        (select-window (display-buffer-in-side-window buffer lsp-treemacs-errors-position-params))
        (lsp-treemacs-errors-list--refresh))
    (let* ((buffer (lsp-treemacs-errors-list--refresh))
           (window (display-buffer-in-side-window buffer lsp-treemacs-errors-position-params)))
      (select-window window)
      (set-window-dedicated-p window t)
      (lsp-treemacs-error-list-mode 1)

      (add-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs-errors-list--refresh)
      (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-buffer nil t)))

  (let ((buf (lsp-treemacs-errors-list--refresh)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (lsp-treemacs-error-list-mode 1))))

(defun lsp-treemacs--diagnostic-icon (severity)
  "Get the icon for DIAGNOSTIC."
  (cl-case severity
    (1 treemacs-icon-error)
    (2 treemacs-icon-warning)
    (t treemacs-icon-info)))

(defun lsp-treemacs--kill-buffer ()
  "Kill buffer hook."
  (remove-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs-errors-list--refresh))

(defvar lsp-treemacs-error-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "x") #'lsp-treemacs-quick-fix)
    (define-key m (kbd "=") #'lsp-treemacs-cycle-severity)
    m)
  "Keymap for `lsp-treemacs-error-list-mode'.")

(define-minor-mode lsp-treemacs-error-list-mode "LSP Treemacs mode for listing errors."
  :keymap lsp-treemacs-error-list-mode-map
  :group 'lsp-treemacs)

(defun lsp-treemacs--match-diagnostic-severity (diagnostic)
  (<= (lsp:diagnostic-severity? diagnostic)
      (prefix-numeric-value lsp-treemacs-error-list-severity)))

(provide 'lsp-treemacs)
;;; lsp-treemacs.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

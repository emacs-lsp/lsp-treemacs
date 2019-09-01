;;; lsp-treemacs-tvp.el --- LSP Tree View Protocol      -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Evgeny Kurnevsky

;; Author: Evgeny Kurnevsky
;; Keywords: languages

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

;; Metals Tree View Protocol implementation

;;; Code:
(require 'treemacs)
(require 'treemacs-extensions)
(require 'treemacs-icons)

(require 'lsp-mode)

(defgroup lsp-treemacs-tvp nil
  "LSP Tree View Protocol."
  :group 'lsp-treemacs
  :link '(url-link "https://scalameta.org/metals/docs/editors/tree-view-protocol.html")
  :package-version '(lsp-treemacs . "0.1"))

(defcustom lsp-treemacs-tvp-theme 'Metals-light
  "The theme for Tree View icons."
  :group 'lsp-treemacs-tvp
  :type '(choice
           (const :tag "Light" 'Metals-light)
           (const :tag "Dark" 'Metals-dark))
  :package-version '(lsp-treemacs . "0.1"))

(treemacs-create-theme "Metals"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode/metals")
  :config
  (progn
    (treemacs-create-icon :file "class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "enum.png" :extensions (enum) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "object.png" :extensions (object) :fallback "-")
    (treemacs-create-icon :file "trait.png" :extensions (trait) :fallback "-")
    (treemacs-create-icon :file "val.png" :extensions (val) :fallback "-")
    (treemacs-create-icon :file "var.png" :extensions (var) :fallback "-")))

(treemacs-create-theme "Metals-dark"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode/metals")
  :config
  (progn
    (treemacs-create-icon :file "book-dark.png" :extensions (book) :fallback "-")
    (treemacs-create-icon :file "bug-dark.png" :extensions (bug) :fallback "-")
    (treemacs-create-icon :file "github-dark.png" :extensions (github) :fallback "-")
    (treemacs-create-icon :file "gitter-dark.png" :extensions (gitter) :fallback "-")
    (treemacs-create-icon :file "issue-opened-dark.png" :extensions (issue-opened) :fallback "-")
    (treemacs-create-icon :file "twitter-dark.png" :extensions (twitter) :fallback "-")))

(treemacs-create-theme "Metals-light"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode/metals")
  :config
  (progn
    (treemacs-create-icon :file "book-light.png" :extensions (book) :fallback "-")
    (treemacs-create-icon :file "bug-light.png" :extensions (bug) :fallback "-")
    (treemacs-create-icon :file "github-light.png" :extensions (github) :fallback "-")
    (treemacs-create-icon :file "gitter-light.png" :extensions (gitter) :fallback "-")
    (treemacs-create-icon :file "issue-opened-light.png" :extensions (issue-opened) :fallback "-")
    (treemacs-create-icon :file "twitter-light.png" :extensions (twitter) :fallback "-")))

(defun lsp-treemacs-workspace-at-point ()
  "Find the LSP workspace at the point in the treemacs buffer."
  (lsp-find-workspace 'metals (treemacs-project->path (treemacs-project-at-point))))

(defun lsp-treemacs-tvp--children (viewId &optional nodeUri)
  "Get children for NODEURI with VIEWID."
  (with-lsp-workspace (lsp-treemacs-workspace-at-point)
    (append (ht-get (lsp-request "metals/treeViewChildren" `(:viewId ,viewId
                                                              :nodeUri ,nodeUri)) "nodes") nil)))

(defun lsp-treemacs-tvp--ret-action (&rest _)
  "Tree View RET action."
  (with-lsp-workspace (lsp-treemacs-workspace-at-point)
    (let ((command (treemacs--prop-at-point :command)))
      (pcase (ht-get command "command")
        (`"metals-echo-command" (lsp-send-execute-command (elt (ht-get command "arguments") 0)))
        (`"metals.goto" (lsp-send-execute-command "goto" (ht-get command "arguments")))
        (`"build-connect" (lsp-send-execute-command "build-connect"))
        (`"build-import" (lsp-send-execute-command "build-import"))
        (`"compile-cascade" (lsp-send-execute-command "compile-cascade"))
        (`"compile-cancel" (lsp-send-execute-command "compile-cancel"))
        (c (lsp-warn "Unknown metals Tree View command: %s" c))))))

(defun lsp-treemacs-tvp--icon (name &optional default)
  "Get the icon for the NAME.
Return DEFAULT if there is no such icon."
  (pcase name
    (`"class" (treemacs-get-icon-value 'class nil 'Metals))
    (`"enum" (treemacs-get-icon-value 'enum nil 'Metals))
    (`"field" (treemacs-get-icon-value 'field nil 'Metals))
    (`"interface" (treemacs-get-icon-value 'interface nil 'Metals))
    (`"method" (treemacs-get-icon-value 'method nil 'Metals))
    (`"object" (treemacs-get-icon-value 'object nil 'Metals))
    (`"trait" (treemacs-get-icon-value 'trait nil 'Metals))
    (`"val" (treemacs-get-icon-value 'val nil 'Metals))
    (`"var" (treemacs-get-icon-value 'var nil 'Metals))
    (`"book" (treemacs-get-icon-value 'book nil lsp-treemacs-tvp-theme))
    (`"bug" (treemacs-get-icon-value 'bug nil lsp-treemacs-tvp-theme))
    (`"github" (treemacs-get-icon-value 'github nil lsp-treemacs-tvp-theme))
    (`"gitter" (treemacs-get-icon-value 'gitter nil lsp-treemacs-tvp-theme))
    (`"issue-opened" (treemacs-get-icon-value 'issue-opened nil lsp-treemacs-tvp-theme))
    (`"twitter" (treemacs-get-icon-value 'twitter nil lsp-treemacs-tvp-theme))
    (_ default)))

(treemacs-define-leaf-node tvp-leaf 'dynamic-icon
  :ret-action #'lsp-treemacs-tvp--ret-action)

(treemacs-define-expandable-node tvp-node
  :icon-open-form (lsp-treemacs-tvp--icon (treemacs-button-get node :icon) (treemacs-as-icon " ▾ "))
  :icon-closed-form (lsp-treemacs-tvp--icon (treemacs-button-get node :icon) (treemacs-as-icon " ▸ "))
  :ret-action #'lsp-treemacs-tvp--ret-action
  :query-function (lsp-treemacs-tvp--children (treemacs-button-get node :viewId) (treemacs-button-get node :nodeUri))
  :render-action (treemacs-render-node
                   :icon (lsp-treemacs-tvp--icon (ht-get item "icon") (if (ht-get item "collapseState")
                                                                        (treemacs-as-icon " ▸ ")
                                                                        (treemacs-as-icon " • ")))
                   :label-form (ht-get item "label")
                   :state (if (ht-get item "collapseState")
                            treemacs-tvp-node-closed-state
                            treemacs-tvp-leaf-state)
                   :face 'default
                   :key-form (ht-get item "nodeUri")
                   :more-properties (:viewId (ht-get item "viewId")
                                      :nodeUri (ht-get item "nodeUri")
                                      :command (ht-get item "command")
                                      :icon (ht-get item "icon"))))

(treemacs-define-expandable-node tvp-root
  :icon-open (treemacs-as-icon " ▾ ")
  :icon-closed (treemacs-as-icon " ▸ ")
  :query-function `(,(ht ("label" "Build") ("viewId" "metalsBuild"))
                     ,(ht ("label" "Compile") ("viewId" "metalsCompile"))
                     ,(ht ("label" "Help") ("viewId" "metalsHelp")))
  :render-action (treemacs-render-node
                   :icon (treemacs-as-icon " ▸ ")
                   :label-form (ht-get item "label")
                   :state treemacs-tvp-node-closed-state
                   :face 'default
                   :key-form (ht-get item "label")
                   :more-properties (:viewId (ht-get item "viewId")
                                      :nodeUri (ht-get item "nodeUri")))
  :root-marker t
  :root-label "Tree View"
  :root-face 'default
  :root-key-form 'TreeView)

(defun lsp-treemacs-tvp--predicate (project)
  "Check if the Metals Tree View should be displayed for the PROJECT."
  (lsp-find-workspace 'metals (treemacs-project->path project)))

(treemacs-define-project-extension
  :extension #'treemacs-TVP-ROOT-extension
  :predicate #'lsp-treemacs-tvp--predicate
  :position 'top)

(provide 'lsp-treemacs-tvp)
;;; lsp-treemacs-tvp.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

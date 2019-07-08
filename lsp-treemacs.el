;;; lsp-treemacs.el --- LSP treemacs                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ivan Yonchovski

;; Author: Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (treemacs "2.5") (lsp-mode "6.0"))
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

(treemacs--select-icon-set)

(defgroup lsp-treemacs nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server")

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

(defun lsp-treemacs--root-folders ()
  "Get root folders containing errors."

  (let ((diagnostics (lsp-diagnostics)))
    (->> (lsp-session)
         lsp-session-folders
         (-filter (lambda (folder-name)
                    (-some (-partial #'s-starts-with? folder-name)
                           (ht-keys diagnostics))))
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

(defun lsp-treemacs-open-file (&rest _)
  "Open file."
  (interactive)
  (find-file-other-window (button-get (treemacs-node-at-point) :key)))

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
                 (when (s-starts-with? root-folder file-name)
                   file-diags)))
       (apply #'append)))

(defun lsp-treemacs--diag-statistics (file-diagnostics)
  "Calculate FILE-DIAGNOSTICS statistics."
  (string-join
   (-map (-lambda ((severity . diagnostics))
           (propertize (f-filename (number-to-string (length diagnostics)))
                       'face (cl-rest (assoc severity lsp-treemacs-file-face-map))))
         (-group-by 'lsp-diagnostic-severity file-diagnostics))
   "/"))

(defun lsp-treemacs--get-files (project-root)
  "Get files with errors in PROJECT-ROOT."
  (--> (lsp-diagnostics)
       ht->alist
       (-keep (-lambda ((file-name . file-diagnostics))
                (when (s-starts-with? project-root file-name)
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
  :icon-open-form (lsp-treemacs--diagnostic-icon (cl-rest (treemacs-button-get btn :data)))
  :icon-closed-form (lsp-treemacs--diagnostic-icon (cl-rest (treemacs-button-get btn :data)))
  :query-function (lsp-treemacs--errors (treemacs-button-get btn :data))
  :ret-action 'lsp-treemacs-open-error
  :render-action
  (treemacs-render-node
   :icon (treemacs-as-icon ". " 'face 'font-lock-string-face)
   :label-form (propertize (lsp-diagnostic-message item) 'face 'default)
   :state treemacs-lsp-error-open-state
   :key-form item))

(treemacs-define-expandable-node lsp-files
  :icon-open-form  (treemacs-icon-for-file (treemacs-button-get btn :key))
  :icon-closed-form  (treemacs-icon-for-file (treemacs-button-get btn :key))
  :query-function (lsp-treemacs--errors (treemacs-button-get btn :key))
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
  :icon-open treemacs-icon-root
  :icon-closed treemacs-icon-root
  :query-function (lsp-treemacs--get-files (treemacs-button-get btn :key))
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
   :icon treemacs-icon-root
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
    m)
  "Keymap for `lsp-treemacs-error-list-mode'.")

(define-minor-mode lsp-treemacs-error-list-mode ""
  nil nil nil
  :keymap lsp-treemacs-error-list-mode-map
  :group 'lsp-treeemacs)

;;;###autoload
(defun lsp-treemacs-errors-list ()
  "Display treemacs error list."
  (interactive)

  (-if-let (buffer (get-buffer "*LSP Error List*"))
      (select-window (display-buffer-in-side-window buffer '((side . bottom))))
    (let* ((buffer (get-buffer-create "*LSP Error List*"))
           (window (display-buffer-in-side-window buffer '((side . bottom)))))
      (select-window window)
      (set-window-dedicated-p window t)
      (treemacs-initialize)
      (lsp-treemacs-error-list-mode 1)

      (treemacs-LSP-ERROR-LIST-extension)

      (add-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs--after-diagnostics)
      (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-buffer nil t))))

(provide 'lsp-treemacs)
;;; lsp-treemacs.el ends here

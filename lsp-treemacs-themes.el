;;; lsp-treemacs-themes.el --- LSP treemacs themes -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Eric Dallo

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
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  LSP treemacs themes
;;
;;; Code:

(require 'treemacs)
(require 'treemacs-themes)

(defcustom lsp-treemacs-theme "Default"
  "The `lsp-treemacs' theme."
  :type 'string
  :group 'lsp-treemacs)

(treemacs-modify-theme "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode")
  :config
  (progn
    (treemacs-create-icon :file "BooleanData.png" :extensions (boolean-data) :fallback "-")
    (treemacs-create-icon :file "ColorPalette.png" :extensions (color-palette) :fallback "-")
    (treemacs-create-icon :file "Document.png" :extensions (document) :fallback "-")
    (treemacs-create-icon :file "Enumerator.png" :extensions (enumerator) :fallback "-")
    (treemacs-create-icon :file "EnumItem.png" :extensions (enumitem) :fallback "-")
    (treemacs-create-icon :file "Indexer.png" :extensions (indexer) :fallback "-")
    (treemacs-create-icon :file "IntelliSenseKeyword.png" :extensions (intellisense-keyword) :fallback "-")
    (treemacs-create-icon :file "LocalVariable.png" :extensions (localvariable) :fallback "-")
    (treemacs-create-icon :file "Numeric.png" :extensions (numeric) :fallback "-")
    (treemacs-create-icon :file "Operator.png" :extensions (operator) :fallback "-")
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
    (treemacs-create-icon :file "file_type_flutter.png" :extensions (flutter) :fallback "-")
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
    (treemacs-create-icon :file "project.png" :extensions (java-project) :fallback "-")

    (treemacs-create-icon :file "symbol-array.png" :extensions (array) :fallback "-")
    (treemacs-create-icon :file "symbol-boolean.png" :extensions (boolean) :fallback "-")
    (treemacs-create-icon :file "symbol-class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "symbol-color.png" :extensions (color) :fallback "-")
    (treemacs-create-icon :file "symbol-constant.png" :extensions (constant) :fallback "-")
    (treemacs-create-icon :file "symbol-enumerator-member.png" :extensions (enum-member) :fallback "-")
    (treemacs-create-icon :file "symbol-enumerator.png" :extensions (enum) :fallback "-")
    (treemacs-create-icon :file "symbol-event.png" :extensions (event) :fallback "-")
    (treemacs-create-icon :file "symbol-field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "symbol-interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "symbol-key.png" :extensions (key) :fallback "-")
    (treemacs-create-icon :file "symbol-keyword.png" :extensions (keyword) :fallback "-")
    (treemacs-create-icon :file "symbol-method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "symbol-misc.png" :extensions (misc) :fallback "-")
    (treemacs-create-icon :file "symbol-namespace.png" :extensions (namespace) :fallback "-")
    (treemacs-create-icon :file "symbol-namespace.png" :extensions (module) :fallback "-")
    (treemacs-create-icon :file "symbol-numeric.png" :extensions (numeric) :fallback "-")
    (treemacs-create-icon :file "symbol-operator.png" :extensions (operator) :fallback "-")
    (treemacs-create-icon :file "symbol-parameter.png" :extensions (parameter) :fallback "-")
    (treemacs-create-icon :file "symbol-property.png" :extensions (property) :fallback "-")
    (treemacs-create-icon :file "symbol-ruler.png" :extensions (ruler) :fallback "-")
    (treemacs-create-icon :file "symbol-snippet.png" :extensions (snippet) :fallback "-")
    (treemacs-create-icon :file "symbol-string.png" :extensions (string) :fallback "-")
    (treemacs-create-icon :file "symbol-structure.png" :extensions (struct) :fallback "-")
    (treemacs-create-icon :file "symbol-variable.png" :extensions (variable) :fallback "-")))

(treemacs-create-theme "Eclipse"
  :extends "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/eclipse")
  :config
  (progn
    (treemacs-create-icon :file "vscode/Namespace.png" :extensions (namespace) :fallback "-")
    (treemacs-create-icon :file "text.png" :extensions (text) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "function.png" :extensions (function) :fallback "-")
    (treemacs-create-icon :file "constructor.png" :extensions (constructor) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "variable.png" :extensions (variable) :fallback "-")
    (treemacs-create-icon :file "class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "module.png" :extensions (module) :fallback "-")
    (treemacs-create-icon :file "property.png" :extensions (property) :fallback "-")
    (treemacs-create-icon :file "unit.png" :extensions (unit) :fallback "-")
    (treemacs-create-icon :file "value.png" :extensions (value) :fallback "-")
    (treemacs-create-icon :file "enum.png" :extensions (enum) :fallback "-")
    (treemacs-create-icon :file "keyword.png" :extensions (keyword) :fallback "-")
    (treemacs-create-icon :file "snippet.png" :extensions (snippet) :fallback "-")
    (treemacs-create-icon :file "color.png" :extensions (color) :fallback "-")
    (treemacs-create-icon :file "file.png" :extensions (file) :fallback "-")
    (treemacs-create-icon :file "reference.png" :extensions (reference) :fallback "-")
    (treemacs-create-icon :file "folder.png" :extensions (folder) :fallback "-")
    (treemacs-create-icon :file "enummember.png" :extensions (enum-member) :fallback "-")
    (treemacs-create-icon :file "constant.png" :extensions (constant) :fallback "-")
    (treemacs-create-icon :file "struct.png" :extensions (struct) :fallback "-")
    (treemacs-create-icon :file "event.png" :extensions (event) :fallback "-")
    (treemacs-create-icon :file "operator.png" :extensions (operator) :fallback "-")
    (treemacs-create-icon :file "typeparameter.png" :extensions (type-parameter) :fallback "-")
    (treemacs-create-icon :file "template.png" :extensions (template) :fallback "-")))

(treemacs-create-theme "Netbeans"
  :extends "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/netbeans")
  :config
  (progn
    (treemacs-create-icon :file "vscode/Namespace.png" :extensions (namespace) :fallback "-")
    (treemacs-create-icon :file "text.png" :extensions (text) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "function.png" :extensions (function) :fallback "-")
    (treemacs-create-icon :file "constructor.png" :extensions (constructor) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "variable.gif" :extensions (variable) :fallback "-")
    (treemacs-create-icon :file "class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "module.png" :extensions (module) :fallback "-")
    (treemacs-create-icon :file "property.png" :extensions (property) :fallback "-")
    (treemacs-create-icon :file "unit.png" :extensions (unit) :fallback "-")
    (treemacs-create-icon :file "value.png" :extensions (value) :fallback "-")
    (treemacs-create-icon :file "enum.png" :extensions (enum) :fallback "-")
    (treemacs-create-icon :file "keyword.png" :extensions (keyword) :fallback "-")
    (treemacs-create-icon :file "snippet.png" :extensions (snippet) :fallback "-")
    (treemacs-create-icon :file "color.png" :extensions (color) :fallback "-")
    (treemacs-create-icon :file "file.png" :extensions (file) :fallback "-")
    (treemacs-create-icon :file "reference.png" :extensions (reference) :fallback "-")
    (treemacs-create-icon :file "folder.png" :extensions (folder) :fallback "-")
    (treemacs-create-icon :file "enummember.png" :extensions (enum-member) :fallback "-")
    (treemacs-create-icon :file "constant.png" :extensions (constant) :fallback "-")
    (treemacs-create-icon :file "struct.png" :extensions (struct) :fallback "-")
    (treemacs-create-icon :file "event.png" :extensions (event) :fallback "-")
    (treemacs-create-icon :file "operator.png" :extensions (operator) :fallback "-")
    (treemacs-create-icon :file "typeparameter.png" :extensions (type-parameter) :fallback "-")
    (treemacs-create-icon :file "template.png" :extensions (template) :fallback "-")))

(treemacs-create-theme "Idea"
  :extends "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/idea")
  :config
  (progn
    (treemacs-create-icon :file "vscode/template.png" :extensions (template) :fallback "-")
    (treemacs-create-icon :file "package.png" :extensions (unknown) :fallback "-")
    ;; (treemacs-create-icon :file "misc.png" :extensions (text) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (method) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (function) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (constructor) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (field) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (variable) :fallback "-")
    (treemacs-create-icon :file "class.png" :extensions (class) :fallback "-")
    (treemacs-create-icon :file "interface.png" :extensions (interface) :fallback "-")
    (treemacs-create-icon :file "package.png" :extensions (module) :fallback "-")
    (treemacs-create-icon :file "property.png" :extensions (property) :fallback "-")
    ;; (treemacs-create-icon :file "misc.png" :extensions (unit) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (value) :fallback "-")
    (treemacs-create-icon :file "enum.png" :extensions (enum) :fallback "-")
    ;; (treemacs-create-icon :file "misc.png" :extensions (keyword) :fallback "-")
    (treemacs-create-icon :file "snippet.png" :extensions (snippet) :fallback "-")
    ;; (treemacs-create-icon :file "misc.png" :extensions (color) :fallback "-")
    (treemacs-create-icon :file "ppFile.png" :extensions (file) :fallback "-")
    (treemacs-create-icon :file "misc.png" :extensions (reference) :fallback "-")
    (treemacs-create-icon :file "ppFile.png" :extensions (folder) :fallback "-")
    (treemacs-create-icon :file "enum.png" :extensions (enumMember) :fallback "-")
    (treemacs-create-icon :file "field.png" :extensions (constant) :fallback "-")
    (treemacs-create-icon :file "class.png" :extensions (struct) :fallback "-")
    (treemacs-create-icon :file "Event.png" :extensions (event) :fallback "-")
    (treemacs-create-icon :file "Misc.png" :extensions (operator) :fallback "-")
    ;; (treemacs-create-icon :file "Misc.png" :extensions (operator) :fallback "-")
    (treemacs-create-icon :file "Class.png" :extensions (typeParameter) :fallback "-")
    (treemacs-create-icon :file "Template.png" :extensions (template) :fallback "-")))

(treemacs-create-theme "Iconless"
  :icon-directory buffer-file-name)

(provide 'lsp-treemacs-themes)
;;; lsp-treemacs-themes.el ends here

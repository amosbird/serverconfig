;; treecrumbs.el --- Fast, tree-sitter based breadcrumbs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Vincent Ambo <tazjin@tvl.su>
;; Created: 2024-03-08
;; Version: 1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; URL: https://code.tvl.fyi/tree/tools/emacs-pkgs/treecrumbs
;;
;; This file is not (yet) part of GNU Emacs.

;;; Commentary:

;; This package provides a tree-sitter based implementation of "breadcrumbs",
;; that is indicators displaying where in the semantic structure of a document
;; the point is currently located.
;;
;; Imagine a large YAML-document where the names of the parent keys are far out
;; of view: Treecrumbs can quickly display the hierarchy of keys (e.g. `foo < []
;; < baz') and help figure out where point is.
;;
;; Treecrumbs only works if a tree-sitter parser for the target language is
;; available in the buffer, and the language is supported in the
;; `treecrumbs-languages'. Adding a new language is not difficult, and patches
;; for this are welcome.
;;
;; To active treecrumbs, enable `treecrumbs-mode'. This buffer-local minor mode
;; adds the crumbs to the buffer's `header-line-format'. Alternatively, users
;; can also use the `treecrumbs-line-segment' either in their own header-line,
;; tab-line or mode-line configuration.

;;; Code:

(require 'seq)
(require 'treesit)

(defvar treecrumbs-languages nil
  "Describes the tree-sitter language grammars supported by
treecrumbs, and how the breadcrumbs for their node types are
generated.

Alist of symbols representing tree-sitter languages (e.g. `yaml')
to another alist (the \"node type list\") describing how
different node types should be displayed in the crumbs.

See `define-treecrumbs-language' for more details on how to add a
language.")

(defmacro define-treecrumbs-language (lang &rest clauses)
  "Defines a new language for use in treecrumbs. LANG should be a
symbol representing the language as understood by treesit (e.g.
`yaml').

Each of CLAUSES is a cons cell mapping the name of a tree
node (in string format) to one of either:

1. a static string, which will become the breadcrumb verbatim

2. a tree-sitter query (in S-expression syntax) which must capture
   exactly one argument named `@key' that will become the
   breadcrumb (e.g. the name of a function, the key in a map, ...)

Treecrumbs will only consider node types that are mentioned in
CLAUSES. All other nodes are ignored when constructing the
crumbs.

The defined languages are stored in `treecrumbs-languages'."

  (declare (indent 1))
  (let ((compiled
         (seq-map (lambda (clause)
                    (if (stringp (cdr clause))
                        `(cons ,(car clause) ,(cdr clause))
                      `(cons ,(car clause)
                             (treesit-query-compile ',lang ',(cdr clause)))))
                  clauses)))
    `(setf (alist-get ',lang treecrumbs-languages nil nil #'equal) (list ,@compiled))))

(define-treecrumbs-language yaml
  ;; In YAML documents, crumbs are generated from the keys of maps, and from
  ;; elements of arrays. "block"-nodes are standard YAML syntax, "flow"-nodes
  ;; are inline JSON-ish syntax.
  ("block_mapping_pair" . ((block_mapping_pair key: (_) @key)))
  ("block_sequence" . "[]")

  ;; TODO: Why can this query not match on to (flow_pair)?
  ("flow_pair" . ((_) key: (_) @key))
  ("flow_sequence" . "[]"))

(define-treecrumbs-language json
  ;; In JSON documents, crumbs are generated from key names and array fields.
  ("pair" . ((pair key: (string (string_content) @key))))
  ("array" . "[]"))

(define-treecrumbs-language toml
  ;; TOML has sections, key names and arrays. Sections are the only
  ;; relevant difference to YAML. Nested keys are not parsed, and just
  ;; displayed as-is.
  ("table" . ((table (_) @key)) )
  ;; TODO: query cannot match on pair in inline_table, hence matching
  ;; directly on keys
  ("pair" . ([(dotted_key)
              (quoted_key)
              (bare_key)]))
  ("array" . "[]"))

;; Use treesit-inspect-node-at-point to debug
(define-treecrumbs-language cpp
  ;; In C++ files, crumbs are generated from namespaces and
  ;; identifier declarations.
  ("namespace_definition" . ([(namespace_definition
                               name: (namespace_identifier) @key)
                              (namespace_definition
                               name: (nested_namespace_specifier (namespace_identifier)) @key)
                              (namespace_definition
                               "namespace" @key
                               !name)]))

  ("function_definition" . ((function_definition
                             declarator:
                             [
                              (function_declarator declarator: (_) @key)
                              (reference_declarator
                               (function_declarator declarator: (_) @key))
                              (pointer_declarator
                               (function_declarator declarator: (_) @key))
                              ]
                             )))

  ("class_specifier" . ((class_specifier
                         name: (type_identifier) @key)))

  ("struct_specifier" . ((struct_specifier
                          name: (type_identifier) @key)))

  ;; ("field_declaration" . ((field_declaration
  ;;                          declarator: (_) @key)))

  ;; ("init_declarator" . ((init_declarator
  ;;                        declarator: (_) @key)))
  )

;; Use treesit-inspect-node-at-point to debug
(define-treecrumbs-language java
  ("class_declaration" . ((class_declaration name: (identifier) @key)))
  ("method_declaration" . ((method_declaration name: (identifier) @key))))

(defvar-local treecrumbs--current-crumbs nil
  "Current crumbs to display in the header line. Only updated when
the node under point changes.")

(defun treecrumbs--crumbs-for (node)
  "Construct the crumbs for the given NODE, if its language is
supported in `treecrumbs-languages'. This functions return value
is undefined, it directly updates the buffer-local
`treecrumbs--current-crumbs'."
  (let ((lang (cdr (assoc (treesit-node-language node) treecrumbs-languages))))
    (unless lang
      (user-error "No supported treecrumbs language at point!"))

    (setq-local treecrumbs--current-crumbs "")
    (treesit-parent-while
     node
     (lambda (parent)
       (when-let ((query (cdr (assoc (treesit-node-type parent) lang))))
         (setq-local treecrumbs--current-crumbs
                     (concat treecrumbs--current-crumbs
                             (if (string-empty-p treecrumbs--current-crumbs) "" " < ")
                             (if (stringp query)
                                 query
                               (substring-no-properties
                                (or (treesit-node-text (cdar (treesit-query-capture parent query))) "???"))))))
       t))))


(defvar-local treecrumbs--last-node nil
  "Caches the node that was last seen at point.")

(defun treecrumbs-at-point ()
  "Returns the treecrumbs at point as a string, if point is on a
node in a language supported in `treecrumbs-languages'.

The last known crumbs in a given buffer are cached, and only if
the node under point changes are they updated."
  (let ((node (treesit-node-at (point))))
    (when (or (not treecrumbs--current-crumbs)
              (not (equal treecrumbs--last-node node)))
      (setq-local treecrumbs--last-node node)
      (treecrumbs--crumbs-for node)))
  treecrumbs--current-crumbs)

(defvar treecrumbs-line-segment
  '(:eval (treecrumbs-at-point))

  "Treecrumbs segment for use in the header-line or mode-line.")

;;;###autoload
(define-minor-mode treecrumbs-mode
  "Display header line hints about current position in structure."
  :init-value nil
  :lighter " Crumbs"
  (if treecrumbs-mode
      (when-let ((parser (car (treesit-parser-list))))
        (if (cdr (assoc (treesit-parser-language parser) treecrumbs-languages))
            (push treecrumbs-line-segment header-line-format)))
    (setq header-line-format
          (delq treecrumbs-line-segment header-line-format))))

(provide 'treecrumbs)
;;; treecrumbs.el ends here

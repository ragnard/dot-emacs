;;; sql-trino.el --- Trino support for sql-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Authors
;; SPDX-License-Identifier: Apache-2.0

;; Author: Ragnar Dahlén <r.dahlen@gmail.com>
;; URL: https://github.com/ragnard/sql-trino
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0
;; Keywords: tools

;;; Commentary:

;;

;;; Code:

(defcustom sql-trino-program
  "docker"
  "Command to start trino."
  :type 'file
  :group 'SQL)

(defcustom sql-trino-options nil
  "List of additional options for `sql-trino-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-trino-docker-options
  '("run" "--network=host" "-i" "trinodb/trino:latest" "trino")
  "List of additional options for `sql-trino-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-trino-login-params
  `((user :default ,(user-login-name))
    ;; (url :default "http://localhost:8080")
    server)
  "List of login parameters needed to connect to trino."
  :type 'sql-login-params
  :group 'SQL)

(defvar sql-mode-trino-font-lock-keywords
  (eval-when-compile
    (list
     ;; SQLite commands
     ;;'("^[.].*$" . font-lock-doc-face)

     ;; SQLite Keyword
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"abort" "action" "add" "after" "all" "alter" "analyze" "and" "as"
"asc" "attach" "autoincrement" "before" "begin" "between" "by"
"cascade" "case" "cast" "check" "collate" "column" "commit" "conflict"
"constraint" "create" "cross" "database" "default" "deferrable"
"deferred" "delete" "desc" "detach" "distinct" "drop" "each" "else"
"end" "escape" "except" "exclusive" "exists" "explain" "fail" "for"
"foreign" "from" "full" "glob" "group" "having" "if" "ignore"
"immediate" "in" "index" "indexed" "initially" "inner" "insert"
"instead" "intersect" "into" "is" "isnull" "join" "key" "left" "like"
"limit" "match" "natural" "no" "not" "notnull" "null" "of" "offset"
"on" "or" "order" "outer" "plan" "pragma" "primary" "query" "raise"
"references" "regexp" "reindex" "release" "rename" "replace"
"restrict" "right" "rollback" "row" "savepoint" "select" "set" "table"
"temp" "temporary" "then" "to" "transaction" "trigger" "union"
"unique" "update" "using" "vacuum" "values" "view" "virtual" "when"
"where"
)
     ;; Data types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"int" "integer" "tinyint" "smallint" "mediumint" "bigint" "unsigned"
"big" "int2" "int8" "character" "varchar" "varying" "nchar" "native"
"nvarchar" "text" "clob" "blob" "real" "double" "precision" "float"
"numeric" "number" "decimal" "boolean" "date" "datetime" "json"
)
     ;; Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
;; Core functions
"abs" "changes" "coalesce" "glob" "ifnull" "hex" "last_insert_rowid"
"length" "like" "load_extension" "lower" "ltrim" "max" "min" "nullif"
"quote" "random" "randomblob" "replace" "round" "rtrim" "soundex"
"sqlite_compileoption_get" "sqlite_compileoption_used"
"sqlite_source_id" "sqlite_version" "substr" "total_changes" "trim"
"typeof" "upper" "zeroblob"
;; Date/time functions
"time" "julianday" "strftime"
"current_date" "current_time" "current_timestamp"
;; Aggregate functions
"avg" "count" "group_concat" "max" "min" "sum" "total"
)))

  "Trino SQL keywords used by font-lock.")

;;;###autoload
(defun sql-trino (&optional buffer)
  "Run trino using sql-mode"
  (interactive "P")
  (sql-product-interactive 'trino buffer))

(defun sql-comint-trino (product options &optional buf-name)
  "Create comint buffer and connect to Trino."
  (let* ((program (sql-get-product-feature product :sqli-program))
         (params (cond
                  ((string= "docker" program)
                   (append
                    sql-trino-docker-options
                    (if (not (string= "" sql-user))
                        (list "--user" sql-user))
                    options
                    (if (not (string= "" sql-server))
                        (list sql-server)))))))
    (sql-comint product params buf-name)))


(defun sql-trino-append-newline (sql)
  (message sql)
  ;sql
  ;(concat sql "\n")
  (sql-escape-newlines-filter sql)
  )

(defun sql-trino-completion-object (sqlbuf _schema)
  (sql-redirect-value sqlbuf "show tables" "^\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)," 1))

(sql-add-product 'trino "trino"
                 :free-software t
                 :font-lock sql-mode-trino-font-lock-keywords
                 :sqli-program sql-trino-program
                 :sqli-options sql-trino-options
                 :sqli-login sql-trino-login-params
                 :sqli-comint-func #'sql-comint-trino
                 :input-filter #'sql-trino-append-newline
                 ;; :list-all "show tables;"
                 ;; :list-table "describe %s;"
                 ;; :completion-object sql-trino-completion-object
                 :prompt-regexp "^trino> "
                 :prompt-length 7
                 :prompt-cont-regexp "^    -> ")

;; (sql-del-product 'trino)

(provide 'sql-trino)

;;; sql-trino.el ends here

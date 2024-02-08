(require 'sql)

;;;###autoload
(defun sql-duckdb (&optional buffer)
  "Run duckdb using sql-mode."
  (interactive "P")
  (sql-product-interactive 'duckdb buffer))

(defun sql-duckdb-completion-object (sqlbuf _schema)
  (sql-redirect-value sqlbuf "show tables" "^\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)," 1))

(defcustom sql-duckdb-program (or (executable-find "duckdb")
                                  "duckdb")
  "Command to start duckdub.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-duckdb-options nil
  "List of additional options for `sql-duckdb-program'."
  :type '(repeat string)
  :version "20.8" ;; FIXME: What is this?
  :group 'SQL)

(defcustom sql-duckdb-login-params '((database :file nil
                                               :must-match confirm))
  "List of login parameters needed to connect to duckdb."
  :type 'sql-login-params
  :version "26.1"
  :group 'SQL)

(sql-add-product 'duckdb "duckdb"
                 :free-software nil
                 :font-lock sql-mode-sqlite-font-lock-keywords  ;; Use sqlite for now
                 :sqli-program sql-duckdb-program
                 :sqli-options sql-duckdb-options
                 :sqli-login sql-duckdb-login-params
                 :sqli-comint-func #'sql-comint-sqlite
                 :list-all "show tables"
                 :list-table "describe %s"
                 :completion-object #'sql-duckdb-completion-object
                 :prompt-regexp "^D "
                 :prompt-length 2
                 :prompt-cont-regexp "^> ")

(provide 'sql-duckdb)

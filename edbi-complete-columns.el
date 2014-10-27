;;; edbi-complete-columns.el --- complete columns in table on dot smartly

;; Copyright (C) 2014 by IMAKADO

;; Prefix: edbi-cc:
;; Author: Kenji Imakado <ken.imakado -at- gmail.com>
;; Maintainer: imakado
;; Created: :2014-10-27
;; Keywords: 
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((imakado "0.12")  (edbi "0.1.3") (auto-complete "1.4.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Note: This program is very experimental.

;; Installation:

;; Add following lines below the setup code.

;; (eval-after-load 'edbi
;;    '(progn
;;       (require 'edbi-complete-columns)
;;       (edbi-cc:setup)))

;;; Code:

(require 'imakado)
(eval-when-compile
  (require 'cl))

(require 'auto-complete)
(require 'edbi)

(defun edbi-cc:advance-paren ()
  (interactive)
  (re-search-forward "([^()]*" nil t)
  (when (looking-at "(")
    (edbi-cc:advance-paren))
  (if (re-search-forward "[^()]*)" nil t)
      t
    (error "bat parenthesis")))

(defvar edbi-cc:from-terminator-re
  (rx string-start
      (or "where"
          "group"
          "having"
          "order"
          "limit"
          "procedure"
          "into"
          "for"
          ";"
          ")")
      string-end))

;; USE {INDEX|KEY}
;;       [FOR {JOIN|ORDER BY|GROUP BY}] ([index_list])
;;   | IGNORE {INDEX|KEY}
;;       [FOR {JOIN|ORDER BY|GROUP BY}] (index_list)
;;   | FORCE {INDEX|KEY}
;;       [FOR {JOIN|ORDER BY|GROUP BY}] (index_list)
(defvar edbi-cc:index-hint-re
  (rx string-start
      (or "use"
          "ignore"
          "force")
      string-end))

;; join_table:
;;     table_reference [INNER | CROSS] JOIN table_factor [join_condition]
;;   | table_reference STRAIGHT_JOIN table_factor
;;   | table_reference STRAIGHT_JOIN table_factor ON conditional_expr
;;   | table_reference {LEFT|RIGHT} [OUTER] JOIN table_reference join_condition
;;   | table_reference NATURAL [{LEFT|RIGHT} [OUTER]] JOIN table_factor
(defvar edbi-cc:join-start-re
  (rx string-start
      (or "inner"
          "cross"
          "join"
          "straight_join"
          "left"
          "right"
          "natural"
          )
      string-end))

(defvar edbi-cc:join-re
  (rx string-start
      (or "straight_join"
          "join"
          )
      string-end))

(defvar edbi-cc:join-condition
  (rx string-start
      (or "on"
          "using"
          )
      string-end))

(defun edbi-cc:sql-unquote (s)
  (let ((re (rx "`" (group (regexp "[^`]+")) "`")))
    (if (string-match re s)
        (match-string 1 s)
      s)))

(defun* edbi-cc:sql->table-alias&name (sql-str)
  (let ((case-fold-search t))
    (imakado-with-point-buffer sql-str
      (goto-char (point-min))
      (let ((from-points (loop while (re-search-forward (rx word-start "from" word-end) nil t)
                               collect (match-beginning 0))))
        (loop for p in from-points
              append
              (edbi-cc:tokens->table-alias&name (edbi-cc:tokenize p)))))))


(defsubst edbi-cc:token-and-advance ()
  (let ((from (point))
        (case-fold-search t))
  (cond
   ((looking-at "[ \t\n]+")
    (prog1 ""
      (goto-char (match-end 0))))
   ((looking-at "(")
    (let ((from (point)))
      (if (ignore-errors (edbi-cc:advance-paren))
          (buffer-substring-no-properties from (point))
        nil)))
    ((looking-at "`")
     (if (re-search-forward "[^`\n]+`" nil t)
         (prog1 (buffer-substring-no-properties from (point))
           (goto-char (match-end 0)))
       nil))
    ((looking-at ",") 
     (prog1 ","
       (forward-char 1)))
    ((looking-at "{") 
     (prog1 "{"
       (forward-char 1)))
    ((looking-at "}") 
     (prog1 "}"
       (forward-char 1)))
    ((looking-at ";") 
     (prog1 ";"
       (forward-char 1)))
    ((looking-at ")") 
     (prog1 ")"
       (forward-char 1)))
    ((looking-at edbi-cc:from-terminator-re) nil)
    ((eobp) nil)
    ((looking-at "[^ \\\\),\\;\t\n]+")
     (prog1 (buffer-substring-no-properties (point) (match-end 0))
       (goto-char (match-end 0))))
    (t (prog1 ""
         (ignore-errors (forward-char 1)))))))

(defun* edbi-cc:tokenize (&optional (p (point)))
  (let ((bufstr (buffer-substring-no-properties (point-min) (point-max))))
    (imakado-with-point-buffer (typecase p
                                 (string p)
                                 (otherwise bufstr))
      (typecase p
        (string nil)
        (otherwise (goto-char p)))
      (imakado-aand
       (loop with tokens
             for token = (edbi-cc:token-and-advance)
             while token
             do (push token tokens)
             finally return (nreverse tokens))
       (imakado-remove-if (lambda (s)
                            (imakado-empty-string-p s))
                          it)))))

(defun* edbi-cc:tokens->table-alias&name (tokens)
  (let ((ret nil)
        (case-fold-search t))
    (when tokens
      (loop for token = (pop tokens)
            while tokens
            do
            (cond
             ((string-match edbi-cc:from-terminator-re token) (return))
             ;; join 
             ((string-match edbi-cc:join-re token)
              (imakado-when-let (next (first tokens))
                (cond
                 ((not (string-match edbi-cc:from-terminator-re next))
                  (let* ((table-name (pop tokens))
                         (next (first tokens)))
                    (cond
                     ((not (string-match edbi-cc:join-condition next))
                      ;; next may be alias
                      (push (cons (edbi-cc:sql-unquote next) (edbi-cc:sql-unquote table-name)) ret)
                      (pop tokens))))))))
             ;; from long_table t, noalias
             ((string-match (rx string-start
                                (or "from" ",")
                                string-end)
                            token)
              (when (first tokens)
                (unless (string-match "^{" (first tokens))
                  (let ((table-name (pop tokens)))
                    (cond
                     ((imakado-aand (first tokens)
                                    (string-match (rx string-start "as" string-end) it))
                      (pop tokens)
                      (imakado-aand (first tokens)
                                    (when (and (not (string-match edbi-cc:from-terminator-re it))
                                               (not (string-match edbi-cc:index-hint-re it)))
                                      (push (cons (edbi-cc:sql-unquote it) (edbi-cc:sql-unquote table-name)) ret)
                                      (pop tokens))))
                     (t (imakado-aand (first tokens)
                                      (when (and (not (string-match edbi-cc:from-terminator-re it))
                                                 (not (string-match edbi-cc:index-hint-re it))
                                                 ;; INNER JOIN hoge...
                                                 (not (string-match edbi-cc:join-start-re it))
                                                 (not (string-match "^,$" it)))
                                        (push (cons (edbi-cc:sql-unquote it) (edbi-cc:sql-unquote table-name)) ret)
                                        (pop tokens)))))))))
             (t nil)))
      (imakado-aand (nreverse ret)
                    (imakado-remove-if 
                     (lambda (alist)
                       (imakado-dbind (alias . table-name) alist
                         (string-match "^(" table-name)))
                     it)))))

; (edbi-cc:tokens->table-alias&name)


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ JOIN +++++")
      (expect '(("w" . "where") ("h" . "hoge"))
        ;;;; XXX
        (edbi-cc:sql->table-alias&name "\
from `where` w, hoge h" )
        )
      (expect '(("u" . "users"))
        ;;;; XXX
        (edbi-cc:sql->table-alias&name "\
from hoge JOIN users u ON hoge.hoge_id = u.id" )
        )
      (expect '(("a" . "aaaaa") ("b" . "bbbbbb") ("c" . "cccc"))
        ;;;; XXX
        (edbi-cc:sql->table-alias&name "\
FROM aaaaa a
INNER JOIN bbbbbb b ON a.id = b.a_id
INNER JOIN cccc  c ON b.id = c.b_id" )
        )
      
      (expect '(("u" . "users"))
        ;;;; XXX
        (edbi-cc:sql->table-alias&name "\
from hoge INNER JOIN users u ON hoge.hoge_id = u.id
  ; " )
        )

      (expect '(("u" . "users"))
        (edbi-cc:sql->table-alias&name "\
from hoge INNER JOIN users u  ON hoge.id = u.id
  ; " )
        )
      (expect '(("d" . "long_d") ("c" . "long_c"))
        (edbi-cc:sql->table-alias&name "\
`!!'  
FROM (select * from (select * from long_c c)) b ,long_d d where 
  ; " )
        )

      (expect '(("c" . "long_c"))
        (edbi-cc:sql->table-alias&name "select * from long_c c)) b ,long_c c where ")
        )
      
      
      
      (expect nil
        (edbi-cc:sql->table-alias&name "\
SELECT * FROM table1 USE INDEX (col1_index,col2_index)
  WHERE col1=1 AND col2=2 AND col3=3;"))

      (expect '(("a" . "where") ("c" . "long_c"))
        (edbi-cc:sql->table-alias&name "\
`!!'  
FROM `where` AS  a , (select * from (select * from long_c c)) b
,c where 
  ; " )
        )
    )))



;; SELECT
;;     [ALL | DISTINCT | DISTINCTROW ]
;;       [HIGH_PRIORITY]
;;       [STRAIGHT_JOIN]
;;       [SQL_SMALL_RESULT] [SQL_BIG_RESULT] [SQL_BUFFER_RESULT]
;;       [SQL_CACHE | SQL_NO_CACHE] [SQL_CALC_FOUND_ROWS]
;;     select_expr [, select_expr ...]
;;     [FROM table_references
;;       [PARTITION partition_list]
;;     [WHERE where_condition]
;;     [GROUP BY {col_name | expr | position}
;;       [ASC | DESC], ... [WITH ROLLUP]]
;;     [HAVING where_condition]
;;     [ORDER BY {col_name | expr | position}
;;       [ASC | DESC], ...]
;;     [LIMIT {[offset,] row_count | row_count OFFSET offset}]
;;     [PROCEDURE procedure_name(argument_list)]
;;     [INTO OUTFILE 'file_name'
;;         [CHARACTER SET charset_name]
;;         export_options
;;       | INTO DUMPFILE 'file_name'
;;       | INTO var_name [, var_name]]
;;     [FOR UPDATE | LOCK IN SHARE MODE]]


(defun* edbi-cc:preceding-string (&optional (count 1))
  "Return before COUNT character preceding point as String.
If COUNT is omitted, COUNT is set to 1.

don't throw error evan if point is at beggning of buffer."
  (buffer-substring-no-properties
   (point)
   (condition-case nil
       (save-excursion (backward-char count) (point))
     (error (point)))))

;; Todo: `table-name` style
(defun edbi-cc:get-info-for-complete-cur-point ()
  (with-syntax-table sql-mode-syntax-table
    (let* ((from (point))
           (start-point (save-excursion 
                          (skip-syntax-backward "w_")
                          (point)))
           (input (buffer-substring-no-properties (point) start-point))
           (dot-p (save-excursion 
                    (goto-char start-point)
                    (when (equal (edbi-cc:preceding-string 1) "`")
                      (forward-char -1))
                    (equal "." (edbi-cc:preceding-string 1)))))
      (cond
       (dot-p
        (save-excursion 

          (goto-char start-point)
          (forward-char -1)
          (when (looking-back "\\(`?\\(?:[a-zA-Z0-9_][_a-zA-Z0-9]*\\)`?\\)" (point-at-bol) t)
            (list 'dot input (match-string 0) start-point))))
       (t
        nil
        )))))

(defmacro edbi-cc:with-edbi-connection (&rest body)
  (declare (debug (body)))
  `(when (and edbi:dbview-buffer-name
              (buffer-live-p (get-buffer edbi:dbview-buffer-name)))
     (with-current-buffer edbi:dbview-buffer-name
       (when edbi:connection
         ,@body))))

(defun edbi-cc:get-columns-aux (conn table-name)
  (let ((re (rx-to-string `(seq  string-start ,table-name string-end)))
        (case-fold-search t))
    (imakado-dbind (_ _ _ ac-sources) conn
      (loop for column-info in (edbi:ac-candidates-columns ac-sources)
            for info = (imakado-dbind (faced-str . str) column-info
                         (imakado-aand (get-text-property 0 'summary faced-str)
                                       (string-match re it)
                                       column-info))
            when info
            collect info))))

(defun edbi-cc:get-columns (table-name)
  (interactive)
  (let ((conn edbi:connection)
        )
    (when conn
      (edbi-cc:get-columns-aux conn table-name))))


(defun edbi-cc:ac-editor-column-candidates-point-min-max ()
  ;; todo customizable
  (let ((min
         (save-excursion 
           (ignore-errors (forward-line -150))
           (point)))
        (max 
         (save-excursion 
           (ignore-errors (forward-line 200))
           (point))))
    (list min max)))

(defun edbi-cc:ac-editor-column-candidates ()
  (with-current-buffer ac-buffer
    (imakado-awhen (edbi-cc:get-info-for-complete-cur-point)
      (imakado-dbind (type _ table-name start-point) it
        (imakado-dbind (min max) (edbi-cc:ac-editor-column-candidates-point-min-max)
          (let* ((table-name (edbi-cc:sql-unquote table-name))
                 (re (rx-to-string `(seq string-start ,table-name string-end)))
                 (real-table-name
                  (imakado-aand
                   (imakado-aif (assoc-default
                                 re
                                 (edbi-cc:sql->table-alias&name
                                  (buffer-substring-no-properties min max)
                                 
                                  )
                                 (lambda (alias re)
                                   (string-match re alias)))
                       it
                     table-name))))
            (when real-table-name
              (edbi-cc:get-columns real-table-name))))))))


(defun edbi-cc:ac-completion-prefix ()
  (imakado-awhen (edbi-cc:get-info-for-complete-cur-point)
    (imakado-dbind (type _ table-name start-point) it
      start-point)))


(defvar edbi-cc:ac-source-columns
    '((candidates . edbi-cc:ac-editor-column-candidates)
      (candidate-face . edbi:face-ac-column-candidate-face)
      (selection-face . edbi:face-ac-column-selection-face)
      (symbol . "C")
      (prefix . edbi-cc:ac-completion-prefix)
      (requires . -1)
      ))

(defvar edbi-cc:edbi:columns-orig
    (quote ((candidates . edbi:ac-editor-column-candidates)
            (candidate-face . edbi:face-ac-column-candidate-face)
            (selection-face . edbi:face-ac-column-selection-face)
            (prefix . edbi-cc:ac-completion-prefix)
            (requires . -1)
            (symbol . "C"))))

(defun edbi-cc:ac-complete ()
  (interactive)
  (cond
   ((imakado-aand (edbi-cc:get-info-for-complete-cur-point)
                  (imakado-dbind (type _ table-name start-point) it
                    (eq type 'dot)))
    (let ((ac-sources '(edbi-cc:ac-source-columns)))
      (ac-start)))
   (t
    (let ((ac-sources '(ac-source-words-in-same-mode-buffers
                        ac-source-edbi:columns
                        )))
      (ac-start)))))

(defun edbi-cc:ac-dot-complete ()
  "Insert dot and complete code at point"
  (interactive)
  (insert ".")
  (edbi-cc:ac-complete))

;; (define-key edbi:sql-mode-map (kbd ".") 'edbi-cc:ac-dot-complete)


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (expect '(dot "hoge_id" "left_tbl" 10)
        (imakado-with-point-buffer "\
left_tbl.hoge_id`!!'"
          (with-syntax-table sql-mode-syntax-table
            (edbi-cc:get-info-for-complete-cur-point)))
          
        )
      (expect '(dot "" "left_tbl" 10)
        (imakado-with-point-buffer "\
left_tbl.`!!'"
          (with-syntax-table sql-mode-syntax-table
            (edbi-cc:get-info-for-complete-cur-point)))
        )
      (expect '(dot "" "`left_tbl`" 12)
        (imakado-with-point-buffer "\
`left_tbl`.`!!'"
          (with-syntax-table sql-mode-syntax-table
            (edbi-cc:get-info-for-complete-cur-point)))
        )
      )))


(defun edbi-cc:setup ()
  (interactive)
  (define-key edbi:sql-mode-map "." 'edbi-cc:ac-dot-complete))

(provide 'edbi-complete-columns)

;;; edbi-complete-columns.el ends here

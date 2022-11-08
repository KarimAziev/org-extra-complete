;;; org-extra-complete.el --- Configure extra complete -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-extra-complete
;; Keywords: convenience, outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This file configures operations with extra complete

;; Commands

;; M-x `org-extra-complete'
;;      Complete `org-mode' keywords and values with annotation.

;; M-x `org-extra-complete-src-headers-args'
;;      Complete `org-mode' keywords and values with annotation.

;;; Code:


(require 'org)
(require 'ox)
(require 'ox-html)

(declare-function imenu--make-index-alist "imenu")
(defvar imenu--index-alist)

(defun org-extra-complete-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun org-extra-complete-bounds-by-chars (chars)
  "Return bounds of thing at point if it is match CHARS.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun org-extra-complete-bounds-of-region-or-chars (chars)
  "Return bounds of active region or bounds of thing at point that match CHARS.

CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (org-extra-complete-bounds-by-chars chars)))

(defun org-extra-complete-get-bounds (&optional chars)
  "Return bounds of active region or bounds of thing at point that match CHARS.

CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (unless chars
    (setq chars (or chars
                    "-'*\"_~$A-Za-z0-9:.#\\+")))
  (org-extra-complete-bounds-of-region-or-chars chars))

(defun org-extra-complete-get-word (&optional chars)
  "Get thing at point matching CHARS.
Optional argument CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)"
  (when-let ((bounds (org-extra-complete-get-bounds chars)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun org-extra-complete-line-jump-to-line (line)
  "Jump to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun org-extra-complete-line-substring (&optional line)
  "Return the characters of part of the LINE, without the text properties."
  (when line
    (org-extra-complete-line-jump-to-line line))
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun org-extra-complete-file-permissions ()
  "Read permissions in octal format, with completions."
  (let ((prompts '("owner" "group" "others"))
        (permissions '("0 - no permissions"
                       "1 - e(x)ecute;"
                       "2 - (w)rite"
                       "3 - e(x)ecute, (w)rite"
                       "4 - (r)ead"
                       "5 - (r)ead, e(x)ecute"
                       "6 - (r)ead, (w)rite"
                       "7 - (r)ead, (w)rite, e(x)ecute"))
        (results))
    (dolist (prompt prompts)
      (let ((choice (completing-read (format "%s can\s" prompt)
                                     (reverse permissions))))
        (setq choice (car (split-string choice)))
        (push choice results)))
    (string-join (reverse results) "")))

(defvar org-extra-complete-browse-url-protocol-regex
  "\\(\\(http[s]?://\\(www\\.\\)?\\)\\|git@\\|file:/~?+\\)")

(defvar org-extra-complete-browse-url-regex
  (concat
   "\\("
   org-extra-complete-browse-url-protocol-regex
   "\\([a-z-0-9]+\\(\\(:[0-9]*\\)\\|\\.[a-z]+\\)+/?[a-z]?[^\];\s\t\n\r\f|]*[a-z-0-9]+\\)"
   "\\)")
  "Regexp with protocols.")

(defun org-extra-complete-search-links ()
  "Return list of urls in current buffer."
  (let ((links))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (or
                (re-search-forward org-link-any-re nil t 1)
                (re-search-forward org-extra-complete-browse-url-regex nil t 1))
          (when-let* ((link (match-string-no-properties 0))
                      (parsed
                       (if (string-match-p "^[[]\\{2\\}" link)
                           (with-temp-buffer
                             (let ((org-inhibit-startup nil))
                               (insert link)
                               (org-mode)
                               (goto-char (point-min))
                               (when-let ((props
                                           (cadr (org-element-link-parser))))
                                 (plist-get props :raw-link))))
                         link)))
            (push parsed links)))))
    links))

(defun org-extra-complete-url-get-urls ()
  "Return list of urls from minibuffer current buffer."
  (append
   (seq-filter (lambda (it) (and (string-match-p
                             "\\(^\\|[^a-zZ-A]\\)http[s]?://" it)
                            (null (string-match-p "[\n\r\f]" it))))
               (append (seq-copy kill-ring)
                       (seq-copy minibuffer-history)))
   (if (minibuffer-window-active-p (selected-window))
       (with-minibuffer-selected-window
         (org-extra-complete-search-links))
     (org-extra-complete-search-links))))

(defun org-extra-complete-get-region ()
  "Return current active region as string or nil."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defun org-extra-complete-strip-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun org-extra-complete-get-prop (item property)
  "Get PROPERTY from ITEM.
ITEM can be propertized string or plist."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

(defun org-extra-complete-stringify (x)
  "Convert X to string effeciently.
X can be any object."
  (cond
   ((stringp x)
    x)
   ((symbolp x)
    (symbol-name x))
   ((integerp x)
    (number-to-string x))
   ((floatp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun org-extra-complete-add-props (string &rest properties)
  "Propertize STRING with PROPERTIES."
  (setq string (org-extra-complete-stringify string))
  (let* ((result (list 'head))
         (last result))
    (while properties
      (let* ((key (pop properties))
             (val (pop properties))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (apply #'propertize string (cdr result))))

(defun org-extra-complete-insert (item &optional separator)
  "Insert or complete ITEM and SEPARATOR.
If word at point is prefix of ITEM, complete it, else insert ITEM.
Optional argument SEPARATOR is a string to insert just after ITEM.
Default value of SEPARATOR is space."
  (let ((parts))
    (setq parts
          (if-let ((current-word (org-extra-complete-get-word
                                  "-*_~$A-Za-z0-9:#\\+")))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties item (length current-word)))
                  (list (or separator "\s") item)))
            (list item)))
    (apply #'insert parts)))

(defun org-extra-complete-plist-pick (keywords plist)
  "Pick KEYWORDS from PLIST without nil."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let ((value (plist-get plist keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

(defun org-extra-complete-line-get-line-words (&optional line)
  "Return substrings of LINE, splitted by `split-string-default-separators'."
  (if line
      (split-string (org-extra-complete-line-substring line) nil t)
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (split-string (buffer-substring-no-properties beg end)))))

(defun org-extra-complete-line-empty-p (&optional line)
  "Return t if LINE or current line is empty."
  (= 0 (length (org-extra-complete-line-get-line-words line))))

(defun org-extra-complete-read-tag ()
  "Read org global tag with completion."
  (completing-read "Tag"
                   (org-global-tags-completion-table
                    (org-agenda-files))))

(defun org-extra-complete-read-tangle-mode-permissions ()
  "Complete value for :tangle-mode."
  (let ((perm (org-extra-complete-file-permissions)))
    (format "(identity #o%s)" perm)))

(defun org-extra-complete-read-dir ()
  "Read abbreviated directory name."
  (abbreviate-file-name
   (read-directory-name "Directory:\s")))

(defun org-extra-complete-read-file ()
  "Read abbreviated filename."
  (abbreviate-file-name (read-file-name "File:\s")))

(defun org-extra-complete-read-wrap ()
  "Read value for wrap."
  (completing-read
   "wrap\s"
   (append
    '("example" "src")
    (mapcar (apply-partially #'format "src %s")
            (mapcar #'car org-babel-load-languages)))))

(defun org-extra-complete-read-variable ()
  "Read value for :var."
  (let ((var (read-string "Variable:\s"))
        (value))
    (setq value (completing-read (format "%s=" var)
                                 (org-extra-complete-search-named-blocks)))
    (concat var "=" value)))

(defun org-extra-complete-get-all-libraries ()
  "Return a list of all files in `load-path'."
  (require 'find-func)
  (let* ((dirs (or find-library-source-path load-path))
         (suffixes (find-library-suffixes)))
    (read-library-name--find-files dirs suffixes)))

(defun org-extra-complete-get-all-languages ()
  "Return a list of all files in `load-path'."
  (require 'find-func)
  (let* ((non-org-libs
          (read-library-name--find-files
           (seq-remove
            (apply-partially #'string-suffix-p "org")
            load-path)
           (find-library-suffixes)))
         (ob-langs (mapcar
                    (lambda (s) (substring s 3))
                    (seq-filter
                     (lambda (lib) (and (string-prefix-p "ob-" lib)
                                   (not
                                    (string-suffix-p "-autoloads"
                                                     lib))))
                     non-org-libs))))
    (append
     ob-langs
     (mapcar 'symbol-name
             (delete-dups
              (append
               (mapcar #'car org-babel-load-languages)
               (mapcar (lambda (it) (car (reverse it)))
                       (cdr (nth 1 (memq :key-type
                                         (get 'org-babel-load-languages
                                              'custom-type)))))))))))

(defun org-extra-complete-read-language ()
  "Read babel language with completion."
  (completing-read "Language:\s" (org-extra-complete-get-all-languages)))

(defun org-extra-complete-read-property ()
  "Complete property keys in the current buffer."
  (completing-read
   "Property:\s"
   (append '("header-args")
           (org-buffer-property-keys nil t t))))

(defun org-extra-complete-read-link ()
  "Read url with completion and label.
Return string with label and url, divided with space."
  (let ((link (completing-read
               "Url\s" (org-extra-complete-url-get-urls) nil nil "https://"))
        (label (read-string "Label:\s")))
    (format "%s %s" label link)))

(defun org-extra-complete-goto-matching-closed-block (&optional keyword)
  "Jump to matching for KEYWORD closed block."
  (when-let ((type (downcase (replace-regexp-in-string
                              "^#\\+\\|:[\s\t\n]*$" ""
                              (or keyword
                                  (org-extra-complete-get-word))))))
    (let ((case-fold-search t))
      (let ((subtype (nth 1 (split-string type "_" t)))
            (beg (point)))
        (when (looking-at (concat "#\\+begin_" subtype))
          (forward-line 1))
        (let ((count 1)
              (exmaplep (equal subtype "example"))
              (case-fold-search t))
          (while (and (> count 0)
                      (re-search-forward "[^,]#\\+" nil t 1))
            (setq count (if (and
                             (not exmaplep)
                             (looking-at "begin_example"))
                            (1+ count)
                          (1- count))))
          (if (looking-at (concat "end_" subtype))
              (progn
                (skip-chars-forward "#-*\"'_~$A-Za-z0-9:.\\+")
                (point))
            (goto-char beg)
            nil))))))

(defun org-extra-complete-begin-src ()
  "Complete language and insert end_src if none."
  (let ((lang (org-extra-complete-read-language))
        (parts))
    (unless (save-excursion
              (org-extra-complete-goto-matching-closed-block "begin_src"))
      (push "\n#+end_src" parts))
    (when (looking-back "#\\+begin_src" 0)
      (push "\s" parts))
    (push (format "%s" lang) parts)
    (string-join parts "")))

(defun org-extra-complete-block-type (block-type)
  "Insert structured org template with BLOCK-TYPE."
  (let*((parts (split-string block-type nil t))
        (type (pop parts))
        (strings))
    (unless (save-excursion
              (org-extra-complete-goto-matching-closed-block
               (concat "begin_" type)))
      (push (concat "\n#+end_" type) strings))
    (when (looking-back (concat "#\\+begin_" type) 0)
      (push "\s" strings))
    (string-join strings "")))

(defun org-extra-complete-begin-export ()
  "Complete #+begin_export keyword."
  (let ((lang (completing-read "Lang\s" '("html" "latext" "beamer" "")))
        (parts))
    (unless (save-excursion
              (org-extra-complete-goto-matching-closed-block "begin_export"))
      (push "\n#+end_export" parts))
    (when (looking-back "#\\+begin_export" 0)
      (push "\s" parts))
    (push (format "%s" lang) parts)
    (string-join parts "")))

(defun org-extra-complete-search-named-blocks ()
  "Return block names from current buffer."
  (require 'org)
  (let ((names))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t))
       (while (re-search-forward "#\\+name:[\s\t]+\\([^\n]+\\)" nil t 1)
         (push (string-trim (match-string-no-properties 1)) names))))
    names))

(defun org-extra-complete-call ()
  "Return string with value for org babel call."
  (format "%s()" (completing-read "Call:\s"
                                  (org-extra-complete-search-named-blocks))))

(defvar org-extra-complete-html-headers
  '("<style> #content{max-width:1800px;}</style>"
    "<link rel=\"stylesheet\" href=\"mystyle.css\">"
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0>"
    "<script> console.log('')</script>"
    "<script type=\"module\" src=\"main.js\"></script>"
    "<script src=\"javascript.js\" type=\"text/javascript\" />"))

(defun org-extra-complete-babel-get-all-src-refs ()
  "Search for all src refs in current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((refs))
     (while (re-search-forward "(ref:\\([^\n]+\\))" nil t 1)
       (push (match-string-no-properties 1) refs))
     refs)))

(defun org-extra-complete-insert-ref-link ()
  "Complete org reference."
  (when-let ((ref (format "(%s)"
                          (completing-read
                           "Ref:\s"
                           (org-extra-complete-babel-get-all-src-refs)))))
    (let ((descr (read-string "Description:\s" (or
                                                (org-extra-complete-get-region)
                                                ""))))
      (when (and (region-active-p)
                 (use-region-p))
        (delete-active-region))
      (if (string-empty-p descr)
          (concat "[[" ref "]]")
        (concat "[[" ref "][" descr "]]")))))

(defvar org-extra-complete-mode-buffer-options-pl
  `((:id "|" :var "org-export-with-tables"
         :description "toggle ~org-export-with-tables~"
         :value boolean)
    (:id "html-style" :description "toggle ~org-html-head-include-default-style~" :value boolean)
    (:id "todo" :var "org-export-with-todo-keywords"
         :description "toggle inclusion of TODO keywords into exported text (~org-export-with-todo-keywords~)"
         :value boolean)
    (:id "toc" :var "org-export-with-toc"
         :description "toggle inclusion of the table of contents, or set the level limit  (~org-export-with-toc~)"
         :sublist
         ((:description "No Table of Contents" :value nil)
          (:description "Full Table of Contents" :value t)
          (:description "TOC to level" :value integer)))
    (:id "title" :var "org-export-with-title"
         :description "toggle inclusion of title (org-export-with-title)"
         :value boolean)
    (:id "timestamp" :var "org-export-time-stamp-file"
         :description "toggle inclusion of the creation time in the exported file  (~org-export-time-stamp-file~)"
         :value boolean)
    (:id "tex" :var "org-export-with-latex"
         :description "nil does not export; t exports; verbatim keeps everything in  verbatim (~org-export-with-latex~)"
         :sublist
         ((:description "Do not process math in any way"
                        :value nil)
          (:description "Interpret math snippets"
                        :value t)
          (:description "Leave math verbatim"
                        :value verbatim)))
    (:id "tasks" :var "org-export-with-tasks"
         :description "toggle inclusion of tasks (TODO items); or nil to remove all  tasks; or todo to remove done tasks; or list the keywords to keep  (~org-export-with-tasks~)"
         :sublist
         ((:description "All tasks" :value t)
          (:description "No tasks" :value nil)
          (:description "Not-done tasks" :value todo)
          (:description "Only done tasks" :value done)
          (:sublist
           (:description "Keyword"
                         :value string)
           :value nil)))
    (:id "tags" :var "org-export-with-tags"
         :description "toggle inclusion of tags, may also be not-in-toc (~org-export-with-tags~)"
         :sublist
         ((:description "Off"
                        :value nil)
          (:description "Not in TOC"
                        :value not-in-toc)
          (:description "On"
                        :value t)))
    (:id "stat"
         :var "org-export-with-statistics-cookies"
         :description "toggle inclusion of statistics cookies  (~org-export-with-statistics-cookies~)"
         :value boolean)
    (:id "prop" :var "org-export-with-properties"
         :description "toggle inclusion of property drawers, or list the properties to  include (~org-export-with-properties~)"
         :sublist
         ((:description "All properties" :value t)
          (:description "None" :value nil)
          (:sublist
           (:description "Property name" :value string)
           :value nil)))
    (:id "pri" :var "org-export-with-priority"
         :description "toggle inclusion of priority cookies  (~org-export-with-priority~)"
         :value boolean)
    (:id "p" :var "org-export-with-planning"
         :description "toggle export of planning information (~org-export-with-planning~)"
         :value boolean)
    (:id "num" :var "org-export-with-section-numbers"
         :description "#+cindex: @samp{UNNUMBERED}, property  toggle section-numbers (~org-export-with-section-numbers~)"
         :value boolean)
    (:id "inline" :var "org-export-with-inlinetasks"
         :description "toggle inclusion of inlinetasks (org-export-with-inlinetasks)"
         :value boolean)
    (:id "H" :var "org-export-headline-levels"
         :description "Set the number of headline levels for export  (~org-export-headline-levels~)"
         :value integer)
    (:id "f" :var "org-export-with-footnotes"
         :description "toggle the inclusion of footnotes (~org-export-with-footnotes~)"
         :value boolean)
    (:id "email" :var "org-export-with-email"
         :description "toggle inclusion of the author's e-mail into exported file  (~org-export-with-email~)"
         :value boolean)
    (:id "e" :var "org-export-with-entities"
         :description "toggle inclusion of entities (~org-export-with-entities~)"
         :value boolean)
    (:id "date" :var "org-export-with-date"
         :description "toggle inclusion of a date into exported file (~org-export-with-date~)"
         :value boolean)
    (:id "d" :var "org-export-with-drawers"
         :description "toggles inclusion of drawers, or list of drawers to include, or list of drawers to exclude (~org-export-with-drawers~)"
         :sublist
         ((:description "All drawers" :value t)
          (:description "None" :value nil)
          (:description "Drawer name" :id "(not \"LOGBOOK\")")
          (:description "Drawer name" :value string)))
    (:id "creator" :var "org-export-with-creator"
         :description "toggle inclusion of creator information in the exported file  (~org-export-with-creator~)"
         :value boolean)
    (:id "c" :var "org-export-with-clocks"
         :description "toggle inclusion of CLOCK keywords (~org-export-with-clocks~)"
         :value boolean)
    (:id "broken-links" :var "org-export-with-broken-links"
         :description "toggles if Org should continue exporting upon finding a broken  internal link"
         :sublist
         ((:description "Ignore broken links" :value t)
          (:description "Mark broken links in output" :value mark)
          (:description "Raise an error" :value nil)))
    (:id "author" :var "org-export-with-author"
         :description "toggle inclusion of author name into exported file  (~org-export-with-author~)"
         :value boolean)
    (:id "arch" :var "org-export-with-archived-trees"
         :description "Configure how archived trees are exported ~org-export-with-archived-trees~"
         :sublist
         ((:description "Not at all" :value nil)
          (:description "Headline only" :value headline)
          (:description "Entirely" :value t)))
    (:id "^"
         :var "org-export-with-sub-superscripts"
         :description "toggle TeX-like syntax for sub- and superscripts (~org-export-with-sub-superscripts~)"
         :sublist
         ((:description "Interpret them" :value t)
          (:description "Curly brackets only" :value {})
          (:description "Do not interpret them" :value nil)))
    (:id "\\n" :description "toggles whether to preserve line breaks  (~org-export-preserve-breaks~)"
         :value boolean)
    (:id "<" :var "org-export-with-timestamps"
         :description "toggle inclusion of time/date active/inactive stamps  (~org-export-with-timestamps~)"
         :sublist
         ((:description "All timestamps" :value t)
          (:description "Only active timestamps" :value active)
          (:description "Only inactive timestamps" :value inactive)
          (:description "No timestamp" :value nil)))
    (:id "-" :var "org-export-with-special-strings"
         :description "toggle conversion of special strings  (~org-export-with-special-strings~)"
         :value boolean)
    (:id "*" :var "org-export-with-emphasize"
         :description "toggle emphasized text (~org-export-with-emphasize~)"
         :value boolean)
    (:id "'" :var "org-export-with-smart-quotes"
         :description "toggle smart quotes (~org-export-with-smart-quotes~)"
         :value boolean)))

(defvar org-extra-complete-src-headers-args-plist
  `((:id ":results"
         :description "Get the lines of the example numbered"
         :multi t
         :sublist
         ((:id "file"
               :description
               "Name of the file to which output should be written"
               :group 0)
          (:id "list"
               :description "Writes an Org mode list"
               :group 0)
          (:id "vector"
               :description "The same as :results table"
               :group 0)
          (:id "table"
               :description "Interprets the results as an Org mode table"
               :group 0)
          (:id "scalar"
               :description "the same as :results verbatim"
               :group 0)
          (:id "verbatim"
               :description "A string to inhibit interpretation as a value"
               :group 0)
          (:id "raw"
               :description
               "Output is a string so hline processing is not performed"
               :group 1)
          (:id "html"
               :description
               "Results are added inside of a #+BEGIN_EXPORT HTML block"
               :group 1)
          (:id "latex"
               :description "The same as :wrap LaTeX"
               :group 1)
          (:id "org"
               :description
               "Wraps raw results in a #+begin_src org block (dead data,comma-escaped lines)"
               :group 1)
          (:id "code"
               :description
               "This will be exported as <LANG> code block (as verbatim or listings to LaTeX)."
               :group 1)
          (:id "pp"
               :description
               "Prints data. Puts the output in an EXAMPLE block? XX"
               :group 1)
          (:id "drawer"
               :description "Wraps code block results in a RESULTS drawer"
               :group 1)
          (:id "link"
               :description "Wraps code block results in a link"
               :group 1)
          (:id "graphics"
               :description "Wraps code block results in a graphics"
               :group 1)
          (:id "replace"
               :description
               "Inserts results after the code block, replacing any previously inserted results"
               :group 2)
          (:id "silent"
               :description "Echoes the results only in the minibuffer"
               :group 2)
          (:id "none"
               :description "No the results, even for the minibuffer"
               :group 2)
          (:id "append"
               :description "Builds new results onto existing results"
               :group 2)
          (:id "prepend"
               :description "Puts new results before the existing results"
               :group 2)
          (:id "output"
               :description "Everything printed to stdout"
               :group 3)
          (:id "value"
               :description "Value of the last statement"
               :group 3)))
    (:id "-n"
         :description "Get the lines of the example numbered"
         :sublist ,(apply-partially #'read-string "Line: "))
    (:id "+n"
         :description "the numbering from the previous numbered snippet"
         :sublist (lambda ()
                    (read-string "Line: ")))
    (:id "-r"
         :description "hide reference labels")
    (:id ":cache"
         :sublist
         ((:id "no")
          (:id "yes")))
    (:id ":colnames"
         :description "Handles column names in tables"
         :sublist ((:id "no"
                        :description
                        "Don't strip the header. Does not pre-process column names at all")
                   (:id "yes"
                        :description
                        "Tells Org Babel that your first row contains column names")
                   (:id "nil"
                        :description "Default names")
                   (:id "'()"
                        :description "Specifies to use LIST as column names")))
    (:id ":comments"
         :description "insertion of extra comments into the tangled files"
         :sublist ((:id "no")
                   (:id "link")
                   (:id "yes")
                   (:id "org")
                   (:id "both")
                   (:id "noweb")))
    (:id ":eval"
         :description "Specifies permissions for every execution of code blocks"
         :sublist
         ((:id "yes"
               :descsription "evaluate the source code")
          (:id "no"
               :descsription "never evaluate the source code")
          (:id "no-export"
               :description
               "Org does not evaluate the source code when exporting, yet the user can evaluate it interactively")
          (:id "query-export"
               :description
               "Org prompts the user for permission to evaluate the source code during export")
          (:id "query"
               :description
               "(~org-confirm-babel-evaluate~) - confirm before evaluating both interactively and during export")))
    (:id ":exports"
         :description
         "Specifies how code and/or results should be handled during export"
         :sublist ((:id "code"
                        :description
                        "Includes the body of the code block into the exported file")
                   (:id "results"
                        :description
                        "Includes the results block in the exported file")
                   (:id "both"
                        :description
                        "Includes both the results block in the exported file")))
    (:id ":hlines"
         :description "Handles horizontal lines in input tables"
         :sublist
         ((:id "no"
               :description "Strips horizontal lines from the input table")
          (:id "yes"
               :description "Preserves horizontal lines in the input table")))
    (:id ":mkdirp"
         :description
         "Toggles creation of parent directories of target files during tangling"
         :sublist ((:id "yes")
                   (:id "no")))
    (:id ":padline"
         :description
         "Controls insertion of padding lines in tangled code files"
         :sublist
         ((:id "yes"
               :description "default")
          (:id "no"
               :description
               "Gets rid of the first blank line preceding tangled output")))
    (:id ":noweb"
         :description
         "Specifies when expansion of noweb style references should occur"
         :sublist ((:id "no"
                        :description "No expand")
                   (:id "yes"
                        :description "during both tangling and evaluation")
                   (:id "tangle"
                        :description "only during tangling")
                   (:id "no-export"
                        :description
                        "during tangling and interactive evaluation")
                   (:id "strip-export"
                        :description
                        "Expands before the block is tangled or evaluated but strip on export")
                   (:id "eval"
                        :description "before evaluating")))
    (:id ":rownames"
         :description "Handles row names in tables"
         :sublist ((:id "yes"
                        "Tells Org that your first column contains row names")
                   (:id "no" "No rownames")))
    (:id ":tangle"
         :description "Toggles tangling and specify file name"
         :sublist ((:id "tangle")
                   (:id "yes")
                   (:id "no")))
    (:id ":tangle-mode"
         :description "what permissions to set for tangled files"
         :sublist org-extra-complete-read-tangle-mode-permissions)
    (:id ":cmdline"
         :description
         "For shell, this allows to make the code inside a Babel code block similar to a real shell script")
    (:id ":no-expand"
         :description "Turns off the code blocks expansion during tangling")
    (:id ":noeval"
         :description "Is the same as ~:eval no~")
    (:id ":noweb-ref"
         :description "Specifies block's noweb reference resolution target")
    (:id ":noweb-prefix"
         :description
         "Prevent the prefix characters from being repeated when expanding a multiline noweb reference"
         :sublist ("no" "yes" "default"))
    (:id ":noweb-sep"
         :description
         "Specifies the string to use to separate accumulated noweb references"
         :sublist ,(apply-partially #'read-directory-name "Output directory:\s"))
    (:id ":output-dir"
         :description
         "If ‘output-dir’ is not specified, Org assumes it is the current directory"
         :sublist ,(apply-partially #'read-directory-name "Output directory:\s"))
    (:id ":post"
         :description "*Post-processes* the *results* of a code block")
    (:id ":mkdirp"
         :description
         "Toggles creation of parent directories of target files during tangling"
         :sublist ((:id "yes")
                   (:id "no")))
    (:id ":prologue"
         :description "Prepends text to code block body")
    (:id ":sep"
         :description
         "Specifies a delimiter for reading or writing tabular results")
    (:id ":session"
         :description
         "Shares data and persists state between (evaluation of) different code blocks")
    (:id ":shebang"
         :description "Make executable"
         :sublist ((:id "#!/bin/bash")
                   (:id "#!/usr/bin/env node")))
    (:id ":var"
         :description "*Passes arguments* to code blocks"
         :sublist org-extra-complete-read-variable)
    (:id ":wrap"
         :description "Delimit the results (of source block evaluation)"
         :sublist org-extra-complete-read-wrap)
    (:id ":dir"
         :description
         "Specifies the default directory during code block execution"
         :sublist org-extra-complete-read-dir)
    (:id ":file" "Specifies to write the results to a file" :sublist
         org-extra-complete-read-file)
    (:id ":file-desc"
         :description "Specifies a description for file results")
    (:id ":epilogue"
         :description "Appends text to code block body")
    (:id ":file-ext"
         :description "File extension")))

(defvar org-extra-complete-buffer-settings-plists
  `((:id "ARCHIVE" :description "archive location of the agenda file (~org-archive-location~)")
    (:id "CATEGORY" :description "category of the agenda file, which applies to the entire document")
    (:id "COLUMNS" :description "default format for columns view")
    (:id "CONSTANTS" :description "constants for table formulas ~org-table-formula-constants-local~ ~org-table-formula-constants~")
    (:id "FILETAGS" :description "tags that all entries in the file inherit from, including the top-level entries"
         :sublist org-extra-complete-read-tag)
    (:id "LINK" :description "Each line specifies one abbreviation for is ~org-link-abbrev-alist~."
         :sublist org-extra-complete-read-link)
    (:id "PRIORITIES" :description "sets limits and the default for the priorities. All three must be either letters A--Z or numbers 0--9.  The highest priority must have a lower ASCII number than the lowest priority")
    (:id "PROPERTY" :description "sets a default inheritance value for entries in the current buffer, most useful for specifying the allowed values of a property"
         :sublist org-extra-complete-read-property)
    (:id "SETUPFILE" :description "The setup file or a URL pointing to such file for additional in-buffer settings"
         :sublist org-extra-complete-read-file)
    (:id "TAGS" :description "~org-tag-alist~ specify the valid tags in this file"
         :sublist org-extra-complete-read-tag)
    (:id "LANGUAGE" :description "(~org-export-default-language~)")
    (:id "TODO" :description "~org-todo-keywords-1~ set the TODO keywords and their interpretation in the current file")
    (:id "STARTUP" :description "Startup options Org uses when first visiting a file"
         :sublist ((:id "overview"	:description "Top-level headlines only." :group 0)
                   (:id "content" :description "All headlines." :group 0)
                   (:id "showall" :description "No folding on any entry." :group 0)
                   (:id "show2levels" :description "Headline levels 1-2." :group 0)
                   (:id "show3levels" :description "Headline levels 1-3." :group 0)
                   (:id "show4levels" :description "Headline levels 1-4." :group 0)
                   (:id "show5levels" :description "Headline levels 1-5." :group 0)
                   (:id "showeverything" :description "Show even drawer contents." :group 0)
                   (:id "indent" :description "Start with Org Indent mode turned on." :group 1)
                   (:id "noindent" :description "Start with Org Indent mode turned off." :group 1)
                   (:id "num" :description "~org-startup-numerated~ Start with Org num mode turned on." :group 2)
                   (:id "nonum" :description "~org-startup-numerated~ Start with Org num mode turned off." :group 2)
                   (:id "align" :description "~org-startup-align-all-tables~ Align all tables." :group 3)
                   (:id "noalign" :description "~org-startup-align-all-tables~ Do not align tables on startup." :group 3)
                   (:id "inlineimages" :description "~org-startup-with-inline-images~ Show inline images." :group 4)
                   (:id "noinlineimages" :description "~org-startup-with-inline-images~ Do not show inline images on startup." :group 4)
                   (:id "logdone" :description "~org-log-done~ Record a timestamp when an item is marked as done." :group 5)
                   (:id "lognotedone" :description "Record timestamp and a note when DONE." :group 5)
                   (:id "nologdone" :description "Do not record when items are marked as done." :group 5)
                   (:id "logrepeat" :description "~org-log-repeat~ Record a time when reinstating a repeating item." :group 5)
                   (:id "lognoterepeat" :description "Record a note when reinstating a repeating item." :group 5)
                   (:id "nologrepeat" :description "Do not record when reinstating repeating item." :group 5)
                   (:id "lognoteclock-out" :description "~org-log-note-clock-out~ Record a note when clocking out." :group 5)
                   (:id "nolognoteclock-out" :description "Do not record a note when clocking out." :group 5)
                   (:id "logreschedule" :description "Record a timestamp when scheduling time changes." :group 5)
                   (:id "lognotereschedule" :description "Record a note when scheduling time changes." :group 5)
                   (:id "nologreschedule" :description "Do not record when a scheduling date changes." :group 5)
                   (:id "logredeadline" :description "Record a timestamp when deadline changes." :group 5)
                   (:id "lognoteredeadline" :description "Record a note when deadline changes." :group 5)
                   (:id "nologredeadline" :description "Do not record when a deadline date changes." :group 5)
                   (:id "logrefile" :description "Record a timestamp when refiling." :group 5)
                   (:id "lognoterefile" :description "Record a note when refiling." :group 5)
                   (:id "nologrefile" :description "Do not record when refiling." :group 5)
                   (:id "hidestars" :description "~org-hide-leading-stars~ Make all but one of the stars starting a headline invisible." :group 6)
                   (:id "showstars" :description "Show all stars starting a headline." :group 6)
                   (:id "indent" :description "Virtual indentation according to outline level." :group 6)
                   (:id "noindent" :description "No virtual indentation according to outline level." :group 6)
                   (:id "odd" :description "~org-odd-levels-only~ Allow only odd outline levels (1, 3, …)." :group 6)
                   (:id "oddeven" :description "Allow all outline levels." :group 6)
                   (:id "customtime" :description "~org-put-time-stamp-overlays~ ~org-time-stamp-overlay-formats~ Overlay custom time format." :group 7)
                   (:id "constcgs" :description "‘constants.el’ should use the c-g-s unit system." :group 8)
                   (:id "constSI" :description "‘constants.el’ should use the SI unit system." :group 8)
                   (:id "fninline" :description "~org-footnote-define-inline~ Define footnotes inline." :group 9)
                   (:id "fnnoinline" :description "Define footnotes in separate section." :group 9)
                   (:id "fnlocal" :description "Define footnotes near first reference, but not inline." :group 9)
                   (:id "fnprompt" :description "Prompt for footnote labels." :group 9)
                   (:id "fnauto" :description "Create ‘[fn:1]’-like labels automatically (default)." :group 9)
                   (:id "fnconfirm" :description "Offer automatic label for editing or confirmation." :group 9)
                   (:id "fnadjust" :description "Automatically renumber and sort footnotes." :group 9)
                   (:id "nofnadjust" :description "Do not renumber and sort automatically." :group 9)
                   (:id "hideblocks" :description "Hide all begin/end blocks on startup." :group 10)
                   (:id "nohideblocks" :description "Do not hide blocks on startup." :group 10)
                   (:id "entitiespretty" :description "Show entities as UTF-8 characters where possible." :group 11)
                   (:id "entitiesplain" :description "Leave entities plain." :group 11)))))

(defvar org-extra-complete-export-settings-plists
  `((:id "AUTHOR"
         :description "The document author (~user-full-name~)"
         :sublist ,(mapcar (lambda (id) `(:id ,id))
                           (delete nil (append
                                        (list user-full-name)
                                        (list user-mail-address)))))
    (:id "INFOJS_OPT"
         :description "~org-html-infojs-options~"
         :separator ":"
         :sublist ((:id "view"
                        :description "Initial view"
                        :sublist ((:id "showall"
                                       :description
                                       "Folding interface, all headlines and text visible")
                                  (:id "content"
                                       :description
                                       "Folding interface, starting with all headlines visible ")
                                  (:id "overview"
                                       :description
                                       "Folding interface, initially showing only top")
                                  (:id "info"
                                       :description
                                       "Info-like interface with one section per page")
                                  (:id "nil"
                                       :description "None")))
                   (:id "sdepth"
                        :description
                        "Maximum headline level as an independent section for info and folding modes"
                        :sublist ((:id "1"
                                       :description "1 level")
                                  (:id "2"
                                       :description "2 level")
                                  (:id "3"
                                       :description "3 level")
                                  (:id "3"
                                       :description "3 level")
                                  (:id "4"
                                       :description "4 level")
                                  (:id "5"
                                       :description "5 level")
                                  (:id "6"
                                       :description "6 level")
                                  (:id "7"
                                       :description "7 level")
                                  (:id "max"
                                       :description "Maximum level")))
                   (:id "toc"
                        :description "show table of contents"
                        :sublist ((:id "t"
                                       :description "show table of contents")
                                  (:id "nil"
                                       :description "hide table of contents")))
                   (:id "tdepth"
                        :description
                        "depth of the table of contents ~org-export-headline-levels~"
                        :sublist ((:id "1"
                                       :description "1 level")
                                  (:id "2"
                                       :description "2 level")
                                  (:id "3"
                                       :description "3 level")
                                  (:id "max"
                                       :description "Maximum level")))
                   (:id "ftoc"
                        :description "display toc as a fixed section"
                        :sublist ((:id "t"
                                       :description
                                       "display toc as a fixed section")))
                   (:id "ltoc"
                        :description "short contents in each section"
                        :sublist ((:id "above"
                                       :description
                                       "section should be above initial text")
                                  (:id "nil"
                                       :description "none")
                                  (:id "t"
                                       :description "yes")
                                  (:id ""
                                       :description "number"
                                       :value integer)))
                   (:id "mouse"
                        :description
                        "Higlhight color for headings on mouse over"
                        :sublist km-read-web-hex-color)
                   (:id "buttons"
                        :description "should view-toggle buttons be everywhere"
                        :sublist ((:id "nil"
                                       :description "only one button")
                                  (:id "0")
                                  (:id "1")))
                   (:id "path"
                        :description "The path to the script"
                        :sublist ((:id
                                   "https://orgmode.org/worg/code/org-info-js/org-info.js")
                                  (:id "https://orgmodeorg/org-infojs")))))
    (:id "DESCRIPTION"
         :description "A HTML meta tag in the HTML file"
         :value string)
    (:id "HTML_DOCTYPE"
         :description "Specify the document type, for example: HTML5."
         :sublist (,(when (boundp 'org-html-doctype-alist)
                      (mapcar #'car
                              (when org-html-doctype-alist)))))
    (:id "HTML_CONTAINER"
         :description
         "HTML container for wrapping sections and elements (~org-html-container-element~)"
         :sublist ("article" "aside"
                   "audio" "canvas" "details"
                   "figcaption" "figure" "footer"
                   "header" "menu" "meter"
                   "nav" "output" "progress"
                   "section" "summary"
                   "video"))
    (:id "HTML_LINK_HOME"
         :description "The URL for home link (~org-html-link-home~)."
         :sublist (,org-html-link-home))
    (:id "HTML_LINK_UP"
         :description
         "The URL for the up link of exported HTML pages (~org-html-link-up~)"
         :sublist (,org-html-link-up))
    (:id "HTML_HEAD_EXTRA"
         :description
         "More arbitrary lines for appending to the HTML document's head (~org-html-head-extra~)"
         :sublist (,org-extra-complete-html-headers))
    (:id "HTML_HEAD"
         :description
         "Arbitrary lines for appending to the HTML document's head (org-html-head)"
         :sublist (,org-extra-complete-html-headers))
    (:id "ATTR_HTML"
         :description
         "More arbitrary lines for appending to the HTML document's head (~org-html-head-extra~)"
         :sublist (,org-extra-complete-html-headers))
    (:id "KEYWORDS"
         :description "HTML meta tags")
    (:id "HTML_MATHJAX"
         :description "Options for MathJax (~org-html-mathjax-options~)")
    (:id "LATEX_HEADER"
         :description "Arbitrary lines for appending to the preamble")
    (:id "TITLE"
         :description "The document's title")
    (:id "SUBTITLE"
         :description "The document's subtitle.")
    (:id "CREATOR"
         :description
         "Entity responsible for output generation (~org-export-creator-string~)"
         :sublist (,org-export-creator-string))
    (:id "DATE"
         :description "A date or a time-stamp[fn:120]")
    (:id "EMAIL"
         :description "The email address (~user-mail-address~)"
         :sublist (,user-mail-address))
    (:id "OPTIONS"
         :separator ":"
         :sublist
         (,@org-extra-complete-mode-buffer-options-pl)
         :description "Compact form of export options")))

(defun org-extra-complete-forward-org-keywords (&optional n)
  "Move lines begins with org keyword forward or backward if N is negative."
  (interactive "P")
  (beginning-of-line)
  (while (looking-at "#+")
    (forward-line (if n -1 1))))

(defun org-extra-complete-elisp-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun org-extra-complete-elisp-src-name ()
  "Jump to place where to insert new definition."
  (goto-char (point-min))
  (org-extra-complete-move-with 'forward-sexp)
  (org-extra-complete-move-with 'backward-list)
  (let ((prev-pos))
    (while (and (or (null prev-pos)
                    (< prev-pos (point)))
                (when-let ((sexp (sexp-at-point)))
                  (and
                   (listp sexp)
                   (memq (car sexp)
                         '(eval-and-compile
                            require
                            eval-when-compile
                            declare-function)))))
      (setq prev-pos (progn
                       (org-extra-complete-move-with 'forward-sexp 2)
                       (org-extra-complete-move-with 'backward-list)))))
  (when-let ((sexp (sexp-at-point)))
    (when (and
           (listp sexp)
           (memq (car sexp)
                 '(eval-and-compile
                    require
                    eval-when-compile
                    declare-function)))
      (org-extra-complete-move-with 'forward-sexp 2))
    (if (memq (car sexp)
              '(defun defmacro defsubst define-inline define-advice defadvice
                      define-skeleton define-compilation-mode define-minor-mode
                      define-global-minor-mode define-globalized-minor-mode
                      define-derived-mode define-generic-mode ert-deftest
                      cl-defun cl-defsubst cl-defmacro cl-define-compiler-macro
                      cl-defgeneric cl-defmethod define-compiler-macro
                      define-modify-macro defsetf define-setf-expander
                      define-method-combination defalias cl-flet defun-ivy-read
                      defhydra defgeneric defmethod defun-ivy+ defgroup deftheme
                      define-widget define-error defface cl-deftype cl-defstruct
                      deftype defstruct define-condition defpackage defclass))
        (when (symbolp (nth 1 sexp))
          (org-extra-complete-elisp-unquote (car (flatten-list (seq-drop sexp 1))))))))

(defun org-extra-complete-name ()
  "Complete name for src block."
  (let ((suggestion (save-excursion
                      (when (looking-at "[\s\t]?+\n#\\+")
                        (forward-line)
                        (org-extra-complete-forward-org-keywords)
                        (when-let* ((params
                                     (org-extra-complete-src-block-params))
                                    (src-mode (org-src-get-lang-mode
                                               (car params)))
                                    (code
                                     (buffer-substring-no-properties
                                      (nth 1 params)
                                      (nth 2 params))))
                          (with-temp-buffer
                            (require 'imenu)
                            (save-excursion
                              (funcall src-mode)
                              (insert code)
                              (let ((re (org-babel-noweb-wrap)))
                                (while (re-search-backward re nil t 1)
                                  (let ((beg (match-beginning 0))
                                        (end (match-end 0)))
                                    (delete-region beg end))))
                              (let ((extra
                                     (progn
                                       (pcase src-mode
                                         ('emacs-lisp-mode
                                          (when-let
                                              ((sym
                                                (org-extra-complete-elisp-src-name)))
                                            (when (symbolp sym)
                                              (symbol-name sym))))))))
                                (ignore-errors (imenu--make-index-alist t))
                                (if (and imenu--index-alist
                                         extra)
                                    (append (list extra) imenu--index-alist)
                                  (or imenu--index-alist extra))))))))))
    (if (and suggestion (listp suggestion))
        (completing-read "NAME\s" suggestion)
      (read-string "NAME\s" suggestion))))

(defvar org-extra-complete-mode-completions-misc-plist
  `(,@(mapcar (lambda (it)
                (list :id (concat "begin_" (cdr it))
                      :description (concat "begin_" (cdr it))
                      :sublist (lambda () (org-extra-complete-block-type (cdr it)))))
              org-structure-template-alist)
    (:id "html"
         :description "html block"
         :sublist (lambda ()
                    (read-string "Html ")))
    (:id "caption"
         :description "Caption"
         :sublist (lambda ()
                    (read-string "CAPTION ")))
    (:id "name"
         :description "Name"
         :sublist org-extra-complete-name)
    (:id "call"
         :description "Call"
         :sublist org-extra-complete-call)
    (:id "(ref)"
         :description "Ref"
         :sublist org-extra-complete-insert-ref-link)
    (:id "include"
         :description "Include content of file"
         :sublist org-extra-complete-read-file)
    (:id "begin_export"
         :description "export block"
         :sublist org-extra-complete-begin-export)
    (:id "begin_src"
         :descriptnion "Code block"
         :sublist org-extra-complete-begin-src)))

(defvar org-extra-complete-completions-plist-vars
  '(org-extra-complete-export-settings-plists
    org-extra-complete-buffer-settings-plists
    org-extra-complete-mode-completions-misc-plist))

(defun org-extra-complete-get-all-plists ()
  "Merge plists defined in `org-extra-complete-completions-plist-vars'."
  (let ((plists))
    (dolist (pl org-extra-complete-completions-plist-vars)
      (setq plists (append plists (symbol-value pl))))
    (reverse plists)))

(defun org-extra-complete-get-completions-alist ()
	"Org complete get completions alist."
  (org-extra-complete-map-plist-completions-to-alist
   (org-extra-complete-get-all-plists)))

(defun org-extra-complete-src-block-params ()
  "If point is inside body of src block return list - (LANGUAGE BEGINNING END)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t))
        (unless (save-excursion
                  (beginning-of-line)
                  (re-search-forward "#\\+\\(begin\\)_src\\($\\|[\s\f\t\n\r\v]\\)"
                                     (line-end-position)
                                     t 1))
          (when (re-search-forward "#\\+\\(begin\\|end\\)_src\\($\\|[\s\f\t\n\r\v]\\)" nil t 1)
            (when-let ((word (match-string-no-properties 1))
                       (end (match-beginning 0)))
              (setq word (downcase word))
              (when (string= word "end")
                (when (re-search-backward "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*" nil t 1)
                  (let ((lang (match-string-no-properties 2)))
                    (forward-line 1)
                    (list lang (point) end)))))))))))

(defun org-extra-complete-make-code-ref ()
  "Read string and insert reference."
  (let ((name (read-string "(ref:)")))
    (insert (format "/* (ref:%s) */" name))))

(defun org-extra-complete-inside-src-code-p ()
  "Return t if point located inside body of src block."
  (when-let ((info (org-extra-complete-src-block-params))
             (pos (point)))
    (and (>= pos (nth 1 info))
         (<= pos (nth 2 info)))))

(defun org-extra-complete-map-values (parts &optional separator)
  "If PARTS is a list, concat it with SEPARATOR.
If PARTS is function, call it without args and return value.
In other cases return PARTS."
  (cond ((and (listp parts)
              (not (functionp parts)))
         (mapconcat
          (lambda (it) (if (and it (listp it))
                      (string-join (flatten-list it) "")
                    (format "%s" it)))
          parts (or separator "\s")))
        ((functionp parts)
         (funcall parts))
        (t parts)))

(defun org-extra-complete-option-variants (it items)
  "Find IT in ITEMS."
  (let ((tag (org-extra-complete-mode-trim-keyword it)))
    (seq-find (lambda (cell)
                (if (stringp cell)
                    (org-extra-complete-strip-props cell)
                  (equal tag (org-extra-complete-strip-props (car cell)))))
              items)))

(defun org-extra-complete-find-keyword-value (it items)
  "Find IT in ITEMS."
  (let ((tag (org-extra-complete-strip-props it)))
    (seq-find (lambda (cell)
                (if (stringp cell)
                    (org-extra-complete-strip-props cell)
                  (equal tag (org-extra-complete-strip-props (car cell)))))
              items)))

(defun org-extra-complete-eval-string (str)
  "Read and evaluate all forms in STR.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case nil
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret)))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))

(defun org-extra-complete-map-plist-completions-to-alist (&optional
                                                          plists-lists)
  "Map PLISTS-LISTS to alist of propertized strings."
  (if (and (listp plists-lists)
           (plist-get plists-lists :value))
      (format "%s" (plist-get plists-lists :value))
    (mapcar
     (lambda (pl)
       (if (stringp pl)
           pl
         (let*
             ((keywords (seq-filter #'keywordp pl))
              (id (or (plist-get pl :id)
                      (if (member :value keywords)
                          (org-extra-complete-stringify
                           (plist-get pl :value))
                        pl)))
              (description (plist-get pl :description))
              (sublist
               (if (functionp (plist-get pl :sublist))
                   (plist-get pl :sublist)
                 (delete nil
                         (org-extra-complete-map-plist-completions-to-alist
                          (plist-get pl :sublist)))))
              (item (if description
                        (org-extra-complete-add-props id :description description)
                      id))
              (rest (org-extra-complete-plist-pick
                     (seq-difference keywords '(:value :sublist :id))
                     pl)))
           (setq item (apply #'org-extra-complete-add-props id rest))
           (pcase (plist-get pl :value)
             ('boolean (cons item `("t" "nil")))
             ('string
              `(,item . (lambda () (format "%s"
                                      (read-string
                                       ,(format "%s (string): " id))))))
             ('integer `(,item . (lambda ()
                                   (format
                                    "%s" (read-number
                                          ,(format
                                            "%s (number):" id))))))
             (_ (cons item sublist))))))
     plists-lists)))

(defvar org-extra-complete-completion-collection nil)

(defun org-extra-complete-completing-read (prompt collection &optional predicate
                                               require-match
                                               initial-input
                                               hist
                                               def
                                               inherit-input-method)
  "Read a string in the minibuffer, with annotated completions.
COLLECTION should be a list of propertized strings. See
`org-extra-complete-src-complete-display-fn'.
PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, INHERIT-INPUT-METHOD
are the same as for `completing-read'."
  (completing-read
   prompt
   (lambda (str pred action)
     (if (eq action
             'metadata)
         `(metadata
           (annotation-function .
                                org-extra-complete-src-complete-display-fn))
       (complete-with-action action collection
                             str pred)))
   predicate
   require-match
   initial-input
   hist
   def
   inherit-input-method))

(defun org-extra-complete-preselect (prompt item)
  "If ITEM is function, call it.
If ITEM is list, read a string with completion and PROMPT.
If ITEM is string, return it."
  (cond ((and item (functionp item))
         (funcall item))
        ((and item (stringp item))
         item)
        ((and item (listp item))
         (org-extra-complete-completing-read prompt item))))

(defun org-extra-complete-src-complete-display-fn (item)
  "Transform ITEM for displaying."
  (let* ((group (org-extra-complete-get-prop item :group))
         (description (or (org-extra-complete-get-prop item :description)
                          (and group (format "%s" group))))
         (face (when (numberp group)
                 (nth group
                      '(file-name-shadow
                        font-lock-keyword-face
                        font-lock-constant-face
                        font-lock-regexp-grouping-construct
                        font-lock-variable-name-face
                        font-lock-type-face)))))
    (if description
        (concat
         " "
         (with-temp-buffer
           (insert (if face
                       (propertize description 'face face)
                     description))
           (while (re-search-backward "~\\([^~]+\\)~" nil t 1)
             (let ((beg (match-beginning 0))
                   (end (match-end 0))
                   (var (string-trim (match-string-no-properties 1)))
                   (value))
               (setq value (if (string-match-p "^[(]" var)
                               (car (org-extra-complete-eval-string var))
                             (let ((sym (intern var)))
                               (when (boundp sym)
                                 (if-let ((minw
                                           (minibuffer-selected-window)))
                                     (with-selected-window minw
                                       (buffer-local-value
                                        sym
                                        (current-buffer)))
                                   (symbol-value sym))))))
               (setq value (if (listp value)
                               (setq value (mapconcat
                                            (apply-partially #'format "%s")
                                            (flatten-list value)
                                            "\s"))
                             (format "%s" value)))
               (replace-region-contents beg
                                        end
                                        (lambda ()
                                          (concat
                                           ""
                                           (propertize
                                            var
                                            'face
                                            'font-lock-variable-name-face)
                                           ": "
                                           (propertize
                                            value
                                            'face
                                            'font-lock-builtin-face)
                                           "")))))
           (buffer-string)))
      "")))

(defun org-extra-complete-alist (&optional alist prompt)
  "Complete ALIST with PROMPT."
  (let ((current)
        (result)
        (count 0))
    (if (functionp alist)
        (funcall alist)
      (while (and alist
                  (not (keywordp alist))
                  (or (listp alist)
                      (vectorp alist))
                  (not (functionp alist)))
        (when (vectorp alist)
          (setq alist
                (seq-map-indexed (lambda (it i) (cons (format "%s" i) it))
                                 (append alist nil))))
        (setq count (1+ count))
        (setq prompt (concat
                      (string-join
                       (delete nil
                               (append (list prompt)
                                       (if (= 1 (% count 2))
                                           (reverse result)
                                         result)))
                       "\s")
                      "\s"))
        (setq current (org-extra-complete-completing-read
                       prompt
                       alist))
        (push current result)
        (setq alist (cdr-safe (assoc current alist)))
        (when (functionp alist)
          (let ((value (funcall alist)))
            (if (and (listp value)
                     (not (listp (car value))))
                (setq alist value)
              (push value result)
              (setq alist nil))))
        alist)
      (reverse result))))

(defun org-extra-complete-mode-trim-keyword (word)
  "Remove whitespaces, # and + chars form WORD."
  (replace-regexp-in-string "^#\\+\\|:[\s\t\n]*$" "" word))

(defun org-extra-complete-normalize-keyword (key &optional separator)
  "Normalize KEY to org keyword and concate it with SEPARATOR.
Default value for separator is `:\s'."
  (setq key (org-extra-complete-strip-props key))
  (unless (string-match-p "\\(begin\\|end\\)_\\|(ref)\\|\\(:[\s\t]*$\\)" key)
    (setq key (concat key (or separator ":\s")))
    (unless (string-match-p "#\\+" key)
      (setq key (concat "#+" key))))
  key)

(defun org-extra-complete-keyword (items)
  "Complete keywords from ITEMS."
  (let* ((init-word (buffer-substring-no-properties
                     (line-beginning-position)
                     (point)))
         (keyword (and
                   init-word
                   (string-match-p "^#\\+" init-word)
                   (org-extra-complete-mode-trim-keyword init-word))))
    (cond ((seq-find (lambda (it)
                       (equal (org-extra-complete-strip-props
                               (if (listp it)
                                   (car it)
                                 it))
                              keyword))
                     items)
           (org-extra-complete-insert
            (concat ":\s"
                    (or
                     (org-extra-complete-map-values
                      (org-extra-complete-alist
                       (cdr (assoc keyword items)))
                      (org-extra-complete-get-prop keyword :separator))
                     ""))))
          ((looking-back "#\\+\\([a-z-_:]+\\)[\s\t]*" 0)
           (let ((prefix (org-extra-complete-mode-trim-keyword
                          init-word))
                 (parts)
                 (key)
                 (rest)
                 (separator))
             (setq parts (org-extra-complete-alist
                          (seq-filter
                           (lambda (it)
                             (when-let ((str (if (listp it)
                                                 (car it)
                                               it)))
                               (string-prefix-p prefix str)))
                           items)))
             (setq key (pop parts))
             (setq separator (org-extra-complete-get-prop key :separator))
             (setq key (if (equal key "(ref)")
                           (org-extra-complete-mode-trim-keyword key)
                         (concat "#+" (org-extra-complete-mode-trim-keyword key)
                                 ": ")))
             (setq rest (or (org-extra-complete-map-values parts separator) ""))
             (skip-chars-backward "\s\t")
             (org-extra-complete-insert (concat key rest))))
          (t (org-extra-complete-insert (org-extra-complete-insert-ref-link))))))

;;;###autoload
(defun org-extra-complete-src-headers-args ()
  "Complete a header argument selecting from lists of common args and values."
  (interactive)
  (setq org-extra-complete-completion-collection
        (org-extra-complete-map-plist-completions-to-alist
         org-extra-complete-src-headers-args-plist))
  (let ((case-fold-search t))
    (cond
     ((looking-back "\\(#\\+begin_src\\)[\s\t]*" 0)
      (insert (org-extra-complete-begin-src)))
     ((looking-back "\\(:[a-z]+\\)[\s\t]+" 0)
      (if-let* ((preselect (match-string-no-properties 1))
                (value (cdr (org-extra-complete-find-keyword-value
                             preselect
                             org-extra-complete-completion-collection))))
          (insert (org-extra-complete-preselect preselect value))
        (insert (read-string (format "Value for %s:\s" preselect)))))
     ((looking-back ":\\([a-z]*\\)" 0)
      (when-let* ((prefix (match-string-no-properties 0))
                  (value (all-completions
                          prefix (mapcar
                                  #'car
                                  org-extra-complete-completion-collection)))
                  (val (if (> (length value) 1)
                           (org-extra-complete-completing-read
                            "Complete:\s" value)
                         (car value))))
        (insert (concat (substring-no-properties val (length
                                                      prefix))
                        "\s"))))
     (t (let* ((key (org-extra-complete-completing-read
                     ""
                     org-extra-complete-completion-collection))
               (value (cdr (assoc key org-extra-complete-completion-collection))))
          (insert (concat (if (looking-back "[\s\t]+" 0) "" "\s")
                          key
                          "\s"
                          (org-extra-complete-preselect key value))))))))

;;;###autoload
(defun org-extra-complete ()
  "Complete `org-mode' keywords and values with annotation."
  (interactive)
  (let* ((line-words (org-extra-complete-line-get-line-words))
         (first-word (pop line-words))
         (inside-src (or (member "header-args" line-words)
                         (and
                          first-word
                          (string-match-p
                           "#\\+\\(header:\\|property:\\|begin_src\\|call:\\)"
                           first-word))))
         (inside-code (org-extra-complete-inside-src-code-p))
         (suboptions))
    (cond
     ((and inside-code (looking-back "<<" 0))
      (when-let ((name (completing-read
                        "Include:\s"
                        (org-extra-complete-search-named-blocks))))
        (if (looking-at ">>")
            (insert name)
          (insert name ">>"))))
     ((and inside-code)
      (org-extra-complete-make-code-ref))
     ((and inside-src (not inside-code))
      (org-extra-complete-src-headers-args))
     ((setq suboptions (and first-word
                            (org-extra-complete-option-variants
                             first-word
                             (org-extra-complete-get-completions-alist))))
      (when-let* ((option (car suboptions))
                  (separator (or (and option (org-extra-complete-get-prop
                                              option :separator))
                                 ": "))
                  (subitems (cdr suboptions)))
        (let ((value (org-extra-complete-map-values
                      (org-extra-complete-alist
                       subitems)
                      separator))
              (trimmed (org-extra-complete-strip-props option))
              (line-beg (line-beginning-position)))
          (cond ((save-excursion (re-search-backward (concat trimmed ":\s")
                                                     line-beg t 1))
                 (org-extra-complete-insert value))
                ((save-excursion (re-search-backward (concat trimmed ":")
                                                     line-beg t 1))
                 (insert "\s" value))
                ((looking-back "[\s\t]" 0)
                 (skip-chars-backward "\s\t")
                 (org-extra-complete-insert
                  (concat
                   (org-extra-complete-normalize-keyword option separator)
                   value)))
                (t
                 (org-extra-complete-insert
                  (concat (org-extra-complete-normalize-keyword option separator)
                          value)))))))
     ((org-extra-complete-line-empty-p)
      (let ((parts (org-extra-complete-alist
                    (org-extra-complete-get-completions-alist)))
            (key)
            (rest)
            (opt)
            (separator))
        (setq opt (org-extra-complete-mode-trim-keyword (pop parts)))
        (setq separator (org-extra-complete-get-prop opt :separator))
        (setq key (if (string-match-p "^begin" opt)
                      (concat "#+" opt)
                    (if (equal opt "(ref)")
                        ""
                      (concat "#+" opt ":"))))
        (setq rest (org-extra-complete-map-values parts separator))
        (org-extra-complete-insert (string-join (list key (or rest "")) "\s"))))
     (t
      (org-extra-complete-keyword (org-extra-complete-get-completions-alist))))))

(provide 'org-extra-complete)
;;; org-extra-complete.el ends here
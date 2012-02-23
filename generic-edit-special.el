;;; generic-edit-special.el --- editing submodes html css javascript

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: editing submodes html css javascript
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Thu Feb 23 21:33:19 2012 (+0800)
;; Version: 0.1
;; Last-Updated: Fri Feb 24 00:03:25 2012 (+0800)
;;           By: Le Wang
;;     Update #: 7
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;; Org setup (telling org-mode to edit javascript with js2)
;;
;;   (push (cons "javascript" 'js2) org-src-lang-modes)
;;

;; For sgml-mode
;;
;;   (require 'generic-edit-special)
;;   (eval-after-load "sgml-mode" '(define-key sgml-mode-map [(control c) ?'] 'ges/org-edit-special))
;;

;; For rinari:
;;
;;   (require 'generic-edit-special)
;;   (require 'rinari)
;;   (define-key rinari-minor-mode-map [(control c) ?'] 'ges/org-edit-special)
;;

;;; Commentary:

;; Packages like nxhtml and mmm-mode try to to activate the "right" major mode
;; for the corresponding section of code.  They never worked perfectly for me.
;;
;; Org mode, on the other hands hands off "src" blocks to new buffers to be
;; edited in their proper mode.  I like this approach much better, as it
;; worked very consistently.
;;
;; This hackand decided
;; to hack it so that other templating languages (ruby on rails in particular)merge different major-modes into
;; the same buffer.
;;
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(eval-when-compile (require 'cl))
(provide 'generic-edit-special)
(require 'org)
(require 'css-mode)

(defvar ges/markers
  (list
   `(javascript
     ;; regexp curtesy of aspx-mode.el
     ,(concat
       "^[\t ]*<\\(script\\|SCRIPT\\)[ \t\n\r\v\f]+"
       "\\("
       "type=[\"']text/javascript[\"'][ \t\n\r\v\f]+language=[\"'][Jj]ava[Ss]cript[\"']"
       "\\|"
       "language=[\"'][Jj]ava[Ss]cript[\"'][ \t\n\r\v\f]+type=[\"']text/javascript[\"']"
       "\\|"
       "type=[\"']text/javascript[\"']"
       "\\|"
       "language=[\"'][Jj]ava[Ss]cript[\"']"
       "\\)"
       "[^>]*>[\t ]*\n")
     "</script>")
   `(css ,(concat
           "<\\(style\\|STYLE\\)"
           "[ \t\n\r\v\f]+"
           "type=[\"']text/css[\"']"
           "[ \t\n\r\v\f]*"
           ">"
           "[\t ]*\n")
         "</style>")
   '(ruby "<%=?[ \t]*"
          "[ \t]*%>"))
  "list of '(language beg-regexp end-regexp) used to generate
special-edit buffers.

org-mode should be ware of the language used, see
`org-src-lang-modes'

The regexps specified should match extra spaces.")

(defun ges/inside-tags ()
  "return '(lang beg end tag-indentation), beg and end are both markers

return nil if not inside any script tags

BEG is beginning of code region inside tag
END is similar

BEG and END should start on newline, if it doesn't newlines are added."
  (let ((limit (point))
        beg
        end
        tag-indentation
        lang
        temp)
    (dolist (data ges/markers)
      (when (save-excursion
              (and
               (setq lang (car data))
               (setq beg (and (re-search-backward (cadr data) nil t)
                              (progn
                                (setq tag-indentation (current-column))
                                (goto-char (match-end 0))
                                (cond ((bolp)
                                       nil)
                                      ((not (eolp))
                                       (insert "\n")))
                                (point-marker))))
               (setq end (and (re-search-forward (nth 2 data) nil t)
                              (>= (point) limit)
                              (prog1
                                  (progn
                                    (goto-char (match-beginning 0))
                                    (unless (bolp)
                                      (insert "\n"))
                                    (setq temp (point-marker))
                                    (set-marker-insertion-type temp t)
                                    temp)
                                (indent-line-to tag-indentation))))))
        (cond ((< limit beg)
               (goto-char beg))
              ((> limit end)
               (goto-char end)))
        ;; some content needs to exist between the tags
        (when (equal beg end)
          (goto-char end)
          (insert "\n"))
        (return (list lang beg end tag-indentation))))))

(defsubst ges/_org-edit-special (info)
  (let ((major-mode 'org-mode)
        (orig-buf (current-buffer))
        (old-undo-list buffer-undo-list)
        beg-src-pos
        end-src-pos
        tag-indentation)
    (save-excursion
      (setq tag-indentation (make-string (nth 3 info) ? ))
      ;; insert fake header and footer
      (goto-char (nth 1 info))
      (unless (looking-at org-babel-src-block-regexp)
        (insert "\n")
        (backward-char)
        (setq beg-src-pos (point))
        (insert tag-indentation)
        (insert (format "#+begin_src %s" (car info)))
        (goto-char (nth 2 info))
        (forward-line 0)
        (insert "\n")
        (backward-char)
        (setq end-src-pos (point))
        (insert tag-indentation)
        (insert "#+end_src")))
    (org-edit-special)
    ;; MMM-mode has its overlay at priority 1
    (overlay-put org-edit-src-overlay 'priority 3)
    (with-current-buffer orig-buf
      (save-excursion
        (goto-char end-src-pos)
        (delete-region (point) (point-at-bol 2))
        (goto-char beg-src-pos)
        (delete-region (point) (point-at-bol 2)))
      (setq buffer-undo-list old-undo-list))))

;;;###autoload
(defun ges/org-edit-special ()
  "leverage org-edit-special for use in
templating languages like html/javascript/css/rails editing."

  (interactive)
  (let (info)
    (cond ((get-char-property (point) 'edit-buffer)
           (org-edit-src-continue (point)))
          ((setq info (ges/inside-tags))
           (ges/_org-edit-special info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic-edit-special.el ends here

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
;; Last-Updated: Thu Mar 15 01:16:08 2012 (+0800)
;;           By: Le Wang
;;     Update #: 53
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;; Org setup (telling org-mode to edit javascript with js2)
;;
;;   (push (cons "javascript" 'js2) org-src-lang-modes)
;;

;; For html-mode
;;
;;   (require 'generic-edit-special)
;;   (eval-after-load "sgml-mode" '(define-key sgml-mode-map [(control c) ?'] 'ges/org-edit-special))
;;

;; For rinari:
;;
;;    ;; Stop rinari binding to "C-c '"
;;   (setq rinari-minor-mode-prefixes '(";"))
;;
;;   (require 'generic-edit-special)
;;   (require 'rinari)
;;   (define-key rinari-minor-mode-map [(control c) ?'] 'ges/org-edit-special)
;;

;;; Commentary:

;; Packages like nxhtml and mmm-mode try to to activate the "right" major mode
;; for the current chunk of embedded code.  They never worked perfectly for
;; me.
;;
;; Orgmode, on the other hand, creates new buffers to edit "src" blocks.  This
;; has a similar effect to `indirect-buffers', but the buffer created is
;; "real".
;;
;; This approach works very well for all major-modes.  That means you can use
;; `js2-mode' to edit javascript chunks.  I find this this approach much more
;; consistently.
;;
;; This hack makes the `org-edit-special' functionality available to any
;; buffer.  Currently, it recognizes chunks of javascript, css, and ruby
;; embeddd in HTML. But it's easy to add other languages, just add them to
;; `ges/chunk-regexps'.
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

(defvar ges/mode-input-history nil)

(defvar ges/major-modes-table
  (delete-duplicates
   (delq nil (mapcar (lambda (x)
                     (when (symbolp (cdr x))
                       (symbol-name (cdr x))))
                   auto-mode-alist)))
  "list of major-modes used for completion")

(defvar ges/chunk-regexps
  (list
   `(javascript
     ;; regexp curtesy of aspx-mode.el
     ,(concat
       "<\\(script\\|SCRIPT\\)[ \t\n\r\v\f]+"
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
     "^[\t ]*</script>")
   `(css ,(concat
           "<\\(style\\|STYLE\\)"
           "[ \t\n\r\v\f]+"
           "type=[\"']text/css[\"']"
           "[ \t\n\r\v\f]*"
           ">"
           "[\t ]*\n")
         "^[\t ]*</style>")
   '(ruby "<%=?[\t ]*"
          "[\t ]*%>"))
  "list of '(language beg-regexp end-regexp) used to generate
special-edit buffers.

org-mode should be aware of the language used, see
`org-src-lang-modes'

The regexps should match extra spaces after beginning tag and before end tag.")

(defun ges/inside-chunk ()
  "return '(lang beg end tag-indentation), beg and end are both markers

return nil if not inside any chunk

BEG is beginning of code region inside tag
END is similar

BEG and END should start on newline, if it doesn't newlines are added."
  (let ((limit (point))
        beg
        end
        tag-indentation
        lang
        temp)
    (dolist (data ges/chunk-regexps)
      (when (save-excursion
              (and
               (setq lang (car data))
               (setq beg (and (re-search-backward (cadr data) nil t)
                              (progn
                                (setq tag-indentation (current-column))
                                (goto-char (match-end 0))
                                (cond ((bolp)
                                       nil)
                                      ((eolp)
                                       (forward-line 1))
                                      (t
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
          (save-excursion
            (insert "\n")))
        (return (list lang beg end tag-indentation))))))

(defun ges/_org-edit-special (info)
  (let ((major-mode 'org-mode)
        (org-edit-src-content-indentation 0)
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
    ;; is this a bug in org?
    (setq buffer-undo-list nil)
    (org-src-mode 0)
    (ges/org-src-mode 1)
    (with-current-buffer orig-buf
      (save-excursion
        (goto-char end-src-pos)
        (delete-region (point) (point-at-bol 2))
        (goto-char beg-src-pos)
        (delete-region (point) (point-at-bol 2)))
      (setq buffer-undo-list old-undo-list))))

;;;###autoload
(defun ges/org-edit-special (&optional beg end major-mode)
  "leverage org-edit-special for use in
templating languages like html/javascript/css/rails editing."
  (interactive (when (use-region-p)
                 (list
                  (region-beginning)
                  (region-end)
                  (intern (completing-read "choose major-mode: " ges/major-modes-table nil nil nil ges/mode-input-history)))))
  (let (info)
    (cond ((get-char-property (point) 'edit-buffer)
           (org-edit-src-continue (point)))
          (beg
           (ges/_org-edit-special
            (save-excursion
              (let* ((beg-marker (copy-marker (progn
                                                (goto-char beg)
                                                (point-at-bol))
                                              t))
                     (end-marker (copy-marker (progn
                                                (goto-char end)
                                                (if (bolp)
                                                    (point)
                                                  (point-at-bol 2)))
                                              t)))
                (list (intern (replace-regexp-in-string "-mode\\'"
                                                        ""
                                                        (symbol-name major-mode)))
                      beg-marker
                      end-marker
                      (progn
                        (goto-char beg-marker)
                        (let ((beg-indent (current-indentation)))
                          (skip-chars-forward " \t\v\n" end-marker)
                          (if (= (point) end-marker)
                              beg-indent
                            (current-indentation)))))))))
          ((setq info (ges/inside-chunk))
           (ges/_org-edit-special info)))))

(define-minor-mode ges/org-src-mode
  "minor-mode for the `generic-edit-buffer' special buffer."
  nil
  ""
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-src-mode-map)
    (define-key map [remap org-edit-src-save] 'ges/save)
    map))

(defun ges/save ()
  "update and save source buffer."
  (interactive)
  (if (buffer-modified-p)
      (let* ((p (point))
             (m (mark))
             (ul buffer-undo-list)
             (overlay org-edit-src-overlay)
             (begin-m (let ((m (make-marker)))
                        (set-marker m (overlay-start overlay) (overlay-buffer overlay))))
             (end-m (let ((m (make-marker)))
                      (set-marker m (overlay-end overlay) (overlay-buffer overlay))
                      (set-marker-insertion-type m t)
                      m))
             (mm major-mode)
             msg)
        (save-window-excursion
          (org-edit-src-exit 'save)
          (when (buffer-file-name)
            (save-buffer))                     ;save source
          (setq msg (current-message))
          (if (eq org-src-window-setup 'other-frame)
              (let ((org-src-window-setup 'current-window))
                (ges/org-edit-special begin-m end-m mm))
            (ges/org-edit-special begin-m end-m mm))
          (setq new-buffer (current-buffer)))
        (set-window-buffer (selected-window) new-buffer)
        (setq buffer-undo-list ul)
        (push-mark m 'nomessage)
        (goto-char (min p (point-max)))
        (message (or msg "")))
    (message "no changes need to be saved.")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic-edit-special.el ends here

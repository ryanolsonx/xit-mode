;;; xit-mode.el --- A [x]it! major mode for Emacs. -*- lexical-binding: t; -*-

;; See: https://xit.jotaen.net/

;; Copyright (C) 2022 Ryan Olson

;; Authors: Ryan Olson <ryolson@me.com>
;; Keywords: xit todo

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

;;

;;; Code:

(defvar xit-mode-hook nil)

(defvar xit--checkbox-regexp "^\\(\\[[ |x|@|~]\\] \\)"
  "The regpexp used to search for the checkbox.")

(defvar xit--priority-regexp "\\([\\!|\\.]+ \\)"
  "The regpexp used to search for the priority.")

(defvar xit--checkbox-string-open "[ ] "
  "The open checkbox string.")

(defvar xit--checkbox-string-checked "[x] "
  "The checked checkbox string.")

(defvar xit--checkbox-string-ongoing "[@] "
  "The progress checkbox string.")

(defvar xit--checkbox-string-obsolete "[~] "
  "The obsolete checkbox string.")

(defun xit--item-replace-checkbox (reg rep)
  "Replace the current item checkbox spotted by REG with REP."
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-min))
    (when (re-search-forward reg nil t)
      (replace-match rep))))

(defun xit-item-open ()
  "Set a xit item to open."
  (interactive)
  (xit--item-replace-checkbox xit--checkbox-regexp xit--checkbox-string-open))

(defun xit-item-checked ()
  "Set a xit item to checked."
  (interactive)
  (xit--item-replace-checkbox xit--checkbox-regexp xit--checkbox-string-checked))

(defun xit-item-ongoing ()
  "Set a xit item to ongoing."
  (interactive)
  (xit--item-replace-checkbox xit--checkbox-regexp xit--checkbox-string-ongoing))

(defun xit-item-obsolete ()
  "Set a xit item to obsolete."
  (interactive)
  (xit--item-replace-checkbox xit--checkbox-regexp xit--checkbox-string-obsolete))

(defun xit-item-cycle ()
  "Cycle through xitem states."
  (interactive)
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-min))
    (when (re-search-forward xit--checkbox-regexp nil t)
      (let ((checkbox (match-string-no-properties 0)))
        (cond
         ((string-equal checkbox xit--checkbox-string-open)
          (replace-match xit--checkbox-string-ongoing))
         ((string-equal checkbox xit--checkbox-string-ongoing)
          (replace-match xit--checkbox-string-checked))
         ((string-equal checkbox xit--checkbox-string-checked)
          (replace-match xit--checkbox-string-obsolete))
         ((string-equal checkbox xit--checkbox-string-obsolete)
          (replace-match xit--checkbox-string-open))
         (t (warn "Checkbox not found")))))))

(defun xit-item-inc-priority ()
  "Increase item priority."
  (interactive)
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-min))
    (if (re-search-forward xit--priority-regexp nil t)
        (replace-match (concat "!" (match-string-no-properties 1)))
      (when (re-search-forward xit--checkbox-regexp nil t)
        (replace-match "\\1! ")))))

(defun xit-item-dec-priority ()
  "Decrease item priority."
  (interactive)
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-min))
    (when (re-search-forward xit--priority-regexp nil t)
      (let ((s (substring (match-string-no-properties 1) 1)))
        (if (string-equal s " ")
            (replace-match "")
          (replace-match s))))))

(defvar xit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'xit-item-open)     ;; o for open
    (define-key map (kbd "C-c C-d") 'xit-item-checked)  ;; d for done
    (define-key map (kbd "C-c C-p") 'xit-item-ongoing)  ;; p for progress
    (define-key map (kbd "C-c C-a") 'xit-item-obsolete) ;; a for archive
    (define-key map (kbd "C-c C-c") 'xit-item-cycle)    ;; c for cycle
    (define-key map (kbd "C-c C-<up>") 'xit-item-inc-priority)
    (define-key map (kbd "C-c C-<down>") 'xit-item-dec-priority)
    map)
  "Keymap for `xit-mode'.")

;; descriptions disabled until tags in descriptions are resolved.
;; right now tags don't display if a description has a face.
(defvar xit-mode-font-lock-keywords
  (list
   '("^[a-zA-Z]+.*$" 0 'xit-group-title)
   '("^\\(\\[ \\]\\) [\\!|\\.]*\\(.*\\)"
     (1 'xit-open-checkbox))
     ;(2 'xit-open-description))
   '("^\\(\\[x\\]\\) \\(.*\\)"
     (1 'xit-checked-checkbox))
     ;(2 'xit-checked-description))
   '("^\\(\\[@\\]\\) [\\!|\\.]*\\(.*\\)"
     (1 'xit-ongoing-checkbox))
     ;(2 'xit-ongoing-description))
   '("^\\(\\[~\\]\\) \\(.*\\)"
     (1 'xit-obsolete-checkbox)
     (2 'xit-obsolete-description))
   '("^\\[[x|@| |~]\\] \\([\\!|\\.]+\\)[^\\!|\\.]" 1 'xit-priority)
   '("#[a-zA-Z0-9\\-_]+" 0 'xit-tag))
  "Highlighting specification for `xit-mode'.")

(defface xit-group-title
  '((t :inherit (bold underline)))
  "Face used for checkboxes group title"
  :group 'xit-faces)

(defface xit-open-checkbox
  '((t :inherit font-lock-function-name-face))
  "Face used for open checkbox."
  :group 'xit-faces)

(defface xit-open-description
  '((t :inherit default))
  "Face used for open checkbox description."
  :group 'xit-faces)

(defface xit-checked-checkbox
  '((t :inherit success))
  "Face used for checked checkbox."
  :group 'xit-faces)

(defface xit-checked-description
  '((t :foreground "#838383"))
  "Face used for checked checkbox description."
  :group 'xit-faces)

(defface xit-ongoing-checkbox
  '((t :inherit font-lock-keyword-face))
  "Face used for ongoing checkbox."
  :group 'xit-faces)

(defface xit-ongoing-description
  '((t :inherit default))
  "Face used for ongoing checkbox description."
  :group 'xit-faces)

(defface xit-obsolete-checkbox
  '((t :foreground "#838383"))
  "Face used for obsolete checkbox."
  :group 'xit-faces)

(defface xit-obsolete-description
  '((t :foreground "#838383"))
  "Face used for obsolete checkbox description."
  :group 'xit-faces)

(defface xit-priority
  '((t :inherit error))
  "Face used for priority markers ! or ."
  :group 'xit-faces)

(defface xit-tag
  '((t :inherit font-lock-constant-face))
  "Face used for tags."
  :group 'xit-faces)

(define-derived-mode xit-mode text-mode "[x]it!"
  "Major mode for [x]it files."
  (kill-all-local-variables)
  (use-local-map xit-mode-map)
  (setq font-lock-defaults '(xit-mode-font-lock-keywords))
  (setq major-mode 'xit-mode)
  (setq mode-name "[x]it!")
  (run-hooks 'xit-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xit\\'" . xit-mode))

(provide 'xit-mode)
;;; xit-mode.el ends here

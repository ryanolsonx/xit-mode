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

(defvar xit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `xit-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xit\\'" . xit-mode))

(defvar xit-mode-font-lock-keywords
  (list
   '("^\\(\\[x\\]\\) \\(.*\\)"
     (1 'xit-checked-checkbox)
     (2 'xit-checked-description))
   '("^#.*$" 0 'bold) ; make the line beginning with "#" bold
   '("\\(foo\\)\\(bar\\)" ; the word foobar in two faces
     (1 'font-lock-warning-face)
     (2 'font-lock-type-face)))
  "Highlighting specification for `xit-mode'.")

(defface xit-checked-checkbox
  '((t :inherit font-lock-type-face))
  "Face used for checked checkbox."
  :group 'xit-faces)

(defface xit-checked-description
  '((t :inherit font-lock-type-face))
  "Face used for checked checkbox description."
  :group 'xit-faces)

(defun xit-mode ()
  "Major mode for [x]it!"
  (interactive)
  (kill-all-local-variables)
  (use-local-map xit-mode-map)
  (setq font-lock-defaults '(xit-mode-font-lock-keywords))
  (setq major-mode 'xit-mode)
  (setq mode-name "[x]it!")
  (run-hooks 'xit-mode-hook))

(provide 'xit-mode)
;;; xit-mode.el ends here

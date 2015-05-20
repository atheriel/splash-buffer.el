;;; splash-buffer.el --- Maintain a custom splash/start-up buffer.

;; Copyright (C) 2015 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; Version: 0.1
;; Keywords: help
;; URL: https://github.com/atheriel/splash-buffer.el

;; This file is not part of GNU Emacs.

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

;; https://github.com/atheriel/splash-buffer.el

;;; Code:

(defgroup splash-buffer nil
  "Maintain a custom splash/start-up buffer."
  :group 'convenience)

(defcustom splash-buffer-buffer-name "*GNU Emacs*"
  "The name of the buffer used by splash-buffer."
  :group 'splash-buffer
  :version 0.1
  :type 'string)

(defcustom splash-buffer-contents fancy-startup-text
  "The contents of the splash buffer."
  :group 'splash-buffer
  :version 0.1)

(defcustom splash-buffer-show-recover-session t
  "Whether to display a message about auto-save files, when present."
  :group 'splash-buffer
  :version 0.1
  :type 'boolean)

(defcustom splash-buffer-recover-session-text
  ;; FIXME: This is currently hand-wrapped. It could be done
  ;; automatically in the future with the `s' library.
  `(:face (default font-lock-comment-face)
	  "Auto-save file(s) found. If an Emacs session crashed "
	  "recently,\ntype "
	  :link ("M-x recover-session RET"
		 (lambda (_) (call-interactively 'recover-session)))
	  " to recover the files you were\nediting.")
  "The message shown in the splash buffer when auto-save files are found, as
long as `splash-buffer-show-recover-session' is non-nil. By default, it is
similar to the message that appears in the built-in *GNU Emacs* buffer."
  :group 'splash-buffer
  :version 0.1)

(defun splash-buffer-show ()
  "Creates or updates the splash buffer, then switches to it.

This is a modifed version of the built-in `fancy-startup-screen'
function from `startup.el'."
  (let ((buff (get-buffer-create splash-buffer-buffer-name)))
    (with-current-buffer buff
      ;; Update the contents of the splash buffer.
      (let ((inhibit-read-only t))
	(erase-buffer)
	(fancy-splash-head)
	(dolist (text splash-buffer-contents)
	  (apply #'fancy-splash-insert text)
	  (insert "\n"))
	(skip-chars-backward "\n")
	(delete-region (point) (point-max))
	(insert "\n")
	(fancy-startup-tail))
      ;; Apply local settings.
      (use-local-map splash-screen-keymap)
      (setq-local browse-url-browser-function 'eww-browse-url)
      (setq tab-width 22
	    buffer-read-only t)
      (set-buffer-modified-p nil)
      ;; Position the cursor.
      (goto-char (point-min))
      (forward-line 4))
    ;; Finally, switch to the buffer.
    (switch-to-buffer buff)))

(provide 'splash-buffer)

;;; splash-buffer.el ends here

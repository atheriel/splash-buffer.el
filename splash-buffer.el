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

(defun splash-buffer (&rest plist)
  "Create or update a splash buffer, then switch to it.

Arguments to this function must be in the form of a property
list. The following keywords have special meaning --- all others
are passed on to `setq' inside of the resulting buffer.

    :name :content :show-recovery :recovery-content
    :before :after :extra :keymap

The details of their usage is below.

    \:name

    A string indicating the name for the splash buffer. If
    absent, the value of `splash-buffer-buffer-name' will be used
    instead.

    \:content

    A list of content that can be parsed by the
    `fancy-splash-insert' function from the `startup.el' file.

    \:show-recovery &
    \:recover-content

    When \:show-recover is non-nil, show the recovery content set
    in `splash-buffer-recover-session-text'. Specifying
    \:recover-content explicitly will override this content with
    the argument.

    \:before, \:after &
    \:extra

    To achieve arbitrary buffer content, pass a function with one
    of these keys. The \:before & \:after functions are run
    before and after the \:content is inserted into the buffer.
    \:extra is intended to change settings for the buffer, such
    as the `tab-width' or the location of the cursor, and is run
    last.

    \:keymap

    Specify a local key mapping for the splash buffer.

Internally, this function is based on the `fancy-startup-screen'
function from `startup.el'."
  (let ((buff    (get-buffer-create
		  (or (plist-get plist :name)
		      splash-buffer-buffer-name)))
	(content (or (plist-get plist :content)
		     splash-buffer-contents))
	(before  (plist-get plist :before))
	(after   (plist-get plist :after))
	(keymap  (plist-get plist :keymap))
	(extra   (plist-get plist :extra))
	;; Get the custom recovery content, if available.
	;; Otherwise, check for suppression here and in the
	;; package customizeable variables before using the
	;; customizeable text as the content.
	(recovery-content
	 (or (plist-get plist :recovery-content)
	     (and (plist-get plist :show-recovery)
		  splash-buffer-show-recover-session
		  splash-buffer-recover-session-text))))
    ;; FIXME: Extract the remaining arguments and pass them on to
    ;; `setq' after the call to `extra' below.
    ;; FIXME: Should argument types be validated beforehand to
    ;; prevent partial filling of the buffer?
    (with-current-buffer buff
      ;; Update the contents of the splash buffer.
      (let ((inhibit-read-only t))
	(erase-buffer)
	(when before (funcall before))
	;; Add the recover-session message to the content, so
	;; long as this option is enabled and auto-save files are
	;; found.
	(when (and recovery-content
		   ;; Check for auto-save files.
		   (file-directory-p
		    (file-name-directory
		     auto-save-list-file-prefix)))
	  (add-to-list 'content recovery-content t))
	;; Format the content and insert it into the buffer.
	(dolist (text content)
	  (apply #'fancy-splash-insert text)
	  (insert "\n"))
	(skip-chars-backward "\n")
	(delete-region (point) (point-max))
	(insert "\n")
	(when after (funcall after)))
      ;; Apply local settings.
      (set-buffer-modified-p nil)
      (when keymap (use-local-map keymap))
      (when extra (funcall extra)))
    ;; Finally, switch to the buffer.
    (switch-to-buffer buff)))

(provide 'splash-buffer)

;;; splash-buffer.el ends here

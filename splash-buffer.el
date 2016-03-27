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

(defvar splash-buffer-default-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    ;; TODO: Add a refresh toggle.
    (define-key map "q" 'quit-window)
    map)
  "A keymap for `splash-buffer-default-mode'.")

(define-derived-mode splash-buffer-default-mode fundamental-mode "SplashBuffer"
  "The default major mode for \"splash\" buffers."
  (setq truncate-lines t
        buffer-read-only t))

(defun splash-buffer-insert-image (filepath)
  "Insert the image at FILEPATH, if possible."
  (if (file-exists-p filepath)
      (progn
	(let ((img (create-image filepath)))
	  (insert-image img)))
    (warn "Could not locate image file %s. Skipping..." filepath)))

(defmacro splash-buffer-define (symbol buffer-name &rest body)
  "Create a \"splash\" buffer function SYMBOL.

The buffer will be denoted by BUFFER-NAME (e.g. \"*GNU Emacs*\").
BODY should contain forms that insert content into such a buffer,
although it may also begin with any nunmber of these keyword
arguments:

:mode MODE

    Use major mode MODE in the buffer, instead of the default
    `splash-buffer-default-mode'.

:keymap MAP

    Use MAP as a local keymap in the buffer.
"
  (declare (indent defun))
  ;; Basic argument checking.
  (when (not (symbolp symbol))
    (error "splash-buffer requires a symbol to denote the buffer."))
  (when (not (stringp buffer-name))
    (error "buffer-name must be a string."))
  ;; FIXME: Support extracting keyword arguments.
  (let ((mode 'splash-buffer-default-mode) ;; FIXME
	(map nil)
	(after nil)
	(create-func (intern (format "%s--create" symbol)))
	(buffer-name-symbol (intern (format "%s--name" symbol))))
    ;; Extract keyword arguments from BODY.
    (while (keywordp (car body))
      (pcase (pop body)
	(`:mode (setq mode (pop body)))
	(`:keymap (setq map (pop body)))
	(`:after (setq after (pop body)))
	(_ (pop body))))
    ;; Create function definitions.
    `(progn
       (defconst ,buffer-name-symbol ,buffer-name
	 ,(format "The name of the %s buffer itself." symbol))

       (defun ,create-func ()
	 ,(format "Inserts the contents of `%s'." symbol)
	 (with-current-buffer (get-buffer-create ,buffer-name-symbol)
	   (save-excursion
	     (goto-char (point-min))
	     ,@body
	     (,mode)
	     ,(when map `(use-local-map ,map))
	     ,(when after `,after)
	     )))

       (defun ,symbol ()
	 ;; FIXME: Insert the docstring.
	 (interactive)
	 (unless (buffer-live-p (get-buffer ,buffer-name-symbol))
	   (,create-func))
	 (switch-to-buffer ,buffer-name-symbol)
	 (redisplay))
       )))

(provide 'splash-buffer)

;;; splash-buffer.el ends here

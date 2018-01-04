;;; autoprefixer.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Kyung Mo Kweon
;;
;; Author: Kyung Mo Kweon<kkweon@gmail.com> and contributors
;; URL: https://github.com/kkweon/emacs-css-autoprefixer
;; Package-Requires: ((emacs "24"))
;; Version: 3.0
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License: MIT

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;; css-autoprefixer
;;
;; Bind the following commands:
;; smex, smex-major-mode-commands
;;
;; For a detailed introduction see:
;; https://github.com/kkweon/emacs-css-autoprefixer/README.org

;;; Code:

;;;###autoload
(defun css-autoprefixer ()
  "Add autoprefix in the current buffer"
  (interactive)
  (save-excursion
    (let* ((temp-name (make-temp-file "css-prefixer" nil ".css"))
           (temp-css (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end))
                       (buffer-string))))
      (with-temp-file temp-name
        (insert temp-css))
      (progn
        (if (region-active-p)
            (delete-region (region-beginning)
                           (region-end))
          (erase-buffer))
        (insert (css-autoprefixer--execute-npx temp-name))))))

(defun css-autoprefixer--build-npx-command (filename)
  (concat "npx postcss "
          (expand-file-name filename)
          " --use autoprefixer"))

(defun css-autoprefixer--trim-first-and-last (message)
  "Delete first line and last line of MESSAGE"
  (s-join "\n"
          (nbutlast (cdr (s-split "\n" message)))))

(defun css-autoprefixer--execute-npx (filename)
  "Execute Autoprefix on FILENAME"
  (css-autoprefixer--trim-first-and-last (shell-command-to-string (css-autoprefixer--build-npx-command filename))))

(provide 'css-autoprefixer)

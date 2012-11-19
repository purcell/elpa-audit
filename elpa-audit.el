;;; elpa-audit.el --- Handy functions for inspecting and comparing package archives

;; Copyright (C) 2012 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Version: DEV
;; URL: https://github.com/purcell/elpa-audit
;; Keywords: maint

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

;; Useful functions for package archive maintainers.

;;; Code:

(defun elpa-audit/clean-description (package-descr)
  "Trim cruft from PACKAGE-DESCR."
  (replace-regexp-in-string " \\(-\\*-\\|\\[source\\).*" "" package-descr))

(defun elpa-audit/read-elisp-datum (file-name)
  "Read the first sexp in FILE-NAME."
  (car (read-from-string
        (with-temp-buffer
          (insert-file-contents-literally file-name)
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun elpa-audit/package-list (archive-name)
  "Return a sorted list of (name . description) for packages in ARCHIVE-NAME."
  (let* ((archive-contents (expand-file-name (concat "archives/" archive-name "/archive-contents")
                                             package-user-dir))
         (packages (mapcar (lambda (entry) (cons (car entry)
                                            (elpa-audit/clean-description
                                             (elt (cdr entry) 2))))
                        (rest (elpa-audit/read-elisp-datum archive-contents)))))
    (sort packages (lambda (p1 p2) (string< (car p1) (car p2))))))

(defun elpa-audit/package-names (archive-name)
  "Return a (sorted) list of package names found in ARCHIVE-NAME."
  (mapcar 'car (elpa-audit/package-list archive-name)))

(defun elpa-audit/archive-sizes ()
  "Return a list of (ARCHIVE-NAME . PACKAGE-COUNT) for all archives."
  (mapcar
   (lambda (archive-name)
     (cons archive-name (length (elpa-audit/package-names archive-name))))
   (mapcar 'car package-archives)))



(defun elpa-audit/read-archive-name (&optional prompt)
  "Ask the name of an archive, optionally using PROMPT."
  (let ((archive-names (mapcar 'car package-archives)))
    (completing-read (or prompt "Package archive: ")
                     archive-names
                     nil t nil nil
                     (first archive-names))))

(defun elpa-audit-dump-package-list-to-buffer (archive-name)
  "Write a list of packages in ARCHIVE-NAME into a new buffer."
  (interactive (list (elpa-audit/read-archive-name)))
  (let ((buffer
         (save-excursion
           (let ((packages (elpa-audit/package-list archive-name)))
             (with-current-buffer (get-buffer-create (format "*package list - %s*" archive-name))
               (erase-buffer)
               (dolist (entry packages)
                 (insert (format "%s - %s\n" (car entry) (cdr entry))))
               (goto-char 0)
               (current-buffer))))))
    (when (called-interactively-p 'any)
      (pop-to-buffer buffer))
    buffer))

(defun elpa-audit-ediff-archives (archive1 archive2)
  "Start an ediff session comparing the package lists for ARCHIVE1 and ARCHIVE2."
  (interactive (list
                (elpa-audit/read-archive-name "Package archive A: ")
                (elpa-audit/read-archive-name "Package archive B: ")))
  (ediff-buffers (elpa-audit-dump-package-list-to-buffer archive1)
                 (elpa-audit-dump-package-list-to-buffer archive2)))


(provide 'elpa-audit)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; elpa-audit.el ends here

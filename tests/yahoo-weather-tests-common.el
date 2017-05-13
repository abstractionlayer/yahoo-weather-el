;;; yahoo-weather-tests-common.el

;; Copyright (c) 2017 Whitesquall

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; Commentary:

;; Common for package tests.

;;
;; emacs -batch -Q -L . -L .. -l yahoo-weather-tests.el -f ert-run-tests-batch-and-exit
;;

;;; Code:


(require 'rng-loc)


(defvar yahoo-weather-test-data-file "yahoo-weather.xml"
  "The file with test data.")

(defun yahoo-weather-find-file-with-test-data ()
  "Find the file with xml data response from the server. It depends how tests
 have been ran — manually or using make."
  (if (file-exists-p yahoo-weather-test-data-file)
      yahoo-weather-test-data-file
    (concat "tests/" yahoo-weather-test-data-file)))

(defun yahoo-weather-emulate-server-response ()
  (let ((buffer-name (generate-new-buffer-name " *temp*")))
    ;; We can skip a header of http response. But we need to fill
    ;; a header-body delimiter which used as pattern for searching response
    ;; body. But after this manipulation the content of buffer stays to be
    ;; a valid xml. Killing of two rabbits with one shot!
    (with-current-buffer (generate-new-buffer buffer-name)
      (insert "\n\n")
      (insert-file-contents (yahoo-weather-find-file-with-test-data)))
    buffer-name))

(defun yahoo-weather-get-data-fixture (body date format data)
  (unwind-protect
      (progn
        (let ((org-yahoo-weather-display-icon-p nil)
              (org-yahoo-weather-location "moscow, ru")
              (org-yahoo-weather-format format))
          (cl-letf (((symbol-function 'yahoo-weather-get-data)
                     (lambda (location expire-time)
                       data))
                    ((symbol-function 'org-yahoo-weather-check-interval)
                     (lambda (date)
                       t)))
            (funcall body))))))

(defun yahoo-weather-url-retrieve-synchronously-mock (body arg-captor)
  "Mock for `url-retrieve-synchronously' because we need to check argument for
requirements.
BODY is a lambda with statements which emulates original
`url-retrieve-synchronously'.
ARG-CAPTOR is a lambda which capture url passing to mocked function."
  (cl-letf
      (((symbol-function 'url-retrieve-synchronously)
        (lambda (url)
          (interactive)
          (funcall arg-captor url)
          (yahoo-weather-emulate-server-response))))
    (funcall body)))

(defun yahoo-weather-get-test-data ()
  "Return stubbed test data in xml format."
  (with-current-buffer
      (yahoo-weather-emulate-server-response)
    (let ((data (xml-parse-region)))
      (kill-buffer (current-buffer))
      data)))


(provide 'yahoo-weather-tests-common)


;;; yahoo-weather-tests-common.el ends here

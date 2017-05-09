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

(defun yahoo-weather-open-xml-file-suppress-msg-what-schema-is-used ()
  "Open file with test data without printing message what schema is used.
Running tests looks more cleaner."
  (cl-letf (((symbol-function 'rng-what-schema)
             (lambda ()
               (interactive))))
    (find-file (yahoo-weather-find-file-with-test-data))))

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

(defun yahoo-weather-get-test-data ()
  "Return stubbed test data in xml format."
  (with-current-buffer
      (yahoo-weather-open-xml-file-suppress-msg-what-schema-is-used)
    (let ((data (xml-parse-region)))
      (kill-buffer (current-buffer))
      data)))


(provide 'yahoo-weather-tests-common)


;;; yahoo-weather-tests-common.el ends here

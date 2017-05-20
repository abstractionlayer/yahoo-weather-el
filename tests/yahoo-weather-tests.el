;;; yahoo-weather-tests.el

;; Copyright (c) 2016 Whitesquall

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

;; Tests for the package.

;;
;; emacs -batch -Q -L .. -l yahoo-weather-tests.el -f ert-run-tests-batch-and-exit
;;

;;; Code:


(require 'ert)
(require 'yahoo-weather)
(require 'org-yahoo-weather)
(require 'yahoo-weather-tests-common)

(ert-deftest yahoo-weather-build-forecast-url-test ()
  "Test the url building."
  :tags '(yahoo-weather)
  (should
   (let ((yahoo-weather-use-https nil))
     (string-equal
      (yahoo-weather-build-forecast-url "moscow, ru")
      (concat "http://query.yahooapis.com/v1/public/yql?q="
              "select * from weather.forecast where woeid in ("
              "select woeid from geo.places(1) where text='moscow, ru'"
              ") and u='c'&format=xml"))))

  (should
   (let ((yahoo-weather-use-https t))
     (string-equal
      (yahoo-weather-build-forecast-url "moscow, ru")
      (concat "https://query.yahooapis.com/v1/public/yql?q="
              "select * from weather.forecast where woeid in ("
              "select woeid from geo.places(1) where text='moscow, ru'"
              ") and u='c'&format=xml"))))
  )

(ert-deftest yahoo-weather-get-city-test ()
  "Test the city name of forecast."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data)))
    (should
     (string-equal
      (yahoo-weather-data->city data)
      "Moscow"))))

(ert-deftest yahoo-weather-prepare-date-test ()
  "Test the preparing of date ready to parsing."
  :tags '(yahoo-weather)
  (should
   (string-equal
    (yahoo-weather-prepare-date (list 5 14 2017))
    "14 May 2017"))

  (should
   (string-equal
    (yahoo-weather-prepare-date (list 12 4 2017))
    "04 Dec 2017"))
  )

(ert-deftest yahoo-weather-get-low-temperature-test ()
  "Test the low temperature."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data))
        (date (list 05 14 2017)))
    (should
     (string-equal
      (yahoo-weather-forecast->low-temperature
       (yahoo-weather-data->forecast-by-date data date))
      "0"))
    ))

(ert-deftest yahoo-weather-get-high-temperature-test ()
  "Test the high temperature."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data))
        (date (list 05 14 2017)))
    (should
     (string-equal
      (yahoo-weather-forecast->high-temperature
       (yahoo-weather-data->forecast-by-date data date))
      "11"))
    ))

(ert-deftest yahoo-weather-get-condition-test ()
  "Test the condition."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data))
        (date (list 05 14 2017)))
    (should
     (string-equal
      (yahoo-weather-forecast->condition
       (yahoo-weather-data->forecast-by-date data date))
      "Partly Cloudy"))
    ))

(ert-deftest yahoo-weather-get-code-test ()
  "Test the code."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data))
        (date (list 05 14 2017)))
    (should
     (string-equal
      (yahoo-weather-forecast->code
       (yahoo-weather-data->forecast-by-date data date))
      "30"))
    ))

(ert-deftest org-yahoo-weather-test ()
  "Test the org entry with the weather for location."
  :tags '(yahoo-weather)
  (let ((data (yahoo-weather-get-test-data)))
    (yahoo-weather-get-data-fixture
     (lambda ()
       (should
        (string-equal (org-yahoo-weather)
                      "Moscow: Partly Cloudy, [0,11]°C")))
     (list 5 14 2017)
     "%C: %c, [%l,%h]%s"
     data)
    ))

(ert-deftest yahoo-weather-check-that-url-was-encoded-test ()
  "Regression. Test that url of API was encoded before requesting.
Otherwise url caching won't work properly."
  :tags '(yahoo-weather)
  (let* ((original-url (yahoo-weather-build-forecast-url "moscow, ru"))
         (captured-url nil)
         (body (lambda () (yahoo-weather-retrieve-data original-url)))
         (arg-captor (lambda (url) (setq captured-url url))))
    (yahoo-weather-url-retrieve-synchronously-mock body arg-captor)
    (should
     (string-equal
      (url-encode-url original-url)
      captured-url))))

(ert-deftest yahoo-weather-check-interval-test ()
  "Test the `org-yahoo-weather-check-interval' function."
  :tags '(yahoo-weather)
  (yahoo-weather-current-time-stub
   (lambda ()
     (should
      ;; "2017-05-20"
      (org-yahoo-weather-check-interval '(5 20 2017)))

     (should
      ;; "2017-05-29"
      (org-yahoo-weather-check-interval '(5 29 2017)))

     (should
      ;; "2017-05-30"
      (not (org-yahoo-weather-check-interval '(5 30 2017))))

     (should
      ;; "2017-05-19"
      (not (org-yahoo-weather-check-interval '(5 19 2017))))
     )
   ;; "2017-05-20 19:04:41"
   '(22816 26906 509599 313000)))


;;; yahoo-weather-tests.el ends here

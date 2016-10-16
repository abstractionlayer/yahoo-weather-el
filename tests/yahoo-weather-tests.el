;;; yandex-weather-tests.el

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


(ert-deftest yahoo-weather-build-forecast-url-test ()
  "Test the url building."
  :tags '(yahoo-weather)
  (should
   (let ((yahoo-weather-use-https nil))
     (string-equal
      (yahoo-weather-build-forecast-url "moscow, ru")
      "http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid in (select woeid from geo.places(1) where text='moscow, ru') and u='c'&format=xml")))

  (should
   (let ((yahoo-weather-use-https t))
     (string-equal
      (yahoo-weather-build-forecast-url "moscow, ru")
      "https://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid in (select woeid from geo.places(1) where text='moscow, ru') and u='c'&format=xml")))
  )




;;; yandex-weather-tests.el ends here

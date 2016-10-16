;;; yahoo-weather.el --- Fetch Yahoo Weather forecasts.

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


(require 'cl-lib)
(require 'url)
(require 'url-cache)
(require 'xml)


(defgroup yahoo-weather nil
  "Yahoo weather."
  :group 'comm)

(defcustom yahoo-weather-use-https t
  "Default protocol to use to access the Yahoo Weather API."
  :group 'yahoo-weather
  :type 'boolean)

(defconst yahoo-weather-forecast-url
  (concat "http%s://query.yahooapis.com/v1/public/yql?q="
          "select * from weather.forecast "
          "where woeid in ("
          "select woeid from geo.places(1) "
          "where text='%s'"
          ") and u='c'"
          "&format=xml")
  "URL of the API.")

(defun yahoo-weather-build-forecast-url (location)
  "Build URL to retrieve weather for LOCATION."
  (format yahoo-weather-forecast-url
          (if yahoo-weather-use-https "s" "")
          location))



(provide 'yahoo-weather)


;;; yandex-weather.el ends here

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


(require 'cl)
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

(defun yahoo-weather-cache-fetch (url)
  "Fetch URL from the local cache."
  (with-current-buffer
      (generate-new-buffer (generate-new-buffer-name " *temp*"))
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

(defun yahoo-weather-retrieve-data-raw (url &optional expire-time)
  "Retrieve URL and return its data as a string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (let* ((expired (if expire-time
                      (yahoo-weather-cache-expired url expire-time)
                    t))
         (buffer (if expired
                     (url-retrieve-synchronously url)
                   (yahoo-weather-cache-fetch url)))
         data)
    (when (and expired expire-time)
      (url-store-in-cache buffer))
    buffer))

(defun yahoo-weather-retrieve-data (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
it is not older than EXPIRE-TIME secnds. Otherwise, it will be fetched
and then cached. Therefore, setting EXPIRE-TIME to 0 will force a cache
renewal."
  (with-current-buffer (yahoo-weather-retrieve-data-raw
                        url expire-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found."))
    (decode-coding-region
     (point) (point-max)
     (detect-coding-region (point) (point-max) t))
    (set-buffer-multibyte t)
    (let ((data (xml-parse-region (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun yahoo-weather-cache-expired (url expire-time)
  "Check if URL is cached for more than EXPIRE-TIME."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   (seconds-to-time expire-time))
                  (current-time))
               t)))))

(defun yahoo-weather-get-data (location &optional expire-time)
  "Get weather data for LOCATION.
See `yahoo-weather-retrieve-data' for the usage of EXPIRE-TIME."
  (yahoo-weather-retrieve-data
   (yahoo-weather-build-forecast-url location) expire-time))

(defun yahoo-weather-data->all-info (data)
  "Return all weather information from xml server response in DATA."
  (car (xml-get-children (car (xml-get-children (assq 'query data)
                                                'results))
                         'channel)))

(defun yahoo-weather-data->city (data)
  "Return the city where the DATA come from."
  (xml-get-attribute-or-nil (car (xml-get-children
                                  (yahoo-weather-data->all-info data)
                                  'yweather:location)) 'city))

(defun yahoo-weather-data->forecasts (data)
  "Return forecasts for all days from the DATA."
  (xml-get-children
   (cdr (cdr (assq 'item (yahoo-weather-data->all-info data))))
   'yweather:forecast))

(defvar yahoo-weather-months-map
  '(( 1 . "Jan") ( 2 . "Feb") ( 3 . "Mar") ( 4 . "Apr")
    ( 5 . "May") ( 6 . "Jun") ( 7 . "Jul") ( 8 . "Aug")
    (09 . "Sep") (10 . "Oct") (11 . "Nov") (12 . "Dec"))
  "The map of numbers and correspond months names.")

(defun yahoo-weather-get-month-short-by-number (month-number)
  "Return the arrow of wind direction by SYMBOL."
  (cdr (assoc month-number yahoo-weather-months-map)))

(defun yahoo-weather-prepare-date (date)
  "Return converted date ready for parsing. DATE should be list of month,
day and year."
  (format "%.2d %s %.4d"
          (nth 1 date)
          (yahoo-weather-get-month-short-by-number (nth 0 date))
          (nth 2 date)))

(defun yahoo-weather-data->forecast-by-date (data date)
  "Return the forecast of DATA by the DATE."
  (let* ((forecast-date (yahoo-weather-prepare-date date))
         (get-date (lambda (x) (cdr (assq 'date (car (cdr x))))))
         (filter (lambda (x) (string-equal forecast-date
                                           (funcall get-date x))))
         (forecasts (yahoo-weather-data->forecasts data)))
    (car (remove-if-not filter forecasts))))

(defun yahoo-weather-forecast->low-temperature (forecast)
  "Return the low temperature for the FORECAST."
  (xml-get-attribute forecast 'low))

(defun yahoo-weather-forecast->high-temperature (forecast)
  "Return the high temperature for the FORECAST."
  (xml-get-attribute forecast 'high))

(defun yahoo-weather-forecast->condition (forecast)
  "Return the condition for the FORECAST."
  (xml-get-attribute forecast 'text))

(defun yahoo-weather-forecast->code (forecast)
  "Return the condition for the FORECAST."
  (xml-get-attribute forecast 'code))


(provide 'yahoo-weather)


;;; yahoo-weather.el ends here

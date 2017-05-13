;;; org-yahoo-weather.el -- Show Yahoo Weather forecasts in Org Agenda.

;; Copyright (C) 2016 Whitesquall

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

;; The facility for the org-mode agenda.

;;; Code:


(require 'cl-lib)
(require 'calendar)
(require 'yahoo-weather)
(require 'image)
(require 'format-spec)


(defgroup org-yahoo-weather nil
  "Yahoo Weather for Org mode."
  :group 'comm
  :group 'org)

(defcustom org-yahoo-weather-location "moscow, ru"
  "Default location for org-yahoo-weather."
  :group 'org-yahoo-weather)

(defcustom org-yahoo-weather-format "%C: %c [%l,%h]%s"
  "Formating string to describe weather.
Valid %-sequences are:
   - %c means the weather condition;
   - %C means the city the weather is for;
   - %l means the lower temperature;
   - %h means the higher temperature;
   - %s means the temperature unit symbol."
  :group 'org-yahoo-weather)

(defcustom org-yahoo-weather-cache-time 7200
  "Define how many seconds we should cache the weather forecast response from
the API server."
  :group 'org-yahoo-weather
  :type 'integer)

(defcustom org-yahoo-weather-display-icon-p nil
  "Show icons or not."
  :group 'org-yahoo-weather
  :type 'boolean)

(defconst org-yahoo-weather-temperature-symbol "°C"
  "Temperature symbol.")

(defun org-yahoo-weather-check-interval (date)
  "Return t if DATE places between current day and current day plus 10 days.
Else return nil."
  (let* ((low-days (time-to-days (current-time)))
         (high-days (+ low-days 10))
         (days-of-date
          (calendar-absolute-from-gregorian
           date)))
    (and
     (>= days-of-date low-days)
     (< days-of-date high-days))))

(defun org-yahoo-weather-build-org-ret-string (data forecast)
  "Build and return forecast stirng for the agenda."
  (let ((condition (yahoo-weather-forecast->condition forecast))
        (low (yahoo-weather-forecast->low-temperature forecast))
        (high (yahoo-weather-forecast->high-temperature forecast))
        (city (yahoo-weather-data->city data)))
    (format-spec org-yahoo-weather-format
                 `((?c . ,condition)
                   (?l . ,low)
                   (?h . ,high)
                   (?C . ,city)
                   (?s . ,org-yahoo-weather-temperature-symbol)))))

;;;###autoload
(defun org-yahoo-weather (&optional location)
  "Return Org entry with the weathe for LOCATION.
If LOCATION is not set, use `org-yahoo-weather-location'."
  (when (org-yahoo-weather-check-interval (with-no-warnings date))
    (let* ((location (or location org-yahoo-weather-location))
           (data (ignore-errors
                   (yahoo-weather-get-data location
                                           org-yahoo-weather-cache-time)))
           (forecast (when data
                       (yahoo-weather-data->forecast-by-date
                        data (with-no-warnings date)))))
      (when forecast
        (org-yahoo-weather-build-org-ret-string data forecast)))))

(provide 'org-yahoo-weather)

;;; org-yahoo-weather.el ends here

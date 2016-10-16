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

(defcustom org-yandex-weather-cache-time 7200
  "Define how many seconds we should cache the weather forecast response from
the API server."
  :group 'org-yahoo-weather
  :type 'integer)

(defcustom org-yahoo-weather-display-icon-p t)

(provide 'org-yahoo-weather)

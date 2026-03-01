
(require 'calendar)

(defun string-to-cal-date-format (date-string)
  "Return (month day year) for date string DATE-STRING like \"01 March 2026\"."
  (let ((parts (parse-time-string date-string)))
    (list (nth 4 parts)
          (nth 3 parts)
          (nth 5 parts))))

(defun calendar-french-date-string (&optional date)
  "String of French Revolutionary date of Gregorian DATE.
Returns the empty string if DATE is pre-French Revolutionary.
Defaults to today's date if DATE is not given."
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (calendar-extract-year french-date))
         (m (calendar-extract-month french-date))
         (d (calendar-extract-day french-date)))
    (cond
     ((< y 1) "")
     (t (format
         "%d %s %d"
         d
         (aref calendar-french-month-name-array (1- m))
         y)))))

(defun filter-tags-from-title (title)
  "Remove any <i> tags from TITLE."
  (replace-regexp-in-string "<[^>]*>" "" title))

;;; utils.el ends here

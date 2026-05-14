
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

(defun org-static-blog-slug-from-filename (filename)
  "Generate a slug from FILENAME for use as og:image filename."
  (file-name-sans-extension (file-name-nondirectory filename)))

(defun org-static-blog-generate-og-image (post-filename output-dir)
  "Generate og:image for POST-FILENAME using ImageMagick.
Output is written to OUTPUT-DIR with filename derived from post filename.
Returns the relative path to the generated image, or nil on failure."
  (require 'org-static-blog)
  (let* ((post-filename (expand-file-name post-filename))
         (output-dir (expand-file-name output-dir))
         (title (org-static-blog-get-title post-filename))
         (date-string (org-static-blog-get-date-string post-filename))
         (slug (org-static-blog-slug-from-filename post-filename))
         (output-filename (concat slug ".png"))
         (output-path (concat-to-dir output-dir output-filename))
         (width (number-to-string org-static-blog-og-image-width))
         (height (number-to-string org-static-blog-og-image-height)))
    (unless (file-exists-p output-dir)
      (make-directory output-dir t))
    (call-process "convert" nil (get-buffer-create "*convert-output*") nil
                  "-size" (concat (number-to-string org-static-blog-og-image-width) "x" 
                                  (number-to-string org-static-blog-og-image-height))
                  (concat "xc:" org-static-blog-og-image-background)
                  "\("
                  "-size" "1000x400"
                  "-background" "none"
                  "-font" org-static-blog-og-image-font
                  "-pointsize" "60"
                  "-fill" org-static-blog-og-image-text-color
                  (concat "caption:" (filter-tags-from-title title))
                  "\)"
                  "-gravity" "NorthWest"
                  "-geometry" "+120+160"
                  "-composite"
                  
                  "-pointsize" "28"
                  "-font" org-static-blog-og-image-font
                  "-gravity" "SouthWest"
                  "-fill" org-static-blog-og-image-text-color
                  "-annotate" "+120+100" date-string
                  "\("
                  (expand-file-name (concat org-static-blog-publish-directory
                                            "/"
                                            (replace-regexp-in-string ".*/" "" org-static-blog-image)))
                  "-resize" "180x180"
                  "\)"
                  "-gravity" "SouthEast"
                  "-geometry" "+80+80"
                  "-composite"
                  (shell-quote-argument output-path))))

(defun org-static-blog-generate-all-og-images ()
  "Generate og:images for all published posts."
  (require 'org-static-blog)
  (let ((output-dir (concat-to-dir org-static-blog-publish-directory
                                   org-static-blog-og-image-directory)))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (org-static-blog-generate-og-image post-filename output-dir))
    (message "Generated og:images in %s" output-dir)))

(provide 'utils)

;;; utils.el ends here

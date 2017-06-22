;;; org-static-blog.el --- a simple org-mode based static blog generator

;; Author: Bastian Bechtold
;; URL: http://github.com/bastibe/org-static-blog
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Static blog generators are a dime a dozen. This is one more, which
;; focuses on being simple. All files are simple org-mode files in a
;; directory. The only requirement is that every org file must have a
;; #+TITLE and a #+DATE.

;; This file is also available from marmalade and melpa-stable.

;; Set up your blog by customizing org-static-blog's parameters, then
;; call M-x org-static-blog-publish to render the whole blog or
;; M-x org-static-blog-publish-file filename.org to render only only
;; the file filename.org.

;; Above all, I tried to make org-static-blog as simple as possible.
;; There are no magic tricks, and all of the source code is meant to
;; be easy to read, understand and modify.

;; If you have questions, if you find bugs, or if you would like to
;; contribute something to org-static-blog, please open an issue or
;; pull request on Github.

;; Finally, I would like to remind you that I am developing this
;; project for free, and in my spare time. While I try to be as
;; accomodating as possible, I can not guarantee a timely response to
;; issues. Publishing Open Source Software on Github does not imply an
;; obligation to *fix your problem right now*. Please be civil.

;;; Code:

(require 'ox-html)

(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.0.3"
  :group 'applications)

(defcustom org-static-blog-publish-url "http://example.com/"
  "URL of the blog."
  :group 'org-static-blog)

(defcustom org-static-blog-publish-title "Example.com"
  "Title of the blog."
  :group 'org-static-blog)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored."
  :group 'org-static-blog)

(defcustom org-static-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored.
When publishing, posts are rendered as HTML, and included in the
index, archive, and RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-drafts-directory "~/blog/drafts/"
  "Directory where unpublished ORG files are stored.
When publishing, draft are rendered as HTML, but not included in
the index, archive, or RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-index-file "index.html"
  "File name of the blog landing page.
The index page contains the most recent
`org-static-blog-index-length` full-text posts."
  :group 'org-static-blog)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :group 'org-static-blog)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog entries.
The archive page lists all posts as headlines."
  :group 'org-static-blog)

(defcustom org-static-blog-rss-file "rss.xml"
  "File name of the RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-page-header ""
  "HTML to put in the <head> of each page."
  :group 'org-static-blog)

(defcustom org-static-blog-page-preamble ""
  "HTML to put before the content of each page."
  :group 'org-static-blog)

(defcustom org-static-blog-page-postamble ""
  "HTML to put after the content of each page."
  :group 'org-static-blog)

;;;###autoload
(defun org-static-blog-publish ()
  "Render all blog entries, the index, archive, and RSS feed.
Only blog entries that changed since the HTML was created are
re-rendered."
  (interactive)
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (drafts (directory-files
                 org-static-blog-drafts-directory t ".*\\.org$" nil))
        (rebuild nil))
    (dolist (file (append posts drafts))
      (when (org-static-blog-needs-publishing-p file)
        (if (not (member file drafts))
            (setq rebuild t))
        (org-static-blog-publish-file file)))
    (when rebuild
      (org-static-blog-create-index)
      (org-static-blog-create-rss)
      (org-static-blog-create-archive))))

(defun org-static-blog-needs-publishing-p (post-filename)
  "Check whether POST-FILENAME was changed since last render."
  (let ((pub-filename
         (org-static-blog-matching-publish-filename post-filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))

(defun org-static-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for entry POST-FILENAME."
  (concat org-static-blog-publish-directory
          (file-name-base post-filename)
          ".html"))

;; This macro is needed for many of the following functions.
(defmacro org-static-blog-with-find-file (file &rest body)
  "Executes BODY within a new buffer that contains FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((buffer-existed (get-buffer (file-name-nondirectory ,file)))
           (buffer (find-file ,file)))
       ,@body
       (switch-to-buffer buffer)
       (save-buffer)
      (unless buffer-existed
        (kill-buffer buffer)))))

(defun org-static-blog-get-date (post-filename)
  "Extract the `#+date:` from entry POST-FILENAME."
  (let ((date nil))
    (org-static-blog-with-find-file
     post-filename
     (goto-char (point-min))
     (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$")
     (setq date (date-to-time (match-string 1))))
    date))

(defun org-static-blog-get-title (post-filename)
  "Extract the `#+title:` from entry POST-FILENAME."
  (let ((title nil))
    (org-static-blog-with-find-file
     post-filename
     (goto-char (point-min))
     (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$")
     (setq title (match-string 1)))
    title))

(defun org-static-blog-get-url (post-filename)
  "Generate a URL to entry POST-FILENAME."
  (concat org-static-blog-publish-url
          (file-name-nondirectory
           (org-static-blog-matching-publish-filename post-filename))))

;;;###autoload
(defun org-static-blog-publish-file (post-filename)
  "Publish a single entry POST-FILENAME.
The index page, archive page, and RSS feed are not updated."
  (interactive "f")
  (org-static-blog-with-find-file post-filename
   (org-export-to-file 'org-static-blog-post
       (org-static-blog-matching-publish-filename post-filename)
     nil nil nil nil nil)))

(defun org-static-blog-create-index ()
  "Re-render the blog index page.
The index page contains the last `org-static-blog-index-length`
entries as full text entries."
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (index-file (concat org-static-blog-publish-directory org-static-blog-index-file))
        (index-entries nil))
    (dolist (file posts)
      (org-static-blog-with-find-file
       file
       (let ((date (org-static-blog-get-date file))
             (title (org-static-blog-get-title file))
             (content (org-export-as 'org-static-blog-post-bare nil nil nil nil))
             (url (org-static-blog-get-url file)))
           (add-to-list 'index-entries (list date title url content)))))
    (org-static-blog-with-find-file
     index-file
     (erase-buffer)
     (insert
      (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<link rel=\"alternate\"
      type=\"appliation/rss+xml\"
      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"
      title=\"RSS feed for " org-static-blog-publish-url "\">
<title>" org-static-blog-publish-title "</title>"
org-static-blog-page-header
"</head>
<body>
<div id=\"preamble\" class=\"status\">"
org-static-blog-page-preamble
"</div>
<div id=\"content\">"))
     (setq index-entries (sort index-entries (lambda (x y) (time-less-p (nth 0 y) (nth 0 x)))))
     (dolist (idx (number-sequence 0 (1- (min org-static-blog-index-length
                                              (length index-entries)))))
       (let ((entry (nth idx index-entries)))
         (insert
          (concat "<div class=\"post-date\">" (format-time-string "%d %b %Y" (nth 0 entry)) "</div>"
                  "<h1 class=\"post-title\">"
                  "<a href=\"" (nth 2 entry) "\">" (nth 1 entry) "</a>"
                  "</h1>\n"
                  (nth 3 entry)))))
     (insert
"<div id=\"archive\">
  <a href=\"" org-static-blog-archive-file "\">Older posts</a>
</div>
</div>
</body>"))))

(defun org-static-blog-create-rss ()
  "Re-render the blog RSS feed.
The RSS-feed is an XML file that contains every blog entry in a
machine-readable format."
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (rss-file (concat org-static-blog-publish-directory org-static-blog-rss-file))
        (rss-entries nil))
    (dolist (file posts)
      (org-static-blog-with-find-file
       file
       (let ((rss-date (org-static-blog-get-date file))
             (rss-text (org-export-as 'org-static-blog-rss nil nil nil nil)))
       (add-to-list 'rss-entries (cons rss-date rss-text)))))
    (org-static-blog-with-find-file
     rss-file
     (erase-buffer)
     (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
<channel>
  <title>" org-static-blog-publish-title "</title>
  <description>" org-static-blog-publish-title "</description>
  <link>" org-static-blog-publish-url "</link>
  <lastBuildDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)) "</lastBuildDate>\n")
     (dolist (entry (sort rss-entries (lambda (x y) (time-less-p (car y) (car x)))))
       (insert (cdr entry)))
     (insert "</channel>
</rss>"))))

(defun org-static-blog-create-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog entry, but no entry body."
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (archive-file (concat org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil))
    (dolist (file posts)
      (org-static-blog-with-find-file
       file
       (let ((date (org-static-blog-get-date file))
             (title (org-static-blog-get-title file))
             (url (org-static-blog-get-url file)))
           (add-to-list 'archive-entries (list date title url)))))
    (org-static-blog-with-find-file
     archive-file
     (erase-buffer)
     (insert (concat
              "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<link rel=\"alternate\"
      type=\"appliation/rss+xml\"
      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"
      title=\"RSS feed for " org-static-blog-publish-url "\">
<title>" org-static-blog-publish-title "</title>"
org-static-blog-page-header
"</head>
<body>
<div id=\"preamble\" class=\"status\">"
org-static-blog-page-preamble
"</div>
<div id=\"content\">
<h1 class=\"title\">Archive</h1>\n"))
       (dolist (entry (sort archive-entries (lambda (x y) (time-less-p (car y) (car x)))))
         (insert
          (concat
           "<div class=\"post-date\">" (format-time-string "%d %b %Y" (nth 0 entry)) "</div>"
           "<h2 class=\"post-title\">"
           "<a href=\"" (nth 2 entry) "\">" (nth 1 entry) "</a>"
           "</h2>\n")))
       (insert "</body>\n </html>"))))

(org-export-define-derived-backend 'org-static-blog-post 'html
  :translate-alist '((template . org-static-blog-post-template)))

(defun org-static-blog-post-template (contents info)
  "Return complete document string after blog post conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (let ((title (org-export-data (plist-get info :title) info))
        (date (org-timestamp-format (car (plist-get info :date)) "%d %b %Y")))
    (concat
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<link rel=\"alternate\"
      type=\"appliation/rss+xml\"
      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"
      title=\"RSS feed for " org-static-blog-publish-url "\">
<title>" title "</title>"
org-static-blog-page-header
"</head>
<body>
<div id=\"preamble\" class=\"status\">"
org-static-blog-page-preamble
"</div>
<div id=\"content\">
<div class=\"post-date\">" date "</div>
<h1 class=\"post-title\">" title "</h1>\n"
contents
"</div>
<div id=\"postamble\" class=\"status\">"
org-static-blog-page-postamble
"</div>
</body>
</html>")))

(org-export-define-derived-backend 'org-static-blog-post-bare 'html
  :translate-alist '((template . org-static-blog-post-bare-template)))

(defun org-static-blog-post-bare-template (contents info)
  "Return complete document string after blog post conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
contents)

(org-export-define-derived-backend 'org-static-blog-rss 'html
  :translate-alist '((template . org-static-blog-rss-template)
                     (timestamp . (lambda (&rest args) ""))))

(defun org-static-blog-rss-template (contents info)
  "Return complete document string after rss entry conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (let ((url (concat org-static-blog-publish-url
                     (file-name-nondirectory
                      (org-static-blog-matching-publish-filename
                       (plist-get info :input-buffer))))))
    (concat
"<item>
  <title>" (org-export-data (plist-get info :title) info) "</title>
  <description><![CDATA["
  contents
  "]]></description>
  <link>" url "</link>
  <pubDate>" (org-timestamp-format (car (plist-get info :date)) "%a, %d %b %Y %H:%M:%S %z") "</pubDate>
</item>\n")))

(provide 'org-static-blog)

;;; org-static-blog.el ends here

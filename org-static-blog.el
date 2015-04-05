(require 'ox-html)

(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :group 'applications)

(defcustom org-static-blog-publish-url "http://example.com/"
  "URL of the blog"
  :group 'org-static-blog)

(defcustom org-static-blog-publish-title "Example.com"
  "Title of the blog"
  :group 'org-static-blog)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored"
  :group 'org-static-blog)

(defcustom org-static-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored"
  :group 'org-static-blog)

(defcustom org-static-blog-drafts-directory "~/blog/drafts/"
  "Directory where unpublished ORG files are stored"
  :group 'org-static-blog)

(defcustom org-static-blog-index-file "index.html"
  "File name of the blog landing page."
  :group 'org-static-blog)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :group 'org-static-blog)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog entries."
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

(defun org-static-blog-publish ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (drafts (directory-files
                 org-static-blog-drafts-directory t ".*\\.org$" nil))
        (rebuild nil))
    (dolist (file (concatenate 'list posts drafts))
      (when (org-static-blog-needs-publishing-p file)
        (if (not (member file drafts))
            (setq rebuild t))
        (org-static-blog-publish-file file)))
    (when rebuild
      (org-static-blog-create-index)
      (org-static-blog-create-rss)
      (org-static-blog-create-archive))))

(defun org-static-blog-needs-publishing-p (post-filename)
  (let ((pub-filename
         (org-static-blog-matching-publish-filename post-filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))

(defun org-static-blog-matching-publish-filename (post-filename)
  (concat org-static-blog-publish-directory
          (file-name-base post-filename)
          ".html"))

(defun org-static-blog-get-date (post-filename)
  (let ((date nil))
    (with-find-file
     post-filename
     (beginning-of-buffer)
     (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$")
     (setq date (date-to-time (match-string 1))))
    date))

(defun org-static-blog-get-title (post-filename)
  (let ((title nil))
    (with-find-file
     post-filename
     (beginning-of-buffer)
     (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$")
     (setq title (match-string 1)))
    title))

(defun org-static-blog-get-url (post-filename)
  (concat org-static-blog-publish-url
          (file-name-nondirectory
           (org-static-blog-matching-publish-filename post-filename))))

(defun org-static-blog-publish-file (post-filename)
  (with-find-file post-filename
   ;; This should really call a derived backend
   (org-export-to-file 'org-static-blog-post
       (org-static-blog-matching-publish-filename post-filename)
     nil nil nil nil nil)))

(defun org-static-blog-create-index ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (index-file (concat org-static-blog-publish-directory org-static-blog-index-file))
        (index-entries nil))
    (dolist (file posts)
      (with-find-file
       file
       (let ((date (org-static-blog-get-date file))
             (title (org-static-blog-get-title file))
             (content (org-export-as 'org-static-blog-post-bare nil nil nil nil))
             (url (org-static-blog-get-url file)))
           (add-to-list 'index-entries (list date title url content)))))
    (with-find-file
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
     (dolist (idx (number-sequence 0 (1- org-static-blog-index-length)))
       (let ((entry (nth idx index-entries)))
         (insert
          (concat "<div class=\"post-date\">" (format-time-string "%d %b %Y" (nth 0 entry)) "</div>"
                  "<h1 class=\"post-title\">"
                  "<a href=\"" (nth 2 entry) "\">" (nth 1 entry) "</a>"
                  "</h1>\n"
                  (nth 3 entry)))))
     (insert
"<div id=\"archive\">
  <a href=\"archive.html\">Older posts</a>
</div>
</div>
</body>"))))

(defun org-static-blog-create-rss ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (rss-file (concat org-static-blog-publish-directory org-static-blog-rss-file))
        (rss-entries nil))
    (dolist (file posts)
      (with-find-file
       file
       (let ((rss-date (org-static-blog-get-date file))
             (rss-text (org-export-as 'org-static-blog-rss nil nil nil nil)))
       (add-to-list 'rss-entries (cons rss-date rss-text)))))
    (with-find-file
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
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (archive-file (concat org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil))
    (dolist (file posts)
      (with-find-file
       file
       (let ((date (org-static-blog-get-date file))
             (title (org-static-blog-get-title file))
             (url (org-static-blog-get-url file)))
           (add-to-list 'archive-entries (list date title url)))))
    (with-find-file
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

(defmacro with-find-file (file &rest body)
  `(save-excursion
     (let ((buffer-existed (get-buffer (file-name-nondirectory ,file)))
           (buffer (find-file ,file)))
       ,@body
       (switch-to-buffer buffer)
       (save-buffer)
      (unless buffer-existed
        (kill-buffer buffer)))))

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

(setq org-static-blog-publish-title "Bastibe.de")
(setq org-static-blog-publish-url "http://bastibe.de/")
(setq org-static-blog-publish-directory "~/Projects/blog/")
(setq org-static-blog-posts-directory "~/Projects/blog/posts/")
(setq org-static-blog-drafts-directory "~/Projects/blog/drafts/")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(setq org-static-blog-page-header
"<meta  name=\"author\" content=\"Bastian Bechtold\" />
<link href='http://fonts.googleapis.com/css?family=Roboto&subset=latin' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono' rel='stylesheet' type='text/css'>
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"http://bastibe.de\">Basti's Scratchpad on the Internet</a>
  <div class=\"sitelinks\">
    <a href=\"http://alpha.app.net/bastibe\">alpha.app.net</a> | <a href=\"http://github.com/bastibe\">Github</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"archive.html\">Other posts</a>
</div>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
  var disqus_shortname = 'bastibe';
  (function() {
    var dsq = document.createElement('script');
    dsq.type = 'text/javascript';
    dsq.async = true;
    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
  })();
</script>
<noscript>Please enable JavaScript to view the
<a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
<center><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"http://purl.org/dc/terms/\" href=\"http://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

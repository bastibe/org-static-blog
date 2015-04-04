
(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :group 'applications)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored"
  :group 'org-static-blog)

(defcustom org-static-blog-posts-directory "~/Projects/blog/posts/"
  "Directory where published ORG files are stored"
  :group 'org-static-blog)

(defcustom org-static-blog-drafts-directory "~/Projects/blog/drafts/"
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

(defun org-static-blog-publish-file (post-filename)
  (with-find-file post-filename
   ;; This should really call a derived backend
   (org-export-to-file 'org-static-blog-post
       (org-static-blog-matching-publish-filename post-filename)
     nil nil nil nil nil)))

(defun org-static-blog-create-index ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (index nil))
    (dolist (file posts)
      (with-find-file
       file
       ;; not sure yet
       ))))

(defun org-static-blog-create-archive ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (archive nil))
    (dolist (file posts)
      (with-find-file
       file
       ;; not sure yet
       ))))

(defun org-static-blog-create-rss ()
  (let ((posts (directory-files
                org-static-blog-posts-directory t ".*\\.org$" nil))
        (rss nil))
    (dolist (file posts)
      (with-find-file
       file
       ;; not sure yet
       ))))

(defmacro with-find-file (file &rest body)
  `(save-excursion
     (let ((buffer-existed (get-buffer (file-name-nondirectory ,file)))
           (buffer (find-file ,file)))
       ,@body
      (unless buffer-existed
        (kill-buffer buffer)))))

(org-export-define-derived-backend 'org-static-blog-post 'html
  :translate-alist '((template . org-static-blog-post-template)))

(defun org-static-blog-post-template (contents info)
  "Return complete document string after blog post conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<title>" title "</title>
<meta  http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<meta  name=\"author\" content=\"Bastian Bechtold\" />
<link rel=\"alternate\" type=\"appliation/rss+xml\"
                href=\"http://bastibe.de/rss.xml\"
                title=\"RSS feed for bastibe.de\">
          <link href='http://fonts.googleapis.com/css?family=Roboto&subset=latin' rel='stylesheet' type='text/css'>
          <link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono' rel='stylesheet' type='text/css'>
          <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
          <link rel=\"icon\" href=\"static/favicon.ico\">
          <link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
          <link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
          <link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
          <title>Basti's Scratchpad on the Internet</title>
          <meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
          <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
</head>
<body>
<div id=\"preamble\" class=\"status\">
<div class=\"header\">
              <a href=\"http://bastibe.de\">Basti's Scratchpad on the Internet</a>
              <div class=\"sitelinks\">
                  <a href=\"http://alpha.app.net/bastibe\">alpha.app.net</a>  | <a href=\"http://github.com/bastibe\">Github</a>
              </div>
          </div>
</div>
<div id=\"content\">
<h1 class=\"title\">" title "</h1>\n"
contents
"</div>
<div id=\"postamble\" class=\"status\">
<div id=\"archive\"><a href=\"archive.html\">Other posts</a></div>
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
<center><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"http://purl.org/dc/terms/\" href=\"http://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>
</div>
</body>
</html>"
    )))

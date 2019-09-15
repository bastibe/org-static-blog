;;; org-static-blog.el --- a simple org-mode based static blog generator

;; Author: Bastian Bechtold
;; URL: https://github.com/bastibe/org-static-blog
;; Version: 1.2.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Static blog generators are a dime a dozen. This is one more, which
;; focuses on being simple. All files are simple org-mode files in a
;; directory. The only requirement is that every org file must have a
;; #+TITLE and a #+DATE, and optionally, #+FILETAGS.

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

(require 'org)
(require 'ox-html)

(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.2.1"
  :group 'applications)

(defcustom org-static-blog-publish-url "https://example.com/"
  "URL of the blog."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-publish-title "Example.com"
  "Title of the blog."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored."
  :group 'org-static-blog)

(defcustom org-static-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored.
When publishing, posts are rendered as HTML, and included in the
index, archive, tags, and RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-drafts-directory "~/blog/drafts/"
  "Directory where unpublished ORG files are stored.
When publishing, draft are rendered as HTML, but not included in
the index, archive, tags, or RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-index-file "index.html"
  "File name of the blog landing page.
The index page contains the most recent
`org-static-blog-index-length` full-text posts."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog posts.
The archive page lists all posts as headlines."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-tags-file "tags.html"
  "File name of the list of all blog posts by tag.
The tags page lists all posts as headlines."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-enable-tags nil
  "Show tags below posts, and generate tag pages."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-enable-deprecation-warning t
  "Show deprecation warnings."
  :group 'org-static-blog)

(defcustom org-static-blog-rss-file "rss.xml"
  "File name of the RSS feed."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-header ""
  "HTML to put in the <head> of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-preamble ""
  "HTML to put before the content of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-postamble ""
  "HTML to put after the content of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-langcode "en"
  "Language code for the blog content."
  :group 'org-static-blog
  :safe t)

;; localization support
(defconst org-static-blog-texts
  '((other-posts
     ("en" . "Other posts")
     ("pl" . "Pozostałe wpisy")
     ("ru" . "Другие посты"))
    (date-format
     ("en" . "%d %b %Y")
     ("pl" . "%Y-%m-%d")
     ("ru" . "%d.%m.%Y"))
    (tags
     ("en" . "Tags")
     ("pl" . "Tagi")
     ("ru" . "Тэги"))
    (archive
     ("en" . "Archive")
     ("pl" . "Archiwum")
     ("ru" . "Архив"))
    (posts-tagged
     ("en" . "Posts tagged")
     ("pl" . "Wpisy z tagiem")
     ("ru" . "Посты с тэгами"))
    (no-prev-post
     ("en" . "There is no previous post")
     ("pl" . "Poprzedni wpis nie istnieje")
     ("ru" . "Нет предыдущего поста"))
    (no-next-post
     ("en" . "There is no next post")
     ("pl" . "Następny wpis nie istnieje")
     ("ru" . "Нет следующего поста"))
    (title
     ("en" . "Title: ")
     ("pl" . "Tytuł: ")
     ("ru" . "Заголовок"))
    (filename
     ("en" . "Filename: ")
     ("pl" . "Nazwa pliku: ")
     ("ru" . "Имя файла: "))))

(defun org-static-blog-gettext (text-id)
  "Return localized text.
Depends on org-static-blog-langcode and org-static-blog-texts."
  (let* ((text-node (assoc text-id org-static-blog-texts))
	 (text-lang-node (if text-node
			     (assoc org-static-blog-langcode text-node)
			   nil)))
    (if text-lang-node
	(cdr text-lang-node)
      (concat "[" (symbol-name text-id) ":" org-static-blog-langcode "]"))))


;;;###autoload
(defun org-static-blog-publish (&optional force-render)
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered.

With a prefix argument, all blog posts are re-rendered
unconditionally."
  (interactive "P")
  (dolist (file (append (org-static-blog-get-post-filenames)
                        (org-static-blog-get-draft-filenames)))
    (when (or force-render (org-static-blog-needs-publishing-p file))
      (org-static-blog-publish-file file)))
  ;; don't spam too many deprecation warnings:
  (let ((org-static-blog-enable-deprecation-warning nil))
    (org-static-blog-assemble-index)
    (org-static-blog-assemble-rss)
    (org-static-blog-assemble-archive)
    (if org-static-blog-enable-tags
        (org-static-blog-assemble-tags))))

(defun org-static-blog-needs-publishing-p (post-filename)
  "Check whether POST-FILENAME was changed since last render."
  (let ((pub-filename
         (org-static-blog-matching-publish-filename post-filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))

(defun org-static-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (concat org-static-blog-publish-directory
          (file-name-base post-filename)
          ".html"))

(defun org-static-blog-get-post-filenames ()
  "Returns a list of all posts."
  (directory-files
   org-static-blog-posts-directory t ".*\\.org$" nil))

(defun org-static-blog-get-draft-filenames ()
  "Returns a list of all drafts."
  (directory-files
   org-static-blog-drafts-directory t ".*\\.org$" nil))

(defun org-static-blog-file-buffer (file)
  "Return the buffer open with a full filepath, or nil."
  (require 'seq)
  (car (seq-filter
         (lambda (buf)
           (string= (with-current-buffer buf buffer-file-name) file))
         (buffer-list))))

;; This macro is needed for many of the following functions.
(defmacro org-static-blog-with-find-file (file contents &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (org-static-blog-file-buffer ,file))
           (result nil)
	   (contents ,contents))
       (if buffer-exists
           (switch-to-buffer buffer-exists)
         (find-file ,file))
       (erase-buffer)
       (insert contents)
       (setq result (progn ,@body))
       (basic-save-buffer)
       (unless buffer-exists
         (kill-buffer))
       (switch-to-buffer current-buffer)
       result)))

(defun org-static-blog-get-date (post-filename)
  "Extract the `#+date:` from POST-FILENAME as date-time."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$" nil t)
	  (date-to-time (match-string 1))
	(time-since 0)))))

(defun org-static-blog-get-title (post-filename)
  "Extract the `#+title:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$")
      (match-string 1))))

(defun org-static-blog-get-tags (post-filename)
  "Extract the `#+filetags:` from POST-FILENAME as list of strings."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+filetags:[ ]*:\\(.*\\):$" nil t)
          (split-string (match-string 1) ":")
	(if (search-forward-regexp "^\\#\\+filetags:[ ]*\\(.+\\)$" nil t)
            (split-string (match-string 1))
	  )))))

(defun org-static-blog-get-tag-tree ()
  "Return an association list of tags to filenames.
e.g. `(('foo' 'file1.org' 'file2.org') ('bar' 'file2.org'))`"
  (let ((tag-tree '()))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (let ((tags (org-static-blog-get-tags post-filename)))
        (dolist (tag tags)
          (if (assoc-string tag tag-tree t)
              (push post-filename (cdr (assoc-string tag tag-tree t)))
            (push (cons tag (list post-filename)) tag-tree)))))
    tag-tree))

(defun org-static-blog-get-body (post-filename &optional exclude-title)
  "Get the rendered HTML body without headers from POST-FILENAME.
Preamble and Postamble are excluded, too."
  (with-temp-buffer
    (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
    (buffer-substring-no-properties
     (progn
       (goto-char (point-min))
       (if exclude-title
           (progn (search-forward "<h1 class=\"post-title\">")
                  (search-forward "</h1>"))
         (search-forward "<div id=\"content\">"))
       (point))
     (progn
       (goto-char (point-max))
       (search-backward "<div id=\"postamble\" class=\"status\">")
       (search-backward "</div>")
       (point)))))

(defun org-static-blog-get-url (post-filename)
  "Generate a URL to the published POST-FILENAME."
  (file-name-nondirectory
   (org-static-blog-matching-publish-filename post-filename)))

;;;###autoload
(defun org-static-blog-publish-file (post-filename)
  "Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated."
  (interactive "f")
  (org-static-blog-with-find-file
   (org-static-blog-matching-publish-filename post-filename)
   (concat
    "<!DOCTYPE html>\n"
    "<html lang=\"" org-static-blog-langcode "\">\n"
    "<head>\n"
    "<meta charset=\"UTF-8\">\n"
    "<link rel=\"alternate\"\n"
    "      type=\"application/rss+xml\"\n"
    "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
    "      title=\"RSS feed for " org-static-blog-publish-url "\"/>\n"
    "<title>" (org-static-blog-get-title post-filename) "</title>\n"
    org-static-blog-page-header
    "</head>\n"
    "<body>\n"
    "<div id=\"preamble\" class=\"status\">\n"
    org-static-blog-page-preamble
    "</div>\n"
    "<div id=\"content\">\n"
    (org-static-blog-post-preamble post-filename)
    (org-static-blog-render-post-content post-filename)
    (org-static-blog-post-postamble post-filename)
    "</div>\n"
    "<div id=\"postamble\" class=\"status\">"
    org-static-blog-page-postamble
    "</div>\n"
    "</body>\n"
    "</html>\n")))

(defun org-static-blog-render-post-content (post-filename)
  "Render blog content as bare HTML without header."
  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t))
    (save-excursion
      (let ((current-buffer (current-buffer))
	    (buffer-exists (org-static-blog-file-buffer post-filename))
	    (result nil))
	(if buffer-exists
	    (switch-to-buffer buffer-exists)
	  (find-file post-filename))
	(setq result
	      (org-export-as 'org-static-blog-post-bare nil nil nil nil))
	(basic-save-buffer)
	(unless buffer-exists
	  (kill-buffer))
	(switch-to-buffer current-buffer)
	result))))

(org-export-define-derived-backend 'org-static-blog-post-bare 'html
  :translate-alist '((template . (lambda (contents info) contents))))

(defun org-static-blog-assemble-index ()
  "Assemble the blog index page.
The index page contains the last `org-static-blog-index-length`
posts as full text posts."
  (let ((post-filenames (org-static-blog-get-post-filenames)))
    ;; reverse-sort, so that the later `last` will grab the newest posts
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
                                                                         (org-static-blog-get-date y)))))
    (org-static-blog-assemble-multipost-page
     (concat org-static-blog-publish-directory org-static-blog-index-file)
     (last post-filenames org-static-blog-index-length))))

(defun org-static-blog-assemble-multipost-page (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                  (org-static-blog-get-date x)))))
  (org-static-blog-with-find-file
   pub-filename
   (concat
    "<!DOCTYPE html>\n"
    "<html lang=\"" org-static-blog-langcode "\">\n"
    "<head>\n"
    "<meta charset=\"UTF-8\">\n"
    "<link rel=\"alternate\"\n"
    "      type=\"application/rss+xml\"\n"
    "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
    "      title=\"RSS feed for " org-static-blog-publish-url "\"/>\n"
    "<title>" org-static-blog-publish-title "</title>\n"
    org-static-blog-page-header
    "</head>\n"
    "<body>\n"
    "<div id=\"preamble\" class=\"status\">"
    org-static-blog-page-preamble
    "</div>\n"
    "<div id=\"content\">\n"
    (when front-matter front-matter)
    (apply 'concat (mapcar 'org-static-blog-get-body post-filenames))
    "<div id=\"archive\">\n"
    "<a href=\"" org-static-blog-archive-file "\">" (org-static-blog-gettext 'other-posts) "</a>\n"
    "</div>\n"
    "</div>\n"
    "</body>\n"
    "</html>\n")))

(defun org-static-blog-post-preamble (post-filename)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   "<div class=\"post-date\">" (format-time-string (org-static-blog-gettext 'date-format)
						   (org-static-blog-get-date post-filename))
   "</div>"
   "<h1 class=\"post-title\">"
   "<a href=\"" (org-static-blog-get-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h1>\n"))

(defun org-static-blog-post-postamble (post-filename)
  "Returns the tag list of the post.
This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
  (let ((taglist-content ""))
    (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
      (setq taglist-content (concat "<div class=\"taglist\">"
                                    "<a href=\""
                                    org-static-blog-tags-file
                                    "\">" (org-static-blog-gettext 'tags) "</a>: "))
      (dolist (tag (org-static-blog-get-tags post-filename))
        (setq taglist-content (concat taglist-content "<a href=\""
                                      "tag-" (downcase tag) ".html"
                                      "\">" tag "</a> ")))
      (setq taglist-content (concat taglist-content "</div>")))
    taglist-content))

(defun org-static-blog-assemble-rss ()
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((rss-filename (concat org-static-blog-publish-directory org-static-blog-rss-file))
        (rss-items nil))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (let ((rss-date (org-static-blog-get-date post-filename))
            (rss-text (org-static-blog-get-rss-item post-filename)))
        (add-to-list 'rss-items (cons rss-date rss-text))))
    (setq rss-items (sort rss-items (lambda (x y) (time-less-p (car y) (car x)))))
    (org-static-blog-with-find-file
     rss-filename
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	     "<rss version=\"2.0\">\n"
	     "<channel>\n"
	     "<title>" org-static-blog-publish-title "</title>\n"
	     "<description>" org-static-blog-publish-title "</description>\n"
	     "<link>" org-static-blog-publish-url "</link>\n"
	     "<lastBuildDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)) "</lastBuildDate>\n"
	     (apply 'concat (mapcar 'cdr rss-items))
	     "</channel>\n"
	     "</rss>\n"))))

(defun org-static-blog-get-rss-item (post-filename)
  "Assemble RSS item from post-filename.
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title>" (org-static-blog-get-title post-filename) "</title>\n"
   "  <description><![CDATA["
   (org-static-blog-get-body post-filename t) ; exclude headline!
   "]]></description>\n"
   (let ((categories ""))
     (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
       (dolist (tag (org-static-blog-get-tags post-filename))
         (setq categories (concat categories
                                  "  <category>" tag "</category>\n"))))
     categories)
   "  <link>"
   (concat org-static-blog-publish-url
           (file-name-nondirectory
            (org-static-blog-matching-publish-filename
             post-filename)))
   "</link>\n"
   "  <pubDate>"
   (format-time-string "%a, %d %b %Y %H:%M:%S %z" (org-static-blog-get-date post-filename))
   "</pubDate>\n"
   "</item>\n"))

(defun org-static-blog-assemble-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (concat org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil)
        (post-filenames (org-static-blog-get-post-filenames)))
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p
                                                        (org-static-blog-get-date y)
                                                        (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     archive-filename
     (concat
      "<!DOCTYPE html>\n"
      "<html lang=\"" org-static-blog-langcode "\">\n"
      "<head>\n"
      "<meta charset=\"UTF-8\">\n"
      "<link rel=\"alternate\"\n"
      "      type=\"application/rss+xml\"\n"
      "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      org-static-blog-page-header
      "</head>\n"
      "<body>\n"
      "<div id=\"preamble\" class=\"status\">\n"
      org-static-blog-page-preamble
      "</div>\n"
      "<div id=\"content\">\n"
      "<h1 class=\"title\">" (org-static-blog-gettext 'archive) "</h1>\n"
      (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames))
      "</body>\n"
      "</html>"))))

(defun org-static-blog-get-post-summary (post-filename)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<div class=\"post-date\">"
   (format-time-string (org-static-blog-gettext 'date-format) (org-static-blog-get-date post-filename))
   "</div>"
   "<h2 class=\"post-title\">"
   "<a href=\"" (org-static-blog-get-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h2>\n"))

(defun org-static-blog-assemble-tags ()
  "Render the tag archive and tag pages."
  (org-static-blog-assemble-tags-archive)
  (dolist (tag (org-static-blog-get-tag-tree))
    (org-static-blog-assemble-multipost-page
     (concat org-static-blog-publish-directory "tag-" (downcase (car tag)) ".html")
     (cdr tag)
     (concat "<h1 class=\"title\">" (org-static-blog-gettext 'posts-tagged) " \"" (car tag) "\":</h1>"))))

(defun org-static-blog-assemble-tags-archive-tag (tag)
  "Assemble single TAG for all filenames."
  (let ((post-filenames (cdr tag)))
    (setq post-filenames
	  (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
						     (org-static-blog-get-date y)))))
    (concat "<h1 class=\"tags-title\">" (org-static-blog-gettext 'posts-tagged) " \"" (downcase (car tag)) "\":</h1>\n"
	    (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames)))))

(defun org-static-blog-assemble-tags-archive ()
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (let ((tags-archive-filename (concat org-static-blog-publish-directory org-static-blog-tags-file))
        (tag-tree (org-static-blog-get-tag-tree)))
    (setq tag-tree (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
    (org-static-blog-with-find-file
     tags-archive-filename
     (concat
      "<!DOCTYPE html>\n"
      "<html lang=\"" org-static-blog-langcode "\">\n"
      "<head>\n"
      "<meta charset=\"UTF-8\">\n"
      "<link rel=\"alternate\"\n"
      "      type=\"application/rss+xml\"\n"
      "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      org-static-blog-page-header
      "</head>\n"
      "<body>\n"
      "<div id=\"preamble\" class=\"status\">"
      org-static-blog-page-preamble
      "</div>\n"
      "<div id=\"content\">\n"
      "<h1 class=\"title\">" (org-static-blog-gettext 'tags) "</h1>\n"
      (apply 'concat (mapcar 'org-static-blog-assemble-tags-archive-tag tag-tree))
      "</body>\n"
      "</html>\n"))))

(defun org-static-blog-open-previous-post ()
  "Opens previous blog post."
  (interactive)
  (let ((posts (sort (org-static-blog-get-post-filenames)
                     (lambda (x y)
                       (time-less-p (org-static-blog-get-date y)
                                    (org-static-blog-get-date x)))))
        (current-post (buffer-file-name)))
    (while (and posts
                (not (string-equal
                      (file-name-nondirectory current-post)
                      (file-name-nondirectory (car posts)))))
      (setq posts (cdr posts)))
    (if (> (list-length posts) 1)
        (find-file (cadr posts))
      (message (org-static-blog-gettext 'no-prev-post)))))

(defun org-static-blog-open-next-post ()
  "Opens next blog post."
  (interactive)
  (let ((posts (sort (org-static-blog-get-post-filenames)
                     (lambda (x y)
                       (time-less-p (org-static-blog-get-date x)
                                    (org-static-blog-get-date y)))))
        (current-post (buffer-file-name)))
    (while (and posts
                (not (string-equal
                      (file-name-nondirectory current-post)
                      (file-name-nondirectory (car posts)))))
      (setq posts (cdr posts)))
    (if (> (list-length posts) 1)
        (find-file (cadr posts))
      (message (org-static-blog-gettext 'no-next-post)))))

(defun org-static-blog-open-matching-publish-file ()
  "Opens HTML for post."
  (interactive)
  (find-file (org-static-blog-matching-publish-filename (buffer-file-name))))

;;;###autoload
(defun org-static-blog-create-new-post ()
  "Creates a new blog post.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose."
  (interactive)
  (let ((title (read-string (org-static-blog-gettext 'title))))
    (find-file (concat
                org-static-blog-posts-directory
                (read-string (org-static-blog-gettext 'filename)
			     (concat (format-time-string "%Y-%m-%d-" (current-time))
				     (replace-regexp-in-string "\s" "-" (downcase title))
				     ".org"))))
    (insert "#+title: " title "\n"
            "#+date: " (format-time-string "<%Y-%m-%d %H:%M>") "\n"
            "#+filetags: ")))

;;;###autoload
(define-derived-mode org-static-blog-mode org-mode "OSB"
  "Blogging with org-mode and emacs."
  (run-mode-hooks)
  :group 'org-static-blog)

;; Key bindings
(define-key org-static-blog-mode-map (kbd "C-c C-f") 'org-static-blog-open-next-post)
(define-key org-static-blog-mode-map (kbd "C-c C-b") 'org-static-blog-open-previous-post)
(define-key org-static-blog-mode-map (kbd "C-c C-p") 'org-static-blog-open-matching-publish-file)
(define-key org-static-blog-mode-map (kbd "C-c C-n") 'org-static-blog-create-new-post)

(provide 'org-static-blog)

;;; org-static-blog.el ends here

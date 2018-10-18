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
  :group 'org-static-blog)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :group 'org-static-blog)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog posts.
The archive page lists all posts as headlines."
  :group 'org-static-blog)

(defcustom org-static-blog-tags-file "tags.html"
  "File name of the list of all blog posts by tag.
The tags page lists all posts as headlines."
  :group 'org-static-blog)

(defcustom org-static-blog-enable-tags nil
  "Show tags below posts, and generate tag pages."
  :group 'org-static-blog)

(defcustom org-static-blog-enable-deprecation-warning t
  "Show deprecation warnings."
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

(defcustom org-static-blog-post-postamble ""
  "HTML to put after the content of each post (e.g. comments)."
  :group 'org-static-blog)

(defcustom org-static-blog-page-postamble ""
  "HTML to put after the content of each page."
  :group 'org-static-blog)

(defcustom org-static-blog-langcode "en"
  "Language code for the blog content."
  :group 'org-static-blog)

;;;###autoload
(defun org-static-blog-publish-posts (posts &optional is_post)
  (dolist (post posts)
    (when (org-static-blog-needs-publishing-p post)
      (org-static-blog-publish-post post is_post))
  ))

(defun org-static-blog-publish ()
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered."
  (interactive)
  (org-static-blog-publish-posts
   (org-static-blog-get-posts org-static-blog-drafts-directory))
  (let ((posts (org-static-blog-get-posts )))
    (org-static-blog-publish-posts posts t)
    ;; don't spam too many deprecation warnings:
    (let ((org-static-blog-enable-deprecation-warning nil))
      (org-static-blog-assemble-index posts)
      (org-static-blog-assemble-rss posts)
      (org-static-blog-assemble-archive posts)
      (if org-static-blog-enable-tags
          (org-static-blog-assemble-tags posts)))))

(defun org-static-blog-needs-publishing-p (post)
  "Check whether POST was changed since last render."
  (let ((pub-filename (plist-get post :publish-filename))
        (post-filename (plist-get post :filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))

(defun org-static-blog-regexp (regexp)
  "Returns first occurrence of REGEXP in current buffer"
  (goto-char (point-min))
  (search-forward-regexp regexp)
  (match-string 1))

(defun org-static-blog-get-post-data (post-filename)
  "Reads blog file and processes it"
  (let ((publish-filename (org-static-blog-matching-publish-filename post-filename)))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (list :filename post-filename
            :contents (buffer-string)
            :title (org-static-blog-regexp "^\\#\\+title:[ ]*\\(.+\\)$")
            :date (date-to-time
                   (org-static-blog-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$"))
            :tags (split-string (org-static-blog-regexp "^\\#\\+filetags:[ ]*\\(.+\\)$"))
            :publish-filename publish-filename
            :url (file-name-nondirectory publish-filename)
    ))))

(defun org-static-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (concat org-static-blog-publish-directory
          (file-name-base post-filename)
          ".html"))

(defun org-static-blog-get-filenames (&optional directory)
  "Returns a list of all posts file names from DIRECTORY."
  (directory-files (or directory org-static-blog-posts-directory) t ".*\\.org$" nil))

(defun org-static-blog-get-posts (&optional directory)
  "Returns a list of all posts from DIRECTORY"
  (mapcar 'org-static-blog-get-post-data (org-static-blog-get-filenames directory)))

;; This macro is needed for many of the following functions.
(defmacro org-static-blog-with-find-file (file &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (get-buffer (file-name-nondirectory ,file)))
           (result nil))
       (if buffer-exists
           (switch-to-buffer buffer-exists)
         (find-file ,file))
       (setq result (progn ,@body))
       (basic-save-buffer)
      (unless buffer-exists
        (kill-buffer))
      (switch-to-buffer current-buffer)
      result)))

(defun org-static-blog-html-start (&optional title)
  (concat
   "<!DOCTYPE html>\n"
   "<html lang=\"" org-static-blog-langcode "\">\n"
   "<head>\n"
   "<meta charset=\"UTF-8\">\n"
   "<link rel=\"alternate\"\n"
   "      type=\"application/rss+xml\"\n"
   "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
   "      title=\"RSS feed for " org-static-blog-publish-url "\"/>\n"
   "<title>" (or title org-static-blog-publish-title) "</title>\n"
   org-static-blog-page-header
   "</head>\n"
   "<body>\n"
   "<div id=\"preamble\" class=\"status\">\n"
   org-static-blog-page-preamble
   "</div>\n"
   "<div id=\"content\">\n"
   (if title (concat "<h1 class=\"title\">" title "</h1>\n"))))

(defun org-static-blog-html-end (&optional postamble)
      (concat
       "</div>\n"
       "<div id=\"postamble\" class=\"status\">"
       (or postamble org-static-blog-page-postamble)
       "</div>\n"
       "</body>\n"
       "</html>\n"))

;;;###autoload
(defun org-static-blog-publish-file (post-filename &optional is_post)
  (interactive "f\nP")
  (org-static-blog-publish-post (org-static-blog-get-post-data post-filename) is_post))

(defun org-static-blog-publish-post (post &optional is_post)
  "Publish a single POST.
IS_POST is true if this post is not a draft.
The index, archive, tags, and RSS feed are not updated."
  (let ((publish-filename (plist-get post :publish-filename)))
    (org-static-blog-with-find-file
     publish-filename
     (erase-buffer)
     (insert
      (org-static-blog-html-start (plist-get post :title))
      (org-static-blog-post-preamble post is_post)
      (org-static-blog-render-post-content post)
      (org-static-blog-post-postamble post is_post)
      (org-static-blog-html-end)))))

(defun org-static-blog-render-post-content (post)
  "Render blog content as bare HTML without header."
  (unless (plist-get post :html-contents)
    (plist-put post :html-contents
               (let ((org-html-doctype "html5")
                     (org-html-html5-fancy t))
                 (org-static-blog-with-find-file
                  (plist-get post :filename)
                  (org-export-as 'org-static-blog-post-bare nil nil nil nil)))))
  (plist-get post :html-contents))

(org-export-define-derived-backend 'org-static-blog-post-bare 'html
  :translate-alist '((template . (lambda (contents info) contents))))

(defun org-static-blog-assemble-index (posts)
  "Assemble the blog index page.
The index page contains the last `org-static-blog-index-length`
posts as full text posts."
  ;; reverse-sort, so that the later `last` will grab the newest posts
  (org-static-blog-assemble-multipost-page
   (concat org-static-blog-publish-directory org-static-blog-index-file)
   (last (org-static-blog-sort-posts posts t) org-static-blog-index-length)))

(defun org-static-blog-sort-posts (posts &optional chronological)
  "Returns an a-chronologiccally sorted copy of POSTS"
  (let ((copy (copy-sequence posts)))
    (if chronological
        (sort copy (lambda (x y) (time-less-p (plist-get x :date)
                                              (plist-get y :date))))
      (sort copy (lambda (x y) (time-less-p (plist-get y :date)
                                            (plist-get x :date)))))))

(defun org-static-blog-render-post-title (post &optional link element)
  (let ((element (or element "h1")))
    (concat
     "<" element " class=\"post-title\">"
     (if link
         (concat "<a href=\"" (plist-get post :url) "\">"
                 (plist-get post :title) "</a>")
       (plist-get post :title))
     "</" element ">\n")))

(defun org-static-blog-render-post-date (post)
  (concat
   "<div class=\"post-date\">"
   (format-time-string "%d %b %Y" (plist-get post :date))
   "</div>"))

(defun org-static-blog-render-post-taglist (post)
  "Returns the tag list of the post."
  (let ((taglist-content "")
        (tags (plist-get post :tags)))
    (when (and tags org-static-blog-enable-tags)
      (setq taglist-content (concat "<div class=\"taglist\">"
                                    "<a href=\""
                                    org-static-blog-tags-file
                                    "\">Tags:</a> "))
      (dolist (tag tags)
        (setq taglist-content (concat taglist-content "<a href=\""
                                      "tag-" (downcase tag) ".html"
                                      "\">" tag "</a> ")))
      (setq taglist-content (concat taglist-content "</div>")))
    taglist-content))

(defun org-static-blog-assemble-multipost-page (pub-filename posts &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (org-static-blog-with-find-file
   pub-filename
   (erase-buffer)
   (insert
    (org-static-blog-html-start front-matter)
    (string-join (mapcar 'org-static-blog-get-post-summary (org-static-blog-sort-posts posts)))
    "<div id=\"archive\">\n"
    "<a href=\"" org-static-blog-archive-file "\">Other posts</a>\n"
    "</div>\n"
    (org-static-blog-html-end))))

(defun org-static-blog-post-preamble (post &optional is_post)
  "Returns the formatted date and headline of the post.
IS_POST is true if this post is not a draft.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   (org-static-blog-render-post-date post)
   (org-static-blog-render-post-title post is_post)))

(defun org-static-blog-post-postamble (post &optional is_post)
  "Returns the tag list of the post.
IS_POST is true if this post is not a draft.
This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
  (org-static-blog-render-post-taglist post))

(defun org-static-blog-assemble-rss (posts)
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((rss-filename (concat org-static-blog-publish-directory
                              org-static-blog-rss-file))
        (rss-items nil))
    (dolist (post posts)
      (add-to-list 'rss-items (cons (plist-get post :date)
                                    (org-static-blog-get-rss-item post))))
    (org-static-blog-with-find-file
     rss-filename
     (erase-buffer)
     (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<rss version=\"2.0\">\n"
             "<channel>\n"
             "<title>" org-static-blog-publish-title "</title>\n"
             "<description>" org-static-blog-publish-title "</description>\n"
             "<link>" org-static-blog-publish-url "</link>\n"
             "<lastBuildDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)) "</lastBuildDate>\n")
     (dolist (item (sort rss-items (lambda (x y) (time-less-p (car y) (car x)))))
       (insert (cdr item)))
     (insert "</channel>\n"
             "</rss>\n"))))

(defun org-static-blog-get-rss-item (post)
  "Assemble RSS item from POST
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title>" (plist-get post :title) "</title>\n"
   "  <description><![CDATA["
   (org-static-blog-render-post-content post)
   "]]></description>\n"
   (let ((categories "")
         (tags (plist-get post :tags)))
     (when (and tags org-static-blog-enable-tags)
       (dolist (tag tags)
         (setq categories (concat categories "  <category>" tag "</category>\n"))))
     categories)
   "  <link>"
   (concat org-static-blog-publish-url
           (file-name-nondirectory
            (plist-get post :publish-filename)))
   "</link>\n"
   "  <pubDate>"
   (format-time-string "%a, %d %b %Y %H:%M:%S %z" (plist-get post :date))
   "</pubDate>\n"
   "</item>\n"))

(defun org-static-blog-assemble-archive (posts)
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (concat org-static-blog-publish-directory
                                  org-static-blog-archive-file))
        (archive-entries nil))
    (org-static-blog-with-find-file
     archive-filename
     (erase-buffer)
     (insert
      (org-static-blog-html-start "Archive")
      (org-static-blog-get-posts-summary posts)
      (org-static-blog-html-end)))))

(defun org-static-blog-get-posts-summary (posts &optional chronological)
   (mapconcat 'org-static-blog-get-post-summary
              (org-static-blog-sort-posts posts chronological)
              "\n"))

(defun org-static-blog-get-post-summary (post &optional element)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   (org-static-blog-render-post-date post)
   (org-static-blog-render-post-title post t (or element "h3"))))

(defun org-static-blog-get-tag-tree (posts)
  "Return an association list of tags to posts.
e.g. `(('foo' 'file1.org' 'file2.org') ('bar' 'file2.org'))`"
  (let ((tag-tree nil))
    (dolist (post posts)
      (dolist (tag (plist-get post :tags))
        (let ((tag-posts (assoc-string tag tag-tree t)))
          (if tag-posts
              (push post (cdr tag-posts))
            (push (cons tag (list post)) tag-tree)))))
    tag-tree))

(defun org-static-blog-assemble-tags (posts)
  "Render the tag archive and tag pages."
  (let ((tag-tree (org-static-blog-get-tag-tree posts)))
    (org-static-blog-assemble-tags-archive tag-tree)
    (dolist (tag tag-tree)
      (org-static-blog-assemble-multipost-page
       (concat org-static-blog-publish-directory "tag-" (downcase (car tag)) ".html")
       (cdr tag)
       (concat "Posts tagged \"" (car tag) "\"")))))

(defun org-static-blog-assemble-tags-archive (tag-tree)
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (let ((tags-archive-filename (concat org-static-blog-publish-directory
                                       org-static-blog-tags-file)))
    (org-static-blog-with-find-file
     tags-archive-filename
     (erase-buffer)
     (insert (org-static-blog-html-start "Tags"))
     (dolist (tag (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
       (insert
        "<h2 class=\"tags-title\">Posts tagged \""
        (downcase (car tag))
        "\"</h2>\n"
        (org-static-blog-get-posts-summary (cdr tag))))
     (insert (org-static-blog-html-end)))))

(defun org-static-blog-goto-first-other (posts message)
  (let ((current-post (file-name-nondirectory (buffer-file-name))))
    (while (and posts
                (not (string-equal
                      current-post
                      (file-name-nondirectory (plist-get (car posts) :filename)))))
      (setq posts (cdr posts)))
    (if (> (list-length posts) 1)
        (find-file (plist-get (cadr posts) :filename))
      (message "There is no %s post" message))))

(defun org-static-blog-open-previous-post ()
  "Opens previous blog post."
  (interactive)
  (org-static-blog-goto-first-other
   (org-static-blog-sort-posts (org-static-blog-get-posts))
   "previous"))

(defun org-static-blog-open-next-post ()
  "Opens next blog post."
  (interactive)
  (org-static-blog-goto-first-other
   (org-static-blog-sort-posts (org-static-blog-get-posts) t)
   "next"))

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
  (let ((title (read-string "Title: ")))
    (find-file (concat
                org-static-blog-posts-directory
                (read-string "Filename: "
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

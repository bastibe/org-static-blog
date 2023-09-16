;;; org-static-blog.el --- a simple org-mode based static blog generator

;; Author: Bastian Bechtold
;; Contrib: Shmavon Gazanchyan, Rafał -rsm- Marek, neeasade,
;; Michael Cardell Widerkrantz, Matthew Bauer, Winny, Yauhen Makei,
;; luhuaei, zngguvnf, Qiantan Hong, Jonas Bernoulli, Théo Jacquin,
;; K. Scarlet, zsxh
;; URL: https://github.com/bastibe/org-static-blog
;; Version: 1.6.0
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

(require 'cl-extra)
(require 'org)
(require 'ox-html)

(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.6.0"
  :group 'applications)

(defcustom org-static-blog-publish-url "https://example.com/"
  "URL of the blog."
  :type '(string)
  :safe t)

(defcustom org-static-blog-publish-title "Example.com"
  "Title of the blog."
  :type '(string)
  :safe t)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored."
  :type '(directory))

(defcustom org-static-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored.
When publishing, posts are rendered as HTML, and included in the
index, archive, tags, and RSS feed."
  :type '(directory))

(defcustom org-static-blog-drafts-directory "~/blog/drafts/"
  "Directory where unpublished ORG files are stored.
When publishing, draft are rendered as HTML, but not included in
the index, archive, tags, or RSS feed."
  :type '(directory))

(defcustom org-static-blog-index-file "index.html"
  "File name of the blog landing page.
The index page contains the most recent
`org-static-blog-index-length` full-text posts."
  :type '(string)
  :safe t)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :type '(integer)
  :safe t)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog posts.
The archive page lists all posts as headlines."
  :type '(string)
  :safe t)

(defcustom org-static-blog-tags-file "tags.html"
  "File name of the list of all blog posts by tag.
The tags page lists all posts as headlines."
  :type '(string)
  :safe t)

(defcustom org-static-blog-enable-tags nil
  "Show tags below posts, and generate tag pages."
  :group 'org-static-blog
  :type '(boolean)
  :safe t)

(defcustom org-static-blog-enable-deprecation-warning t
  "Show deprecation warnings."
  :type '(boolean))

(defcustom org-static-blog-rss-file "rss.xml"
  "File name of the RSS feed."
  :type '(string)
  :safe t)

(defcustom org-static-blog-rss-excluded-tag nil
  "Posts with this tag won't be included in the RSS feeds."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Tag name"))
  :safe t)

(defcustom org-static-blog-no-comments-tag nil
  "Posts with this tag won't include comments."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Tag name"))
  :safe t)

(defcustom org-static-blog-rss-extra ""
  "Extra information for the RSS feed header.
This information is placed right before the sequence of posts.
You can add an icon for the feed, or advertise that you built
your blog with emacs, org-mode and org-static-blog.
"
  :type '(string)
  :safe t)

(defcustom org-static-blog-rss-max-entries nil
  "Maximum number of entries in the RSS feed.
If nil (the default), all existing posts are included."
  :type '(choice (const nil) integer)
  :safe t)

(defcustom org-static-blog-enable-tag-rss nil
  "Whether to generate per tag RSS feeds.

When this flag is set, an RSS file with name given by prefixing
`org-static-blog-rss-file' with '<tag>-' is created for each
existing tag.  The options `org-static-blog-rss-extra',
`org-static-blog-rss-max-entries' and
`org-static-blog-rss-excluded-tag' are also used to construct
per-tag RSS feeds."
  :type '(boolean))

(defcustom org-static-blog-page-header ""
  "HTML to put in the <head> of each page."
  :type '(string)
  :safe t)

(defcustom org-static-blog-page-preamble ""
  "HTML to put before the content of each page."
  :type '(string)
  :safe t)

(defcustom org-static-blog-page-postamble ""
  "HTML to put after the content of each page."
  :type '(string)
  :safe t)

(defcustom org-static-blog-index-front-matter ""
  "HTML to put at the beginning of the index page."
  :type '(string)
  :safe t)

(defcustom org-static-blog-post-comments ""
  "HTML code for comments to put after each blog post."
  :type '(string)
  :safe t)

(defcustom org-static-blog-langcode "en"
  "Language code for the blog content."
  :type '(string)
  :safe t)

(defcustom org-static-blog-use-preview nil
  "Use preview versions of posts on multipost pages.

See also `org-static-blog-preview-start',
`org-static-blog-preview-end', `org-static-blog-preview-ellipsis'
and `org-static-blog-preview-link-p'."
  :type '(boolean)
  :safe t)

(defcustom org-static-blog-preview-start nil
  "Marker indicating the beginning of a post's preview.

When set to nil, we look for the first occurence of <p> in the
generated HTML.  See also `org-static-blog-preview-end'."
  :type '(choice (const :tag "First paragraph" nil) (string))
  :safe t)

(defcustom org-static-blog-preview-end nil
  "Marker indicating the end of a post's preview.

When set to nil, we look for the first occurence of </p> after
`org-static-blog-preview-start' (or the first <p> if that is nil)
in the generated HTML."
  :type '(choice (const :tag "First paragraph" nil) (string))
  :safe t)

(defcustom org-static-blog-preview-convert-titles t
  "When preview is enabled, convert <h1> to <h2> for the previews."
  :type '(boolean)
  :safe t)

(defcustom org-static-blog-preview-ellipsis "(...)"
  "The HTML appended to the preview if some part of the post is hidden.

The contents shown in the preview is determined by the values of
the variables `org-static-blog-preview-start' and
`org-static-blog-preview-end'."
  :type '(string)
  :safe t)

(defcustom org-static-blog-no-post-tag "nonpost"
  "Do not pushlish the subtree with this tag or property."
  :type '(string)
  :safe t)

(defcustom org-static-blog-preview-link-p nil
  "Whether to make the preview ellipsis a link to the article's page."
  :type '(boolean)
  :safe t)

(defcustom org-static-blog-preview-date-first-p nil
  "If t, print post dates before title in the preview view."
  :type '(boolean)
  :safe t)

(defcustom org-static-blog-suggested-filename-cleaning-regexp "\s"
  "Regexp used to clean the suggested filename."
  :type'(string)
  :safe t)

;; localization support
(defconst org-static-blog-texts
  '((other-posts
     ("en" . "Other posts")
     ("pl" . "Pozostałe wpisy")
     ("ru" . "Другие публикации")
     ("by" . "Іншыя публікацыі")
     ("it" . "Altri articoli")
     ("es" . "Otros artículos")
     ("fr" . "Autres articles")
     ("zh" . "其他帖子")
     ("ja" . "他の投稿"))
    (date-format
     ("en" . "%d %b %Y")
     ("pl" . "%Y-%m-%d")
     ("ru" . "%d.%m.%Y")
     ("by" . "%d.%m.%Y")
     ("it" . "%d/%m/%Y")
     ("es" . "%d/%m/%Y")
     ("fr" . "%d-%m-%Y")
     ("zh" . "%Y-%m-%d")
     ("ja" . "%Y/%m/%d"))
    (tags
     ("en" . "Tags")
     ("pl" . "Tagi")
     ("ru" . "Ярлыки")
     ("by" . "Ярлыкі")
     ("it" . "Categorie")
     ("es" . "Categoría")
     ("fr" . "Tags")
     ("zh" . "标签")
     ("ja" . "タグ"))
    (archive
     ("en" . "Archive")
     ("pl" . "Archiwum")
     ("ru" . "Архив")
     ("by" . "Архіў")
     ("it" . "Archivio")
     ("es" . "Archivo")
     ("fr" . "Archive")
     ("zh" . "归档")
     ("ja" . "アーカイブ"))
    (posts-tagged
     ("en" . "Posts tagged")
     ("pl" . "Wpisy z tagiem")
     ("ru" . "Публикации с ярлыками")
     ("by" . "Публікацыі")
     ("it" . "Articoli nella categoria")
     ("es" . "Artículos de la categoría")
     ("fr" . "Articles tagués")
     ("zh" . "打标签的帖子")
     ("ja" . "タグ付けされた投稿"))
    (no-prev-post
     ("en" . "There is no previous post")
     ("pl" . "Poprzedni wpis nie istnieje")
     ("ru" . "Нет предыдущей публикации")
     ("by" . "Няма папярэдняй публікацыі")
     ("it" . "Non c'è nessun articolo precedente")
     ("es" . "No existe un artículo precedente")
     ("fr" . "Il n'y a pas d'article précédent")
     ("zh" . "无更旧的帖子")
     ("ja" . "前の投稿はありません"))
    (no-next-post
     ("en" . "There is no next post")
     ("pl" . "Następny wpis nie istnieje")
     ("ru" . "Нет следующей публикации")
     ("by" . "Няма наступнай публікацыі")
     ("it" . "Non c'è nessun articolo successivo")
     ("es" . "No hay artículo siguiente")
     ("fr" . "Il n'y a pas d'article suivants")
     ("zh" . "无更新的帖子")
     ("ja" . "次の投稿はありません"))
    (title
     ("en" . "Title: ")
     ("pl" . "Tytuł: ")
     ("ru" . "Заголовок: ")
     ("by" . "Загаловак: ")
     ("it" . "Titolo: ")
     ("es" . "Título: ")
     ("fr" . "Titre : ")
     ("zh" . "标题：")
     ("ja" . "タイトル: "))
    (filename
     ("en" . "Filename: ")
     ("pl" . "Nazwa pliku: ")
     ("ru" . "Имя файла: ")
     ("by" . "Імя файла: ")
     ("it" . "Nome del file: ")
     ("es" . "Nombre del archivo: ")
     ("fr" . "Nom du fichier :")
     ("zh" . "文件名：")
     ("ja" . "ファイル名: "))))

(defun concat-to-dir (dir filename)
  "Concat filename to another path interpreted as a directory."
  (concat (file-name-as-directory dir) filename))

(defun org-static-blog-template (tTitle tContent &optional tDescription)
  "Create the template that is used to generate the static pages."
  (concat
   "<!DOCTYPE html>\n"
   "<html lang=\"" org-static-blog-langcode "\">\n"
   "<head>\n"
   "<meta charset=\"UTF-8\">\n"
   (when tDescription
     (format "<meta name=\"description\" content=\"%s\">\n" tDescription))
   "<link rel=\"alternate\"\n"
   "      type=\"application/rss+xml\"\n"
   "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
   "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
   "<title>" tTitle "</title>\n"
   org-static-blog-page-header
   "</head>\n"
   "<body>\n"
   "<div id=\"preamble\" class=\"status\">"
   org-static-blog-page-preamble
   "</div>\n"
   "<div id=\"content\">\n"
   tContent
   "</div>\n"
   "<div id=\"postamble\" class=\"status\">"
   org-static-blog-page-postamble
   "</div>\n"
   "</body>\n"
   "</html>\n"))

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
  (concat-to-dir org-static-blog-publish-directory
		 (org-static-blog-get-post-public-path post-filename)))

(defun org-static-blog-get-post-filenames ()
  "Returns a list of all posts."
  (directory-files-recursively
   org-static-blog-posts-directory ".*\\.org$"))

(defun org-static-blog-get-draft-filenames ()
  "Returns a list of all drafts."
  (directory-files-recursively
   org-static-blog-drafts-directory ".*\\.org$"))

(defun org-static-blog-file-buffer (file)
  "Return the buffer open with a full filepath, or nil."
  (require 'seq)
  (make-directory (file-name-directory file) t)
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
           (auto-insert nil)
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
      (if (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$" nil t)
          (match-string 1)
        (warn "%s file does not have a title, using %s as the title" post-filename post-filename)
        post-filename))))

(defun org-static-blog-get-description (post-filename)
  "Extract the `#+description:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (when (search-forward-regexp "^\\#\\+description:[ ]*\\(.+\\)$" nil t)
        (let ((description (string-trim (match-string 1))))
          (unless (zerop (length description))
            description))))))

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
        (dolist (tag (remove org-static-blog-rss-excluded-tag tags))
          (if (assoc-string tag tag-tree t)
              (push post-filename (cdr (assoc-string tag tag-tree t)))
            (push (cons tag (list post-filename)) tag-tree)))))
    tag-tree))

(defun org-static-blog--preview-region ()
  "Find the start and end of the preview in the current buffer."
  (goto-char (point-min))
  (if org-static-blog-preview-end
      (when (or (search-forward (or org-static-blog-preview-start "<p>") nil t)
                (search-forward "<p>" nil t))
        (let ((start (match-beginning 0)))
          (or (search-forward org-static-blog-preview-end nil t)
              (search-forward "</p>" nil t))
          (buffer-substring-no-properties start (point))))
    (when (search-forward (or org-static-blog-preview-start "<p>") nil t)
      (let ((start (match-beginning 0)))
        (search-forward "</p>")
        (buffer-substring-no-properties start (point))))))

(defun org-static-blog-get-preview (post-filename)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML.
If the HTML body contains multiple paragraphs, include only the first paragraph,
and display an ellipsis.
Preamble and Postamble are excluded, too."
  (with-temp-buffer
    (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
    (let ((post-title (org-static-blog-get-title post-filename))
          (post-date (org-static-blog-get-date post-filename))
          (post-taglist (org-static-blog-post-taglist post-filename))
          (post-ellipsis "")
          (preview-region (org-static-blog--preview-region)))
      (when (and preview-region (search-forward "<p>" nil t))
        (setq post-ellipsis
              (concat (when org-static-blog-preview-link-p
                        (format "<a href=\"%s\">"
                                (org-static-blog-get-post-url post-filename)))
                      org-static-blog-preview-ellipsis
                      (when org-static-blog-preview-link-p "</a>\n"))))
      ;; Put the substrings together.
      (let ((title-link
             (format "<h2 class=\"post-title\"><a href=\"%s\">%s</a></h2>"
                     (org-static-blog-get-post-url post-filename) post-title))
            (date-link
             (format-time-string (concat "<div class=\"post-date\">"
                                         (org-static-blog-gettext 'date-format)
                                         "</div>")
                                 post-date)))
        (concat
         (if org-static-blog-preview-date-first-p
             (concat date-link title-link)
           (concat title-link date-link))
         preview-region
         post-ellipsis
         (format "<div class=\"taglist\">%s</div>" post-taglist))))))


(defun org-static-blog-get-post-content (post-filename &optional exclude-title)
  "Get the rendered HTML body without headers from POST-FILENAME.
Preamble and Postamble are excluded, too."
  ;; NB! The following code assumes the post is using default template.
  ;; See: org-static-blog-publish-file
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
       ;; Search backward for the post content (by org-static-blog-render-post-content).
       ;; See: org-static-blog-template
       (search-backward "<div id=\"postamble\" class=\"status\">")
       (search-backward "</div>")
       ;; If comments section exists, it is then one div backward.
       ;; See: org-static-blog-post-postamble
       (search-backward "<div id=\"comments\">" nil t)
       (point)))))

(defun org-static-blog-get-absolute-url (relative-url)
  "Returns absolute URL based on the RELATIVE-URL passed to the function.

For example, when `org-static-blog-publish-url` is set to 'https://example.com/'
and `relative-url` is passed as 'archive.html' then the function
will return 'https://example.com/archive.html'."
  (concat-to-dir org-static-blog-publish-url relative-url))

(defun org-static-blog-get-post-url (post-filename)
  "Returns absolute URL to the published POST-FILENAME.

This function concatenates publish URL and generated custom filepath to the
published HTML version of the post."
  (org-static-blog-get-absolute-url
          (org-static-blog-get-post-public-path post-filename)))

(defun org-static-blog-get-post-public-path (post-filename)
  "Returns post filepath in public directory.
  This function retrieves relative path to the post file in posts
  or drafts directories to published HTML version of the post."
  (let* ((true-post-filename (file-truename post-filename))
         (true-posts-directory (file-truename org-static-blog-posts-directory))
         (root-dir (if (string-prefix-p true-posts-directory true-post-filename)
                       true-posts-directory
                     (file-truename org-static-blog-drafts-directory))))
    (concat (file-name-sans-extension (file-relative-name true-post-filename root-dir)) ".html")))

(defun org-static-blog-get-relative-path (post-filename)
  "Removes absolute directory path from POST-FILENAME and changes file extention
from `.org` to `.html`. Returns filepath to HTML file relative to posts or drafts directories.

Works with both posts and drafts directories.

For example, when `org-static-blog-posts-directory` is set to '~/blog/posts'
and `post-filename` is passed as '~/blog/posts/my-life-update.org' then the function
will return 'my-life-update.html'."
  (concat (file-name-sans-extension (file-relative-name post-filename org-static-blog-posts-directory))
	  ".html"))

;;;###autoload
(defun org-static-blog-publish-file (post-filename)
  "Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated."
  (interactive "f")
  (org-static-blog-with-find-file
   (org-static-blog-matching-publish-filename post-filename)
   (org-static-blog-template
    (org-static-blog-get-title post-filename)
    (concat
     (org-static-blog-post-preamble post-filename)
     (org-static-blog-render-post-content post-filename)
     (org-static-blog-post-postamble post-filename))
    (org-static-blog-get-description post-filename))))


(defun org-static-blog-render-post-content (post-filename)
  "Render blog content as bare HTML without header."
  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t))
    (save-excursion
      (let ((current-buffer (current-buffer))
            (buffer-exists (org-static-blog-file-buffer post-filename))
            (result nil))
        (with-temp-buffer
          (if buffer-exists
              (insert-buffer-substring buffer-exists)
            (insert-file-contents post-filename))
          (org-mode)
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (setq org-map-continue-from (point))
             (org-cut-subtree))
           org-static-blog-no-post-tag)
          (setq result
                (org-export-as 'org-static-blog-post-bare nil nil nil nil))
          (switch-to-buffer current-buffer)
          result)))))

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
     (concat-to-dir org-static-blog-publish-directory org-static-blog-index-file)
     (last post-filenames org-static-blog-index-length)
     org-static-blog-index-front-matter)))

(defun org-static-blog-assemble-multipost-page (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                  (org-static-blog-get-date x)))))
  (org-static-blog-with-find-file
   pub-filename
   (org-static-blog-template
    org-static-blog-publish-title
   (concat
    (when front-matter front-matter)
    (apply 'concat (mapcar
                    (if org-static-blog-use-preview
                        'org-static-blog-get-preview
                      'org-static-blog-get-post-content) post-filenames))
    "<div id=\"archive\">\n"
    "<a href=\"" (org-static-blog-get-absolute-url org-static-blog-archive-file) "\">" (org-static-blog-gettext 'other-posts) "</a>\n"
    "</div>\n"))))



(defun org-static-blog-post-preamble (post-filename)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   "<div class=\"post-date\">" (format-time-string (org-static-blog-gettext 'date-format)
						   (org-static-blog-get-date post-filename))
   "</div>"
   "<h1 class=\"post-title\">"
   "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h1>\n"))


(defun org-static-blog-post-taglist (post-filename)
  "Returns the tag list of the post.
This part will be attached at the end of the post, after
the taglist, in a <div id=\"taglist\">...</div> block."
  (let ((taglist-content "")
        (tags (remove org-static-blog-no-comments-tag
                      (remove org-static-blog-rss-excluded-tag
                              (org-static-blog-get-tags post-filename)))))
    (when (and tags org-static-blog-enable-tags)
      (setq taglist-content (concat "<a href=\""
                                    (org-static-blog-get-absolute-url org-static-blog-tags-file)
                                    "\">" (org-static-blog-gettext 'tags) "</a>: "))
      (dolist (tag tags)
        (setq taglist-content (concat taglist-content "<a href=\""
                                      (org-static-blog-get-absolute-url (concat "tag-" (downcase tag) ".html"))
                                      "\">" tag "</a> "))))
    taglist-content))

(defun org-static-blog-post-postamble (post-filename)
  "Returns the tag list and comment box at the end of a post.
This function is called for every post and the returned string is
appended to the post body, and includes the tag list generated by
followed by the HTML code for comments."
  (concat "<div class=\"taglist\">"
          (org-static-blog-post-taglist post-filename)
          "</div>"
          (if (or (string= org-static-blog-post-comments "")
                  (member org-static-blog-no-comments-tag (org-static-blog-get-tags post-filename)))
              ""
            (concat "\n<div id=\"comments\">"
                    org-static-blog-post-comments
                    "</div>"))))

(defun org-static-blog--prune-items (items)
  "Limit, if needed, the items to be included in an RSS feed."
  (if (and org-static-blog-rss-max-entries (> org-static-blog-rss-max-entries 0))
      (let ((excess (- (length items) org-static-blog-rss-max-entries)))
        (if (> excess 0) (butlast items excess) items))
    items))

(defun org-static-blog--rss-filename (&optional tag)
  "Full path to the RSS file for the given TAG."
  (concat-to-dir org-static-blog-publish-directory
                 (concat tag (when tag "-") org-static-blog-rss-file)))

(defun org-static-blog--write-rss (items &optional tag)
  "Generates an RSS file for the given TAG, or for all tags is TAG is nil."
  (let ((title (format "%s%s"
                       org-static-blog-publish-title
                       (if tag (concat " - " tag) "")))
        (url (format "%s%s"
                     org-static-blog-publish-url
                     (if tag (concat "/tag-" (downcase tag) ".html") "")))
        (items (sort items (lambda (x y) (time-less-p (car y) (car x))))))
    (org-static-blog-with-find-file
     (org-static-blog--rss-filename tag)
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	     "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
	     "<channel>\n"
	     "<title><![CDATA[" title "]]></title>\n"
	     "<description><![CDATA[" title "]]></description>\n"
	     "<link>" url "</link>\n"
	     "<lastBuildDate>" (let ((system-time-locale "C")) ; force dates to render as per RSS spec
				 (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)))
             "</lastBuildDate>\n"
             org-static-blog-rss-extra
	     (apply 'concat (mapcar 'cdr (org-static-blog--prune-items items)))
	     "</channel>\n"
	     "</rss>\n"))))

(defun org-static-blog-assemble-rss ()
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((system-time-locale "en_US.utf-8") ; force dates to render as per RSS spec
        (rss-items nil)
        (rss-tag-items nil))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (let ((rss-date (org-static-blog-get-date post-filename))
            (rss-text (org-static-blog-get-rss-item post-filename))
            (tags (org-static-blog-get-tags post-filename)))
        (when (or (null org-static-blog-rss-excluded-tag)
                  (not (member org-static-blog-rss-excluded-tag tags)))
          (let ((item (cons rss-date rss-text)))
            (add-to-list 'rss-items item)
            (when org-static-blog-enable-tag-rss
              (dolist (tag tags)
                (let ((items (cons item (cdr (assoc tag rss-tag-items)))))
                  (setf (alist-get tag rss-tag-items nil t 'string=) items))))))))
    (org-static-blog--write-rss rss-items)
    (message "%s" rss-tag-items)
    (dolist (x rss-tag-items) (org-static-blog--write-rss (cdr x) (car x)))))

(defun org-static-blog-get-rss-item (post-filename)
  "Assemble RSS item from post-filename.
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title><![CDATA[" (org-static-blog-get-title post-filename) "]]></title>\n"
   "  <description><![CDATA["
   (org-static-blog-get-post-content post-filename t) ; exclude headline!
   "]]></description>\n"
   (let ((categories ""))
     (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
       (dolist (tag (org-static-blog-get-tags post-filename))
         (setq categories (concat categories
                                  "  <category><![CDATA[" tag "]]></category>\n"))))
     categories)
   "  <link>"
   (url-encode-url (org-static-blog-get-post-url post-filename))
   "</link>\n"
   "  <guid>"
   (url-encode-url (org-static-blog-get-post-url post-filename))
   "</guid>\n"
   "  <pubDate>"
   (let ((system-time-locale "C")) ; force dates to render as per RSS spec
     (format-time-string "%a, %d %b %Y %H:%M:%S %z" (org-static-blog-get-date post-filename)))
   "</pubDate>\n"
   "</item>\n"))

(defun org-static-blog-assemble-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (concat-to-dir org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil)
        (post-filenames (org-static-blog-get-post-filenames)))
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p
                                                        (org-static-blog-get-date y)
                                                        (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     archive-filename
     (org-static-blog-template
      org-static-blog-publish-title
      (concat
       "<h1 class=\"title\">" (org-static-blog-gettext 'archive) "</h1>\n"
       (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames)))))))

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
   "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h2>\n"))

(defun org-static-blog-assemble-tags ()
  "Render the tag archive and tag pages."
  (org-static-blog-assemble-tags-archive)
  (dolist (tag (org-static-blog-get-tag-tree))
    (org-static-blog-assemble-multipost-page
     (concat-to-dir org-static-blog-publish-directory
		    (concat "tag-" (downcase (car tag)) ".html"))
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
  (let ((tags-archive-filename (concat-to-dir org-static-blog-publish-directory org-static-blog-tags-file))
        (tag-tree (org-static-blog-get-tag-tree)))
    (setq tag-tree (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
    (org-static-blog-with-find-file
     tags-archive-filename
     (org-static-blog-template
      org-static-blog-publish-title
      (concat
       "<h1 class=\"title\">" (org-static-blog-gettext 'tags) "</h1>\n"
       (apply 'concat (mapcar 'org-static-blog-assemble-tags-archive-tag tag-tree)))))))

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
    (if (> (cl-list-length posts) 1)
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
    (if (> (cl-list-length posts) 1)
        (find-file (cadr posts))
      (message (org-static-blog-gettext 'no-next-post)))))

(defun org-static-blog-open-matching-publish-file ()
  "Opens HTML for post."
  (interactive)
  (find-file (org-static-blog-matching-publish-filename (buffer-file-name))))

;;;###autoload
(defun org-static-blog-create-new-post (&optional draft)
  "Creates a new blog post.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose."
  (interactive)
  (let ((title (read-string (org-static-blog-gettext 'title))))
    (find-file (concat-to-dir
                (if draft
                    org-static-blog-drafts-directory
                  org-static-blog-posts-directory)
                (read-string (org-static-blog-gettext 'filename)
			     (concat (format-time-string "%Y-%m-%d-" (current-time))
				     (replace-regexp-in-string org-static-blog-suggested-filename-cleaning-regexp
							       "-" (downcase title))
				     ".org"))))
    (insert "#+title: " title "\n"
            "#+date: " (format-time-string "<%Y-%m-%d %H:%M>") "\n"
            "#+description: \n"
            "#+filetags: ")))

;;;###autoload
(defun org-static-blog-create-new-draft ()
  "Creates a new blog draft.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose."
  (interactive)
  (org-static-blog-create-new-post 't))


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
(define-key org-static-blog-mode-map (kbd "C-c C-d") 'org-static-blog-create-new-draft)

(provide 'org-static-blog)

;;; org-static-blog.el ends here

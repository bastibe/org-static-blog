
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
   (org-export-to-file 'html
       (org-static-blog-matching-publish-filename post-filename)
     nil nil nil t nil)))

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

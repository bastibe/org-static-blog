;; -*- mode: eask; lexical-binding: t -*-

(package "org-static-blog"
         "1.7.0"
         "a simple org-mode based static blog generator")

(website-url "https://github.com/bastibe/org-static-blog")
(keywords "outlines")

(package-file "org-static-blog.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)

(depends-on "emacs" "26.1")

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)
             (setq byte-compile-docstring-max-column 1000)))

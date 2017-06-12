ORG-STATIC-BLOG
===============

Static blog generators are a dime a dozen. This is one more, which
focuses on being simple. All files are simple org-mode files in a
directory. The only requirement is that every org file must have a
`#+TITLE` and a `#+DATE`.

This file is also available from marmalade and melpa-stable.

Set up your blog by customizing org-static-blog's parameters, then
call `M-x org-static-blog-publish` to render the whole blog or
`M-x org-static-blog-publish-file filename.org` to render only only
the file `filename.org`.

Above all, I tried to make org-static-blog as simple as possible.
There are no magic tricks, and all of the source code is meant to be
easy to read, understand and modify.

If you have questions, if you find bugs, or if you would like to
contribute something to org-static-blog, please open an issue or pull
request on Github.

Finally, I would like to remind you that I am developing this project
for free, and in my spare time. While I try to be as accomodating as
possible, I can not guarantee a timely response to issues. Publishing
Open Source Software on Github does not imply an obligation to *fix
your problem right now*. Please be civil.

Examples
--------

`org-static-blog` was used to render [http://bastibe.de/][blog]. Have
a look at my [init.el][init] and the [repository][repo] for the blog
itself to see an example of how to use `org-static-blog` in practice.

[blog]: http://bastibe.de
[init]: https://github.com/bastibe/.emacs.d/blob/master/init.el#L670
[repo]: https://github.com/bastibe/bastibe.github.com

LICENSE
-------

Copyright 2015, Bastian Bechtold

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

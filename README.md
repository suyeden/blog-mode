

# blog-mode

[![GitHub license](<https://img.shields.io/github/license/suyeden/blog-mode?color=blue>)](<https://github.com/suyeden/blog-mode/blob/master/LICENSE>)  

`blog-mode` is a major mode for Emacs.  
It enables you to easily take notes or make a personal wiki on your Emacs.  


## Installation

First of all, make sure you can use `Org-mode` in your Emacs because `blog-mode` uses the functions of `Org-mode`. Please check other websites for more information about `Org-mode`.  
Then, place `blog-mode.el` file in any directory on your computer like `~/.emacs.d/lisp/` and add the code below to your Emacs configuration file like `.emacs` or `init.el`.  

```emacs-lisp
;; Add to your Emacs configuration file
(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "blog-mode.el")
```


## Usage

When you want to start `blog-mode`, press `C-c b`.  
`/org/blog` directory will be created under your home directory and blog-mode related files will be created here.  
`M-x blog-mode` can also be used to enable `blog-mode` on your current document.  
You can see `help` with `C-c C-h`. To quit from `help`, type `q`.  

You can use the key bindings and the functions below.  

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">key binding</th>
<th scope="col" class="org-left">function</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c b</td>
<td class="org-left">Start blog-mode</td>
</tr>


<tr>
<td class="org-left">C-c C-h</td>
<td class="org-left">Display help for blog-mode</td>
</tr>


<tr>
<td class="org-left">C-c M-r</td>
<td class="org-left">Restart blog-mode (Refresh blog-mode)</td>
</tr>


<tr>
<td class="org-left">M-RET</td>
<td class="org-left">Insert a new heading</td>
</tr>


<tr>
<td class="org-left">C-c n</td>
<td class="org-left">Make a new topic (Make a link)</td>
</tr>


<tr>
<td class="org-left">C-c C-o</td>
<td class="org-left">Open the topic (Jump to the link destination)</td>
</tr>


<tr>
<td class="org-left">C-c C-LEFT</td>
<td class="org-left">Go back to previous page</td>
</tr>


<tr>
<td class="org-left">C-c r</td>
<td class="org-left">Rename the topic (Rename the link and the linked file)</td>
</tr>


<tr>
<td class="org-left">C-M-d</td>
<td class="org-left">Delete the topic (Delete the link and the linked file)</td>
</tr>


<tr>
<td class="org-left">C-c C-e h H/h/o</td>
<td class="org-left">Export current-buffer's org-file to HTML-file</td>
</tr>


<tr>
<td class="org-left">C-c x</td>
<td class="org-left">Export all modified and newly created org-files to HTML-files</td>
</tr>


<tr>
<td class="org-left">C-c e</td>
<td class="org-left">Close blog-mode</td>
</tr>


<tr>
<td class="org-left">M-LEFT or M-RIGHT</td>
<td class="org-left">Change the heading level</td>
</tr>


<tr>
<td class="org-left">M-UP or M-DOWN</td>
<td class="org-left">Rearrange the list</td>
</tr>


<tr>
<td class="org-left">C-c C-l</td>
<td class="org-left">Insert a stored-link</td>
</tr>


<tr>
<td class="org-left">S-TAB or C-u C-i</td>
<td class="org-left">Fold all subtrees up to their root level</td>
</tr>


<tr>
<td class="org-left">TAB or C-i</td>
<td class="org-left">Fold the current subtree up to its root level</td>
</tr>


<tr>
<td class="org-left">C-c C-,</td>
<td class="org-left">Insert a code block</td>
</tr>


<tr>
<td class="org-left">C-c '</td>
<td class="org-left">Edit a source code block</td>
</tr>


<tr>
<td class="org-left">C-M-i</td>
<td class="org-left">Display a list of supported languages in the source code block</td>
</tr>


<tr>
<td class="org-left">C-c M-s</td>
<td class="org-left">Insert space at the beginning of the line within the region</td>
</tr>


<tr>
<td class="org-left">C-j</td>
<td class="org-left">Start a new line considering leading whitespace</td>
</tr>


<tr>
<td class="org-left">C-c C-n/p</td>
<td class="org-left">Move to next/previous heading</td>
</tr>
</tbody>
</table>

In addition, most of the key bindings and the syntax of `Org-mode` can be used in this mode.  

To attach a stylesheet to exported HTML files, edit `~/org/blog/css/style.css`.  
When you want to change your blog's title (`#+TITLE` of `index.org`), author's name, and your language, edit `index.org` directly.  


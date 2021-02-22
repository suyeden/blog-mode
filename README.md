

# blog-mode

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

When you want to start `blog-mode`, press `C-c b`. `/org/blog` directory will be created under your home directory.  
`M-x blog-mode` can also be used to enable `blog-mode` on your current document.  
You can see `help` with `C-c C-h`. To quit from `help`, type `q` and then press `Return` on mini-buffer.  

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
<td class="org-left">C-c n</td>
<td class="org-left">make a new topic (make a link)</td>
</tr>


<tr>
<td class="org-left">C-c C-l</td>
<td class="org-left">insert a stored-link</td>
</tr>


<tr>
<td class="org-left">M-RET</td>
<td class="org-left">insert a new heading</td>
</tr>


<tr>
<td class="org-left">M-left or M-right</td>
<td class="org-left">change the heading level</td>
</tr>


<tr>
<td class="org-left">M-Up or M-Down</td>
<td class="org-left">rearrange the list</td>
</tr>


<tr>
<td class="org-left">C-c C-o</td>
<td class="org-left">open the topic (jump to the link destination)</td>
</tr>


<tr>
<td class="org-left">C-c C-left</td>
<td class="org-left">go back to previous page</td>
</tr>


<tr>
<td class="org-left">C-c C-e h H/h/o</td>
<td class="org-left">export current-buffer's org-file to HTML-file</td>
</tr>


<tr>
<td class="org-left">C-M-d</td>
<td class="org-left">delete the topic (delete the link and the linked file)</td>
</tr>


<tr>
<td class="org-left">C-c x</td>
<td class="org-left">export all org-files to HTML-files and return to the top page</td>
</tr>


<tr>
<td class="org-left">C-c e</td>
<td class="org-left">close blog-mode</td>
</tr>


<tr>
<td class="org-left">S-TAB or C-u C-i</td>
<td class="org-left">fold all subtrees up to their root level</td>
</tr>


<tr>
<td class="org-left">TAB or C-i</td>
<td class="org-left">fold the current subtree up to its root level</td>
</tr>


<tr>
<td class="org-left">C-c C-,</td>
<td class="org-left">insert a code block</td>
</tr>


<tr>
<td class="org-left">C-c '</td>
<td class="org-left">edit a source code block</td>
</tr>


<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">execute a source code block</td>
</tr>


<tr>
<td class="org-left">C-M-i</td>
<td class="org-left">display a list of supported languages in the source code block</td>
</tr>


<tr>
<td class="org-left">C-c M-s</td>
<td class="org-left">insert leading whitespace from current-point to end-of-buffer</td>
</tr>


<tr>
<td class="org-left">C-j</td>
<td class="org-left">start a new line considering leading whitespace</td>
</tr>


<tr>
<td class="org-left">C-c C-n/p</td>
<td class="org-left">move to next/previous heading</td>
</tr>
</tbody>
</table>

And also, you can use most of the key bindings and the syntax of `Org-mode` in this mode.  


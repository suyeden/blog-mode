;;;; -*- Emacs-Lisp -*-
;;;; blog-mode written by suyeden

(define-key global-map "\C-cb" 'start-blog)

;;; keymap
(defvar org-mode-map (make-keymap))
(defvar blog-mode-map (make-keymap))
(set-keymap-parent blog-mode-map org-mode-map)
;;; define-key
(define-key blog-mode-map "\C-cn" 'blog-new)
(define-key blog-mode-map "\C-ce" 'blog-end)
(define-key blog-mode-map "\C-c\C-h" 'blog-help)
(define-key blog-mode-map "\C-cx" 'all-export-to-html)
(define-key blog-mode-map "\C-c\M-s" 'blog-insert-space)
(define-key blog-mode-map "\C-\M-i" 'blog-complete-symbol)
(define-key blog-mode-map "\C-\M-d" 'blog-delete)
(define-key blog-mode-map "\C-cr" 'blog-rename)

;;; blog-mode (my new major-mode)
(defun blog-mode ()
  "blog-mode"
  (interactive)
  ;; org-open-at-point の際に別ウィンドウで開かないようにする
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))
  ;; org-export-dispatch (org-fileからhtml-fileへの変換) のキーボードマクロ
  (fset 'auto-export-to-html
        "\C-c\C-ehh")
  (use-local-map blog-mode-map)
  (setq major-mode 'blog-mode
        mode-name "blog")
  ;; define-key
  ;; 1. キーバインドに矢印キーを用いたい -> (local-set-key)
  ;; 2. local-set-key をしたいが、 blog-mode に対してキーバインドを追加したい
  ;; という理由のため、blog-mode が有効になったこのタイミングで設定する  
  (local-set-key (kbd "C-c <C-left>") 'back-page))

(defun start-blog ()
  "blog-modeを開く"
  (interactive)
  (if (file-exists-p "~/org/blog/index.org")
      (progn
        (find-file "~/org/blog/index.org")
        (goto-char (point-min))
        (re-search-forward "* Category" nil t)
        (beginning-of-line))
    (find-file "~/org/blog/index.org")
    (insert "#+TITLE: すえーでんの技術ブログ\n#+AUTHOR: suyeden\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n** [[file:koushin.org][更新履歴]]\n\n------------------------------------------------------------------------------------------\n\n* Category\n\n\n------------------------------------------------------------------------------------------\n")
    (re-search-backward "* Category" nil t)
    (beginning-of-line))
  (blog-mode))

(defadvice org-open-at-point (after my-change-keymap ())
  "リンク先を開いた際にキーマップを blog-mode-map に変更する"
  (if (get-buffer "index.org")
      (blog-mode)
    nil))
(ad-activate 'org-open-at-point)

;; この操作を行っておかないとC-c 'で行った編集が適用されない
(defadvice org-edit-special (around my-change-keymap-blog-to-org ())
  "ソースコードブロックを編集する前に blog-mode-map から org-mode-map に変更する"
  (let (blog-edit-flag)
    (if (get-buffer "index.org")
        (progn
          (org-mode)
          (setq blog-edit-flag (ignore-errors
                                 ad-do-it
                                 t))
          (unless blog-edit-flag
            (blog-mode)
            (message "No such language mode: nil-mode")))
      ad-do-it)))
(ad-activate 'org-edit-special)
;;
(defadvice org-edit-src-exit (after my-change-keymap-org-to-blog ())
  "ソースコードブロックを編集した後に org-mode-map から blog-mode-map に変更する"
  (if (get-buffer "index.org")
      (blog-mode)
    nil))
(ad-activate 'org-edit-src-exit)
;;
(defadvice org-edit-src-abort (after my-change-keymap-abort-org-to-blog ())
  "ソースコードブロックの編集を破棄し、org-mode-map から blog-mode-map に変更する"
  (if (get-buffer "index.org")
      (blog-mode)
    nil))
(ad-activate 'org-edit-src-abort)

(defun blog-complete-symbol ()
  "ソースコードブロックに適用できる言語一覧を表示する"
  (interactive)
  (org-mode)
  (complete-symbol nil)
  (blog-mode)
  (pop-to-buffer "*Completions*"))

(defun back-page ()
  "1つ前の画面に戻る"
  (interactive)
  (catch 'flag
    (if (string= "index.org" (buffer-name (current-buffer)))
        (progn
          (message "if you want to end blog-mode, type 'C-c e' !")
          (throw 'flag t))
      nil)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun blog-new ()
  "新しいブログトピックを作成する"
  (interactive)
  (let (f-or-d blog-name (real-name nil) now-time)
    (setq blog-name (read-string "new topic's name? : "))
    (while (string= (format "%s" blog-name) "")
        (setq blog-name (read-string "enter new topic's name again : "))
      nil)
    (setq real-name (read-string "file's name? (without .org) : "))
    (if (string= (format "%s" real-name) "")
        (setq real-name (format "%s" blog-name))
      nil)
    (while (file-exists-p (format "~/org/blog/%s.org" real-name))
      (setq real-name (read-string "File exists! Another file's name? (without .org) : "))
      (if (string= (format "%s" real-name) "")
          (setq real-name (format "%s" blog-name))
        nil))
    (setq f-or-d (read-string "file(f) or directory(d) ? : "))
    (while (not
            (or
             (string= (format "%s" f-or-d) "file")
             (string= (format "%s" f-or-d) "f")
             (string= (format "%s" f-or-d) "directory")
             (string= (format "%s" f-or-d) "d")))
        (setq f-or-d (read-string "non-valid variable! file(f) or directory(d) ? : ")))
    (if (or (string= f-or-d "file") (string= f-or-d "f"))
        (progn
          (insert (format "[[file:%s.org][%s]]" real-name blog-name))
          (backward-char)
          (find-file (format "~/org/blog/%s.org" real-name))
          (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n# file\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
          (write-file (format "~/org/blog/%s.org" real-name))
          (kill-buffer (current-buffer))
          (if (file-exists-p "~/org/blog/koushin.org")
              (progn
                (find-file "~/org/blog/koushin.org")
                (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
                (goto-char (point-min))
                (re-search-forward "^\\*\\*\\* " nil t)
                (beginning-of-line)
                (insert (format "*** %s [[file:%s.org][%s]] を作成しました。\n" now-time real-name blog-name))
                (write-file "~/org/blog/koushin.org")
                (kill-buffer (current-buffer)))
            (find-file "~/org/blog/koushin.org")
            (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
            (insert (format "#+TITLE: 更新履歴\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n*** %s [[file:%s.org][%s]] を作成しました。" now-time real-name blog-name))
            (write-file "~/org/blog/koushin.org")
            (kill-buffer (current-buffer))))
      (if (or (string= f-or-d "directory") (string= f-or-d "d"))
          (progn
            (insert (format "[[file:%s.org][%s]]" real-name blog-name))
            (find-file (format "~/org/blog/%s.org" real-name))
            (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n# directory\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
            (write-file (format "~/org/blog/%s.org" real-name))
            (kill-buffer (current-buffer)))
        nil))))

(defun all-export-to-html ()
  "すべての orgファイル を HTMLファイル にエクスポートする"
  (interactive)
  (save-buffer)
  (if (y-or-n-p "do you want to export all org-files to HTML-files? : ")
      (progn
        (let (current-point)
          (switch-to-buffer "index.org")
          (setq current-point (point))
          (let (inside-files org-or-html org-files)
            (setq inside-files (directory-files "~/org/blog/" t))
            (while inside-files
              (setq org-or-html (car inside-files))
              (if (string-match "\\.org$" (format "%s" org-or-html))
                  (setq org-files (cons org-or-html org-files))
                nil)
              (setq inside-files (cdr inside-files)))
            (let (export-file)
              (while org-files
                (setq export-file (car org-files))
                (find-file (format "%s" export-file))
                (execute-kbd-macro (symbol-function 'auto-export-to-html))
                (kill-buffer (current-buffer))
                (setq org-files (cdr org-files)))))
          (find-file "~/org/blog/index.org")
          (goto-char current-point)
          (message "Done!")))
    nil))
(defadvice all-export-to-html (after all-export-to-html-change-keymap ())
  (use-local-map blog-mode-map)
  (setq major-mode 'blog-mode
        mode-name "blog"))
(ad-activate 'all-export-to-html)

(defun blog-insert-space ()
  "選択範囲内の行頭において、指定数分のスペースを挿入あるいは削除する"
  (interactive)
  (let (space-count (blog-region-end (make-marker)))
    (if (use-region-p)
        (progn
          (setq space-count (read-string "how many spaces ? : "))
          (setq space-count (string-to-number space-count))
          (set-marker blog-region-end (region-end))
          (if (< 0 space-count)
              (progn
                (goto-char (region-beginning))
                (beginning-of-line)
                (while (<= (point) (marker-position blog-region-end))
                  (let ((i 0))
                    (while (not (= i space-count))
                      (insert " ")
                      (setq i (1+ i))))
                  (forward-line)
                  (beginning-of-line)))
            ;; if space-count < 0
            (setq space-count (* -1 space-count))
            (goto-char (region-beginning))
            (beginning-of-line)
            (while (<= (point) (marker-position blog-region-end))
              (delete-char space-count)
              (forward-line)
              (beginning-of-line)))
          (set-marker blog-region-end nil))
      nil)))

(defun blog-delete ()
  "現在行上にあるリンクを削除して必要ならorgファイルも削除する"
  (interactive)
  (let (line-contents delete-file-name delete-topic-name file-or-dir)
    (catch 'foo-blog-delete
      (save-excursion
        (beginning-of-line)
        (setq line-contents (buffer-substring (point) (progn (end-of-line) (point))))
        (with-temp-buffer
          (insert (format "%s" line-contents))
          (goto-char (point-min))
          (if (re-search-forward "\\[\\[file:\\(.+\\)\.org\\]\\[\\(.+\\)\\]\\]" nil t)
              (progn
                (setq delete-file-name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                (setq delete-topic-name (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
            (throw 'foo-blog-delete t))))
      (if (y-or-n-p (format "Make sure there is no link in [ %s ]. Delete this topic ?" delete-topic-name))
          (progn
            (find-file (format "~/org/blog/%s.org" delete-file-name))
            (goto-char (point-min))
            (re-search-forward "\\*\\* \\[\\[file:index.org\\]\\[home\\]\\]" nil t)
            (if (re-search-forward "\\[\\[file:.+\\]\\[.+\\]\\]" nil t)
                (progn
                  (kill-buffer (current-buffer))
                  (message (format "There is a link in [ %s ] ! Can't delete this topic !" delete-topic-name))
                  (throw 'foo-blog-delete t))
              nil)
            (goto-char (point-min))
            (if (re-search-forward "+STARTUP" nil t)
                nil
              (throw 'foo-blog-delete t))
            (forward-line)
            (forward-char 2)
            (setq file-or-dir (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
            (kill-buffer (current-buffer))
            (if (y-or-n-p "Delete linked file too ?")
                (progn
                  (delete-file (format "~/org/blog/%s.org" delete-file-name))
                  (if (file-exists-p (format "~/org/blog/%s.html" delete-file-name))
                      (delete-file (format "~/org/blog/%s.html" delete-file-name))
                    nil))
              nil)
            (beginning-of-line)
            (delete-region (point) (progn (forward-line) (point)))
            (if (string= "file" (format "%s" file-or-dir))
                (progn
                  (if (file-exists-p "~/org/blog/koushin.org")
                      (progn
                        (find-file "~/org/blog/koushin.org")
                        (goto-char (point-min))
                        (re-search-forward (format "\\[\\[.*\\]\\[%s\\]\\]" delete-topic-name) nil t)
                        (beginning-of-line)
                        (delete-region (point) (progn (forward-line) (point)))
                        (save-buffer)
                        (kill-buffer (current-buffer)))
                    nil))
              nil)
            (message (format "[ %s ] is deleted!" delete-topic-name)))
        nil))))

(defun blog-rename ()
  "現在行上にあるリンク名を編集して必要ならorgファイル名も編集する"
  (interactive)
  (let (line-contents rename-file-name rename-topic-name new-topic-name new-file-name file-or-dir now-time)
    (catch 'foo-blog-rename
      (save-excursion
        (beginning-of-line)
        (setq line-contents (buffer-substring (point) (progn (end-of-line) (point))))
        (with-temp-buffer
          (insert (format "%s" line-contents))
          (goto-char (point-min))
          (if (re-search-forward "\\[\\[file:\\(.+\\)\.org\\]\\[\\(.+\\)\\]\\]" nil t)
              (progn
                (setq rename-file-name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                (setq rename-topic-name (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
            (throw 'foo-blog-rename t))))
      (setq new-topic-name (read-string "New topic name ? : " (format "%s" rename-topic-name)))
      (setq new-file-name (read-string "New linked-file name ? : " (format "%s" rename-file-name)))
      (beginning-of-line)
      (re-search-forward "\\[" nil t)
      (re-search-backward "\\[" nil t)
      (delete-region (point) (progn (forward-line) (point)))
      (insert (format "[[file:%s.org][%s]]\n" new-file-name new-topic-name))
      (rename-file (format "~/org/blog/%s.org" rename-file-name) (format "~/org/blog/%s.org" new-file-name))
      (if (file-exists-p (format "~/org/blog/%s.html" rename-file-name))
          (delete-file (format "~/org/blog/%s.html" rename-file-name))
        nil)
      (find-file (format "~/org/blog/%s.org" new-file-name))
      (re-search-forward "+TITLE: " nil t)
      (delete-region (point) (progn (end-of-line) (point)))
      (insert (format "%s" new-topic-name))
      (goto-char (point-min))
      (if (re-search-forward "+STARTUP" nil t)
          nil
        (throw 'foo-blog-rename t))
      (forward-line)
      (forward-char 2)
      (setq file-or-dir (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
      (save-buffer)
      (kill-buffer (current-buffer))
      (if (string= "file" (format "%s" file-or-dir))
          (progn
            (if (file-exists-p "~/org/blog/koushin.org")
                (progn
                  (find-file "~/org/blog/koushin.org")
                  (goto-char (point-min))
                  (re-search-forward (format "\\[\\[.*\\]\\[%s\\]\\]" rename-topic-name) nil t)
                  (beginning-of-line)
                  (delete-region (point) (progn (forward-line) (point)))
                  (goto-char (point-min))
                  (re-search-forward "^\\*\\*\\* " nil t)
                  (beginning-of-line)
                  (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))      
                  (insert (format "*** %s [[file:%s.org][%s]] を作成しました。\n" now-time new-file-name new-topic-name))
                  (save-buffer)
                  (kill-buffer (current-buffer)))
              nil))
        nil)
      (message (format "[ %s ] is renamed [ %s ] !" rename-topic-name new-topic-name)))))

(defun blog-end ()
  "current-buffer が index.org の時に blog-mode を閉じる"
  (interactive)
  (if (string= "index.org" (buffer-name (current-buffer)))
      (progn
        (if (file-exists-p "~/org/blog/koushin.org")
            (progn
              (find-file "~/org/blog/koushin.org")
              (save-buffer)
              (kill-buffer (current-buffer)))
          nil)
        (save-buffer)
        (kill-buffer (current-buffer)))
    (message "you can't end blog-mode! go to index.org!")))

(defun blog-help ()
  "利用できるキーバインドを表示"
  (interactive)
  (get-buffer-create "*blog-help*")
  (switch-to-buffer "*blog-help*")
  (delete-region (point-min) (point-max))
  (insert " C-c n : make a new topic (make a link)\n C-c C-l : insert a stored-link\n M-<RET> : insert a new heading\n <M-left> or <M-right> : change the heading level\n <M-Up> or <M-Down> : rearrange the list\n C-c C-o : open the topic (jump to the link destination)\n C-c <C-left> : go back to previous page\n C-c C-e h H/h/o : export current-buffer's org-file to HTML-file\n C-c r : rename the topic (rename the link and the linked file)\n C-M-d : delete the topic (delete the link and the linked file)\n C-c x : export all org-files to HTML-files and return to the top page\n C-c e : close blog-mode\n\n S-<TAB> or C-u C-i : fold all subtrees up to their root level\n <TAB> or C-i : fold the current subtree up to its root level\n C-c C-, : insert a code block\n C-c ' : edit a source code block\n C-c C-c : execute a source code block\n C-M-i : display a list of supported languages in the source code block\n C-c M-s : insert space at the beginning of the line within the region\n C-j : start a new line considering leading whitespace\n C-c C-n/p : move to next/previous heading\n\n----------------------------------------------------------------------\n < Syntax note >\n\n - : a list without number\n 1. : a list with number\n (C-c C-c : renumber the list)\n (C-c - : change the format of the list)\n\n *bold*\n /italic/\n _underline_\n +strikethrough+\n ~inline code~\n ----- : horizontal rule")
  (while (not (string= "q" (read-string "when you quit from this page, please press 'q' and press return : ")))
    (message "non-valid variable!")
    (sleep-for 0.5))
  (kill-buffer "*blog-help*"))

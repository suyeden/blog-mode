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
(define-key blog-mode-map "\C-c\M-r" 'blog-restart)

;;; blog-mode の有効無効判断のためのフラグ
(defvar is-blog-mode-enabled nil)
;;; 開いたブログ名を格納するリスト
(defvar blog-open-list nil)

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
  (local-set-key (kbd "C-c <C-left>") 'blog-back-page))

(defun start-blog ()
  "blog-modeを開く"
  (interactive)
  (if (string= "nil" (format "%s" blog-open-list))
      (progn
        (setq is-blog-mode-enabled nil)
        (if (file-exists-p "~/org/blog/index.org")
            (progn
              (find-file "~/org/blog/index.org")
              (goto-char (point-min))
              (re-search-forward "\\* Category" nil t)
              (beginning-of-line))
          (find-file "~/org/blog/index.org")
          (insert "#+TITLE: すえーでんの技術ブログ\n#+AUTHOR: suyeden\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n** [[file:koushin.org][更新履歴]]\n\n------------------------------------------------------------------------------------------\n\n* Category\n\n\n------------------------------------------------------------------------------------------\n")
          (re-search-backward "\\* Category" nil t)
          (beginning-of-line))
        (blog-mode)
        (setq blog-open-list (cons "index.org" blog-open-list))
        (message "Hello!"))
    (message "blog-mode has already been started!")))

(defadvice org-open-at-point (around blog-open-at-point ())
  "リンク先を開く際の操作"
  (let (blog-current-buffer-name)
    ;; リンク先を開く前に現在のモードが blog-mode であるか否かを記録
    (if (string= "blog" (format "%s" mode-name))
        (progn
          (setq is-blog-mode-enabled t)
          (setq blog-current-buffer-name (buffer-name (current-buffer))))
      (setq is-blog-mode-enabled nil))
    ;; org-open-at-point
    ad-do-it
    ;; リンク先を開いた後の操作
    (if (and (string= "t" (format "%s" is-blog-mode-enabled))
             (string-match ".+org" (buffer-name (current-buffer))))
        (progn
          ;; blog-mode の有効化
          (blog-mode)
          ;; 点線の下の行に移動
          (if (and (= 1 (point))
                   (re-search-forward "\\*\\* \\[\\[file:index.org\\]\\[home\\]\\]" nil t))
              (forward-line 2)
            nil)
          ;; リンク先を開いた後に開いたファイル名を記録する
          ;; リンクを開く前に記録を行うと、blog-back-page 操作によって最終的に index.org までリストから消えてしまう
          (if (not (member (buffer-name (current-buffer)) blog-open-list))
              (setq blog-open-list (cons (buffer-name (current-buffer)) blog-open-list))
            nil))
      nil)
    (setq is-blog-mode-enabled nil)))
(ad-activate 'org-open-at-point)

;; この操作を行っておかないとC-c 'で行った編集が適用されない
(defadvice org-edit-special (around my-change-keymap-blog-to-org ())
  "ソースコードブロックを編集する前に blog-mode-map から org-mode-map に変更する"
  (let (blog-edit-flag)
    (if (string= "blog" (format "%s" mode-name))
        (progn
          (setq is-blog-mode-enabled t)
          (org-mode)
          (setq blog-edit-flag (ignore-errors
                                 ad-do-it
                                 t))
          (unless blog-edit-flag
            (blog-mode)
            (setq is-blog-mode-enabled nil)
            (message "No such language mode: nil-mode")))
      ad-do-it)))
(ad-activate 'org-edit-special)
;;
(defadvice org-edit-src-exit (after my-change-keymap-org-to-blog ())
  "ソースコードブロックを編集した後に org-mode-map から blog-mode-map に変更する"
  (if (string= "t" (format "%s" is-blog-mode-enabled))
      (progn
        (blog-mode)
        (setq is-blog-mode-enabled nil))
    nil))
(ad-activate 'org-edit-src-exit)
;;
(defadvice org-edit-src-abort (after my-change-keymap-abort-org-to-blog ())
  "ソースコードブロックの編集を破棄し、org-mode-map から blog-mode-map に変更する"
  (if (string= "t" (format "%s" is-blog-mode-enabled))
      (progn
        (blog-mode)
        (setq is-blog-mode-enabled nil))
    nil))
(ad-activate 'org-edit-src-abort)

(defun blog-complete-symbol ()
  "ソースコードブロックに適用できる言語一覧を表示する"
  (interactive)
  (org-mode)
  (complete-symbol nil)
  (blog-mode)
  (pop-to-buffer "*Completions*"))

(defun blog-back-page ()
  "1つ前の画面に戻る"
  (interactive)
  (if (and (string= "index.org" (buffer-name (current-buffer)))
           (string= "nil" (format "%s" (cdr blog-open-list))))
      (message "If you want to end blog-mode, type 'C-c e' !")
    (if (not (string= (buffer-name (current-buffer)) (car blog-open-list)))
        (progn
          (save-buffer)
          (switch-to-buffer (car blog-open-list)))
      (save-buffer)
      (kill-buffer (current-buffer))
      (switch-to-buffer (car (cdr blog-open-list)))
      (setq blog-open-list (cdr blog-open-list)))))

(defun blog-new ()
  "新しいブログトピックを作成する"
  (interactive)
  (let (f-or-d blog-name (real-name nil) now-time)
    (setq blog-name (read-string "New topic's name? : "))
    (while (string= (format "%s" blog-name) "")
      (setq blog-name (read-string "Enter new topic's name again : ")))
    (setq real-name (read-string "File's name? (without .org) : "))
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
      (setq f-or-d (read-string "Non-valid variable! file(f) or directory(d) ? : ")))
    (if (or (string= f-or-d "file") (string= f-or-d "f"))
        (progn
          (insert (format "[[file:%s.org][%s]]" real-name blog-name))
          (backward-char)
          (find-file (format "~/org/blog/%s.org" real-name))
          (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n# file\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
          (blog-mode)
          (save-buffer)
          (kill-buffer (current-buffer))
          (if (file-exists-p "~/org/blog/koushin.org")
              (progn
                (find-file "~/org/blog/koushin.org")
                (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
                (goto-char (point-min))
                (re-search-forward "^\\*\\*\\* " nil t)
                (beginning-of-line)
                (insert (format "*** %s [[file:%s.org][%s]] を作成しました。\n" now-time real-name blog-name))
                (blog-mode)
                (save-buffer)
                (kill-buffer (current-buffer)))
            (find-file "~/org/blog/koushin.org")
            (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
            (insert (format "#+TITLE: 更新履歴\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n*** %s [[file:%s.org][%s]] を作成しました。" now-time real-name blog-name))
            (blog-mode)
            (save-buffer)
            (kill-buffer (current-buffer))))
      (if (or (string= f-or-d "directory") (string= f-or-d "d"))
          (progn
            (insert (format "[[file:%s.org][%s]]" real-name blog-name))
            (find-file (format "~/org/blog/%s.org" real-name))
            (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n# directory\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
            (blog-mode)
            (save-buffer)
            (kill-buffer (current-buffer)))
        nil))))

(defun all-export-to-html ()
  "Export-list.txt 中に記録されている orgファイル を HTMLファイル にエクスポートする"
  (interactive)
  (if (y-or-n-p "Do you want to export all org-files to HTML-files ?")
      (progn
        (let ((check-list blog-open-list) opening-list current-point-list)
          (save-buffer)
          (while check-list
            (switch-to-buffer (car check-list))
            (setq opening-list (cons (format "~/org/blog/%s" (buffer-name (current-buffer))) opening-list))
            (setq current-point-list (cons (point) current-point-list))
            (save-buffer)
            (setq check-list (cdr check-list)))
          (find-file "~/org/blog/Export-list.txt")
          (while (re-search-forward "^.+org$" nil t)
            (find-file (format "%s" (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
            (execute-kbd-macro (symbol-function 'auto-export-to-html))
            (kill-buffer (current-buffer)))
          (kill-buffer (current-buffer)) ; kill Export-list.txt
          (while opening-list
            (find-file (car opening-list))
            (goto-char (car current-point-list))
            (blog-mode)
            (setq opening-list (cdr opening-list))
            (setq current-point-list (cdr current-point-list)))
          (delete-file "~/org/blog/Export-list.txt")
          (message "Done!")))
    (message "Process killed")))

(defun blog-insert-space ()
  "選択範囲内の行頭において、指定数分のスペースを挿入あるいは削除する"
  (interactive)
  (let (space-count (blog-region-end (make-marker)))
    (if (use-region-p)
        (progn
          (setq space-count (read-string "How many spaces ? : "))
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
          (if (re-search-forward "\\[\\[file:\\(.+\\)\\.org\\]\\[\\(.+\\)\\]\\]" nil t)
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
                        (blog-mode)
                        (save-buffer)
                        (kill-buffer (current-buffer)))
                    nil))
              nil)
            (find-file "~/org/blog/Export-list.txt")
            (goto-char (point-min))
            (if (re-search-forward (format "^~/org/blog/%s.org$" delete-file-name) nil t)
                (progn
                  (beginning-of-line)
                  (delete-region (point) (progn (forward-line) (point)))
                  (save-buffer))
              nil)
            (kill-buffer (current-buffer))
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
          (if (re-search-forward "\\[\\[file:\\(.+\\)\\.org\\]\\[\\(.+\\)\\]\\]" nil t)
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
      (blog-mode)
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
                  (blog-mode)
                  (save-buffer)
                  (kill-buffer (current-buffer)))
              nil))
        nil)
      (find-file "~/org/blog/Export-list.txt")
      (goto-char (point-min))
      (if (re-search-forward (format "^~/org/blog/%s.org$" rename-file-name) nil t)
          (progn
            (beginning-of-line)
            (delete-region (point) (progn (forward-line) (point))) ; 新しいファイル名はもう記録されている
            (save-buffer))
        nil)
      (kill-buffer (current-buffer))
      (message (format "[ %s ( %s.org ) ] is renamed [ %s ( %s.org ) ] !" rename-topic-name rename-file-name new-topic-name new-file-name)))))

(defun blog-visited-record (filename)
  "Export-list.txt に現在バッファのファイル名を記録する"
    (find-file "~/org/blog/Export-list.txt")
    (goto-char (point-min))
    (if (re-search-forward (format "%s" filename) nil t)
        nil
      (goto-char (point-max))
      (insert (format "%s\n" filename))
      (save-buffer))
    (kill-buffer (current-buffer)))

(defadvice save-buffer (after blog-save-buffer)
  "ファイルを訪れた後、あるいは新しく作成した後にそのファイル名を Export-list.txt に記録する"
  (if (string= "blog" (format "%s" mode-name))
      (blog-visited-record (format "~/org/blog/%s" (buffer-name (current-buffer))))
    nil))
(ad-activate 'save-buffer)

(defun blog-end ()
  "current-buffer が index.org の時に blog-mode を閉じる"
  (interactive)
  (let (blog-current-buffer-contents blog-current-point blog-window-start (blog-auto-save-files nil) (check-files (directory-files "~/org/blog")))
    (if (y-or-n-p "Do you want to end blog-mode ?")
        (progn
          (setq blog-current-buffer-contents (buffer-substring (point-min) (point-max)))
          (setq blog-current-point (point))
          (setq blog-window-start (window-start))
          (while blog-open-list
            (switch-to-buffer (car blog-open-list))
            (save-buffer)
            (kill-buffer (current-buffer))
            (setq blog-open-list (cdr blog-open-list)))
          (while check-files
            (if (string-match "^#.+#$" (car check-files))
                (setq blog-auto-save-files (cons (car check-files) blog-auto-save-files))
              nil)
            (setq check-files (cdr check-files)))
          (if (not (string= "nil" (format "%s" blog-auto-save-files)))
              (progn
                (get-buffer-create "*Caution*")
                (switch-to-buffer "*Caution*")
                (delete-region (point-min) (point-max))
                (insert (format "%s" blog-current-buffer-contents))
                (goto-char blog-current-point)
                (set-window-start (selected-window) blog-window-start)
                (if (y-or-n-p "Do you want to delete auto save data?")
                    (while blog-auto-save-files
                      (delete-file (format "~/org/blog/%s" (car blog-auto-save-files)))
                      (setq blog-auto-save-files (cdr blog-auto-save-files)))
                  nil)
                (kill-buffer "*Caution*"))
            nil)
          (setq blog-open-list nil)
          (message "Bye!"))
      (message "Process killed"))))

(defun blog-restart ()
  "blog-mode が一時的に保持している情報を破棄して起動し直す"
  (interactive)
  (if (y-or-n-p "Do you really want to restart blog-mode ?")
      (if (not (string= "nil" (format "%s" blog-open-list)))
          (progn
            (while blog-open-list
              (if (get-buffer (car blog-open-list))
                  (progn
                    (switch-to-buffer (car blog-open-list))
                    (save-buffer)
                    (kill-buffer (current-buffer)))
                nil)
              (setq blog-open-list (cdr blog-open-list)))
            (setq blog-open-list nil)
            (start-blog))
        (start-blog))
    (message "Process killed")))

(defun blog-help ()
  "利用できるキーバインドを表示"
  (interactive)
  (get-buffer-create "*blog-help*")
  (switch-to-buffer "*blog-help*")
  (delete-region (point-min) (point-max))
  (insert " C-c n : Make a new topic (Make a link)\n C-c C-l : Insert a stored-link\n M-<RET> : Insert a new heading\n <M-left> or <M-right> : Change the heading level\n <M-Up> or <M-Down> : Rearrange the list\n C-c C-o : Open the topic (Jump to the link destination)\n C-c <C-left> : Go back to previous page\n C-c C-e h H/h/o : Export current-buffer's org-file to HTML-file\n C-c r : Rename the topic (Rename the link and the linked file)\n C-M-d : Delete the topic (Delete the link and the linked file)\n C-c x : Export all visited and newly created org-files to HTML-files\n C-c e : Close blog-mode\n\n S-<TAB> or C-u C-i : Fold all subtrees up to their root level\n <TAB> or C-i : Fold the current subtree up to its root level\n C-c C-, : Insert a code block\n C-c ' : Edit a source code block\n C-c C-c : Execute a source code block\n C-M-i : Display a list of supported languages in the source code block\n C-c M-s : Insert space at the beginning of the line within the region\n C-j : Start a new line considering leading whitespace\n C-c C-n/p : Move to next/previous heading\n\n----------------------------------------------------------------------\n < Syntax note >\n\n - : a list without number\n 1. : a list with number\n (C-c C-c : Renumber the list)\n (C-c - : Change the format of the list)\n\n *bold*\n /italic/\n _underline_\n +strikethrough+\n ~inline code~\n ----- : horizontal rule")
  (while (not (y-or-n-p "Quit from Help ?"))
    nil)
  (kill-buffer "*blog-help*"))

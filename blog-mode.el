;;;; -*- Emacs-Lisp -*-
;;;; blog-mode written by suyeden

(define-key global-map "\C-cb" 'blog-mode)

;;; keymap
(defvar org-mode-map (make-keymap))
;;; define-key
(define-key org-mode-map "\C-c\C-n" 'new-blog)
(define-key org-mode-map "\C-ce" 'end-blog)
(define-key org-mode-map "\C-c\C-h" 'blog-help)
(define-key org-mode-map "\C-cx" 'all-export-to-html)

;;; blog-mode (my new major-mode)
(defun blog-mode ()
  "blog-mode"
  (interactive)
  ;; blog-mode を開く前のバッファを記録
  (defvar my-current-buffer)
  (setq my-current-buffer (current-buffer))
  ;; リンクを開く前のウィンドウを記録
  (defvar my-pre-win)
  (setq my-pre-win (selected-window))
  ;; org-export-dispatch (org-fileからhtml-fileへの変換) のキーボードマクロ
  (fset 'auto-export-to-html
        "\C-c\C-ehh")
  (delete-other-windows)
  (start-blog)
  ;; define-key
  ;; 1. キーバインドに矢印キーを用いたい -> (local-set-key)
  ;; 2. local-set-key をしたいが、 org-mode に対してキーバインドを追加したい
  ;; という理由のため、org-mode が有効になったこのタイミングで設定する
  (local-set-key (kbd "C-c <C-left>") 'back-page)
  (use-local-map org-mode-map)
  (setq major-mode 'org-mode
        mode-name "Org"))

(defun start-blog ()
  "blog-modeを開く"
  (if (file-exists-p "~/org/blog/index.org")
      (progn
        (find-file "~/org/blog/index.org")
        (goto-char (point-min))
        (re-search-forward "* Category" nil t)
        (beginning-of-line))
    (find-file "~/org/blog/index.org")
    (insert "#+TITLE: すえーでんの技術ブログ\n#+AUTHOR: suyeden\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n** [[file:koushin.org][更新履歴]]\n\n------------------------------------------------------------------------------------------\n\n* Category\n\n\n------------------------------------------------------------------------------------------\n")
    (re-search-backward "* Category" nil t)
    (beginning-of-line)))

(defadvice org-open-at-point (before pre-win ())
  "org-open の前に 1つ前の画面を表示するウィンドウを記録"
  (setq my-pre-win (selected-window)))
(ad-activate 'org-open-at-point)

(defun back-page ()
  "1つ前の画面に戻る"
  (interactive)
  (catch 'flag
    (if (string= "index.org" (buffer-name (current-buffer)))
        (progn
          (message "if you want to end blog-mode, type 'C-c e' !")
          (throw 'flag t))
      nil)
    (export-to-html)
    (kill-buffer (current-buffer))
    (select-window my-pre-win)
    (if (string= "index.org" (buffer-name (current-buffer)))
        (delete-window)
      nil)))

(defun new-blog ()
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
        (setq f-or-d (read-string "non-valid variable! file(f) or directory(d) ? : "))
      nil)
    (if (or (string= f-or-d "file") (string= f-or-d "f"))
        (progn
          (insert (format "[[file:%s.org][%s]]" real-name blog-name))
          (backward-char)
          (find-file (format "~/org/blog/%s.org" real-name))
          (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n# file\n*  \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
          (write-file (format "~/org/blog/%s.org" real-name))
          (kill-buffer (current-buffer))
          (if (file-exists-p "~/org/blog/koushin.org")
              (progn
                (find-file "~/org/blog/koushin.org")
                (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
                (goto-char (point-min))
                (re-search-forward "^\\*\\*\\* " nil t)
                (beginning-of-line)
                (insert (format "*** %s [[file:%s.org][%s]] を更新しました。\n" now-time real-name blog-name))
                (write-file "~/org/blog/koushin.org")
                (kill-buffer (current-buffer)))
            (find-file "~/org/blog/koushin.org")
            (setq now-time (format-time-string "%Y/%m/%d(%a)" (current-time)))
            (insert (format "#+TITLE: 更新履歴\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n*** %s [[file:%s.org][%s]] を更新しました。" now-time real-name blog-name))
            (write-file "~/org/blog/koushin.org")
            (kill-buffer (current-buffer))))
      (if (or (string= f-or-d "directory") (string= f-or-d "d"))
          (progn
            (insert (format "[[file:%s.org][%s]]" real-name blog-name))
            (find-file (format "~/org/blog/%s.org" real-name))
            (insert (format "#+TITLE: %s\n#+EMAIL: \n#+LANGUAGE: ja\n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t\n#+STARTUP: showall\n# directory\n* \n------------------------------------------------------------------------------------------\n** [[file:index.org][home]]\n------------------------------------------------------------------------------------------\n" blog-name))
            (write-file (format "~/org/blog/%s.org" real-name))
            (kill-buffer (current-buffer)))
        nil))))

(defun export-to-html ()
  "current-buffer の orgファイル を HTMLファイル にエクスポート"
  (interactive)
  (if (y-or-n-p "export to HTML file? : ")
      (execute-kbd-macro (symbol-function 'auto-export-to-html))
    nil))

(defun all-export-to-html ()
  "すべての orgファイル を HTMLファイル にエクスポートする"
  (interactive)
  (if (y-or-n-p "do you want to export all org-files to HTML-files? : ")
      (progn
        (let ((current-point (point)))
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
          (goto-char current-point)))
    nil))

(defun end-blog ()
  "current-buffer が index.org の時に blog-mode を閉じる"
  (interactive)
  (if (string= "index.org" (buffer-name (current-buffer)))
      (progn
        (if (file-exists-p "~/org/blog/koushin.org")
            (progn
              (find-file "~/org/blog/koushin.org")
              (export-to-html)
              (kill-buffer (current-buffer)))
          nil)
        (export-to-html)
        (kill-buffer (current-buffer)))
    (message "you can't end blog-mode! go to index.org!")))

(defun blog-help ()
  "利用できるキーバインドをメッセージ表示"
  (interactive)
  (message "C-c C-n : make a new topic (make a link)\nM-<RET> : insert a new heading\n<TAB> (next to heading) : demote a heading level\n<TAB> (on heading) : fold the current subtree up to its root level\nC-x C-s : save changes\nC-c C-o : open a topic (jump to a link destination)\nC-c <C-left> : go back to previous page\nC-c x : export all org-files to HTML-files\nC-c e : close blog-mode"))

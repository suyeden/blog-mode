;;;; -*- Emacs-Lisp -*-
;;;; blog-mode written by suyeden

(define-key global-map "\C-cb" 'blog-mode)

;;; keymap
 (defvar org-mode-map (make-keymap))
;;; define-key
(define-key org-mode-map "\C-c\C-n" 'new-blog)
(define-key org-mode-map "\C-ce" 'export-to-html)

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
  (start-blog)
  ;; define-key
  ;; 1. キーバインドに矢印キーを用いたい -> (local-set-key)
  ;; 2. local-set-key をしたいが、 org-mode に対してキーバインドを追加したい
  ;; という特殊な理由のため、org-mode が有効になったこのタイミングで設定する
  (local-set-key (kbd "C-c <C-left>") 'back-page)
  (local-set-key (kbd "C-c <C-up>") 'show-pre-buff)
  (use-local-map org-mode-map)
  (setq major-mode 'org-mode
        mode-name "Org"))

(defun start-blog ()
  "blog-mode を開く"
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
        (throw 'flag t)
      nil)
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

(defun show-pre-buff ()
  "blog-mode を開く前のバッファを隣のバッファに表示"
  (interactive)
  (pop-to-buffer my-current-buffer))

(defun export-to-html ()
  "HTMLファイルにまだエクスポートしていないorgファイルを順にエクスポート"
  (interactive)
  (let (inside-dir testfile orgfile orgfilecopy htmlfile htmlfilecopy orgtestfile htmltestfile readyfile notyetfile deletefile targetfile)
    (setq inside-dir (directory-files "~/org/blog/" t))
    (while inside-dir
      (setq testfile (car inside-dir))
      (if (string-match "\\.org$" (format "%s" testfile))
          (setq orgfile (cons testfile orgfile))
        (if (string-match "\\.html$" (format "%s" testfile))
            (setq htmlfile (cons testfile htmlfile)))
        nil)
      (setq inside-dir (cdr inside-dir)))
    (setq orgfilecopy orgfile)
    (setq htmlfilecopy htmlfile)
    (while orgfile
      (setq testfile (car orgfile))
      (string-match "\\(.+\\).org" (format "%s" testfile))
      (setq orgtestfile (substring (format "%s" testfile) (match-beginning 1) (match-end 1)))
      (catch 'finish
        (while htmlfile
          (setq htmltestfile (car htmlfile))
          (if (string-match (format "%s" orgtestfile) (format "%s" htmltestfile))
              (progn
                (setq readyfile (cons testfile readyfile))
                (throw 'finish t))
            nil)
          (setq htmlfile (cdr htmlfile))))
      (setq htmlfile htmlfilecopy)
      (setq orgfile (cdr orgfile)))
    (setq notyetfile orgfilecopy)
    (while readyfile
      (setq deletefile (car readyfile))
      (delete deletefile notyetfile)
      (setq readyfile (cdr readyfile)))
    (while notyetfile
      (setq targetfile (car notyetfile))
      (find-file targetfile)
      (org-export-dispatch)
      (kill-buffer (current-buffer))
      (setq notyetfile (cdr notyetfile)))
    (find-file "~/org/blog/koushin.org")
    (org-export-dispatch)
    (kill-buffer (current-buffer))))

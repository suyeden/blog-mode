;;;; -*-Emacs Lisp-*-
;;;; nikki-mode written by suyeden

;;; ローカルキーマップ作成
(defvar nikki-mode-map (make-keymap))
;;; define-key
(define-key nikki-mode-map "n" 'nikki-next)
(define-key nikki-mode-map "p" 'nikki-previous)
(define-key nikki-mode-map "x" 'nikki-select)
(define-key nikki-mode-map "q" 'nikki-quit)
(define-key nikki-mode-map "o" 'nikki-list-open)
(define-key nikki-mode-map "\C-cn" 'nikki-new-topic)
(define-key nikki-mode-map "\C-cr" 'nikki-rename)
(define-key nikki-mode-map "\C-cR" 'nikki-replace)
(define-key nikki-mode-map "\C-cf" 'nikki-fix)
(define-key nikki-mode-map "h" 'nikki-help)

;;; メイン関数
(defun nikki ()
  "nikki-mode の本体を定義していく"
  (interactive)
  (defvar my-indent-time)     ; index.txt の行の先頭の空白数を保持
  (setq my-indent-time 0)
  ;; * work* に nikki ディレクトリ内の index.txt の内容を書き込む
  (get-buffer-create "* work*")
  (set-buffer "* work*")
  (erase-buffer)
  (insert-file-contents "~/nikki/index.txt")
  ;; *nikki-list* に（先頭）* ...（末尾）の行を書き込む
  (get-buffer-create "*nikki-list*")
  (set-buffer "*nikki-list*")
  (erase-buffer)
  (save-excursion
    (let (nikkilist)
      (set-buffer "* work*")
      (while (re-search-forward "^\\*.+$" nil t)
        (setq nikkilist (buffer-substring (match-beginning 0) (match-end 0)))
        (switch-to-buffer "*nikki-list*")
        (insert (format "%s\n" nikkilist))
        (set-buffer "* work*"))))
  (nikki-heading)
  ;; 使用するマップの定義
  (use-local-map nikki-mode-map)
  (setq major-mode 'nikki-mode
        mode-name "nikki-mode"))

;;; ここから nikki-mode に必要な関数群

(defun nikki-select ()
  "x を押したときの挙動"
  (interactive)
  ;; for debug!
  (message (format "%d" my-indent-time))
  (let (selectfile)
    ;; x を押した対象項目を記録
    (forward-line 1)
    (re-search-backward "^\\([.*0-9]+\\) \\(.+\\)$" nil t)     ; 今 *nikki-list*
    (setq selectfile (buffer-substring (match-beginning 2) (match-end 2)))
    ;; x を押した項目が * か 数字 かで条件分岐
    (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
        ;; x を押した項目が * のとき
        (progn
          ;; for debug!
          (switch-to-buffer "* work*")
          (goto-char (point-min))
          (re-search-forward (format "%s" selectfile) nil t)     ; x を押した項目へ移動（今 * work*）
          ;; 1つ下の層の項目の始まりが * か - かを調べる
          (setq my-indent-time (1+ my-indent-time))
          (re-search-forward (format "^ \\{%d\\}\\([*-]\\) .+$" my-indent-time) nil t)
          ;; 上記の検索結果によってその後の表示を変える
          (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
              ;; 1つ下の層の項目の始まりが * のとき
              (progn
                ;; 検索範囲を狭めてから
                (beginning-of-line)
                (save-restriction
                  (narrow-to-region
                   (point)
                   (if (re-search-forward (format "^ \\{%d\\}\\* .+$" (1- my-indent-time)) nil t)
                       (progn
                         (forward-line -1)
                         (end-of-line)
                         (point))
                     (point-max)))     ; * work*
                  ;; * から始まる行を検索して *nikki-list* に書き込む
                  (goto-char (point-min))
                  (set-buffer "*nikki-list*")     ; *nikki-list* 更新前の準備
                  (erase-buffer)
                  (set-buffer "* work*")
                  (while (re-search-forward (format "^ \\{%d\\}\\(* .+\\)$" my-indent-time) nil t)
                    (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                    (switch-to-buffer "*nikki-list*")
                    (insert (format "%s\n" selectfile))
                    ;; for debug!
                    (switch-to-buffer "* work*"))))
            ;; 1つ下の層の項目の始まりが - のとき
            ;; 検索範囲を狭めてから
            (beginning-of-line)
            (save-restriction
              (narrow-to-region
               (point)
               (if (re-search-forward (format "^ \\{%d\\}\\* .+$" (1- my-indent-time)) nil t)
                   (progn
                     (re-search-forward (format "^ \\{%d\\}\\* .+$" (1- my-indent-time)) nil t)
                     (forward-line -1)
                     (end-of-line)
                     (point))
                 (point-max)))
              ;; - から始まる行を検索して *nikki-list* に書き込む
              (let ((number 1))
                (goto-char (point-min))
                (set-buffer "*nikki-list*")     ; *nikki-list* 更新前の準備
                (erase-buffer)
                (set-buffer "* work*")
                (while (re-search-forward (format "^ \\{%d\\}- \\(.+ / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+\\)$" my-indent-time) nil t)     ; * work*
                  (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                  (switch-to-buffer "*nikki-list*")
                  (insert (format "%d. %s\n" number selectfile))
                  (1+ number)))))
          (switch-to-buffer "*nikki-list*")
          (goto-char (point-min)))
      ;; x を押した項目が 数字 のとき
      (beginning-of-line)
      (re-search-forward "^[.0-9]+ \\(.+\\) / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+$" nil t)
      (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
      (get-buffer-create "*full-nikki*")
      (switch-to-buffer "*full-nikki*")
      (goto-char (point-min))
      (insert-file-contents (format "~/nikki/%s.txt" selectfile))
      (toggle-read-only)
      (let ((sw (selected-window)))
        (select-window (get-buffer-window "*nikki-heading*"))
        (delete-window)
        (select-window sw)))
    (nikki-heading)))

(defun nikki-heading ()
  "ファイルの先頭部分を隣のバッファに表示する"
  (let (heading-file
        (sw (selected-window)))
    (if (string= "*nikki-list*" (format "%s"(current-buffer)))
        (progn
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "^[.0-9]+ \\(.+\\) / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+$" nil t)
                (progn
                  (setq heading-file (buffer-substring (match-beginning 1) (match-end 1)))
                  (get-buffer-create "*nikki-heading*")
                  (set-buffer "*nikki-heading*")
                  (erase-buffer)
                  (insert-file-contents (format "~/nikki/%s.txt" heading-file) nil 0 (* 10 130))
                  (pop-to-buffer "*nikki-heading*")
                  (select-window sw))
              (if (get-buffer-window "*nikki-heading*")
                  (progn
                    (select-window (get-buffer-window "*nikki-heading*"))
                    (delete-window)
                    (select-window sw))
                nil)
              (if (get-buffer "*nikki-heading*")
                  (kill-buffer "*nikki-heading*")
                nil))))
      nil)))

(defun nikki-next (arg)
  "arg行進む"
  (interactive "p")
  (forward-line arg)
  (nikki-heading))

(defun nikki-previous (arg)
  "arg行戻る"
  (interactive "p")
  (nikki-next (- arg)))

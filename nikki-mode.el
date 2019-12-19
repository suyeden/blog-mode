;;;; -*-Emacs Lisp-*-
;;;; nikki-mode written by suyeden

;;; ローカルキーマップ作成
(defvar nikki-mode-map (make-keymap))
;;; define-key
(define-key nikki-mode-map "n" 'nikki-next)
(define-key nikki-mode-map "p" 'nikki-previous)
(define-key nikki-mode-map "return" 'nikki-select)
(define-key nikki-mode-map "q" 'nikki-quit)
(define-key nikki-mode-map "o" 'nikki-list-open)
(define-key nikki-mode-map "\C-cn" 'nikki-new-topic)
(define-key nikki-mode-map "\C-cr" 'nikki-rename)
(define-key nikki-mode-map "\C-cR" 'nikki-replace)
(define-key nikki-mode-map "\C-cf" 'nikki-fix)
(define-key nikki-mode-map "h" 'nikki-help)

;;; メイン関数
(defun main ()
  "nikki-mode の本体を定義していく"
  (interactive)
  ;; * work* に nikki ディレクトリ内の index.txt の内容を書き込む
  (cd ~/nikki)
  (get-buffer-create "* work*")
  (set-buffer "* work*")
  (erase-buffer)
  (insert-file-contents "~/nikki/index.txt")
  ;; *nikki-list* に（先頭）* ...（末尾）の行を書き込む
  (get-buffer-create "*nikki-list*")
  (set-buffer "*nikki-list*")
  (save-excursion
    (let (nikkilist)
      (set-buffer "* work*")
      (while (re-search-forward "^\\*.+$")
        (setq nikkilist (buffer-substring (match-beginning 0) (match-end 0)))
        (switch-to-buffer "*nikki-list*")
        (insert (format "%s\n" nikkilist))
        (set-buffer " *work*"))))
  ;; 使用するマップの定義
  (use-local-map nikki-mode-map)
  (setq major-mode 'nikki-mode
        mode-name "nikki"))

;;; ここから nikki-mode に必要な関数群

(defun nikki-select ()
  "return-key を押したときの挙動"
  (let (selectfile (time 0))
    ;; return-key を押した対象項目を記録
    (re-search-backward "^\\([*0-9]+\\) \\(.?\\)$")     ; 今 *nikki-list*
    (setq selectfile (buffer-substring (match-beginning 2) (match-end 2)))
    ;; return-key を押した項目が * か 数字 かで条件分岐
    (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
        ;; return-key を押した項目が * のとき
        (progn
          (set-buffer "* work*")
          (point-min)
          (search-forward (format "%s" selectfile))     ; return-key を押した項目へ移動（今 * work*）
          ;; 1つ下の層の項目の始まりが * か - かを調べる
          (setq time (1+ time))
          (re-search-forward (format "^ \\{%d\\}\\([-*]\\) .?$" time))
          ;; 上記の検索結果によってその後の表示を変える
          (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
              ;; 1つ下の層の項目の始まりが * のとき
              (progn
                ;; 検索範囲を狭めてから
                (beginning-of-line)
                (save-restriction
                  (narrow-to-region
                   (point)
                   (progn (re-search-forward (format "^ \\{%d\\}\\* .?$" (1- time))) (forward-line -1) (end-of-line) (point)))
                  ;; * から始まる行を検索して *nikki-list* に書き込む
                  (point-min)
                  (while (re-search-forward (format "^ \\{%d\\}\\(\\* .?\\)$" time))
                    (setq selectfile (buffer-substring (match-beginning1) (match-end 1)))
                    (set-buffer "*nikki-list*")
                    (insert (format "%s\n" selectfile)))))
            ;; 1つ下の層の項目の始まりが - のとき
            ;; 検索範囲を狭めてから
            (beginning-of-line)
            (save-restriction
              (narrow-to-region
               (point)
               (progn (re-search-forward (format "^ \\{%d\\}- .?$" (1- time))) (forward-line -1) (end-of-line) (point)))
              ;; - から始まる行を検索して *nikki-list* に書き込む
              (let ((number 1))
                (point-min)
                (while (re-search-forward (format "^ \\{%d\\}\\(\\* .?\\)$" time))
                  (setq selectfile (buffer-substring (match-beginning1) (match-end 1)))
                  (set-buffer "*nikki-list*")
                  (insert (format "%d. %s\n" number selectfile)))))))
      ;; return-key を押した項目が 数字 のとき
      )))

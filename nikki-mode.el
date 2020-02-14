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
(define-key nikki-mode-map "\C-cr" 'nikki-replace)
(define-key nikki-mode-map "\C-cf" 'nikki-fix)
(define-key nikki-mode-map "h" 'nikki-help)

;;; ローカルキーマップ 2 作成
(defvar nikki-fix-map (make-keymap))
;;; define-key
(define-key nikki-fix-map "\C-cs" 'fix-save)
(define-key nikki-fix-map "\C-cq" 'fix-quit)

;;; ローカルキーマップ 3 作成
(defvar nikki-replace-map (make-keymap))
;;; define-key
(define-key nikki-replace-map "n" 'replace-next)
(define-key nikki-replace-map "p" 'replace-previous)
(define-key nikki-replace-map "x" 'replace-select)
(define-key nikki-replace-map "q" 'replace-quit)
(define-key nikki-replace-map "r" 'replace-nikki)
(define-key nikki-replace-map "\C-q" 'replace-nikki-forced)

;;; 最初に実行される関数
(defun nikki ()
  "nikki-mode の本体を定義していく"
  (interactive)
  (defvar my-indent-time)     ; index.txt の行の先頭の空白数を保持
  (setq my-indent-time 0)
  (defvar nikki-current-topic)
  (setq nikki-current-topic nil)
  (defvar replace-topic)
  (setq replace-topic nil)
  (defvar replace-head)
  (setq replace-head nil)
  (defvar my-indent-time-replace)
  (setq my-indent-time-replace nil)
  (defvar nikki-nowtime)
  (setq nikki-nowtime nil)
  ;; * work* に nikki ディレクトリ内の index.txt の内容を書き込む
  (get-buffer-create "* work*")
  (set-buffer "* work*")
  (erase-buffer)
  (insert-file-contents "~/nikki/index.txt")
  ;; *nikki-list* に（先頭）* ...（末尾）の行を書き込む
  (get-buffer-create "*nikki-list*")
  (switch-to-buffer "*nikki-list*")
  (erase-buffer)
  (save-excursion
    (let (nikkilist)
      (set-buffer "* work*")
      (while (re-search-forward "^\\*.+$" nil t)
        (setq nikkilist (buffer-substring (match-beginning 0) (match-end 0)))
        (switch-to-buffer "*nikki-list*")
        (insert (format "%s\n" nikkilist))
        (set-buffer "* work*"))))
  (toggle-read-only)
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
  (let (selectfile head-of-topic)
    ;; *full-nikki* バッファのときは適用しない
    (if (string= "*full-nikki*" (format "%s" (current-buffer)))
        nil
      ;; *empty-buffer* のときは何もしない
      (if (string= "*empty-buffer*" (format "%s" (current-buffer)))
          nil
        ;; x を押した対象項目を記録
        (forward-line 1)
        (re-search-backward "^\\([.*0-9]+\\) \\(.+\\)$" nil t)     ; 今 *nikki-list*
        (setq selectfile (buffer-substring (match-beginning 2) (match-end 2)))
        ;; x を押した項目が * か 数字 かで条件分岐
        (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
            ;; x を押した項目が * のとき
            (progn
              ;; x を押した項目へ移動（今 * work*）
              ;; for debug!
              (switch-to-buffer "* work*")
              (goto-char (point-min))
              (re-search-forward (format "%s" selectfile) nil t)
              (save-excursion
                (if (re-search-forward (format "^ \\{%d\\}\\([*-]\\) .+$" (1+ my-indent-time)) nil t)
                    (setq head-of-topic (buffer-substring (match-beginning 1) (match-end 1)))
                  (setq head-of-topic nil)))
              ;; 1つ下の層が存在するかどうかで場合分け
              (if head-of-topic
                  (progn
                    ;; 1つ下の層の項目の始まりが * か - かを調べる
                    (setq my-indent-time (1+ my-indent-time))
                    (re-search-forward (format "^ \\{%d\\}\\([*-]\\) .+$" my-indent-time) nil t)
                    ;; 上記の検索結果によってその後の表示を変える
                    (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
                        ;; 1つ下の層の項目の始まりが * のとき
                        (progn
                          ;; 検索範囲を狭めてから(* work*)
                          (let ((indent-count (1- my-indent-time)))
                            (beginning-of-line)
                            (save-restriction
                              (narrow-to-region
                               (point)
                               (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                   (progn
                                     (forward-line -1)
                                     (end-of-line)
                                     (point))
                                 (setq indent-count (1- indent-count))
                                 (if (catch 'found
                                       (while (>= indent-count 0)
                                         (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                             (progn
                                               (forward-line -1)
                                               (end-of-line)
                                               (throw 'found t))
                                           (setq indent-count (1- indent-count)))))
                                     (point)
                                   (point-max))))     ; narrow-to-region
                              ;; * から始まる行を検索して *nikki-list* に書き込む
                              (goto-char (point-min))
                              (set-buffer "*nikki-list*")     ; *nikki-list* 更新前の準備
                              (toggle-read-only)
                              (erase-buffer)
                              (set-buffer "* work*")
                              (while (re-search-forward (format "^ \\{%d\\}\\(* .+\\)$" my-indent-time) nil t)
                                (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                                (switch-to-buffer "*nikki-list*")
                                (insert (format "%s\n" selectfile))
                                ;; for debug!
                                (switch-to-buffer "* work*")))))     ; progn
                      ;; 1つ下の層の項目の始まりが - のとき
                      ;; 検索範囲を狭めてから
                      (let ((indent-count (1- my-indent-time)) nowpoint)
                        (beginning-of-line)
                        (save-restriction
                          (narrow-to-region
                           (point)
                           (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                               (progn
                                 (forward-line -1)
                                 (end-of-line)
                                 (point))
                             (setq indent-count (1- indent-count))
                             (if (catch 'found
                                   (while (>= indent-count 0)
                                     (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                         (progn
                                           (forward-line -1)
                                           (end-of-line)
                                           (throw 'found t))
                                       (setq indent-count (1- indent-count)))))
                                 (point)
                               (point-max))))     ; narrow-to-region
                          ;; - から始まる行を検索して *nikki-list* に書き込む
                          (let ((number 1))
                            (goto-char (point-min))
                            (set-buffer "*nikki-list*")     ; *nikki-list* 更新前の準備
                            (toggle-read-only)
                            (erase-buffer)
                            ;; for debug!
                            (switch-to-buffer "* work*")
                            (while (re-search-forward (format "^ \\{%d\\}- \\(.+ / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+\\)$" my-indent-time) nil t)     ; * work*
                              (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                              (setq nowpoint (point))
                              ;; for debug!
                              (switch-to-buffer "*nikki-list*")
                              (insert (format "%d. %s\n" number selectfile))
                              (setq number (1+ number))
                              ;; for debug!
                              (switch-to-buffer "* work*"))))
                        (goto-char nowpoint)))
                    (switch-to-buffer "*nikki-list*")
                    (goto-char (point-min))
                    (toggle-read-only))
                ;; 1つ下の層が存在しないとき
                (get-buffer-create "*empty-buffer*")
                (switch-to-buffer "*empty-buffer*")
                (insert "There is no file in this topic.")
                (toggle-read-only)
                (use-local-map nikki-mode-map)
                (setq major-mode 'nikki-mode
                      mode-name "nikki-mode")))
          ;; x を押した項目が 数字 のとき
          (beginning-of-line)
          (re-search-forward "^[.0-9]+ \\(.+\\) / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+$" nil t)
          (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
          (setq nikki-current-topic (buffer-substring (match-beginning 1) (match-end 1)))
          (get-buffer-create "*full-nikki*")
          (switch-to-buffer "*full-nikki*")
          (goto-char (point-min))
          (insert-file-contents (format "~/nikki/%s.txt" selectfile))
          (toggle-read-only)
          (let ((sw (selected-window)))
            (if (get-buffer-window "*nikki-heading*")
                (progn
                  (select-window (get-buffer-window "*nikki-heading*"))
                  (delete-window)))
            (select-window sw))
          (switch-to-buffer "*full-nikki*")
          (use-local-map nikki-mode-map)
          (setq major-mode 'nikki-mode
                mode-name "nikki-mode"))))
    nil)
  (nikki-heading))

(defun nikki-heading ()
  "ファイルの先頭部分を隣のバッファに表示する"
  (let (heading-file (sw (selected-window)))
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
      (if (get-buffer "*nikki-heading*")
          (kill-buffer "*nikki-heading*")
        nil))))

(defun nikki-next (arg)
  "arg行進む"
  (interactive "p")
  (forward-line arg)
  (if (string= "*nikki-list*" (format "%s" (current-buffer)))
      (progn
        (save-excursion
          (beginning-of-line)
          (save-excursion
            (re-search-forward "^\\([.*0-9]+\\) \\(.*\\)$"))
          (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
              (setq nikki-current-topic (buffer-substring (match-beginning 2) (match-end 2)))
            (re-search-forward "^[.0-9]+ \\(.*\\) / .*$")
            (setq nikki-current-topic (buffer-substring (match-beginning 1) (match-end 1))))))
    nil)
  (nikki-heading))

(defun nikki-previous (arg)
  "arg行戻る"
  (interactive "p")
  (nikki-next (- arg)))

(defun nikki-quit ()
  "1つ前の画面に戻る"
  (interactive)
  (let (current-buffer-word indent-count selectfile current-topic)
    (if (= (point) (point-max))
        (forward-line -1)
      nil)
    (cond
     ;; カレントバッファが *empty-buffer* のとき
     ((string= "*empty-buffer*" (format "%s" (current-buffer)))
      (progn
        (kill-buffer (current-buffer))
        (switch-to-buffer "*nikki-list*")))
     ;; カレントバッファが *nikki-list* かつ my-indent-time が 0 でないとき
     ((and (/= my-indent-time 0) (string= "*nikki-list*" (format "%s" (current-buffer))))
      (progn
        ;; 1つ上の層の先頭の空白数を記録 (my-indent-time を再設定)
        (setq my-indent-time (1- my-indent-time))
        ;; 現在のバッファでカーソル上にある項目を設定
        (save-excursion
          (beginning-of-line)
          (re-search-forward "^[.*0-9]+ \\(.+\\)$" nil t)
          (setq current-buffer-word (buffer-substring (match-beginning 1) (match-end 1))))
        ;; 記録したら *nikki-list* の内容を削除
        (toggle-read-only)
        (erase-buffer)
        ;; for debug!
        (switch-to-buffer "* work*")
        (goto-char (point-min))
        ;; 再設定した my-indent-time の数値によって場合分け
        (if (= my-indent-time 0)
            ;; my-indent-time = 0 (1つ上の層が限界)のとき
            (progn
              (save-excursion
                (re-search-forward (format "%s" current-buffer-word) nil t)
                (re-search-backward "^\\(\\* .+\\)$" nil t)
                (setq current-topic (buffer-substring (match-beginning 1) (match-end 1))))
              (while (re-search-forward "^\\* .+$" nil t)
                (setq selectfile (buffer-substring (match-beginning 0) (match-end 0)))
                (switch-to-buffer "*nikki-list*")
                (insert (format "%s\n" selectfile))
                ;; for debug!
                (switch-to-buffer "* work*"))
              (switch-to-buffer "*nikki-list*"))
          ;; my-indent-time /= 0 (1つ上の層のさらに上に層がある)のとき
          ;; 記録した項目へ移動 (* work*)
          (re-search-forward (format "%s" current-buffer-word) nil t)
          ;; 検索範囲を狭めて(2つ上の層に挟まれた領域)から1つ上の層の項目を検索
          (setq indent-count (1- my-indent-time))
          (re-search-backward (format "^ \\{%d\\}\\(\\* .+\\)$" indent-count) nil t)
          (setq current-topic (buffer-substring (match-beginning 1) (match-end 1)))
          (forward-line 1)
          (beginning-of-line)
          (save-restriction
            (narrow-to-region
             (point)
             (progn
               (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
               (forward-line -1)
               (end-of-line)
               (point)))     ; narrow-to-region
            (goto-char (point-min))
            (while (re-search-forward (format "^ \\{%d\\}\\(\\* .+\\)$" my-indent-time) nil t)
              (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
              (switch-to-buffer "*nikki-list*")
              (insert (format "%s\n" selectfile))
              ;; for debug!
              (switch-to-buffer "* work*"))     ; while
            (switch-to-buffer "*nikki-list*")))
        (goto-char (point-min))
        (re-search-forward (format "%s" current-topic) nil t)
        (beginning-of-line)
        (toggle-read-only)
        (nikki-heading)))     ; cond条件式1
     ;; カレントバッファが *nikki-list* かつ my-indent-time が 0 (1番最初の画面)のとき
     ((and (= my-indent-time 0) (string= "*nikki-list*" (format "%s" (current-buffer))))
      (progn
        (if (get-buffer "* work*")
            (kill-buffer "* work*")
          nil)
        (if (get-buffer "*nikki-list*")
            (kill-buffer "*nikki-list*")
          nil)
        (if (get-buffer "*nikki-heading*")
            (kill-buffer "*nikki-heading*")
          nil)
        (if (get-buffer "*full-nikki*")
            (kill-buffer "*full-nikki*")
          nil)))
     ;; カレントバッファが *full-nikki* のとき
     ((string= "*full-nikki*" (format "%s" (current-buffer)))
      (progn
        (kill-buffer "*full-nikki*")
        ;; for debug!
        (switch-to-buffer "* work*")
        ;; 新しい * work* に基づいた *nikki-list* の更新
        (goto-char (point-min))
        (re-search-forward (format "%s" nikki-current-topic) nil t)
        (re-search-backward "^ *\\* .*$" nil t)
        (forward-line 1)
        (save-restriction
          (narrow-to-region
           (point)
           (if (re-search-forward "^ *\\* .*$" nil t)
               (progn
                 (re-search-forward "^ *\\* .*$" nil t)
                 (forward-line -1)
                 (end-of-line)
                 (point))
             (point-max)))
          ;; ここで *nikki-list* 更新
          ;; for debug!
          (switch-to-buffer "*nikki-list*")
          (toggle-read-only)
          (erase-buffer)
          ;; for debug!
          (switch-to-buffer "* work*")
          (goto-char (point-min))
          (let (my-topic (my-number 1))
            (while (re-search-forward "^ *- \\(.* / .*\\)$" nil t)
              (setq my-topic (buffer-substring (match-beginning 1) (match-end 1)))
              (switch-to-buffer "*nikki-list*")
              (insert (format "%d. %s\n" my-number my-topic))
              (setq my-number (1+ my-number))
              ;; for debug!
              (switch-to-buffer "* work*"))))
        (switch-to-buffer "*nikki-list*")
        (goto-char (point-min))
        (re-search-forward (format "%s" nikki-current-topic) nil t)
        (beginning-of-line)
        (toggle-read-only)
        (nikki-heading)))
     (t nil))))

(defun nikki-fix ()
  "ファイル内容を変更する"
  (interactive)
  (let (filecontents nowpoint)
    (cond
     ((string= "*full-nikki*" (format "%s" (current-buffer)))
      (progn
        (setq nowpoint (point))
        (setq filecontents (buffer-substring (point-min) (point-max)))
        (get-buffer-create "*nikki-fix*")
        (switch-to-buffer "*nikki-fix*")
        (insert (format "%s" filecontents))
        (goto-char nowpoint)
        ;; 使用するマップの定義
        (use-local-map nikki-fix-map)
        (setq major-mode 'nikki-fix
              mode-name "nikki-fix")))
     ((string= "*nikki-list*" (format "%s" (current-buffer)))
      (progn
        (beginning-of-line)
        (toggle-read-only)
        (defvar oldfilename)
        (setq oldfilename nil)
        (interactive-get-filename)
        (switch-to-buffer "* work*")
        (re-search-forward "^ *\\* " nil t)
        (forward-line -1)
        (end-of-line)
        (switch-to-buffer "*nikki-list*")
        (beginning-of-line)
        (toggle-read-only)))
     (t nil))))

(defun interactive-get-filename ()
  "*nikki-list* で新しく付けたいファイル名を受け付けて関連ファイルやバッファを編集する"
  (let (newfilename head-of-topic)
    (setq newfilename (read-string "新しいファイル名を入力してください。: "))
    ;; 旧ファイル名の取得
    (save-excursion
      (if (= (point) (point-max))
          (forward-line -1)
        nil)
      (beginning-of-line)
      (re-search-forward "^\\([.*0-9]+\\) \\(.*\\)$" nil t)
      (setq head-of-topic (buffer-substring (match-beginning 1) (match-end 1)))
      (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
          (setq oldfilename (buffer-substring (match-beginning 2) (match-end 2)))
        (beginning-of-line)
        (re-search-forward "^[.0-9]+ \\(.*\\) / .*$" nil t)
        (setq oldfilename (buffer-substring (match-beginning 1) (match-end 1)))))
    ;; とりあえず更新日時だけ更新 (先頭が * のときは関係ない)
    (save-excursion
      (re-search-forward "^\\([.*0-9]+\\) .*$" nil t))
    (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
        (beginning-of-line)
      ;; タイムスタンプを更新するかしないか
      (if (y-or-n-p "更新日時を変更しますか?")
          (progn
            ;; * work* と index.txt の更新
            (nikki-timestamp)
            ;; *nikki-list* の更新
            (switch-to-buffer "*nikki-list*")
            (save-excursion
              (re-search-forward "^\\([.0-9]+ .* / \\).*$" nil t))
            (re-search-forward (buffer-substring (match-beginning 1) (match-end 1)) nil t)
            (delete-region (point) (progn (end-of-line) (point)))
            (insert nikki-nowtime)
            ;; 順序の変更 (更新項目を一番上に)
            (let (koushin-topic)
              (setq koushin-topic (buffer-substring (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
              (delete-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point)))
              (goto-char (point-min))
              (insert (format "%s\n" koushin-topic))
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (delete-forward-char 1))
            ;; 横の番号の変更
            (goto-char (point-min))
            (let ((my-number 1))
              (while (re-search-forward "^\\([.0-9]+\\) .* / .*$" nil t)
                (beginning-of-line)
                (re-search-forward (buffer-substring (match-beginning 1) (match-end 1)) nil t)
                (delete-region (point) (progn (beginning-of-line) (point)))
                (beginning-of-line)
                (insert (format "%d." my-number))
                (end-of-line)
                (setq my-number (1+ my-number))))
            (goto-char (point-min))
            (re-search-forward oldfilename nil t)
            (beginning-of-line))
        (beginning-of-line)))
    ;; 新しいファイル名の挿入 (*nikki-list*)
    (re-search-forward oldfilename nil t)
    (delete-region (point) (progn (re-search-backward oldfilename nil t) (point)))
    (insert (format "%s" newfilename))
    ;; 新しいファイル名の挿入 (* work*)
    (switch-to-buffer "* work*")
    (goto-char (point-min))
    (re-search-forward oldfilename nil t)
    (delete-region (point) (progn (re-search-backward oldfilename nil t) (point)))
    (insert (format "%s" newfilename))
    ;; 更新した * work* を index.txt に書き込む
    (let (allcontents (get-buffer-create "* work3*"))
      (setq allcontents (buffer-substring (point-min) (point-max)))
      (switch-to-buffer "* work3*")
      (erase-buffer)
      (insert (format "%s" allcontents))
      (delete-file "~/nikki/index.txt")
      (write-file "~/nikki/index.txt")
      (kill-buffer (current-buffer)))
    ;; テキストファイル本体の名前も変更 (先頭が * でないとき)
    (if (string= "*" (format "%s" head-of-topic))
        nil
      (rename-file (format "~/nikki/%s.txt" oldfilename) (format "~/nikki/%s.txt" newfilename)))))

(defun fix-save ()
  "nikki-fix (*full-nikki*) で修正した内容を記録する"
  (interactive)
  (let (fixcontents (get-buffer-create "* work2*"))
    (save-excursion
      (setq fixcontents (buffer-substring (point-min) (point-max)))
      ;; for debug!
      (switch-to-buffer "*full-nikki*")
      (toggle-read-only)
      (delete-region (point-min) (point-max))
      (insert (format "%s" fixcontents))
      (toggle-read-only)
      ;; for debug!
      (switch-to-buffer "* work2*")
      (delete-region (point-min) (point-max))
      (insert (format "%s" fixcontents)) 
      ;; for debug!
      (switch-to-buffer "* work2*")
      (delete-file (format "~/nikki/%s.txt" nikki-current-topic))
      (write-file (format "~/nikki/%s.txt" nikki-current-topic))
      (kill-buffer (current-buffer))
      (switch-to-buffer "*nikki-fix*")
      (if (y-or-n-p "更新日時を変更しますか?")
          (nikki-timestamp)
        nil)
      (switch-to-buffer "*nikki-fix*"))))

(defun fix-quit ()
  "nikki-fix から抜け出す"
  (interactive)
  (let (nowtopic)
      (if (y-or-n-p "本当に前画面に戻りますか?")
          (progn
            (setq nowpoint (point))
            (kill-buffer "*nikki-fix*")
            (switch-to-buffer "*full-nikki*")
            (goto-char nowpoint))
        nil)))

(defun nikki-timestamp ()
  "タイムスタンプを更新して * work* を書き換えて index.txt も上書きする (順序、タイムスタンプの書き換えのみ)"
  (setq nikki-nowtime (format-time-string "%Y/%m/%d(%a) %H:%M:%S" (current-time)))
  ;; for debug!
  (switch-to-buffer "* work*")
  (let (fixedfile (get-buffer-create "* work2*") newindex)
    (save-excursion
      (re-search-backward (format "%s" nikki-current-topic) nil t)
      (skip-chars-forward "^/")
      (delete-region
       (point)
       (save-excursion
         (progn (end-of-line) (point))))
      (insert (format "/ %s" nikki-nowtime))
      (end-of-line)
      (re-search-backward "^ *- .* / .*$" nil t)
      (setq fixedfile (buffer-substring (match-beginning 0) (match-end 0)))
      (delete-region (point) (progn (end-of-line) (point)))
      (delete-forward-char 1)
      (re-search-backward "^ *\\* .*$" nil t)
      (forward-line 1)
      (insert (format "%s\n" fixedfile)))
    (setq newindex (buffer-substring (point-min) (point-max)))
    (switch-to-buffer "* work2*")
    (delete-region (point-min) (point-max))
    (insert (format "%s" newindex))
    (delete-file "~/nikki/index.txt")
    (write-file "~/nikki/index.txt")
    (kill-buffer (current-buffer))))

(defun nikki-replace ()
  "ファイルの移動を行う"
  (interactive)
  ;; カレントバッファが *nikki-list* のときだけ実行
  (if (string= "*nikki-list*" (format "%s" (current-buffer)))
      (progn
        (setq my-indent-time-replace my-indent-time)
        (let (current-all-topic all-contents now-point)
          (get-buffer-create "* work-replace*")
          (switch-to-buffer "* work-replace*")
          (erase-buffer)
          (switch-to-buffer "* work*")
          (setq all-contents (buffer-substring (point-min) (point-max)))
          (switch-to-buffer "* work-replace*")
          (insert (format "%s" all-contents))
          (goto-char (point-min))
          (switch-to-buffer "*nikki-list*")
          (setq now-point (point))
          (setq current-all-topic (buffer-substring (point-min) (point-max)))     ; *replace-nikki* にコピーするため
          (toggle-read-only)
          (if (= (point) (point-max))
              (forward-line -1)
            nil)
          (save-excursion
            (beginning-of-line)
            (insert "-> ")
            (re-search-forward "\\([.*0-9]+\\) \\(.*\\)$")
            (setq replace-head (buffer-substring (match-beginning 1) (match-end 1)))
            (setq replace-topic (buffer-substring (match-beginning 2) (match-end 2)))
            (if (string= "*" (format "%s" replace-head))
                nil
              (beginning-of-line)
              (re-search-forward "[.0-9]+ \\(.*\\) / .*$")
              (setq replace-topic (buffer-substring (match-beginning 1) (match-end 1)))))
          (skip-chars-forward "-> ")
          (switch-to-buffer "* work-replace*")
          (re-search-forward (format "%s" replace-topic) nil t)
          (if (get-buffer "*replace-nikki*")
              (progn
                (kill-buffer "*replace-nikki*")
                (get-buffer-create "*replace-nikki*"))
            (get-buffer-create "*replace-nikki*"))
          (set-buffer "*replace-nikki*")
          (erase-buffer)
          (insert (format "%s" current-all-topic))
          (switch-to-buffer "*nikki-list*")
          (pop-to-buffer "*replace-nikki*")
          (select-window (get-buffer-window "*replace-nikki*"))
          (toggle-read-only)
          (goto-char now-point)
          (use-local-map nikki-replace-map)
          (setq major-mode 'nikki-replace
                mode-name "nikki-replace")))
    nil))

(defun replace-next (arg)
  "*replace-nikki* バッファ内で上に移動する"
  (interactive "p")
  (forward-line arg))

(defun replace-previous (arg)
  "*replace-nikki* バッファ内で下に移動する"
  (interactive "p")
  (replace-next (- arg)))

(defun replace-select ()
  "*replace-nikki* バッファ内での x を押したときの挙動"
  (interactive)
  (save-excursion
    (let (selectfile head-of-topic)
      ;; x を押した対象項目を記録
      (forward-line 1)
      (re-search-backward "^\\([.*0-9]+\\) \\(.+\\)$" nil t)     ; 今 *nikki-list*
      (setq selectfile (buffer-substring (match-beginning 2) (match-end 2)))
      ;; x を押した項目が * か 数字 かで条件分岐
      (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
          ;; x を押した項目が * のとき
          (progn
            ;; x を押した項目へ移動(* work-replace*)
            (switch-to-buffer "* work-replace*")
            (goto-char (point-min))
            (re-search-forward (format "%s" selectfile) nil t)
            (save-excursion
              (if (re-search-forward (format "^ \\{%d\\}\\([*-]\\) .+$" (1+ my-indent-time-replace)) nil t)
                  (setq head-of-topic (buffer-substring (match-beginning 1) (match-end 1)))
                (setq head-of-topic nil)))
            ;; 1つ下の層が存在するかどうかで場合分け
            (if head-of-topic
                (progn
                  ;; 1つ下の層の項目の始まりが * か - かを調べる
                  (setq my-indent-time-replace (1+ my-indent-time-replace))
                  (re-search-forward (format "^ \\{%d\\}\\([*-]\\) .+$" my-indent-time-replace) nil t)
                  ;; 上記の検索結果によってその後の表示を変える
                  (if (string= "*" (buffer-substring (match-beginning 1) (match-end 1)))
                      ;; 1つ下の層の項目の始まりが * のとき
                      (progn
                        ;; 検索範囲を狭めてから(* work-replace*)
                        (let ((indent-count (1- my-indent-time-replace)))
                          (beginning-of-line)
                          (save-restriction
                            (narrow-to-region
                             (point)
                             (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                 (progn
                                   (forward-line -1)
                                   (end-of-line)
                                   (point))
                               (setq indent-count (1- indent-count))
                               (if (catch 'found
                                     (while (>= indent-count 0)
                                       (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                           (progn
                                             (forward-line -1)
                                             (end-of-line)
                                             (throw 'found t))
                                         (setq indent-count (1- indent-count)))))
                                   (point)
                                 (point-max))))     ; narrow-to-region
                            ;; * から始まる行を検索して *replace-nikki* に書き込む
                            (goto-char (point-min))
                            (switch-to-buffer "*replace-nikki*")     ; *replace-nikki* 更新前の準備
                            (toggle-read-only)
                            (erase-buffer)
                            (switch-to-buffer "* work-replace*")
                            (while (re-search-forward (format "^ \\{%d\\}\\(* .+\\)$" my-indent-time-replace) nil t)
                              (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                              (switch-to-buffer "*replace-nikki*")
                              (insert (format "%s\n" selectfile))
                              (switch-to-buffer "* work-replace*")))))     ; progn
                    ;; 1つ下の層の項目の始まりが - のとき
                    ;; 検索範囲を狭めてから
                    (let ((indent-count (1- my-indent-time-replace)) nowpoint)
                      (beginning-of-line)
                      (save-restriction
                        (narrow-to-region
                         (point)
                         (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                             (progn
                               (forward-line -1)
                               (end-of-line)
                               (point))
                           (setq indent-count (1- indent-count))
                           (if (catch 'found
                                 (while (>= indent-count 0)
                                   (if (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
                                       (progn
                                         (forward-line -1)
                                         (end-of-line)
                                         (throw 'found t))
                                     (setq indent-count (1- indent-count)))))
                               (point)
                             (point-max))))     ; narrow-to-region
                        ;; - から始まる行を検索して *replace-nikki* に書き込む
                        (let ((number 1))
                          (goto-char (point-min))
                          (switch-to-buffer "*replace-nikki*")     ; *replace-nikki* 更新前の準備
                          (toggle-read-only)
                          (erase-buffer)
                          (switch-to-buffer "* work-replace*")
                          (while (re-search-forward (format "^ \\{%d\\}- \\(.+ / [0-9]+/[0-9]+/[0-9]+(.)[ :0-9]+\\)$" my-indent-time-replace) nil t)     ; * work*
                            (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
                            (setq nowpoint (point))
                            (switch-to-buffer "*replace-nikki*")
                            (insert (format "%d. %s\n" number selectfile))
                            (setq number (1+ number))
                            ;; for debug!
                            (switch-to-buffer "* work-replace*"))))
                      (goto-char nowpoint)))
                  (switch-to-buffer "*replace-nikki*")
                  (goto-char (point-min))
                  (toggle-read-only))
              ;; 1つ下の層が存在しないとき
              (if (get-buffer "*nikki-replace-empty*")
                  (progn
                    (kill-buffer "*nikki-replace-empty*")
                    (get-buffer-create "*replace-nikki-empty*")
                    (switch-to-buffer "*replace-nikki-empty*"))
                (get-buffer-create "*replace-nikki-empty*")
                (switch-to-buffer "*replace-nikki-empty*"))
              (erase-buffer)
              (toggle-read-only)
              (use-local-map nikki-replace-map)
              (setq major-mode 'nikki-replace
                    mode-name "nikki-replace")))
        ;; x を押した項目が 数字 のとき
        nil)
      nil)))

(defun replace-quit ()
  "*replace-nikki* バッファ内において1つ前の画面に戻る"
  (interactive)
  (let (current-buffer-word indent-count selectfile current-topic)
    (if (= (point) (point-max))
        (forward-line -1)
      nil)
    (cond
     ;; カレントバッファが *replace-nikki-empty* のとき
     ((string= "*replace-nikki-empty*" (format "%s" (current-buffer)))
      (progn
        (kill-buffer (current-buffer))
        (switch-to-buffer "*replace-nikki*")))
     ;; カレントバッファが *replace-nikki* かつ my-indent-time-replace が 0 でないとき
     ((and (/= my-indent-time-replace 0) (string= "*replace-nikki*" (format "%s" (current-buffer))))
      (progn
        ;; 1つ上の層の先頭の空白数を記録 (my-indent-time-replace を再設定)
        (setq my-indent-time-replace (1- my-indent-time-replace))
        ;; 現在のバッファでカーソル上にある項目を設定
        (save-excursion
          (beginning-of-line)
          (re-search-forward "^[.*0-9]+ \\(.+\\)$" nil t)
          (setq current-buffer-word (buffer-substring (match-beginning 1) (match-end 1))))
        ;; 記録したら *replace-nikki* の内容を削除
        (toggle-read-only)
        (erase-buffer)
        (switch-to-buffer "* work-replace*")
        (goto-char (point-min))
        ;; 再設定した my-indent-time の数値によって場合分け
        (if (= my-indent-time-replace 0)
            ;; my-indent-time-replace = 0 (1つ上の層が限界)のとき
            (progn
              (save-excursion
                (re-search-forward (format "%s" current-buffer-word) nil t)
                (re-search-backward "^\\(\\* .+\\)$" nil t)
                (setq current-topic (buffer-substring (match-beginning 1) (match-end 1))))
              (while (re-search-forward "^\\* .+$" nil t)
                (setq selectfile (buffer-substring (match-beginning 0) (match-end 0)))
                (switch-to-buffer "*replace-nikki*")
                (insert (format "%s\n" selectfile))
                (switch-to-buffer "* work-replace*"))
              (switch-to-buffer "*replace-nikki*"))
          ;; my-indent-time-replace /= 0 (1つ上の層のさらに上に層がある)のとき
          ;; 記録した項目へ移動 (* work-replace*)
          (re-search-forward (format "%s" current-buffer-word) nil t)
          ;; 検索範囲を狭めて(2つ上の層に挟まれた領域)から1つ上の層の項目を検索
          (setq indent-count (1- my-indent-time-replace))
          (re-search-backward (format "^ \\{%d\\}\\(\\* .+\\)$" indent-count) nil t)
          (setq current-topic (buffer-substring (match-beginning 1) (match-end 1)))
          (forward-line 1)
          (beginning-of-line)
          (save-restriction
            (narrow-to-region
             (point)
             (progn
               (re-search-forward (format "^ \\{%d\\}\\* .+$" indent-count) nil t)
               (forward-line -1)
               (end-of-line)
               (point)))     ; narrow-to-region
            (goto-char (point-min))
            (while (re-search-forward (format "^ \\{%d\\}\\(\\* .+\\)$" my-indent-time-replace) nil t)
              (setq selectfile (buffer-substring (match-beginning 1) (match-end 1)))
              (switch-to-buffer "*replace-nikki*")
              (insert (format "%s\n" selectfile))
              (switch-to-buffer "* work-replace*"))     ; while
            (switch-to-buffer "*replace-nikki*")))
        (goto-char (point-min))
        (re-search-forward (format "%s" current-topic) nil t)
        (beginning-of-line)
        (toggle-read-only)))     ; cond条件式1
     ;; カレントバッファが *replace-nikki* かつ my-indent-time-replace が 0 (1番最初の画面)のとき
     ((and (= my-indent-time-replace 0) (string= "*replace-nikki*" (format "%s" (current-buffer))))
      (progn
        (if (get-buffer-window "*replace-nikki*")
            (progn
              (select-window (get-buffer-window "*replace-nikki*"))
              (delete-window)
              (select-window (get-buffer-window "*nikki-list*")))
          (select-window (get-buffer-window "*nikki-list*")))
        (if (get-buffer "* work-replace*")
            (kill-buffer "* work-replace*")
          nil)
        (if (get-buffer "*replace-nikki*")
            (kill-buffer "*replace-nikki*")
          nil)
        (goto-char (point-min))
        (re-search-forward "-> " nil t)
        (while (not (bolp))
          (delete-backward-char 1))
        (toggle-read-only)))
     ;; その他
     (t nil))))

(defun replace-nikki-forced ()
  "replace-nikki モードを強制的に終了する"
  (interactive)
  (setq my-indent-time-replace 0)
  (if (get-buffer-window "*replace-nikki*")
      (progn
        (select-window (get-buffer-window "*replace-nikki*"))
        (delete-window)
        (select-window (get-buffer-window "*nikki-list*")))
    (select-window (get-buffer-window "*nikki-list*")))
  (if (get-buffer "* work-replace*")
      (kill-buffer "* work-replace*")
    nil)
  (if (get-buffer "*replace-nikki*")
      (kill-buffer "*replace-nikki*")
    nil)
  (if (get-buffer "*nikki-replace-empty*")
      (kill-buffer "*nikki-replace-empty*")
    nil)
  (switch-to-buffer "*nikki-list*")
  (goto-char (point-min))
  (re-search-forward "-> " nil t)
  (while (not (bolp))
    (delete-backward-char 1))
  (toggle-read-only))

(defun replace-nikki ()
  "選択したファイルを移動する"
  (interactive)
  )

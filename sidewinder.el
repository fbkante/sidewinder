;; Author: Koji Mitsuda <fbkante2u atmark gmail.com>
;; Keywords: convenience
;; Version: 0.94

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use sidewinder, just add the following code into your .emacs:
;;
;;    (require 'sidewinder)
;;    (sidewinder-mode 1)
;;

(eval-when-compile (require 'cl))

;; version 0.91 ウィンドウ座標がマイナスの場合の挙動を修正した。フレーム中心が存在するスクリーンで拡大するようにした。
;; version 0.92 minibuffer-exit-hook,abort-recursive-editの時にsidewinder-adjustを呼ぶようにした。
;; version 0.93 sidewinder-frame-widthを実装
;; version 0.94 sidewinder-border-listを設定する事により、解像度が異なるマルチスクリーンに対応。

;; todo
;; emacs24.4対応 sidewinder-border-listの置き換え

;;拡大した時のフレーム左端座標。ピクセル単位。
;;floatで指定すると画面比率単位となる。0.0fなら左寄せ、1.0fで右寄せ 0.5fで中心寄せである。
;;ウィンドウ枠やスクロールバーなどの影響で、想定する座標からずれる場合がある。
(defvar sidewinder-left 0.3)
;;拡大した時の横幅。文字単位。floatで指定すると、画面比率単位となる。
(defvar sidewinder-width 0.95)
;;横方向の分割数が、この値以上になると拡大する
(defvar sidewinder-threshold 2)

;;解像度の異なる複数のモニターを使っている時にも正しく動作させるために
;;境界座標リストを設定できるようにした。負の数も受け付ける。但しソートされている必要がある。
;;偽の境界座標を与えるとウィンドウ幅を元に戻した時に問題がある。
(defvar sidewinder-border-list nil)

;;leftを数値化する。
(defun sidewinder-normalize (left)
  (cadr (frame-geom-value-cons 'left left)))

;;leftをパラメータ化する。nが正の整数ならそのまま、負の値ならリストにする。
(defun sidewinder-denormalize (left)
  (if (>= left 0) left (list '+ left)))
  
;;水平方向分割数に応じたフレーム幅を返す。分割数を指定しない時は、拡大時のフレーム幅を返す
(defun sidewinder-frame-width (&optional division)
  (unless division (setq division sidewinder-threshold))
  (if (and sidewinder-mode (>= division sidewinder-threshold))
      (if (floatp sidewinder-width)
	  (floor (* (x-display-pixel-width) sidewinder-width) (frame-char-width))
	sidewinder-width)
    (frame-width)))

;;フレーム中心座標を与えると、拡大すべき画面の左座標と横幅をconsで返す。
(defun sidewinder-select-screen (center border-list)
  (let ((display-width (x-display-pixel-width)))
    (if (null border-list)
	(cons (* display-width (floor center display-width)) display-width)
      (let* ((current (car border-list))
	     (next-list (cdr border-list))
	     (next (car next-list)))
	(if (null next-list)
	    (cons current display-width)
	  (if (< center next)
	      (cons current (- next current))
	    (sidewinder-select-screen center (cdr border-list))))))))

;;frame-parameterのleft-back,width-backにフレームx座標とフレーム幅を保存し、
;;フレーム幅を広げる。
;;フレームx座標は単純な座標を保存するのではなく、表示スクリーン相対座標を保存している。
(defun sidewinder-expand ()
  (let* ((fr (selected-frame))
         (fp (frame-parameters fr))
         (wb (cdr-safe (assq 'width-back fp)))
         (lb (cdr-safe (assq 'left-back fp))))
    (when (and (null wb) (null lb))
      (let* ((w (cdr (assq 'width fp)))
             (l (sidewinder-normalize (cdr (assq 'left fp))))
             (sw sidewinder-width)
             (sl sidewinder-left)
	     (cw (frame-char-width))
	     (center (+ l (* w cw 0.5)))
	     (screen (sidewinder-select-screen center sidewinder-border-list))
             (screen-width (cdr screen))
             (screen-left (car screen)))
        (when (floatp sidewinder-width) (setq sw (floor (* screen-width sidewinder-width) cw)))
        (when (floatp sidewinder-left) (setq sl (truncate (* (- screen-width (* sw cw)) sidewinder-left))))
	(modify-frame-parameters
	 fr
	 (list (cons 'width sw)
	       (cons 'left (sidewinder-denormalize (+ screen-left sl)))
	       (cons 'width-back w)
	       (cons 'left-back (- l screen-left))))))))

;;保存したフレーム横幅とx座標に復帰する
(defun sidewinder-back ()
  (let* ((fr (selected-frame))
         (fp (frame-parameters fr))
         (wb (cdr-safe (assq 'width-back fp)))
         (lb (cdr-safe (assq 'left-back fp))))
    (when (and wb lb)
      (let* ((w (cdr (assq 'width fp)))
             (l (sidewinder-normalize (cdr (assq 'left fp))))
             (display-width (x-display-pixel-width))
             (display-left (* display-width (floor l display-width))))
	(modify-frame-parameters
	 fr
	 (list (cons 'width-back nil)
	       (cons 'left-back nil)
	       (cons 'width wb)
	       (cons 'left (sidewinder-denormalize (+ display-left lb)))))))))

;;frameの水平、垂直分割数を返す
(defun sidewinder-division-inter (node)
  (if (windowp node) (cons 1 1)
    (let ((h 0) (v 0))
      (destructuring-bind (dir edges . windows) node
        (dolist (win windows)
          (destructuring-bind (ch . cv) (sidewinder-division-inter win)
            (setq h (if dir (max h ch) (+ h ch)))
            (setq v (if dir (+ v cv) (max v cv))))))
      (cons h v))))

(defun sidewinder-division (&optional frame)
  (multiple-value-bind (root mini) (window-tree frame) (sidewinder-division-inter root)))

(defun sidewinder-split-horizontally-p ()
    (>= (car (sidewinder-division)) sidewinder-threshold))

;;現在のウィンドウ配置にフレーム幅を対応させる
(defun sidewinder-adjust ()
  (interactive)
  (if (and (sidewinder-split-horizontally-p) sidewinder-mode) (sidewinder-expand) (sidewinder-back)))

(defadvice delete-window (after sidewinder-delete-window)
  (unless (sidewinder-split-horizontally-p) (sidewinder-back)))

(defadvice delete-other-windows (after sidewinder-delete-other-windows)
  (sidewinder-back))

(defadvice split-window (after sidewinder-split-window)
  (if (sidewinder-split-horizontally-p) (sidewinder-expand)))

;;popwin対策
(defadvice delete-other-windows (around popwin:sidewinder-delete-other-windows)
  (let ((division (car (sidewinder-division))))
    ad-do-it
    (when (and (boundp 'position)
               (popwin:position-horizontal-p position)
               (= (1+ division) sidewinder-threshold))
      (sidewinder-expand))))

(defun sidewinder-before-popup ()
  (when sidewinder-mode
    (sidewinder-disable)
    (ad-enable-advice 'delete-other-windows 'around 'popwin:sidewinder-delete-other-windows)
    (ad-activate 'delete-other-windows)))

(defun sidewinder-after-popup ()
  (when sidewinder-mode (sidewinder-enable)))

(defun sidewinder-delayed-adjust ()
  (run-with-idle-timer 0 nil 'sidewinder-adjust))

;;helm,anything終了時の、ウィンドウ構成の変更に対応する。
;;もっと正規な方法がある気もする。
(defadvice abort-recursive-edit (before sidewinder-abort-recursive-edit)
  (sidewinder-delayed-adjust))

(defun sidewinder-enable ()
  (ad-enable-advice 'delete-window 'after 'sidewinder-delete-window)
  (ad-enable-advice 'delete-other-windows 'after 'sidewinder-delete-other-windows)
  (ad-enable-advice 'split-window 'after 'sidewinder-split-window)
  (ad-enable-advice 'abort-recursive-edit 'before 'sidewinder-abort-recursive-edit)
  (ad-disable-advice 'delete-other-windows 'around 'popwin:sidewinder-delete-other-windows)
  (add-hook 'minibuffer-exit-hook 'sidewinder-delayed-adjust)
  (ad-activate 'delete-window)
  (ad-activate 'delete-other-windows)
  (ad-activate 'split-window)
  (ad-activate 'abort-recursive-edit)
  (add-to-list 'frame-inherited-parameters 'width-back)
  (add-to-list 'frame-inherited-parameters 'left-back))

(defun sidewinder-disable ()
  (ad-disable-advice 'delete-window 'after 'sidewinder-delete-window)
  (ad-disable-advice 'delete-other-windows 'after 'sidewinder-delete-other-windows)
  (ad-disable-advice 'split-window 'after 'sidewinder-split-window)
  (ad-disable-advice 'abort-recursive-edit 'before 'sidewinder-abort-recursive-edit)
  (remove-hook 'minibuffer-exit-hook 'sidewinder-delayed-adjust)
  (ad-activate 'delete-window)
  (ad-activate 'delete-other-windows)
  (ad-activate 'split-window)
  (ad-activate 'abort-recursive-edit)
  (setq frame-inherited-parameters (delq 'left-back (delq 'width-back frame-inherited-parameters))))

(define-minor-mode sidewinder-mode
  "sidewinder mode"
  :global t
  (if sidewinder-mode (sidewinder-enable) (sidewinder-disable))
  (sidewinder-adjust))

(provide 'sidewinder)

;; Author: Koji Mitsuda <fbkante2u atmark gmail.com>
;; Keywords: convenience
;; Version: 0.9

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

;;拡大した時のフレーム左端座標。ピクセル単位。
;;floatで指定すると画面比率単位となる。0.0fなら左寄せ、1.0fで右寄せ 0.5fで中心寄せである。
;;ウィンドウ枠やスクロールバーなどの影響で、想定する座標からずれる場合がある。
(defvar sidewinder-left 0.3)
;;拡大した時の横幅。文字単位。floatで指定すると、画面比率単位となる。
(defvar sidewinder-width 0.95)
;;横方向の分割数が、この値以上になると拡大する
(defvar sidewinder-threshold 2)

;;frame-parameterのleft-back,width-backにフレームx座標とフレーム幅を保存し、
;;フレーム幅を広げる。
(defun sidewinder-expand ()
  (let* ((fr (selected-frame))
         (fp (frame-parameters fr))
         (wb (cdr-safe (assq 'width-back fp)))
         (lb (cdr-safe (assq 'left-back fp))))
    (when (and (null wb) (null lb))
      (let* ((w (cdr (assq 'width fp)))
             (l (cdr (assq 'left fp)))
             (sw sidewinder-width)
             (sl sidewinder-left)
             (display-width (x-display-pixel-width))
             (display-left (* display-width (floor l display-width))))
        (when (floatp sidewinder-width) (setq sw (floor (* display-width sidewinder-width) (frame-char-width))))
        (when (floatp sidewinder-left) (setq sl (truncate (* (- display-width (* sw (frame-char-width))) sidewinder-left))))
        (modify-frame-parameters fr
          (list (cons 'width sw) (cons 'left (+ display-left sl)) (cons 'width-back w) (cons 'left-back l)))))))

;;保存したフレーム横幅とx座標に復帰する
(defun sidewinder-back ()
  (let* ((fr (selected-frame))
         (fp (frame-parameters fr))
         (wb (cdr-safe (assq 'width-back fp)))
         (lb (cdr-safe (assq 'left-back fp))))
    (when (and wb lb)
      (let* ((w (cdr (assq 'width fp)))
             (l (cdr (assq 'left fp)))
             (display-width (x-display-pixel-width))
             (display-left (* display-width (floor l display-width))))
        (modify-frame-parameters fr
      	  (list (cons 'width-back nil) (cons 'left-back nil)
      	    (cons 'width wb) (cons 'left (+ display-left (% lb display-width)))))))))

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

(defun sidewinder-enable ()
  (ad-enable-advice 'delete-window 'after 'sidewinder-delete-window)
  (ad-enable-advice 'delete-other-windows 'after 'sidewinder-delete-other-windows)
  (ad-enable-advice 'split-window 'after 'sidewinder-split-window)
  (ad-disable-advice 'delete-other-windows 'around 'popwin:sidewinder-delete-other-windows)
  (ad-activate 'delete-window)
  (ad-activate 'delete-other-windows)
  (ad-activate 'split-window)
  (add-to-list 'frame-inherited-parameters 'width-back)
  (add-to-list 'frame-inherited-parameters 'left-back))

(defun sidewinder-disable ()
  (ad-disable-advice 'delete-window 'after 'sidewinder-delete-window)
  (ad-disable-advice 'delete-other-windows 'after 'sidewinder-delete-other-windows)
  (ad-disable-advice 'split-window 'after 'sidewinder-split-window)
  (ad-activate 'delete-window)
  (ad-activate 'delete-other-windows)
  (ad-activate 'split-window)
  (setq frame-inherited-parameters (delq 'left-back (delq 'width-back frame-inherited-parameters))))

(define-minor-mode sidewinder-mode
	"sidewinder mode"
 	:global t
 	(if sidewinder-mode (sidewinder-enable) (sidewinder-disable))
 	(sidewinder-adjust))

(provide 'sidewinder)

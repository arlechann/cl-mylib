(in-package #:list-queue)

(defun make-list-queue ()
  "空の連結リストキューを生成して返す。"
  (cons nil nil))

(defun list-queue-empty-p (queue)
  "QUEUE が空なら真を返す。"
  (null (car queue)))

(defun list-queue-peek (queue)
  "先頭要素を返す（削除しない）。"
  (when (list-queue-empty-p queue)
    (error "LIST-QUEUE is empty. Cannot peek any element."))
  (caar queue))

(defun list-queue-raw (queue)
  "内部リスト（先頭から末尾まで）を返す。"
  (car queue))

(defun list-queue-enqueue (queue value)
  "末尾に VALUE を追加し、QUEUE を返す。"
  (let ((cell (cons value nil)))
    (if (list-queue-empty-p queue)
        (setf (car queue) cell
              (cdr queue) cell)
        (setf (cdr (cdr queue)) cell
              (cdr queue) cell))
    queue))

(defun list-queue-dequeue (queue)
  "先頭要素を取り出して返す。"
  (when (list-queue-empty-p queue)
    (error "LIST-QUEUE is empty. Cannot dequeue any element."))
  (prog1 (caar queue)
    (or (setf (car queue) (cdar queue))
        (setf (cdr queue) nil))))

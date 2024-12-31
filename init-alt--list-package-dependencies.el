;; usage:
;;  emacs -l ~/.emacs.d/init-alt--list-package-dependencies.el
;;
;; thx to:
;;   how to list emacs package dependencies
;;   https://yoo2080.wordpress.com/2014/05/16/how-to-list-emacs-package-dependencies/
;; and mike's comment there

(require 'cl-lib)

(package-initialize)
(setq package-enable-at-startup nil)


(defvar my-dependency-alist
  (cl-loop for pkg in package-activated-list
        ;; for pkg-vec = (cdr (assq pkg package-alist))
           for pkg-vec = (cadr (assq pkg package-alist))    ;; mod, thx to mike
           when pkg-vec
           collect (cons pkg
                         (cl-loop for req in (package-desc-reqs pkg-vec)
                                  for req-name = (car req)
                                  when (memq req-name package-activated-list)
                                  collect req-name))))


(defun my-grow-list-by-dependency (original-list dependency-alist)
  "Return a new list (of symbols) including all items in ORIGINAL-LIST and
also recursively including all dependency as specified in DEPENDENCY-ALIST."
  (cl-labels ((adjoin-with-dependency (item list)
                                      (setq list (cl-adjoin item list))
                                      (dolist (dep (cdr (assq item dependency-alist)))
                                        (setq list (adjoin-with-dependency dep list)))
                                      list))
    (let ((ret nil))
      (dolist (item original-list ret)
        (setq ret (adjoin-with-dependency item ret))))))

(message "elnode full dependency: %S"
         (sort (my-grow-list-by-dependency (list 'elnode)
                                           my-dependency-alist)
               'string<))


(message "full dependency for elnode and org2blog together: %S"
         (sort (my-grow-list-by-dependency (list 'elnode
                                                 'org2blog)
                                           my-dependency-alist)
               'string<))

(view-echo-area-messages)

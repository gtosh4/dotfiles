(load "term/xterm")

(defun terminal-init-screen ()
    "Terminal initialization function for screen."
    ;; Use the xterm color initialization code.
    (load "term/xterm")
    (terminal-init-xterm)
    (tty-set-up-initial-frame-faces))

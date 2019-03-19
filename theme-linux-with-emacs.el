(require 'color)


(defvar themelinux-theming-method 'ansi
  "How to extract terminal colours from Emacs.

Possible values are `ansi' and `fonts'.

`ansi'  - Use the ansi colors defined in
          `ansi-color-names-vector'. This is the preferred
          method, but it will fail if the theme failed to define
          ansi colors.

`fonts' - Try to extract a nice color palette from inbuilt fonts.
          Legacy method.")


(defvar themelinux--preferred-fonts
  '(
    ;; Color0 uses the background of the default face so we don't include it.
    nil                                 ; Color0
    font-lock-string-face               ; Color1
    font-lock-doc-face                  ; Color2
    font-lock-keyword-face              ; Color3
    font-lock-preprocessor-face         ; Color4 - is this right?
    error                               ; Color5
    font-lock-function-name-face        ; Color6
    default                             ; Color7 - foreground
    ;; Colours 8-15 are alternate colours. Normally this seems to mean they are
    ;; "pale versions" of the main 8 colours, except colour 8, which is halfway
    ;; between the background and foreground. Pywal doesn't seem to use pale
    ;; colours here - it just adds a shadow for color 8, then defers back to the
    ;; first 8 colours.
    shadow                              ; Color8 - pale background.
    ;; Defer to normal colours for colors 9-15.
    font-lock-string-face               ; Color9
    font-lock-doc-face                  ; Color10
    font-lock-keyword-face              ; Color11
    font-lock-preprocessor-face         ; Color12
    error                               ; Color13
    font-lock-function-name-face        ; Color14
    default                             ; Color15 - foreground pale
    )
  ;; TODO: Docstring
  "List of fonts to extract the 16 terminal colours from.")


(defvar themelinux--scripts-directory
  (concat (file-name-directory
           (or
            ;; `load-file-name' should point to this file when loading.
            load-file-name
            ;; For debugging/development: if not loaded as a package, use the
            ;; buffer for this file instead.
            buffer-file-name))
          "python/")
  "Directory where the python scripts for manipulating pywal should be.")


(defvar themelinux--pywal-python-script
  (concat themelinux--scripts-directory "wal_change_colors.py")
  "Name of the python script that sets the theme from 16 colours.")


(defun themelinux--color-name-to-hex (color-name)
  "Convert a `color-name' into a 6-digit hex value.

E.g. \"Orange\" -> \"#FFA500\"."
  ;; Upcase result to make it neat.
  (upcase
   ;; Have to convert to rgb first, *then* convert back to hex.
     (apply
      'color-rgb-to-hex
      (append (color-name-to-rgb
               color-name)
              ;; We have to specify "2" as the fourth argument
              '(2)))))


(defun themelinux--extract-background-color ()
  "Extract the background color from the default font."
  (themelinux--color-name-to-hex
   (face-background 'default)))


(defun themelinux--extract-shadow-color ()
  (themelinux--color-name-to-hex
   (face-foreground 'shadow)))


(defun themelinux--extract-font-colors ()
  "Extract 16 terminal colours from inbuilt fonts."
  ;; TODO: When duplicate colors are found, iterate over fallback fonts.

  ;; First colour is just the background color of the default face.
  (mapcar 'themelinux--color-name-to-hex
          (cons
           (themelinux--extract-background-color)
           (mapcar (lambda (font)
                     (face-foreground font))
                   ;; Ignore the first font - it should be nil because we get
                   ;; the background color a different way.
                   (cdr themelinux--preferred-fonts)))))


(defun themelinux--check-dependencies ()
  "Ensure dependencies are installed. Throws an error if not.

Specifically, this checks that both python and pywal are
installed and accessible from the user's home dir."
  ;; If we're in a pyenv directory, we might accidentally run the virtual
  ;; version of Python instead of the user's root version. To fix this, we
  ;; temporarily change to the user's dir.
  (let ((default-directory "~/"))
    (unless (executable-find "wal")
      (user-error (concat "Could not find 'wal' executable. "
                          "Is Pywal installed and on the path?")))
    ;; TODO: Check wal is up-to-date enough to use, and the python implementation.
    (unless (executable-find "python")
      (user-error (concat "Could not find 'python' executable. "
                          "Is Python installed and on the path?")))))


(defun themelinux--call-pywal-process (colors)
  (let (
        ;; If we're in a pyenv directory, we might accidentally run the virtual
        ;; version of Python instead of the user's root version. To fix this, we
        ;; temporarily change to the user's dir.
        (default-directory "~/")
        ;; The color modification script will work with python 2 or 3, so just
        ;; use the default Python.
        (python-executable "python")
        (theming-script themelinux--pywal-python-script)
        )
    ;; We have to use apply here to expand the list of colors.
    (apply 'call-process
           (append
            ;; Append the first arguments to the colors list to create one long
            ;; list of arguments.
            (list
             python-executable
             ;; These are the positional arguments that `call-process' takes.
             nil nil nil
             theming-script)
            ;; Now we expand the list of colors
            colors))))


(defun themelinux--apply-colors-with-pywal (colors)
  (message "Applying colors: %s" colors)
  (if (eq 0 (themelinux--call-pywal-process colors))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors.")))


(defun themelinux--16-colors-from-ansi ()
  (let* ((ansi-colors-vector
          ;; Duplicate the 8 basic ansi colors to get a 16-color palette.
          (vconcat ansi-color-names-vector
                   ansi-color-names-vector)))
    ;; Ansi colors are inconsistent. The first of the 8 ansi colors may be the
    ;; background color, but it might also be the shadow color. We modify them
    ;; manually to ensure consistency.
    (aset ansi-colors-vector 0 (themelinux--extract-background-color))
    (aset ansi-colors-vector 8 (themelinux--extract-shadow-color))
    (seq-into ansi-colors-vector 'list)))


(defun themelinux-theme-from-ansi ()
  (interactive)
  (themelinux--check-dependencies)
  (themelinux--apply-colors-with-pywal
   (themelinux--16-colors-from-ansi)))


(defun themelinux-theme-from-fonts ()
  (interactive)
  (themelinux--check-dependencies)
  (themelinux--apply-colors-with-pywal
   (themelinux--extract-font-colors)))


(defun themelinux-theme-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  ;; This will actually check dependencies twice, but that's fine - we want to
  ;; do it up front.
  (themelinux--check-dependencies)
  (cond ((eq themelinux-theming-method 'ansi)
         (themelinux-theme-from-ansi))
        ((eq themelinux-theming-method 'fonts)
         (themelinux-theme-from-fonts))
        (t (user-error (format "Unknown theming method: '%s'"
                               themelinux-theming-method)))))


(provide 'theme-linux-with-emacs)
;;; theme-linux-with-emacs.el ends here

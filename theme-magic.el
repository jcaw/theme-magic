;;; theme-magic.el --- Apply your Emacs theme to the rest of Linux  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; URL: https://github.com/jcaw/theme-magic.el
;; Keywords: unix, faces, terminals, extensions
;; Version: 0.1.5
;; Package-Requires: ((emacs "24.4") (seq "1.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; What's the point in an Emacs theme if the rest of Linux looks different?
;;
;; Just call `theme-magic-from-emacs' and your Emacs theme will be applied
;; to your entire Linux session. Now all your colors match!
;;
;; `theme-magic' uses pywal to set its themes. Pywal must be installed
;; separately. When you log out, the theme will be reset to normal. To restore
;; your theme, call "wal -R" in a shell. To reload it whenever you log in, add
;; "pywal -R" to your .xprofile (or whatever file you use to initialise programs
;; when logging in graphically).
;;
;; See the documentation of Pywal for more information:
;; https://github.com/dylanaraps/pywal
;;
;; Please note that pywal version 1.0.0 or greater is required.


;;; Code:


(require 'color)
(require 'font-lock)
(require 'ansi-color)
(require 'seq)


(defvar theme-magic--theming-functions
  '(
    load-theme
    ;; When these are enabled, changing the theme calls wal multiple times.
    ;; Might be fixable by running wal with an idle timer, but the updates would
    ;; be less synchronised. Note that without these, disabling a theme will not
    ;; trigger a wal update.
    ;;
    ;; enable-theme
    ;; disable-theme
    )
  "Functions that should trigger an update of the linux theme.

\(Iff auto-updating is enabled.\)")


(defvar theme-magic--scripts-directory
  (concat (file-name-directory
           (or
            ;; `load-file-name' should point to this file when loading.
            load-file-name
            ;; For debugging/development: if not loaded as a package, use the
            ;; buffer for this file instead.
            buffer-file-name))
          "python/")
  "Directory where the Python scripts for manipulating pywal should be.")


(defvar theme-magic--pywal-python-script
  (concat theme-magic--scripts-directory "wal_change_colors.py")
  "Path to the Python script that sets the theme from 16 colours.")


(defvar theme-magic--pywal-buffer-name "*pywal*"
  "Name to use for pywal's output buffer.")


(defvar theme-magic--preferred-extracted-colors
  '(
    ;; Black
    ;; This is a special face - it should inherit from the background.
    (0 . ((face-background 'default)))
    ;; Red
    ;; The red color is special - it should actually be red whenever
    ;; possible, because it will likely be used to denote errors.
    (1 . (;; Errors tend to be red.
          (face-foreground 'error)))
    ;; Green
    (2 . ((face-foreground 'font-lock-string-face)))
    ;; Yellow
    (3 . ((face-foreground 'font-lock-warning-face)
          (face-foreground 'warning)))
    ;; Blue
    ;; Treat this as the most common color. Treat cyan as the second most
    ;; common.
    (4 . ((face-foreground 'font-lock-keyword-face)
          (face-foreground 'font-lock-constant-face)))
    ;; Purple
    (5 . ((face-foreground 'font-lock-constant-face)))
    ;; Cyan
    (6 . (;; The function name face tends to be the most dominant color.
          (face-foreground 'font-lock-function-name-face)
          ;; The builtin face matches in at least one theme
          ;; TODO: Does it match builtin in all themes?
          ;; Answer: not in zenburn.
          (face-foreground 'font-lock-builtin-face)
          (face-foreground 'font-lock-constant-face)))
    ;; White
    (7 . ((face-foreground 'default)))
    ;; Black-light
    (8 . ((face-foreground 'shadow)
          (face-foreground 'font-lock-comment-face)))
    ;; The rest of the light faces should inherit from their regular
    ;; equivalents.
    )
  "How should we extract each color?

This should be an alist of font numbers, mapped to a list of
colors. Each color should be a form that can be evaluated. For
example:

   '((1 . ((font-foreground 'preferred-face)
           (font-background 'backup-face))))

")


(defvar theme-magic--fallback-extracted-colors
  '(
    ;; These two faces are the dominant faces. Use them up first.
    (face-foreground 'font-lock-keyword-face)
    (face-foreground 'font-lock-function-name-face)

    (face-foreground 'font-lock-variable-name-face)
    (face-foreground 'font-lock-constant-face)
    (face-foreground 'font-lock-doc-face)
    (face-foreground 'font-lock-string-face)
    (face-foreground 'font-lock-type-face)

    ;; These four faces tend to be similar in color
    (face-foreground 'font-lock-negation-char-face)
    (face-foreground 'font-lock-preprocessor-face)
    (face-foreground 'font-lock-regexp-grouping-backslash)
    (face-foreground 'font-lock-regexp-grouping-construct)

    ;; Other faces of interest
    (face-foreground 'button)
    (face-foreground 'custom-variable-tag)
    (face-foreground 'message-cited-text)
    (face-foreground 'message-header-cc)
    (face-foreground 'message-cited-text)
    (face-foreground 'message-header-cc)
    (face-foreground 'message-header-name)
    (face-foreground 'message-header-newsgroups)
    (face-foreground 'message-header-other)
    (face-foreground 'message-header-subject)
    (face-foreground 'message-header-to)
    (face-foreground 'message-header-xheader)
    (face-foreground 'message-mml)
    (face-foreground 'message-separator)
    (face-foreground 'success)

    ;; TODO: Maybe include the mode line colors?

    ;; Some font-lock faces that should not be used as fallbacks.
    ;; font-lock-builtin-face
    ;; font-lock-comment-delimiter-face
    ;; font-lock-comment-face
    ;; font-lock-warning-face
    )
  "Colors to fall back on if the preferred faces are invalid.

Each color should be a form that can be evaluated. For example:

    '(face-foreground 'button)

If a color cannot be filled by one of the preferred faces, this
list will be scanned for the first valid color. That face will be
used instead. This list is ordered best to worst.

A valid color is defined as a color that hasn't been used
already." )


(defvar theme-magic--color-priority
  ;; Split over multiple lines for easy commenting and reordering.
  '(
    ;; Black (background)
    ;; Black and white _must_ be set correctly, so they're first.
    0
    ;; White
    7
    ;; Red
    ;;
    ;; Red is special because it's used for warnings. It's important that red
    ;; has a high chance of nabbing the error color, so we define it quickly.
    ;; Note that this does cause some conflicts, e.g. in `monokai', where the
    ;; error color is also used for keywords.
    ;;
    ;; TODO: Maybe add a special fallback for red, that evaluates whether a
    ;;   color has a sufficient red content to be used as a warning?
    1
    ;; Black-light
    8
    ;; Cyan - seems most popular
    6
    ;; Blue - also seems popular
    4
    ;; Yellow
    3
    ;; Purple
    5
    ;; Green
    2
    )
  "The order in which to assign extracted colors to ANSI values.

When extracting colors, the colors higher on this list get first
pick. If a later color runs into a duplicate, it will have to use
a fallback color.")


(defvar theme-magic--same-color-threshold 0.02
  "Difference in values at which two colors should be considered the same."
  ;; TODO: Explain same-color-threshold properly
  )


(defun theme-magic--color-name-to-hex (color-name)
  "Convert a `COLOR-NAME' into a 6-digit hex value.

E.g. \"Orange\" -> \"#FFA500\".

Note that this conversion method IS LOSSY. If you supply a hex
name as the color-name, it may spit out a slightly different hex
value due to rounding errors."
  (if color-name
      ;; Upcase result to make it neat.
      (upcase
       ;; Have to convert to rgb first, *then* convert back to hex.
       (apply
        'color-rgb-to-hex
        (append (color-name-to-rgb
                 color-name)
                ;; We have to specify "2" as the fourth argument
                '(2))))
    nil))


(defun theme-magic--color-difference (color1 color2)
  (let ((color1-rgb (color-name-to-rgb color1))
        (color2-rgb (color-name-to-rgb color2))
        (max-difference 0))
    ;; I want to avoid accidentally comparing alphas. Explicitly compare the
    ;; red, green and blue.
    (max (abs (- (nth 0 color1-rgb) (nth 0 color2-rgb)))
         (abs (- (nth 1 color1-rgb) (nth 1 color2-rgb)))
         (abs (- (nth 2 color1-rgb) (nth 2 color2-rgb))))))


(defun theme-magic--colors-match (color1 color2)
  "Check if two colors are very similar - whether they look the same.

If they look the same (are minimally different), they match.

Returns `t' if they match, `nil' if not."
  ;; Failsafe - only compare if both colors are defined.
  (if (and color1 color2)
      (progn
        ;; The colors are only the same if the difference is within the acceptable
        ;; threshold.
        (<= (theme-magic--color-difference color1 color2)
            theme-magic--same-color-threshold))
    ;; If one of the colors is nil, they don't match. Even if both are nil, they
    ;; don't match.
    nil))


(defun theme-magic--extract-background-color ()
  "Extract the background color from the default font."
  (theme-magic--color-name-to-hex
   (face-background 'default)))


(defun theme-magic--extract-shadow-color ()
  "Extract the color of the shadow face, in hex."
  (theme-magic--color-name-to-hex
   (face-foreground 'shadow)))


(defun theme-magic--extract-default-color ()
  "Extract the foreground color of the default face, in hex."
  (theme-magic--color-name-to-hex
   (face-foreground 'default)))


(defun theme-magic--safe-eval (form)
  "Call `eval' on `FORM', catching any errors.

If an error is thrown, just return nil. Does not propagate the
error. Does not interrupt execution."
  (condition-case nil
      (eval form)
    (error nil)))


(defun theme-magic--check-dependencies ()
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


(defun theme-magic--erase-pywal-buffer ()
  "Erase the contents of the pywal output buffer iff it exists."
  (when (get-buffer theme-magic--pywal-buffer-name)
    (with-current-buffer theme-magic--pywal-buffer-name
      (erase-buffer))))


(defun theme-magic--call-pywal-process (colors)
  "Call the Python script that sets the theme with pywal.

`COLORS' should be the 16 hexadecimal colors to use as the theme.

This just calls the python script from the home directory. It
doesn't provide any wrapper feedback to the user."
  ;; Kill pywal buffer if it already exists
  (theme-magic--erase-pywal-buffer)
  (let (
        ;; If we're in a pyenv directory, we might accidentally run the virtual
        ;; version of Python instead of the user's root version. To fix this, we
        ;; temporarily change to the user's dir.
        (default-directory "~/")
        ;; The color modification script will work with python 2 or 3, so just
        ;; use the default Python.
        (python-executable "python")
        (theming-script theme-magic--pywal-python-script)
        )
    ;; We have to use apply here to expand the list of colors.
    (apply 'call-process
           (append
            ;; Append the first arguments to the colors list to create one long
            ;; list of arguments.
            (list
             python-executable
             ;; These are the positional arguments that `call-process' takes.
             nil theme-magic--pywal-buffer-name t
             theming-script)
            ;; Now we expand the list of colors
            colors))))


(defun theme-magic--apply-colors-with-pywal (colors)
  "Change the linux theme to use the 16 `COLORS' (using pywal).

`COLORS' should be a list of 16 hexadecimal terminal colors.

Provides some wrapper feedback to the user, plus some error
handling."
  (message "Applying colors: %s"
           ;; Number the colors to make it clearer for the user which color is
           ;; being applied where.
           (mapcar* #'cons
                    (number-sequence 0 (length colors))
                    colors))
  (if (zerop (theme-magic--call-pywal-process colors))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors. See buffer \"*pywal*\" for details.")))


(defun theme-magic--16-colors-from-ansi ()
  "Construct a set of 16 terminal colors from the current ansi colors."
  (let* ((ansi-colors-vector
          ;; Duplicate the 8 basic ansi colors to get a 16-color palette.
          (vconcat ansi-color-names-vector
                   ansi-color-names-vector)))
    ;; Ansi colors are inconsistent. The first of the 8 ansi colors may be the
    ;; background color, but it might also be the shadow color. We modify them
    ;; manually to ensure consistency.
    (aset ansi-colors-vector 0 (theme-magic--extract-background-color))
    (aset ansi-colors-vector 8 (theme-magic--extract-shadow-color))
    ;; Some themes mess up the foreground color (seen in `material-theme').
    ;; Foreground color is very important anyway, and should match Emacs even if
    ;; it deviates from the Ansi palette. Manually fix it.
    (aset ansi-colors-vector 7 (theme-magic--extract-default-color))
    ;; Finally, we ensure each color is hexadecimal. (We also want to output a
    ;; list - this will also serve that purpose.)
    (mapcar 'theme-magic--color-name-to-hex
            ansi-colors-vector)))


(defun theme-magic--get-preferred-colors (ansi-index)
  (mapcar (lambda (color-form)
            (theme-magic--color-name-to-hex
             (theme-magic--safe-eval color-form)))
          (alist-get ansi-index theme-magic--preferred-extracted-colors)))


(defun theme-magic--color-taken (color existing-colors)
  (catch 'color-taken
    (mapc (lambda (existing-color)
            ;; `existing-color' will be a cons cell, because it comes from an
            ;; alist. Take the `cdr' - this is the color string.
            (when (theme-magic--colors-match (cdr existing-color) color)
              (throw 'color-taken t)))
          existing-colors)
    nil))


(defun theme-magic--extract-color (ansi-index existing-colors)
  (let ((possible-colors (theme-magic--get-preferred-colors ansi-index)))
    ;; Check each color in turn to see if it's a new color. If it is, stop
    ;; immediately and return it.
    (catch 'new-color
      (mapc (lambda (possible-color)
              (unless (and possible-color
                           (theme-magic--color-taken possible-color existing-colors))
                (throw 'new-color possible-color)))
            possible-colors)
      ;; If no color could be extracted, return nil for now.
      nil)))


(defun theme-magic--extract-fallback-color (ansi-index existing-colors)
  (catch 'new-color
    (mapc (lambda (possible-color-form)
            (let ((possible-color (theme-magic--color-name-to-hex
                                   (theme-magic--safe-eval possible-color-form))))
              ;; When the color exists and is not taken, we have a match.
              (when (and possible-color
                         (not (theme-magic--color-taken possible-color existing-colors)))
                (throw 'new-color possible-color))))
          theme-magic--fallback-extracted-colors)
    nil))


(defun theme-magic--force-extract-color (ansi-index)
  (theme-magic--color-name-to-hex
   (or (theme-magic--safe-eval (car (alist-get ansi-index theme-magic--preferred-extracted-colors)))
       ;; It's possible even the above will return nil, because the preferred
       ;; color form fails to evaluate. As a final fallback, just use the ANSI
       ;; color.
       (nth ansi-index (theme-magic--16-colors-from-ansi))
       ;; Final failsafe - should never get here, but just in case, a neutral
       ;; color.
       "#888888")))


(defun theme-magic--auto-extract-16-colors ()
  ;; Note that color extraction is worst-case speed complexity o(n*16), where
  ;; `n' is (roughly) the number of color options (preferred and fallback). This
  ;; scales faster than O(n) but it should still be negligible.
  ;;
  ;; This can be easily reduced by ignoring colors we've already tried to fall
  ;; back to, but with 16 colors, it's not worth it.
  (let (
        ;; `extracted-colors' is an alist mapping ANSI numbers to colors.
        (extracted-colors '())
        )
    ;; Go through the colors in the preferred order, and attempt to extract a
    ;; color for each.
    (mapc (lambda (ansi-index)
            (push (cons ansi-index
                        (theme-magic--extract-color ansi-index extracted-colors))
                  extracted-colors))
          theme-magic--color-priority)

    ;; It may not have been possible to find unique colors for every index. We
    ;; go through any indexes that were missed, and try to find a fallback
    ;; color.
    ;;
    ;; Note that we do this as a separate process, *after* the initial color
    ;; assignments. This is because we don't want nil colors to go through the
    ;; fallback list and accidentally take the primary choices of later colors.
    ;; Every color should have a chance at its primary choice *first*. Only then
    ;; do we go back and fill in the blanks.
    (mapc (lambda (ansi-index)
            (unless (alist-get ansi-index extracted-colors)
              ;; TODO: Is this an in-place deletion?
              (assq-delete-all ansi-index extracted-colors)
              (push (cons ansi-index
                          (or
                           ;; Try and find an unused color in the fallback colors.
                           (theme-magic--extract-fallback-color ansi-index extracted-colors)
                           ;; If we couldn't find a unique color, fall back to
                           ;; the best duplicate color.
                           (theme-magic--force-extract-color ansi-index)))
                    extracted-colors)))
          theme-magic--color-priority)

    ;; We now have an alist of the first 9 ANSI indexes, mapped to colors. We
    ;; need to return a straight list of 16 colors. Extract the colors one by
    ;; one.
    (append (mapcar (lambda (index)
                      (alist-get index extracted-colors))
                    '(0 1 2 3 4 5 6 7 8))
            ;; For now, colors 9-15 (the "light" color variants) should just
            ;; mirror their non-light counterparts.
            (mapcar (lambda (index)
                      ;; Subtract 8 to get the dark version of the light index.
                      (alist-get (- index 8) extracted-colors))
                    '(9 10 11 12 13 14 15)))))


;;;###autoload
(defun theme-magic-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  ;; This will actually check dependencies twice, but that's fine - it's cheap
  ;; we want to do it up front.
  (theme-magic--check-dependencies)
  (theme-magic--apply-colors-with-pywal
   (theme-magic--16-colors-from-ansi)))


(defun theme-magic-from-emacs--wrapper (&rest _)
  "Wrapper for `theme-magic-from-emacs' to be used as advice."
  (theme-magic-from-emacs))


;;;###autoload
(define-minor-mode theme-magic-export-theme-mode
  "Automatically export the Emacs theme to all Linux terminals, using Pywal.

If this mode is active, the Linux theme will be updated
automatically when you change the Emacs theme.

Note that if an Emacs theme has already been set, it will not be
exported when this mode is activated. You must explicitly export
it, or change the theme again to trigger the auto-update.

Under the hood, this mode calls `theme-magic-from-emacs' when you
change the theme. See `theme-magic-from-emacs' for more
information."
  :lighter " TME"
  :global t
  :after-hook (if theme-magic-export-theme-mode
                  ;; Was disabled. We should now enable.
                  (progn
                    (theme-magic--enable-auto-update)
                    ;; TODO: Maybe update the theme overtly now? It will slow down
                    ;; startup of the mode (and consquently, Emacs) so might be
                    ;; best to leave this to the user.
                    )
                ;; Was enabled. We should now disable.
                (theme-magic--disable-auto-update)))


(defun theme-magic--enable-auto-update ()
  "Enable automatic Linux theme updating.

Once enabled, the Linux theme will be updated whenever the Emacs
theme is changed.

Note that if an Emacs theme has already been set, it will not be
exported - you must do that manually or change the theme again."
  (mapc (lambda (func)
          (advice-add func :after 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


(defun theme-magic--disable-auto-update ()
  "Disable automatic Linux theme updating.

Once disabled, the Linux theme will need to be updated manually
with `theme-magic-from-emacs'."
  (mapc (lambda (func)
          (advice-remove func 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


(provide 'theme-magic)
;;; theme-magic.el ends here

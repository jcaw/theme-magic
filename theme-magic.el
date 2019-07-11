;;; theme-magic.el --- Apply your Emacs theme to the rest of Linux  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; URL: https://github.com/jcaw/theme-magic.el
;; Keywords: unix, faces, terminals, extensions
;; Version: 0.2.3
;; Package-Requires: ((emacs "25") (seq "1.8"))

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
(require 'cl-lib)


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
    ;; This is a special face - it should match the background.
    (0 . ((face-background 'default)))
    ;; Red
    ;; The red color should look like an error, because it is probably going
    ;; to be used to denote errors.
    (1 . (
          ;; The error face is best. The error face also tends to actually
          ;; be red.
          (face-foreground 'error)
          ;; Sometimes, errors are denoted by their background color.
          (face-background 'error)
          ;; The warning face hopefully also looks like an error. But, it is
          ;; less likely to be red.
          (face-foreground 'warning)
          ;; Likewise, sometimes warnings are denoted by their background.
          (face-background 'warning)))
    ;; Yellow
    ;; Try to give yellow a warning face, if available.
    (3 . ((face-foreground 'font-lock-warning-face)
          (face-foreground 'warning)))
    ;; Cyan
    ;; Cyan needs to be the secondary dominant face.
    (6 . ((face-foreground 'font-lock-function-name-face)
          (face-foreground 'font-lock-variable-name-face)))
    ;; White
    ;; Special color - it should match normal text.
    (7 . ((face-foreground 'default)))
    ;; Black-light
    ;; Special color - it is used for text that's faded, e.g. in code
    ;; comments (but note that while most themes make shadow a faded color,
    ;; the comment face can sometimes be vibrant).
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
           (font-background 'backup-face))))")


(defvar theme-magic--fallback-extracted-colors
  '(
    ;; These faces are ordered by preferred dominance. Colors at the top will be
    ;; placed in more dominant color slots.
    ;; ------------------------------------------------------------------------

    ;; These two faces are the two primary, dominant faces. Use them up first.
    (face-foreground 'font-lock-keyword-face)
    (face-foreground 'font-lock-function-name-face)

    ;; Some themes use a colorful comment face, such as `spacemacs-dark' and
    ;; `zenburn'. These colors consequently become very dominant. Use the
    ;; comment face, but only if it's colorful.
    (theme-magic--filter-unsaturated
     (face-foreground 'font-lock-comment-face))
    ;; Strings tend to be common (and long), so the string face becomes
    ;; dominant.
    (face-foreground 'font-lock-string-face)
    ;; Docstrings are common too (perhaps more common) but docstring colors tend
    ;; to be uglier than string colors. We therefore demote it, slightly.
    (theme-magic--filter-unsaturated
     (face-foreground 'font-lock-doc-face))
    ;; variables, constants and types are peppered throughout code. These colors
    ;; are less common, but are still defining colors of the color scheme.
    ;;
    ;; HACK: Some doom themes set the variable name to white
    ;;   (e.g. `doom-vibrant'). Only accept colorful variable names.
    (theme-magic--filter-unsaturated
     (face-foreground 'font-lock-variable-name-face))
    (face-foreground 'font-lock-constant-face)
    ;; HACK: At least one doom theme sets the type face to be white too
    ;;   (e.g. `doom-peacock').
    (theme-magic--filter-unsaturated
     (face-foreground 'font-lock-type-face))

    ;; Other faces of interest
    (face-foreground 'link)
    (face-foreground 'button)
    (face-foreground 'custom-variable-tag)
    (face-foreground 'success)

    ;; As a last resort, use the ansi colors themselves. These should only be
    ;; used if all the other colors have been used up.
    ;;
    ;; Don't use colors 0 or 7 (black and white).
    (theme-magic--get-ansi-color 4)  ; Blue
    (theme-magic--get-ansi-color 6)  ; Cyan
    (theme-magic--get-ansi-color 3)  ; Yellow
    (theme-magic--get-ansi-color 5)  ; Magenta
    (theme-magic--get-ansi-color 2)  ; Green
    (theme-magic--get-ansi-color 1)  ; Red
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
    ;; Black-light
    8
    ;; Blue - seems most popular
    4
    ;; Cyan - also seems popular
    6
    ;; Red
    ;;
    ;; Red is special because it's used for warnings. It's important that red
    ;; has a high chance of nabbing the error color, so we define it relatively
    ;; quickly.
    ;;
    ;; Note that this causes conflicts, e.g. in `monokai', where red is used for
    ;; errors and keywords. Nabbing red too early makes the output look
    ;; terrible.
    1
    ;; Green - seems to be third most popular
    2
    ;; Purple
    5
    ;; Yellow
    3
    )
  "The order in which to assign extracted colors to ANSI values.

When extracting colors, the colors higher on this list get first
pick. If a later color runs into a duplicate, it will have to use
a fallback color.")


(defvar theme-magic--same-color-threshold 0.1
  "Max difference between RGB values for two colors to be considered the same.

Refers to RGB values on the 0.0 to 1.0 scale.

When generating a set of colors, it's important that the same
color is not duplicated. Each ANSI color should look different,
if possible. Two very similar colors are generated. This is the
threshold at which we say \"these colors are too visually
similar, we should treat them as the same.\"

There is some slack in this variable. At higher values, such as
0.1, colors that are visually distinct will be treated as the
same. That's fine - it stops very similar colors from being
generated.")


(defvar theme-magic--saturated-color-threshold 0.1
  "Threshold at which a color counts as \"saturated\".

This corresponds to the saturation component of the HSV color
value (scale 0.0 to 1.0). If a color has a saturation value equal
to or above this value, it counts as saturated, rather than
greyscale.")


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
  "Calculate the difference between two colors.

For the purposes of this method, this is the max of all the
differences in RGB values.

The difference is returned on a scale of 0.0 to 1.0

In more detail: the red, green and blue values of `COLOR1' and
`COLOR2' are each compared. R to R, G to G, and B to B. The
difference is the maximum of these differences."
  (let ((color1-rgb (color-name-to-rgb color1))
        (color2-rgb (color-name-to-rgb color2))
        (max-difference 0))
    (max (abs (- (nth 0 color1-rgb) (nth 0 color2-rgb)))
         (abs (- (nth 1 color1-rgb) (nth 1 color2-rgb)))
         (abs (- (nth 2 color1-rgb) (nth 2 color2-rgb))))))


(defun theme-magic--measure-saturation (color)
  "How saturated is `COLOR' on a scale of 0.0 to 1.0?

Uses the saturation component of HSV.

If `COLOR' is nil, the saturation is treated as 0."
  (if color
      ;; Use HSV over HSL for more consistent results on light colors.
      (nth 1 (apply 'color-rgb-to-hsv
                    (color-name-to-rgb color)))
    0))


(defun theme-magic--filter-unsaturated (color)
  "Return color iff `COLOR' is not close to greyscale.

Otherwise, return nil.

If color is saturated enough, it's ok. Otherwise, treat it as
greyscale.

In practical terms, this method eliminates colors that are shades
of grey, rather than shades of a color."
  (if (> (theme-magic--measure-saturation color)
         theme-magic--saturated-color-threshold)
        color
    nil))


;; TODO: Rename to embody the fact it's comparing similarity, not equality.
(defun theme-magic--colors-match (color1 color2)
  "Check if two colors look very similar.

The R, G and B components of `COLOR1' and `COLOR2' are compared,
and the biggest difference is measured. If this difference is
below a certain threshold, it is assumed that the colors are
similar enough that they count as a match.

The threshold is defined in `theme-magic--same-color-threshold'.

Returns t if they match, nil if not."
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
  "Call `eval' on `FORM', ignoring any errors.

This method ensures the program is not interrupted in the case of
an error. If an error does occur, this method will catch it and
return nil."
  (condition-case nil
      (eval form)
    (error nil)))


(defun theme-magic--check-dependencies ()
  "Ensure dependencies are installed. Throw an error if not.

Specifically, this checks that both Python and Pywal are
installed - and accessible from the user's home dir."
  ;; If we're in a pyenv directory, we might accidentally run the virtual
  ;; version of Python instead of the user's root version. To fix this, we
  ;; temporarily change to the user's dir.
  (let ((default-directory "~/"))
    (unless (executable-find "python")
      (user-error (concat "Could not find 'python' executable. "
                          "Is Python installed and on the path?")))
    (unless (executable-find "wal")
      (user-error (concat "Could not find 'wal' executable. "
                          "Is Pywal installed and on the path?")))
    ;; TODO: Check wal is up-to-date enough to use, and the python implementation.
    ))


(defun theme-magic--erase-pywal-buffer ()
  "Erase the contents of the pywal output buffer iff it exists."
  (when (get-buffer theme-magic--pywal-buffer-name)
    (with-current-buffer theme-magic--pywal-buffer-name
      (erase-buffer))))


(defun theme-magic--call-pywal-process (colors)
  "Call the Python script that sets the theme with Pywal.

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
  (message "Applying colors:\n%s"
           ;; Number the colors to make it clearer for the user which color is
           ;; being applied where.
           (cl-mapcar #'cons
                      (number-sequence 0 (length colors))
                      colors))
  (if (zerop (theme-magic--call-pywal-process colors))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors. See buffer \"*pywal*\" for details")))


(defun theme-magic--get-ansi-color (ansi-index)
  "Get the ansi color at `ANSI-INDEX', as a hex string.

Note that this refers to the *in-built, Emacs ANSI colors* - not
the set of 16 generated by `theme-magic--16-colors-from-ansi'.
Thus, it only works with *indexes 0-7* (inclusive)."
  (theme-magic--color-name-to-hex
   (aref ansi-color-names-vector ansi-index)))


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
  "Get the best colors to use for a particular `ANSI-INDEX'.

Colors are evaluated at runtime within this method. Each color
should be a form that can be evaluated wth `eval'. If an error
occurs while evaluating the form, that color will be skipped.

Preferred colors are stored in
`theme-magic--preferred-extracted-colors'. This is an alist
mapping ANSI color indexes to a list of color forms, ranked best
to worst. See `theme-magic--preferred-extracted-colors' for more
details."
  (mapcar (lambda (color-form)
            (theme-magic--color-name-to-hex
             (theme-magic--safe-eval color-form)))
          (alist-get ansi-index theme-magic--preferred-extracted-colors)))


(defun theme-magic--color-taken (color existing-colors)
  "Check if a particular `COLOR' has already been taken in `EXISTING-COLORS'.

This method checks color similarity. If `COLOR' is too similar to
another color that's already been assigned, we count it as taken.
This ensures each ANSI color generated is fairly different from
every other color.

There are two main reasons to supress similar color assignments:

  1. Terminal colors are primarily used to highlight and
     segregate information. It's important to ensure the colors
     stay visually distinct, so the user can clearly tell each
     color apart at a glance.

  2. Some themes use many subtle variations of one color (e.g.
     `doom-one' uses many shades of deep purple). When processed,
     the color palette can end up being mainly different variants
     of that color. Back to our example: `doom-one' is not a
     purple theme, but without correcting for this tendency,
     the theme produced by `theme-magic' will look very purple.

     Suppressing similar colors prevents many similar colors from
     accruing in the result, which makes it harder for this kind
     of color shift to happen.

Note that these results were determined via trial and error. In
practice, banning similar colors simply produces better looking
results, in general."
  (catch 'color-taken
    (mapc (lambda (existing-color)
            ;; `existing-color' will be a cons cell, because it comes from an
            ;; alist. Take the `cdr' - this is the color string.
            (when (theme-magic--colors-match (cdr existing-color) color)
              (throw 'color-taken t)))
          existing-colors)
    nil))


(defun theme-magic--extract-color (ansi-index existing-colors)
  "Extract a preferred color from the current theme for `ANSI-INDEX'.

`EXISTING-COLORS' should contain the colors that have already
been assigned. It should be an alist mapping ANSI indexes to
their assigned hexadecimal colors, e.g:

    '((0 . \"#FFFFFF\")
      (1 . \"#FF0000\"))

Returns the best valid color, given `EXISTING-COLORS'.

If none of the preferred colors are valid, returns nil."
  (let ((possible-colors (theme-magic--get-preferred-colors ansi-index)))
    ;; Check each color in turn to see if it's a new color. If it is, stop
    ;; immediately and return it.
    (catch 'new-color
      (mapc (lambda (possible-color)
              (when (and possible-color
                         (not (theme-magic--color-taken possible-color existing-colors)))
                (throw 'new-color possible-color)))
            possible-colors)
      ;; If no color could be extracted, return nil for now.
      nil)))


(defun theme-magic--extract-fallback-color (ansi-index existing-colors)
  "Extract a color for `ANSI-INDEX' from the set of fallback colors.

`theme-magic--fallback-extracted-colors' is the list of fallback
colors. See that variable for more information.

This method returns the first fallback color that can be used,
given `EXISTING-COLORS'. A color can be used if it is
sufficiently different from all the existing colors.

Returns nil if no valid color could be found."
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
  "Extract a color for `ANSI-INDEX', with no concern for the overall theme.

This is a fallback method that should be used when no valid color
could be found. It will provide the best possible color for a
particular index, *even if* it clashes with another color."
  (theme-magic--color-name-to-hex
   (or (theme-magic--safe-eval (car (alist-get ansi-index theme-magic--preferred-extracted-colors)))
       ;; It's possible even the above will return nil, because the preferred
       ;; color form fails to evaluate. As a final fallback, just use the ANSI
       ;; color.
       ;;
       ;; TODO: The ansi colors should have already been in the fallback colors.
       ;; Is it worth duplicating them here?
       (theme-magic--get-ansi-color ansi-index)
       ;; Final failsafe - should never get here, but just in case, a neutral
       ;; color.
       "#888888")))


(defun theme-magic--auto-extract-16-colors ()
  "Automatically extract a set of 16 ANSI colors from the current theme.

The way this method works is it takes each ANSI color slot and
tries to extract a color from the current theme, assigning it to
that slot. Most of these colors are extracted from the currently
assigned fonts.

For example, one of the more prominent \"colors\" for the current
theme is embedded in the font used for keywords. We can extract
it as so:

    (face-foreground 'font-lock-keyword-face) -> \"#4f97d7\"

This color can then be assigned to one of the ANSI slots.

Certain colors are preferred for certain slots. For example:

  1. The ANSI color at index \"1\" is \"red\". Many terminal
     applications use this color to denote errors, so we attempt
     to extract ANSI color 1 from the theme's `error' face. If
     that doesn't work, we try the `warning' face. If that
     doesn't work, we fall back to the other colors.The point is
     to ensure `red' looks like an error.

  2. The first ANSI color is \"black\" and denotes the background
     for most terminal applications. We want this color to match
     the background color of the current theme, so we prefer
     that.

We repeat this process for each of the first 8 ANSI colors (plus
color 8, the off-background face[1], so 9 total), until all
colors have been assigned. Note that we cross-reference against
slots that have already been assigned, to ensure each color is
sufficiently different. No two ANSI colors should be the same, or
too similar[2].

After this is done, the last *7* colors are filled in. These are
the \"light variant\" colors[1]. These are simply duplicated from
their non-light counterparts (this is the same method used by
vanilla Pywal). For example, \"red-light\" (color 9) becomes the
same color as \"red\" (color 1). \"White-light\" (color 15)
becomes the same as \"white\" (color 7).

---

Footnotes:

  [1]: Ansi color 8 is special. It is \"black-bright\" - i.e,
       grey. In practice, this means it is used for faded text -
       it's the color used to denote unimportant information or
       to prevent text from standing out. The Emacs corollary is
       the `shadow' face.

       Many syntax highlighters denote code comments with this
       color.

       Note that this means we cannot have \"black-bright\"
       inherit from \"black\" - it has to be extracted
       separately.

  [2]: All ANSI colors should be somewhat different because their
       purpose is to denote different types of information. They
       need to be differentiable at a glance.

       HOWEVER, some themes may not actually have enough distinct
       colors to construct an entire set. In these cases, this
       method will use a fallback and duplicates may be produced.
       In practice, this is very rare."
  ;; Note that color extraction is worst-case speed complexity o(n*16), where
  ;; `n' is (roughly) the number of color options (preferred and fallback). This
  ;; scales faster than O(n) but it should still be negligible.
  ;;
  ;; If the number of colors were to grow above 16, this complexity would
  ;; increase. If that became an issue, it is possible to rewrite this algorithm
  ;; to reduce that complexity, by maintaining a record of unused colors and
  ;; pruning it as we progress. Right now, that's not worth it.
  (let (
        ;; `extracted-colors' is an alist mapping ANSI numbers to colors.
        (extracted-colors '())
        )
    ;; Go through the colors in the preferred order, and attempt to extract a
    ;; color for each.
    (mapc (lambda (ansi-index)
            (push (cons ansi-index
                        (or (theme-magic--extract-color ansi-index extracted-colors)
                            ;; Try and find an unused color in the fallback colors.
                            (theme-magic--extract-fallback-color ansi-index extracted-colors)
                            ;; If we couldn't find a unique color, fall back to
                            ;; the best duplicate color.
                            (theme-magic--force-extract-color ansi-index)))
                  extracted-colors))
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
  "Apply the current Emacs theme to the rest of Linux.

This method uses Pywal to set the theme. Ensure you have Pywal
installed and that its executable, `wal', is available.

See Pywal's documentation for more information:

  https://github.com/dylanaraps/pywal

Pywal is designed to be unobtrusive, so it only sets your theme
for the current session. You have to explicitly tell Pywal to
reload its theme on a fresh login, by calling \"wal -R\". To do
this automatically, place the line \"wal -R\" in your
\"~/.xprofile\" file (or whichever file starts programs on a
graphical login).

See `theme-magic--auto-extract-16-colors' to understand how this
method chooses colors for the Linux theme."
  (interactive)
  ;; This will actually check dependencies twice, but that's fine - it's cheap
  ;; and we want to do it up front.
  (theme-magic--check-dependencies)
  (theme-magic--apply-colors-with-pywal
   (theme-magic--auto-extract-16-colors)))


(defun theme-magic-from-emacs--wrapper (&rest _)
  "Wrapper for `theme-magic-from-emacs' to be used as advice.

Using the normal, autoloaded and interactive method can cause
strange problems with the advice system. It will also fail if
arguments are passed to the advised function. This is a wrapper
method that can be used safely."
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

Note for end users: DO NOT use this method directly. Use the
minor mode function, `theme-magic-export-theme-mode', instead.

Once enabled, the Linux theme will be updated whenever the Emacs
theme is changed.

Note that if an Emacs theme has already been set, it will not be
exported - you must do that manually or change the theme again."
  (mapc (lambda (func)
          (advice-add func :after 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


(defun theme-magic--disable-auto-update ()
  "Disable automatic Linux theme updating.

Note for end users: DO NOT use this method directly. Use the
minor mode function, `theme-magic-export-theme-mode', instead.

Once disabled, the Linux theme will need to be updated manually
with `theme-magic-from-emacs'."
  (mapc (lambda (func)
          (advice-remove func 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


(provide 'theme-magic)
;;; theme-magic.el ends here

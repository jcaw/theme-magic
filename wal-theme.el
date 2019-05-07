;;; wal-theme.el --- Automatically generate a theme from Pywal

;; Copyright (C) 2015-2018 Nasser Alshammari
;; Copyright (C) 2019 GitHub user "jcaw"

;; Author: GitHub user "jcaw"
;; Original Spacemacs Theme Author: Nasser Alshammari
;; URL: <https://github.com/jcaw/theme-magic>
;;
;; Version: 0.0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "25"))

;; Original Spacemacs themes initially created with the help of
;; emacs-theme-generator, <https://github.com/mswift42/theme-creator>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This is a color theme that takes 16 Pywal colors as input, and generates
;; its look automatically.

;;; Code:

(require 'color)


(defgroup wal-theme nil
  "Wal-theme options."
  :group 'faces)

;; TODO Variable to control whether comments are faded or colored

(defcustom wal-theme-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-comment-italic nil
  "Enable italics for comments.

This setting will also disable the background."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-comment-colored t
  "Should comments be colored?"
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-agenda-height nil
  "If non-nil, use varying text heights for agenda items.

Note that if you change this to a non-nil value, you may want to
also adjust the value of `org-agenda-tags-column'. If that is set
to 'auto, tags may not be properly aligned. "
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-bold t
  "Inherit text bold for org headings"
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view"
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-underline-parens t
  "If non-nil, underline matching parens when using `show-paren-mode' or similar."
  :type 'boolean
  :group 'wal-theme)


;; (defmacro wal-dyn-let (varlist fn setfaces setvars)
;;   ;; TODO: Document
;;   (list 'let* (append varlist (funcall fn)) setfaces setvars))


(defun wal--true-color-p ()
  "Can the current Emacs session display true colors?"
  ;; TODO: Document
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))


;; (defun wal--custom-colors-override ()
;;   ;; TODO: Document
;;   (mapcar (lambda (x) (list (car x) (cdr x)))
;;           wal-theme-custom-colors))


(defun wal--to-256 (color)
  "Convert a color to 256-color palette."
  ;; TODO: Require tty?
  (tty-color-translate color))


(defun wal--weighted-mean (float1 float2 alpha)
  "Calculate a weighted mean of two numbers. `ALPHA' is the weighting.

`ALPHA' should be a float in the range 0.0-1.0. Specify 0.0 to
weight `FLOAT1' fully. Specify 1.0 to weight `FLOAT2' fully.
Specify 0.5 to weight both equally."
  (unless (<= 0.0 alpha 1.0)
    (error "`ALPHA' must be between 0.0 and 1.0, inclusive."))
  (let ((float1-weighting (- 1.0 alpha))
        (float2-weighting alpha))
    (+ (* float1-weighting float1)
       (* float2-weighting float2))))


(defun wal-blend-hsl (color1 color2 hue-alpha saturation-alpha value-alpha)
  "Blend two colors together, specifying the HSL weightings.

Each weighting is a value from 0.0 to 1.0. This value specifies
the weight to give to `COLOR1' vs `COLOR2'. 0.0 weights `COLOR1'
fully. 1.0 weights `COLOR2' fully. 0.5 weights both equally.

The `hue-alpha', `saturation-alpha' and `LIGHTNESS' components should each be
given their own weighting.

`COLOR1' and `COLOR2' should be color names (e.g. \"white\") or
RGB triplet strings (e.g. \"#ff12ec\")."
  ;; Note that it may be preferable to use HSV here since the color cylinder is
  ;; closer to a diamond, whereas HSL is a cone, so light colors end up more
  ;; vibrant on HSL. However, Emacs' color library has limited HSV functionality
  ;; - we need certain features only available to HSL.
  (let ((color1-hsl (wal-name-to-hsl color1))
        (color2-hsl (wal-name-to-hsl color2)))
    (wal-hsl-to-hex
     (list (wal--weighted-mean (nth 0 color1-hsl) (nth 0 color2-hsl) hue-alpha)
           (wal--weighted-mean (nth 1 color1-hsl) (nth 1 color2-hsl) hue-alpha)
           (wal--weighted-mean (nth 2 color1-hsl) (nth 2 color2-hsl) hue-alpha)))))


(defun wal-blend (color1 color2 alpha)
  "Blend two colors together, with some `ALPHA'.

`ALPHA' is a weighting, a value from 0.0 to 1.0. It specifies the
weight to give to `COLOR1' vs `COLOR2'. 0.0 weights `COLOR1'
fully. 1.0 weights `COLOR2' fully. 0.5 weights both equally.

`COLOR1' and `COLOR2' should be color names (e.g. \"white\") or
RGB triplet strings (e.g. \"#ff12ec\")."
  (apply 'color-rgb-to-hex
         (-zip-with '(lambda (color1-value color2-value)
                       (wal--weighted-mean color1-value color2-value alpha))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))


(defun wal-override-hsl (color new-hue new-saturation new-lightness)
  "Override the HSL values of a color.

`NEW-HUE', `NEW-SATURATION' and `NEW-LIGHTNESS' represent the new
values.

If you want to keep the original value, set the parameter to
nil."
  (let* ((color-hsl (wal-name-to-hsl color))
         (final-hue (if (eq new-hue nil)
                        (nth 0 color-hsl)
                      new-hue))
         (final-saturation (if (eq new-saturation nil)
                               (nth 1 color-hsl)
                             new-saturation))
         (final-lightness (if (eq new-lightness nil)
                              (nth 2 color-hsl)
                            new-lightness)))
    (wal-hsl-to-hex
     (list final-hue final-saturation final-lightness))))


(defun wal--mean (&rest values)
  "Compute the mean of a set of values"
  (/ (float (apply '+ values))
     (length values)))


(defun wal--average-saturation (&rest colors)
  "Get the average HSL saturation of a list of numbers."
  (apply
   'wal--mean
   (mapcar (lambda (color)
             (nth 1
                  ;; TODO: Extract to dedicated "name-to-hsl" method
                  (wal-name-to-hsl color)))
           colors)))


(defun wal--average-lightness (&rest colors)
  "Get the average HSL lightness of a list of numbers."
  (apply
   'wal--mean
   (mapcar (lambda (color)
             (nth 2
                  ;; TODO: Extract to dedicated "name-to-hsl" method
                  (wal-name-to-hsl color)))
           colors)))


;; (defun wal-filter-tty (color)
;;   "Return the appropriate color, given the environment.

;; Provide `LIGHT-COLOR' iff it's different to the dark color."
;;   (if (wal--true-color-p)
;;       color
;;     (wal--to-256 color)))


;; TODO: Rename from tuple?
(defun wal-name-to-hsl (color)
  "Convert a `COLOR' name to a HSL tuple."
  (apply 'color-rgb-to-hsl
         (color-name-to-rgb color)))


(defun wal-hsl-to-hex (hsl-tuple)
  "Convert an HSL tuple to a hex string."
  (apply 'color-rgb-to-hex
         (append (apply 'color-hsl-to-rgb
                        hsl-tuple)
                 ;; We have to manually specify 2-digits-per-color.
                 '(2))))


(defun wal--is-dark-theme (background-color foreground-color)
  "Is this a dark theme, based on the background color?"
  (let* ((background-hsl (wal-name-to-hsl background-color))
         (foreground-hsl (wal-name-to-hsl foreground-color))
         (background-lightness (nth 2 background-hsl))
         (foreground-lightness (nth 2 foreground-hsl)))
    (or
     ;; TODO: Do we need the double check here?
     ;; If the background is sufficiently dark, it is a dark theme.
     (< background-lightness 0.4)
     ;; If the background is sufficiently light, it is a light theme.
     (not (> background-lightness 0.6))
     ;; This is a more complex check that should rarely be used. It's a fallback
     ;; for ambiguous colors. If the text is lighter than the background, it's
     ;; probably a dark theme.
     (< background-lightness foreground-lightness))))


(defun wal-create-theme (wal-black   wal-red   wal-green   wal-yellow   wal-blue   wal-magenta   wal-cyan   wal-white
                                     wal-black-l wal-red-l wal-green-l wal-yellow-l wal-blue-l wal-magenta-l wal-cyan-l wal-white-l)
  (let*
      (
       ;; First, determine whether this is a dark or light theme. This will
       ;; determine how specific colors are derived.
       (dark-theme (wal--is-dark-theme wal-black wal-white))
       (average-saturation
        (wal--average-saturation wal-red wal-green wal-yellow wal-blue wal-magenta wal-cyan))
       (average-lightness
        (wal--average-lightness wal-red wal-green wal-yellow wal-blue wal-magenta wal-cyan))

       ;; TODO: What is `class' for?
       (class '((class color) (min-colors 89)))
       ;; True colors
       ;;
       ;; These colors are used when a genuine hue is needed. For example: though
       ;; the second color provided by wal is _named_ green, it may not actually
       ;; have a green hue. `true-green' can be used to impart a genuine green
       ;; hue, when needed.
       (true-black    "#000000")
       (true-red      "#FF0000")
       (true-green    "#00FF00")
       (true-yellow   "#FFFF00")
       (true-blue     "#0000FF")
       (true-magenta  "#FF00FF")
       (true-cyan     "#00FFFF")
       (true-white    "#FFFFFF")


       ;; Primary colors
       ;;
       ;; TODO: Explain primary colors
       ;; TODO: Pick better names for primary colors?
       ;; TODO: Soften the background color blends. They're way too intense as-is.
       (background    wal-black)
       (foreground    wal-white)
       (aqua          wal-magenta)
       (aqua-bg       (wal-blend background aqua 0.05))
       ;; TODO: rename, maybe to "tertiary"?
       (green         wal-green)
       (green-bg      (wal-blend background green 0.05))
       (green-bg-s    (wal-blend background green 0.1))
       ;; TODO: Rename to "key"
       (blue          wal-blue)
       (blue-bg       (wal-blend background blue 0.05))
       (blue-bg-s     (wal-blend background blue 0.1))
       ;; TODO: Rename to "warn"? No. Yellow is good.
       (yellow        wal-yellow)
       ;; TODO: Should yellow have a different BG gradient to the otehrs?
       (yellow-bg     (wal-blend background yellow 0.1))
       (red           wal-red)
       (red-bg        (wal-blend background red 0.05))
       (red-bg-s      (wal-blend background red 0.1))
       (magenta       wal-cyan)
       (cyan          wal-magenta)


       ;; Useful names
       (text          foreground)
       (shadow        wal-black-l)
       ;; TODO: what was accent-pale used for?
       ;; (accent-pale   "#7590db" "#715ab1")
       ;; (dominant      "#bc6ec5" "#6c3163")
       (dominant      wal-cyan)
       (secondary        blue)
       ;; (dominant-fade (if dark-theme
       ;;                    ;; If it's a dark theme, we darken the dominant color
       ;;                    ;; to create the faded version.
       ;;                    (color-darken-name
       ;;                     (color-desaturate-name dominant 10)
       ;;                     30)
       ;;                  ;; If it's a light theme, lighten the dominant color to
       ;;                  ;; create the faded version.
       ;;                  (color-lighten-name
       ;;                   (color-desaturate-name dominant 10)
       ;;                   10)))
       (dominant-fade (wal-blend dominant background 0.5))
       ;; (dominant-fade (wal-blend dominant background 0.5))

       (comment       (if wal-theme-comment-colored
                          ;; Comments should be desaturated even when colored.
                          (if dark-theme
                              (color-desaturate-name aqua 40)
                            (color-darken-name
                             (color-desaturate-name aqua 10)
                             10))
                        shadow))


       ;; Actual colors
       ;;
       ;; Some colors need to have a certain hue, regardless of the overall
       ;; theme. For example, it's better if diffs are always red and green.
       ;; These colors have a static hue, but otherwise match the feel of the
       ;; supplied color palette.
       (actual-red        (wal-override-hsl true-red nil average-saturation average-lightness))
       (actual-red-bg     (wal-blend background actual-red 0.05))
       (actual-red-bg-s   (wal-blend background actual-red 0.1))
       (actual-green      (wal-override-hsl true-green nil average-saturation average-lightness))
       (actual-green-bg   (wal-blend background actual-green 0.05))
       (actual-green-bg-s (wal-blend background actual-green 0.1))
       (actual-blue      (wal-override-hsl true-blue nil average-saturation average-lightness))
       (actual-blue-bg   (wal-blend background actual-blue 0.05))
       (actual-blue-bg-s (wal-blend background actual-blue 0.1))
       (actual-yellow      (wal-override-hsl true-yellow nil average-saturation average-lightness))
       (actual-yellow-bg   (wal-blend background actual-yellow 0.05))
       (actual-yellow-bg-s (wal-blend background actual-yellow 0.1))
       (actual-cyan      (wal-override-hsl true-cyan nil average-saturation average-lightness))
       (actual-cyan-bg   (wal-blend background actual-cyan 0.05))
       (actual-cyan-bg-s (wal-blend background actual-cyan 0.1))


       ;; (act1          (if (eq variant 'dark) "#222226" "#e7e5eb"))
       (act1          (if dark-theme
                          (wal-blend background true-black 0.2)
                        (wal-blend background dominant 0.1)))
       ;; (act2          (if (eq variant 'dark) "#5d4d7a" "#d3d3e7"))
       (act2          dominant-fade)
       ;; (base          (if (eq variant 'dark) "#b2b2b2" "#655370"))
       (base          text)
       ;; (base-dim      (if (eq variant 'dark) "#686868" "#a094a2"))
       (base-dim      shadow)
       ;; (bg1           (if (eq variant 'dark) "#292b2e" "#fbf8ef"))
       (bg1           background)
       ;; The alt backgrounds differ
       ;; (bg2           (if (eq variant 'dark) "#212026" "#efeae9"))
       (bg2           (if dark-theme
                          ;; (wal-blend background true-black 0.15)
                          (color-lighten-name background 3)
                        (wal-blend background dominant 0.075)))
       ;; (bg3           (if (eq variant 'dark) "#100a14" "#e3dedd"))
       (bg3           (if dark-theme
                          ;; (wal-blend background true-black 0.30)
                          ;; (wal-blend background dominant 0.085)
                          (color-lighten-name background 6)
                        (wal-blend background dominant 0.115)))
       ;; (bg4           (if (eq variant 'dark) "#0a0814" "#d2ceda"))
       (bg4           (if dark-theme
                          ;; (wal-blend background true-black 0.45)
                          (color-lighten-name background 9)
                        (wal-blend background dominant 0.15)))
       ;; TODO: Should we differ between light and dark, like the default?
       ;; (border        (if (eq variant 'dark) act2 "#b3b9be"))
       (border        act2)
       ;; (cblk          (if (eq variant 'dark) "#cbc1d5" base))
       (cblk          (wal-blend dominant base 0.9))
       ;; (cblk-bg       (if (eq variant 'dark) "#2f2b33" "#e8e3f0"))
       (cblk-bg       (wal-blend dominant background 0.2))
       ;; (cblk-ln       (if (eq variant 'dark) "#827591" "#9380b2"))
       (cblk-ln       (wal-blend dominant text 0.5))
       ;; (cblk-ln-bg    (if (eq variant 'dark) "#373040" "#ddd8eb"))
       (cblk-ln-bg    (wal-blend dominant background 0.05))
       ;; (cursor        (if (eq variant 'dark) "#e3dedd" "#100a14"))
       (cursor        dominant)
       ;; (const         (if (eq variant 'dark) "#a45bad" "#4e3163"))
       ;; TODO: What colors are unused? Should we use that?
       ;; (const         (wal-blend dominant ))
       (const         (wal-blend dominant background 0.2))
       ;; (comment       (if (eq variant 'dark) "#2aa1ae" "#2aa1ae"))
       (comment-light (if dark-theme comment (wal-blend dominant shadow 0.5)))
       ;; (comment-bg    (if (eq variant 'dark) "#292e34" "#ecf3ec"))
       (comment-bg    (wal-blend background comment 0.05))
       ;; (comp          (if (eq variant 'dark) "#c56ec3" "#6c4173"))
       (comp          (wal-blend dominant background 0.05))
       ;; (err           (if (eq variant 'dark) "#e0211d" "#e0211d"))
       (err           actual-red)
       ;; (dominant      "#bc6ec5" "#6c3163")
       (func          dominant)
       (head1         blue)
       (head1-bg      blue-bg)
       (head2         aqua)
       (head2-bg      aqua-bg)
       (head3         green)
       (head3-bg      green-bg)
       (head4         yellow)
       (head4-bg      yellow-bg)
       ;; (highlight     (if (eq variant 'dark) "#444155" act2))
       (highlight     (wal-blend-hsl dominant background 0.6 0.7 0.4))
       ;; (highlight-dim (if (eq variant 'dark) "#3b314d" "#e7e7fc"))
       (highlight-dim (wal-blend-hsl dominant background  0.4 0.1 0.6))
       (keyword       secondary)
       ;; (lnum          (if (eq variant 'dark) "#44505c" "#a8a8bf"))
       (lnum          (wal-blend shadow aqua 0.2))
       ;; (mat           (if (eq variant 'dark) "#86dc2f" "#ba2f59"))
       ;; TODO: How to do mat?
       ;; I think mat == match.
       ;; TODO: Mat is weird, make sure it works.
       (mat           (wal-blend dominant green 0.8))
       ;; (meta          (if (eq variant 'dark) "#9f8766" "#da8b55"))
       ;; TODO: These are docstrings                     . We don't use yellow, so use yellow?
       (meta          yellow)
       ;; TODO: Aqua for string?
       (str           green)
       ;; (suc           (if (eq variant 'dark) mat "#42ae2c"))
       (suc           actual-green)
       ;; (ttip          (if (eq variant 'dark) "#9a9aba" "#8c799f"))
       (ttip          (wal-blend dominant-fade text       0.7))
       ;; (ttip-sl       (if (eq variant 'dark) "#5e5079" "#c8c6dd"))
       (ttip-sl       (wal-blend dominant-fade background 0.1))
       ;; (ttip-bg       (if (eq variant 'dark) "#34323e" "#e2e0ea"))
       (ttip-bg       (wal-blend dominant-fade background 0.9))
       ;; (type          (if (eq variant 'dark) "#ce537a" mat))
       (type          red)
       (var           magenta)
       ;; Warning
       (war           (wal-blend yellow actual-red 0.5))


       ;; (progn ;; RAW
       ;;   ;; colors
       ;;   (aqua          (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
       ;;   (aqua-bg       (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
       ;;   (green         (if (eq variant 'dark) (if (true-color-p) "#67b11d" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
       ;;   (green-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
       ;;   (green-bg-s    (if (eq variant 'dark) (if (true-color-p) "#29422d" "#262626") (if (true-color-p) "#dae6d0" "#ffffff")))
       ;;   (cyan          (if (eq variant 'dark) (if (true-color-p) "#28def0" "#00ffff") (if (true-color-p) "#21b8c7" "#008080")))
       ;;   (red           (if (eq variant 'dark) (if (true-color-p) "#f2241f" "#d70000") (if (true-color-p) "#f2241f" "#d70008")))
       ;;   (red-bg        (if (eq variant 'dark) (if (true-color-p) "#3c2a2c" "#262626") (if (true-color-p) "#faede4" "#ffffff")))
       ;;   (red-bg-s      (if (eq variant 'dark) (if (true-color-p) "#512e31" "#262626") (if (true-color-p) "#eed9d2" "#ffffff")))
       ;;   (blue          (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
       ;;   (blue-bg       (if (eq variant 'dark) (if (true-color-p) "#293239" "#262626") (if (true-color-p) "#edf1ed" "#d7d7ff")))
       ;;   (blue-bg-s     (if (eq variant 'dark) (if (true-color-p) "#2d4252" "#262626") (if (true-color-p) "#d1dcdf" "#d7d7ff")))
       ;;   (magenta       (if (eq variant 'dark) (if (true-color-p) "#a31db1" "#af00df") (if (true-color-p) "#a31db1" "#800080")))
       ;;   (yellow        (if (eq variant 'dark) (if (true-color-p) "#b1951d" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
       ;;   (yellow-bg     (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))

       ;;   (act1          (if (eq variant 'dark) (if (true-color-p) "#222226" "#121212") (if (true-color-p) "#e7e5eb" "#d7dfff")))
       ;;   (act2          (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#444444") (if (true-color-p) "#d3d3e7" "#afafd7")))
       ;;   (base          (if (eq variant 'dark) (if (true-color-p) "#b2b2b2" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
       ;;   (base-dim      (if (eq variant 'dark) (if (true-color-p) "#686868" "#585858") (if (true-color-p) "#a094a2" "#afafd7")))
       ;;   (bg1           (if (eq variant 'dark) (if (true-color-p) "#292b2e" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
       ;;   (bg2           (if (eq variant 'dark) (if (true-color-p) "#212026" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
       ;;   (bg3           (if (eq variant 'dark) (if (true-color-p) "#100a14" "#121212") (if (true-color-p) "#e3dedd" "#d0d0d0")))
       ;;   (bg4           (if (eq variant 'dark) (if (true-color-p) "#0a0814" "#080808") (if (true-color-p) "#d2ceda" "#bcbcbc")))
       ;;   (border        (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#111111") (if (true-color-p) "#b3b9be" "#b3b9be")))
       ;;   (cblk          (if (eq variant 'dark) (if (true-color-p) "#cbc1d5" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
       ;;   (cblk-bg       (if (eq variant 'dark) (if (true-color-p) "#2f2b33" "#262626") (if (true-color-p) "#e8e3f0" "#ffffff")))
       ;;   (cblk-ln       (if (eq variant 'dark) (if (true-color-p) "#827591" "#af5faf") (if (true-color-p) "#9380b2" "#af5fdf")))
       ;;   (cblk-ln-bg    (if (eq variant 'dark) (if (true-color-p) "#373040" "#333333") (if (true-color-p) "#ddd8eb" "#dfdfff")))
       ;;   (cursor        (if (eq variant 'dark) (if (true-color-p) "#e3dedd" "#d0d0d0") (if (true-color-p) "#100a14" "#121212")))
       ;;   (const         (if (eq variant 'dark) (if (true-color-p) "#a45bad" "#d75fd7") (if (true-color-p) "#4e3163" "#8700af")))
       ;;   (comment       (if (eq variant 'dark) (if (true-color-p) "#2aa1ae" "#008787") (if (true-color-p) "#2aa1ae" "#008787")))
       ;;   (comment-light (if (eq variant 'dark) (if (true-color-p) "#2aa1ae" "#008787") (if (true-color-p) "#a49da5" "#008787")))
       ;;   (comment-bg    (if (eq variant 'dark) (if (true-color-p) "#292e34" "#262626") (if (true-color-p) "#ecf3ec" "#ffffff")))
       ;;   (comp          (if (eq variant 'dark) (if (true-color-p) "#c56ec3" "#d75fd7") (if (true-color-p) "#6c4173" "#8700af")))
       ;;   (err           (if (eq variant 'dark) (if (true-color-p) "#e0211d" "#e0211d") (if (true-color-p) "#e0211d" "#e0211d")))
       ;;   (func          (if (eq variant 'dark) (if (true-color-p) "#bc6ec5" "#d75fd7") (if (true-color-p) "#6c3163" "#8700af")))
       ;;   (head1         (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
       ;;   (head1-bg      (if (eq variant 'dark) (if (true-color-p) "#293239" "#262626") (if (true-color-p) "#edf1ed" "#ffffff")))
       ;;   (head2         (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
       ;;   (head2-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
       ;;   (head3         (if (eq variant 'dark) (if (true-color-p) "#67b11d" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
       ;;   (head3-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
       ;;   (head4         (if (eq variant 'dark) (if (true-color-p) "#b1951d" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
       ;;   (head4-bg      (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
       ;;   (highlight     (if (eq variant 'dark) (if (true-color-p) "#444155" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
       ;;   (highlight-dim (if (eq variant 'dark) (if (true-color-p) "#3b314d" "#444444") (if (true-color-p) "#e7e7fc" "#d7d7ff")))
       ;;   (keyword       (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
       ;;   (lnum          (if (eq variant 'dark) (if (true-color-p) "#44505c" "#444444") (if (true-color-p) "#a8a8bf" "#af87af")))
       ;;   (mat           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#ba2f59" "#af005f")))
       ;;   (meta          (if (eq variant 'dark) (if (true-color-p) "#9f8766" "#af875f") (if (true-color-p) "#da8b55" "#df5f5f")))
       ;;   (str           (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
       ;;   (suc           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#42ae2c" "#00af00")))
       ;;   (ttip          (if (eq variant 'dark) (if (true-color-p) "#9a9aba" "#888888") (if (true-color-p) "#8c799f" "#5f5f87")))
       ;;   (ttip-sl       (if (eq variant 'dark) (if (true-color-p) "#5e5079" "#333333") (if (true-color-p) "#c8c6dd" "#afafff")))
       ;;   (ttip-bg       (if (eq variant 'dark) (if (true-color-p) "#34323e" "#444444") (if (true-color-p) "#e2e0ea" "#dfdfff")))
       ;;   (type          (if (eq variant 'dark) (if (true-color-p) "#ce537a" "#df005f") (if (true-color-p) "#ba2f59" "#af005f")))
       ;;   (var           (if (eq variant 'dark) (if (true-color-p) "#7590db" "#8787d7") (if (true-color-p) "#715ab1" "#af5fd7")))
       ;;   (war           (if (eq variant 'dark) (if (true-color-p) "#dc752f" "#dc752f") (if (true-color-p) "#dc752f" "#dc752f")))
       ;;   )

       ) ;; End of let* varlist

    ;; TODO: Filter colors here for TTY
    ;; wal--custom-colors-override

    (custom-theme-set-faces
     'wal

;;;;; basics
     `(cursor ((,class (:background ,cursor))))
     `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
     `(default ((,class (:background ,bg1 :foreground ,base))))
     `(default-italic ((,class (:italic t))))
     `(error ((,class (:foreground ,err))))
     `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
     `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
     `(font-lock-builtin-face ((,class (:foreground ,keyword))))
     `(font-lock-comment-face ((,class (:foreground ,(if wal-theme-comment-italic comment-light comment) :background ,(when wal-theme-comment-bg comment-bg) :slant ,(if wal-theme-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-doc-face ((,class (:foreground ,meta))))
     `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
     `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword :slant ,(if wal-theme-keyword-italic 'italic 'normal)))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face ((,class (:foreground ,func))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     `(fringe ((,class (:background ,bg1 :foreground ,base))))
     `(header-line ((,class :background ,bg4)))
     `(highlight ((,class (:foreground ,base :background ,highlight))))
     `(hl-line ((,class (:background ,bg2))))
     `(isearch ((,class (:foreground ,bg1 :background ,mat))))
     `(lazy-highlight ((,class (:background ,green-bg-s :weight normal))))
     `(link ((,class (:foreground ,comment :underline t))))
     `(link-visited ((,class (:foreground ,comp :underline t))))
     `(match ((,class (:background ,highlight :foreground ,mat))))
     `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
     `(page-break-lines ((,class (:foreground ,act2))))
     `(region ((,class (:background ,highlight))))
     `(secondary-selection ((,class (:background ,bg3))))
     `(shadow ((,class (:foreground ,base-dim))))
     `(success ((,class (:foreground ,suc))))
     `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
     `(vertical-border ((,class (:foreground ,border))))
     `(warning ((,class (:foreground ,war))))

;;;;; ace-window
     `(aw-leading-char-face ((,class (:foreground ,func :weight bold :height 2.0 :box (:line-width 1 :color ,keyword :style released-button)))))

;;;;; ahs
     `(ahs-face ((,class (:background ,highlight))))
     `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))

;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
     `(avy-lead-face   ((,class (:background ,green-bg :foreground ,green))))
     `(avy-lead-face-0 ((,class (:background ,green-bg :foreground ,yellow))))
     `(avy-lead-face-1 ((,class (:background ,green-bg :foreground ,magenta))))
     `(avy-lead-face-2 ((,class (:background ,green-bg :foreground ,blue))))

;;;;; calfw
     `(cfw:face-title               ((,class (:foreground ,head1 :height 2.0 :weight bold :inherit variable-pitch))))
     `(cfw:face-header              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-saturday            ((,class (:foreground ,base :weight bold))))
     `(cfw:face-sunday              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-holiday             ((,class (:foreground ,head1 :weight bold))))
     `(cfw:face-grid                ((,class (:foreground ,border))))
     `(cfw:face-default-content     ((,class (:foreground ,green))))
     `(cfw:face-periods             ((,class (:foreground ,cyan))))
     `(cfw:face-periods             ((,class (:foreground ,aqua))))
     `(cfw:face-day-title           ((,class (:background ,head1-bg))))
     `(cfw:face-default-day         ((,class (:foreground ,base :weight bold))))
     `(cfw:face-annotation          ((,class (:foreground ,aqua))))
     `(cfw:face-disable             ((,class (:foreground ,base-dim))))
     `(cfw:face-today-title         ((,class (:background ,blue :weight bold))))
     `(cfw:face-today               ((,class (:background ,head1-bg :weight bold))))
     `(cfw:face-select              ((,class (:background ,magenta :weight bold))))
     `(cfw:face-toolbar             ((,class (:foreground ,base :background ,bg1))))
     `(cfw:face-toolbar-button-off  ((,class (:foreground ,base :weight bold))))
     `(cfw:face-toolbar-button-on   ((,class (:foreground ,base :weight bold))))

;;;;; cider
     `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
     `(cider-enlightened-local ((,class (:foreground ,yellow))))
     `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
     `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
     `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
     `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
     `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
     `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
     `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
     `(company-preview-search ((,class (:inherit match))))
     `(company-scrollbar-bg ((,class (:background ,bg2))))
     `(company-scrollbar-fg ((,class (:background ,act2))))
     `(company-template-field ((,class (:inherit region))))
     `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-tooltip-annotation ((,class (:foreground ,type))))
     `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,keyword))))
     `(company-tooltip-common-selection ((,class (:foreground ,base))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-search ((,class (:inherit match))))
     `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; diff
     `(diff-added             ((,class :background nil :foreground ,actual-green)))
     `(diff-changed           ((,class :background nil :foreground ,actual-blue)))
     `(diff-header            ((,class :background ,cblk-ln-bg :foreground ,func)))
     `(diff-file-header       ((,class :background ,cblk-ln-bg :foreground ,cblk)))
     `(diff-indicator-added   ((,class :background nil :foreground ,actual-green)))
     `(diff-indicator-changed ((,class :background nil :foreground ,actual-blue)))
     `(diff-indicator-removed ((,class :background nil :foreground ,actual-red)))
     `(diff-refine-added      ((,class :background ,actual-green :foreground ,bg1)))
     `(diff-refine-changed    ((,class :background ,actual-blue :foreground ,bg1)))
     `(diff-refine-removed    ((,class :background ,actual-red :foreground ,bg1)))
     `(diff-removed           ((,class :background nil :foreground ,actual-red)))

;;;;; diff-hl
     `(diff-hl-change ((,class :background ,actual-blue-bg-s :foreground ,actual-blue)))
     `(diff-hl-delete ((,class :background ,actual-red-bg-s :foreground ,actual-red)))
     `(diff-hl-insert ((,class :background ,actual-green-bg-s :foreground ,actual-green)))

;;;;; dired
     `(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,comp :inherit bold))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,comp :inherit bold))))
     `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
     `(dired-perm-write ((,class (:foreground ,base :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(dired-warning ((,class (:foreground ,war))))

;;;;; ediff
     `(ediff-current-diff-A ((,class(:background ,actual-red-bg :foreground ,actual-red))))
     `(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua))))
     `(ediff-current-diff-B ((,class(:background ,actual-green-bg :foreground ,actual-green))))
     `(ediff-current-diff-C ((,class(:background ,actual-blue-bg :foreground ,actual-blue))))
     `(ediff-even-diff-A ((,class(:background ,bg3))))
     `(ediff-even-diff-Ancestor ((,class(:background ,bg3))))
     `(ediff-even-diff-B ((,class(:background ,bg3))))
     `(ediff-even-diff-C ((,class(:background ,bg3))))
     `(ediff-fine-diff-A ((,class(:background ,actual-red :foreground ,bg1))))
     `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
     `(ediff-fine-diff-B ((,class(:background ,actual-green :foreground ,bg1))))
     `(ediff-fine-diff-C ((,class(:background ,actual-blue :foreground ,bg1))))
     `(ediff-odd-diff-A ((,class(:background ,bg4))))
     `(ediff-odd-diff-Ancestor ((,class(:background ,bg4))))
     `(ediff-odd-diff-B ((,class(:background ,bg4))))
     `(ediff-odd-diff-C ((,class(:background ,bg4))))

;;;;; ein
     `(ein:cell-input-area((,class (:background ,bg2))))
     `(ein:cell-input-prompt ((,class (:foreground ,suc))))
     `(ein:cell-output-prompt ((,class (:foreground ,err))))
     `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
     `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face ((,class (:foreground ,head2))))
     `(elfeed-search-feed-face ((,class (:foreground ,blue))))
     `(elfeed-search-tag-face ((,class (:foreground ,func))))
     `(elfeed-search-title-face ((,class (:foreground ,var))))
     `(elfeed-search-unread-title-face ((,class (:foreground ,base))))

;;;;; enh-ruby
     `(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,base))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

;;;;; erc
     `(erc-input-face ((,class (:foreground ,func))))
     `(erc-my-nick-face ((,class (:foreground ,keyword))))
     `(erc-nick-default-face ((,class (:foreground ,keyword))))
     `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
     `(erc-notice-face ((,class (:foreground ,str))))
     `(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
     `(erc-timestamp-face ((,class (:foreground ,keyword))))

;;;;; eshell
     `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
     `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base))))
     `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; ESS
     `(ess-assignment-face ((,class (:foreground ,type :inherit bold))))
     `(ess-backquoted-face ((,class (:foreground ,var))))
     `(ess-constant-face ((,class (:inherit font-lock-constant-face))))
     `(ess-f-t-face ((,class (:inherit font-lock-constant-face))))
     `(ess-function-call-face ((,class (:foreground ,func))))
     `(ess-keyword-face ((,class (:inherit font-lock-keyword-face))))
     `(ess-matrix-face ((,class (:foreground ,base-dim))))
     `(ess-modifiers-face ((,class (:foreground ,keyword))))
     `(ess-numbers-face ((,class (:inherit font-lock-constant-face))))
     `(ess-operator-face ((,class (:foreground ,var))))
     `(ess-paren-face ((,class (:foreground ,blue))))
     `(ess-r-control-flow-keyword-face ((,class (:foreground ,keyword))))
     `(ess-r-signal-keyword-face ((,class (:foreground ,war))))

;;;;; evil
     ;; TODO: Replace with actual values?
     `(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
     `(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))

;;;;; evil-goggles
     ;; TODO: Replace yellow with actual-yellow here?
     `(evil-goggles--pulse-face ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-change-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-commentary-face ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-delete-face ((,class (:background ,actual-red-bg-s :foreground ,actual-red))))
     `(evil-goggles-fill-and-move-face ((,class (:background ,actual-green-bg-s :foreground ,actual-green))))
     `(evil-goggles-indent-face ((,class (:background ,actual-green-bg-s :foreground ,actual-green))))
     `(evil-goggles-join-face ((,class (:background ,actual-green-bg-s :foreground ,actual-green))))
     `(evil-goggles-nerd-commenter-face ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-paste-face ((,class (:background ,actual-green-bg-s :foreground ,actual-green))))
     `(evil-goggles-record-macro-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-replace-with-register-face ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-set-marker-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-shift-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-surround-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-yank-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-undo-actual-redo-add-face ((,class (:background ,actual-green-bg-s :foreground ,actual-green))))
     `(evil-goggles-undo-actual-redo-change-face ((,class (:background ,actual-blue-bg-s :foreground ,actual-blue))))
     `(evil-goggles-undo-actual-redo-remove-face ((,class (:background ,actual-red-bg-s :foreground ,actual-red))))

;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,err)))
        (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
     `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,keyword)))
        (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,war)))
        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flymake
     `(flymake-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,err)))
                      (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flymake-note ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color ,keyword)))
                     (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color ,war)))
                        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flyspell
     `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,war)))
                           (,class (:foreground ,base :background ,war :inherit bold :underline t))))
     `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,keyword)))
                           (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))

;;;;; jabber
     `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,class (:foreground ,base))))
     `(jabber-chat-text-local ((,class (:foreground ,base))))
     `(jabber-rare-time-face ((,class (:foreground ,green))))
     `(jabber-roster-user-away ((,class (:foreground ,yellow))))
     `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-dnd ((,class (:foreground ,red))))
     `(jabber-roster-user-error ((,class (:foreground ,err))))
     `(jabber-roster-user-offline ((,class (:foreground ,base))))
     `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-xa ((,class (:foreground ,aqua))))

;;;;; git-gutter-fr
     ;; TODO: Genuine green?
     `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,war :inherit bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,comp))))
     `(gnus-header-subject ((,class (:foreground ,func :inherit bold))))
     `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,base))))
     `(guide-key/key-face ((,class (:foreground ,keyword))))
     `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base))))
     `(helm-bookmark-gnus ((,class (:foreground ,comp))))
     `(helm-bookmark-info ((,class (:foreground ,comp))))
     `(helm-bookmark-man ((,class (:foreground ,comp))))
     `(helm-bookmark-w3m ((,class (:foreground ,comp))))
     `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
     `(helm-buffer-process ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
     `(helm-candidate-number ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
     `(helm-ff-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     ;; TODO: Symlink color was cyan. What should it be?
     ;; Go over all symlinks
     `(helm-ff-dotted-symlink-directory ((,class (:foreground ,actual-cyan :background ,bg1 :inherit bold))))
     `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
     `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,actual-red :background ,bg1 :inherit bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     ;; TODO: Symlink color was cyan. What should it be?
     `(helm-ff-symlink ((,class (:foreground ,actual-cyan :background ,bg1 :inherit bold))))
     `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,type :background ,bg1 :inherit bold))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
     `(helm-header-line-left-margin ((,class (:foreground ,keyword :background ,nil))))
     `(helm-match ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-match-item ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-moccur-buffer ((,class (:foreground ,var :background ,bg1))))
     `(helm-selection ((,class (:background ,highlight))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
     `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :inherit bold))))
     `(helm-time-zone-current ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
     `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg3))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-line-face ((,class (:background ,highlight))))
     `(helm-swoop-target-word-face ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
     ;; TODO: Do highlights need to be actual values?
     `(hi-green  ((,class (:foreground ,green :background ,green-bg))))
     `(hi-yellow ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,comment-bg))))

;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:background ,bg2))))

;;;;; hydra
     `(hydra-face-blue ((,class (:foreground ,blue))))
     `(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
     `(ido-first-match ((,class (:foreground ,comp :inherit bold))))
     `(ido-only-match ((,class (:foreground ,mat :inherit bold))))
     `(ido-subdir ((,class (:foreground ,keyword))))
     `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     `(info-header-xref ((,class (:foreground ,func :underline t))))
     `(info-menu ((,class (:foreground ,suc))))
     `(info-node ((,class (:foreground ,func :inherit bold))))
     `(info-quoted-name ((,class (:foreground ,keyword))))
     `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
     `(info-string ((,class (:foreground ,str))))
     `(info-title-1 ((,class (:height 1.4 :inherit bold))))
     `(info-title-2 ((,class (:height 1.3 :inherit bold))))
     `(info-title-3 ((,class (:height 1.3))))
     `(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
     `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     ;; TODO: Does remote count as symlink?
     `(ivy-remote ((,class (:foreground ,actual-cyan))))

;;;;; latex
     `(font-latex-bold-face ((,class (:foreground ,comp))))
     `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when wal-theme-org-highlight head4-bg)))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(font-latex-warning-face ((,class (:foreground ,war))))

;;;;; ledger-mode
     `(ledger-font-directive-face ((,class (:foreground ,meta))))
     `(ledger-font-posting-amount-face ((,class (:foreground ,yellow))))
     `(ledger-font-posting-date-face ((,class (:foreground ,head1))))
     `(ledger-occur-xact-face ((,class (:background ,bg2))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,lnum :background ,bg2 :inherit default))))

;;;;; line-numbers
     `(line-number ((,class (:foreground ,lnum :background ,bg2 :inherit default))))
     `(line-number-current-line ((,class (:foreground ,base :background ,bg2 :inherit line-number))))

;;;;; linum-relative
     `(linum-relative-current-face ((,class (:foreground ,comp))))

;;;;; magit
     ;; TODO: Should git blame be genuine values?
     `(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-branch ((,class (:foreground ,const :inherit bold))))
     `(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
     `(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
     `(magit-branch-remote ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
     `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,base))))
     `(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-sl :foreground ,base))))
     `(magit-hash ((,class (:foreground ,var))))
     `(magit-hunk-heading           ((,class (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
     `(magit-item-highlight ((,class :background ,bg2)))
     `(magit-log-author ((,class (:foreground ,func))))
     `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
     `(magit-log-sha1 ((,class (:foreground ,str))))
     `(magit-process-ng ((,class (:foreground ,war :inherit bold))))
     `(magit-process-ok ((,class (:foreground ,func :inherit bold))))
     `(magit-reflog-amend ((,class (:foreground ,magenta))))
     `(magit-reflog-checkout ((,class (:foreground ,blue))))
     `(magit-reflog-cherry-pick ((,class (:foreground ,green))))
     `(magit-reflog-commit ((,class (:foreground ,green))))
     `(magit-reflog-merge ((,class (:foreground ,green))))
     `(magit-reflog-other ((,class (:foreground ,cyan))))
     `(magit-reflog-rebase ((,class (:foreground ,magenta))))
     `(magit-reflog-remote ((,class (:foreground ,cyan))))
     `(magit-reflog-reset ((,class (:foreground ,red))))
     `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold))))
     `(magit-section-highlight      ((,class (:background ,bg2))))
     `(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))

;;;;; man
     `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     `(Man-reverse ((,class (:foreground ,highlight))))
     `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     `(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when wal-theme-org-highlight head4-bg)))))
     `(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
     `(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))
     `(markdown-table-face ((,class (:foreground ,base :background ,head1-bg))))

;;;;; mode-line
     `(mode-line           ((,class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))))
     `(mode-line-buffer-id ((,class (:inherit bold :foreground ,func))))
     `(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))))

;;;;; mu4e
     `(mu4e-attach-number-face ((,class (:foreground ,var))))
     `(mu4e-cited-1-face ((,class (:foreground ,head1))))
     `(mu4e-cited-2-face ((,class (:foreground ,head2))))
     `(mu4e-cited-3-face ((,class (:foreground ,head3))))
     `(mu4e-cited-4-face ((,class (:foreground ,head4))))
     `(mu4e-cited-5-face ((,class (:foreground ,head1))))
     `(mu4e-cited-6-face ((,class (:foreground ,head2))))
     `(mu4e-cited-7-face ((,class (:foreground ,head3))))
     `(mu4e-contact-face ((,class (:foreground ,func))))
     `(mu4e-draft-face ((,class (:foreground ,var))))
     `(mu4e-flagged-face ((,class (:foreground ,yellow :inherit bold))))
     `(mu4e-header-key-face ((,class (:foreground ,meta :inherit bold))))
     `(mu4e-header-title-face ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-header-marks-face ((,class (:foreground ,comp))))
     `(mu4e-header-value-face ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-header-highlight-face ((,class (:background ,highlight))))
     `(mu4e-highlight-face ((,class (:foreground ,comp))))
     `(mu4e-title-face ((,class (:foreground ,head2 :inherit bold))))
     `(mu4e-replied-face ((,class (:foreground ,green))))
     `(mu4e-modeline-face ((,class (:foreground ,yellow))))
     `(mu4e-special-header-value-face ((,class (:foreground ,green))))
     `(mu4e-unread-face ((,class (:foreground ,head1 :inherit bold))))
     `(mu4e-view-url-number-face ((,class (:foreground ,comp))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face ((,class (:foreground ,head1 :inherit bold))))

;;;;; notmuch
     `(notmuch-search-date ((,class (:foreground ,func))))
     `(notmuch-search-flagged-face ((,class (:weight extra-bold))))
     `(notmuch-search-non-matching-authors ((,class (:foreground ,base-dim))))
     `(notmuch-search-unread-face ((,class (:background ,highlight-dim :box ,border))))
     `(notmuch-tag-face ((,class (:foreground ,keyword))))
     `(notmuch-tag-flagged ((,class (:foreground ,war))))

;;;;; neotree
     `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
     `(neo-expand-btn-face ((,class (:foreground ,base))))
     `(neo-file-link-face ((,class (:foreground ,base))))
     `(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

;;;;; org
     `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
     `(org-agenda-date ((,class (:foreground ,var :height ,(if wal-theme-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold :height ,(if wal-theme-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
     `(org-agenda-done ((,class (:foreground ,suc :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
     `(org-block ((,class (:background ,cblk-bg :foreground ,cblk))))
     `(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
     `(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
     `(org-clock-overlay ((,class (:foreground ,comp))))
     `(org-code ((,class (:foreground ,cyan))))
     `(org-column ((,class (:background ,highlight))))
     `(org-column-title ((,class (:background ,highlight))))
     `(org-date ((,class (:underline t :foreground ,var))))
     `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
     `(org-document-info-keyword ((,class (:foreground ,meta))))
     `(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if wal-theme-org-height 1.4 1.0) :underline t))))
     `(org-done ((,class (:foreground ,suc :inherit bold :background ,actual-green-bg))))
     `(org-ellipsis ((,class (:foreground ,keyword))))
     `(org-footnote  ((,class (:underline t :foreground ,base))))
     `(org-hide ((,class (:foreground ,base))))
     `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     `(org-level-1 ((,class (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground ,head1 :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(org-level-2 ((,class (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground ,head2 :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(org-level-4 ((,class (:bold nil :foreground ,head4 :background ,(when wal-theme-org-highlight head4-bg)))))
     `(org-level-5 ((,class (:bold nil :foreground ,head1))))
     `(org-level-6 ((,class (:bold nil :foreground ,head2))))
     `(org-level-7 ((,class (:bold nil :foreground ,head3))))
     `(org-level-8 ((,class (:bold nil :foreground ,head4))))
     `(org-link ((,class (:underline t :foreground ,comment))))
     `(org-meta-line ((,class (:foreground ,meta))))
     `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
     `(org-priority ((,class (:foreground ,war :inherit bold :bold ,(if wal-theme-org-priority-bold 'unspecified nil)))))
     `(org-quote ((,class (:inherit org-block :slant italic))))
     `(org-scheduled ((,class (:foreground ,comp))))
     `(org-scheduled-today ((,class (:foreground ,func :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
     `(org-scheduled-previously ((,class (:foreground ,base :slant italic))))
     `(org-sexp-date ((,class (:foreground ,base))))
     `(org-special-keyword ((,class (:foreground ,func))))
     `(org-table ((,class (:foreground ,base :background ,head1-bg))))
     `(org-tag ((,class (:foreground ,meta))))
     `(org-time-grid ((,class (:foreground ,str))))
     ;; TODO: Should TODO take actual yellow?
     `(org-todo ((,class (:foreground ,war :inherit bold :background ,actual-yellow-bg))))
     `(org-upcoming-deadline ((,class (:foreground ,war :inherit org-priority))))
     `(org-upcoming-distant-deadline ((,class (:foreground ,suc :inherit org-priority))))
     `(org-verbatim ((,class (:foreground ,keyword))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-warning ((,class (:foreground ,err :inherit org-priority))))

;;;;; outline
     `(outline-1 ((,class (:inherit org-level-1))))
     `(outline-2 ((,class (:inherit org-level-2))))
     `(outline-3 ((,class (:inherit org-level-3))))
     `(outline-4 ((,class (:inherit org-level-4))))
     `(outline-5 ((,class (:inherit org-level-5))))
     `(outline-6 ((,class (:inherit org-level-6))))
     `(outline-7 ((,class (:inherit org-level-7))))
     `(outline-8 ((,class (:inherit org-level-8))))

;;;;; perspective
     `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
     `(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,base))))
     `(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(popup-isearch-match ((,class (:inherit match))))
     `(popup-menu-face ((,class (:background ,ttip-bg :foreground ,base))))
     `(popup-menu-mouse-face ((,class (:inherit highlight))))
     `(popup-scroll-bar-background-face ((,class (:background ,bg2))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
     `(popup-tip-face ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active1 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-active2 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))

;;;;; rainbow-delimiters
     ;; TODO: Redo rainbow delims to ensure they're actually all different
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,keyword)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,keyword)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))
     `(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))

;;;;; rcirc
     `(rcirc-bright-nick ((,class (:background ,aqua-bg :foreground ,cyan))))
     `(rcirc-dim-nick ((,class (:foreground ,base-dim))))
     `(rcirc-keyword ((,class (:background ,green-bg-s :foreground ,green))))
     `(rcirc-timestamp ((,class (:foreground ,keyword))))
     `(rcirc-track-keyword ((,class (:background ,green :foreground ,bg1))))
     `(rcirc-url ((,class (:inherit link))))

;;;;; shm
     `(shm-current-face ((,class (:background ,green-bg-s))))
     `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
     ;; TODO: How to combine mat and green-bg-s?
     `(show-paren-match ((,class (:foreground ,mat :inherit bold  :underline ,(when wal-theme-underline-parens t)))))
     `(show-paren-match-expression ((,class (:background ,green-bg-s))))
     `(show-paren-mismatch ((,class (:foreground ,err :inherit bold :underline ,(when wal-theme-underline-parens t)))))

;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
     `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold  :underline ,(when wal-theme-underline-parens t)))))

;;;;; smerge
     ;; TODO: Actual colors?
     `(smerge-base ((,class (:background ,yellow-bg))))
     `(smerge-markers ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(smerge-mine ((,class (:background ,red-bg))))
     `(smerge-other ((,class (:background ,green-bg))))
     `(smerge-refined-added ((,class (:background ,green-bg-s :foreground ,green))))
     `(smerge-refined-changed ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(smerge-refined-removed ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; spaceline
     `(spaceline-flycheck-error  ((,class (:foreground ,err))))
     `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
     `(spaceline-flycheck-warning((,class (:foreground ,war))))
     `(spaceline-python-venv ((,class (:foreground ,comp))))

;;;;; wal-specific
     `(wal-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face ((,class (:background ,highlight :inherit bold))))
     `(swiper-match-face-1 ((,class (:inherit bold))))
     `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; tabbar
     `(tabbar-button ((,class (:inherit tabbar-default ))))
     `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
     `(tabbar-default ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
     `(tabbar-highlight ((,class (:underline t))))
     `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
     `(tabbar-separator ((,class (:inherit tabbar-default))))
     `(tabbar-unselected ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))

;;;;; term
     `(term ((,class (:foreground ,base :background ,bg1))))
     `(term-color-black ((,class (:foreground ,bg4))))
     `(term-color-blue ((,class (:foreground ,wal-blue))))
     `(term-color-cyan ((,class (:foreground ,wal-cyan))))
     `(term-color-green ((,class (:foreground ,wal-green))))
     `(term-color-magenta ((,class (:foreground ,wal-magenta))))
     `(term-color-red ((,class (:foreground ,wal-red))))
     `(term-color-white ((,class (:foreground ,base))))
     `(term-color-yellow ((,class (:foreground ,wal-yellow))))

;;;;; tide
     `(tide-hl-identifier-face ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; treemacs
     `(treemacs-git-added-face ((,class (:foreground ,actual-green :background ,actual-green-bg))))
     `(treemacs-git-conflict-face ((,class (:foreground ,actual-red :background ,actual-red-bg))))
     `(treemacs-git-ignored-face ((,class (:foreground ,actual-yellow))))
     `(treemacs-git-modified-face ((,class (:foreground ,actual-blue :background ,actual-blue-bg))))
     ;; TODO: Actual aqua for untracked stuff?
     `(treemacs-git-untracked-face ((,class (:foreground ,aqua :background ,aqua-bg))))

;;;;; web-mode
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-current-element-highlight-face ((,class (:background ,bg3))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face ((,class (:foreground ,keyword))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-symbol-face ((,class (:foreground ,type))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face ((,class (:foreground ,base))))
     `(which-key-group-description-face ((,class (:foreground ,keyword))))
     `(which-key-key-face ((,class (:foreground ,func :inherit bold))))
     `(which-key-separator-face ((,class (:background nil :foreground ,str))))
     `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
     `(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
     ;; TODO: Actual yellows in whitespace mode?
     `(whitespace-empty ((,class (:background nil :foreground ,yellow))))
     `(whitespace-indentation ((,class (:background nil :foreground ,war))))
     `(whitespace-line ((,class (:background nil :foreground ,comp))))
     `(whitespace-newline ((,class (:background nil :foreground ,comp))))
     `(whitespace-space ((,class (:background nil :foreground ,act2))))
     `(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-tab ((,class (:background nil :foreground ,act2))))
     `(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; other, need more work
     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     `(ffap ((,class (:foreground ,base))))
     `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
     `(icompletep-determined ((,class :foreground ,keyword)))
     `(js2-external-variable ((,class (:foreground ,comp))))
     `(js2-function-param ((,class (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-private-member ((,class (:foreground ,base))))
     `(js3-error-face ((,class (:underline ,war))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,keyword))))
     `(js3-instance-member-face ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
     `(trailing-whitespace ((,class :foreground nil :background ,err)))
     `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var))))

    (custom-theme-set-variables
     'wal

;;;;; ansi-color-names
     `(ansi-color-names-vector [,bg4 ,wal-red ,wal-green ,wal-yellow ,wal-blue ,wal-magenta ,wal-cyan ,base])

;;;;; hl-todo
     ;; TODO: How to hl-todo?
     ;; TODO: SHould we really highlight TODOs with the warning face?
     `(hl-todo-keyword-faces '(("TODO"   . ,war)
                               ("NEXT"   . ,war)
                               ("THEM"   . ,aqua)
                               ("PROG"   . ,blue)
                               ("OKAY"   . ,blue)
                               ("DONT"   . ,err)
                               ("FAIL"   . ,err)
                               ("DONE"   . ,suc)
                               ;; TODO: Actaul yellows in hl-todo?
                               ("NOTE"   . ,yellow)
                               ("KLUDGE" . ,yellow)
                               ("HACK"   . ,yellow)
                               ("TEMP"   . ,yellow)
                               ("FIXME"  . ,war)
                               ("XXX"    . ,war)
                               ("XXXX"   . ,war)
                               ("???"    . ,war)))

;;;;; pdf-tools
     `(pdf-view-midnight-colors '(,base . ,bg1)))
    ))


;;;###autoload
(and (boundp 'custom-theme-load-path)
     (if load-file-name
         ;; Production load path modification.
         ;;
         ;; This is the standard way to add themes to the load-path.
         (add-to-list 'custom-theme-load-path
                      (file-name-as-directory
                       (file-name-directory load-file-name)))
       ;; Development load path mofification.
       ;;
       ;; For debugging/development: if this file was not loaded as a package,
       ;; it will have no `load-file-name'. use the buffer file name instead.
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory buffer-file-name)))))


(defun wal-get-wal-colors ()
  "Load Pywal's colors from the cache folder.

Only gets the 16 terminal colors. Ignores the foreground, cursor
& background.

Returns a list of 16 colors (color0, color1, etc.)."
  (let* ((wal-cache-folder "~/.cache/wal")
         (wal-cache-json-file (concat wal-cache-folder "/colors.json"))
         (colors-json (json-read-file wal-cache-json-file)))
    (mapcar (lambda (color-index)
              (let ((colors-alist (alist-get 'colors colors-json))
                    (color-name (format "color%s" color-index)))
                (alist-get (intern color-name) colors-alist)))
            (number-sequence 0 15))))


;; TODO: Uncomment and make functional
;; (deftheme wal "Theme generated from Pywal's colors.")
;; (wal-create-theme 'wal)
;; (provide-theme 'wal)

;; Testing section
;; TODO: Remove
(deftheme wal "Theme generated from Pywal's colors.")
;; (wal-create-theme
;;  "#27292C" "#F1231E" "#66B01C" "#B0941C" "#4E96D6" "#A21CB0" "#27DDEF" "#B0B0B0"
;;  "#666666" "#F1231E" "#66B01C" "#B0941C" "#4E96D6" "#A21CB0" "#27DDEF" "#B1B1B1")
(apply 'wal-create-theme
       (wal-get-wal-colors))

;; TODO: For testing. Remove.
(when nil
  (progn
    (helm-themes--delete-theme)
    (load-theme 'wal t nil)))


(provide-theme 'wal)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:

;;; wal-theme.el ends here

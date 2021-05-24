;;; svg-lib.el --- SVG tags, bars & icons -*- lexical-binding: t -*-

;; Copyright (C) 2021 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/svg-lib
;; Keywords: convenience
;; Version: 0.1

;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage example:
;;
;; (insert-image (svg-lib-tag "TODO"))
;; (insert-image (svg-lib-progress-bar 0.33))
;; (insert-image (svg-lib-icon "material" "star" :stroke 0))
;;
;;
;; (dotimes (i 10)
;;   (insert-image (svg-lib-progress-bar (/ (+ i 1) 10.0)
;;                     :width 5 :margin 1 :stroke 2 :padding 2)))
;;
;;
;; Icons ares created by parsing remote collections whose license are
;; compatibles with GNU Emacs:
;;
;; - Boxicons (https://github.com/atisawd/boxicons), available under a
;;   Creative Commons 4.0 license.  As of version 2.07 (December 2020),
;;   this collection offers 1500 icons in two styles (regular & solid).
;;   Gallery is available at https://boxicons.com/
;;
;; - Octicons (https://github.com/primer/octicons), available under a
;;   MIT License with some usage restriction for the GitHub logo.  As of
;;   version 11.2.0 (December 2020), this collection offers 201 icons.
;;   Gallery available at https://primer.style/octicons/
;;
;; - Material (https://github.com/google/material-design-icons),
;;   available under an Apache 2.0 license.  As of version 4.0.0
;;   (December 2020), this collection offers 500+ icons in 4 styles
;;   (filled, outlined, rounded, sharp).  Gallery available at
;;   https://material.io/resources/icons/?style=baseline
;;
;; - Bootstrap (https://github.com/twbs/icons), available under an MIT
;;   license.  As of version 1.2.1 (December 2020), this collection
;;   offers 1200+ icons in 2 styles (regular & filled).  Gallery
;;   available at https://icons.getbootstrap.com/
;;
;; The default size of an icon is exactly 2x1 characters such that it
;; can be inserted inside a text without disturbing alignment.
;;
;; Note: Each icon is cached locally to speed-up loading the next time
;;       you use it.  If for some reason the cache is corrupted you can
;;       force reload using the svg-icon-get-data function.
;;
;; If you want to add new collections (i.e. URL), make sure the icons
;; are monochrome, their size is consistent.

;;; Code:
(require 'svg)

(defgroup svg-lib nil
  "SVG tags, bars & icons."
  :group 'convenience
  :prefix "svg-lib-")

(defcustom svg-lib-default-margin 1
  "Default margin in characters."
  :type 'integer
  :group 'svg-lib)

(defcustom svg-lib-default-padding 1
  "Default padding in characters for tags, in pixels for bars & icons."
  :type 'integer
  :group 'svg-lib)

(defcustom svg-lib-default-radius 3
  "Default radius in pixels."
  :type 'integer
  :group 'svg-lib)

(defcustom svg-lib-default-zoom 1
  "Default zoom level for icons."
  :type 'integer
  :group 'svg-lib)

(defcustom svg-lib-default-width 20
  "Default width of progress bar in characters."
  :type 'integer
  :group 'svg-lib)

(defcustom svg-lib-default-stroke 1
  "Default stroke width in pixels."
  :type 'integer
  :group 'svg-lib)

(defface svg-lib-default-face
  `((t :foreground ,(face-attribute 'default :foreground)
       :background ,(face-attribute 'default :background)
       :box (:line-width ,svg-lib-default-stroke
             :color ,(face-attribute 'default :foreground)
             :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "Default face used for all SVG objects"
:group 'svg-lib)

;; SVG font weights translation
(defvar svg-lib--font-weights '((thin       . 100)
                                (ultralight . 200)
                                (light      . 300)
                                (regular    . 400)
                                (medium     . 500)
                                (semibold   . 600)
                                (bold       . 700)
                                (extrabold  . 800)
                                (black      . 900)))

(defcustom  svg-lib-icon-collections
  '(("bootstrap" .
     "https://icons.getbootstrap.com/icons/%s.svg")
    ("material" .
     "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" .
     "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    ("boxicons" .
     "https://boxicons.com/static/img/svg/regular/bx-%s.svg"))
    
  "Various icons collections stored as (name . base-url).

The name of the collection is used as a pointer for the various
icon creation methods.  The base-url is a string containing a %s
such that is can be replaced with the name of a specific icon.
User is responsible for finding/giving proper names for a given
collection (there are way too many to store them)."

  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "URL"))
  :group 'svg-lib)



(defun svg-lib-convert-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun svg-lib-foreground (face)
  "Return the foreground color of FACE, ensuring it is specified."
  (face-attribute face :foreground nil 'default))

(defun svg-lib-background (face)
  "Return the background color of FACE, ensuring it is specified."
  (face-attribute face :background nil 'default))


;; --- Tags ------------------------------------------------------------
(defun svg-lib-tag (label &rest args)
  "Create an SVG image displaying LABEL in a rounded box.

Visual aspect can be controlled using the ARGS parameters:

  :face FACE              The face to use
  :radius RADIUS          The radius in pixels of the box
  :margin MARGIN          The (external) margin in characters
  :padding PADDING        The (internal) padding in characters
  :text-color TEXT-COLOR  The color of the label
  :line-color LINE-COLOR  The border color of the box
  :fill-color FILL-COLOR  The background color of the box
  :inverse INVERSE        Whether to swap text and fill colors
  :stroke STROKE          The width in pixels of the border of the box
  :weight WEIGHT          The font weight of the label
                            (takes precedence over face)
  :family FAMILY          The font family of the label
                            (takes precedence over face)"
  
  (let* ((face          (or (plist-get args :face)
                            'svg-lib-default-face))
         (padding       (or (plist-get args :padding)
                            svg-lib-default-padding))
         (margin        (or (plist-get args :margin)
                            svg-lib-default-margin))
         (radius        (or (plist-get args :radius)
                            svg-lib-default-radius))
         (inverse       (or (plist-get args :inverse)
                            nil))
         
         (text-color (or (plist-get args :text-color)
                         (if inverse
                             (svg-lib-background face)
                           (svg-lib-foreground face))))
         
         (fill-color (or (plist-get args :fill-color)
                         (if inverse
                             (svg-lib-foreground face)
                           (svg-lib-background face))))

         (line-color (or (plist-get args :line-color)
                         (plist-get args :text-color)
                         (plist-get (face-attribute face :box) :color)
                         text-color))
         
         (stroke     (or (plist-get args :stroke)
                         (plist-get (face-attribute face :box) :line-width)
                         svg-lib-default-stroke))
         
         (weight     (or (plist-get args :weight)
                         (face-attribute face :weight nil 'default)))
         (weight     (or (cdr (assoc weight svg-lib--font-weights))
                         weight))

         (family     (or (plist-get args :family)
                         (face-attribute face :family)))

         (size       (/ (face-attribute face :height nil 'default) 10))
         (ascent     (elt (font-info (format "%s:%d" family size)) 8))
         
         (tag-char-width  (window-font-width nil face))
         (tag-char-height (window-font-height nil face))
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         
         (tag-width (* (+ (length label) padding) txt-char-width))
         (tag-height (* txt-char-height 0.9))

         (svg-width (+ tag-width (* margin txt-char-width)))
         (svg-height tag-height)

         (tag-x (/ (- svg-width tag-width) 2))
         (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width)) 2)))
         (text-y ascent)
         (svg (svg-create svg-width svg-height)))

    (if (>= stroke 0.25)
        (svg-rectangle svg tag-x 0 tag-width tag-height
                       :fill        (svg-lib-convert-color line-color)
                       :rx          radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                   (- tag-width stroke) (- tag-height stroke)
                   :fill        (svg-lib-convert-color fill-color)
                   :rx          (- radius (/ stroke 2.0)))
    (svg-text      svg label
                   :font-family family
                   :font-weight weight
                   :font-size   size
                   :fill        (svg-lib-convert-color text-color)
                   :x           text-x
                   :y           text-y)
    (svg-image svg :scale 1 :ascent 'center)))



;; --- Progress bars ---------------------------------------------------
(defun svg-lib-progress-bar (value &rest args)
  "Create a SVG progress bar image with value VALUE.

Visual aspect can be controlled using the ARGS parameters:

  :face FACE              The face to use
  :width WIDTH            Total width in characters of the bar
  :radius RADIUS          The radius in pixels of the bar
  :margin MARGIN          The (external) margin in characters
  :padding PADDING        The (internal) padding in pixels
  :bar-color TEXT-COLOR   The color of the progress bar
  :line-color LINE-COLOR  The border color of the bar
  :fill-color FILL-COLOR  The background color of the bar
  :inverse INVERSE        Whether to swap bar and fill colors
  :stroke STROKE          The width in pixels of the border of the bar"

  (let* ((face          (or (plist-get args :face)
                            'svg-lib-default-face))
         (padding       (or (plist-get args :padding)
                            svg-lib-default-padding))
         (margin        (or (plist-get args :margin)
                            svg-lib-default-margin))
         (radius        (or (plist-get args :radius)
                            svg-lib-default-radius))
         (inverse       (or (plist-get args :inverse)
                            nil))

         (bar-color (or (plist-get args :bar-color)
                        (if inverse
                            (svg-lib-background face)
                          (svg-lib-foreground face))))

         (fill-color (or (plist-get args :fill-color)
                         (if inverse
                             (svg-lib-foreground face)
                           (svg-lib-background face))))

         (line-color (or (plist-get args :line-color)
                         (plist-get args :text-color)
                         (plist-get (face-attribute face :box) :color)
                         bar-color))
         
         (stroke     (or (plist-get args :stroke)
                         (plist-get (face-attribute face :box) :line-width)
                         svg-lib-default-stroke))
         (width      (or (plist-get args :width)
                         svg-lib-default-width))
         
         (weight     (or (plist-get args :weight)
                         (face-attribute face :weight)))
         (weight     (cdr (assoc weight svg-lib--font-weights)))

         (family     (or (plist-get args :family)
                         (face-attribute face :family)))

         (size       (face-attribute face :height))
         (size       (if (eq size 'unspecified)
                         (face-attribute 'default :height) size))
         (size       (/ size 10))
         (ascent (elt (font-info (format "%s:%d" family size)) 8))
         
         (tag-char-width  (window-font-width nil face))
         (tag-char-height (window-font-height nil face))
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         
         (tag-width (* width txt-char-width))
         (tag-height (* txt-char-height 0.9))

         (svg-width (+ tag-width (* margin txt-char-width)))
         (svg-height tag-height)

         (tag-x (/ (- svg-width tag-width) 2))
         (svg (svg-create svg-width svg-height)))

    (if (>= stroke 0.25)
        (svg-rectangle svg tag-x 0 tag-width tag-height
                       :fill        (svg-lib-convert-color line-color)
                       :rx          radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0))
                       (/ stroke 2.0)
                       (- tag-width stroke)
                       (- tag-height stroke)
                   :fill  (svg-lib-convert-color fill-color)
                   :rx    (- radius (/ stroke 2.0)))

    (svg-rectangle svg (+ tag-x (/ stroke 2.0) padding)
                       (+ (/ stroke 2.0) padding)
                       (- (* value tag-width) stroke (* 2 padding))
                       (- tag-height stroke (* 2 padding))
                   :fill (svg-lib-convert-color bar-color)
                   :rx   (- radius (/ stroke 2.0)))
    (svg-image svg :scale 1 :ascent 'center)))



;; --- Icons -----------------------------------------------------------
(defun svg-lib--icon-get-data (collection name &optional force-reload)
  "Retrieve icon NAME from COLLECTION.

Cached version is returned if it exists unless FORCE-RELOAD is t."
  
  ;; Build url from collection and name without checking for error
  (let ((url (format (cdr (assoc collection svg-lib-icon-collections)) name)))

    ;; Get data only if not cached or if explicitely requested
    (if (or force-reload (not (url-is-cached url)))
        (let ((url-automatic-caching t)
              (filename (url-cache-create-filename url)))
          (with-current-buffer (url-retrieve-synchronously url)
            (write-region (point-min) (point-max) filename))))

    ;; Get data from cache
    (let ((buffer (generate-new-buffer " *temp*")))
      (with-current-buffer buffer
        (url-cache-extract (url-cache-create-filename url)))
      (with-temp-buffer
        (url-insert-buffer-contents buffer url)
        (xml-parse-region (point-min) (point-max))))))

(defun svg-lib-icon (collection name &rest args)
  "Create a SVG image displaying icon NAME from COLLECTION.

Default size is 2x1 characters.
Visual aspect can be controlled using the ARGS parameters:

  :face FACE              The face to use
  :zoom ZOOM              Size of the icon (interger value)
  :radius RADIUS          The radius in pixels of the box
  :margin MARGIN          The (external) margin in characters
  :padding PADDING        The (internal) padding in characters
  :icon-color TEXT-COLOR  The color of the icon
  :line-color LINE-COLOR  The border color of the box
  :fill-color FILL-COLOR  The background color of the box
  :inverse INVERSE        Whether to swap text and fill colors
  :stroke STROKE          The width in pixels of the border of the box"
  
  (let* ((root (svg-lib--icon-get-data collection name))

         (face          (or (plist-get args :face)
                            'svg-lib-default-face))
         (padding       (or (plist-get args :padding)
                            svg-lib-default-padding))
         (margin        (or (plist-get args :margin)
                            svg-lib-default-margin))
         (radius        (or (plist-get args :radius)
                            svg-lib-default-radius))
         (zoom          (or (plist-get args :zoom)
                            svg-lib-default-zoom))
         (inverse       (or (plist-get args :inverse)
                            nil))
         (icon-color (or (plist-get args :icon-color)
                         (if inverse
                             (svg-lib-background face)
                           (svg-lib-foreground face))))
         (fill-color (or (plist-get args :fill-color)
                         (if inverse
                             (svg-lib-foreground face)
                           (svg-lib-background face))))
         (line-color (or (plist-get args :line-color)
                         (plist-get args :text-color)
                         (plist-get (face-attribute face :box) :color)
                         icon-color))
         (stroke     (or (plist-get args :stroke)
                         (plist-get (face-attribute face :box) :line-width)
                         svg-lib-default-stroke))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 2x1 characters
         (svg-width  (* (window-font-width)  2))
         (svg-height (* (window-font-height) 1))

         ;; Compute the new viewbox (adjust y origin and height)
         (ratio (/ view-width svg-width))
         (delta-h (ceiling (/ (- view-height (* svg-height ratio) ) 2)))
         (view-y (- view-y delta-h))
         (view-height (+ view-height (* delta-h 2)))

         ;; Zoom the icon by using integer factor only
         (zoom (max 1 (truncate (or zoom 1))))
         (svg-width  (* svg-width zoom))
         (svg-height (* svg-height zoom))

         (svg-viewbox (format "%f %f %f %f"
                              view-x view-y view-width view-height))
         (f-ratio (/ (float view-width) (float svg-width)))
         (transform (format "translate(%f,%f) scale(%f)"  view-x view-y f-ratio))
         (svg (svg-create svg-width svg-height
                          :viewBox svg-viewbox
                          :stroke-width 0
                          :fill (svg-lib-convert-color fill-color))))

    (if (>= stroke 0.25)
        (svg-rectangle svg 0 0 svg-width svg-height
                           :fill (svg-lib-convert-color icon-color)
                           :rx radius
                           :transform transform))
      (svg-rectangle svg (/ stroke 2.0) (/ stroke 2.0)
                         (- svg-width stroke) (- svg-height stroke)
                         :fill (svg-lib-convert-color fill-color)
                         :rx  (- radius (/ stroke 2.0))
                         :transform transform)

    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             (fill (or (cdr (assoc 'fill attrs))
                       (svg-lib-convert-color icon-color))))
        (message fill)
        (svg-node svg 'path :d path :fill fill)))
    (svg-image svg :ascent 'center :scale 1)))


(provide 'svg-lib)
;;; svg-lib.el ends here

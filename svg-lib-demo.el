(dotimes (i 5)
  (insert-image (svg-lib-tag "TODO" nil
                      :font-family "Roboto Mono" :font-weight (* (+ i 2) 100))))

          

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :padding 1 :stroke (/ i 4.0))))

          

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :stroke 2 :radius i)))

          

(dotimes (i 10)
  (insert-image (svg-lib-progress (/ (+ i 1) 10.0) nil
                    :width 5 :margin 1 :stroke 2 :padding 2)))

          

(insert-image (svg-lib-progress 0.75 nil :radius 8 :stroke 2 :padding 0))

 

(dotimes (i 10)
  (insert-image (svg-lib-icon "star" nil :scale (/ (+ i 1) 10.0))))

          

(insert-image (svg-lib-button "check-bold" "DONE" nil
                              :font-family "Roboto Mono"
                              :font-weight 500
                         :stroke 0 :background "#673AB7" :foreground "white"))
 

(insert-image (svg-lib-icon "gnuemacs" nil :collection "simple"
                            :stroke 0 :scale 1 :padding 0))
 GNU Emacs

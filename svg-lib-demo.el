(dotimes (i 5)
  (insert-image (svg-lib-tag "TODO" nil
                             :family "Roboto Mono" :weight (* (+ i 2) 100))))

     

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :padding 1 :thickness (/ i 4.0))))

          

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" nil :thickness 2 :radius i)))

          

(dotimes (i 10)
  (insert-image (svg-lib-progress (/ (+ i 1) 10.0) nil
                    :width 5 :margin 1 :thickness 2 :padding 2)))

          

(insert-image (svg-lib-progress .75 nil
                   :foreground "#999999" :stroke "#999999" :margin 0
                   :background "#f0f0f0" :radius 0 :thickness .5 :padding 0))

 

(insert-image (svg-lib-progress 0.75 nil :radius 8 :thickness 2 :padding 0))

 

(dotimes (i 10)
  (insert-image (svg-lib-icon "material" "star" nil
                              :scale (/ (+ i 1) 10.0))))

          

(insert-image (svg-lib-icon "material" "star" nil :radius 8
                            :thickness 2 :scale 0.75 :padding 0))
 


(dotimes (i 5)
  (insert-image (svg-lib-tag "TODO" :padding 1
                             :family "Roboto Mono" :weight (* (+ i 2) 100))))

     

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" :padding 1 :stroke (/ i 4.0))))

          

(dotimes (i 10)
  (insert-image (svg-lib-tag "TODO" :padding 1 :stroke 2 :radius i)))

          

(dotimes (i 10)
  (insert-image (svg-lib-progress-bar (/ (+ i 1) 10.0)
                    :width 5 :margin 1 :stroke 2 :padding 2)))

          

(insert-image (svg-lib-progress-bar .75
                   :bar-color "#999999" :line-color "#999999" :margin 0
                   :fill-color "#f0f0f0" :radius 0 :stroke .5 :padding 0))

 

(insert-image (svg-lib-progress-bar 0.75 :radius 8 :stroke 2 :padding 0))

 


(insert-image (svg-lib-icon "material" "star" :stroke 0))
(insert-image (svg-lib-icon "material" "star" :stroke 1.5))
(insert-image (svg-lib-icon "material" "star" :inverse t))
      

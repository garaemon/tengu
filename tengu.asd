(defsystem tengu
    :depends-on (chimi iterate)
    :components
    ((:file "tengu")
     (:file "graph" :depends-on ("tengu"))
     (:file "strips" :depends-on ("tengu" "graph"))
     ))

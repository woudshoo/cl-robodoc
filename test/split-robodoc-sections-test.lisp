(in-package :cl-robodoc-test)


(deftestsuite split-robodoc-tests ()
  ((simple-rd '(:robodoc ((#\c #\m) "Hallo" "Daar") nil)))
  (:tests
   (name-test 
    (ensure-same (cl-robodoc::names simple-rd) '("Hallo" "Daar")))))





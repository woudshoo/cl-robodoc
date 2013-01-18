(in-package :cl-robodoc)


(deftestsuite split-robodoc-tests ()
  ((simple-rd '(:robodoc ((#\c #\m) "Hallo" "Daar") nil)))
  (:tests
   (name-test 
    (ensure-same (names simple-rd) '("Hallo" "Daar")))))





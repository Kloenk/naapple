(defpackage :dev.kloenk.naapple
  (:use common-lisp :dev.kloenk.naapple.napster)
  (:export
   ))


(defun write-napster-playlist-csv (napster-playlist-source file &optional (offset 0) verbose)
  (let ((playlist (get-playlist-tracks-all napster-playlist-source offset verbose)))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (format out "id, name, artist, album, disc, time, explicit isrc~%")
        (mapcar #'(lambda(x) (with-slots (id name track-count description disc seconds explicit isrc artist album) x
                                (format out "~A, ~A, ~A, ~A, ~D, ~Ds, ~A, ~A~%" id name artist album disc seconds (if explicit 1 0) isrc))
                              ) playlist)))))

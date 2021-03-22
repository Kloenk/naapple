(defpackage :dev.kloenk.naapple.napster
  (:use common-lisp)
  (:export
   :napster-source
   :napster-playlist-source))

(in-package :dev.kloenk.naapple.napster)

(defvar *napster-doc-api-key* "YTkxZTRhNzAtODdlNy00ZjMzLTg0MWItOTc0NmZmNjU4Yzk4")
(defvar *napster-api-url* "https://api.napster.com/v2.2/")

(defclass napster-source ()
  ((apikey
    :initarg :apikey
    :accessor apikey
    :initform *napster-doc-api-key*
    :documentation "api key to connect to napster api"
  )))

(defun get-url (napster-source url &optional verbose)
  (with-slots (apikey) napster-source
    (dex:get url :verbose verbose :headers `(("apikey" . ,apikey) ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")))))

(defmethod print-object ((object napster-source) stream)
  (print-unreadable-object (object stream :type t)
      (format stream "Napster source")))

(defclass napster-playlist-source (napster-source)
  ((playlist
    :initarg :playlist
    :accessor playlist
    :initform (error "you have to supply a :playlist to connect to")
    :documentation "playlist id for napster playlists")
   (name
    :initform nil
    :documentation "Human readable name")
   (track-count
    :initform nil
    :documentation "number of tracks in this playlist")
   (description
    :initform nil
    :documentation "description of the playlist")
   (modified
    :initform nil
    :documentation "last modified date")))

(defmethod print-object ((object napster-playlist-source) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (playlist name) object
      (format stream "Napster playlist ~s name: ~A" playlist name))))

(defun gen-playlist-url (napster-playlist-source)
  (with-slots (playlist) napster-playlist-source
    (format nil "~Aplaylists/~A" *napster-api-url* playlist)))

(defun get-playlist (napster-playlist-source &optional verbose)
  (get-url napster-playlist-source (gen-playlist-url napster-playlist-source) verbose))

(defun get-playlist-json (napster-playlist-source &optional verbose)
  (json:decode-json-from-string (get-playlist napster-playlist-source verbose)))

(defun get-playlist-meta-uncached (napster-playlist-source &optional verbose)
  (let* ((json (get-playlist-json napster-playlist-source verbose))
         (playlist (getf (nth 0 json) :playlists))
         (meta (assoc :meta json))
         (returned-count (cdr (nth 1 meta)))
         (name (cdr (assoc :name playlist)))
         )
    (if (not (= 1 returned-count)) (error "More that one playlist reported"))
    (setf (slot-value napster-playlist-source 'name) name)
    (setf (slot-value napster-playlist-source 'track-count) (cdr (assoc :track-count playlist)))
    (setf (slot-value napster-playlist-source 'description) (cdr (assoc :description playlist)))
    (setf (slot-value napster-playlist-source 'modified) (cdr (assoc :modified playlist))))
  (with-slots (name track-count description modified) napster-playlist-source (values name track-count description modified)))

(defun get-playlist-meta (napster-playlist-source &optional verbose)
  ; FIXME: add ttl based on modified or similar
  (if (slot-value napster-playlist-source 'modified) ;FIXME: verbose log
      (with-slots (name track-count description modified) napster-playlist-source (values name track-count description modified))
  (get-playlist-meta-uncached napster-playlist-source verbose)))

(defun gen-playlist-tracks-url (napster-playlist-source &optional (offset 0) (limit 200))
  (let ((base-url (gen-playlist-url napster-playlist-source)))
    (format nil "~A/tracks?offset=~D&limit=~D" base-url offset limit)))

(defun get-playlist-tracks (napster-playlist-source &optional (offset 0) (limit 200) verbose)
  (get-url napster-playlist-source (gen-playlist-tracks-url napster-playlist-source offset limit) verbose))
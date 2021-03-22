(defpackage :dev.kloenk.naapple-system (:use :asdf :cl))

(in-package :dev.kloenk.naapple-system)

(defsystem "naapple"
  :name "naapple"
  :author "Finn Behrens <me@kloenk.dev>"
  :version "0.1.0"
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "napster")
                 )))
  :depends-on ("dexador" "lparallel" "cl-json"))

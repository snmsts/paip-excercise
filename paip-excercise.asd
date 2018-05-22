;;don't edit
(DEFSYSTEM "paip-excercise" :DEPENDS-ON (:PAIP :CL-PPCRE) :LICENSE "mit" :CLASS
 :PACKAGE-INFERRED-SYSTEM :COMPONENTS
 ((:FILE "src/chapter04") (:FILE "src/read") (:FILE "src/chapter03")
  (:FILE "src/export") (:FILE "src/chapter02") (:FILE "src/chapter01"))
 :AUTHOR "SANO Masatoshi" :MAILTO "snmsts@gmail.com" :IN-ORDER-TO
 ((TEST-OP (TEST-OP "paip-excercise/tests"))))
(DEFSYSTEM "paip-excercise/tests" :DEPENDS-ON ("paip-excercise" "rove")
 :COMPONENTS NIL :PERFORM
 (TEST-OP (O C) (SYMBOL-CALL :ROVE :RUN "paip-excercise")))
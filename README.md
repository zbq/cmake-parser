# cmake-parser

This is a project to parse cmake scripts.

## Example
    (defun common-usage ()
      (let ((binding (make-hash-table :test 'equal))
            (prod (cmake-parser:parse-string "SET(TARGET_NAME \"HelloWorld\")
                                              ADD_EXECUTABLE(${TARGET_NAME} hello.cpp world.cpp)")))
        (loop for call in prod
          do (cond
               ((string-equal "SET" (first call))
                (setf (gethash (second call) binding) (third call)))
               ((string-equal "ADD_EXECUTABLE" (first call))
                (format t "bin: ~A, src: ~{~A ~}~%"
                        (cmake-parser:expand-argument (second call) binding)
                        (cddr call)))
               (t (format t "Not Supported Command Invocation"))))))

This will output: bin: HelloWorld, src: hello.cpp world.cpp

## CMake language grammar

https://cmake.org/cmake/help/v3.12/manual/cmake-language.7.html

## Repository

https://github.com/zbq/cmake-parser/

## License

MIT


# cmake-parser

This is a project to parse cmake scripts, it will parse script into a list of command invocation.

## API

1. expand-argument (arg binding)
     Expand argument if it contains variable reference.
     You should provide a variable binding hash-table.
     Ex. arg: 'Hello ${someone}', binding: 'someone'->'Bob',
     it will expand to 'Hello Bob'.

2. grammar ()
     Return the cmake grammar.

3. parse-string (str)
     Parse cmake script string, return a list of command invocation.

4. parse-file (pathname)
     Parse cmake script file, return a list of command invocation.

## Example

    (defun common-usage ()
      (let ((binding (make-hash-table :test 'equal))
            (prod (cmake-parser:parse-string "# A demo helloworld
                                              SET(TARGET_NAME \"HelloWorld\")
                                              ADD_EXECUTABLE(${TARGET_NAME} hello.cpp world.cpp)")))
        (loop for call in prod
          do (progn
               (format t "~{~A ~}~%" call)
               (cond
                 ((string-equal "SET" (first call))
                  (setf (gethash (second call) binding) (third call)))
                 ((string-equal "ADD_EXECUTABLE" (first call))
                  (format t "bin: ~A, src: ~{~A ~}~%"
                          (cmake-parser:expand-argument (second call) binding)
                          (cddr call)))
                 (t (format t "Not Supported Command Invocation")))))))

This will output: 

    - SET TARGET_NAME HelloWorld 
    - ADD_EXECUTABLE ${TARGET_NAME} hello.cpp world.cpp 
    - bin: HelloWorld, src: hello.cpp world.cpp

## CMake language grammar

https://cmake.org/cmake/help/v3.12/manual/cmake-language.7.html

## Repository

https://github.com/zbq/cmake-parser.git

## License

MIT


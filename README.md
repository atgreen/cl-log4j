# cl-log4j
## A Common Lisp wrapper for log4j using OpenLDK

`cl-log4j` is an experiment in bringing Java libraries to Common Lisp
using [OpenLDK](https://github.com/atgreen/openldk).  The real [Apache
Log4J](https://logging.apache.org/log4j/2.x/index.html) code is
executed in-process without any external JVM.  This is possible
because OpenLDK transpiles Java bytecode into Common Lisp code in
memory, ready for compilation and execution by your Lisp runtime
environment.

Check out [log4cl](https://github.com/sharplispers/log4cl) if you want
a proper logging system for Common Lisp.  The purpose of this project
is exercise OpenLDK.

Log4J is an interesting library for exercising OpenLDK thanks to
extensive use of reflection, dynamic code generation, unsafe methods,
stack walking, and more.

The OpenLDK APIs are under very active development and are sure to
change.  Any feedback would really be appreciated!

You will need to set your `JAVA_HOME` environment variable, as well as
`LDK_CLASSPATH`.  `JAVA_HOME` should point at your JDK 1.8
installation.  On my Fedora Linux box, I have it set like so:

```
export LDK_HOME=/usr/lib/jvm/java-1.8.0/jre
```

`LDK_CLASSPATH` should point at the log4j jar files. I run sbcl like so:

```
$ LDK_CLASSPATH=log4j-api-2.3.2.jar:log4j-core-2.3.2.jar sbcl
```

OpenLDK is available in the [ocicl](https://github.com/ocicl/ocicl)
repo.  Just run `ocicl install openldk` to download OpenLDK.

Before using `cl-log4j`, you must first initialize it like so:
```
(log4j:initialize)
```

This transpiles the necessary java bytecode to common lisp in memory.
This initial step is currently very slow.  I expect great improvements
over time.

To log an error message, use:
```
(log4j:log-error "Hello World!")
```

The first message logged will trigger more classes to be loaded and
methods to be compiled (OpenLDK is a JIT compiler).  Subsequent
logging should be fast.

The output may look unusual.  There's work to be done. I expect things
to improve fairly rapidly.

Happy Hacking,</br>
Anthony Green

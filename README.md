# cl-log4j
## An OpenLDK wrapper for log4j

`cl-log4j` is an experiment in bringing Java libraries to Common Lisp
using [OpenLDK](https://github.com/atgreen/openldk).

Check out [log4cl](https://github.com/sharplispers/log4cl) if you want
a proper logging system for Common Lisp.  The purpose of this project
is exercise OpenLDK.

`log4j` is an interesting library for exercising OpenLDK thanks to
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
system library.  Just run `ocicl install openldk` to download OpenLDK.

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

The output may look unusual.  There's work to be done. I expect things
to improve fairly rapidly.


Happy Hacking,
Anthony Green

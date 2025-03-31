check: libs/log4j-api-2.3.2.jar libs/log4j-core-2.3.2.jar
	LDK_CLASSPATH=.:libs/log4j-api-2.3.2.jar:libs/log4j-core-2.3.2.jar JAVA_HOME=/usr/lib/jvm/java-1.8.0/jre LDK_DEBUG=l sbcl --load test.lisp

libs/log4j-api-2.3.2.jar log4j-core-2.3.2.jar:
	mvn dependency:copy-dependencies -DoutputDirectory=./libs

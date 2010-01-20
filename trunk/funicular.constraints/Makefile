JAVAC = javac -d bin -sourcepath src -classpath lib/jna.jar
SCALAC = scalac -d bin -sourcepath src -classpath bin:lib/jna.jar

jar: constraints.jar

bin/x10constraints/constraint.class: src/x10constraints/constraint.scala
	$(SCALAC) src/x10constraints/constraint.scala

bin/x10constraints/Constraints.class: src/x10constraints/Constraints.scala
	$(SCALAC) src/x10constraints/Constraints.scala

constraints.jar: lib/scalac-plugin.xml bin/x10constraints/Constraints.class bin/x10constraints/constraint.class
	cp lib/scalac-plugin.xml bin
	(cd bin; jar cf ../constraints.jar .)
	(cd lib; jar xf jna.jar com; jar uf ../constraints.jar com; rm -rf com)

dumb:
	(cd lib; jar uf ../constraints.jar libstp.dylib)

test: constraints.jar Test.scala
	scalac -Xplugin:constraints.jar -classpath constraints.jar:lib/jna.jar Test.scala

stp:
	$(JAVAC) src/stp/jna/*.java
	$(SCALAC) src/stp/STP.scala


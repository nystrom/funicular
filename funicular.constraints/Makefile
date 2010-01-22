JAVAC = javac -d bin -sourcepath src -classpath lib/jna.jar
SCALAC = scalac -d bin -sourcepath src -classpath bin:lib/jna.jar

jar: constraints.jar

bin/funicular/constraints/where.class: src/funicular/constraints/where.scala
	$(SCALAC) src/funicular/constraints/where.scala

bin/funicular/constraints/Constraints.class: src/funicular/constraints/Constraints.scala
	$(SCALAC) src/funicular/constraints/Constraints.scala

constraints.jar: lib/scalac-plugin.xml bin/funicular/constraints/Constraints.class bin/funicular/constraints/where.class
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


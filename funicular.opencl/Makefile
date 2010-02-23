JAVAC = javac -d bin -sourcepath src -classpath bin:lib/javacl-1.0-beta-4-shaded.jar
SCALAC = fsc -d bin -sourcepath src -classpath bin:lib/javacl-1.0-beta-4-shaded.jar

# jna: bin/opencl/jna/cl.class
# 
# bin/opencl/jna/cl.class: src/opencl/jna/cl.java
	# $(JAVAC) src/opencl/jna/cl.java

jnas: bin/opencl/OpenCL.class
	$(SCALAC) src/opencl/OpenCL.scala

bin/opencl/OpenCL.class: src/opencl/OpenCL.scala
	$(SCALAC) src/opencl/OpenCL.scala

bin/Test.class: Test.scala bin/opencl/OpenCL.class
	$(SCALAC) Test.scala

# bin/opencl/jna/cl.class

test: bin/Test.class
	scala -classpath bin:lib/javacl-1.0-beta-4-shaded.jar Test

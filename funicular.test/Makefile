RT = ../x10/src/x10/*.scala ../x10/src/x10/runtime/*.scala
RTC = $(patsubst %.scala,%.class,$(subst /src/,/bin/,$(RT)))
CP = bin:../x10/bin:../x10/lib/jsr166y.jar
SCALAC = scalac -optimise -Yinline -Yclosure-elim -Ydead-code

compilert: $(RTC)

rtc:
	scalac -optimise -d ../x10/bin -sourcepath ../x10/src -classpath bin:../x10/bin:../x10/lib/jsr166y.jar ../x10/src/x10/runtime/*.scala ../x10/src/x10/*.scala

../x10/bin/x10/%.class: ../x10/src/x10/%.scala
	$(SCALAC) -d ../x10/bin -classpath $(CP) -sourcepath ../x10/src $<

../x10/bin/x10/runtime/%.class: ../x10/src/x10/runtime/%.scala
	$(SCALAC) -d ../x10/bin -classpath $(CP) -sourcepath ../x10/src $<

bin/x10test/%.class: src/x10test/%.scala
	$(SCALAC) -d bin -classpath $(CP) $<

test1: compilert bin/x10test/Test1.class
	scala -cp $(CP) x10test.Test1

test2: compilert bin/x10test/Test2.class
	scala -cp $(CP) x10test.Test2

test3: compilert bin/x10test/Test3.class
	scala -cp $(CP) x10test.Test3

test4: compilert bin/x10test/Test4.class
	scala -cp $(CP) x10test.Test4

test5: compilert bin/x10test/Test5.class
	scala -cp $(CP) x10test.Test5

pra: compilert bin/x10test/ParRandomAccess1.class
	scala -cp $(CP) x10test.ParRandomAccess1

fra: compilert bin/x10test/FRASimpleDist.class
	scala -cp $(CP) x10test.FRASimpleDist

fs: compilert bin/x10test/FSSimpleDist.class
	scala -cp $(CP) x10test.FSSimpleDist

fib: compilert bin/x10test/Fib.class
	scala -cp $(CP) x10test.Fib 15

histogram: compilert bin/x10test/Histogram.class
	scala -cp $(CP) x10test.Histogram 100 5

pi: compilert bin/x10test/MontyPi.class
	scala -cp $(CP) x10test.MontyPi 100000

integrate: compilert bin/x10test/Integrate.class
	JAVA_OPTS="-Xmx1g -Xss1m" scala -cp $(CP) x10test.Integrate

sieve: compilert bin/x10test/Sieve.class
	scala -cp $(CP) x10test.Sieve

sieve2: compilert bin/x10test/Sieve2.class
	scala -cp $(CP) x10test.Sieve2

sum: compilert bin/x10test/ArraySum.class
	scala -cp $(CP) x10test.ArraySum

kmeans: compilert bin/x10test/KMeans.class
	scala -cp $(CP) x10test.KMeans

kmeans2: compilert bin/x10test/KMeansSPMD.class
	scala -cp $(CP) x10test.KMeansSPMD src/x10test/points.dat

nqueensj: compilert bin/x10test/NQueensJava.class
	scala -cp $(CP) x10test.NQueensJava 4
	scala -cp $(CP) x10test.NQueensJava 8
	scala -cp $(CP) x10test.NQueensJava 12

nqueens: compilert bin/x10test/NQueensPar.class
	scala -cp $(CP) x10test.NQueensPar 4
	scala -cp $(CP) x10test.NQueensPar 8
	scala -cp $(CP) x10test.NQueensPar 12

gcspheres: compilert bin/x10test/GCSpheres.class
	scala -cp $(CP) x10test.GCSpheres

stream: compilert bin/x10test/Stream.class
	scala -cp $(CP) x10test.Stream

bs: compilert bin/x10test/BlackScholes.class
	JAVA_OPTS="-Xmx1g -Xss1m" scala -cp $(CP) x10test.BlackScholes

clean:
	-rm -rf ../x10/bin/* bin/*

touch:
	-touch $(RT)

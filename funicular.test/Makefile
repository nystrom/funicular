RT = ../funicular/src/funicular/*.scala ../funicular/src/funicular/runtime/*.scala
RTC = $(patsubst %.scala,%.class,$(subst /src/,/bin/,$(RT)))
CP = bin:../funicular/bin:../funicular/lib/jsr166y.jar
SCALAC = scalac -optimise -Yinline -Yclosure-elim -Ydead-code

compilert: $(RTC)

rtc:
	scalac -optimise -d ../funicular/bin -sourcepath ../funicular/src -classpath bin:../funicular/bin:../funicular/lib/jsr166y.jar ../funicular/src/funicular/runtime/*.scala ../funicular/src/funicular/*.scala

../funicular/bin/funicular/%.class: ../funicular/src/funicular/%.scala
	$(SCALAC) -d ../funicular/bin -classpath $(CP) -sourcepath ../funicular/src $<

../funicular/bin/funicular/runtime/%.class: ../funicular/src/funicular/runtime/%.scala
	$(SCALAC) -d ../funicular/bin -classpath $(CP) -sourcepath ../funicular/src $<

bin/funicular.test/%.class: src/funicular.test/%.scala
	$(SCALAC) -d bin -classpath $(CP) $<

test1: compilert bin/funicular.test/Test1.class
	scala -cp $(CP) funicular.test.Test1

test2: compilert bin/funicular.test/Test2.class
	scala -cp $(CP) funicular.test.Test2

test3: compilert bin/funicular.test/Test3.class
	scala -cp $(CP) funicular.test.Test3

test4: compilert bin/funicular.test/Test4.class
	scala -cp $(CP) funicular.test.Test4

test5: compilert bin/funicular.test/Test5.class
	scala -cp $(CP) funicular.test.Test5

pra: compilert bin/funicular.test/ParRandomAccess1.class
	scala -cp $(CP) funicular.test.ParRandomAccess1

fra: compilert bin/funicular.test/FRASimpleDist.class
	scala -cp $(CP) funicular.test.FRASimpleDist

fs: compilert bin/funicular.test/FSSimpleDist.class
	scala -cp $(CP) funicular.test.FSSimpleDist

fib: compilert bin/funicular.test/Fib.class
	scala -cp $(CP) funicular.test.Fib 15

histogram: compilert bin/funicular.test/Histogram.class
	scala -cp $(CP) funicular.test.Histogram 100 5

pi: compilert bin/funicular.test/MontyPi.class
	scala -cp $(CP) funicular.test.MontyPi 100000

integrate: compilert bin/funicular.test/Integrate.class
	JAVA_OPTS="-Xmx1g -Xss1m" scala -cp $(CP) funicular.test.Integrate

sieve: compilert bin/funicular.test/Sieve.class
	scala -cp $(CP) funicular.test.Sieve

sieve2: compilert bin/funicular.test/Sieve2.class
	scala -cp $(CP) funicular.test.Sieve2

sum: compilert bin/funicular.test/ArraySum.class
	scala -cp $(CP) funicular.test.ArraySum

kmeans: compilert bin/funicular.test/KMeans.class
	scala -cp $(CP) funicular.test.KMeans

kmeans2: compilert bin/funicular.test/KMeansSPMD.class
	scala -cp $(CP) funicular.test.KMeansSPMD src/funicular.test/points.dat

nqueensj: compilert bin/funicular.test/NQueensJava.class
	scala -cp $(CP) funicular.test.NQueensJava 4
	scala -cp $(CP) funicular.test.NQueensJava 8
	scala -cp $(CP) funicular.test.NQueensJava 12

nqueens: compilert bin/funicular.test/NQueensPar.class
	scala -cp $(CP) funicular.test.NQueensPar 4
	scala -cp $(CP) funicular.test.NQueensPar 8
	scala -cp $(CP) funicular.test.NQueensPar 12

gcspheres: compilert bin/funicular.test/GCSpheres.class
	scala -cp $(CP) funicular.test.GCSpheres

stream: compilert bin/funicular.test/Stream.class
	scala -cp $(CP) funicular.test.Stream

bs: compilert bin/funicular.test/BlackScholes.class
	JAVA_OPTS="-Xmx1g -Xss1m" scala -cp $(CP) funicular.test.BlackScholes

clean:
	-rm -rf ../funicular/bin/* bin/*

touch:
	-touch $(RT)

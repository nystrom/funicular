RT = ../funicular/src/funicular/*.scala ../funicular/src/funicular/runtime/*.scala
RTC = $(patsubst %.scala,%.class,$(subst /src/,/bin/,$(RT)))
CP = bin:../funicular/bin:../funicular/lib/jsr166y.jar
SCALAC = fsc
#SCALAC = scalac -optimise -Yinline -Yclosure-elim -Ydead-code
SCALA = time scala
#SCALA = scala

compilert: $(RTC)

rtc:
	$(SCALAC) -d ../funicular/bin -sourcepath ../funicular/src -classpath bin:../funicular/bin:../funicular/lib/jsr166y.jar ../funicular/src/funicular/runtime/*.scala ../funicular/src/funicular/array/*.scala ../funicular/src/funicular/*.scala

../funicular/bin/funicular/%.class: ../funicular/src/funicular/%.scala
	$(SCALAC) -d ../funicular/bin -classpath $(CP) -sourcepath ../funicular/src $<

../funicular/bin/funicular/runtime/%.class: ../funicular/src/funicular/runtime/%.scala
	$(SCALAC) -d ../funicular/bin -classpath $(CP) -sourcepath ../funicular/src $<

bin/funicular/test/%.class: src/funicular/test/%.scala
	$(SCALAC) -d bin -classpath $(CP) $<

sums: compilert bin/funicular/test/Sums.class
	$(SCALA) -cp $(CP) funicular.test.Sums

adds: compilert bin/funicular/test/Adds.class
	$(SCALA) -cp $(CP) funicular.test.Adds

collatz: compilert bin/funicular/test/Collatz.class
	$(SCALA) -cp $(CP) funicular.test.Collatz 100

test1: compilert bin/funicular/test/Test1.class
	$(SCALA) -cp $(CP) funicular.test.Test1

test2: compilert bin/funicular/test/Test2.class
	$(SCALA) -cp $(CP) funicular.test.Test2

test3: compilert bin/funicular/test/Test3.class
	$(SCALA) -cp $(CP) funicular.test.Test3

test4: compilert bin/funicular/test/Test4.class
	$(SCALA) -cp $(CP) funicular.test.Test4

test5: compilert bin/funicular/test/Test5.class
	$(SCALA) -cp $(CP) funicular.test.Test5

test6: compilert bin/funicular/test/Test6.class
	$(SCALA) -cp $(CP) funicular.test.Test6

test7: compilert bin/funicular/test/Test7.class
	$(SCALA) -cp $(CP) funicular.test.Test7

test8: compilert bin/funicular/test/Test8.class
	$(SCALA) -cp $(CP) funicular.test.Test8

inc: compilert bin/funicular/test/Inc.class
	$(SCALA) -cp $(CP) funicular.test.Inc

pra: compilert bin/funicular/test/ParRandomAccess1.class
	$(SCALA) -cp $(CP) funicular.test.ParRandomAccess1

fra: compilert bin/funicular/test/FRASimpleDist.class
	$(SCALA) -cp $(CP) funicular.test.FRASimpleDist

fs: compilert bin/funicular/test/FSSimpleDist.class
	$(SCALA) -cp $(CP) funicular.test.FSSimpleDist

stencil: compilert bin/funicular/test/Stencil.class
	$(SCALA) -cp $(CP) funicular.test.Stencil

fib2: compilert bin/funicular/test/Fib2.class
	$(SCALA) -cp $(CP) funicular.test.Fib2 15
	$(SCALA) -cp $(CP) funicular.test.Fib2 1
	$(SCALA) -cp $(CP) funicular.test.Fib2 2
	$(SCALA) -cp $(CP) funicular.test.Fib2 3
	$(SCALA) -cp $(CP) funicular.test.Fib2 4
	$(SCALA) -cp $(CP) funicular.test.Fib2 5

fib: compilert bin/funicular/test/Fib.class
	$(SCALA) -cp $(CP) funicular.test.Fib 15
	$(SCALA) -cp $(CP) funicular.test.Fib 1
	$(SCALA) -cp $(CP) funicular.test.Fib 2
	$(SCALA) -cp $(CP) funicular.test.Fib 3
	$(SCALA) -cp $(CP) funicular.test.Fib 4
	$(SCALA) -cp $(CP) funicular.test.Fib 5

histogram: compilert bin/funicular/test/Histogram.class
	$(SCALA) -cp $(CP) funicular.test.Histogram 100 5

pi: compilert bin/funicular/test/MontyPi.class
	$(SCALA) -cp $(CP) funicular.test.MontyPi 100000

integrate: compilert bin/funicular/test/Integrate.class
	JAVA_OPTS="-Xmx1g -Xss1m" $(SCALA) -cp $(CP) funicular.test.Integrate

sieve: compilert bin/funicular/test/Sieve.class
	$(SCALA) -cp $(CP) funicular.test.Sieve

sieve2: compilert bin/funicular/test/Sieve2.class
	$(SCALA) -cp $(CP) funicular.test.Sieve2

sum: compilert bin/funicular/test/ArraySum.class
	$(SCALA) -cp $(CP) funicular.test.ArraySum

kmeans: compilert bin/funicular/test/KMeans.class
	$(SCALA) -cp $(CP) funicular.test.KMeans

kmeans2: compilert bin/funicular/test/KMeansSPMD.class
	$(SCALA) -cp $(CP) funicular.test.KMeansSPMD src/funicular/test/points.dat

nqueensj: compilert bin/funicular/test/NQueensJava.class
	$(SCALA) -cp $(CP) funicular.test.NQueensJava 4
	$(SCALA) -cp $(CP) funicular.test.NQueensJava 8
	$(SCALA) -cp $(CP) funicular.test.NQueensJava 12

nqueens: compilert bin/funicular/test/NQueensPar.class
	$(SCALA) -cp $(CP) funicular.test.NQueensPar 4
	$(SCALA) -cp $(CP) funicular.test.NQueensPar 8
	$(SCALA) -cp $(CP) funicular.test.NQueensPar 12

deadlock: compilert bin/funicular/test/Deadlock.class
	$(SCALA) -cp $(CP) funicular.test.Deadlock

gcspheres: compilert bin/funicular/test/GCSpheres.class
	$(SCALA) -cp $(CP) funicular.test.GCSpheres

stream: compilert bin/funicular/test/Stream.class
	$(SCALA) -cp $(CP) funicular.test.Stream

bs: compilert bin/funicular/test/BlackScholes.class
	JAVA_OPTS="-Xmx1g -Xss1m" $(SCALA) -cp $(CP) funicular.test.BlackScholes

dfs: compilert bin/funicular/test/DFS.class
	JAVA_OPTS="-Xmx1g -Xss1m" $(SCALA) -cp $(CP) funicular.test.DFS

bfs: compilert bin/funicular/test/BFS.class
	JAVA_OPTS="-Xmx1g -Xss1m" $(SCALA) -cp $(CP) funicular.test.BFS

clean:
	-rm -rf ../funicular/bin/* bin/*

touch:
	-touch $(RT)

all:
	fsc -encoding UTF-8 -feature LibFx.scala
	fsc -encoding UTF-8 -feature icpcrank.scala

clean:
	rm *.class

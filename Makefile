all: LibFx icpcrank

LibFx: LibFx.scala
	fsc -encoding UTF-8 -feature LibFx.scala

icpcrank: icpcrank.scala
	fsc -encoding UTF-8 -feature icpcrank.scala

clean:
	rm *.class

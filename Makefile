all: LibFx icpcrank log

LibFx: LibFx.scala
	fsc -encoding UTF-8 -feature LibFx.scala

icpcrank: icpcrank.scala
	fsc -encoding UTF-8 -feature icpcrank.scala

log:
	mkdir -p log

clean:
	rm *.class

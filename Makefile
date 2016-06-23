all:
	fsc -encoding UTF-8 -feature -cp LibFx.scala
	fsc -encoding UTF-8 -feature -cp icpcrank.scala

clean:
	rm *.class

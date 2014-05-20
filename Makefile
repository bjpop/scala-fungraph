# make this point to your scala compiler
SCALAC = ~/code/scala-2.10.4/bin/scalac

Main.class: fun_graph.scala
	$(SCALAC) fun_graph.scala

.phony: clean

clean:
	rm -fr *.class

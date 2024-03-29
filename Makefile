SCALA_CLI := scala-cli
SCALA_VER := 3.3.1


define SCALA_DEPS =
//> using scala ${SCALA_VER}
//> using repository sonatype:snapshots
//> using dep org.typelevel::cats-core:2.10.0
//> using dep org.typelevel::cats-effect:3.5.2
endef

export SCALA_DEPS


aoc.jar: dependencies.scala
	${SCALA_CLI} --power package . -o $@ --assembly


aoc: dependencies.scala
	${SCALA_CLI} --power package . -o $@ --native-image \
		-- --gc=G1 --no-fallback --static -march=native -O2


dependencies.scala: Makefile
	echo "$$SCALA_DEPS" > $@


clean:
	rm -f dependencies.scala aoc aoc.jar *.build_artifacts.txt


clean-cache: clean
	rm -rf .bsp .scala-build
	${SCALA_CLI} clean .


native: aoc
	strip -s aoc
	[ -x "$(shell command -v upx)" ] && upx --best --lzma aoc


repl: dependencies.scala
	${SCALA_CLI} repl .

.PHONY: clean clean-cache native repl

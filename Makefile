all: aoc.jar

aoc: aoc.jar
	${GRAALVM_HOME}/bin/native-image \
	--allow-incomplete-classpath --no-fallback \
	--initialize-at-build-time \
	-jar aoc.jar

aoc.jar: target
	cp $(shell find target/ | grep aoc.jar) .

clean:
	rm -fr aoc aoc.jar aoc.build_artifacts.txt project/target target

target:
	sbt assembly

native: aoc
	strip -s aoc
	[ -x "$(command -v upx)" ] && upx aoc

.PHONY: all clean native

all: aoc_2020.jar

aoc_2020: aoc_2020.jar
	${GRAALVM_HOME}/bin/native-image \
	--allow-incomplete-classpath --no-server --no-fallback --initialize-at-build-time --static \
	-jar aoc_2020.jar

aoc_2020.jar: target
	cp $(shell find target/ | grep aoc_2020.jar) .

clean:
	rm -fr aoc_2020 aoc_2020.jar project/target target

target:
	sbt assembly

native: aoc_2020
	strip -s aoc_2020

.PHONY: all clean native

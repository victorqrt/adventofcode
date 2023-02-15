aoc.jar: target
	cp $(shell find target/ | grep aoc.jar) .

aoc: aoc.jar
	${GRAALVM_HOME}/bin/native-image \
	--allow-incomplete-classpath --no-fallback \
	--initialize-at-build-time \
	-jar aoc.jar

clean:
	rm -fr aoc aoc.jar aoc.build_artifacts.txt project/target target

target:
	sbt assembly

native: aoc
	strip -s aoc
	[ -x "$(shell command -v upx)" ] && upx aoc

re: clean aoc.jar 

.PHONY: clean native re

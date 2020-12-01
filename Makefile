all: aoc_2020.jar
	cp $(shell find target/ | grep aoc_2020.jar) .

aoc_2020.jar: target
	sbt assembly

clean:
	rm -fr aoc_2020.jar project/target target

target:

.PHONY: all clean

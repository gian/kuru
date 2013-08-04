LIB=../../lib

LIBKURU=$(LIB)/kuru.jar

all: $(LIBKURU)

$(LIBKURU): kuru/Kuru.class
	jar cf $(LIBKURU) kuru

kuru/Kuru.class: kuru/Kuru.java
	javac kuru/Kuru.java

clean:
	rm -f $(LIBKURU) kuru/*.class


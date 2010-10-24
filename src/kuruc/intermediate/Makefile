all: temp.sml intermediate.sml translate.sml 

temp.sml: temp.k
	cp temp.k temp.sml

intermediate.sml: intermediate.k
	cp intermediate.k intermediate.sml

translate.sml: translate.k
	cp translate.k translate.sml

clean:
	rm -f temp.sml intermediate.sml translate.sml 

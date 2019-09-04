
SML = sml
CAPFILE = lastTry

try: clean
	echo "CM.make'(\"../sources.cm\");CM.make();use(\"predicates.sml\");" | $(SML) 2>&1 | tee $(CAPFILE)| less

runpredicates: clean
	echo "use(\"predicates.sml\");" | $(SML)| tee $(CAPFILE) | less 

predicates: clean
	echo "use(\"predicates.sml\");" | $(SML) 2>&1 | grep -v '^GC' | tee $(CAPFILE) | less 

test: clean
	echo "CM.make();" | $(SML) 2>&1 | tee $(CAPFILE)| less

again:
	cat $(CAPFILE) | less

clean: 
	-rm -f /tmp/vim*.err


HS = ghc
HSFLAGS = -O2 --make

EXENAME = euler


all :
	$(HS) $(HSFLAGS) -threaded Main.hs -o $(EXENAME)

run : all
	./$(EXENAME) +RTS -N4

prof :
	$(HS) $(HSFLAGS) -prof -auto-all Main.hs -o $(EXENAME)
	./euler +RTS -P
	less euler.prof

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete

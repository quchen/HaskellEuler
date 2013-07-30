HS = ghc
HSFLAGS = -O2 --make $(INCLUDE)
INCLUDE = -i:$(SRC)

EXENAME = euler
SRC = src/


all :
	$(HS) $(HSFLAGS) -threaded $(SRC)Main.hs -o $(EXENAME)

run : all
	./$(EXENAME) +RTS -N4

wall :
	$(HS) $(INCLUDE) -Wall -fno-warn-type-defaults $(SRC)Main.hs -o $(EXENAME)
	make clean


prof :
	$(HS) $(HSFLAGS) -prof -auto-all $(SRC)Main.hs -o $(EXENAME)
	./euler +RTS -P
	less euler.prof

clean :
	rm -f $(EXENAME)
	find $(SRC) -iname "*.prof" -delete
	find $(SRC) -iname "*.eventlog" -delete
	find $(SRC) -iname "*.hi" -delete
	find $(SRC) -iname "*.o" -delete

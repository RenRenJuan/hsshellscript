CFLAGS=-XDeriveDataTypeable 

PROG_HS  = $(notdir $(basename $(wildcard [_abcdefghijklmnopqrstuvwxyzäöü]*.hs)))
PROG_CHS = $(notdir $(basename $(wildcard [_abcdefghijklmnopqrstuvwxyzäöü]*.chs)))

# Löscht Doppelgänger
PROGS    = $(sort $(PROG_HS) $(PROG_CHS))


% : %.o cteile.o
	ghc -dynamic -o $@ $^ -package hsshellscript

# % : %.O cteile.o
# 	ghc -dynamic -o $@ $^ -package unix -package directory -package random -package parsec -package hsshellscript -XScopedTypeVariables

%.o : %.hs
	ghc -dynamic -c $(CFLAGS) $^ -XScopedTypeVariables

# %.o : %.hs
# 	ghc -c $(CFLAGS) $^ -XScopedTypeVariables

# %.o : %.c

cteile.o : cteile.c
	gcc -fPIC -c -o $@ $<

%.hs : %.chs
	-chmod u+w $@
	c2hs -o $@ $<
	chmod u-w $@

clean :: depend
	rm -f -- *.hi *.o *~ *.bak $(PROGS) *.chi *.chs.h \
	      $(foreach m, $(PROG_CHS), "$(m).hs")

depend :: $(foreach m, $(PROGS), $(m).hs)
	ghc -M $(CFLAGS) -dep-makefile depend -dep-suffix "p_" \
	       $(foreach m, $(PROGS), "$(m).hs")
#	make cteile.o

#z : z.c
#	gcc -o $@ $<

-include depend

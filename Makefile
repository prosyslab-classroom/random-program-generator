MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm
EXE=generator

all:
	$(DUNE) build src/main.exe
	$(LN) _build/default/src/main.exe $(EXE)

test: all
	$(DUNE) test

clean:
	$(MAKE) -C test clean
	$(DUNE) clean
	$(RM) -rf $(EXE)

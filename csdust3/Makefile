PROG:=../csdust3.bin
GFORT:=gfortran
FFLAGS=-w

$(PROG) : csdust3_linux.f
	$(GFORT) $(FFLAGS) -o $@ $^

clean:
	$(RM) $(PROG)

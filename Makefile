all: dungeon

dungeon:
	gfortran -c player.f95
	gfortran -c trap.f95
	gfortran world.f95 player.o trap.o -o dungeon

clean:
	rm -f dungeon; rm -f *.o; rm -f *.mod; rm *~

all: dungeon

dungeon:
	gfortran -c player.f95
	gfortran -c mob.f95
	gfortran -c dungeon_floor.f95
	gfortran -c spells.f95
	gfortran -c trap.f95
	gfortran -c treasure.f95
	gfortran -c world.f95
	gfortran world.o player.o mob.o trap.o spells.o treasure.o dungeon_floor.o -o dungeon
	
clean:
	rm -f dungeon; rm -f *.o; rm -f *.mod; rm -f *~

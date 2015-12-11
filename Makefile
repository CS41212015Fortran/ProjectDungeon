all: dungeon

dungeon: world.f95; gfortran -o dungeon world.f95 player.f95 trap.f95 dungeon_floor.f95

clean:; rm -f dungeon; *.mod;

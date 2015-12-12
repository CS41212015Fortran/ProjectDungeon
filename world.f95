program world
	use classTrap
	use class_dungeon_floor
	use classPlayer

	!Need to use this line for every program
	implicit none

	!declaring type variables
	type(player) 				:: p
	type(Trap)   				:: t = Trap("Bear-trap", 1)
	type(dungeon_floor) :: d

	!init player
	p%name= "Son Goku"
	p%strength=1
	p%intelegence=1
	p%moxie=1
	p%xp=0
	p%score=0
	call update_derived_stats(p)
	call print_stats(p)
	call update_player_stats(p,'s',4)
	call update_player_stats(p,'i',4)
	call update_player_stats(p,'m',4)

	call print_stats(p)
	call trapPrint(t)
	call triggerTrap(t)
	call effectPlayer(t, p)
	call trapPrint(t)
	call print_stats(p)

	call make_new_room(d, .true.)
	call go_east(d)

end program world

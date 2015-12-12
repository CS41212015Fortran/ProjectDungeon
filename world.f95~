program world
	use classTrap
	use classTreasure
	use class_dungeon_floor
	use classPlayer

	!Need to use this line for every program
	implicit none

	!declaring type variables
	type(Trap) 					:: t = Trap("bear-trap", 1)
	type(Treasure)			:: c
	type(player) 				:: p
	type(dungeon_floor) :: d

	!init player
	p%name="Son Goku"
	p%strength=1
	p%intelegence=1
	p%moxie=1
	p%xp=0
	p%keys=1
	p%score=0
	call update_derived_stats(p)
	!call update_player_stats(p,'s',4)
	!call update_player_stats(p,'i',4)
	!call update_player_stats(p,'m',4)
			
	call print_stats(p)
	call make_new_room(d, .true.)
	call go_east(d)
	
	! If the room contains a trap
  if (d%has_trap) then
  	print*, "There is a trap in this room."
  	t = Trap("mine", 1)
  	call triggerTrap(t)
  	call effectPlayer(t, p)
  end if
  
  !If the room contains a treasure chest
  if (d%has_treasure) then
  	print*, "There is a locked treasure chest in this room."
  	call unlockChest(c, p)
  end if
  
  !If the room contains a secret room
  if (d%has_secret) then
  	print*, "There is a secret room somewhere here."
  end if

end program world

program world
	use classTrap
	use class_player
	use class_dungeon_floor
	!Need to use this line for every program
	implicit none

	!declaring type variables
	type(player) 				:: p
	type(Trap)   				:: t = Trap("Bear-trap", 1)
	type(dungeon_floor) :: d

	!accessing the components of the structure
	p%name= "Son Goku"
	p%strength=1
	p%intelegence=1
	p%moxie=1
	p%hp=80 + p%strength*5
	p%mana=60 + p%intelegence*10
	p%xp=0
	p%score=0

	!display info
	Print  "(a10)",adjustl(p%name)
	Print  "(a14,i2)",'STRENGTH    = ',p%strength
	Print  "(a14,i2)",'INTELEGENCE = ',p%intelegence
	Print  "(a14,i2)",'MOXIE       = ',p%moxie
	Print  "(a14,i2)",'HP          = ',p%hp
	Print  "(a14,i2)",'MANA        = ',p%mana
	Print  "(a14,i2)",'XP          = ',p%xp
	Print  "(a14,i2)",'SCORE       = ',p%score

	!display info
	Print  "(a10)",adjustl(p%name)
	Print  "(a14,i2)",'STRENGTH    = ',p%strength
	Print  "(a14,i2)",'INTELEGENCE = ',p%intelegence
	Print  "(a14,i2)",'MOXIE       = ',p%moxie
	Print  "(a14,i2)",'HP          = ',p%hp
	Print  "(a14,i2)",'MANA        = ',p%mana
	Print  "(a14,i2)",'XP          = ',p%xp
	Print  "(a14,i2)",'SCORE       = ',p%score

	call make_new_room(d, .true.)
	call go_east(d)

end program world

program world
	use classTrap
	use classPlayer
	!Need to use this line for every program
	implicit none	
	
	!declaring type variables
	type(Player) :: p
	type(Trap)   :: t = Trap("Bear-trap", 1)

	!accessing the components of the structure
	p%name= "Son Goku"
	p%strength=1
	p%intelegence=1
	p%moxie=1
	p%hp=10
	p%mana=1
	p%xp=0
	p%score=0

	p%strength=p%strength+1
	
	!display info
	Print  "(a10)",adjustl(p%name)
	Print  "(a14,i2)",'STRENGTH    = ',p%strength
	Print  "(a14,i2)",'INTELEGENCE = ',p%intelegence
	Print  "(a14,i2)",'MOXIE       = ',p%moxie
	Print  "(a14,i2)",'HP          = ',p%hp
	Print  "(a14,i2)",'MANA        = ',p%mana
	Print  "(a14,i2)",'XP          = ',p%xp
	Print  "(a14,i2)",'SCORE       = ',p%score
	
	call trapPrint(t)
	call triggerTrap(t)
	call effectPlayer(t, p)
	call trapPrint(t)

	!display info
	Print  "(a10)",adjustl(p%name)
	Print  "(a14,i2)",'STRENGTH    = ',p%strength
	Print  "(a14,i2)",'INTELEGENCE = ',p%intelegence
	Print  "(a14,i2)",'MOXIE       = ',p%moxie
	Print  "(a14,i2)",'HP          = ',p%hp
	Print  "(a14,i2)",'MANA        = ',p%mana
	Print  "(a14,i2)",'XP          = ',p%xp
	Print  "(a14,i2)",'SCORE       = ',p%score
		 
end program world

program dungeon

	   !type declaration
	   type Player
	      character(len=50) :: name
				integer :: strength
				integer :: intelegence
				integer :: moxie
				integer :: hp
				integer :: mana
				integer :: xp
				integer :: score
	   end type Player

	   !declaring type variables
	   type(Player) :: p

	   !accessing the components of the structure
	   p%name= "Son Goku"
		 p%strength=1
		 p%intelegence=1
		 p%moxie=1
		 p%hp=1
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

end program dungeon

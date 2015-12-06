program dungeon

	   !type declaration
	   type Player
	      character(len=50) :: name
				integer :: strength
				integer :: intelegence
				integer :: moxie
				integer :: hp
				integer :: mana
	   end type Player

	   !declaring type variables
	   type(Player) :: player

	   !accessing the components of the structure
	   player%name= "Son Goku"
		 player%strength=1
		 player%intelegence=1
		 player%moxie=1
		 player%hp=1
		 player%mana=1


		 Print *, player%name
		 Print *, player%strength
		 Print *, player%intelegence
		 Print *, player%moxie
		 Print *, player%hp
		 Print *, player%mana

end program dungeon

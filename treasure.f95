module classTreasure
	implicit none
	private
	public :: Treasure, unlockChest, treasurePrint
		
	type Treasure 
		logical :: locked									!Whether or not the treasure chest is locked
	end type Treasure
	
! Set up the methods for the trap module.
contains
	
	! This function gets called when a player triggers a trap
	subroutine unlockChest(this, plr)
		use class_player  
		implicit none
		type(Treasure) :: this
		type(Player) :: plr
		
		if (this%locked .eqv. .true.) then
			if (plr%
			this%locked = .false.
		else
			print*, "This chest has already been opened"
		end if
	end subroutine unlockChest
	
	subroutine treasurePrint(this)
		implicit none
    type(Treasure), intent(in) :: this
  end subroutine treasurePrint
end module classTreasure


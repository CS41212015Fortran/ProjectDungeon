module classTreasure
	implicit none
	private
	public :: Treasure, unlockChest, treasurePrint
		
	type Treasure 
		logical :: locked = .true.									!Whether or not the treasure chest is locked
	end type Treasure
	
! Set up the methods for the trap module.
contains
	
	! This function gets called when a player triggers a trap
	subroutine unlockChest(this, plr)
		use classPlayer  
		implicit none
		type(Treasure) :: this
		type(Player) :: plr
		
		if (this%locked .eqv. .true.) then
			if (has_key(plr)) then
				print*, "You use one of your keys to open the chest."
				this%locked = .false.
			else
				print*, "You don't have any keys to open this chest."
			end if
		else
			print*, "This chest has already been opened!"
		end if
	end subroutine unlockChest
	
	subroutine treasurePrint(this)
		implicit none
    type(Treasure), intent(in) :: this
  end subroutine treasurePrint
end module classTreasure


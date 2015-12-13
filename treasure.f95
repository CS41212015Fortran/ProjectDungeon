module classTreasure
	implicit none
	private
	public :: Treasure, unlockChest
		
	type Treasure 
		logical :: locked = .true.									!Whether or not the treasure chest is locked
	end type Treasure
	
! Set up the methods for the trap module.
contains
	
	! This function gets called when a player triggers a trap
	subroutine unlockChest(this, plr, dungeon)
		use classPlayer
		use class_dungeon_floor
		implicit none
		type(Treasure) :: this
		type(Player) :: plr	
		type(dungeon_floor) :: dungeon
		integer :: i, n, clock, itype, num
    integer, dimension(:), allocatable :: seed
    real    :: rrand
    
    !set up random
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
		
		if (this%locked .eqv. .true.) then
			if (has_key(plr)) then
				print*, "You use one of your keys to open the chest."
				this%locked = .false.
				call RANDOM_NUMBER(rrand)
				
				itype = floor(rrand * 3)+1
				
				select case (itype)
				case (1)
					call RANDOM_NUMBER(rrand)
					num = floor(rrand * dungeon%floor_number)+1
					print*, "You find ", num, " health potions in the chest!" 
					plr%hp_pots = plr%hp_pots + num
				case (2)
					call RANDOM_NUMBER(rrand)
					num = floor(rrand * dungeon%floor_number)+1
					print*, "You find ", num, " mana potions in the chest!"
					plr%mp_pots = plr%mp_pots + num
				case (3)
					plr%keys = plr%keys + 1
				end select
			else
				print*, "You don't have any keys to open this chest."
			end if
		else
			print*, "This chest has already been opened!"
		end if
	end subroutine unlockChest
end module classTreasure


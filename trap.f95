module classTrap
	implicit none
	private
	public :: Trap, getTrapName, getTrapType, triggerTrap, effectPlayer, trapPrint

	type Trap
		character (len=40) :: trap_name  	!The name of the trap
		integer :: trap_type	 						!The trap type: 1 - damage, 2 - displace, 3 - impair
		logical :: triggered = .false.		!Whether or not the trap has been triggered
	end type Trap

! Set up the methods for the trap module.
contains	
	! Return the name of the trap.
	function getTrapName(this) result (trap_name)
		implicit none
		type(Trap), intent(in) :: this
		character (len=40) :: trap_name

		! Return the name of the trap that was passed in
		trap_name = this%trap_name
	end function getTrapName

	! Return the type of the trap.
	function getTrapType(this) result (trap_type)
		implicit none
		type(Trap), intent(in) :: this
		integer :: trap_type

		! Return the type of the trap
		trap_type = this%trap_type
	end function getTrapType

	! This function gets called when a player triggers a trap
	subroutine triggerTrap(this)
		implicit none
		type(Trap) :: this

		if (this%triggered .eqv. .false.) then
			this%triggered = .true.
		else
			print*, "Trap has already been triggered"
		end if
	end subroutine triggerTrap

	subroutine effectPlayer(this, plr, dungeon)
		use class_dungeon_floor
		use classPlayer
		implicit none
		
		type(Trap), intent(in) :: this
		type(Player) :: plr
		type(dungeon_floor) :: dungeon
		integer :: i, n, clock, irand, damage
    integer, dimension(:), allocatable :: seed
    real    :: rrand
		
		!set up random
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
    
    call RANDOM_NUMBER(rrand)
    irand = rrand / 5

		! Check if the player dodges the trap.
		if (plr%dodge_chance > (irand + (dungeon%floor_number/20))) then
			print*, " Due to your deftly abilities, you succesfully dodge the ", trim(this%trap_name), "!"
		else		
			if (this%trap_type .eq. 1) then			
				else 
					! calculate the damage
					call RANDOM_NUMBER(rrand)
					damage = floor(rrand * (plr%hp / 3)) + 1
				
					Print  "(a5,a,a22,i3,a7)",' The ', trim(this%trap_name), ' injures you. You take ', damage, ' damage.'
				
					! damage the player up to a third of their health
					plr%hp = plr%hp - damage
				end if
			else if (this%trap_type .eq. 2) then
				! if else if condition is true
				print*, "You accidentally trigger trap. After a bright flash you find yourself in an unfamiliar room."
				! generate a new room
				make_new_room(dungeon, .false.)
			else
				! if none of the conditions are true
				print*, "You stumble upon a dud trap. Whew, consider this your lucky day."
			end if
		end if
	end subroutine effectPlayer

	subroutine trapPrint(this)
		implicit none
    type(Trap), intent(in) :: this
    print *, 'Trap: name = ', trim(this%trap_name), ' type = ', this%trap_type
  end subroutine trapPrint
end module classTrap

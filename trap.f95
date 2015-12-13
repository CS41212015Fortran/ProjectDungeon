module classTrap
  use class_dungeon_floor
  
	implicit none
	private
	public :: Trap, getTrapName, getTrapType, disarmTrap, checkForTrap, triggerTrap, effectPlayer, trapPrint

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

	! Called when the player attemps to disarm the trap
	subroutine disarmTrap(this, plr, dungeon)
		use classPlayer
		implicit none
		type(Trap) :: this
		type(Player) :: plr
		type(dungeon_floor) :: dungeon
		integer :: i, n, clock, irand
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

		if (dungeon%has_trap .eqv. .true.) then
			if (this%triggered .eqv. .false.) then
				if (plr%disarm_chance > (irand + (dungeon%floor_number/20))) then
					print*, "You've disarmed the ", trim(this%trap_name), " succesfully!"
					this%triggered = .true.
				else
					print*, "You've failed to disarm the ", trim(this%trap_name), ". It backfires."
					this%triggered = .true.
					call effectPlayer(this, plr, dungeon)
				end if
			else
				print*, "Trap has already been triggered. You can't disarm it!"
			end if
		else 
			print*, "This room doesn't contain a trap!"
		end if
	end subroutine disarmTrap

	subroutine checkForTrap(this, plr, dungeon) 
		use classPlayer
		implicit none
		type(Trap) :: this
		type(Player) :: plr
		type(dungeon_floor) :: dungeon
		
		if (plr%perception_chance > (dungeon%floor_number/20)) then
			if (dungeon%has_trap) then
				print*, "You find a hidden trap ahead of you."
			else
				print*, "After a thorough search, you are certain that this room doesn't contain any traps."
			end if
		else 
			print*, "It's hard to tell if this room has a trap or not."
		end if
	end subroutine checkForTrap

	! This function gets called when a player triggers a trap
	subroutine triggerTrap(this)
		use classPlayer
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
		
		type(Trap) :: this
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
		if (this%triggered .eqv. .false.) then
			this%triggered = .true.
			if (plr%dodge_chance > (irand + (dungeon%floor_number/20))) then
				print*, " Due to your deftly abilities, you succesfully dodge the ", trim(this%trap_name), "!"
			else 
				if (this%trap_type .eq. 1) then			
					! calculate the damage
					call RANDOM_NUMBER(rrand)
					damage = floor(rrand * (plr%hp / 3)) + 1
				
					Print  "(a5,a,a22,i3,a7)",' The ', trim(this%trap_name), ' injures you. You take ', damage, ' damage.'
				
					! damage the player up to a third of their health
					plr%hp = plr%hp - damage
				else if (this%trap_type .eq. 2) then
					! if else if condition is true
					print*, "You accidentally trigger trap. After a bright flash you find yourself in an unfamiliar room."
					! generate a new room
					call make_new_room(dungeon, .false.)
				else
					! if none of the conditions are true
					print*, "You stumble upon a dud trap. Whew, consider this your lucky day."
				end if
			end if
		end if
	end subroutine effectPlayer

	subroutine trapPrint(this)
		implicit none
    type(Trap), intent(in) :: this
    print *, 'Trap: name = ', trim(this%trap_name)
  end subroutine trapPrint
end module classTrap

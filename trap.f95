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

	subroutine effectPlayer(this, plr)
		use classPlayer

		implicit none
		type(Trap), intent(in) :: this
		type(Player) :: plr
		integer :: seed

		if (this%trap_type .eq. 1) then
			! if condition is true then print the following
			print*, "The bear-trap catches your foot and injures you."

      call srand(seed)
      print*, int(rand()*4)+1, int(rand()*4)+1, int(rand()*4)+1, int(rand()*4)+1

			plr%hp = plr%hp - 2

		else if (this%trap_type .eq. 2) then
			! if else if condition is true
			print*, "This is a displacement trap."
			! Re-generate a new room.
		else if(this%trap_type .eq. 3) then
			! if else if condition is true
			print*, "This is an impair trap."
		else
			! if none of the conditions is true
			print*, "You stumble upon a dud trap. Whew, consider this your lucky day."
		end if
	end subroutine effectPlayer

	subroutine trapPrint(this)
		implicit none
    type(Trap), intent(in) :: this
    print *, 'Trap: name = ', trim(this%trap_name), ' type = ', this%trap_type
  end subroutine trapPrint
end module classTrap

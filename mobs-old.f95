module class_Mob
	implicit none
	
	private
	public :: Mob, Mob_print, Mob_name, attack

	type Mob
		character (len=20) :: name
		integer :: health
		integer :: strength
	end type Mob

	contains

	subroutine Mob_print(this)
		type(Mob), intent(in) :: this
		print *, this%name, ' has ', this%health, ' points of health.'
	end subroutine Mob_print

	subroutine Mob_name(this)
		type(Mob), intent(in) :: this
		print *, 'Mob name is ', this%name
	end subroutine Mob_name


	subroutine attack(att, def)
		type(Mob), intent(in) :: att
		type(Mob), intent(inout) :: def

	!	print *, att%name, ' attacked ', def%name, ' for ', att%strength, ' damage'

		print "(a10 a12 a10 a5 i2 a10)", att%name, 'attacked', def%name, 'for', att%strength, 'damage'
		
		def%health = def%health - att%strength
		
	end subroutine attack
		



end module class_Mob



program mob_test
	use class_Mob
	implicit none

	type(Mob) :: e 		! create a mob
	type(mob) :: p

	e = Mob('Ureel', 20, 10)
	call Mob_name(e)

	p = Mob('Kevin', 20, 10)
	call Mob_name(p)

	call attack(p, e)

	call mob_print(p)
	call Mob_print(e)
	
end program mob_test
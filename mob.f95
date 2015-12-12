module class_mob
  implicit none
  
	private
	public :: Mob, new_mob, get_info, attack

	type Mob
		character (len=20) :: name
		integer :: health
		integer :: strength
	end type Mob

	contains

  subroutine new_mob(this, dungeon_floor)
    implicit none
    type(mob)           :: this
    integer             :: dungeon_floor
    integer :: i, n, clock, irand
    integer, dimension(:), allocatable :: seed
    real    :: rrand, dungeon_scale
    dungeon_scale = (dungeon_floor / 20)
    
    
    !set up our random stuff
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
    
    call RANDOM_NUMBER(rrand)
    irand = floor(rrand * 10)+1
    
    !randomly generate a name
    if (irand == 10) then
    	this%name = "Mummy"
    else if (irand == 9) then
    	this%name = "Giant Spider"
    else if (irand == 8) then
    	this%name = "Witch"
    else if (irand == 7) then
    	this%name = "Gargoyle"
    else if (irand == 6) then
    	this%name = "Imp"
    else if (irand == 5) then
      this%name = "Skeleton"
    else if (irand == 4) then
      this%name = "Wraith"
    else if (irand == 3) then
      this%name = "Shade"
    else if (irand == 2) then
      this%name = "Zombie"
    else 
      this%name = "Slime"
    end if
    
    call RANDOM_NUMBER(rrand)
    !health
    irand = ceiling(rrand * (100 * dungeon_scale)) + (5 * dungeon_scale)
    this%health = irand
    
    call RANDOM_NUMBER(rrand)
    !strength
    irand = ceiling(rrand * (100 * dungeon_scale)) + (5 * dungeon_scale)
    this%strength = irand
    
  end subroutine new_mob
  
	subroutine get_info(this)
		type(Mob), intent(in) :: this
		!print *, 'Mob name is ', this%name
		print *, this%name, ' has ', this%health, ' HP.'
	end subroutine get_info


	subroutine attack(att, def)
		type(Mob), intent(in) :: att
		type(Mob), intent(inout) :: def

		print *, att%name, ' attacked ', def%name, ' for ', att%strength, ' damage'
		def%health = def%health - att%strength
  end subroutine attack
    
end module class_Mob

!program mob_test
!	use class_Mob
!	implicit none

!	type(Mob) :: e 		! create a mob
!	type(Mob) :: p

!	e = Mob('Ureel', 20, 10)
!	call Mob_print(e)

!	p = Mob('Kevin', 20, 10)
!	call attack(p, e)

!	call Mob_print(e)

!end program mob_test

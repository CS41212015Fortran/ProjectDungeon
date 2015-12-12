module class_Mob
	implicit none

	private
	public :: Mob, Mob_print, attack

	type Mob
		character (len=20) :: name
		integer :: health
		integer :: strength
	end type Mob

	contains

  subroutine new_mob(this, d)
    implicit none
    type(mob)           :: this
    type(dungeon_floor) :: d
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    real    :: rrand
    integer :: irand
    real    :: dungeon_scale
    dungeon_scale = (d%get_floor_number / 20)
    
    
    !set up our random stuff
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
    
    call RANDOM_NUMBER(rrand)
    irand = floor(rrand * 5)+1
    
    !randomly generate a name
    if (irand == 5) then
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
    irand ceiling(rran * (100 * dungeon_scale)) + (5 * dungeon_scale)
    this%health = irand
    
    call RANDOM_NUMBER(rrand)
    !strength
    irand ceiling(rran * (100 * dungeon_scale)) + (5 * dungeon_scale)
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

module class_mob
  implicit none
  
	private
	public :: mob, new_mob, get_info

	type mob
		character (len=20) :: name
		integer :: health
		integer :: strength
		real    :: dodge_chance
	end type mob

	contains

  subroutine new_mob(this, dungeon_floor)
    implicit none
    type(mob)           :: this
    integer             :: dungeon_floor
    integer :: i, n, clock, irand
    integer, dimension(:), allocatable :: seed
    real    :: rrand, dungeon_scale
    dungeon_scale = (REAL(dungeon_floor) / 20)
    
    
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
    irand = ceiling(rrand * (100 * dungeon_scale)) + (5 * dungeon_scale) + 1
    this%health = irand
    
    call RANDOM_NUMBER(rrand)
    !strength
    irand = ceiling(rrand * (100 * dungeon_scale)) + (5 * dungeon_scale) + 1
    this%strength = irand

	call RANDOM_NUMBER(rrand)
	!dodge
	if(rrand > .5) then
		rrand = .5
	end if
	this%dodge_chance = rrand


  end subroutine new_mob
  
	subroutine get_info(this)
		type(Mob), intent(in) :: this
		!print *, 'Mob name is ', this%name
		print *, this%name, ' has ', this%health, ' HP.'
		print *, this%name, ' has ', this%strength, ' strength.'
	end subroutine get_info
    
end module class_Mob

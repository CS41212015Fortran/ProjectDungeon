module class_dungeon_floor
  use class_mob

	implicit none
  private
  public :: dungeon_floor, make_new_room, get_floor_number, get_is_north, get_is_east, &
            get_is_south, get_is_west, get_is_up, get_is_down, go_north, go_east, &
            go_south, go_west, go_down, go_up, examine_room
            
            !TODO: probably include some form of get mobs
	
  type dungeon_floor
    !TODO make these constants
    real :: direction_chance = 0.66 !chance that any direction will be available
    real :: stair_chance = .15      !chance that there are stairs and we can go down a floor
    real :: trap_chance = .25				!chance that there is a trap in the room
    real :: treasure_chance = .20		!chance that there is a treasure chest in the room
    real :: secret_chance = .10			!chance that there is a secret room
    real :: mob_chance = .25        !chance that there is a mob per mob 5 * .05 = .25
  
    !TODO	might want to move this to a greater scope
    integer :: floor_number = 0 !number of the floor
  
    !Set up Cardinal Directions
  	logical :: is_north = .FALSE., is_east = .FALSE., is_south = .FALSE., is_west = .FALSE. 
    logical :: is_up = .FALSE., is_down = .FALSE.
  
    logical :: is_stairs = .FALSE.     !for the stairs
  
    !TODO make some stuff to hold mobs n stuff
    logical :: has_trap = .FALSE.				!for the traps
    logical :: has_treasure = .FALSE.		!for the treasure
    logical :: has_secret = .FALSE.			!for the secret room
    logical :: is_secret = .TRUE.
    
    logical   :: has_mob = .FALSE.      !if we have a mob
    type(mob) :: mob                    !the mob
    
  end type dungeon_floor
	
! Set up the methods for the room module.
contains
  ! generate a floor based on number
  subroutine make_new_room(this, went_down)
    implicit none
    type(dungeon_floor) :: this
    logical :: went_down   ! if we went down
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    real :: new_seed       ! for random functions
    
    !set up our random stuff
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
    
    !no up ladders unless explicitely stated
    this%is_up = .FALSE.
    
    !same for the down ladder
    this%is_down = .FALSE.
    
    !reset directions
    this%is_north = .FALSE.
    this%is_east = .FALSE.
    this%is_south = .FALSE.
    this%is_west = .FALSE.
    
    !for traps secret rooms
    this%has_trap = .FALSE.
    this%has_treasure = .FALSE.
    this%has_secret = .FALSE.
    this%is_secret = .FALSE.
    
    !reset mob count
    this%has_mob = .FALSE.
    
    !generate a random width potentially based on floor number
    call RANDOM_NUMBER(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_north = .TRUE.
    end if
    
    call RANDOM_NUMBER(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_east = .TRUE.
    end if
    
    call RANDOM_NUMBER(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_south = .TRUE.
    end if
    
    call RANDOM_NUMBER(new_seed)  
    if (new_seed > (1-this%direction_chance)) then
      this%is_west = .TRUE.
    end if
    
    !In the event RNG sucks
    if ((.NOT. this%is_north).AND.(.NOT. this%is_east).AND.(.NOT. this%is_south).AND.(.NOT. this%is_west)) then
      !GO NORTH!
      this%is_north = .TRUE.
    end if
  
    !for the down stairs
    call RANDOM_NUMBER(new_seed)
    if (new_seed > (1 - this%stair_chance)) then
      this%is_down = .TRUE.
    end if
    
    !for the traps
    call RANDOM_NUMBER(new_seed)
    if (new_seed > (1 - this%trap_chance)) then
      this%has_trap = .TRUE.
    end if
    
    !for the treasure chests
    call RANDOM_NUMBER(new_seed)
    if (new_seed > (1 - this%treasure_chance)) then
      this%has_treasure = .TRUE.
    end if
    
    !for the secret rooms
    call RANDOM_NUMBER(new_seed)
    if (new_seed > (1 - this%secret_chance)) then
      this%has_secret = .TRUE.
    end if
    
    !for mobs
    call RANDOM_NUMBER(new_seed)
    if (new_seed > (1 - this%mob_chance)) then
      this%has_mob = .TRUE.
      call new_mob(this%mob, this%floor_number)
    end if
  
    IF (went_down) then
      !increment the floor
      this%floor_number = this%floor_number + 1
      
      !set that there is an UP ladder in the room
      this%is_up = .TRUE.
      
      print "(a17,i4,a1)", "Welcome to floor ", this%floor_number, "."
    END IF
    
    
    !output
    call examine_room(this)
    
  end subroutine make_new_room
	
  
  !secret room
  subroutine make_secret_room(this) 
    implicit none
    type(dungeon_floor) :: this
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    real :: new_seed       ! for random functions
    
    !set up our random stuff
    call RANDOM_SEED(size = n)
    allocate(seed(n))
    
    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)
    
    !no up ladders unless explicitely stated
    this%is_up = .FALSE.
    
    !same for the down ladder
    this%is_down = .FALSE.
    
    !reset directions
    this%is_north = .FALSE.
    this%is_east = .FALSE.
    this%is_south = .FALSE.
    this%is_west = .FALSE.
    
    !for traps secret rooms
    this%has_trap = .FALSE.
    this%has_treasure = .FALSE.
    this%has_secret = .FALSE.
    
    this%is_secret = .TRUE.
    
    !random
    call RANDOM_NUMBER(new_seed)
    
    if (new_seed > .5) then
      !treasure!
      this%has_treasure = .TRUE.
    else
      !bad ass mob
      this%has_mob = .TRUE.
      call new_mob(this%mob, this%floor_number*4)
    end if
    
    !output
    call examine_room(this)
    
  end subroutine make_secret_room
  
  subroutine examine_room(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    if (this%is_secret) then
      !SECRET ROOM
      if (this%has_treasure) then
        print *, "The room contains a valuable treasure."
      else 
        print *, "The room contains a", trim(this%mob%name)
      end if
    
      print *, "There is also an exit. You may leave."
    else
      !NORMAL ROOM
      !Output directions and instances of events, items, mobs, secret rooms, etc.
      print*, "You find yourself in a room." 
     
      if(this%has_mob) then
        print*, "You are not alone. There is a ", trim(this%mob%name), " here."
      end if
            
      if(this%has_treasure) then
        print*, "There is also a treasure chest."
      end if
      
      IF (this%is_north.AND.this%is_east.AND.this%is_south.AND.this%is_west) THEN
        print*, "Possible directions are North, South, East, West."
      ELSE IF (this%is_north.AND.this%is_east.AND.this%is_south) THEN
        print*, "Possible directions are North, East, and South."
      ELSE IF (this%is_north.AND.this%is_east.AND.this%is_west) THEN
        print*, "There is North, East, and West."
      ELSE IF (this%is_north.AND.this%is_south.AND.this%is_west) THEN
        print*, "Possible directions are North, South, and West."
      ELSE IF (this%is_east.AND.this%is_south.AND.this%is_west) THEN
        print*, "Possible directions are East, South and West."
      ELSE IF (this%is_north.AND.this%is_east) THEN
        print*, "There is both North and East."
      ELSE IF (this%is_north.AND.this%is_south) THEN
        print*, "You may go North or South."
      ELSE IF (this%is_north.AND.this%is_west) THEN
        print*, "There is North and West."
      ELSE IF (this%is_east.AND.this%is_south) THEN
        print*, "There is room to both the East and the South."
      ELSE IF (this%is_east.AND.this%is_west) THEN
        print*, "There are rooms to the East and West."
      ELSE IF (this%is_south.AND.this%is_west) THEN
        print*, "You may go South or West."
      ELSE IF (this%is_north) THEN
        print*, "You may go North."
      ELSE IF (this%is_east) THEN
        print*, "You may go East."
      ELSE IF (this%is_south) THEN
        print*, "You may go South."
      ELSE IF (this%is_west) THEN
        print*, "You may go West."
      ELSE
        !You will never get here
      END IF
      
      if (this%is_down.AND.this%is_up) then
        print*, "There are also stairs here that lead up and down."
      else if (this%is_down) then
        print*, "There are also stairs here that lead down."
      else if (this%is_up) then
        print*, "There are also stairs here that go up."
      end if  

      
    end if
  end subroutine examine_room
  
  !GETTERS
	!Floor number
	function get_floor_number(this) result (floor_number)
    implicit none
    type(dungeon_floor), intent(in) :: this
    integer :: floor_number
    
    !return the floor number
    floor_number = this%floor_number
	end function get_floor_number
  
  !Get North externally
  function get_is_north(this) result (is_north)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_north
    
    !return north
    is_north = this%is_north
	end function get_is_north
  
  !Get East externally
  function get_is_east(this) result (is_east)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_east
    
    !return east
    is_east = this%is_east
	end function get_is_east
  
  !Get South externally
  function get_is_south(this) result (is_south)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_south
    
    !return south
    is_south = this%is_south
	end function get_is_south
  
  !Get West externally
  function get_is_west(this) result (is_west)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_west
    
    !return west
    is_west = this%is_west
	end function get_is_west
  
  !Directions externally
  function get_is_up(this) result (is_up)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_up
    
    !return up
    is_up = this%is_up
	end function get_is_up
  
  !Directions externally
  function get_is_down(this) result (is_down)
    implicit none
    type(dungeon_floor), intent(in) :: this
    logical::is_down
    
    !return down
    is_down = this%is_down
	end function get_is_down
  !END OF GETTERS
  
  ! Direction handling
  subroutine go_north(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_north) THEN
      !output
      print*, "You head to the North."
      
      !destroy old room and make a new one "to the north"
      call make_new_room(this, .FALSE.)
    
    ELSE    
      print*, "There is no North here."
    END IF
  end subroutine go_north
  
  subroutine go_east(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
  
    IF (this%is_east) THEN
      print*, "You head to the East."
    
      !destroy old room and make a new one "to the east"
      call make_new_room(this, .FALSE.)
      
    ELSE
      print*, "There is no East here."
    END IF
  end subroutine go_east
  
  subroutine go_south(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_south) THEN
      print*, "You head to the South."
    
      !destroy old room and make a new one "to the south"
      call make_new_room(this, .FALSE.)
    ELSE
      print*, "The is no South here."
    END IF
  end subroutine go_south
  
  subroutine go_west(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_west) THEN
      print*, "You head to the Weast."
    
      !destroy old room and make a new one "to the west"
      call make_new_room(this, .FALSE.)
    ELSE
      print*, "There is no West here."
    END IF
  end subroutine go_west
  
  subroutine go_down(this)
    implicit none
    type(dungeon_floor) :: this
    
    IF (this%is_down) THEN
      print*, "You head Down into the darkness."
    
      !destroy old room and make a new one "to the down"
      call make_new_room(this, .TRUE.)
      
    ELSE
      print*, "There is no ladder here with which to go Down."
      
    END IF
  end subroutine go_down
  
  subroutine go_up(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    if (this%is_up) then
      print*, "You exit the dungeon before madness consumes you."
      !TODO: Do the points and exit
    ELSE
      print*, "There is no ladder here with which to go Up."
    end if
  end subroutine go_up
  
  subroutine leave_secret(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_secret) THEN
      print*, "You leave the room."
    
      !destroy old room and make a regular room
      call make_new_room(this, .FALSE.)
    ELSE
      print*, "You cannot leave what is not a secret room."
    END IF
  end subroutine leave_secret
  
end module class_dungeon_floor

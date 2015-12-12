module class_dungeon_floor
	implicit none
  private
  public :: dungeon_floor, make_new_room, get_floor_number, get_is_north, get_is_east, &
            get_is_south, get_is_west, get_is_up, get_is_down
            
            !TODO: probably include some form of get mobs
	
  type dungeon_floor
    !TODO make these constants
    real :: direction_chance = 0.66 !chance that any direction will be available
    real :: stair_chance = .15      !chance that there are stairs and we can go down a floor
  
    !TODO	might want to move this to a greater scope
    integer :: floor_number = 0 !number of the floor
  
    !Set up Cardinal Directions
  	logical :: is_north = .FALSE., is_east = .FALSE., is_south = .FALSE., is_west = .FALSE. 
    logical :: is_up = .FALSE., is_down = .FALSE.
  
    logical :: is_stairs = .FALSE.     !for the stairs
  
    !TODO make some stuff to hold mobs n stuff
    
  end type dungeon_floor
	
! Set up the methods for the room module.
contains
  ! generate a floor based on number
  subroutine make_new_room(this, went_down)
    implicit none
    type(dungeon_floor) :: this
    logical :: went_down   ! if we went down
    integer :: the_time(8) ! the time
    integer :: mili_time   ! in miliseconds
    real :: new_seed       ! for random functions
    
    !set up our random stuff
    
    call date_and_time(VALUES = the_time)
    mili_time = mili_time + the_time(3) * 86000000
    mili_time = mili_time + the_time(5) * 3600000
    mili_time = mili_time + the_time(6) * 60000
    mili_time = mili_time + the_time(7) * 1000
    mili_time = mili_time + the_time(8)
    call RANDOM_SEED(mili_time)
    
    !no up ladders unless explicitely stated
    this%is_up = .FALSE.
    
    !same for the down ladder
    this%is_down = .FALSE.
    
    !reset directions
    this%is_north = .FALSE.
    this%is_east = .FALSE.
    this%is_south = .FALSE.
    this%is_west = .FALSE.
    
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
  
    IF (went_down) then
      !increment the floor
      this%floor_number = this%floor_number + 1
      
      !set that there is an UP ladder in the room
      this%is_up = .TRUE.
      
      print*, "Welcome to floor ", this%floor_number, "."
    END IF
    
    !TODO populate mobs and monsters
    
    !Output directions and stuff
    print*, "You find yourself in a room."
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
      print*, "You may go South or East."
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
    
      
  end subroutine make_new_room
	
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
      call make_room(this, .FALSE.)
    
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
      call make_room(this, .FALSE.)
      
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
      call make_room(this, .FALSE.)
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
      call make_room(this, .FALSE.)
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
      call make_room(this, .TRUE.)
      
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
  
end module class_dungeon_floor

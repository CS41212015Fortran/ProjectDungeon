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
  subroutine make_new_room(this)
    implicit none
    type(dungeon_floor) :: this
    real :: new_seed ! for random functions
    
    !increment our floor number
    this%floor_number = this%floor_number + 1 !set our new floor number
    
    !set up our random stuff
    call init_random_seed()
    
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
    call random_number(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_north = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_east = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1 - this%direction_chance)) then
      this%is_south = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1-this%direction_chance)) then
      this%is_west = .TRUE.
    end if
    
    !In the event RNG sucks
    if ((.NOT. this%is_north).AND.(.NOT. this%is_east).AND.(.NOT. this%is_south).AND.(.NOT. this%is_west)) then
      !GO NORTH!
      this%is_north = .TRUE.
    end if
  
    !for the down stairs
    call random_number(new_seed)
    if (new_seed > (1 - this%stair_chance)) then
      this%is_down = .TRUE.
    end if
  
    !TODO populate mobs and monsters
    
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
      !destroy old room and make a new one "to the north"
      call make_room(this)
    END IF
  end subroutine go_north
  
  subroutine go_east(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
  
    IF (this%is_east) THEN
      !destroy old room and make a new one "to the east"
      call make_room(this)
    END IF
  end subroutine go_east
  
  subroutine go_south(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_south) THEN
      !destroy old room and make a new one "to the south"
      call make_room(this)
    END IF
  end subroutine go_south
  
  subroutine go_west(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    IF (this%is_west) THEN
      !destroy old room and make a new one "to the west"
      call make_room(this)
    END IF
  end subroutine go_west
  
  subroutine go_down(this)
    implicit none
    type(dungeon_floor) :: this
    
    IF (this%is_down) THEN
      !destroy old room and make a new one "to the north"
      call make_room(this)
      
      !set that there is an UP ladder in the room
      this%is_up = .TRUE.
    END IF
  end subroutine go_down
  
  subroutine go_up(this)
    implicit none
    type(dungeon_floor), intent(in) :: this
    
    if (this%is_up) then
      !TODO: Do the points and exit
      
    end if
  end subroutine go_up
  
end module class_dungeon_floor

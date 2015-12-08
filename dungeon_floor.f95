module dungeon_floor
	implicit none
	
  !TODO make these constants
  real :: direction_chance = 0.66 !chance that any direction will be available
  real :: stair_chance = .15      !chance that there are stairs and we can go down a floor
  
  !TODO	might want to move this to a greater scope
  integer :: floor_number = 0 !number of the floor
  
  !Set up Cardinal Directions
	logical :: is_north = .FALSE., is_east = .FALSE., is_south = .FALSE., is_west = .FALSE. 
  logical :: is_up = .FALSE., is_down = .FALSE.
  
  logical :: is_stairs = .FALSE.     !for the stairs
  
  logical :: is_up = .FALSE.         !to exit
  
  !TODO make some stuff to hold mobs n stuff
  
	
  ! Set up the methods for the room module.
  contains
  ! generate a floor based on number
  subroutine make_room(new_floor_number)
    real :: new_seed
    
    floor_number = new_floor_number !set our new floor number
    
    !set up our random stuff
    call init_random_seed()
    
    !no up ladders
    is_up = .FALSE.
    
    !generate a random width potentially based on floor number
    call random_number(new_seed)  
    if (new_seed > (1 - direction_chance)) then
      is_north = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1 - direction_chance)) then
      is_east = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1 - direction_chance)) then
      is_south = .TRUE.
    end if
    
    call random_number(new_seed)  
    if (new_seed > (1-direction_chance)) then
      is_west = .TRUE.
    end if
    
    !In the event RNG sucks
    if ((.NOT. is_north).AND.(.NOT. is_east).AND.(.NOT. is_south).AND.(.NOT. is_west)) then
      !GO NORTH!
      is_north = .TRUE.
    end if
  
    !TODO randomly populate some form of matrix for navigation
    
  end subroutine make_room
	
	! Return the number of the floor
	function get_floor_number() result (floor_number)
	
	end function get_floor_number
  
  subroutine set_up_ladder()
    is_up = .TRUE.
  end subroutine set_up_ladder
  
  ! Direction handling
  subroutine go_north()
    IF (is_north) THEN
      !destroy old room and make a new one "to the north"
      call make_room(floor_number)
    END IF
  end subroutine go_north
  
  subroutine go_east()
    IF (is_east) THEN
      !destroy old room and make a new one "to the east"
      call make_room(floor_number)
    END IF
  end subroutine go_east
  
  subroutine go_south()
    IF (is_south) THEN
      !destroy old room and make a new one "to the south"
      call make_room(floor_number)
    END IF
  end subroutine go_south
  
  subroutine go_west()
    IF (is_west) THEN
      !destroy old room and make a new one "to the west"
      call make_room(floor_number)
    END IF
  end subroutine go_west
  
  subroutine go_down()
    IF (is_down) THEN
      !destroy old room and make a new one "to the north"
      call make_room(floor_number+1)
      
      !set that there is an UP ladder in the room
      is_up = .TRUE.
      call set_up_ladder()
    END IF
  end subroutine go_down
  
  subroutine go_up()
    if (is_up) then
      !Do the points and exit
    end if
  end subroutine go_up
  
end module dungeon_floor

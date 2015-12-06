module dungeon_floor
	implicit none
		
  integer :: floor_number              !number of the floor
	integer :: width                     !width of the floor
  integer :: height                    !height of the floor
	
! Set up the methods for the floor module.
contains
  ! generate a floor based on number
  subroutine makeFloor(new_floor_number)
    real :: new_seed
    
    floor_number = new_floor_number !set our new floor number
    
    !set up our random stuff
    call init_random_seed()
    
    !generate a random width potentially based on floor number
    call random_number(new_seed)  
    !this should end up as a width between 3 and floor_number/3 + 3
    width = floor(new_seed * (floor_number/3) + 3)
    
    !same for the height
    call random_number(new_seed) 
    !this should end up as a height between 3 and floor_number/3 + 3
    height = floor(new_seed * (floor_number/3) + 3)    
  
    !TODO randomly populate some form of matrix for navigation
    
  end subroutine makeFloor
	
	! Return the number of the floor
	function getFloorNumber() result (floor_number)
	
	end function getFloorNumber
	
end module dungeon_floor

module trap
	implicit none
		
	character (len=40) :: trap_name  	!The name of the trap
	character (len=10) :: trap_type	 	!The trap type: damage, displace, impair
	logical :: triggered = .false.		!Whether or not the trap has been triggered
	
! Set up the methods for the trap module.
contains
	! Set the name of the trap.
	subroutine setTrapName(new_trap_name)
		trap_name = new_trap_name
	end subroutine setTrapName
	
	! Return the name of the trap.
	function getTrapName() result (trap_name)
		
	end function getTrapName
	
	! Set the type of the trap.
	subroutine setTrapType(new_trap_type)
		trap_type = new_trap_type
	end subroutine setTrapType
	
	! Return the type of the trap. 
	function getTrapType() result (trap_type)
	
	end function getTrapType
	
	! This function gets called when a player triggers a trap
	subroutine triggerTrap() 
		triggered = .true.
		call effectPlayer()
	end subroutine triggerTrap
	
	! This method effects the player once a trap is triggered
	subroutine effectPlayer() 
		 if (trap_type == "damage") then
		 	
		 else if (trap_type == "displace") then
		 	
		 else if (trap_type == "impair") then
		 	
		 else 
		 	
	end subroutine effectPlayer
	
end module trap

module classSecretRoom
	implicit none
	private
	public :: SecretRoom, secretRoomPrint
		
	type SecretRoom 
		logical :: discovered									!Whether or not the secret room has been found
	end type SecretRoom
	
! Set up the methods for the trap module.
contains

	subroutine secretRoomPrint(this)
		implicit none
    type(SecretRoom), intent(in) :: this
  end subroutine secretRoomPrint
end module classSecretRoom

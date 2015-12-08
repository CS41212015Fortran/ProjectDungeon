module classPlayer
	implicit none

	!type declaration
	type Player
		character(len=50) :: name
		integer :: strength
		integer :: intelegence
		integer :: moxie
		integer :: hp
		integer :: mana
		integer :: xp
		integer :: score
	end type Player
	   
end module classPlayer

module class_player
	implicit none

	!type declaration
	type player
		character(len=32) :: name
		integer :: strength
		integer :: intelegence
		integer :: moxie
		integer :: hp
		integer :: mana
		integer :: xp
		integer :: score
		integer :: melee_damage
	end type player

	! subroutine update_player_stats(this, plr)
	! 	use class_player
	! 	implicit none
	! 	type(player) :: plr
	!
	! end subroutine update_player_stats

end module class_player

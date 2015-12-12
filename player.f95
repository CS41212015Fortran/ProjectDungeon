module classPlayer
	implicit none
	private
	public :: Player,update_player_stats,update_derived_stats,print_stats,has_key

	!type declaration
	type Player
		character(len=32) :: name
		integer :: strength
		integer :: intelegence
		integer :: moxie
		integer :: hp
		integer :: hp_max
		integer :: mana
		integer :: xp
		integer :: score
		integer :: melee_damage
		integer :: keys
		real :: dodge_chance
		real :: disarm_chance
		real :: perception_chance
	end type Player

	contains

	function has_key(this) result (bool)
		implicit none
		type(Player), intent(in) :: this
		logical bool;

		if (this%keys > 0) then
			bool = .true.
		else
			bool = .false.
		end if
	end function has_key

	subroutine update_player_stats(this,c,i)
		implicit none
		type(Player), intent(inout) :: this
		type(character), intent(in) :: c
		type(integer), intent(in) :: i

		select case (c)
			case ('s')
				this%strength = this%strength + i
				call update_derived_stats(this)
	 		case ('i')
				this%intelegence = this%intelegence + i
				call update_derived_stats(this)
	 		case ('m')
				this%moxie = this%moxie + i
				call update_derived_stats(this)
			case ('x')
				this%xp = this%xp + i
			case ('k')
				this%keys = this%keys + i
	 		case default
				this%score = this%score + i
		end select
	end subroutine update_player_stats

	subroutine update_derived_stats(this)
		implicit none
		type(Player), intent(inout) :: this

		this%hp_max = 80 + this%strength*5
		this%hp = this%hp_max
		this%mana = 60 + this%intelegence*10
		this%melee_damage = (this%strength*2)-1
		this%dodge_chance = log(real(this%moxie))/5
		this%disarm_chance = log(real(this%moxie))/3
		this%perception_chance = log(real(this%intelegence))/5

	end subroutine update_derived_stats

	subroutine print_stats(this)
		implicit none
		type(Player), intent(in) :: this

		Print  "(a10)",adjustl(this%name)
		Print  "(a14,i10)",'STRENGTH    = ',this%strength
		Print  "(a14,i10)",'INTELEGENCE = ',this%intelegence
		Print  "(a14,i10)",'MOXIE       = ',this%moxie
		Print  "(a14,i10)",'HP          = ',this%hp
		Print  "(a14,i10)",'MANA        = ',this%mana
		Print  "(a14,i10)",'SCORE       = ',this%score
		Print  "(a14,f10.1)",'PERCEPTION% = ',this%perception_chance
		Print  "(a14,f10.1)",'DODGE%      = ',this%dodge_chance
		Print  "(a14,f10.1)",'DISARM%     = ',this%disarm_chance
		Print  "(a14,i10)",'MELEE DMG   = ',this%melee_damage
		Print  "(a14,i10)",'KEYS        = ',this%keys
		Print  "(a14,i10)",'XP          = ',this%xp
		Print  "(a14,i10)",'SCORE       = ',this%score

	end subroutine print_stats

end module classPlayer

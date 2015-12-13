module classPlayer
	implicit none
	private
	public :: Player,update_player_stats,update_derived_stats,print_stats, &
            has_key,short_stats,get_score, player_level_up, use_hp_pot, &
            use_mp_pot

	!type declaration
	type Player
		character(len=32) :: name
		integer :: strength
		integer :: intelegence
		integer :: moxie
		integer :: hp
		integer :: hp_max
		integer :: mana
    integer :: mana_max
		integer :: xp
		integer :: skill_points
		integer :: score
		integer :: melee_damage
		integer :: keys
    integer :: hp_pots
    integer :: mp_pots
		integer :: gold
		real :: dodge_chance
		real :: disarm_chance
		real :: perception_chance
    
    
    
	end type Player

	contains

	!check if the player has at least one key (to open chests and doors)
	function has_key(this) result (bool)
		implicit none
		type(Player), intent(in) :: this
		logical :: bool

		if (this%keys > 0) then
			bool = .true.
		else
			bool = .false.
		end if
	end function has_key
  
  !check for and use a health pot
	subroutine use_hp_pot(this)
		implicit none
		type(Player), intent(inout) :: this

		if (this%hp_pots > 0) then
      this%hp_pots = this%hp_pots - 1
      
      this%hp = this%hp + this%hp_max * .25
      if (this%hp > this%hp_max) then
        this%hp = this%hp_max
      end if
      
			print *, "you chugged an HP pot and have", &
                this%hp,"/",this%hp_max, "health."
		else
			print *, "You must construct additional HP potions"
		end if
	end subroutine use_hp_pot
  
  !check for and use a mana pot
	subroutine use_mp_pot(this)
		implicit none
		type(Player), intent(inout) :: this

		if (this%mp_pots > 0) then
      this%mp_pots = this%mp_pots - 1
      
      this%mana = this%mana + this%mana_max * .25
      if (this%mana > this%mana_max) then
        this%mana = this%mana_max
      end if
      
			print *, "you chugged an MP pot and have", &
                this%mana,"/",this%mana_max, "mana."
		else
			print *, "You must construct additional HP potions"
		end if
	end subroutine use_mp_pot

	!used for intial character creation and leveling up player stats
	subroutine player_level_up(this)
		implicit none
		type(Player), intent(inout) :: this
		type(character) :: input

		do while (this%skill_points>0)
			print *,'you have ',this%skill_points,' skill point(s) remaining'
			read *,input
			select case (input)
				case ('s')
					call update_player_stats(this,'s',1)
					this%skill_points = this%skill_points - 1
				case ('i')
		 			call update_player_stats(this,'i',1)
					this%skill_points = this%skill_points - 1
				case ('m')
		 			call update_player_stats(this,'m',1)
					this%skill_points = this%skill_points - 1
				case default
					print *,input,' is not a valid command'
			end select
		end do
	end subroutine player_level_up

	!mutator for changing player stats
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

	!used to calculate derived stats
	subroutine update_derived_stats(this)
		implicit none
		type(Player), intent(inout) :: this

		this%hp_max = 80 + this%strength*5
		this%hp = this%hp_max
		this%mana_max = 60 + this%intelegence*10
    this%mana = this%mana_max
		this%melee_damage = (this%strength*2)-1
		this%dodge_chance = log(real(this%moxie))/5
		this%disarm_chance = log(real(this%moxie))/3
		this%perception_chance = log(real(this%intelegence))/5

	end subroutine update_derived_stats

	!prints out prevalent charater statistics
	subroutine short_stats(this)
		implicit none
		type(Player), intent(in) :: this
		Print  "(a14,i10)",'HP          = ',this%hp
		Print  "(a14,i10)",'MANA        = ',this%mana
		Print  "(a14,i10)",'KEYS        = ',this%keys
		Print  "(a14,i10)",'GOLD        = ',this%gold
		Print  "(a14,i10)",'SKILL POINTS= ',this%skill_points
	end subroutine short_stats

	!prints out full character sheet
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
		Print  "(a14,i10)",'GOLD        = ',this%gold
		Print  "(a14,i10)",'XP          = ',this%xp
		Print  "(a14,i10)",'SKILL POINTS= ',this%skill_points
		Print  "(a14,i10)",'SCORE       = ',this%score

	end subroutine print_stats

  !prints the players final score
  function get_score(this) result (score)
    implicit none
    type(Player) :: this
    integer :: score !score
    score = 0

    score = score + this%hp
    score = score + this%mana
    score = score + this%gold * 1000
    score = score + this%keys * 100
    score = score + this%strength * 150
    score = score + this%intelegence * 150
    score = score + this%moxie * 150

  end function get_score

end module classPlayer

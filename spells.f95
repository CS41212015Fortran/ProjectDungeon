module class_spells
	use class_dungeon_floor
	use class_Mob
	use classPlayer

	implicit none
	private
	public :: Spell, apply_offensive_spell, apply_defensive_spell

type Spell
  character(len=32) :: name
  integer :: mana_cost
  integer :: dice_roll
	logical :: known
end type Spell

contains

  subroutine apply_offensive_spell(this,p,m)
		type(Spell) :: this
		type(Player) :: p
		type(Mob) :: m
    integer :: i, n, clock, dice
    integer, dimension(:), allocatable :: seed
    real    :: rrand
    !set up random
    call RANDOM_SEED(size = n)
    allocate(seed(n))

    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)

    call RANDOM_NUMBER(rrand)
    dice = ((modulo(rrand, real(this%dice_roll))+1)+ceiling(real(p%intelegence/2)))

		if ((p%mana-this%mana_cost)>=0) then !check to see if the player has enough mana for the spell
			m%health=m%health-dice
			p%mana = p%mana - this%mana_cost
			print *,'You do ',dice,' damage to the ',m%name
		else
			print *,'Out of mana'
		end if

  end subroutine apply_offensive_spell

	subroutine apply_defensive_spell(this,p)
		type(Spell) :: this
		type(Player) :: p
		type(Mob) :: m
    integer :: i, n, clock, dice
    integer, dimension(:), allocatable :: seed
    real    :: rrand
    !set up random
    call RANDOM_SEED(size = n)
    allocate(seed(n))

    call system_clock(count = clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call RANDOM_SEED(PUT = seed)
    deallocate(seed)

    call RANDOM_NUMBER(rrand)
    dice = ((modulo(rrand, real(this%dice_roll))+1)+ceiling(real(p%intelegence/2)))

		if ((p%mana-this%mana_cost)>=0) then !check to see if the player has enough mana for the spell
			if(p%hp+dice<=p%hp) then !heal the player by dice ammount if the player doesnt go over max hp
				p%hp=p%hp+dice
				p%mana = p%mana - this%mana_cost
				print *,'You heal yourelf +',dice,' HP'
			else
				print *,'You are too healthy to use this spell'
			end if
		else
			print *,'Out of mana'
		end if

	end subroutine apply_defensive_spell

end module class_spells

module class_spells
	implicit none
	private
	public :: Spell

type Spell
  character(len=32) :: name
  integer :: mana_cost
  integer :: dice_roll
  integer :: bonus
end type Spell

contains

  subroutine test_spell
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

    dice = (rrand%20)+1
    

  end subroutine test_spell

end module class_spells

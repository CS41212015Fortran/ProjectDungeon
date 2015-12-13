module classItem
	implicit none
	private
	public :: Item, itemDrop, itemUse, itemSell, itemPrint
		
	type Item 
		character (len=40) :: item_name
		character (len=200) :: item_effect
		
		integer :: item_type
		integer :: item_value
	end type Item
	
! Set up the methods for the trap module.
contains
	
	subroutine playerItemDrop(this, plr, dungeon)
		use classPlayer
		implicit none
    type(Item), intent(in) :: this
	end subroutine playerItemDrop
	
	subroutine itemUse(this, plr)
		use classPlayer
		implicit none
    type(Item), intent(in) :: this
	end subroutine itemUse
	
	subroutine itemSell(this, plr)
		use classPlayer
		implicit none
    type(Item), intent(in) :: this
	end subroutine itemSell
	
	! Print the item name
	subroutine itemPrint(this)
		implicit none
    type(Item), intent(in) :: this
    character (len=40) :: item_type_name
    
		select case (item_type)
		case (1)
			item_type_name = "head"
		case (2)
			item_type_name = "hand"
		case (3)
			item_type_name = "upper-body"
		case (4)
			item_type_name = "lower-body"
		case (5)
			item_type_name = "scroll"
		case (6)
			item_type_name = "consumable"
		case (7)
			item_type_name = "key"
		case default
			item_type_name = "Unidentified"
		end select
    
    print*, trim(this%item_name), ": This is a ", trim(item_type_name), " item. 
  end subroutine itemPrint
end module classItem


module class_Shop
	use class
	implicit none

	private
	public :: Shop, buy_item, sell_item, check_inventory

	type Shop
		
	end type Shop
	
	!upper body, lower body, head, left hand, right hand

contains
	! This function is called when a player  buys an item
	subroutine buy_item(this, item, plr)
	
	end subroutine buy_item
	
	! This function is called when a player sells an item
	subroutine sell_item(this, item, plr)
	
	end subroutine sell_item
	
	subroutine check_inventory(this)
		
	end subroutine check_inventory
end module class_Shop


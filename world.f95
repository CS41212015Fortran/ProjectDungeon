program world
	use classTrap
	use classTreasure
	use class_dungeon_floor
	use classPlayer

	!Need to use this line for every program
	implicit none

	!declaring type variables
	type(Trap) 					:: t = Trap("bear-trap", 1)
	type(Treasure)			:: c
	type(player) 				:: p
	type(dungeon_floor) :: d
	type(integer) :: points = 6
	type(character) :: input
	character(len=32) :: command

	!init player
	print *,' Enter player name '
	read (*,'(A)') p%name
	p%strength=1
	p%intelegence=1
	p%moxie=1
	p%xp=0
	p%keys=1
	p%score=0
	p%gold=0
	call update_derived_stats(p)

	print *,''
  print *,adjustl('Good Morrow '), p%name
	print *,adjustl('Time to boost your stats')
	print *,adjustl('You major stats are Strength, Intelegence, and Moxie')
	print *,adjustl('Strength determines you max HP and how much damage your melee attacks will do')
	print *,adjustl('Intelegence determines your max Mana, your Perception, and how much damage spells will do')
	print *,adjustl('Moxie determines your chance for Dodging, Disarming traps, and other "Rouge-like" abilites')
	print *,adjustl('Entering the following chacter will boost that stat by 1')
	print *,adjustl('Valid Commands are "s" for strength, "i" for intelegence, and "m" for moxie')
	print *,''

	do while (points>0)
		print *,'you have ',points,' point(s) remaining'
		read *,input
		select case (input)
			case ('s')
				call update_player_stats(p,'s',1)
				points = points - 1
			case ('i')
	 			call update_player_stats(p,'i',1)
				points = points - 1
			case ('m')
	 			call update_player_stats(p,'m',1)
				points = points - 1
			case default
				print *,input,' is not a valid command'
		end select
	end do
	print *,''

	call make_new_room(d, .true.)

	read (*,'(A)') command

	main: do while(.true.)
			if(index(command, "combat") > 0) then
				!start combat
				combat: do while(.true.)
					read (*,'(A)') command

					! if(mob%health<=0)
					! 	exit combat
					! endif

					if (p%hp<0) then
						exit main
				  end if

					if(index(command, "attack") > 0) then
					else if(index(command, "magic") > 0) then
					else if(index(command, "run") > 0) then
						exit combat
					else
						print *, "I don't know what you mean by ",command
					end if

					!mob does their turn

				end do combat
				!end combat

			! Go North
		  else if(index(command, "north") > 0) then
		  	! If the room contains a trap
				if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p, d)
					call effectPlayer(t, p, d)
				end if
				call go_north(d)

			! Go South
		  else if(index(command, "south") > 0) then
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p , d)
					call effectPlayer(t, p, d)
				end if
				call go_south(d)

			! Go East
		  else if(index(command, "east") > 0) then
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p, d)
					call effectPlayer(t, p, d)
				end if
				call go_east(d)

			! Go West
		  else if(index(command, "west") > 0) then
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p, d)
					call effectPlayer(t, p, d)
				end if
				call go_west(d)

			! Move up a floor
		  else if(index(command, "up") > 0) then
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p, d)
					call effectPlayer(t, p, d)
				end if
				call go_up(d)

			! Move down a floor
		  else if(index(command, "down") > 0) then
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call triggerTrap(t, p, d)
					call effectPlayer(t, p, d)
				end if
				call go_down(d)

			! Take an item
		  else if(index(command, "take") > 0) then

		  ! Check for traps
		  else if(index(command, "check-trap") > 0) then

		  ! Disarm a trap
		  else if(index(command, "disarm-trap") > 0) then

		  ! Unlock a chest if you have keys
		  else if(index(command, "unlock") > 0) then
		  	!If the room contains a treasure chest
				if (d%has_treasure) then
					print*, "There is a locked treasure chest in this room."
					call unlockChest(c, p)
				end if

			! Buy an item from a shop
		  else if(index(command, "buy") > 0) then

		 	! Sell an item at a shop
		  else if(index(command, "sell") > 0) then

		  ! Drop an item
		  else if(index(command, "drop") > 0) then

		  ! Check your stats
		  else if(index(command, "check-stats") > 0) then
		  	call print_stats(p)

		  ! Check your items
		  else if(index(command, "check-item") > 0) then

		  ! Look around you to gather your bearings
		  else if(index(command, "look") > 0) then

			! Quit the game
			else if(index(command, "quit") > 0) then
				exit main
			else
				print *, "I don't know what you mean by ",command
			end if

		  read (*,'(A)') command
	end do main
	print *, "Game Over: Your Final Score is ",p%score
end program world

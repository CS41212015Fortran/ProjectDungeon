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
	character(len=32) :: command
	real :: 				rrand

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
	p%skill_points=6
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
	call player_level_up(p)
	print *,''

	call make_new_room(d, .true.)

	read (*,'(A)') command

	main: do while(.true.)
			if(index(command, "combat") > 0) then
				! TODO Should Mobs have moxie?  To see who gets the initiative
				!start combat
				combat: do while(.true.)
					read (*,'(A)') command


					action: do while(.true.)
						if(index(command, "attack") > 0) then
							print *, "You choose to attack the ", mob%name, " for ", p%strength, " damage"
							mob%health = mob%health - p%strength
							print *, "The ", mob%name, " is now at ", mob%health, " health"
						end if
						else if(index(command, "magic") > 0) then
						end if
						else if(index(command, "run") > 0) then
							print *, "You ran away from the ", mob%name
							exit combat
						end if
						else
							print *, "I don't know what you mean by ",command
						end if
					end do action

					if(mob%health<=0) then
						print *, "You defeated the ", mob%name, "!"
					 	exit combat
					endif
					if (p%hp<0) then
						exit main
				  end if

					else
						!mob does their turn
						print *, "It is the ", mob%name, "'s turn to attack!"



					    call RANDOM_NUMBER(rrand)
					    if (new_seed > (1 - p%dodge_chance)) then
							!attack will connect
							p%hp = p%hp - mob%strength
							print *, "You were hit by the ", mob%name, " for ", mob%strength, " damage!"
							print *, "You now have ", p%hp, " health"

					    end if
						else
							!attack misses
							print *, "You were able to dodge the ", mob%name, "'s attack!"
						end if


						if(p%hp<0)
							!killed in action
							print *, "You were killed by the ", mob%name
							exit main
						end if

					end if

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

        exit main
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
  !calculate the score
	print *, "Game Over: Your Final Score is ",get_score(p)
end program world

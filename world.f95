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

	main: do while(p%hp>0)
			if(index(command, "combat") > 0) then
				!do combat
				read (*,'(A)') command
				if(index(command, "attack") > 0) then
				else if(index(command, "magic") > 0) then
				else if(index(command, "run") > 0) then
				else
					print *, "I don't know what you mean by '"//command//"'"
				end if
				!end combat
		  else if(index(command, "north") > 0) then
		  else if(index(command, "south") > 0) then
		  else if(index(command, "east") > 0) then
		  else if(index(command, "west") > 0) then
		  else if(index(command, "up") > 0) then
		  else if(index(command, "down") > 0) then
		  else if(index(command, "take") > 0) then
		  else if(index(command, "check-trap") > 0) then
		  else if(index(command, "disarm-trap") > 0) then
		  else if(index(command, "unlock") > 0) then
		  else if(index(command, "buy") > 0) then
		  else if(index(command, "sell") > 0) then
		  else if(index(command, "drop") > 0) then
		  else if(index(command, "check-stats") > 0) then
		  else if(index(command, "check-item") > 0) then
			else if(index(command, "quit") > 0) then
				exit main
			else
				print *, "I don't know what you mean by ",command
			end if

		  read (*,'(A)') command
	end do main
print *, "Game Over: Your Final Score is ",p%score

!NEW STUFF ABOVE!

	call print_stats(p)
	call make_new_room(d, .true.)
	call go_east(d)

	! If the room contains a trap
  if (d%has_trap) then
  	print*, "There is a trap in this room."
  	t = Trap("mine", 1)
  	call triggerTrap(t)
  	call effectPlayer(t, p, d)
  end if

  !If the room contains a treasure chest
  if (d%has_treasure) then
  	print*, "There is a locked treasure chest in this room."
  	call unlockChest(c, p)
  end if

  !If the room contains a secret room
  if (d%has_secret) then
  	print*, "There is a secret room somewhere here."
  end if

end program world

program world
	use classTrap
	use classPlayer
	use classTreasure
	use class_spells
	use class_dungeon_floor

	!Need to use this line for every program
	implicit none

	!declaring type variables
	type(Trap) 					:: t = Trap("bear-trap", 1)
	type(Treasure)			:: c
	type(player) 				:: p
	type(spell)					:: fireball
	type(spell)					:: heal
	type(dungeon_floor) :: d
	character(len=32) 	:: command
	real 								:: rrand

	!init player
	print '(a19)',' Enter player name '
	read (*,'(A)') p%name
	p%strength    =1
	p%intelegence =1
	p%moxie       =1
	p%xp          =0
	p%keys        =1
	p%score       =0
	p%gold        =0
	p%skill_points=6
	p%hp_pots     =3
	p%mp_pots     =3
	call update_derived_stats(p)

	!init spellbook
	fireball%name     ="Fireball"
	fireball%mana_cost=30
	fireball%dice_roll=20
	fireball%known    =.false.


	heal%name     ="Heal"
	heal%mana_cost=30
	heal%dice_roll=20
	heal%known    =.false.

	print *,''
  print *,'Good Morrow ', trim(p%name)
	print *,'Time to boost your stats'
	print *,'You major stats are Strength, Intelegence, and Moxie'
	print *,'Strength determines you max HP and how much damage your melee attacks will do'
	print *,'Intelegence determines your max Mana, your Perception, and how much damage spells will do'
	print *,'Moxie determines your chance for Dodging, Disarming traps, and other "Rouge-like" abilites'
	print *,''
	print *,"Valid Commands:"
	print *,"up down north south east west"
	print *,"combat check-stats check-bag use-hp use-mp"
	print *,"look unlock distarm-trap check-trap quit"
	print *,''
	print *,"Remeber you can ask for 'help' at any time"
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

				if(d%has_mob .eqv. .FALSE.) then
					print *, "There is no mob here to fight!"
					exit combat
				else
					print *, "The ", d%mob%name, " has ", d%mob%health, " health"
					print *, "You may 'attack', use 'magic', or 'run' away."
				end if

				read (*,'(A)') command

				if(index(command, "attack") > 0) then
					print *, "You choose to attack the ", d%mob%name, " for ", p%strength, " damage"
					d%mob%health = d%mob%health - p%strength
					print *, "The ", d%mob%name, " is now at ", d%mob%health, " health"
				else if(index(command, "magic") > 0) then
					print *, "Enter F to shoot a Fireball or H to heal yourself"
					read (*,'(A)') command
					if (command.eq.'F' .or. command.eq.'f') then
						call apply_offensive_spell(fireball,p,d%mob)
					else
						call apply_defensive_spell(heal,p)
					end if
				else if(index(command, "run") > 0) then
					print *, "You ran away from the ", d%mob%name
					exit combat

				else
					print *, "I don't know what you mean by ",command
				end if

				if(d%mob%health<=0) then
					print *, "You defeated the ", d%mob%name, "!"
					d%has_mob = .false.
				 	exit combat

				else
					!mob does their turn
					print *, "It is the ", d%mob%name, "'s turn to attack!"

				    call RANDOM_NUMBER(rrand)
				    if (rrand > (1 - p%dodge_chance)) then
						!attack will connect
						p%hp = p%hp - d%mob%strength
						print *, "You were hit by the ", d%mob%name, " for ", d%mob%strength, " damage!"
						print *, "You now have ", p%hp, " health"

					else
						!mob does their turn
						print *, "It is the ", d%mob%name, "'s turn to attack!"

					    call RANDOM_NUMBER(rrand)
					    if (rrand > (1 - p%dodge_chance)) then
							!attack will connect
							p%hp = p%hp - d%mob%strength
							print *, "You were hit by the ", d%mob%name, " for ", d%mob%strength, " damage!"
							print *, "You now have ", p%hp, " health"

						else
							!attack misses
							print *, "You were able to dodge the ", d%mob%name, "'s attack!"
						end if

						if(p%hp < 0) then
							!killed in action
							print *, "You were killed by the ", d%mob%name
							exit main
						end if
					end if

					if(p%hp < 0) then
						!killed in action
						print *, "You were killed by the ", d%mob%name
						exit main
					end if
				end if
			end do combat
			!end combat

		! Go North
		else if(index(command, "north") > 0) then
		  	! If the room contains a trap
        call go_north(d)
				if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if


		! Go South
		else if(index(command, "south") > 0) then
        call go_south(d)
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if


		! Go East
		else if(index(command, "east") > 0) then
        call go_east(d)
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if


		! Go West
		else if(index(command, "west") > 0) then
		  	call go_west(d)
        if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if

		! Move up a floor
		else if(index(command, "up") > 0) then
		  	call go_up(d)
        if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if

        exit main

		! Move down a floor
		else if(index(command, "down") > 0) then
      p%skill_points=3
		  call player_level_up(p)
      call go_down(d)
		  	if (d%has_trap) then
					t = Trap("mine", 1)
					call effectPlayer(t, p, d)
				end if


		! Take an item
		else if(index(command, "take") > 0) then

	  ! Check for traps
	  else if(index(command, "check-trap") > 0) then
			call checkForTrap(t, p, d)

	  ! Disarm a trap
	  else if(index(command, "disarm-trap") > 0) then
			call disarmTrap(t, p, d)

	  ! Unlock a chest if you have keys
	  else if(index(command, "unlock") > 0) then
	  	!If the room contains a treasure chest
			if (d%has_treasure) then
				print*, "There is a locked treasure chest in this room."
        c%locked = .TRUE.
				call unlockChest(c, p, d)
			end if

		else if(index(command, "use-hp") > 0) then
			call use_hp_pot(p)
		else if(index(command, "use-mp") > 0) then
			call use_mp_pot(p)

		! Check your stats
		else if(index(command, "check-stats") > 0) then
		  call print_stats(p)

		! Check your items
		else if(index(command, "check-bag") > 0) then
			call check_bag(p)

	  ! Look around you to gather your bearings
	  else if(index(command, "look") > 0) then
			call examine_room(d)

		! display help
		else if(index(command, "help") > 0) then
			print *,''
			print *,"Valid Commands: up down north south east west"
			print *,"combat check-stats check-bag use-hp use-mp"
			print *,"look unlock distarm-trap check-trap quit"
			print *,''
		! Quit the game
		else if(index(command, "quit") > 0) then
			exit main
		else
			print *, "I don't know what you mean by ",command
		end if

		read (*,'(A)') command
	end do main
  !calculate the score
	print *, trim(p%name),"'s Quest has come to an end: Your Final Score is ",get_score(p)
end program world

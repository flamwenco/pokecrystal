# Bugs and Glitches


## Thick Club and Light Ball can decrease damage done with boosted (Special) Attack

([Video](https://www.youtube.com/watch?v=rGqu3d3pdok&t=450))

This is a bug with `SpeciesItemBoost` in [battle/effect_commands.asm](battle/effect_commands.asm):

```asm
; Double the stat
	sla l
	rl h
	ret
```

**Fix:**

```asm
; Double the stat
	sla l
	rl h

	ld a, 999 / $100
	cp h
	jr c, .cap
	ld a, 999 % $100
	cp l
	ret nc

.cap
	ld h, 999 / $100
	ld l, 999 % $100
	ret
```


## Metal Powder can increase damage taken with boosted (Special) Defense

([Video](https://www.youtube.com/watch?v=rGqu3d3pdok&t=450))

This is a bug with `DittoMetalPowder` in [battle/effect_commands.asm](battle/effect_commands.asm):

```asm
	ld a, c
	srl a
	add c
	ld c, a
	ret nc

	srl b
	ld a, b
	and a
	jr nz, .done
	inc b
.done
	scf
	rr c
	ret
```

**Fix:**

```asm
	ld a, c
	srl a
	add c
	ld c, a
	ret nc

	srl b
	ld a, b
	and a
	jr nz, .done
	inc b
.done
	scf
	rr c

	ld a, 999 / $100
	cp b
	jr c, .cap
	ld a, 999 % $100
	cp c
	ret nc

.cap
	ld b, 999 / $100
	ld c, 999 % $100
	ret
```


## Belly Drum sharply boosts Attack even with under 50% HP

([Video](https://www.youtube.com/watch?v=zuCLMikWo4Y))

This is a bug with `BattleCommand_BellyDrum` in [battle/effect_commands.asm](battle/effect_commands.asm):

```asm
BattleCommand_BellyDrum: ; 37c1a
; bellydrum
; This command is buggy because it raises the user's attack
; before checking that it has enough HP to use the move.
; Swap the order of these two blocks to fix.
	call BattleCommand_AttackUp2
	ld a, [AttackMissed]
	and a
	jr nz, .failed

	callab GetHalfMaxHP
	callab CheckUserHasEnoughHP
	jr nc, .failed
```

**Fix:**

```asm
BattleCommand_BellyDrum: ; 37c1a
; bellydrum
	callab GetHalfMaxHP
	callab CheckUserHasEnoughHP
	jr nc, .failed

	call BattleCommand_AttackUp2
	ld a, [AttackMissed]
	and a
	jr nz, .failed
```


## HP bar animation is slow for high HP

([Video](https://www.youtube.com/watch?v=SE-BfsFgZVM))

This is a bug with `LongAnim_UpdateVariables` in [engine/anim_hp_bar.asm](engine/anim_hp_bar.asm):

```asm
	; This routine is buggy. The result from ComputeHPBarPixels is stored
	; in e. However, the pop de opcode deletes this result before it is even
	; used. The game then proceeds as though it never deleted that output.
	; To fix, uncomment the line below.
	call ComputeHPBarPixels
	; ld a, e
	pop bc
	pop de
	pop hl
	ld a, e ; Comment or delete this line to fix the above bug.
	ld hl, wCurHPBarPixels
	cp [hl]
	jr z, .loop
	ld [hl], a
	and a
	ret
```

**Fix:** Move `ld a, e` to right after `call ComputeHPBarPixels`.


## HP bar animation off-by-one error for low HP

([Video](https://www.youtube.com/watch?v=9KyNVIZxJvI))

This is a bug with `ShortHPBar_CalcPixelFrame` in [engine/anim_hp_bar.asm](engine/anim_hp_bar.asm):

```asm
	ld b, 0
; This routine is buggy. If [wCurHPAnimMaxHP] * [wCurHPBarPixels] is divisible
; by 48, the loop runs one extra time. To fix, uncomment the line below.
.loop
	ld a, l
	sub 6 * 8
	ld l, a
	ld a, h
	sbc $0
	ld h, a
	; jr z, .done
	jr c, .done
	inc b
	jr .loop
```

**Fix:** Uncomment `jr z, .done`.


## Experience underflow for level 1 Pokémon with Medium-Slow growth rate

([Video](https://www.youtube.com/watch?v=SXH8u0plHrE))

This can bring Pokémon straight from level 1 to 100 by gaining just a few experience points.

This is a bug with `CalcExpAtLevel` in [main.asm](main.asm):

```asm
CalcExpAtLevel: ; 50e47
; (a/b)*n**3 + c*n**2 + d*n - e
	ld a, [BaseGrowthRate]
	add a
	add a
	ld c, a
	ld b, 0
	ld hl, GrowthRates
	add hl, bc
```

**Fix:**

```asm
CalcExpAtLevel: ; 50e47
; (a/b)*n**3 + c*n**2 + d*n - e
	ld a, d
	cp 1
	jr nz, .UseExpFormula
; Pokémon have 0 experience at level 1
	xor a
	ld hl, hProduct
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ret

.UseExpFormula
	ld a, [BaseGrowthRate]
	add a
	add a
	ld c, a
	ld b, 0
	ld hl, GrowthRates
	add hl, bc
```


## Five-digit experience gain is printed incorrectly

([Video](https://www.youtube.com/watch?v=o54VjpAEoO8))

This is a bug with `Text_ABoostedStringBuffer2ExpPoints` and `Text_StringBuffer2ExpPoints` in [text/common_2.asm](text/common_2.asm):

```asm
Text_ABoostedStringBuffer2ExpPoints::
	text ""
	line "a boosted"
	cont "@"
	deciram StringBuffer2, 2, 4
	text " EXP. Points!"
	prompt

Text_StringBuffer2ExpPoints::
	text ""
	line "@"
	deciram StringBuffer2, 2, 4
	text " EXP. Points!"
	prompt
```

**Fix:** Change both `deciram StringBuffer2, 2, 4` to `deciram StringBuffer2, 2, 5`.


## NPC use of Full Heal or Full Restore does not cure Nightmare status

([Video](https://www.youtube.com/watch?v=rGqu3d3pdok&t=322))

This is a bug with `AI_HealStatus` in [battle/ai/items.asm](battle/ai/items.asm):

```asm
AI_HealStatus: ; 384e0
	ld a, [CurOTMon]
	ld hl, OTPartyMon1Status
	ld bc, PARTYMON_STRUCT_LENGTH
	call AddNTimes
	xor a
	ld [hl], a
	ld [EnemyMonStatus], a
	; Bug: this should reset SUBSTATUS_NIGHTMARE too
	; Uncomment the lines below to fix
	; ld hl, EnemySubStatus1
	; res SUBSTATUS_NIGHTMARE, [hl]
	ld hl, EnemySubStatus5
	res SUBSTATUS_TOXIC, [hl]
	ret
; 384f7
```

**Fix:** Uncomment `ld hl, EnemySubStatus1` and `res SUBSTATUS_NIGHTMARE, [hl]`.


## "Smart" AI encourages Mean Look if its own Pokémon is badly poisoned

([Video](https://www.youtube.com/watch?v=cygMO-zHTls))

This is a bug with `AI_Smart_MeanLook` in [battle/ai/scoring.asm](battle/ai/scoring.asm):

```asm
; 80% chance to greatly encourage this move if the enemy is badly poisoned (buggy).
; Should check PlayerSubStatus5 instead.
	ld a, [EnemySubStatus5]
	bit SUBSTATUS_TOXIC, a
	jr nz, .asm_38e26
```

**Fix:** Change `EnemySubStatus5` to `PlayerSubStatus5`.


## A Disabled, PP Up–enhanced move may not trigger automatic Struggling

([Video](https://www.youtube.com/watch?v=1v9x4SgMggs))

This is a bug with `CheckPlayerHasUsableMoves` in [battle/core.asm](battle/core.asm):

```asm
.done
	; Bug: this will result in a move with PP Up confusing the game.
	; Replace with "and $3f" to fix.
	and a
	ret nz

.force_struggle
	ld hl, BattleText_PkmnHasNoMovesLeft
	call StdBattleTextBox
	ld c, 60
	call DelayFrames
	xor a
	ret
```

**Fix:** Change `and a` to `and $3f`.


## Counter and Mirror Coat still work if the opponent uses an item

([Video](https://www.youtube.com/watch?v=uRYyzKRatFk))

*To do:* Identify specific code causing this bug and fix it.


## Present damage is incorrect in link battles

([Video](https://www.youtube.com/watch?v=XJaQoKtrEuw))

This bug existed for all battles in Gold and Silver, and was only fixed for single-player battles in Crystal to preserve link compatibility.

This is a bug with `BattleCommand_Present` in [battle/effects/present.asm](battle/effects/present.asm):

```asm
BattleCommand_Present: ; 37874
; present

	ld a, [wLinkMode]
	cp LINK_COLOSSEUM
	jr z, .colosseum_skippush
	push bc
	push de
.colosseum_skippush

	call BattleCommand_Stab

	ld a, [wLinkMode]
	cp LINK_COLOSSEUM
	jr z, .colosseum_skippop
	pop de
	pop bc
.colosseum_skippop
```

**Fix:**

```asm
BattleCommand_Present: ; 37874
; present

	push bc
	push de
	call BattleCommand_Stab
	pop de
	pop bc
```


## BRN/PSN/PAR do not affect catch rate

This is a bug with `PokeBall` in [items/item_effects.asm](items/item_effects.asm):

```asm
.statuscheck
; This routine is buggy. It was intended that SLP and FRZ provide a higher
; catch rate than BRN/PSN/PAR, which in turn provide a higher catch rate than
; no status effect at all. But instead, it makes BRN/PSN/PAR provide no
; benefit.
; Uncomment the line below to fix this.
	ld b, a
	ld a, [EnemyMonStatus]
	and 1 << FRZ | SLP
	ld c, 10
	jr nz, .addstatus
	; ld a, [EnemyMonStatus]
	and a
	ld c, 5
	jr nz, .addstatus
	ld c, 0
.addstatus
	ld a, b
	add c
	jr nc, .max_1
	ld a, $ff
.max_1
```

**Fix:** Uncomment `ld a, [EnemyMonStatus]`.


## Moon Ball does not boost catch rate

This is a bug with `MoonBallMultiplier` in [items/item_effects.asm](items/item_effects.asm):

```asm
MoonBallMultiplier:
; This function is buggy.
; Intent:  multiply catch rate by 4 if mon evolves with moon stone
; Reality: no boost

...

; Moon Stone's constant from Pokémon Red is used.
; No Pokémon evolve with Burn Heal,
; so Moon Balls always have a catch rate of 1×.
	push bc
	ld a, BANK(EvosAttacks)
	call GetFarByte
	cp MOON_STONE_RED ; BURN_HEAL
	pop bc
	ret nz
```

**Fix:** Change `MOON_STONE_RED` to `MOON_STONE`.


## Love Ball boosts catch rate for the wrong gender

This is a bug with `LoveBallMultiplier` in [items/item_effects.asm](items/item_effects.asm):

```asm
LoveBallMultiplier:
; This function is buggy.
; Intent:  multiply catch rate by 8 if mons are of same species, different sex
; Reality: multiply catch rate by 8 if mons are of same species, same sex

...

	ld a, d
	pop de
	cp d
	pop bc
	ret nz ; for the intended effect, this should be "ret z"
```

**Fix:** Change `ret nz` to `ret z`.


## Fast Ball only boosts catch rate for three Pokémon

This is a bug with `FastBallMultiplier` in [items/item_effects.asm](items/item_effects.asm):

```asm
FastBallMultiplier:
; This function is buggy.
; Intent:  multiply catch rate by 4 if enemy mon is in one of the three
;          FleeMons tables.
; Reality: multiply catch rate by 4 if enemy mon is one of the first three in
;          the first FleeMons table.

...

	inc hl
	cp -1
	jr z, .next
	cp c
	jr nz, .next ; for the intended effect, this should be "jr nz, .loop"
	sla b
	jr c, .max
```

**Fix:** Change `jr nz, .next` to `jr nz, .loop`.


## Friend Ball catches sent to the PC overwrite the wrong Pokémon's happiness

This is a bug with `PokeBall` in [items/item_effects.asm](items/item_effects.asm):

```asm
	ld a, [CurItem]
	cp FRIEND_BALL
	jr nz, .SkipBoxMonFriendBall
	; Bug: overwrites the happiness of the first mon in the box!
	ld a, FRIEND_BALL_HAPPINESS
	ld [sBoxMon1Happiness], a
.SkipBoxMonFriendBall:
```

`sBoxMon1Happiness` is written *before* the Friend Ball Pokémon is deposited.


## Dragon Scale, not Dragon Fang, boosts Dragon-type moves

This is a bug with `ItemAttributes` in [items/item_attributes.asm](items/item_attributes.asm):

```asm
; DRAGON FANG
	item_attribute 100, 0, 0, CANT_SELECT, ITEM, ITEMMENU_NOUSE, ITEMMENU_NOUSE

...

; DRAGON SCALE
	item_attribute 2100, HELD_DRAGON_BOOST, 10, CANT_SELECT, ITEM, ITEMMENU_NOUSE, ITEMMENU_NOUSE
```

**Fix:** Move `HELD_DRAGON_BOOST` to the `DRAGON FANG` attributes and `0` to `DRAGON SCALE`.


## Daisy's massages don't always increase happiness

This is a bug with `MassageOrHaircut` in [event/special.asm](event/special.asm):

```asm
; Bug: Subtracting $ff from $ff fails to set c.
; This can result in overflow into the next data array.
; In the case of getting a massage from Daisy, we bleed
; into CopyPokemonName_Buffer1_Buffer3, which passes
; $d0 to ChangeHappiness and returns $73 to the script.
; The end result is that there is a 0.4% chance your
; Pokemon's happiness will not change at all.
.loop
	sub [hl]
	jr c, .ok
	inc hl
	inc hl
	inc hl
	jr .loop

.ok
	inc hl
	ld a, [hli]
	ld [ScriptVar], a
	ld c, [hl]
	call ChangeHappiness
	ret

...

Data_DaisyMassage: ; 746b
	db $ff, 2, HAPPINESS_MASSAGE ; 99.6% chance

CopyPokemonName_Buffer1_Buffer3: ; 746e
	ld hl, StringBuffer1
	ld de, StringBuffer3
	ld bc, PKMN_NAME_LENGTH
	jp CopyBytes
```

**Fix:**

```asm
Data_DaisyMassage: ; 746b
	db $80, 2, HAPPINESS_MASSAGE ; 50% chance
	db $ff, 2, HAPPINESS_MASSAGE ; 50% chance
```


## Magikarp in Lake of Rage are shorter, not longer

This is a bug with `LoadEnemyMon.CheckMagikarpArea` in [battle/core.asm](battle/core.asm):

```asm
.CheckMagikarpArea:
; The z checks are supposed to be nz
; Instead, all maps in GROUP_LAKE_OF_RAGE (mahogany area)
; and routes 20 and 44 are treated as Lake of Rage

; This also means Lake of Rage Magikarp can be smaller than ones
; caught elsewhere rather than the other way around

; Intended behavior enforces a minimum size at Lake of Rage
; The real behavior prevents size flooring in the Lake of Rage area
	ld a, [MapGroup]
	cp GROUP_LAKE_OF_RAGE
	jr z, .Happiness
	ld a, [MapNumber]
	cp MAP_LAKE_OF_RAGE
	jr z, .Happiness
```

**Fix:** Change both `jr z, .Happiness` to `jr nz, .Happiness`.


## Battle transitions fail to account for the enemy's level

([Video](https://www.youtube.com/watch?v=eij_1060SMc))

This is a bug with `StartTrainerBattle_DetermineWhichAnimation` in [engine/battle_start.asm](engine/battle_start.asm):

```asm
StartTrainerBattle_DetermineWhichAnimation: ; 8c365 (23:4365)
; The screen flashes a different number of times depending on the level of
; your lead Pokemon relative to the opponent's.
; BUG: BattleMonLevel and EnemyMonLevel are not set at this point, so whatever
; values happen to be there will determine the animation.
	ld de, 0
	ld a, [BattleMonLevel]
	add 3
	ld hl, EnemyMonLevel
	cp [hl]
	jr nc, .okay
	set 0, e
.okay
	ld a, [wPermission]
	cp CAVE
	jr z, .okay2
	cp PERM_5
	jr z, .okay2
	cp DUNGEON
	jr z, .okay2
	set 1, e
.okay2
	ld hl, .StartingPoints
	add hl, de
	ld a, [hl]
	ld [wJumptableIndex], a
	ret
; 8c38f (23:438f)

.StartingPoints: ; 8c38f
	db 1,  9
	db 16, 24
; 8c393
```

*To do:* Fix this bug.


## No bump noise if standing on tile `$3E`

This is a bug with `DoPlayerMovement.CheckWarp` in [engine/player_movement.asm](engine/player_movement.asm):

```asm
; Bug: Since no case is made for STANDING here, it will check
; [.edgewarps + $ff]. This resolves to $3e at $8035a.
; This causes wd041 to be nonzero when standing on tile $3e,
; making bumps silent.

	ld a, [WalkingDirection]
	ld e, a
	ld d, 0
	ld hl, .EdgeWarps
	add hl, de
	ld a, [PlayerStandingTile]
	cp [hl]
	jr nz, .not_warp

	ld a, 1
	ld [wd041], a
	ld a, [WalkingDirection]
	cp STANDING
	jr z, .not_warp
```

**Fix:**

```asm
	ld a, [WalkingDirection]
	cp STANDING
	jr z, .not_warp
	ld e, a
	ld d, 0
	ld hl, .EdgeWarps
	add hl, de
	ld a, [PlayerStandingTile]
	cp [hl]
	jr nz, .not_warp

	ld a, 1
	ld [wd041], a
	ld a, [WalkingDirection]
```


## `LoadMetatiles` wrap around past 128 blocks

[home/map.asm](home/map.asm):

```asm
	; Set hl to the address of the current metatile data ([TilesetBlocksAddress] + (a) tiles).
	; This is buggy; it wraps around past 128 blocks.
	; To fix, uncomment the line below.
	add a ; Comment or delete this line to fix the above bug.
	ld l, a
	ld h, 0
	; add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, [TilesetBlocksAddress]
	add l
	ld l, a
	ld a, [TilesetBlocksAddress + 1]
	adc h
	ld h, a
```

**Fix:** Delete `add a` and uncomment `add hl, hl`.


## Surfing directly across a map connection does not load the new map

([Video](https://www.youtube.com/watch?v=XFOWvMNG-zw))

*To do:* Identify specific code causing this bug and fix it.


## `CheckOwnMon` only checks the first five letters of OT names

([Video](https://www.youtube.com/watch?v=GVTTmReM4nQ))

This bug can allow you to talk to Eusine in Celadon City and encounter Ho-Oh with only traded legendary beasts.

[engine/search.asm](engine/search.asm):

```asm
; check OT
; This only checks five characters, which is fine for the Japanese version,
; but in the English version the player name is 7 characters, so this is wrong.

	ld hl, PlayerName

rept NAME_LENGTH_JAPANESE +- 2 ; should be PLAYER_NAME_LENGTH +- 2
	ld a, [de]
	cp [hl]
	jr nz, .notfound
	cp "@"
	jr z, .found ; reached end of string
	inc hl
	inc de
endr

	ld a, [de]
	cp [hl]
	jr z, .found

.notfound
	pop de
	pop hl
	pop bc
	and a
	ret
```

**Fix:** Change `rept NAME_LENGTH_JAPANESE +- 2` to `rept PLAYER_NAME_LENGTH +- 2`.


## `HELD_CATCH_CHANCE` has no effect

This is a bug with `PokeBall` in [items/item_effects.asm](items/item_effects.asm):

```asm
	; BUG: callba overwrites a,
	; and GetItemHeldEffect takes b anyway.

	; This is probably the reason
	; the HELD_CATCH_CHANCE effect
	; is never used.

	; Uncomment the line below to fix.

	ld a, [BattleMonItem]
;	ld b, a
	callba GetItemHeldEffect
	ld a, b
	cp HELD_CATCH_CHANCE
```

**Fix:** Uncomment `ld b, a`.


## `ScriptCall` can overflow `wScriptStack` and crash

[engine/scripting.asm](engine/scripting.asm):

```asm
ScriptCall:
; Bug: The script stack has a capacity of 5 scripts, yet there is
; nothing to stop you from pushing a sixth script.  The high part
; of the script address can then be overwritten by modifications
; to ScriptDelay, causing the script to return to the rst/interrupt
; space.

	push de
	ld hl, wScriptStackSize
	ld e, [hl]
	inc [hl]
	ld d, $0
	ld hl, wScriptStack
	add hl, de
	add hl, de
	add hl, de
	pop de
	ld a, [ScriptBank]
	ld [hli], a
	ld a, [ScriptPos]
	ld [hli], a
	ld a, [ScriptPos + 1]
	ld [hl], a
	ld a, b
	ld [ScriptBank], a
	ld a, e
	ld [ScriptPos], a
	ld a, d
	ld [ScriptPos + 1], a
	ret
```


## `LoadSpriteGFX` does not limit the capacity of `UsedSprites`

[engine/overworld.asm](engine/overworld.asm):

```asm
LoadSpriteGFX: ; 14306
; Bug: b is not preserved, so
; it's useless as a next count.

	ld hl, UsedSprites
	ld b, SPRITE_GFX_LIST_CAPACITY
.loop
	ld a, [hli]
	and a
	jr z, .done
	push hl
	call .LoadSprite
	pop hl
	ld [hli], a
	dec b
	jr nz, .loop

.done
	ret

.LoadSprite:
	call GetSprite
	ld a, l
	ret
; 1431e
```

`GetSprite` modifies `b`. Surround it with `push bc`/`pop bc` to fix.


## `ChooseWildEncounter` doesn't really validate the wild Pokémon species

[engine/wildmons.asm](engine/wildmons.asm):

```asm
ChooseWildEncounter: ; 2a14f
...

	ld a, b
	ld [CurPartyLevel], a
	ld b, [hl]
	; ld a, b
	call ValidateTempWildMonSpecies
	jr c, .nowildbattle

	ld a, b ; This is in the wrong place.
	cp UNOWN
	jr nz, .done

...

ValidateTempWildMonSpecies: ; 2a4a0
; Due to a development oversight, this function is called with the wild Pokemon's level, not its species, in a.
```

**Fix:**

```asm
	ld a, b
	ld [CurPartyLevel], a
	ld b, [hl]
	ld a, b
	call ValidateTempWildMonSpecies
	jr c, .nowildbattle

	cp UNOWN
	jr nz, .done
```

## `TryObjectEvent` arbitrary code execution

[engine/events.asm](engine/events.asm):

```asm
; Bug: If IsInArray returns nc, data at bc will be executed as code.
	push bc
	ld de, 3
	ld hl, .pointers
	call IsInArray
	jr nc, .nope_bugged
	pop bc

	inc hl
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp hl

.nope_bugged
	; pop bc
	xor a
	ret
```

**Fix:** Uncomment `pop bc`.


## `Special_CheckBugContestContestantFlag` can read beyond its data table

[event/bug_contest_2.asm](event/bug_contest_2.asm):

```asm
Special_CheckBugContestContestantFlag: ; 139ed
; Checks the flag of the Bug Catching Contestant whose index is loaded in a.

; Bug: If a >= 10 when this is called, it will read beyond the table.

	ld hl, BugCatchingContestantEventFlagTable
	ld e, a
	ld d, 0
	add hl, de
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld b, CHECK_FLAG
	call EventFlagAction
	ret
; 139fe

BugCatchingContestantEventFlagTable: ; 139fe
	dw EVENT_BUG_CATCHING_CONTESTANT_1A
	dw EVENT_BUG_CATCHING_CONTESTANT_2A
	dw EVENT_BUG_CATCHING_CONTESTANT_3A
	dw EVENT_BUG_CATCHING_CONTESTANT_4A
	dw EVENT_BUG_CATCHING_CONTESTANT_5A
	dw EVENT_BUG_CATCHING_CONTESTANT_6A
	dw EVENT_BUG_CATCHING_CONTESTANT_7A
	dw EVENT_BUG_CATCHING_CONTESTANT_8A
	dw EVENT_BUG_CATCHING_CONTESTANT_9A
	dw EVENT_BUG_CATCHING_CONTESTANT_10A
; 13a12
```


## `ClearWRAM` only clears WRAM bank 1

[home/init.asm](home/init.asm):

```asm
ClearWRAM:: ; 25a
; Wipe swappable WRAM banks (1-7)
; Assumes CGB or AGB

	ld a, 1
.bank_loop
	push af
	ld [rSVBK], a
	xor a
	ld hl, $d000
	ld bc, $1000
	call ByteFill
	pop af
	inc a
	cp 8
	jr nc, .bank_loop ; Should be jr c
	ret
; 270
```

**Fix:** Change `jr nc, .bank_loop` to `jr c, .bank_loop`.


## `GetForestTreeFrame` works, but it's still bad

[tilesets/animations.asm](tilesets/animations.asm):

```asm
GetForestTreeFrame: ; fc54c
; Return 0 if a is even, or 2 if odd.
	and a
	jr z, .even
	cp 1
	jr z, .odd
	cp 2
	jr z, .even
	cp 3
	jr z, .odd
	cp 4
	jr z, .even
	cp 5
	jr z, .odd
	cp 6
	jr z, .even
.odd
	ld a, 2
	scf
	ret
.even
	xor a
	ret
; fc56d
```

**Fix:**

```asm
GetForestTreeFrame: ; fc54c
; Return 0 if a is even, or 2 if odd.
	and 1
	add a
	ret
; fc56d
```
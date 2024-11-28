DrawPlayerHP:
	ld a, $1
	jr DrawHP

DrawEnemyHP:
	ld a, $2

DrawHP:
	ld [wWhichHPBar], a
	push hl
	push bc
	; box mons have full HP
	ld a, [wMonType]
	cp BOXMON
	jr z, .at_least_1_hp

	ld a, [wTempMonHP]
	ld b, a
	ld a, [wTempMonHP + 1]
	ld c, a

; Any HP?
	or b
	jr nz, .at_least_1_hp

	xor a
	ld c, a
	ld e, a
	ld a, 6
	ld d, a
	jp .fainted

.at_least_1_hp
	ld a, [wTempMonMaxHP]
	ld d, a
	ld a, [wTempMonMaxHP + 1]
	ld e, a
	ld a, [wMonType]
	cp BOXMON
	jr nz, .not_boxmon

	ld b, d
	ld c, e

.not_boxmon
	predef ComputeHPBarPixels
	ld a, 6
	ld d, a
	ld c, a

.fainted
	ld a, c
	pop bc
	ld c, a
	pop hl
	push de
	push hl
	push hl
	call DrawBattleHPBar
	pop hl

; Print HP
	bccoord 1, 1, 0
	add hl, bc
	ld de, wTempMonHP
	ld a, [wMonType]
	cp BOXMON
	jr nz, .not_boxmon_2
	ld de, wTempMonMaxHP
.not_boxmon_2
	lb bc, 2, 3
	call PrintNum

	ld a, "/"
	ld [hli], a

; Print max HP
	ld de, wTempMonMaxHP
	lb bc, 2, 3
	call PrintNum
	pop hl
	pop de
	ret

PrintTempMonStats:
; Print wTempMon's stats at hl, with spacing bc.
	push bc
	push hl
	ld de, .StatNames
	call PlaceString
	pop hl
	pop bc
	add hl, bc
	ld bc, SCREEN_WIDTH
	add hl, bc
	ld de, wTempMonAttack
	lb bc, 2, 3
	call .PrintStat
	ld de, wTempMonDefense
	call .PrintStat
	ld de, wTempMonSpclAtk
	call .PrintStat
	ld de, wTempMonSpclDef
	call .PrintStat
	ld de, wTempMonSpeed
	jp PrintNum

.PrintStat:
	push hl
	call PrintNum
	pop hl
	ld de, SCREEN_WIDTH * 2
	add hl, de
	ret

.StatNames:
	db   "ATTACK"
	next "DEFENSE"
	next "SPCL.ATK"
	next "SPCL.DEF"
	next "SPEED"
	next "@"

GetGender:
; Return the gender of a given monster (wCurPartyMon/wCurOTMon/wCurWildMon).
; When calling this function, a should be set to an appropriate wMonType value.

; return values:
; a = 1: f = nc|nz; male
; a = 0: f = nc|z;  female
;        f = c:  genderless

; This is determined by comparing the Attack and Speed DVs
; with the species' gender ratio.

; Figure out what type of monster struct we're looking at.

; 0: PartyMon
	ld hl, wPartyMon1DVs
	ld bc, PARTYMON_STRUCT_LENGTH
	ld a, [wMonType]
	and a
	jr z, .PartyMon

	ld hl, wBufferMonDVs
	cp BUFFERMON
	jr z, .DVs

; 1: OTPartyMon
	ld hl, wOTPartyMon1DVs
	dec a
	jr z, .PartyMon

; 2: sBoxMon
	dec a
	jr z, .sBoxMon

; 3: Unknown
	ld hl, wTempMonDVs
	dec a
	jr z, .DVs

; else: WildMon
	ld hl, wEnemyMonDVs
	jr .DVs

; Get our place in the party/box.

.sBoxMon:
	; old box code access; crash
	di
	jp @

.PartyMon:
	ld a, [wCurPartyMon]
	call AddNTimes

.DVs:
; Attack DV
	ld a, [hli]
	and $f0
	ld b, a
; Speed DV
	ld a, [hl]
	and $f0
	swap a

; Put our DVs together.
	or b
	ld b, a

; We need the gender ratio to do anything with this.
	push bc
	ld a, [wCurPartySpecies]
	dec a
	ld hl, BaseData + BASE_GENDER
	ld bc, BASE_DATA_SIZE
	call AddNTimes
	pop bc

	ld a, BANK(BaseData)
	call GetFarByte

; The higher the ratio, the more likely the monster is to be female.

	cp GENDER_UNKNOWN
	jr z, .Genderless

	and a ; GENDER_F0?
	jr z, .Male

	cp GENDER_F100
	jr z, .Female

; Values below the ratio are male, and vice versa.
	cp b
	jr c, .Male

.Female:
	xor a
	ret

.Male:
	ld a, 1
	and a
	ret

.Genderless:
	scf
	ret

ListMovePP:
	ld a, [wNumMoves]
	inc a
	ld c, a
	ld a, NUM_MOVES
	sub c
	ld b, a
	push hl
	ld a, [wListMovesLineSpacing]
	ld e, a
	ld d, 0
	ld a, $3e ; P
	call .load_loop
	ld a, b
	and a
	jr z, .skip
	ld c, a
	ld a, "-"
	call .load_loop

.skip
	pop hl
	inc hl
	inc hl
	inc hl
	ld d, h
	ld e, l
	ld hl, wTempMonMoves
	ld b, 0
.loop
	ld a, [hli]
	and a
	jr z, .done
	push bc
	push hl
	push de
	ld hl, wMenuCursorY
	ld a, [hl]
	push af
	ld [hl], b
	push hl
	callfar GetMaxPPOfMove
	pop hl
	pop af
	ld [hl], a
	pop de
	pop hl
	push hl
	ld bc, wTempMonPP - (wTempMonMoves + 1)
	add hl, bc
	ld a, [hl]
	and $3f
	ld [wStringBuffer1 + 4], a
	ld h, d
	ld l, e
	push hl
	ld de, wStringBuffer1 + 4
	lb bc, 1, 2
	call PrintNum
	ld a, "/"
	ld [hli], a
	ld de, wTempPP
	lb bc, 1, 2
	call PrintNum
	pop hl
	ld a, [wListMovesLineSpacing]
	ld e, a
	ld d, 0
	add hl, de
	ld d, h
	ld e, l
	pop hl
	pop bc
	inc b
	ld a, b
	cp NUM_MOVES
	jr nz, .loop

.done
	ret

.load_loop
	ld [hli], a
	ld [hld], a
	add hl, de
	dec c
	jr nz, .load_loop
	ret

BrokenPlacePPUnits: ; unreferenced
; Probably would have these parameters:
; hl = starting coordinate
; de = SCREEN_WIDTH or SCREEN_WIDTH * 2
; c = the number of moves (1-4)
.loop
	ld [hl], $32 ; typo for P?
	inc hl
	ld [hl], $3e ; P
	dec hl
	add hl, de
	dec c
	jr nz, .loop
	ret

Unused_PlaceEnemyHPLevel:
	push hl
	push hl
	ld hl, wPartyMonNicknames
	ld a, [wCurPartyMon]
	call GetNickname
	pop hl
	call PlaceString
	call CopyMonToTempMon
	pop hl
	ld a, [wCurPartySpecies]
	cp EGG
	jr z, .egg
	push hl
	ld bc, -12
	add hl, bc
	ld b, 0
	call DrawEnemyHP
	pop hl
	ld bc, 5
	add hl, bc
	push de
	call PrintLevel
	pop de

.egg
	ret
ListMoves:
; List moves at hl, spaced every [wListMovesLineSpacing] tiles.
	ld de, wListMoves_MoveIndicesBuffer
	ld b, 0
.moves_loop
	ld a, [de]
	inc de
	and a
	jr z, .no_more_moves
	push de
	push hl
	push hl
	ld [wCurSpecies], a
	ld a, MOVE_NAME
	ld [wNamedObjectType], a
	call GetName
	ld de, wStringBuffer1
	pop hl
	push bc
	call PlaceString
	pop bc
	ld a, b
	ld [wNumMoves], a
	inc b
	pop hl
	push bc
	ld a, [wListMovesLineSpacing]
	ld c, a
	ld b, 0
	add hl, bc
	pop bc
	pop de
	ld a, b
	cp NUM_MOVES
	jr z, .done
	jr .moves_loop

.no_more_moves
	ld a, b
.nonmove_loop
	push af
	ld [hl], "-"
	ld a, [wListMovesLineSpacing]
	ld c, a
	ld b, 0
	add hl, bc
	pop af
	inc a
	cp NUM_MOVES
	jr nz, .nonmove_loop

.done
	ret

GetMonTypeIndex:
	; type in 'c', because farcall clobbers 'a'
	ld a, c
IF DEF(PSS)	
	and TYPE_MASK ; Phys/Spec Split only
ENDC

	; Skip Bird
	cp BIRD
	jr c, .done
	cp UNUSED_TYPES
	dec a
	jr c, .done
	sub UNUSED_TYPES
.done
	ld c, a
	ret

IF DEF(PSS)
ELSE
GetVanillaMoveCategoryIndex::
	ld a, c
	; given Type in 'c'
	; get Category index for vanilla, which is based on Type alone
	; if BP is 0, then it's a status move
	push af
	ld a, [wCurSpecies]
	dec a
	ld hl, Moves + MOVE_POWER
	ld bc, MOVE_LENGTH
	call AddNTimes
	ld a, BANK(Moves)
	call GetFarByte ; we are getting the move's power (BP)
	cp 2
	jr c, .statusmove ; means it's a status move
.notstatusmove
	pop af
	cp SPECIAL
	jr c, .phys
; .special
	ld a, 1 ; index for special
	jr .vanilla_done
.statusmove
	pop af
	ld a, 2 ; index for status move
	jr .vanilla_done
.phys
	xor a ; index for physical move
.vanilla_done
	ld c, a
	ret
ENDC
GetStatusConditionIndex:
; de points to status condition bytes of a pokemon from a party_struct or battle_struct
; return the status condition index in 'a', and also 'd' for those who farcall
	push de
	inc de
	inc de
	ld a, [de]
	ld b, a
	inc de
	ld a, [de]
	or b
	pop de
	jr z, .fnt
	ld a, [de]
	ld b, a
	and SLP_MASK
	ld a, 0
	jr nz, .slp
	bit PSN, b
	jr nz, .psn
	bit PAR, b
	jr nz, .par
	bit BRN, b
	jr nz, .brn
	bit FRZ, b
	jr nz, .frz
	ld d, a
	ret
	
.fnt
	inc a ; 6
.frz
	inc a ; 5
.brn
	inc a ; 4
.slp
	inc a ; 3
.par
	inc a ; 2
.psn
	inc a ; 1
	ld d, a
	ret

Player_CheckToxicStatus:
	ld a, [wPlayerSubStatus5]
	bit SUBSTATUS_TOXIC, a
	ret z
	scf ; if we are Toxic'd set carry flag
	ret

Enemy_CheckToxicStatus:
	ld a, [wEnemySubStatus5]
	bit SUBSTATUS_TOXIC, a
	ret z
	scf ; if we are Toxic'd set carry flag
	ret

Player_LoadNonFaintStatus:
	ld bc, 0	
	call Player_CheckToxicStatus
	jr nc, .player_check_status_nottoxic
	ld a, 7 ; status condition index for Toxic
	jr .player_loadgfx ; yes, we are toxic
.player_check_status_nottoxic
	ld de, wBattleMonStatus
	call GetStatusConditionIndex
	and a
	ret z ; .no_status
	cp $6 ; status condition index for FNT
	ret z
.player_loadgfx
	push af ; status index
; Load Player Status Tiles GFX into VRAM
	ld hl, StatusIconGFX
	ld bc, 2 * LEN_2BPP_TILE
	call AddNTimes
	ld d, h
	ld e, l
	ld hl, vTiles2 tile $70
	lb bc, BANK(StatusIconGFX), 2
	call Request2bpp
	pop de ; status index, needs to be in 'd'
	push de ; status condition index
	farcall LoadPlayerStatusIconPalette
	pop af
	cp 6
	jr z, .player_fnt
	ld c, a
	ret
.player_fnt
	xor a
	ld c, a
	ret

Enemy_LoadNonFaintStatus:
	ld bc, 0
	call Enemy_CheckToxicStatus
	jr nc, .enemy_check_nontoxic
	ld a, 7 ; status condition index for Toxic
	jr .enemy_loadgfx ; yes, we are toxic
.enemy_check_nontoxic
	ld de, wEnemyMonStatus
	call GetStatusConditionIndex
	and a ; could also use c but this was a local call, so a is not clobbered
	ret z ; .no_status
	cp $6 ; faint
	ret z
.enemy_loadgfx
; Load Enemy Status Tiles GFX into VRAM
	push af ; status condition index
	ld hl, EnemyStatusIconGFX
	ld bc, 2 * LEN_2BPP_TILE
	call AddNTimes
	ld d, h
	ld e, l
	ld hl, vTiles2 tile $72
	lb bc, BANK(EnemyStatusIconGFX), 2
	call Request2bpp

	pop de ; status condition index, needs to be in 'd'
	push de ; status condition index
	farcall LoadEnemyStatusIconPalette
	pop af ; status condition index
	cp 6 ; index 6 means the mon is fainted
	jr z, .enemy_fnt
	ld c, a ; status condition index
	ret
.enemy_fnt
	xor a ; status condition index
	ld c, a
	ret

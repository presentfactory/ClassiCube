#include "Inventory.h"
#include "Funcs.h"
#include "Game.h"
#include "Block.h"
#include "Event.h"
#include "Chat.h"

struct _InventoryData Inventory;
static int inventory_size;
static cc_uint8* inventory_default;

cc_bool Inventory_CheckChangeSelected(void) {
	if (!Inventory.CanChangeSelected) {
		Chat_AddRaw("&cThe server has forbidden you from changing your held block.");
		return false;
	}
	return true;
}

void Inventory_SetSelectedIndex(int index) {
	if (!Inventory_CheckChangeSelected()) return;
	Inventory.SelectedIndex = index;
	Event_RaiseVoid(&UserEvents.HeldBlockChanged);
}

void Inventory_SetHotbarIndex(int index) {
	if (!Inventory_CheckChangeSelected() || Game_ClassicMode) return;
	Inventory.Offset = index * INVENTORY_BLOCKS_PER_HOTBAR;
	Event_RaiseVoid(&UserEvents.HeldBlockChanged);
}

void Inventory_SwitchHotbar(void) {
	int index = Inventory.Offset == 0 ? 1 : 0;
	Inventory_SetHotbarIndex(index);
}

void Inventory_SetSelectedBlock(BlockID block) {
	int i;
	if (!Inventory_CheckChangeSelected()) return;

	/* Swap with currently selected block if given block is already in the hotbar */
	for (i = 0; i < INVENTORY_BLOCKS_PER_HOTBAR; i++) {
		if (Inventory_Get(i) != block) continue;
		Inventory_Set(i, Inventory_SelectedBlock);
		break;
	}

	Inventory_Set(Inventory.SelectedIndex, block);
	Event_RaiseVoid(&UserEvents.HeldBlockChanged);
}

void Inventory_PickBlock(BlockID block) {
	int i;
	if (!Inventory_CheckChangeSelected() || Inventory_SelectedBlock == block) return;

	/* Try to replace same block */
	for (i = 0; i < INVENTORY_BLOCKS_PER_HOTBAR; i++) {
		if (Inventory_Get(i) != block) continue;
		Inventory_SetSelectedIndex(i); return;
	}

	if (AutoRotate_Enabled) {
		/* Try to replace existing autorotate variant */
		for (i = 0; i < INVENTORY_BLOCKS_PER_HOTBAR; i++) {
			if (AutoRotate_BlocksShareGroup(Inventory_Get(i), block)) {
				Inventory_SetSelectedIndex(i);
				Inventory_SetSelectedBlock(block);
				return;
			}
		}
	}

	/* Is the currently selected block an empty slot? */
	if (Inventory_SelectedBlock == BLOCK_AIR) {
		Inventory_SetSelectedBlock(block); return;
	}

	/* Try to replace empty slots */
	for (i = 0; i < INVENTORY_BLOCKS_PER_HOTBAR; i++) {
		if (Inventory_Get(i) != BLOCK_AIR) continue;
		Inventory_Set(i, block);
		Inventory_SetSelectedIndex(i); return;
	}

	/* Finally, replace the currently selected block */
	Inventory_SetSelectedBlock(block);
}

static const cc_uint8 v6_inventory[32] = {
	BLOCK_STONE, BLOCK_COBBLE, BLOCK_DIRT, BLOCK_WOOD, BLOCK_LOG, BLOCK_LEAVES, BLOCK_SAPLING, BLOCK_DANDELION,
	BLOCK_ROSE, BLOCK_BROWN_SHROOM, BLOCK_RED_SHROOM, BLOCK_SAND, BLOCK_GRAVEL, BLOCK_GLASS, BLOCK_SPONGE, BLOCK_GOLD,
	BLOCK_RED, BLOCK_ORANGE, BLOCK_YELLOW, BLOCK_LIME, BLOCK_GREEN, BLOCK_TEAL, BLOCK_AQUA, BLOCK_CYAN, BLOCK_BLUE,
	BLOCK_INDIGO, BLOCK_VIOLET, BLOCK_MAGENTA, BLOCK_PINK, BLOCK_BLACK, BLOCK_GRAY, BLOCK_WHITE,
};
static const cc_uint8 v7_inventory[42] = {
	BLOCK_STONE, BLOCK_COBBLE, BLOCK_BRICK, BLOCK_DIRT, BLOCK_WOOD, BLOCK_LOG, BLOCK_LEAVES, BLOCK_GLASS, BLOCK_SLAB,
	BLOCK_MOSSY_ROCKS, BLOCK_SAPLING, BLOCK_DANDELION, BLOCK_ROSE, BLOCK_BROWN_SHROOM, BLOCK_RED_SHROOM, BLOCK_SAND, BLOCK_GRAVEL, BLOCK_SPONGE,
	BLOCK_RED, BLOCK_ORANGE, BLOCK_YELLOW, BLOCK_LIME, BLOCK_GREEN, BLOCK_TEAL, BLOCK_AQUA, BLOCK_CYAN, BLOCK_BLUE,
	BLOCK_INDIGO, BLOCK_VIOLET, BLOCK_MAGENTA, BLOCK_PINK, BLOCK_BLACK, BLOCK_GRAY, BLOCK_WHITE, BLOCK_COAL_ORE, BLOCK_IRON_ORE,
	BLOCK_GOLD_ORE, BLOCK_IRON, BLOCK_GOLD, BLOCK_BOOKSHELF, BLOCK_TNT, BLOCK_OBSIDIAN,
};

static const cc_uint8 v6_hotbar[INVENTORY_BLOCKS_PER_HOTBAR] = {
	BLOCK_STONE, BLOCK_COBBLE, BLOCK_DIRT, BLOCK_WOOD, BLOCK_LOG, BLOCK_LEAVES, BLOCK_SAPLING, BLOCK_DANDELION, BLOCK_ROSE
};
static const cc_uint8 v7_hotbar[INVENTORY_BLOCKS_PER_HOTBAR] = {
	BLOCK_STONE, BLOCK_COBBLE, BLOCK_BRICK, BLOCK_DIRT, BLOCK_WOOD, BLOCK_LOG, BLOCK_LEAVES, BLOCK_GLASS, BLOCK_SLAB
};

/* Returns default block that should go in the given inventory slot */
static BlockID DefaultMapping(int slot) {
	if (Game_ClassicMode) {
		if (slot < inventory_size) return inventory_default[slot];
	}else if (slot < BLOCK_MAX_CPE) {
		return (BlockID)(slot + 1);
	}
	return BLOCK_AIR;
}

void Inventory_ResetMapping(void) {
	int slot;
	for (slot = 0; slot < Array_Elems(Inventory.Map); slot++) {
		Inventory.Map[slot] = DefaultMapping(slot);
	}
}

void Inventory_AddDefault(BlockID block) {
	int slot;
	if (block >= BLOCK_CPE_COUNT) {
		Inventory.Map[block - 1] = block; return;
	}
	
	for (slot = 0; slot < BLOCK_MAX_CPE; slot++) {
		if (DefaultMapping(slot) != block) continue;
		Inventory.Map[slot] = block;
		return;
	}
}

void Inventory_Remove(BlockID block) {
	int slot;
	for (slot = 0; slot < Array_Elems(Inventory.Map); slot++) {
		if (Inventory.Map[slot] == block) Inventory.Map[slot] = BLOCK_AIR;
	}
}


/*########################################################################################################################*
*--------------------------------------------------Inventory component----------------------------------------------------*
*#########################################################################################################################*/
static void OnReset(void) {
	Inventory_ResetMapping();
	Inventory.CanChangeSelected = true;
}

static void OnInit(void) {
	cc_uint8* hotbar;
	int i;

	/* TODO maybe refactor to struct instead of using if/else */
	if (Game_Version.Protocol == 6) {
		hotbar            = v6_hotbar;
		inventory_size    = Array_Elems(v6_inventory);
		inventory_default = v6_inventory;
		Inventory.BlocksPerRow = 8;
	} else {
		hotbar            = v7_hotbar;
		inventory_size    = Array_Elems(v7_inventory);
		inventory_default = v7_inventory;
		Inventory.BlocksPerRow = Game_ClassicMode ? 9 : 10;
	}
	
	for (i = 0; i < INVENTORY_BLOCKS_PER_HOTBAR; i++) {
		Inventory.Table[i] = hotbar[i];
	}
	OnReset();
}

struct IGameComponent Inventory_Component = {
	OnInit,  /* Init  */
	NULL,    /* Free  */
	OnReset, /* Reset */
};

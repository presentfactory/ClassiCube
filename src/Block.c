#include "Block.h"
#include "Funcs.h"
#include "ExtMath.h"
#include "TexturePack.h"
#include "Game.h"
#include "Entity.h"
#include "Inventory.h"
#include "Event.h"
#include "Picking.h"

struct _BlockLists Blocks;

const char* const Sound_Names[SOUND_COUNT] = {
	"none", "wood", "gravel", "grass", "stone",
	"metal", "glass", "cloth", "sand", "snow",
};

/*########################################################################################################################*
*---------------------------------------------------Default properties----------------------------------------------------*
*#########################################################################################################################*/
#define FOG_NONE  0
#define FOG_WATER PackedCol_Make(  5,   5,  51, 255)
#define FOG_LAVA  PackedCol_Make(153,  25,   0, 255)

struct SimpleBlockDef {
	cc_uint8 topTexture, sideTexture, bottomTexture, height;
	PackedCol fogColor; cc_uint8 fogDensity;
	cc_bool fullBright, blocksLight; cc_uint8 gravity;
	cc_uint8 draw, collide, digSound, stepSound;
};
static const struct SimpleBlockDef invalid_blockDef = { 
	0,0,0,16, FOG_NONE,0, false,true, 100, DRAW_OPAQUE,COLLIDE_SOLID,0
};

/* Properties for all Classic and CPE blocks */
static const struct SimpleBlockDef core_blockDefs[BLOCK_CPE_COUNT] = {
/*TOP SID BOT HEI FOG_COLOR  DENS  FULL  BLOCKS GRAV DRAW_MODE    COLLIDE_MODE   DIG_SOUND     STEP_SOUND   */
{  0,  0,  0, 16, FOG_NONE ,   0, false, false, 100, DRAW_GAS,    COLLIDE_NONE,  SOUND_NONE,   SOUND_NONE   }, /* AIR */
{  1,  1,  1, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* STONE */
{  0,  3,  2, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_GRASS,  SOUND_GRASS  }, /* GRASS */
{  2,  2,  2, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_GRAVEL, SOUND_GRAVEL }, /* DIRT */
{ 16, 16, 16, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* COBBLE */
{  4,  4,  4, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_WOOD,   SOUND_WOOD   }, /* WOOD */
{ 15, 15, 15, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_GRASS,  SOUND_NONE   }, /* SAPLING */
{ 17, 17, 17, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* BEDROCK */

{ 14, 14, 14, 16, FOG_WATER,  10, false,  true, 100, DRAW_TRANSLUCENT, COLLIDE_WATER, SOUND_NONE, SOUND_NONE },/* WATER */
{ 14, 14, 14, 16, FOG_WATER,  10, false,  true, 100, DRAW_TRANSLUCENT, COLLIDE_WATER, SOUND_NONE, SOUND_NONE },/* STILL_WATER */
{ 30, 30, 30, 16, FOG_LAVA , 180,  true,  true, 100, DRAW_OPAQUE, COLLIDE_LAVA,  SOUND_NONE,   SOUND_NONE   }, /* LAVA */
{ 30, 30, 30, 16, FOG_LAVA , 180,  true,  true, 100, DRAW_OPAQUE, COLLIDE_LAVA,  SOUND_NONE,   SOUND_NONE   }, /* STILL_LAVA */
{ 18, 18, 18, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_SAND,   SOUND_SAND   }, /* SAND */
{ 19, 19, 19, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_GRAVEL, SOUND_GRAVEL }, /* GRAVEL */
{ 32, 32, 32, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* GOLD_ORE */
{ 33, 33, 33, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* IRON_ORE */

{ 34, 34, 34, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* COAL_ORE */
{ 21, 20, 21, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_WOOD,   SOUND_WOOD   }, /* LOG */
{ 22, 22, 22, 16, FOG_NONE ,   0, false, false,  40, DRAW_TRANSPARENT_THICK, COLLIDE_SOLID, SOUND_GRASS, SOUND_GRASS }, /* LEAVES */
{ 48, 48, 48, 16, FOG_NONE ,   0, false,  true,  90, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_GRASS,  SOUND_GRASS  }, /* SPONGE */
{ 49, 49, 49, 16, FOG_NONE ,   0, false, false, 100, DRAW_TRANSPARENT, COLLIDE_SOLID, SOUND_GLASS,SOUND_STONE},/* GLASS */
{ 64, 64, 64, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* RED */
{ 65, 65, 65, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* ORANGE */
{ 66, 66, 66, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* YELLOW */
	
{ 67, 67, 67, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* LIME */
{ 68, 68, 68, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* GREEN */
{ 69, 69, 69, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* TEAL */
{ 70, 70, 70, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* AQUA */
{ 71, 71, 71, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* CYAN */
{ 72, 72, 72, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* BLUE */
{ 73, 73, 73, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* INDIGO */
{ 74, 74, 74, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* VIOLET */

{ 75, 75, 75, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* MAGNETA */
{ 76, 76, 76, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* PINK */
{ 77, 77, 77, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* BLACK */
{ 78, 78, 78, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* GRAY */
{ 79, 79, 79, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* WHITE */
{ 13, 13, 13, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_GRASS,  SOUND_NONE   }, /* DANDELION */
{ 12, 12, 12, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_GRASS,  SOUND_NONE   }, /* ROSE */
{ 29, 29, 29, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_GRASS,  SOUND_NONE   }, /* BROWN_SHROOM */

{ 28, 28, 28, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_GRASS,  SOUND_NONE   }, /* RED_SHROOM */
{ 24, 40, 56, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_METAL,  SOUND_METAL  }, /* GOLD */
{ 23, 39, 55, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_METAL,  SOUND_METAL  }, /* IRON */
{  6,  5,  6, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* DOUBLE_SLAB */
{  6,  5,  6,  8, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* SLAB */
{  7,  7,  7, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* BRICK */
{  9,  8, 10, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_GRASS,  SOUND_GRASS  }, /* TNT */
{  4, 35,  4, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_WOOD,   SOUND_WOOD   }, /* BOOKSHELF */

{ 36, 36, 36, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* MOSSY_ROCKS */
{ 37, 37, 37, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* OBSIDIAN */
{ 16, 16, 16,  8, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* COBBLE_SLAB */
{ 11, 11, 11, 16, FOG_NONE ,   0, false, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_CLOTH,  SOUND_CLOTH  }, /* ROPE */
{ 25, 41, 57, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* SANDSTONE */
{ 50, 50, 50,  4, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_NONE,  SOUND_SNOW,   SOUND_SNOW   }, /* SNOW */
{ 38, 38, 38, 16, FOG_NONE ,   0,  true, false, 100, DRAW_SPRITE, COLLIDE_NONE,  SOUND_WOOD,   SOUND_NONE   }, /* FIRE */
{ 80, 80, 80, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* LIGHT_PINK */

{ 81, 81, 81, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* FOREST_GREEN */
{ 82, 82, 82, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* BROWN */
{ 83, 83, 83, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* DEEP_BLUE */
{ 84, 84, 84, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_CLOTH,  SOUND_CLOTH  }, /* TURQUOISE */
{ 51, 51, 51, 16, FOG_NONE ,   0, false,  true, 100, DRAW_TRANSLUCENT, COLLIDE_ICE, SOUND_STONE, SOUND_STONE },/* ICE */
{ 54, 54, 54, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* CERAMIC_TILE */
{ 86, 86, 86, 16, FOG_NONE ,   0,  true,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* MAGMA */
{ 26, 42, 58, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }, /* PILLAR */

{ 53, 53, 53, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_WOOD,   SOUND_WOOD   }, /* CRATE */
{ 52, 52, 52, 16, FOG_NONE ,   0, false,  true, 100, DRAW_OPAQUE, COLLIDE_SOLID, SOUND_STONE,  SOUND_STONE  }  /* STONE_BRICK */
/*TOP SID BOT HEI FOG_COLOR  DENS  FULL  BLOCKS GRAV DRAW_MODE    COLLIDE_MODE   DIG_SOUND     STEP_SOUND   */
};

/* Returns a backwards compatible collide type of a block. */
static cc_uint8 DefaultSet_MapOldCollide(BlockID b, cc_uint8 collide) {
	if (b == BLOCK_ROPE && collide == COLLIDE_NONE)   return COLLIDE_CLIMB_ROPE;
	if (b == BLOCK_ICE  && collide == COLLIDE_SOLID) return COLLIDE_ICE;

	if ((b == BLOCK_WATER || b == BLOCK_STILL_WATER) && collide == COLLIDE_LIQUID)
		return COLLIDE_WATER;
	if ((b == BLOCK_LAVA  || b == BLOCK_STILL_LAVA)  && collide == COLLIDE_LIQUID)
		return COLLIDE_LAVA;
	return collide;
}


/*########################################################################################################################*
*---------------------------------------------------------Block-----------------------------------------------------------*
*#########################################################################################################################*/
static cc_uint32 definedCustomBlocks[BLOCK_COUNT >> 5];
static char Block_NamesBuffer[STRING_SIZE * BLOCK_COUNT];
#define Block_NamePtr(i) &Block_NamesBuffer[STRING_SIZE * i]

cc_bool Block_IsCustomDefined(BlockID block) {
	return (definedCustomBlocks[block >> 5] & (1u << (block & 0x1F))) != 0;
}

void Block_SetCustomDefined(BlockID block, cc_bool defined) {
	if (defined) {
		definedCustomBlocks[block >> 5] |=  (1u << (block & 0x1F));
	} else {
		definedCustomBlocks[block >> 5] &= ~(1u << (block & 0x1F));
	}
}

void Block_DefineCustom(BlockID block) {
	PackedCol black = PackedCol_Make(0, 0, 0, 255);
	cc_string name  = Block_UNSAFE_GetName(block);
	Blocks.Tinted[block] = Blocks.FogCol[block] != black && String_IndexOf(&name, '#') >= 0;

	Block_SetDrawType(block, Blocks.Draw[block]);
	Block_CalcRenderBounds(block);
	Block_UpdateCulling(block);
	Block_CalcLightOffset(block);

	Inventory_AddDefault(block);
	Block_SetCustomDefined(block, true);
	Event_RaiseVoid(&BlockEvents.BlockDefChanged);
}

static void Block_RecalcIsLiquid(BlockID b) {
	cc_uint8 collide = Blocks.ExtendedCollide[b];
	Blocks.IsLiquid[b] =
		(collide == COLLIDE_WATER && Blocks.Draw[b] == DRAW_TRANSLUCENT) ||
		(collide == COLLIDE_LAVA  && Blocks.Draw[b] == DRAW_TRANSPARENT);
}

void Block_SetCollide(BlockID block, cc_uint8 collide) {
	/* necessary if servers redefined core blocks, before extended collide types were added */
	collide = DefaultSet_MapOldCollide(block, collide);
	Blocks.ExtendedCollide[block] = collide;
	Block_RecalcIsLiquid(block);

	/* Reduce extended collision types to their simpler forms */
	if (collide == COLLIDE_ICE)          collide = COLLIDE_SOLID;
	if (collide == COLLIDE_SLIPPERY_ICE) collide = COLLIDE_SOLID;

	if (collide == COLLIDE_WATER) collide = COLLIDE_LIQUID;
	if (collide == COLLIDE_LAVA)  collide = COLLIDE_LIQUID;
	Blocks.Collide[block] = collide;
}

void Block_SetDrawType(BlockID block, cc_uint8 draw) {
	if (draw == DRAW_OPAQUE && Blocks.Collide[block] != COLLIDE_SOLID) draw = DRAW_TRANSPARENT;
	Blocks.Draw[block] = draw;
	Block_RecalcIsLiquid(block);

	/* Whether a block is opaque and exactly occupies a cell in the world */
	/* The mesh builder module needs this information for optimisation purposes */
	Blocks.FullOpaque[block] = draw == DRAW_OPAQUE
		&& Blocks.MinBB[block].X == 0 && Blocks.MinBB[block].Y == 0 && Blocks.MinBB[block].Z == 0
		&& Blocks.MaxBB[block].X == 1 && Blocks.MaxBB[block].Y == 1 && Blocks.MaxBB[block].Z == 1;
}


#define BLOCK_RAW_NAMES "Air_Stone_Grass_Dirt_Cobblestone_Wood_Sapling_Bedrock_Water_Still water_Lava"\
"_Still lava_Sand_Gravel_Gold ore_Iron ore_Coal ore_Log_Leaves_Sponge_Glass_Red_Orange_Yellow_Lime_Green_Teal"\
"_Aqua_Cyan_Blue_Indigo_Violet_Magenta_Pink_Black_Gray_White_Dandelion_Rose_Brown mushroom_Red mushroom_Gold"\
"_Iron_Double slab_Slab_Brick_TNT_Bookshelf_Mossy rocks_Obsidian_Cobblestone slab_Rope_Sandstone_Snow_Fire_Light pink"\
"_Forest green_Brown_Deep blue_Turquoise_Ice_Ceramic tile_Magma_Pillar_Crate_Stone brick"

static const cc_string Block_DefaultName(BlockID block) {
	static const cc_string names   = String_FromConst(BLOCK_RAW_NAMES);
	static const cc_string invalid = String_FromConst("Invalid");
	int i, beg = 0, end;

	if (block > BLOCK_MAX_CPE) return invalid;
	/* Find start and end of this particular block name. */
	for (i = 0; i < block; i++) {
		beg = String_IndexOfAt(&names, beg, '_') + 1;
	}

	end = String_IndexOfAt(&names, beg, '_');
	if (end == -1) end = names.length;
	return String_UNSAFE_Substring(&names, beg, end - beg);
}

void Block_ResetProps(BlockID block) {
	const struct SimpleBlockDef* def = block <= BLOCK_MAX_CPE ? &core_blockDefs[block] : &invalid_blockDef;
	const cc_string name = Block_DefaultName(block);

	Blocks.BlocksLight[block] = def->blocksLight;
	Blocks.FullBright[block]  = def->fullBright;
	Blocks.FogCol[block]      = def->fogColor;
	Blocks.FogDensity[block]  = def->fogDensity / 100.0f;
	Block_SetCollide(block,     def->collide);
	Blocks.DigSounds[block]   = def->digSound;
	Blocks.StepSounds[block]  = def->stepSound;
	Blocks.SpeedMultiplier[block] = 1.0f;
	Block_SetName(block, &name);
	Blocks.Tinted[block]       = false;
	Blocks.SpriteOffset[block] = 0;

	Blocks.Draw[block] = def->draw;
	if (def->draw == DRAW_SPRITE) {
		Vec3_Set(Blocks.MinBB[block], 2.50f/16.0f, 0, 2.50f/16.0f);
		Vec3_Set(Blocks.MaxBB[block], 13.5f/16.0f, 1, 13.5f/16.0f);
	} else {		
		Vec3_Set(Blocks.MinBB[block], 0, 0,                   0);
		Vec3_Set(Blocks.MaxBB[block], 1, def->height / 16.0f, 1);
	}

	Block_SetDrawType(block, def->draw);
	Block_CalcRenderBounds(block);
	Block_CalcLightOffset(block);

	Block_Tex(block, FACE_YMAX) = def->topTexture;
	Block_Tex(block, FACE_YMIN) = def->bottomTexture;
	Block_SetSide(def->sideTexture, block);

	Blocks.ParticleGravity[block] = 5.4f * (def->gravity / 100.0f);
}

STRING_REF cc_string Block_UNSAFE_GetName(BlockID block) {
	return String_FromRaw(Block_NamePtr(block), STRING_SIZE);
}

void Block_SetName(BlockID block, const cc_string* name) {
	String_CopyToRaw(Block_NamePtr(block), STRING_SIZE, name);
}

int Block_FindID(const cc_string* name) {
	cc_string blockName;
	int block;

	for (block = BLOCK_AIR; block < BLOCK_COUNT; block++) {
		blockName = Block_UNSAFE_GetName(block);
		if (String_CaselessEquals(&blockName, name)) return block;
	}
	return -1;
}

int Block_Parse(const cc_string* name) {
	int b;
	if (Convert_ParseInt(name, &b) && b < BLOCK_COUNT) return b;
	return Block_FindID(name);
}

void Block_SetSide(TextureLoc texLoc, BlockID blockId) {
	int index = blockId * FACE_COUNT;
	Blocks.Textures[index + FACE_XMIN] = texLoc;
	Blocks.Textures[index + FACE_XMAX] = texLoc;
	Blocks.Textures[index + FACE_ZMIN] = texLoc;
	Blocks.Textures[index + FACE_ZMAX] = texLoc;
}


/*########################################################################################################################*
*--------------------------------------------------Block bounds/culling---------------------------------------------------*
*#########################################################################################################################*/
void Block_CalcRenderBounds(BlockID block) {
	Vec3 min = Blocks.MinBB[block], max = Blocks.MaxBB[block];

	if (Blocks.IsLiquid[block]) {
		min.X -= 0.1f/16.0f; max.X -= 0.1f/16.0f;
		min.Z -= 0.1f/16.0f; max.Z -= 0.1f/16.0f;
		min.Y -= 1.5f/16.0f; max.Y -= 1.5f/16.0f;
	} else if (Blocks.Draw[block] == DRAW_TRANSLUCENT && Blocks.Collide[block] != COLLIDE_SOLID) {
		min.X += 0.1f/16.0f; max.X += 0.1f/16.0f;
		min.Z += 0.1f/16.0f; max.Z += 0.1f/16.0f;
		min.Y -= 0.1f/16.0f; max.Y -= 0.1f/16.0f;
	}

	Blocks.RenderMinBB[block] = min; Blocks.RenderMaxBB[block] = max;
}

void Block_CalcLightOffset(BlockID block) {
	int flags = 0xFF;
	Vec3 min = Blocks.MinBB[block], max = Blocks.MaxBB[block];

	if (min.X != 0) flags &= ~(1 << FACE_XMIN);
	if (max.X != 1) flags &= ~(1 << FACE_XMAX);
	if (min.Z != 0) flags &= ~(1 << FACE_ZMIN);
	if (max.Z != 1) flags &= ~(1 << FACE_ZMAX);

	if ((min.Y != 0 && max.Y == 1) && Blocks.Draw[block] != DRAW_GAS) {
		flags &= ~(1 << FACE_YMAX);
		flags &= ~(1 << FACE_YMIN);
	}
	Blocks.LightOffset[block] = flags;
}

void Block_RecalculateAllSpriteBB(void) {
	int block;
	for (block = BLOCK_AIR; block < BLOCK_COUNT; block++) {
		if (Blocks.Draw[block] != DRAW_SPRITE) continue;

		Block_RecalculateBB((BlockID)block);
	}
}

static float GetSpriteBB_MinX(int size, int tileX, int tileY, const struct Bitmap* bmp) {
	BitmapCol* row;
	int x, y;

	for (x = 0; x < size; x++) {
		for (y = 0; y < size; y++) {
			row = Bitmap_GetRow(bmp, tileY * size + y) + (tileX * size);
			if (BitmapCol_A(row[x])) { return (float)x / size; }
		}
	}
	return 1.0f;
}

static float GetSpriteBB_MinY(int size, int tileX, int tileY, const struct Bitmap* bmp) {
	BitmapCol* row;
	int x, y;

	for (y = size - 1; y >= 0; y--) {
		row = Bitmap_GetRow(bmp, tileY * size + y) + (tileX * size);
		for (x = 0; x < size; x++) {
			if (BitmapCol_A(row[x])) { return 1.0f - (float)(y + 1) / size; }
		}
	}
	return 1.0f;
}

static float GetSpriteBB_MaxX(int size, int tileX, int tileY, const struct Bitmap* bmp) {
	BitmapCol* row;
	int x, y;

	for (x = size - 1; x >= 0; x--) {
		for (y = 0; y < size; y++) {
			row = Bitmap_GetRow(bmp, tileY * size + y) + (tileX * size);
			if (BitmapCol_A(row[x])) { return (float)(x + 1) / size; }
		}
	}
	return 0.0f;
}

static float GetSpriteBB_MaxY(int size, int tileX, int tileY, const struct Bitmap* bmp) {
	BitmapCol* row;
	int x, y;

	for (y = 0; y < size; y++) {
		row = Bitmap_GetRow(bmp, tileY * size + y) + (tileX * size);
		for (x = 0; x < size; x++) {
			if (BitmapCol_A(row[x])) { return 1.0f - (float)y / size; }
		}
	}
	return 0.0f;
}

void Block_RecalculateBB(BlockID block) {
	struct Bitmap* bmp = &Atlas2D.Bmp;
	int tileSize = Atlas2D.TileSize;
	TextureLoc texLoc = Block_Tex(block, FACE_XMAX);
	int x = Atlas2D_TileX(texLoc), y = Atlas2D_TileY(texLoc);

	Vec3 centre = { 0.5f, 0.0f, 0.5f };
	float minX = 0, minY = 0, maxX = 1, maxY = 1;
	Vec3 minRaw, maxRaw;

	if (y < Atlas2D.RowsCount) {
		minX = GetSpriteBB_MinX(tileSize, x, y, bmp);
		minY = GetSpriteBB_MinY(tileSize, x, y, bmp);
		maxX = GetSpriteBB_MaxX(tileSize, x, y, bmp);
		maxY = GetSpriteBB_MaxY(tileSize, x, y, bmp);
	}

	minRaw = Vec3_RotateY3(minX - 0.5f, minY, 0.0f, 45.0f * MATH_DEG2RAD);
	maxRaw = Vec3_RotateY3(maxX - 0.5f, maxY, 0.0f, 45.0f * MATH_DEG2RAD);
	Vec3_Add(&Blocks.MinBB[block], &minRaw, &centre);
	Vec3_Add(&Blocks.MaxBB[block], &maxRaw, &centre);
	Block_CalcRenderBounds(block);
}


static void Block_CalcStretch(BlockID block) {
	/* faces which can be stretched on X axis */
	if (Blocks.MinBB[block].X == 0.0f && Blocks.MaxBB[block].X == 1.0f) {
		Blocks.CanStretch[block] |= 0x3C;
	} else {
		Blocks.CanStretch[block] &= 0xC3; /* ~0x3C */
	}

	/* faces which can be stretched on Z axis */
	if (Blocks.MinBB[block].Z == 0.0f && Blocks.MaxBB[block].Z == 1.0f) {
		Blocks.CanStretch[block] |= 0x03;
	} else {
		Blocks.CanStretch[block] &= 0xFC; /* ~0x03 */
	}
}

static cc_bool Block_MightCull(BlockID block, BlockID other) {
	cc_uint8 bType, oType;
	/* Sprite blocks can never cull blocks. */
	if (Blocks.Draw[block] == DRAW_SPRITE) return false;

	/* NOTE: Water is always culled by lava */
	if ((block == BLOCK_WATER || block == BLOCK_STILL_WATER)
		&& (other == BLOCK_LAVA || other == BLOCK_STILL_LAVA))
		return true;

	/* All blocks (except for say leaves) cull with themselves */
	if (block == other) return Blocks.Draw[block] != DRAW_TRANSPARENT_THICK;

	/* An opaque neighbour (asides from lava) culls this block. */
	if (Blocks.Draw[other] == DRAW_OPAQUE && !Blocks.IsLiquid[other]) return true;
	/* Transparent/Gas blocks don't cull other blocks (except themselves) */
	if (Blocks.Draw[block] != DRAW_TRANSLUCENT || Blocks.Draw[other] != DRAW_TRANSLUCENT) return false;

	/* Some translucent blocks may still cull other translucent blocks */
	/* e.g. for water/ice, don't need to draw faces of water */
	bType = Blocks.Collide[block]; oType = Blocks.Collide[other]; 
	return (bType == COLLIDE_SOLID && oType == COLLIDE_SOLID) || bType != COLLIDE_SOLID;
}

static void Block_CalcCulling(BlockID block, BlockID other) {
	Vec3 bMin, bMax, oMin, oMax;
	cc_bool occludedX, occludedY, occludedZ, bothLiquid;
	int f;

	/* Some blocks may not cull 'other' block, in which case just skip per-face check */
	/* e.g. sprite blocks, default leaves, will not cull any other blocks */
	if (!Block_MightCull(block, other)) {	
		Blocks.Hidden[(block * BLOCK_COUNT) + other] = 0;
		return;
	}

	bMin = Blocks.MinBB[block]; bMax = Blocks.MaxBB[block];
	oMin = Blocks.MinBB[other]; oMax = Blocks.MaxBB[other];

	/* Extend offsets of liquid down to match rendered position */
	/* This isn't completely correct, but works well enough */
	if (Blocks.IsLiquid[block]) bMax.Y -= 1.50f / 16.0f;
	if (Blocks.IsLiquid[other]) oMax.Y -= 1.50f / 16.0f;

	bothLiquid = Blocks.IsLiquid[block] && Blocks.IsLiquid[other];
	f = 0; /* mark all faces initially 'not hidden' */

	/* Whether the 'texture region' of a face on block fits inside corresponding region on other block */
	occludedX = (bMin.Z >= oMin.Z && bMax.Z <= oMax.Z) && (bMin.Y >= oMin.Y && bMax.Y <= oMax.Y);
	occludedY = (bMin.X >= oMin.X && bMax.X <= oMax.X) && (bMin.Z >= oMin.Z && bMax.Z <= oMax.Z);
	occludedZ = (bMin.X >= oMin.X && bMax.X <= oMax.X) && (bMin.Y >= oMin.Y && bMax.Y <= oMax.Y);

	f |= occludedX && oMax.X == 1.0f && bMin.X == 0.0f ? (1 << FACE_XMIN) : 0;
	f |= occludedX && oMin.X == 0.0f && bMax.X == 1.0f ? (1 << FACE_XMAX) : 0;
	f |= occludedZ && oMax.Z == 1.0f && bMin.Z == 0.0f ? (1 << FACE_ZMIN) : 0;
	f |= occludedZ && oMin.Z == 0.0f && bMax.Z == 1.0f ? (1 << FACE_ZMAX) : 0;
	f |= occludedY && (bothLiquid || (oMax.Y == 1.0f && bMin.Y == 0.0f)) ? (1 << FACE_YMIN) : 0;
	f |= occludedY && (bothLiquid || (oMin.Y == 0.0f && bMax.Y == 1.0f)) ? (1 << FACE_YMAX) : 0;
	Blocks.Hidden[(block * BLOCK_COUNT) + other] = f;
}

void Block_UpdateAllCulling(void) {
	int block, neighbour;
	for (block = BLOCK_AIR; block < BLOCK_COUNT; block++) {
		Block_CalcStretch((BlockID)block);
		for (neighbour = BLOCK_AIR; neighbour < BLOCK_COUNT; neighbour++) {
			Block_CalcCulling((BlockID)block, (BlockID)neighbour);
		}
	}
}

void Block_UpdateCulling(BlockID block) {
	int neighbour;
	Block_CalcStretch(block);
	
	for (neighbour = BLOCK_AIR; neighbour < BLOCK_COUNT; neighbour++) {
		Block_CalcCulling(block, (BlockID)neighbour);
		Block_CalcCulling((BlockID)neighbour, block);
	}
}


/*########################################################################################################################*
*-------------------------------------------------------AutoRotate--------------------------------------------------------*
*#########################################################################################################################*/
cc_bool AutoRotate_Enabled;

#define AR_GROUP_CORNERS 0
#define AR_GROUP_VERTICAL 1
#define AR_GROUP_DIRECTION 2
#define AR_GROUP_PILLAR  3

#define AR_EQ1(x)    (dir0 == x && dir1 == '\0')
#define AR_EQ2(x, y) (dir0 == x && dir1 == y)
static int AR_CalcGroup(const cc_string* dir) {
	char dir0, dir1;
	dir0 = dir->length > 1 ? dir->buffer[1] : '\0'; Char_MakeLower(dir0);
	dir1 = dir->length > 2 ? dir->buffer[2] : '\0'; Char_MakeLower(dir1);

	if (AR_EQ2('n','w') || AR_EQ2('n','e') || AR_EQ2('s','w') || AR_EQ2('s','e')) {
		return AR_GROUP_CORNERS;
	} else if (AR_EQ1('u') || AR_EQ1('d')) {
		return AR_GROUP_VERTICAL;
	} else if (AR_EQ1('n') || AR_EQ1('w') || AR_EQ1('s') || AR_EQ1('e')) {
		return AR_GROUP_DIRECTION;
	} else if (AR_EQ2('u','d') || AR_EQ2('w','e') || AR_EQ2('n','s')) {
		return AR_GROUP_PILLAR;
	}
	return -1;
}

/* replaces a portion of a string, appends otherwise */
static void AutoRotate_Insert(cc_string* str, int offset, const char* suffix) {
	int i = str->length - offset;

	for (; *suffix; suffix++, i++) {
		if (i < str->length) {
			str->buffer[i] = *suffix;
		} else {
			String_Append(str, *suffix);
		}
	}
}
/* finds proper rotated form of a block, based on the given name */
static int FindRotated(cc_string* name, int offset);

static int GetRotated(cc_string* name, int offset) {
	int rotated = FindRotated(name, offset);
	return rotated == -1 ? Block_FindID(name) : rotated;
}

static int RotateCorner(cc_string* name, int offset) {
	float x = Game_SelectedPos.Intersect.X - (float)Game_SelectedPos.TranslatedPos.X;
	float z = Game_SelectedPos.Intersect.Z - (float)Game_SelectedPos.TranslatedPos.Z;

	if (x < 0.5f && z < 0.5f) {
		AutoRotate_Insert(name, offset, "-NW");
	} else if (x >= 0.5f && z < 0.5f) {
		AutoRotate_Insert(name, offset, "-NE");
	} else if (x < 0.5f && z >= 0.5f) {
		AutoRotate_Insert(name, offset, "-SW");
	} else if (x >= 0.5f && z >= 0.5f) {
		AutoRotate_Insert(name, offset, "-SE");
	}
	return GetRotated(name, offset);
}

static int RotateVertical(cc_string* name, int offset) {
	float y = Game_SelectedPos.Intersect.Y - (float)Game_SelectedPos.TranslatedPos.Y;

	if (y >= 0.5f) {
		AutoRotate_Insert(name, offset, "-U");
	} else {
		AutoRotate_Insert(name, offset, "-D");
	}
	return GetRotated(name, offset);
}

static int RotateFence(cc_string* name, int offset) {
	float yaw;
	/* Fence type blocks */
	yaw = LocalPlayer_Instance.Base.Yaw;
	yaw = LocationUpdate_Clamp(yaw);

	if (yaw < 45.0f || (yaw >= 135.0f && yaw < 225.0f) || yaw > 315.0f) {
		AutoRotate_Insert(name, offset, "-WE");
	} else {
		AutoRotate_Insert(name, offset, "-NS");
	}
	return GetRotated(name, offset);
}

static int RotatePillar(cc_string* name, int offset) {
	/* Thin pillar type blocks */
	Face face = Game_SelectedPos.Closest;

	if (face == FACE_YMAX || face == FACE_YMIN) {
		AutoRotate_Insert(name, offset, "-UD");
	} else if (face == FACE_XMAX || face == FACE_XMIN) {
		AutoRotate_Insert(name, offset, "-WE");
	} else if (face == FACE_ZMAX || face == FACE_ZMIN) {
		AutoRotate_Insert(name, offset, "-NS");
	}
	return GetRotated(name, offset);
}

static int RotateDirection(cc_string* name, int offset) {
	float yaw;
	yaw = LocalPlayer_Instance.Base.Yaw;
	yaw = LocationUpdate_Clamp(yaw);

	if (yaw >= 45.0f && yaw < 135.0f) {
		AutoRotate_Insert(name, offset, "-E");
	} else if (yaw >= 135.0f && yaw < 225.0f) {
		AutoRotate_Insert(name, offset, "-S");
	} else if (yaw >= 225.0f && yaw < 315.0f) {
		AutoRotate_Insert(name, offset, "-W");
	} else {
		AutoRotate_Insert(name, offset, "-N");
	}
	return GetRotated(name, offset);
}

static int FindRotated(cc_string* name, int offset) {
	cc_string dir;
	int group;
	int dirIndex = String_LastIndexOfAt(name, offset, '-');
	if (dirIndex == -1) return -1; /* not a directional block */

	dir = String_UNSAFE_SubstringAt(name, dirIndex);
	dir.length -= offset;
	if (dir.length > 3) return -1;

	offset += dir.length;
	group = AR_CalcGroup(&dir);

	if (group == AR_GROUP_CORNERS) return RotateCorner(name, offset);
	if (group == AR_GROUP_VERTICAL) return RotateVertical(name, offset);
	if (group == AR_GROUP_DIRECTION) return RotateDirection(name, offset);

	if (group == AR_GROUP_PILLAR) {
		AutoRotate_Insert(name, offset, "-UD");
		if (Block_FindID(name) == -1) {
			return RotateFence(name, offset);
		} else {
			return RotatePillar(name, offset);
		}
	}
	return -1;
}

BlockID AutoRotate_RotateBlock(BlockID block) {
	cc_string str; char strBuffer[STRING_SIZE * 2];
	cc_string name;
	int rotated;
	
	name = Block_UNSAFE_GetName(block);
	String_InitArray(str, strBuffer);
	String_AppendString(&str, &name);

	/* need to copy since we change characters in name */
	rotated = FindRotated(&str, 0);
	return rotated == -1 ? block : (BlockID)rotated;
}

static void GetAutoRotateTypes(cc_string* name, int* dirTypes) {
	int dirIndex, i;
	cc_string dir;
	dirTypes[0] = -1;
	dirTypes[1] = -1;

	for (i = 0; i < 2; i++) {
		/* index of rightmost group separated by dashes */
		dirIndex = String_LastIndexOf(name, '-');
		if (dirIndex == -1) return;

		dir = String_UNSAFE_SubstringAt(name, dirIndex);
		dirTypes[i]  = AR_CalcGroup(&dir);
		name->length = dirIndex;
	}
}

cc_bool AutoRotate_BlocksShareGroup(BlockID block, BlockID other) {
	cc_string bName, oName;
	int bDirTypes[2], oDirTypes[2];

	bName = Block_UNSAFE_GetName(block);
	GetAutoRotateTypes(&bName, bDirTypes);
	if (bDirTypes[0] == -1) return false;

	oName = Block_UNSAFE_GetName(other);
	GetAutoRotateTypes(&oName, oDirTypes);
	if (oDirTypes[0] == -1) return false;

	return bDirTypes[0] == oDirTypes[0] && bDirTypes[1] == oDirTypes[1] 
		&& String_CaselessEquals(&bName, &oName);
}

#include "Platform.h"
/*########################################################################################################################*
*----------------------------------------------------Blocks component-----------------------------------------------------*
*#########################################################################################################################*/
static void OnReset(void) {
	int i, block;
	for (i = 0; i < Array_Elems(definedCustomBlocks); i++) {
		definedCustomBlocks[i] = 0;
	}

	for (block = BLOCK_AIR; block < BLOCK_COUNT; block++) {
		Block_ResetProps((BlockID)block);
	}

	Block_UpdateAllCulling();
	Block_RecalculateAllSpriteBB();

	for (block = BLOCK_AIR; block < BLOCK_COUNT; block++) {
		Blocks.CanPlace[block]  = true;
		Blocks.CanDelete[block] = true;
	}
}

static void OnAtlasChanged(void* obj) { Block_RecalculateAllSpriteBB(); }
static void OnInit(void) {
	AutoRotate_Enabled = true;
	Event_Register_(&TextureEvents.AtlasChanged, NULL, OnAtlasChanged);
	OnReset();
}

struct IGameComponent Blocks_Component = {
	OnInit,  /* Init  */
	NULL,    /* Free  */
	OnReset, /* Reset */
};

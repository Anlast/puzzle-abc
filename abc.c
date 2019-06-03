/*
 * abc.c - an implementation of Tetsuya Nishio's & Naoki Inaba's 'ABC' puzzle
 * 
 * The puzzle fails to generate grids of grid size bigger than 5 and letter count equivalent to the grid size (i.e. grid 6x6 with count 6)
 *
 * There also exists a similar puzzle called 'Blood group' which
 * can be also found on Naoki Inaba's puzzles webpage.
 * 
 * TODO:
 * - far_pos elim is wrong: the counting of val_other is incorrect!
 * - solver forcing as in latin.c (adapted for empty cells)
 * - see fix in STANDALONE_SOLVER
 * - add immutable squares
 * - finish DF_CLUE_DONE in drawing routine
 * - improve the solver so that it can actually solve all the puzzles from the web 
 *	 (perhaps add more difficulty levels with this 
 *	  and difficulty grading in the standalone solver)
 * - add printing routines
 * - error checking to highlight wrong input on obvious cells(i.e. edge neigbour)
 *
 * BUGS:
 * - tricky difficulty not working correctly with immutable squares
 * - new_game_desc doesn't generate games with immutable squares
 * - dup_game doesn't work properly with blank clues (maybe due to blanks stored as '\0's?)
 * - letter F not shown in the solution on A-F
 * - holding 'N'(new game) throws assertion abc.c line 879
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <math.h> /* FIX: just for (char)log2 */

#include "puzzles.h"
#include "latin.h"

typedef unsigned char byte;

#ifdef STANDALONE_SOLVER
int solver_show_working, verbose;
#endif

/*
 * Difficulty levels. I do some macro ickery here to ensure that my
 * enum and the various forms of my name list always match up.
 */
#define DIFFLIST(A) \
    A(EASY,Easy,e) \
    A(TRICKY,Tricky,t)
#define ENUM(upper,title,lower) DIFF_ ## upper,
#define TITLE(upper,title,lower) #title,
#define ENCODE(upper,title,lower) #lower
#define CONFIG(upper,title,lower) ":" #title
enum { DIFFLIST(ENUM) DIFFCOUNT };
static char const *const abc_diffnames[] = { DIFFLIST(TITLE) };
static char const abc_diffchars[] = DIFFLIST(ENCODE);
#define DIFFCONFIG DIFFLIST(CONFIG)

enum {
	COL_BACKGROUND,
	COL_GRID,
	COL_USER,
	COL_HIGHLIGHT,
	COL_ERROR,
	COL_PENCIL,
	COL_DONE,
	COL_EDGE,
	NCOLOURS
};

struct game_params {
	int wh, max, diff;
};

struct edges {
	byte *top, *bottom, *left, *right;
};

struct game_state {
	game_params par;
	struct edges edges;
	bool *clues_completed;
	byte *grid;
	byte *pencil;
	bool *immutable;
	bool completed, cheated;
};

static game_params *default_params(void)
{
	game_params *ret = snew(game_params);
	ret->wh = 5;
	ret->max = 3;
	ret->diff = DIFF_EASY;
	return ret;
}

static const struct game_params abc_presets[] = {
    {  4, 3, DIFF_EASY	   },
    {  4, 3, DIFF_TRICKY   },
    {  4, 4, DIFF_TRICKY     },
    {  5, 3, DIFF_EASY   },
    {  5, 3, DIFF_TRICKY     },
    {  5, 4, DIFF_TRICKY   },
    {  5, 5, DIFF_TRICKY 	   },
	{  6, 3, DIFF_TRICKY   },
	{  6, 4, DIFF_TRICKY   },
	{  6, 5, DIFF_TRICKY   },
	{  6, 6, DIFF_TRICKY   },
};

static bool game_fetch_preset(int i, char **name, game_params **params)
{
    game_params *ret;
    char buf[80];

    if (i < 0 || i >= lenof(abc_presets))
        return false;

    ret = snew(game_params);
    *ret = abc_presets[i]; /* structure copy */

    sprintf(buf, "%dx%d A-%c %s", ret->wh, ret->wh, 'A'+(ret->max-1), abc_diffnames[ret->diff]);

    *name = dupstr(buf);
    *params = ret;
    return true;
}

static void free_params(game_params *params)
{
    sfree(params);
}

static game_params *dup_params(const game_params *params)
{
    game_params *ret = snew(game_params);
    *ret = *params;
    return ret;
}

static void decode_params(game_params *params, char const *string)
{
    char const *p = string;

    params->wh = atoi(p);
    while (*p && isdigit((unsigned char)*p)) p++;

	if (*p == 'c')
	params->max = atoi(++p);

    if (*p == 'd') {
        int i;
        p++;
        params->diff = DIFFCOUNT+1; /* ...which is invalid */
        if (*p) {
            for (i = 0; i < DIFFCOUNT; i++) {
                if (*p == abc_diffchars[i])
                    params->diff = i;
            }
            p++;
        }
    }
}

static char *encode_params(const game_params *params, bool full)
{
    char ret[80];

    sprintf(ret, "%dc%d", params->wh, params->max);
    if (full)
        sprintf(ret + strlen(ret), "d%c", abc_diffchars[params->diff]);

    return dupstr(ret);
}

static config_item *game_configure(const game_params *params)
{
	config_item *ret;
	char buf[80];
	
	ret = snewn(4, config_item);
	
	ret[0].name = "Grid size";
    ret[0].type = C_STRING;
    sprintf(buf, "%d", params->wh);
    ret[0].u.string.sval = dupstr(buf);
	
    ret[1].name = "Letter count"; /* FIX: */
    ret[1].type = C_STRING;
    sprintf(buf, "%d", params->max);
    ret[1].u.string.sval = dupstr(buf);
	
	ret[2].name = "Difficulty";
    ret[2].type = C_CHOICES;
    ret[2].u.choices.choicenames = DIFFCONFIG;
    ret[2].u.choices.selected = params->diff;
	
	ret[3].name = NULL;
    ret[3].type = C_END;
	
	return ret;
}

static game_params *custom_params(const config_item *cfg)
{
    game_params *ret = snew(game_params);

    ret->wh = atoi(cfg[0].u.string.sval);
	ret->max = atoi(cfg[1].u.string.sval);
	ret->diff = cfg[2].u.choices.selected;

    return ret;
}

static const char *validate_params(const game_params *params, bool full)
{
    if (params->wh < 3)
		return "Grid size must be at least 3.";
	if(params->max < 2 || params->max > params->wh)
		return "Letter count must be between 2 and the grid size.";
	if (params->diff >= DIFFCOUNT)
        return "Unknown difficulty rating";
	
	return NULL; 
}

 /* ----------------------------------------------------------------------
 * Solver.
 */
 
enum {
	MASK_X = 0x10,
	MASK_CURSOR = 0x20,
	MASK_PENCIL = 0x40,
	MASK_CLUE_DONE = 0x80
};

struct solver_usage {
	game_params par;
	/* Final deductions */
	byte *grid;
	/* Keeping track of possibilities for each cell, bitmaps using bits 1<<1..1<<n */
	byte *values;
	/* Values still left to be allocated in each row and column */
    byte *row, *col;
	/* Switch on every bit */
	byte all;
	int area;
};

static struct solver_usage *new_solver_usage(const game_params *params) 
{	
	struct solver_usage *ret = snew(struct solver_usage);
	ret->par = *params;
	ret->area = ret->par.wh*ret->par.wh;
	ret->par.max = params->max;
	ret->grid = snewn(ret->area, byte);
	ret->values = snewn(ret->area, byte);
	ret->row = snewn(ret->par.wh, byte);
	ret->col = snewn(ret->par.wh, byte);
	ret->all = (1 << ret->par.max)-1;
	
	memset(ret->grid, 0, ret->area);
	memset(ret->values, ret->all, ret->area);
	memset(ret->row, ret->all, ret->par.wh);
	memset(ret->col, ret->all, ret->par.wh);
	
	return ret;
}

static void free_solver_usage(struct solver_usage *usage) {
	sfree(usage->grid);
	sfree(usage->values);
	sfree(usage->row);
	sfree(usage->col);
	sfree(usage);
}

/* Helper macros for the solver, the program uses bit masks to encode possible values for each cell */
#define solver_removeval(idx, val) (usage->values[idx] &= ~(val))
#define solver_isunique(val) ((val & (val - 1)) == 0)
#define solver_ispossible(val, invalues) (((invalues) & (val)) > 0) /* at least one value is possible */
#define solver_isassigned(idx) (usage->grid[idx] > 0)

/* Place values in a final grid and eliminate the possible value in the according row and column */
static void solver_place(struct solver_usage *usage, int idx, byte val, const struct edges *edges) {
	int row = idx/usage->par.wh;
	int col = idx%usage->par.wh;
    int i;
	
	usage->grid[idx] = val;
	usage->values[idx] = 0;
	
	/* Remove possible values from a row */
	for(i = idx-col; i<idx-col+usage->par.wh; i++)
		solver_removeval(i, val);
	/* Remove possible values from a column */
	for(i = col; i<usage->area; i+=usage->par.wh)
		solver_removeval(i, val);
	
	/* If the placed value is the same as clue value on the edge, then
	 * eliminate all values towards the edge */
	 
	if(val != edges->bottom[col])
	for(i = idx; i >= col; i-=usage->par.wh)
		solver_removeval(i, edges->bottom[col]);
	
	if(val != edges->top[col])
	for(i = idx; i <= usage->par.wh*(usage->par.wh-1) + col; i+=usage->par.wh)
		solver_removeval(i, edges->top[col]);
	
	if(val != edges->right[row])
	for(i = idx; i >= row*usage->par.wh+col; i--)
		solver_removeval(i, edges->right[row]);
	
	if(val != edges->left[row])
	for(i = idx; i <= row*(usage->par.wh+1)-1; i++)
		solver_removeval(i, edges->left[row]);
	
	/* Remove value from to allocate list */
	usage->row[row] &= ~val;
	usage->col[col] &= ~val;
}

/* Determine if there is a single slot for a value in the whole row or column
 * (first avaliable slot is saved in the idx parameter) */
static bool solver_isunique_inrow(struct solver_usage *usage, int row, byte val, int *idx) {
	byte count = 0;
	int i;
	for(i = row*usage->par.wh; i < (row+1)*usage->par.wh; i++)
	if(solver_ispossible(val, usage->values[i]))
	{
		if(++count > 1)
			return false;
		*idx = i;
	}
	
	return (count == 1);
}
	
static bool solver_isunique_incol(struct solver_usage *usage, int col, byte val, int *idx) {
	byte count = 0;
	
	int i;
	for(i = col; i < usage->area; i+=usage->par.wh)
	if(solver_ispossible(val, usage->values[i]))
	{
		if(++count > 1)
			return false;
		*idx = i;
	}
	
	return (count == 1);
}

/* Eliminate edge value past(>=) the closest of the others' furthest posibble cells,
 * otherwise that other value would be the closest to the edge */
static bool solver_elim_farpos_inrow(struct solver_usage *usage, const struct edges *edges, int row) {
	bool ret = false;
	
	byte val_elim;
	byte val_other;
	
	int row_beg = row*usage->par.wh;
	int row_end = (row+1)*usage->par.wh-1;
	int j, c;
	
	/* Left cell */
	if(edges->left[row] && solver_ispossible(edges->left[row], usage->row[row])) { /* If value had been placed already it would have been eliminated also */ 
	val_elim = edges->left[row];
	val_other = usage->all ^ val_elim;
	j = row_end;
	/* Begin the search from the opposite end */
	c = 0;
	while((c += ((val_other & (usage->values[j] | usage->grid[j]))) >= 1) < (usage->par.max-1) && j > row_beg)
	{
		j--;
	}
	for( ; j <= row_end; j++)
	if(solver_ispossible(val_elim, usage->values[j])) {
		if(j == row_beg) // FIX: MOVE THIS OUTSIDE IF
			continue;
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("far pos elim(from left): row %d \n"
				   "\telim val %d idx %d\n", row, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	/* Right cell */
	if(edges->right[row] && solver_ispossible(edges->right[row], usage->row[row])) {
	val_elim = edges->right[row];
	val_other = usage->all ^ val_elim;
	j = row_beg;
	c = 0;
	while((c += ((val_other & (usage->values[j] | usage->grid[j])) >= 1)) < (usage->par.max-1) && j < row_end)
	{
		j++;
	}
	for( ; j >= row_beg; j--)
	if(solver_ispossible(val_elim, usage->values[j])) {
		if(j == row_end)
			continue;
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("far pos elim(from right): row %d \n"
				   "\telim val %d idx %d\n", row, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	return ret;
}

static bool solver_elim_farpos_incol(struct solver_usage *usage, const struct edges *edges, int col) {
	bool ret = false;
	
	byte val_elim;
	byte val_other;
	
	int col_beg = col;
	int col_end = (usage->par.wh-1)*usage->par.wh + col;
	int j, c;
	
	/* Make sure top edge letter isn't the furthest */
	if(edges->top[col] && solver_ispossible(edges->top[col], usage->col[col])) {
	val_elim = edges->top[col];
	val_other = usage->all ^ val_elim;
	j = col_end;
	c = 0;
	while((c += ((val_other & (usage->values[j] | usage->grid[j]))) >= 1) < (usage->par.max-1) && j > col_beg)
	{
		val_other &= ~(usage->values[j] | usage->grid[j]); // FIX: ELIM ONLY ONE VAL NOT ALL OF THEM AT ONCE ----- THIS LOOP HAS TO BE COMPLETELY REMADE
		j-=usage->par.wh;
	}
	for( ; j <= col_end; j+=usage->par.wh)
	if(solver_ispossible(val_elim, usage->values[j]))
	{
		if(j == col_beg)
		 continue;
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("far pos elim(from top): col %d \n"
				   "\telim val %d idx %d\n", col, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	/* Bottom not the furthest */
	if(edges->bottom[col] && solver_ispossible(edges->bottom[col], usage->col[col])) {
	val_elim = edges->bottom[col];
	val_other = usage->all ^ val_elim;
	j = col_beg;
	c = 0;
	while((c += ((val_other & (usage->values[j] | usage->grid[j]))) >= 1) < (usage->par.max-1) && j < col_end)
	{
		val_other &= ~(usage->values[j] | usage->grid[j]); // FIX: ELIM ONLY ONE VAL NOT ALL OF THEM AT ONCE ----- THIS LOOP HAS TO BE COMPLETELY REMADE
		j+=usage->par.wh;
	}
	
	for( ; j < ; j+=usage->par.wh)
	{
		for(int i = 0; i < 
	for( ; j >= col_beg; j-=usage->par.wh)
	if(solver_ispossible(val_elim, usage->values[j]))
	{
		if(j == col_end)
			continue;
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("far pos elim(from bottom): col %d \n"
				   "\telim val %d idx %d\n", col, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	return ret;
}

/* Eliminate other values before the first possible edge cell value 
 * i.e. if edge cell is B, eliminate A and C before(<=) the first possible B */
static bool solver_elim_closepos_inrow(struct solver_usage *usage, const struct edges *edges, int row) {
	bool ret = false;
	byte val_elim, val_other;
	int row_beg = row*usage->par.wh;
	int row_end = row*usage->par.wh + usage->par.wh-1;
	int j;
	
	/* Make sure left edge letter isn't the furthest */
	if(edges->left[row])
	{
	val_elim = usage->all ^ edges->left[row];
	val_other = edges->left[row];
	j=row_beg;
	while(!(val_other & (usage->values[j] | usage->grid[j])) && j < row_end)
		j++;
	for( ; j>=row_beg; j--)
	if(solver_ispossible(val_elim, usage->values[j])) {
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("closest pos elimination at row %d\n"
				   "\tremoving %d at idx %d\n", row, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	/* Make sure right edge letter isn't the furthest */
	if(edges->right[row])
	{
	val_elim = usage->all ^ edges->right[row];
	val_other = edges->right[row];
	j=row_end;
	while(!(val_other & (usage->values[j] | usage->grid[j])) && j > row_beg)
		j--;
	for( ; j<=row_end; j++)
	if(solver_ispossible(val_elim, usage->values[j])) {
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("closest pos elimination at row %d\n"
				   "\tremoving %d at idx %d\n", row, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	return ret;
}

static bool solver_elim_closepos_incol(struct solver_usage *usage, const struct edges *edges, int col) {
	bool ret = false;
	
	byte val_elim;
	byte val_other;
	
	int col_beg = col;
	int col_end = (usage->par.wh-1)*usage->par.wh + col;
	int j;
	
	
	/* Make sure top edge letter isn't the furthest */
	if(edges->top[col])
	{
	val_elim = usage->all ^ edges->top[col];
	val_other = edges->top[col];
	j=col_beg;
	while(!(val_other & (usage->values[j] | usage->grid[j])) && j < col_end)
		j+=usage->par.wh;
	for( ; j>=col_beg; j-=usage->par.wh)
	if(solver_ispossible(val_elim, usage->values[j])) {
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
		   printf("closest pos elimination at col %d\n"
				  "\tremoving %d at idx %d\n", col, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	/* Make sure bottom edge letter isn't the furthest */
	if(edges->bottom[col])
	{
	val_elim = usage->all ^ edges->bottom[col];
	val_other = edges->bottom[col];
	j=col_end;
	while(!(val_other & (usage->values[j] | usage->grid[j])) && j > col_beg)
		j-=usage->par.wh;
	for( ; j<=col_end; j+=usage->par.wh)
	if(solver_ispossible(val_elim, usage->values[j]))
	{
		#ifdef STANDALONE_SOLVER
		if(solver_show_working && verbose)
			printf("closest pos elimination at col %d\n"
				   "\tremoving %d at idx %d\n", col, val_elim, j);
		#endif
		solver_removeval(j, val_elim);
		ret = true;
	}
	}
	
	return ret;
}

#ifdef STANDALONE_SOLVER
static void printvalues(struct solver_usage *usage) {
	printf("\n");
	int r, c;
	for(r = 0; r<usage->par.wh; r++) 
	{
		for(c = 0; c<usage->par.wh; c++)
		printf("%3d", usage->values[r*usage->par.wh+c]);
	printf("\n");
	}
}
#endif

/* Check if the grid corresponds to the edges */
static bool check_valid(const game_state *state, bool *reperr, bool *closerr)
{
	int w = state->par.wh;
	int max = state->par.max;
	int i, r, c;
	byte *grid = state->grid;
	byte lt, rb;
	bool errs = false;
	
	if (reperr && closerr)
	{
	for (i = 0; i < w*w; i++)
	    reperr[i] = false;
	for (i = 0; i < 4*w; i++)
		closerr[i] = false;
	}
	
	for (r = 0; r < w; r++) {
	byte mask = 0, errmask = 0;
	
	c = 0;
	while(c < w && !grid[r*w+c]) /* Find leftmost value */
		c++;
	mask |= grid[r*w+c];
	lt = mask;
	
	c++;
	for ( ; c < w; c++) {
	    unsigned long bit = grid[r*w+c];
	    errmask |= (mask & bit);
	    mask |= bit;
	}
	
	c = w-1;
	while(c >= 0 && !grid[r*w+c]) /* Find rightmost value */
		c--;
	rb = grid[r*w + c];

	if(reperr) {
	for (c = 0; c < w; c++)
		if (errmask & grid[r*w+c])
		reperr[r*w+c] = true;
	}
	
	
	if(mask == (1 << max) - 1)
	{	
		if(state->edges.left[r] && lt != state->edges.left[r])
		{
			if(closerr) // FIX: clean this up
			closerr[2*w+r] = true;
		
			errs = true;
		}
		if(state->edges.right[r] && rb != state->edges.right[r])
		{
			if(closerr)
			closerr[3*w+r] = true;
		
			errs = true;
		}
	}
	else 
		errs = true;
	}
	
	for (c = 0; c < w; c++) {
	byte mask = 0, errmask = 0;
	
	r = 0;
	while(r < w && !grid[r*w+c]) /* Find upmost value */
		r++;
	mask |= grid[r*w+c];
	lt = mask;
	r++;
	
	for ( ; r < w; r++) {
	    byte bit = grid[r*w+c];
	    errmask |= (mask & bit);
	    mask |= bit;
	}
	
	r = w-1;
	while(r >= 0 && !grid[r*w+c]) /* Find downmost value */
		r--;
	rb = grid[r*w + c];
	
	if(reperr)
	{
	for (r = 0; r < w; r++)
		if (errmask & grid[r*w+c])
		reperr[r*w+c] = true;
	}
	  
	if(mask == (1 << max) - 1)
	{
		if(state->edges.top[c] && lt != state->edges.top[c])
		{
			if(closerr)
			closerr[c] = true;
		
			errs = true;
		}
		
		if(state->edges.bottom[c] && rb != state->edges.bottom[c])
		{
			if(closerr)
			closerr[w+c] = true;
		
			errs = true;
		}
	}
	else 
		errs = true;
	}
	
	return !errs; /* success, no errors */
}

int count_set_bits(byte b)
{
	int c = 0;
	while(b != 0)
	{
		if(b & 1)
		c++;
		
		b >>= 1;
	}
	return c;
}

static bool solver(struct solver_usage *usage, const struct edges *edges)
{	
	bool res;
	int i, idx, val;
	int wh = usage->par.wh;
	
	do {
	res = false;
	
	/* Eliminating values which would contradict game's definition */
	for(i = 0; i<usage->par.wh; i++)
	res |= solver_elim_closepos_inrow(usage, edges, i) ||
		solver_elim_farpos_inrow(usage, edges, i) ||
		solver_elim_closepos_incol(usage, edges, i) ||
		solver_elim_farpos_incol(usage, edges, i);
	
	/* ----- new --- */
	int r, c, pos;
	for(r = 0; r<usage->par.wh; r++)
	{
		pos = 0;
		for(c = 0; c<usage->par.wh; c++)
		{
		if(usage->values[r*wh+c] != 0)
		pos++;
		}
		/* Available slots == count of values left to alloc */
		if(pos == count_set_bits(usage->row[r]))
		for(c = 0; c<usage->par.wh; c++)
		{
		if(count_set_bits(usage->values[r*usage->par.wh+c]) == 1)
		{
			#ifdef STANDALONE_SOLVER
			printf("New elim at row %d at idx %d\n", r, r*wh+c);
			#endif
			solver_place(usage, r*wh+c, usage->values[r*wh+c], edges);
		}
		}
	}
	
	for(c = 0; c<usage->par.wh; c++)
	{
		pos = 0;
		for(r = 0; r<usage->par.wh; r++)
		{
		if(usage->values[r*wh+c] != 0)
		pos++;
		}
		/* Available slots == count of values left to alloc */
		if(pos == count_set_bits(usage->col[c]))
		for(r = 0; r<usage->par.wh; r++)
		{
		if(count_set_bits(usage->values[r*usage->par.wh+c]) == 1)
		{
			#ifdef STANDALONE_SOLVER
			printf("New elim at col %d at idx %d\n", c, r*wh+c);
			#endif
			solver_place(usage, r*wh+c, usage->values[r*wh+c], edges);
		}
		}
	}			
		
	/* Place unique values in rows and columns */
	for(i = 0; i<usage->par.wh; i++)
	for(val = 1; val <= usage->all; val<<=1)
	{
		if(solver_ispossible(val, usage->row[i]) && /* Check if the value is required in this row */
			solver_isunique_inrow(usage, i, val, &idx))
		{
			#ifdef STANDALONE_SOLVER
			if(solver_show_working)
				printf("unique val in row %d\n"
			           "\tplacing %d at idx %d\n", i, val, idx);
			#endif
			solver_place(usage, idx, val, edges);
			res = true;
		}
	
		if(solver_ispossible(val, usage->col[i]) &&
		   solver_isunique_incol(usage, i, val, &idx))
		{
			#ifdef STANDALONE_SOLVER
			if(solver_show_working)
				printf("unique val in col %d\n"
					   "\tplacing %d at idx %d\n", i, val, idx, edges);
			#endif
			solver_place(usage, idx, val, edges);
			res = true;
		}
	}
	}while(res);
	
	res = true;
	
	/* FIX: fake game state for check_valid */
	game_state *temp = snew(game_state);
	temp->par.wh = usage->par.wh;
	temp->par.max = usage->par.max;
	temp->edges.top = edges->top;
	temp->edges.bottom = edges->bottom;
	temp->edges.left = edges->left;
	temp->edges.right = edges->right;
	temp->grid = usage->grid;
	
	/* Final check for board validity */
	if(!check_valid(temp, NULL, NULL))
		res = false;
	
	for(i = 0; i < usage->par.wh; i++)
	if(usage->row[i] > 0 || usage->col[i])
		res = false;
	
	#ifdef STANDALONE_SOLVER
	if(solver_show_working) {
		if(!res)
		{
		printf("solution not found.\n");
		if(verbose)
		{
			printvalues(usage);
			printf("\n");
		}
		}	
	}
	#endif

	sfree(temp);
	
	return res;
}

static char *new_game_desc(const game_params *params, random_state *rs,
			   char **aux, int interactive)
{
	#ifdef STANDALONE_SOLVER
	int idcounter = 0;
	#endif
	int wh = params->wh;
	int area = wh*wh;
	int i, r, c;
	char *desc, *p;
	byte *grid = NULL, *order;
	struct solver_usage *usage = new_solver_usage(params);
	struct edges edges;
	edges.top = snewn(4*wh, byte);
	edges.bottom = edges.top + wh;
	edges.left = edges.top + 2*wh;
	edges.right = edges.top+ 3*wh;
	byte *extra = snewn(area, byte);
	
	order = snewn(max(4*wh,area), byte);
	while(1)
	{
		bool ret = false; //
		#ifdef STANDALONE_SOLVER
		idcounter++;
		#endif

		if(!grid)
			sfree(grid);
		grid = latin_generate(wh, rs);
	
		for(i = 0; i < wh*wh; i++)
		if(grid[i] > params->max)
		grid[i] = 0;
		else 
		grid[i] = 1 << (grid[i] - 1);
	
	
		// FIX: This loop was designed with only A-C in mind. Revise this loop */
		/* Read the closest values to each edge */
		byte enc, val, valc; /* values encounter in the row or col so far */
 		for(r = 0; r < wh; r++)
		{
			enc = 0, val = 0, valc = 0;
			c = 0;
			while(valc < (params->max-1))
			{
				if((val = grid[r*wh+c]) > 0)
				{
					enc |= val;
					if(valc == 0)
						edges.left[r] = val;
					valc++;
				}
				c++;
			}
			edges.right[r] = usage->all ^ enc;
		}
		
		for(c = 0; c < wh; c++)
		{
			enc = 0, val = 0, valc = 0;
			r = 0;
			while(valc < (params->max-1))
			{
				if((val = grid[r*wh+c]) > 0)
				{
					enc |= val;
					if(valc == 0)
						edges.top[c] = val;
					valc++;
				}
				r++;
			}
			edges.bottom[c] = usage->all ^ enc;
		}
		
		memset(extra, 0, wh*wh);
		
		if(solver(usage, &edges))
		{
			#ifdef STANDALONE_SOLVER
			printf("idcounter:%d\n", idcounter);
			#endif
			break;
		}
		memset(usage->grid, 0, wh*wh);
		memset(usage->values, usage->all, wh*wh);
		memset(usage->row, usage->all, params->wh);
		memset(usage->col, usage->all, params->wh);
		
		
		/* FIX: Reveal some values on the board */
		/* ------- */
		
		int count = 0; // FIX: NOT ISOC90
		for (i = 0; i < area; i++)
			order[i] = i;
		shuffle(order, area, sizeof(*order), rs);
		for (i = 0; i < area && count < params->max-1; i++) { /* count limit is a somewhat arbitary */
			int j = order[i];
			if(!grid[j] || usage->grid[j])
				continue;
			solver_place(usage, j, grid[j], &edges);
			extra[j] = grid[j]; //
			++count;
			if(solver(usage, &edges))
				goto end; // FIX
		}
		
		/* ------ */
		
		memset(usage->grid, 0, wh*wh);
		memset(usage->values, usage->all, wh*wh);
		memset(usage->row, usage->all, params->wh);
		memset(usage->col, usage->all, params->wh);
	}
	end:
	/* Hide the unnecessary clues */
	if(params->diff > DIFF_EASY)
	{
	for (i = 0; i < 4*wh; i++)
	order[i] = i;
	shuffle(order, 4*wh, sizeof(*order), rs);
	for (i = 0; i < 4*wh; i++) {
		int j = order[i];
		int clue = edges.top[j];

		//memcpy(soln2, grid, a);
		edges.top[j] = 0;
		
		// ----
		memset(usage->values, usage->all, wh*wh);
		memset(usage->row, usage->all, params->wh);
		memset(usage->col, usage->all, params->wh);
		memset(usage->grid, 0, wh*wh);
		for(int k = 0; k < area; k++) // not ISOC90
		if(extra[k])
		solver_place(usage, k, extra[k], &edges);
		//-----
	
		
		if(!solver(usage, &edges))
			edges.top[j] = clue;
	}
	}
			memset(usage->values, usage->all, wh*wh);
		memset(usage->row, usage->all, params->wh);
		memset(usage->col, usage->all, params->wh);
		memset(usage->grid, 0, wh*wh);
		for(int k = 0; k < area; k++) // not ISOC90
		if(extra[k])
		solver_place(usage, k, extra[k], &edges);
	solver(usage, &edges);
	/*
     * Encode the puzzle description.
     */
    desc = snewn(40*area, char);
    p = desc;
    for (i = 0; i < 4*wh; i++) {
        if (i)
            *p++ = '/';
        if (edges.top[i])
            *p++ = (char)log2(edges.top[i]) + 'A';
    }
	/* FIX: immutable squares */
	/* ------ */
	
    for (i = 0; i < area; i++)
	if (extra[i])
	    break;
    if (i < area) {
	*p++ = ',';
	
	char temp[32];
	for( ; i < area; i++)
	if(extra[i])
	{
		itoa(i, temp, 10);
		char *t = temp;
		while(*t)*p++ = *t++;
			;
		*p++ = (char)log2(extra[i])+'A';
	}
    }
	
	/* ------ */
    *p++ = '\0';
    desc = sresize(desc, p - desc, char);
	
	/*
     * Encode the solution.
     */
	if (*aux)
		sfree(*aux);
	*aux = snewn(area+2, char);
	(*aux)[0] = 'S';
	memcpy((*aux)+1, usage->grid, area);
	for(i = 1; i <= wh*wh; i++)
	if((*aux)[i] == 0)
		(*aux)[i] = ' ';
	(*aux)[area+1] = '\0';
	
	sfree(extra);
	sfree(order);
	sfree(edges.top);
	free_solver_usage(usage);

	#ifdef STANDALONE_SOLVER
	printf("idcounter:%d\n", idcounter);
	#endif
	
	return desc;
}

static const char *validate_desc(const game_params *params, const char *desc)
{
    int w = params->wh, a = w*w;
    const char *p = desc;
    int i, clue;

    /*
     * Verify that the right number of clues are given, and that
     * they're in range.
     */
    for (i = 0; i < 4*w; i++) {
	if (!*p)
	    return "Too few clues for grid size";

	if (i > 0) {
	    if (*p != '/')
		return "Expected commas between clues";
	    p++;
	}

	if (isalpha((unsigned char)*p)) {
	    if (*p < 'A' || *p > 'A'+params->max-1)
		return "Clue letter out of range";
		
		p++;
	}
    }
    if (*p == '/')
	return "Too many clues for grid size";
	
	if(*p == ',') {
	int pos = 0;
	p++;
	while (*p) {
	    if (isdigit(*p)) {
			pos = atoi(p);
			if(pos < 0 || pos >= a)
			return "Invalid grid index";
		
			while (*p && isdigit((unsigned char)*p)) p++;
		} else if (*p == '_') {
		/* do nothing */;
	    } else if (isalpha(*p)) {
			if(*p < 'A' || *p > 'A' + params->max - 1)
			return "Grid letter out of range";
			
			p++;
		}
	    else
			return "Invalid character in game description";
	}
	}
	
    return NULL;
}

static key_label *game_request_keys(const game_params *params, int *nkeys)
{
    int i;
    key_label *keys = snewn(params->max, key_label);
    *nkeys = params->max + 1;

    for (i = 0; i < params->max; i++) {
		keys[i].button = 'a' + i;
        keys[i].label = NULL;
    }
	
    keys[params->max].button = '\b';
    keys[params->max].label = NULL;

    return keys;
}

static game_state *new_game(midend *me, const game_params *params,
                            const char *desc)
{
	game_state *state = snew(game_state);
	
	const char *p = desc;
	int wh, area;
    int i;
	state->par.wh = wh = params->wh;
	state->par.max = params->max;
	area = wh*wh;
	
	state->edges.top = snewn(4*wh, byte);
	state->edges.bottom = state->edges.top + wh;
	state->edges.left = state->edges.top + 2*wh;
	state->edges.right = state->edges.top + 3*wh;
	
	for (i = 0; i < 4*wh; i++) {
	if (i > 0) {
	    assert(*p == '/');
	    p++;
	}
	if (*p && isalpha((unsigned char)*p)) {
	    state->edges.top[i] = 1 << (*p - 'A');
	    p++;
	} else
	    state->edges.top[i] = 0;
    }

	state->clues_completed = snewn(4*wh, bool);
	memset(state->clues_completed, 0, 4*wh);
	
	state->grid = snewn(area, byte);
	memset(state->grid, 0, area);
	state->pencil = snewn(area, byte);
	memset(state->pencil, 0, area);
	
	state->immutable = snewn(area, bool);
	memset(state->immutable, 0, area);
	
	if (*p == ',') {
	int pos = 0;
	p++;
	while (*p) {
	    if (isdigit(*p)) {
		pos = atoi(p);
		while (*p && isdigit((unsigned char)*p)) p++;
	    } else if (*p == '_') {
		/* do nothing */;
	    } else if (isalpha(*p)) {
			state->grid[pos] = 1 << (*p - 'A');
			state->immutable[pos] = true;
			p++;
		}
	    else
			assert(!"Corrupt game description");
	}
	}
    assert(!*p);
	
	state->completed = state->cheated = false;
	
	return state;
}

static game_state *dup_game(const game_state *state)
{	
	int wh, area;
	wh = state->par.wh;
	area = wh*wh;
	
	game_state *ret = snew(game_state);
	
	ret->par = state->par;
	
	ret->edges.top = state->edges.top;
	ret->edges.bottom = state->edges.top + wh;
	ret->edges.left = state->edges.top + 2*wh;
	ret->edges.right = state->edges.top + 3*wh;
	ret->immutable = state->immutable;
	
	ret->clues_completed = snewn(4*wh, bool);
	memcpy(ret->clues_completed, state->clues_completed, 4*wh*sizeof(bool));
	ret->grid = snewn(area, byte);
	memcpy(ret->grid, state->grid, area);
	ret->pencil = snewn(area, byte);
	memcpy(ret->pencil, state->pencil, area);
	
	ret->completed = state->completed;
	ret->cheated = state->cheated;
	
	return ret;
}

static void free_game(game_state *state)
{
	sfree(state->grid);
	sfree(state->pencil);
	sfree(state->clues_completed);
	sfree(state);
}
		
static char *solve_game(const game_state *state, const game_state *currstate,
                        const char *aux, const char **error)
{
	char *ret;
	int wh = state->par.wh;
	int i ;
    game_params params; /* faking params for solver_usage */
    params.wh = wh;
	params.max = state->par.max;
	
	if (aux)
       return dupstr(aux);
	
	ret	= snewn(wh*wh+2, char);
	ret[0] = 'S';
	ret[wh*wh+1] = '\0';
	
	*error = NULL;
	
    {
    struct solver_usage *usage = new_solver_usage(&params);
	if(!solver(usage, &state->edges))
	{
		*error = "Solution not found.";
		free_solver_usage(usage);
		return NULL;
	}
	
	memcpy(ret+1, usage->grid, wh*wh);
	
	for(i = 1; i <= wh*wh; i++)
	if(ret[i] == 0)
		ret[i] = ' ';
	
	free_solver_usage(usage);
    }
	
	return ret;
}

static bool game_can_format_as_text_now(const game_params *params)
{
    return true;
}

static char *game_text_format(const game_state *state)
{
	int wh = state->par.wh; /* , a = wh*wh */;
    char *ret;
    char *p;
    int x, y;
    int total;

    /*
	 * same as towers.c
     */
	
    total = 2*wh*wh + 10*wh + 9;
    ret = snewn(total, char);
    p = ret;

    /* Top clue row. */
    *p++ = ' '; *p++ = ' ';
    for (x = 0; x < wh; x++) {
	*p++ = ' ';
	*p++ = (state->edges.top[x] ? 'A' + (char)log2(state->edges.top[x]) : ' ');
    }
    *p++ = '\n';

    /* Blank line. */
    *p++ = '\n';

    /* Main grid. */
    for (y = 0; y < wh; y++) {
	*p++ = (state->edges.top[y+2*wh] ? 'A' + (char)log2(state->edges.top[y+2*wh]) :
		' ');
	*p++ = ' ';
	for (x = 0; x < wh; x++) {
	    *p++ = ' ';
	    *p++ = (state->grid[y*wh+x] ? 'A' + (char)log2(state->grid[y*wh+x]) : ' ');
	}
	*p++ = ' '; *p++ = ' ';
	*p++ = (state->edges.top[y+3*wh] ? 'A' + (char)log2(state->edges.top[y+3*wh]) :
		' ');
	*p++ = '\n';
    }

    /* Blank line. */
    *p++ = '\n';

    /* Bottom clue row. */
    *p++ = ' '; *p++ = ' ';
    for (x = 0; x < wh; x++) {
	*p++ = ' ';
	*p++ = (state->edges.top[x+wh] ? 'A' + (char)log2(state->edges.top[x+wh]) :
		' ');
    }
    *p++ = '\n';

    *p++ = '\0';
    assert(p == ret + total);

    return ret;
}

struct game_ui {
    int hx, hy;
    bool hshow, hpencil, hcursor;
};

static game_ui *new_ui(const game_state *state)
{
    game_ui *ui = snew(game_ui);

    ui->hx = ui->hy = -1;
    ui->hpencil = ui->hshow = ui->hcursor = false;

    return ui;
}

static void free_ui(game_ui *ui)
{
	sfree(ui);
}

static char *encode_ui(const game_ui *ui)
{
    return NULL;
}

static void decode_ui(game_ui *ui, const char *encoding)
{
}

static void game_changed_state(game_ui *ui, const game_state *oldstate,
                               const game_state *newstate)
{
	int wh = newstate->par.wh;
    /* Prevent pencil-mode highlighting */
    if (ui->hshow && ui->hpencil && !ui->hcursor &&
        newstate->grid[ui->hy * wh + ui->hx] != 0) {
        ui->hshow = 0;
    }
}

#define PREFERRED_TILESIZE 48
#define FLASH_TIME 0.4F
#define TILE_SIZE (ds->tilesize)
#define BORDER (TILE_SIZE / 2)
#define GRIDEXTRA max((TILE_SIZE / 32),1)
#define CLUEPOS(x, y, index, w) do { \
    if (index < w) \
	x = index, y = -1; \
    else if (index < 2*w) \
	x = index-w, y = w; \
    else if (index < 3*w) \
	x = -1, y = index-2*w; \
    else \
	x = w, y = index-3*w; \
} while (0)
	
#define DF_PENCIL_SHIFT 16
#define DF_CLUE_DONE 0x10000
#define DF_ERROR 0x8000
#define DF_HIGHLIGHT 0x4000
#define DF_HIGHLIGHT_PENCIL 0x2000
#define DF_IMMUTABLE 0x1000
#define DF_PENCIL_XMARK 0x0400
#define DF_DIGIT_MASK 0x00FF


struct game_drawstate {
    bool started;
    int wh;
    int tilesize;
	long *clues;
	bool *clues_completed;
    long *grid;
    byte *pencil;
	bool *err1;
	bool *err2;
};

static int clue_index(const game_state *state, int x, int y)
{
    int w = state->par.wh;

    if (x == -1 || x == w)
        return w * (x == -1 ? 2 : 3) + y;
    else if (y == -1 || y == w)
        return (y == -1 ? 0 : w) + x;

    return -1;
}

static bool is_clue(const game_state *state, int x, int y)
{
    int w = state->par.wh;

    if (((x == -1 || x == w) && y >= 0 && y < w) ||
        ((y == -1 || y == w) && x >= 0 && x < w))
            return true;

    return false;
}

static char *interpret_move(const game_state *state, game_ui *ui,
                            const game_drawstate *ds,
                            int x, int y, int button)
{
    int wh = state->par.wh;
    int tx, ty;
    char buf[80];
	
	button &= ~MOD_MASK;

    tx = (x + TILE_SIZE - BORDER) / TILE_SIZE - 2;
    ty = (y + TILE_SIZE - BORDER) / TILE_SIZE - 2;
	
    if (tx >= 0 && tx < wh && ty >= 0 && ty < wh) {
        if (button == LEFT_BUTTON) {
			if (tx == ui->hx && ty == ui->hy &&
                       ui->hshow && !ui->hpencil) {
                ui->hshow = 0;
            } else {
                ui->hx = tx;
                ui->hy = ty;
                ui->hshow = true;
                ui->hpencil = false;
            }
            ui->hcursor = false;
            return UI_UPDATE;
        }
        if (button == RIGHT_BUTTON) {
            /*
             * Pencil-mode highlighting for non filled squares.
             */
            if (state->grid[ty*wh+tx] == 0) {
                if (tx == ui->hx && ty == ui->hy &&
                    ui->hshow && ui->hpencil) {
                    ui->hshow = 0;
                } else {
                    ui->hpencil = true;
                    ui->hx = tx;
                    ui->hy = ty;
                    ui->hshow = true;
                }
            } else {
                ui->hshow = false;
            }
            ui->hcursor = false;
            return UI_UPDATE;
        }
    } else if (button == LEFT_BUTTON) {
        if (is_clue(state, tx, ty)) {
            sprintf(buf, "%c%d,%d", 'D', tx, ty);
            return dupstr(buf);
       }
    }
			
	if (IS_CURSOR_MOVE(button)) {
        move_cursor(button, &ui->hx, &ui->hy, wh, wh, 0);
        ui->hshow = ui->hcursor = true;
        return UI_UPDATE;
    }
	
    if (ui->hshow &&
        (button == CURSOR_SELECT)) {
        ui->hpencil ^= 1;
        ui->hcursor = true;
        return UI_UPDATE;
    }

    if (ui->hshow &&
	   ((button >= 'A' && button < 'A' + state->par.max)||
	    (button >= 'a' && button < 'a' + state->par.max)||
	     button == CURSOR_SELECT2 || button == '\b')) {
		int n;
		if (button >= 'A' && button < 'A' + state->par.max)
			n = button - 'A' + 1;
		else if (button >= 'a' && button < 'a' + state->par.max)
			n = button - 'a' + 1;
		else if (button == CURSOR_SELECT2 || button == '\b')
			n = 0;

        /*
         * Can't make pencil marks in a filled square. Again, this
         * can only become highlighted if we're using cursor keys.
         */
        if (ui->hpencil && state->grid[ui->hy*wh+ui->hx])
            return NULL;

		sprintf(buf, "%c%d,%d,%d",
			    (char)(ui->hpencil && n > 0 ? 'P' : 'R'), ui->hx, ui->hy, n);

        if (!ui->hcursor) 
			ui->hshow = false;
	

		return dupstr(buf);
    }
	
	
	// FIX 'X'
	#if 0
	if(ui->hshow && (button == 'X' || button == 'x'))
	{
		sprintf(buf, "%c%d,%d", (char)('X'), ui->hx, ui->hy);
		
		if (!ui->hcursor) 
			ui->hshow = false;
		
		return dupstr(buf);
	}
	#endif

    if (button == 'M' || button == 'm')
        return dupstr("M");

    return NULL;
}

static game_state *execute_move(const game_state *from, const char *move)
{
    int wh = from->par.wh;
    game_state *ret;
    int x, y, n, i;

    if (move[0] == 'S') {
		ret = dup_game(from);
		ret->completed = ret->cheated = true;
		
		for(i = 1; i < wh*wh+1; i++)
		if((move[i] & ~((1 << from->par.max)-1)) && move[i] != ' ')
		{
			free_game(ret);
			return NULL;
		}
		
		if(move[wh*wh+1] != '\0')
		{
			free_game(ret);
			return NULL;
		}
		
		memcpy(ret->grid, move+1, wh*wh);
		
		for(i = 0; i < wh*wh; i++)
		if(ret->grid[i] == ' ')
				ret->grid[i] = 0;
			
		memset(ret->pencil, 0, wh*wh);

		return ret;
    } else if ((move[0] == 'P' || move[0] == 'R') &&
				sscanf(move+1, "%d,%d,%d", &x, &y, &n) == 3 &&
				x >= 0 && x < wh && y >= 0 && y < wh && n >= 0 && n <= from->par.max) {
		ret = dup_game(from);
        if (move[0] == 'P' && n > 0)
            ret->pencil[y*wh+x] ^= 1 << (n-1);
        else {
            ret->grid[y*wh+x] = 1 << (n-1);
            ret->pencil[y*wh+x] = 0;
			
            if (!ret->completed && check_valid(ret, NULL, NULL))
                ret->completed = true;
		}
		
		return ret;
    } else if(move[0] == 'X' && sscanf(move+1, "%d,%d", &x, &y) == 2 &&
			x >= 0 && x < wh && y >= 0 && y < wh) {
		ret = dup_game(from);
		ret->grid[y*wh+x] = 0;
		ret->pencil[y*wh+x] = 8;
		return ret;
	} else if(move[0] == 'M') {
        int i;
		ret = dup_game(from);
		for(i = 0; i < wh*wh; i++)
		if(!ret->grid[i] && !(ret->pencil[i] & MASK_X))
			ret->pencil[i] = (1 << from->par.max)-1;
		
		return ret;
	} else if (move[0] == 'D' && sscanf(move+1, "%d,%d", &x, &y) == 2 &&
               is_clue(from, x, y)) {
        int index = clue_index(from, x, y);
		ret = dup_game(from);
        ret->clues_completed[index] = !ret->clues_completed[index];
        return ret;
    }
	
	return NULL;
}

/* ----------------------------------------------------------------------
 * Drawing routines.
 */
static void game_compute_size(const game_params *params, int tilesize,
                              int *x, int *y)
{
	*x = *y = (params->wh + 3) * tilesize;
}

static void game_set_size(drawing *dr, game_drawstate *ds,
                          const game_params *params, int tilesize)
{
    ds->tilesize = tilesize;
}

static float *game_colours(frontend *fe, int *ncolours)
{
    float *ret = snewn(3 * NCOLOURS, float);

    frontend_default_colour(fe, &ret[COL_BACKGROUND * 3]);
	
	ret[COL_EDGE * 3 + 0] = 1.0F;
    ret[COL_EDGE * 3 + 1] = 0.8F;
    ret[COL_EDGE * 3 + 2] = 0.0F;

    ret[COL_GRID * 3 + 0] = 0.0F;
    ret[COL_GRID * 3 + 1] = 0.0F;
    ret[COL_GRID * 3 + 2] = 0.0F;

    ret[COL_USER * 3 + 0] = 0.0F;
    ret[COL_USER * 3 + 1] = 0.6F * ret[COL_BACKGROUND * 3 + 1];
    ret[COL_USER * 3 + 2] = 0.0F;

    ret[COL_HIGHLIGHT * 3 + 0] = 0.78F * ret[COL_BACKGROUND * 3 + 0];
    ret[COL_HIGHLIGHT * 3 + 1] = 0.78F * ret[COL_BACKGROUND * 3 + 1];
    ret[COL_HIGHLIGHT * 3 + 2] = 0.78F * ret[COL_BACKGROUND * 3 + 2];
	
	ret[COL_ERROR * 3 + 0] = 1.0F;
    ret[COL_ERROR * 3 + 1] = 0.0F;
    ret[COL_ERROR * 3 + 2] = 0.0F;

    ret[COL_PENCIL * 3 + 0] = 0.5F * ret[COL_BACKGROUND * 3 + 0];
    ret[COL_PENCIL * 3 + 1] = 0.5F * ret[COL_BACKGROUND * 3 + 1];
    ret[COL_PENCIL * 3 + 2] = ret[COL_BACKGROUND * 3 + 2];
	
    ret[COL_DONE * 3 + 0] = ret[COL_BACKGROUND * 3 + 0] / 1.5F;
    ret[COL_DONE * 3 + 1] = ret[COL_BACKGROUND * 3 + 1] / 1.5F;
    ret[COL_DONE * 3 + 2] = ret[COL_BACKGROUND * 3 + 2] / 1.5F;

    *ncolours = NCOLOURS;
    return ret;
}

static game_drawstate *game_new_drawstate(drawing *dr, const game_state *state)
{
    struct game_drawstate *ds = snew(struct game_drawstate);
	int wh = state->par.wh;
	int i;

	ds->tilesize = 0;
	ds->started = false;
    ds->wh = wh;
    ds->grid = snewn(wh*wh, long);
    for(i = 0; i < wh*wh; i++)
		ds->grid[i] = -1;
	
	ds->pencil = snewn(wh*wh, byte);
	memset(ds->pencil, 0, wh*wh);
	
	ds->clues = snewn(4*wh, long);
	for(i = 0; i < 4*wh; i++)
		ds->clues[i] = -1;
	
	ds->clues_completed = snewn(4*wh, bool);
	memset(ds->clues_completed, 0, 4*wh);
	
	ds->err1 = snewn(wh*wh, bool);
	memset(ds->err1, 0, wh*wh);
	
	ds->err2 = snewn(4*wh, bool);
	memset(ds->err2, 0, 4*wh);
	
    return ds;
}

static void game_free_drawstate(drawing *dr, game_drawstate *ds)
{
    sfree(ds->grid);
	sfree(ds->pencil);
	sfree(ds->clues);
	sfree(ds->clues_completed);
	sfree(ds->err1);
	sfree(ds->err2);
    sfree(ds);
}

static void draw_clue_letter(drawing *dr, game_drawstate *ds,
                        const game_state *state, int x, int y)
{
	int wh = state->par.wh;
    int tx, ty, tw, th;
    char str[2];
	
    tw = th = TILE_SIZE-1;
	str[1] = '\0';
	
 
	if(y == -1 || y == wh)
	{
		tx = BORDER + (x+1)*TILE_SIZE + 1;
		
		if(y == -1) { /* Top */		
		ty = BORDER - GRIDEXTRA + 1;
		clip(dr, tx, ty, tw, th);
		draw_rect(dr, tx, ty, tw, th, COL_EDGE);
		str[0] = (state->edges.top[x]) ? (char)log2(state->edges.top[x]) + 'A' : ' '; //FIX
		draw_text(dr, tx + tw/2, ty + th/2,
		  FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_HCENTRE | ALIGN_VCENTRE,
		  (ds->err2[x]) ? COL_ERROR : state->clues_completed[x] ? COL_DONE : COL_GRID, str); 
		unclip(dr); 
		} else if(y == wh) {/* Bottom */
		ty = BORDER + (wh+1)*TILE_SIZE + GRIDEXTRA + 1;
		clip(dr, tx, ty, tw, th);
		draw_rect(dr, tx, ty, tw, th, COL_EDGE);
		str[0] = (state->edges.bottom[x]) ? ((char)log2(state->edges.bottom[x])) + 'A' : ' '; //FIX

		draw_text(dr, tx + tw/2, ty + th/2,
		  FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_HCENTRE | ALIGN_VCENTRE,
		  (ds->err2[wh+x]) ? COL_ERROR : state->clues_completed[wh+x] ? COL_DONE : COL_GRID, str);
		unclip(dr);
		}
	}

	else if(x == -1 || x == wh)
	{
		ty = BORDER + (y+1)*TILE_SIZE + 1;
		
		if(x == -1) {/* Left */
		tx = BORDER - GRIDEXTRA +1 ;
		clip(dr, tx, ty, tw, th);
		draw_rect(dr, tx, ty, tw, th, COL_EDGE);
		str[0] = (state->edges.left[y]) ? (char)log2(state->edges.left[y]) + 'A' : ' '; //FIX
		draw_text(dr, tx + tw/2, ty + th/2,
		  FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_HCENTRE | ALIGN_VCENTRE,
		ds->err2[2*wh+y] ? COL_ERROR : (state->clues_completed[2*wh+y]) ? COL_DONE : COL_GRID, str);
		unclip(dr);
		} else if(x == wh) {/* Right */
		tx = BORDER + (wh+1)*TILE_SIZE + GRIDEXTRA + 1;
		clip(dr, tx, ty, tw, th);
		draw_rect(dr, tx, ty, tw, th, COL_EDGE);
		str[0] = (state->edges.right[y]) ? (char)log2(state->edges.right[y]) + 'A' : ' '; //FIX
		draw_text(dr, tx + tw/2, ty + th/2,
		  FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_HCENTRE | ALIGN_VCENTRE,
		  (ds->err2[3*wh+y]) ? COL_ERROR : (ds->clues[3*wh+y] & DF_CLUE_DONE) ? COL_DONE : COL_GRID, str);
		unclip(dr);
		}
	}
}

static void draw_user_letter(drawing *dr, game_drawstate *ds,
                        const game_state *state, int x, int y)
{
	int wh = state->par.wh;
    int tx, ty, tw, th;
    char str[8];
	
	tx = BORDER + (x+1)*TILE_SIZE+1;
    ty = BORDER + (y+1)*TILE_SIZE+1;
	tw = TILE_SIZE-1;
    th = TILE_SIZE-1;
	
	clip(dr, tx, ty, tw, th);
	
	/* background needs erasing */
    draw_rect(dr, tx, ty, tw, th, (ds->grid[y*wh+x] & DF_HIGHLIGHT) ? COL_HIGHLIGHT : COL_BACKGROUND);
	
	/* pencil-mode highlight */
    if (ds->grid[y*wh+x] & DF_HIGHLIGHT_PENCIL) {
        int coords[6];
        coords[0] = tx;
        coords[1] = ty;
        coords[2] = tx+tw/2;
        coords[3] = ty;
        coords[4] = tx;
        coords[5] = ty+th/2;
        draw_polygon(dr, coords, 3, COL_HIGHLIGHT, COL_HIGHLIGHT);
    } 
	
	if (state->grid[y*wh+x]) {
		str[1] = '\0';
		str[0] = (char)log2(state->grid[y*wh+x]) + 'A';
		draw_text(dr, tx + tw/2, ty + th/2,
		  FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_VCENTRE | ALIGN_HCENTRE,
		  (ds->err1[y*wh+x]) ? COL_ERROR : COL_USER, str);
    } 
	#if 0
	else if(state->pencil[y*wh+x] & MASK_X) { // FIX THIS!!!!
			draw_text(dr, tx + tw/2, ty + th/2,
				FONT_VARIABLE, TILE_SIZE*2/3, ALIGN_VCENTRE | ALIGN_HCENTRE, 
				COL_DONE, "X");
	}
	#endif
	else {
		int i, j, npencil;
	int pl, pr, pt, pb;
	float bestsize;
	int pw, ph, minph, pbest, fontsize;

        /* Count the pencil marks required. */

		byte pencil = state->pencil[y*wh+x];
		
		for(i = npencil = 0; i < state->par.max; i++)
			if(pencil & (1 << i))
		npencil++;
	if (npencil) {

	    minph = 2;

	    /*
	     * Determine the bounding rectangle within which we're going
	     * to put the pencil marks.
	     */
	    /* Start with the whole square */
	    pl = tx + GRIDEXTRA;
	    pr = pl + TILE_SIZE - GRIDEXTRA;
	    pt = ty + GRIDEXTRA;
	    pb = pt + TILE_SIZE - GRIDEXTRA;


	    /*
	     * We arrange our pencil marks in a grid layout, with
	     * the number of rows and columns adjusted to allow the
	     * maximum font size.
	     *
	     * So now we work out what the grid size ought to be.
	     */
	    bestsize = 0.0;
	    pbest = 0;
	    /* Minimum */
	    for (pw = 3; pw < max(npencil,4); pw++) {
		float fw, fh, fs;

		ph = (npencil + pw - 1) / pw;
		ph = max(ph, minph);
		fw = (pr - pl) / (float)pw;
		fh = (pb - pt) / (float)ph;
		fs = min(fw, fh);
		if (fs > bestsize) {
		    bestsize = fs;
		    pbest = pw;
		}
	    }
	    assert(pbest > 0);
	    pw = pbest;
	    ph = (npencil + pw - 1) / pw;
	    ph = max(ph, minph);

	    /*
	     * Now we've got our grid dimensions, work out the pixel
	     * size of a grid element, and round it to the nearest
	     * pixel. (We don't want rounding errors to make the
	     * grid look uneven at low pixel sizes.)
	     */
	    fontsize = min((pr - pl) / pw, (pb - pt) / ph);

	    /*
	     * Centre the resulting figure in the square.
	     */
	    pl = tx + (TILE_SIZE - fontsize * pw) / 2;
	    pt = ty + (TILE_SIZE - fontsize * ph) / 2;

	    /*
	     * Now actually draw the pencil marks.
	     */
	    for (i = j = 0; i < state->par.max; i++)
		if(pencil & (1 << i)) {
		    int dx = j % pw, dy = j / pw;

		    str[1] = '\0';
		    str[0] = i + 'A';
		    draw_text(dr, pl + fontsize * (2*dx+1) / 2,
			      pt + fontsize * (2*dy+1) / 2,
			      FONT_VARIABLE, fontsize,
			      ALIGN_VCENTRE | ALIGN_HCENTRE, COL_PENCIL, str);
		    j++;
		}
    }
	}
	
    unclip(dr);
}

static void game_redraw(drawing *dr, game_drawstate *ds,
                        const game_state *oldstate, const game_state *state,
                        int dir, const game_ui *ui,
                        float animtime, float flashtime)
{
    int wh = state->par.wh;
    int x, y, i;

    if (!ds->started) {
		draw_rect(dr, 0, 0, TILE_SIZE*(wh+3), TILE_SIZE*(wh+3), COL_BACKGROUND);

		draw_rect(dr, BORDER-2*GRIDEXTRA, BORDER-2*GRIDEXTRA,
			(wh+2)*TILE_SIZE+4*GRIDEXTRA+1, (wh+2)*TILE_SIZE+4*GRIDEXTRA+1,
			COL_GRID);
		
		draw_rect(dr, BORDER-2*GRIDEXTRA, BORDER-2*GRIDEXTRA, TILE_SIZE+GRIDEXTRA, TILE_SIZE+GRIDEXTRA, COL_BACKGROUND);
		draw_rect(dr, BORDER-2*GRIDEXTRA, BORDER+GRIDEXTRA+TILE_SIZE*(wh+1)+1, TILE_SIZE+GRIDEXTRA, TILE_SIZE+GRIDEXTRA, COL_BACKGROUND);
		draw_rect(dr, BORDER+GRIDEXTRA+TILE_SIZE*(wh+1)+1, BORDER-2*GRIDEXTRA, TILE_SIZE+GRIDEXTRA, TILE_SIZE+GRIDEXTRA, COL_BACKGROUND);
		draw_rect(dr, BORDER+GRIDEXTRA+TILE_SIZE*(wh+1)+1, BORDER+GRIDEXTRA+TILE_SIZE*(wh+1)+1, TILE_SIZE+GRIDEXTRA, TILE_SIZE+GRIDEXTRA, COL_BACKGROUND);
		
		ds->started = true;
	}
	
	check_valid(state, ds->err1, ds->err2);
	
	for(i = 0; i < 4*wh; i++) {
		long cell = state->edges.top[i];
		
		if(ds->err2[i])
			cell |= DF_ERROR;
		else if(state->clues_completed[i])
			cell |= DF_CLUE_DONE;
		
		if(ds->clues[i] == cell)
			continue;
		
		ds->clues[i] = cell;
		//ds->clues_completed[i] = state->clues_completed[i];
		
		CLUEPOS(x, y, i, wh);
		draw_clue_letter(dr, ds, state, x, y);
	}
	
    for (x = 0; x < wh; x++) {
	for (y = 0; y < wh; y++) {
        long cell = state->grid[y*wh+x];
			
		if (ui->hshow && x == ui->hx && y == ui->hy)
			cell |= ui->hpencil ? DF_HIGHLIGHT_PENCIL : DF_HIGHLIGHT;
		
		if (flashtime > 0 && 
			(flashtime <= FLASH_TIME/3 ||
			 flashtime >= FLASH_TIME*2/3))
            cell |= DF_HIGHLIGHT;
			
		if(ds->err1[y*wh+x])
			cell |= DF_ERROR;

		if (ds->grid[y*wh+x] == cell &&
			ds->pencil[y*wh+x] == state->pencil[y*wh+x])
		continue;
		

		ds->grid[y*wh+x] = cell;
		ds->pencil[y*wh+x] = state->pencil[y*wh+x];
			
		draw_user_letter(dr, ds, state, x, y);
	}
	}
	
	draw_update(dr, 0, 0,
                TILE_SIZE * (wh+3), TILE_SIZE * (wh+3));
}

static float game_anim_length(const game_state *oldstate,
                              const game_state *newstate, int dir, game_ui *ui)
{
    return 0.0F;
}

static float game_flash_length(const game_state *oldstate,
                               const game_state *newstate, int dir, game_ui *ui)
{
	if (!oldstate->completed && newstate->completed &&
	!oldstate->cheated && !newstate->cheated)
        return FLASH_TIME;
    return 0.0F;
}

static int game_status(const game_state *state)
{
    return state->completed ? +1 : 0;
}

static bool game_timing_state(const game_state *state, game_ui *ui)
{
    if (state->completed)
		return false;
    return true;
}

static void game_print_size(const game_params *params, float *x, float *y)
{
	#if 0
    int pw, ph;

    /*
     * We use 9mm squares by default, like Solo.
     */
    game_compute_size(params, 900, &pw, &ph);
    *x = pw / 100.0F;
    *y = ph / 100.0F;
	#endif
}

static void game_print(drawing *dr, const game_state *state, int tilesize)
{
	#if 0
    int w = state->par.w;
    int ink = print_mono_colour(dr, 0);
    int i, x, y;

    /* Ick: fake up `ds->tilesize' for macro expansion purposes */
    game_drawstate ads, *ds = &ads;
    game_set_size(dr, ds, NULL, tilesize);

    /*
     * Border.
     */
    print_line_width(dr, 3 * TILESIZE / 40);
    draw_rect_outline(dr, BORDER, BORDER, w*TILESIZE, w*TILESIZE, ink);

    /*
     * Main grid.
     */
    for (x = 1; x < w; x++) {
	print_line_width(dr, TILESIZE / 40);
	draw_line(dr, BORDER+x*TILESIZE, BORDER,
		  BORDER+x*TILESIZE, BORDER+w*TILESIZE, ink);
    }
    for (y = 1; y < w; y++) {
	print_line_width(dr, TILESIZE / 40);
	draw_line(dr, BORDER, BORDER+y*TILESIZE,
		  BORDER+w*TILESIZE, BORDER+y*TILESIZE, ink);
    }

    /*
     * Clues.
     */
    for (i = 0; i < 4*w; i++) {
	char str[128];

	if (!state->clues->clues[i])
	    continue;

	CLUEPOS(x, y, i, w);

	sprintf (str, "%d", state->clues->clues[i]);

	draw_text(dr, BORDER + x*TILESIZE + TILESIZE/2,
		  BORDER + y*TILESIZE + TILESIZE/2,
		  FONT_VARIABLE, TILESIZE/2,
		  ALIGN_VCENTRE | ALIGN_HCENTRE, ink, str);
    }

    /*
     * Numbers for the solution, if any.
     */
    for (y = 0; y < w; y++)
	for (x = 0; x < w; x++)
	    if (state->grid[y*w+x]) {
		char str[2];
		str[1] = '\0';
		str[0] = state->grid[y*w+x] + '0';
		draw_text(dr, BORDER + x*TILESIZE + TILESIZE/2,
			  BORDER + y*TILESIZE + TILESIZE/2,
			  FONT_VARIABLE, TILESIZE/2,
			  ALIGN_VCENTRE | ALIGN_HCENTRE, ink, str);
	    }
	#endif
}

#ifdef COMBINED
#define thegame abc
#endif

const struct game thegame = {
    "ABC", "games.abc", "abc",
    default_params,
    game_fetch_preset, NULL,
    decode_params,
    encode_params,
    free_params,
    dup_params,
    true, game_configure, custom_params,
    validate_params,
    new_game_desc,
    validate_desc,
    new_game,
    dup_game,
    free_game,
    true, solve_game,
    false, game_can_format_as_text_now, game_text_format,
    new_ui,
    free_ui,
    encode_ui,
    decode_ui,
    game_request_keys,
    game_changed_state,
    interpret_move,
    execute_move,
    PREFERRED_TILESIZE, game_compute_size, game_set_size,
    game_colours,
    game_new_drawstate,
    game_free_drawstate,
    game_redraw,
    game_anim_length,
    game_flash_length,
    game_status,
    true, false, game_print_size, game_print,
    false,			       /* wants_statusbar */
    false, game_timing_state,
    REQUIRE_RBUTTON,  /* flags */
};

 /* Solver fails on IDs from janko.at webpage:
  * Janko Nr20 - 5l3://C///C//A/////C/B//A/B/B//A	**** Solver makes almost no progress on this ID ****
  * Janko Nr48 - 7l4:/D/////D//B//A/D/B/A/B/A//C/A/A//////D/D/ 
  * Janko Nr35 - 6l4:C//D///A//D//B/////B//B/D//D///A/B		
  * Janko Nr251 - 5l4:/B/D/////B//D/C///D//B//A/C/
  * Janko Nr480
  * test Janko Nr100
  */
  
#ifdef STANDALONE_SOLVER
int main(int argc, char **argv)
{		
    game_params *params;
    game_state *state;
    char *id = NULL, *desc;
	const char *err;

    while (--argc > 0) {
        char *p = *++argv;
        if (!strcmp(p, "-v"))
            solver_show_working = true;
		else if (!strcmp(p, "-e"))
            verbose = true;
		else if (*p == '-') {
            printf("%s: unrecognised option `%s'\n", argv[0], p);
            return 1;
        } else {
            id = p;
        }
    }
	
	if (!id) {
        printf("usage: %s [-v | -e] <game_id>\n", argv[0]);
        return 1;
    }
	
	params = default_params();
	decode_params(params, id);
	

	desc = strchr(id, ':');
	if (!desc) {
		printf("%s: game id expects a colon in it\n", argv[0]);
		return 1;
	}
	*desc++ = '\0';
	#if 0 // FIX: BUG
	err = validate_desc(params, desc);	
	if(err)
	{
		fprintf(stderr, "%s: %s\n", argv[0], err);
		return 1;
	}
	#endif
	state = new_game(NULL, params, desc);
	
	struct solver_usage *usage = new_solver_usage(params);
	/* FIX: copy grid clues into solver via solver_place */
	solver(usage, &state->edges);
	memcpy(state->grid, usage->grid, state->par.wh*state->par.wh);
	
	fputs(game_text_format(state), stdout);
	
	free_solver_usage(usage);
	free_params(params);
	free_game(state);
	
	return 0;
}
#endif

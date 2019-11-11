#ifndef MINIG_H
#define MINIG_H


int bell(void);
int cvxy_(float *x, float *y, int *ix, int *iy, int *pmode);
int define_color(char *color_name);
int erase_(void);
int finig_(void);
int get_focus(int *focus_id);
int grax(float *plo_axis_val, float *phi_axis_val, float *axis_tick_vals, int *n_vals, int *pyflag);
int hcopy(void);
int initg_(int *win_size_x, int *win_size_y);
int ivect_(int *pix, int *piy);
int limg_(int *pnx, int *pix, int *pny, int *piy);
int minig_fill(void);
int mspot_(int *nx, int *ny);
int point(float x, float y);
int pspot_(float *px, float *py);
int putg_(char *text, int *pnc, int *pkad);
int retic_(float *, float *, char *);
int save_xwg__(char *in);
int set_focus(int focus_id);
int set_xwg__(char *in);
int setcolor_(int *picol);
int symbg_(int *psymbol, float *px, float *py, int *psize);
int trax(float *pnx, float *pix, float *pny, float *piy, int *paxis_mode);
int vect_(float *px, float *py);
int set_minig_color_rgb(float red, float green, float blue);
int set_minig_line_width(int width);
int getglobals_(float *dx,float *x0,float *dy,float *y0,int *di,int *i0,int *dj,int *j0,int *flagg);
int putglobals_(float *dx,float *x0,float *dy,float *y0,int *di,int *i0,int *dj,int *j0,int *flag);

#endif // MINIG_H
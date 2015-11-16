#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <termio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "defs.h"
#define FONTNAME "-ADOBE-HELVETICA-MEDIUM-R-NORMAL--*-100-*-*-P-*"
#define WINDNAME "UPDATE 1.4"

static Display   *disp_id;
static Screen    *screen_id;
static Window    root_win_id,win_id;
static XSetWindowAttributes setwinattr;
static GC        gc_id,gc_comp_id;
static int       win_width = 400,win_height = 300;
static int       color[21],ixp,iyp;
static int       zero=0;
Status           istat;
Bool             bstat;

XGCValues xgcvl;
XColor    screen_color,exact_color;
XPoint    xpoints[512];
XEvent    event;

void setup_read_();
void setblock();
void syserr();

/*--------------------------------------------------------*/
void open_gwindow_( )
{
/*
 * create new graphics window on first entry....
 */
  int                          font,black,white;
  int                          scr_width,scr_height;
  int                          j;
  unsigned long                valuemask;
  static int                   depth;    /* number of planes  */
  static Visual               *visual;   /*VISUAL TYPE       */
  static XSizeHints            win_position;   /*position and size for
                                            window manager.*/

/*
 * initialize display id and screen id....
 */
      disp_id = XOpenDisplay(0);
      if (!disp_id) {
         printf("Display not opened!\n");
         exit(-1);
      }
/*
 * next instruction for debugging only....
 */
/*      XSynchronize(disp_id, 1); */
  
  screen_id  =  XDefaultScreenOfDisplay(disp_id);
  root_win_id = XRootWindowOfScreen(screen_id);
  black =       XBlackPixelOfScreen(screen_id);
  white =       XWhitePixelOfScreen(screen_id);
  scr_width =   XWidthOfScreen(screen_id);
  scr_height =  XHeightOfScreen(screen_id);
  depth  =      XDefaultDepthOfScreen(screen_id);
  visual =      XDefaultVisualOfScreen(screen_id);


/*
 * set up backing store....
 */
      valuemask = CWBitGravity | CWBackingStore | CWBackPixel;
      setwinattr.bit_gravity = SouthWestGravity;
      setwinattr.backing_store = Always;
      setwinattr.background_pixel = white;
/*
 * create the window....
 */
      win_id  = XCreateWindow(disp_id,
			      root_win_id,
			      scr_width - win_width - 15,
			       + 100,
			      win_width, 
			      win_height, 
			      10,
			      depth,
			      InputOutput,
			      visual,
			      valuemask,
			      &setwinattr);
/* WMHints structure */
/*  win_position.x = scr_width - win_width - 15;
  win_position.y = scr_height - win_height - 35;*/  

  win_position.x = scr_width - win_width - 15;
  win_position.y = + 100;

  win_position.width = win_width;
  win_position.height = win_height;
  win_position.flags=USPosition|USSize;
  
  XSetWMNormalHints(disp_id, win_id, &win_position); 

      XStoreName(disp_id, win_id, WINDNAME);
/*
 * get named color values....
 */
      color[1]   = define_color_("BLUE");
      color[2]   = define_color_("DEEP SKY BLUE");
      color[3]   = define_color_("LIGHT SKY BLUE");

      color[4]   = define_color_("SEA GREEN");
      color[5]  = define_color_("MEDIUM SEA GREEN");
      color[6]  = define_color_("GREEN");

      color[7]  = define_color_("BROWN");
      color[8]  = define_color_("CHOCOLATE");
      color[9]  = define_color_("SANDY BROWN");

      color[10]   = define_color_("RED");
      color[11]   = define_color_("CORAL");
      color[12]   = define_color_("ORANGE");

      color[13]  = define_color_("YELLOW3");
      color[14]  = define_color_("YELLOW2");
      color[15]  = define_color_("YELLOW");

      color[16]  = define_color_("PEACH PUFF");
      color[17]  = define_color_("PAPAYA WHIP");
      color[18]  = define_color_("OLD LACE");

      color[19]   = white;
      color[20]   = black;
      color[21]   = black;

/*
 * create graphics context....
 */
      xgcvl.background = color[19];
      xgcvl.foreground = color[20];
      gc_id = XCreateGC(disp_id, win_id, GCForeground | GCBackground, &xgcvl);
      xgcvl.function = GXinvert;
      gc_comp_id = XCreateGC(disp_id, win_id, GCFunction, &xgcvl);
/*
 * load the font for text writing....
 */
      font = XLoadFont(disp_id, FONTNAME);
      XSetFont(disp_id, gc_id, font);

  /* Map the window.... */
  XSelectInput(disp_id,win_id,ExposureMask|VisibilityChangeMask);
  XMapRaised(disp_id,win_id);

  /*
   * Wait for the window to be raised. Some X servers do not 
   * generate an initial expose event, so also check the visibility
   * event.
   */
  XMaskEvent(disp_id,ExposureMask|VisibilityChangeMask,&event);
  XSelectInput(disp_id,win_id,0);
  XSync(disp_id,1);
}


/*--------------------------------------------------------*/
static int define_color_(color_name)
char *color_name;
{
/*
 * create color....
 */
      int color_map;

      color_map = XDefaultColormapOfScreen(screen_id);
      istat = XAllocNamedColor(disp_id, color_map, color_name,
                                &screen_color, &exact_color);
      if (istat) 
         return screen_color.pixel;
      else      
 
        return XBlackPixelOfScreen(disp_id);
}
/*--------------------------------------------------------*/
void get_gw_geometry_(ret_width,ret_height)
unsigned long *ret_width,*ret_height;
{
      Window       junk_id;
      int          ix,iy;
      unsigned int w,h,junk_border,junk_depth;

      istat = XGetGeometry(disp_id,win_id,&junk_id,&ix,&iy,
                           &w,&h,&junk_border,&junk_depth);
      win_width = (short) w;
      win_height = (short) h;

      *ret_width = (long) win_width;
      *ret_height = (long) win_height;
}
/*--------------------------------------------------------*/
void clear_gw_( )
{
      XClearWindow(disp_id,win_id);
}
/*--------------------------------------------------------*/
void draw_gw_string_(ix,iy,len,text)
long   *ix,*iy,*len;
char   *text;
{
      int  x,y,n;

      x = (short) (*ix);
      y = (short) (*iy);
      n = (short) (*len);

      XDrawString(disp_id,win_id,gc_id,x,y,text,n);
}
/*--------------------------------------------------------*/
void draw_gw_lines_(points,nstored)
long *nstored,*points;
{
      int i,n;

      n = (short) (*nstored);
      for (i=0;i<n;i++) {
         xpoints[i].x = (short) (*(points+i+i));
         xpoints[i].y = (short) (*(points+i+i+1));
      }
      XDrawLines(disp_id,win_id,gc_id,xpoints,n,CoordModeOrigin);
}
/*--------------------------------------------------------*/
void draw_gw_point_(newx,newy)
long *newx,*newy;
{
      int X,Y;
      X = (short) (*newx);
      Y = (short) (*newy);
      XDrawPoint(disp_id,win_id,gc_id,X,Y);
}
/*--------------------------------------------------------*/
void flush_gw_( )
{
      XFlush(disp_id);
}
/*--------------------------------------------------------*/
void set_up_cursor_( )
{
      Window          jroot,jchild;
      int             ix_root,iy_root;
      unsigned int    jmask;
/*
 * accept mouse pointer move and button move events....
 */
      XSelectInput(disp_id, win_id, PointerMotionMask |
                                    KeyPressMask |
                                    ButtonPressMask);
/*
 * get position of mouse pointer....
 */
      bstat = XQueryPointer(disp_id,win_id,&jroot,&jchild,
                            &ix_root,&iy_root,&ixp,&iyp,&jmask);
      if (!bstat) {
         ixp = 100;
         iyp = 80;
      }
/*
 * draw cross hairs (complement)....
 */
      XDrawLine(disp_id,win_id,gc_comp_id,ixp,0,ixp,win_height);
      XDrawLine(disp_id,win_id,gc_comp_id,0,iyp,win_width,iyp);
/*
 * enable input from terminal window....
 */
      setup_read_(1);
}
/*--------------------------------------------------------*/
void done_cursor_( )
{
/*
 * remove mouse input events....
 */
      XSelectInput(disp_id, win_id, 0);
/*
 * discard all input events....
 */
      XSync(disp_id,1);
/*
 * remove cross hairs (complement)....
 */
      XDrawLine(disp_id,win_id,gc_comp_id,ixp,0,ixp,win_height);
      XDrawLine(disp_id,win_id,gc_comp_id,0,iyp,win_width,iyp);
      XFlush(disp_id);
/*
 * disable input from terminal window....
 */
      setup_read_(0);
}
/*--------------------------------------------------------*/
void check_gw_bp_(button,ixi,iyi)
long *button,*ixi,*iyi;
{
/*
 * check for button press on graphics window....
 */
      *button = 0L;
      bstat = XCheckWindowEvent(disp_id,win_id,ButtonPressMask,&event);
      if (bstat)
      {
         *button = 1L;
         if (event.xbutton.button == Button2) *button = 2L;
         if (event.xbutton.button == Button3) *button = 3L;
         *ixi = (long) event.xbutton.x;
         *iyi = (long) event.xbutton.y;
      }
}
/*--------------------------------------------------------*/
void check_gw_kp_(key,ixi,iyi)
long *key,*ixi,*iyi;
{
/*
 * check for key press on graphics window....
 */

      *key = 0L;
      bstat = XCheckWindowEvent(disp_id,win_id,KeyPressMask,&event);
      if (bstat)
      {
         *key = (long) event.xkey.keycode;
         *ixi = (long) event.xkey.x;
         *iyi = (long) event.xkey.y;
      }
}
/*--------------------------------------------------------*/
void move_gw_pointer_(ixi,iyi)
long *ixi,*iyi;
{
/*
 * move pointer to new location for arrow key press....
 */
      Window          jroot,jchild;
      int             ix_root,iy_root;
      unsigned int    jmask;
      int             ixin,iyin,ixout,iyout;
/*
 * get position of mouse pointer....
 */
      bstat = XQueryPointer(disp_id,win_id,&jroot,&jchild,
                            &ix_root,&iy_root,&ixin,&iyin,&jmask);
/*
 * move pointer to new location....
 */
      ixout = ixin + (short) (*ixi);
      iyout = iyin + (short) (*iyi);
      XWarpPointer(disp_id,win_id,win_id,ixin,iyin,
                   win_width,win_height,ixout,iyout);
}
/*--------------------------------------------------------*/
void check_gw_pm_( )
{
      Window          jroot,jchild;
      int             ix_root,iy_root;
      unsigned int    jmask;
/*
 * check for pointer motion on graphics window....
 */
      bstat = XCheckWindowEvent(disp_id,win_id,PointerMotionMask,&event);
      if (bstat) {
         do
         {
            bstat = XCheckWindowEvent(disp_id,win_id,PointerMotionMask,&event);
         } while (bstat);
/*
 * remove cross hairs (complement)....
 */
         XDrawLine(disp_id,win_id,gc_comp_id,ixp,0,ixp,win_height);
         XDrawLine(disp_id,win_id,gc_comp_id,0,iyp,win_width,iyp);
/*
 * get new position of mouse pointer....
 */
         bstat = XQueryPointer(disp_id,win_id,&jroot,&jchild,
                               &ix_root,&iy_root,&ixp,&iyp,&jmask);
/*
 * draw cross hairs (complement)....
 */
         XDrawLine(disp_id,win_id,gc_comp_id,ixp,0,ixp,win_height);
         XDrawLine(disp_id,win_id,gc_comp_id,0,iyp,win_width,iyp);
      }
}
/*--------------------------------------------------------*/
void set_gw_foreground_(jcol)
long *jcol;
{
      int j;
      j = (short) (*jcol);
      XSetForeground(disp_id,gc_id,color[j]);
}
/*--------------------------------------------------------*/
void setup_read_(enable)
int enable;
{
      static struct termio    tbuf, tbufsave;
      static int              first = 1;
      int                     ioctl_stat;

      if (first == 1)
      {
          first = 0;
/*
 *       save the terminal settings....
 */
         ioctl_stat = ioctl(0, TCGETA, &tbufsave);
/*	  ioctl_stat = tcgetattr(0,&tbufsave); */
         if (ioctl_stat == -1)
         {
            printf("chk_read: save terminal ioctl call failed...\n");
            perror(0);
         }

         tbuf = tbufsave;
         tbuf.c_lflag &= ~(ICANON | ECHO);
         tbuf.c_cc[4] = 0;   /* MIN no. of chars to be accepted */
         tbuf.c_cc[5] = 1;   /* TIMEout period in 0.1-second units */
      }

      if (enable == 1) 
      {
/*
 *       turn off echo and blocking for terminal;
 *                   i.e. do not wait for <return>.... 
 */
         ioctl_stat = ioctl(0, TCSETAF, &tbuf);
/*	 ioctl_stat = tcsetattr(0,TCSANOW,&tbuf); */
         if (ioctl_stat == -1)
         {
            printf("chk_read: set terminal ioctl call failed...\n");
           perror(0);
         }
	 setblock(0,FALSE);
      }
      else
      {
/*
 *      turn echo and blocking back on....
 */
         ioctl_stat = ioctl(0, TCSETAF, &tbufsave);
/*	  ioctl_stat = tcsetattr(0,TCSANOW,&tbufsave); */
         if (ioctl_stat == -1)
         {
            printf("chk_read: reset terminal ioctl call failed...\n");
            perror(0);
         }
	 setblock(0,TRUE);
      }
}
/*--------------------------------------------------------*/
void setblock(fd, on) /* turn blocking on or off */
int fd;
BOOLEAN on;
{
  static int blockf, nonblockf;
  static BOOLEAN first = TRUE;
  int flags;

  if(first) {
    first = FALSE;
    if((flags = fcntl(fd, F_GETFL, 0)) == -1)
      syserr("fnctl");
    blockf    = flags & ~O_NDELAY; /* make sure O_NDELAY is off */
    nonblockf = flags |  O_NDELAY; /* make sure O_NDELAY is on */
  }
  if (fcntl(fd, F_SETFL, on ? blockf: nonblockf) == -1)
    syserr("fcntl2");
}
/*--------------------------------------------------------*/
void chk_read_(cout, ret_nc, ixi, iyi)
char *cout;
long *ret_nc, *ixi, *iyi;
{
      Window          jroot,jchild;
      int             ix_root,iy_root,x,y;
      unsigned int    jmask;
      int             nchar, i;
      char            cbuf[10];
/*
 *        look for a single character on terminal input buffer....
 */
      nchar = read(0, cbuf, sizeof(cbuf));
/*
 *      if (nchar == -1)
 *    {
 *        printf("chk_read: read call failed...\n");
 *        perror();
 *     }
 */
      if (nchar >= 0) {
	for (i=0; i < nchar; i++) {*(cout + i) = cbuf[i];}
	*ret_nc = nchar;
      }
      else {
	*ret_nc = zero;
      }
/*
 * get new position of mouse pointer....
 */
      bstat = XQueryPointer(disp_id,win_id,&jroot,&jchild,
                            &ix_root,&iy_root,&x,&y,&jmask);
      *ixi = (long) x;
      *iyi = (long) y;
}
/*--------------------------------------------------------*/
void syserr(msg) /* print system call error message and terminate */
char *msg;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];

  fprintf(stderr, "ERROR: %s (%d", msg, errno);
  if (errno >0 && errno < sys_nerr)
    fprintf(stderr, ";%s)\n", sys_errlist[errno]);
  else
    fprintf(stderr, ")\n");
  exit(1);
}


typedef enum{FALSE, TRUE} BOOLEAN;

#define lowbyte(w) ((w) & 0377)
#define highbyte(w) lowbyte((w) >> 8)
/* byte oerations may be wrong RWM june 92 */

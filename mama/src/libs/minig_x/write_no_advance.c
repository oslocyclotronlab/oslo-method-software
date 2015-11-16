#include <stdio.h>
#include <strings.h>

void write_no_advance_(char *ostring)
{
    printf("%s",ostring);
    fflush(stdout);
}

void write_no_advance__(char *ostring)
{
    write_no_advance_(ostring);
}

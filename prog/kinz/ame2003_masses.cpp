
#include <algorithm>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "ask_par.h"

namespace {
#include "ame2003_mass_tables.cppi"

    inline bool operator<(const ame2003_mass_t& m1, const ame2003_mass_t& m2)
    {
        return (m1.A < m2.A) || ((m1.A == m2.A) && (m1.Z < m2.Z));
    }

    const ame2003_mass_t& find_entry(int A, int Z)
    {
        const ame2003_mass_t search = { A, Z, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        const ame2003_mass_t *begin = ame2003_masses, *end = begin + ame2003_n_masses;
        const ame2003_mass_t *p = std::lower_bound(begin, end, search);
        // invalid entry with A=0 at the end, so if lower bound does
        // not find any match, return this one
        if( p->A == A && p->Z == Z )
            return *p;
        else
            return *end;
    }
}

const char* get_element_name(int Z)
{
    if( Z>=0 && Z<element_count )
        return element_names[Z];
    else
        return "?";
}

double ame2003_get_mass_amu(int A, int Z)
{
    return find_entry(A, Z).mass_u_amu / 1e6;
}
    
double ame2003_get_Q_keV(int Ai1, int Zi1, int Ai2, int Zi2, int Ao1, int Zo1)
{
    const ame2003_mass_t& i1 = find_entry(Ai1, Zi1);
    const ame2003_mass_t& i2 = find_entry(Ai2, Zi2);
    const ame2003_mass_t& o1 = find_entry(Ao1, Zo1);
    const ame2003_mass_t& o2 = find_entry(Ai1+Ai2-Ao1, Zi1+Zi2-Zo1);

    if( i1.A && i2.A && o1.A && o2.A ) {
        return i1.mass_excess + i2.mass_excess - o1.mass_excess - o2.mass_excess;
    } else {
        return -1e22;
    }
}

/**************************************************************************/
/**************************************************************************/

void ask_par_Z(const char* ask, int* Z)
{
    const char* r = ask_par_input(ask, *Z, get_element_name(*Z));

    if( sscanf(r,"%d", Z) != 1 && strlen(r)>0 && r[0] != '\n' ) {
        // no number found, try to find element name

        // strip newline from end
        char tmp[128];
        sscanf(r, "%s", tmp);

        // search list of element names
        int z=0;
        for(; z<element_count; ++z)
            if( !strcasecmp(tmp, element_names[z]) )
                break;

        // assign value if found
        if( z == element_count )
            fprintf(stderr, "No element named '%s' found!\n", tmp);
        else
            *Z = z;
    }
}

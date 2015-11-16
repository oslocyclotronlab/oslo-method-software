
import sys

ame = open(sys.argv[1], "r")

# skip header
for i in range(39):
    ame.readline()

names = ["?"]*119
betatypes = []
origins = []

def float_extrapolated(txt):
    txt = txt.strip()
    return txt.replace('#','.'), ('#') in txt

def v_Dv(txt):
    txt = txt.strip()
    if txt == "*":
        return 0.0, -1.0, False
    v, Dv = txt.split()
    v,   vep = float_extrapolated( v)
    Dv, Dvep = float_extrapolated(Dv)
    return v, Dv, (vep or Dvep)

print """struct ame2003_mass_t {
    const int A, Z, origin;
    const double mass_excess, Dmass_excess;
    const double binding_energy, Dbinding_energy;
    const int beta_decay_type;
    const double beta_decay_energy, Dbeta_decay_energy;
    const double mass_u_amu, Dmass_u_amu;
    const bool mass_extrapolated, beta_decay_energy_extrapolated;
};

const ame2003_mass_t ame2003_masses[] = {"""

n_masses = 0
for line in ame:
    #    format    :  a1,i3,i5,i5,i5,1x,a3,a4,1x,f13.5,f11.5,f11.3,f9.3,1x,a2,f11.3,f9.3,1x,i3,1x,f12.5,f11.3,1x
    Z = int(line[9:14])
    A = int(line[14:19])

    names[Z] = line[20:23].strip()
    origin = line[23:27].strip()
    if len(origin):
        if not origin in origins:
            origins.append(origin)
        origin = origins.index(origin)+1
    else:
        origin = 0
    mexc, Dmexc, mexc_ep = v_Dv(line[28:53])
    be, Dbe, be_ep = v_Dv(line[54:72])
    betatype = line[73:75]
    if not betatype in betatypes:
        betatypes.append(betatype)
    betatype = betatypes.index(betatype)
    bde, Dbde, bde_ep = v_Dv(line[76:95])
    amu, Damu, amu_ep = v_Dv(line[96:99]+line[100:])
    if amu_ep == mexc_ep and be_ep == amu_ep:
        mep = amu_ep and 1 or 0
    else:
        raise "bad extrapolation for A=%d Z=%d %d %d %d" % (A, Z, amu_ep, mexc_ep, be_ep)
    bdep = bde_ep and 1 or 0

    print "{%3d, %3d, %3d,   %12s, %10s,   %10s, %7s,   %d, %10s, %8s,   %13s, %8s,   %d, %d}," \
        % ( A, Z, origin, mexc, Dmexc, be, Dbe, betatype, bde, Dbde, amu, Damu, mep, bdep )
    n_masses += 1
print "{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} // invalid entry with A=0"
print "};"
print "const int ame2003_n_masses = %d;" % n_masses

print
print "const int   element_count = %d;" % len(names)
print "const char *element_names[element_count] = {"
for i in range(len(names)):
    nn = '"%s"' % names[i]
    comma = (i < len(names)-1) and "," or ""
    print "%5s%s" % (nn,comma),
    if i == len(names)-1 or (i%20)==19:
        print
print "};"

print
print "const char* origins[%d] = { \"?\", " % (len(origins)+1)
for i in range(len(origins)):
    oo = '"%s"' % origins[i]
    comma = (i < len(origins)-1) and "," or ""
    print "%7s%s" % (oo,comma),
    if i == len(origins)-1 or (i%10)==9:
        print
print "};"

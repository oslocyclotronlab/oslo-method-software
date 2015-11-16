                                     ELAST
                         Energy Loss And Straggeling Tool

ELAST is partially based on eneloss, a programm developped by XXX for
VAX/VMS Fortran.  Other than eneloss, ELAST is a shell- and pipe
controlled programm without an interactive environement but supporting
the features of the UNIX operation system.  In another mode, ELAST
performs calculations of relavistic kinematics for heavy ion
reactions. It is completly rewritten in ANSI-C and *should* be
portable to any UNIX-System without any changes. For more detailed
information on system requirements see INSTALLATION.

1. FEATURES

ELAST performs six major calculations on a ionic beam passing matter. It
calculates the average energy loss per particle, the average angular
straggeling and the widening of the energy peak, assuming that the oncoming 
beam was perfect (no divergenve, monoenergetic). It also calculates the
position of the bragg peak for that beam, given a thick target, the dE/dx-
value for the specified energy, and the average charge state of the projektile
ions. 

In reaction mode, ELAST does relativistic calculation on maximum scattering
angles and energies in the laboratory at given angles. There is a built in 
Database to determine the Q-values of a reaction, given all particles stay in
the ground state. If this is not true, the user should give a crrected Q-value.
In this mode, the 4th particle in a reaction of the type 1(2,3,)4
"(non-observed-particle)" can be a uncharged particle, like a photon (0ng)
or a neutron (1ng). The third particle, however, has to be a charged particle,
if any stopping power-related command is used.

2. INPUT FORMAT AND SYNTAX

As an excecutabel file, ELAST is not spelled in capital letters. Therefor, the 
 *syntax to use ELAST* is:

    elast [-switches switchparameter] command[s] target thickness projejtil 
              [reaction product (reaction_product_Q-value | d) ] 
                 energy [seccond_energy NoOfenergy_steps]

Here is a list of the commands, switches and parameters of ELAST: 

 *switches and switchparameter*

    -v              : *Verbous* Tell everything that you know about 
                      the particles (and the reaction) in question.
    -q              : *Quite*. No copyright messages etc. in the output. This
                      switch should be used, if one wants to use the output as
                      an input for a seccond-level program. In fact, this is
                      the major purpose of ELAST.
    -Q              : *Very Quite* Same as q, but in addition, there are no
                      headers printed on the output tables (like dE/dX,
                      Energy Loss, ...). This is handy if a program like
                      gnuplot chockes on the colum headers.
    -s <sep._char>  : This option is used to redefine the ROW-seperator-
                      character in the output of ELAST to any desired
                      character. Default is a blank space " ". 
    -S <sep.-char>  : This option is used to redefine the line-seperator
                      character in the output of ELAST to any desired
                      character. Default is NewLine.
		      
		      This and the last switch are implemented to allow the
                      generation of tables for import in a picky tool, that
                      requires a certain seperator format.

    -g              : The Target is treated as a gas. By default, ELAST assumes
                      a solid state target. So far, the Ziegler tables used
                      are identical, and there is no difference
    -a <angel[deg]>:  The *angle* for kinematics calculation in the Lab is
        [angle2       given and followes this switch imidately. If -a
          DeltaAngle] is not used, the angle is by default 0 deg. Optionally,
                      a second angle and a stepwith can be supplied to loop
                      over a range of angles.
    -c <ion. level>   Integer number, that is used instead of Z for
                      calculations, that involve the ionic charge, like the
                      charge state distribution commands s,Rs,R4s (see
                      below in the command description)
    -x <Ex [MeV]>:    Excitation energy of beam particle 2 in 1(2,3)4 prior
                      to reaction. The given energy is converted to a mass
                      and added to the mass of particle 2 in the relativistic
                      kinematics calculation. Q displayed is changed
                      accordingly.
    -X <Ex [MeV]>:    Excitation energy of product particle 3 in 1(2,3)4 after
                      the reaction. The given energy is converted to a mass
                      and added to the mass of particle 2 in the relativistic
                      kinematics calculation. Q displayed is changed
                      accordingly.
                      If you override the internal Q-value, (value instead of
                      'x'), the value here is only used, if m3 < m4, otherwise
                      overwritten. I don't recomend to use -Q and -X at the
                      same time, since the outcome is compilcated!
                         
                      
 *commands* are

  Combinations of single or multiple letters as described below, either directely
  following each other or seperated by dots '.'.

  For particles crossing or stopping in the target due to elastic and nuclear
  straggeling including the exertation energies of electrons from the target
  atoms:

    A: [degree]
       Angle output (just print the angle that is used for some calculations)
    E: [MeV]
       Energy output (just print the energy that is used for this calculation)
    a: [degree]
       Angular straggeling (output in degree, 1sigma of an assumed gaussian).
    e: [MeV]
       Energy straggeling (1 sigma of an assumed gaussian).
    l: [MeV]
       energy Loss (calculated from the Ziegler formulas & tables)
    r: [mg/cm**2]
       Range until particles are stopped in thick target (bragg peak)
    p: [MeV/(mg/cm**2)]
       Power deposit per thickness unit "dE/dx"
    P: [MeV/(mg/cm**2)]
       Maximum Power deposited at one point in the target,
       i.g. the bragg peak, if a sufficient energetic particle is stopped
       completly or slowed down beoind the bragg peak.
    c: [e+]
       average charge state of the projectile in equilibrium at its
       energy AFTER the target. (Use o-command to see the energy)
       This crude estimate is resonabely good for Carbon.
    o: [MeV]
       Energy at end of target (= Energy - Energy loss)
    s: [1]
       Charge State population of the ion at the energy E after leaving a thin
       (but not too thin for equilibration) target foil as a fraction of one.
       Attention! For a composit target, only the first component is used
       in the target correction! This shortcomming can be used at a "feature"
       by adding a minimmal contribution of the material (e.g. carbon) as
       first component to simmulate a last target layer (e.g. a stripper 
       foil).
       A Robinett/Bobinett algorithm is used, for the Z-Target correction
       the procedure of Baron & Delaunay PRA 12(1975)40 is used.
       By default, the calculation for the fully stripped charge state (Q=Z)
       is performed. The c-switch allows to change this default to any value
       between 0 and Z. Values larger than Z may produce nonsense.

  For particles invloved in a binary nuclear reaction and values related to
  this problem. The RX-commands refer to this problem. The reaction assumed is
  Target(Projektil,Product)Residual, where Product is given and the residual
  is calculated. In a target from multiple components, the first component is
  used for the reaction.

    RE+: Energy of the product after the reaction (upper limit) without 
         losses due to straggeling.
    RE-: Energy of the product after the reaction (lower limit) without
         losses due to straggeling.
    RE0: Energy of the product after the reaction, if emmited at a given
         angle in the CM system. The parameter of the angle switch
         is interpreted as a CM-angle in degree for this purpose.
     all [MeV]
    RJ+: The Jakobian for the forward (or the only) solution
    RJ-: The Jakobian for the backward solution, if existant.
     all [1]
    RT:  The maximum angle of the reaction product (3rd partiucle) in the lab
         system
    Rt+: The CM angle of the 3rd particle, fwd. or only solution.
         0 if not possible
    Rt-: The CM angle of the 3rd particle, bwd. solution.
         0 if no bwd. solution.
     all [degree]
    Rs:  [1]
         Using the energy of the forward solution (as given by the
	 RE+-command), the charge state population of a given charge state
         is calculated. For more details, see the s-command.
         (see also c-switch)
    RC:  [MeV]
         Energy for the reaction in center of mass system, including Q-value

    A group of commands, that is probabely not that usefull, and stems from
    early versions of elast... However:

    Ra:  Angular straggeling of reaction product
             (output in degree *half* FWHM of beam (one direction only))
    Re:  Energy straggeling of reaction product (1 sigma)
    Rl:  energy Loss of reaction product, defined as
                                      E(projektile(in)) - E(product(out))
         This assmes the following: The projektile encounters a nuclear
         reaction at the position "target thickness" before the end of the
         target, and so the product has to make its way out through that
         layer. The total energy loss constits therfore of the reaction
         kinematic (average value and the energy loss by multible scattering
         of the product.
    Rc:  average Charge state of the projectile in equilibrium. The energy
         after all losses is used. (Use Ro-command to monitor used energy
         value)
    Ro:  Energy of reaction product after target. This assumes, that the
	 reaction with an Energy E happens in front of a target layer of the
         given thickness. Therfore, from am estimated middle reaktion energy,
         a energy loss due to straggeling in the target layer is substracted.
         For a better estimate, calculate for certaine solutions by hand.


   The following group for the 4th particle works in the following logic:
   Backward is the partner of the 3rd-particle forward solution. Therfore, if
   there is only one kinematic solution avaiable, the 4- = 4th-particle
   backward solution has non-zero values.

    R4s:  [1]
          Like Rs, but the backward/only energy of the 4th particle is used to
          calculate its likelyhood to be in a given charge state after a 
          thin target. (see also c-switch). Attention: The c-switch affects
          particles 2,3,4 in P1(P2,P3)P4. Therfore, a combination of Xs-commands
          is only of restricted value.
    R4E+: Energy of the 4th particle of the reaction (3rd forward solution)
    R4E-: Energy of the 4th particle of the reaction (3rd backward or only
          solution)
      all [MeV]
    R4A+: Angle of the 4th particle of the reaction (3rd forward solution)
    R4A-: Angle of the 4th particle of the reaction (3rd backward or only
          solution)
    R4t-: The CM angle of the 4rd particle, bwd. solution. 0, if no solution
          exists.
    R4t-: The CM angle of the 4rd particle, fwd. solution. 0 if no fwd
          solution.
      all [degree]

   Rutherford (and in futer other) cross-section commands start with the
   letter X. So far implemented:

    Xr: Rutherford cross-secions in the cm-system, using the angle argument
        as cm-angle, and the energy argument as LAB energy. The classical
        Lab2CM transformatioin of the energy is used: Ecm=Elab * M1 /(M1+M2).

    Data as internally used can be retrieved with D-commands. Currently avaible
    are this commands:

    Dm: Mass of the projectile in unit
    Dz: Nuclear charge of the projectile
    Ds: The Atomic Symbol of the target. Usefull in scripts, that use
        the (Z,M) notation for the target definition
    Dq: The Q-value as used in the kinematics, e.g. if there is a q-value
        given, that number (+- a little diviation for numerical reasons),
        and if -x or -X switches are used, the resulting q-value.
        NOTE: The dQ-command produces undefined output, if no reaction
              products are defined.

 Any combination of this commands, even repititions, are OK. The order of
 commands is meaningfull: The values will be calculated and printed in the
 same order, that the commands are placed in the command line. This is
 a feature to improve the flexibility in generating tables, not a bug!

  *target*

    The target may consist of more than one component. Therefore, 
    the complete target definition consists of a chain of *basic_target* -
    definitions. Each basic target definition beginns with the number of
    nuclei in one target moelecule (unit cell, ...) which is a integer number.
    This number is directly followed by a bracked which contains a definition
    of this target compound, that my constst of either 
       * The one or two character element symbol in capital letters
           In this case, the average atomic is used.
       * An mass number followed by an element symbol
       * Two interger Numbers, seperated by a comma: (Z,A)
    A valid target definition for Methane (two compounds) could be:

           1(C)4(H)               or, to exclude deuterium,
           1(C)4(1,1)             
	   1(C)4(1H)              or, to enrich target with C13:
           5(6,13)95(12C)400(H)   which are technical 3 compunds.

	   note, that (1H) and (1,1) or e.g. (6,13) and (13C) produce
                                                           the same result.

    Remember, however, that this programm is based on semi-empirical
    formulars, and can't handel details of nuclear physics.

  INSTEAD of a target, the letters DC can be given. This will set the 
    target to a gamma without energy, allowing for calculations of kinematics
    of decaying particles.
    In this mode, all energy loss commands make no sense, evenso syntactically
    allowed.

  *thickness*

    A real number in the format n.n, n and .n, but not n.nEn (not exponential
    writing). The unit is mg/cm**2. This parameter MUST be present, even if on
    is only interested in stopping propertys of a ion type in a certain target.
    In the latter case, the value auf *thickness* can be set to any number or
    even 0. (Note that "0" is not a missing parameter, but a parameter with
    value zero!). A thickness has also to be given for kinematics calculations
    that don't use the thickness at all.

    Note, that this is the target thickness assumed under 0deg. All
    calculations, that are affected by the target thickness assume the
    particle passing throug the target with the angle defined by the angle
    switch. This can be used, for example, to simmulate the influnece of
    a detector window/dead layer as fuction of incident angle of the particle!

  *projektile*

    Same as *basic_target*, BUT: the average mixture symbol (e.g. (C) ) is NOT
    supported, the projectile has to be a specific nucleus with fixed mass and
    charge.

    Neutrons or gammas as projectile are not supported so far, sorry.

  *reaction_product* (and implicit 4th reaction partner)

    Same as *projectile*. MUST be given, if reaction is needed for calculation,
    which is the case for all 2x-commands. MUST NOT be given, when not needed.
      (Sorry, but the parsing is complicated enough...)

    The 4th reaction partner is calculated from the balance of mass and charge.

    Neutrons (1ng in the output) and gammas (0ng in the output) are allowed
    as 4th paricle in reactions.

    Neutrons can be definded as 3rd particle (product) with some commands.
    A neutron can be written either as (0,1) or as (n), but not as (1n),
    because this would be interpretet as a (non-existing) Nitrogene isotope
    with A = 1. (=> error message, that Z has to be equal or bigger that A)

    Commands, which imply the calculation of neutrons passing through matter
    won't work (Energy loss, scattering, etc. commands with a leading "2".
    2E and 2T etc. work, because this is a strictly kinematic calculation.).

  *Q-value*

    A real value [MeV]. MUST be present, when *reaction-product* is give.
    May be negative or positive. MUST NOT be present, if *reaction-product* 
    is not given (no reaction calculation, see *reaction_product*

    Instead of a value, a 'd' (or 'x') can be given. In this case, elast will take
    the data from its built-in database. In case of the use of the -X or -x switch,
    the q-value is modified accordingly.

    If a Q-Value is given, elast will make the following assumtion about the
    masses of the particles:

       The Q-Value is the used as additional mass for the heavier Reaction
       product.

    This is important, because the relativiustic kinematic used her makes
    impliciet use of Q-values via masses. 

  *energy*

    A number in MeV. This energy is meassured in target system (which is
    normaly equal to th lab. system)

  *seccond energy*

    Syntax as *energy*
    
    If this optional parameter is given, energies starting at *energy* and
    ending at this value are printed in *energy_steps* steps which must be
    provided in this case. Since this is a floatin point calculation, DON'T
    reley on the last energy in a table to be exact *seccond energy*, even
    so I observed no problems so far.

  *energy_steps*

    An integer, has to be present, if *seccond_energy* is given.

    (See *seccond_energy*)

3. OUTPUT FORMAT

In this section, no difference is made wether the output file, which is by
default the UNIX standard output file, is redirected or not. See section
SWITCHES above.

The output auf ELAST is in general a table with a header in which the
requested values are listed in the order of aperance of the correlated command
(see section COMMANDS). The table has a header which can be switched off by the
-Q Option. If no *seccond_energy* and no angle range is given, the output
consists of the header and one line of data. Each line consists of so many
fileds as commands are given. They are seperated by a "TAB"-character
or whatever is defined by the -s -option. Lines are seperated by a
NewLine-character, or whatever is defined by the -S -option.

4. EXAMPLES

A few possible applications of elast for differnt purposes. Note, that
the quotation marks arround the target definitions are needed to keep
the unix shell from attempting to evaluate the brackets. Note also, that
floating point numbers don't necessarily need a period in them. (2 := 2.0)

A: Energy loss commands and related stuff

Calculate the Energy, a 30 MeV 16O ion has after passing through 
a 200mueg CH2-foil

     elast o "1(C)2(H)" 0.2 16O 30

...same, but also display energy loss, energy straggling, angular straggling,
the velocity of the incident particle and its average charge state.

     elast oleavc "1(C)2(H)" 0.2 16O 30

Make a table for energies between 30 and 60 MeV in 31 steps, that has
in the first colum the energy of the particle before the 0.2mg foil, in the 
seccond colum the energy loss and in the last colum the energy after the
foil. Don't display a Copy-right message (-q).

     elast -q Elo "1(C)2(H)" 0.2 16O 30 60 31

.. A table over in 11 energy steps for a 2mg foil, particles passing through
at an angle of 45deg.

     elast -qa 45 Elo "1(C)2(H)" 2 16O 30 60 11

.. Same, but now take only one energy (60 MeV), and loop over angles and
display the current angle instead of the energy!

     elast -qa 30 60 1 Alo "1(C)2(H)" 2 16O 60

.. Same, but even omitt the colum headings, and output in the file
"Alo.dat" for further use (plotting, e.g. with gnuplot!).

     elast -Qa 30 60 1 Alo "1(C)2(H)" 2 16O 60 > Alo.dat

B: Reactions
   
Calculate the maximum 14O product energy (RE+) for the famous p(17F,14O)Alpha
reaction at E(17F) = 60 MeV. Don't tell again, who wrote this program!
NOTE that the target thickness (0.1) is syntactical necessary, but not used
for this calculation!

     elast -q RE+ "1(1H)" 0.1 17F 14O x 60

Tell me everything you know about this reaction (-v instead of -q) and all
the parameter, I gave you!

     elast -v RE+ "1(1H)" 0.1 17F 14O x 60

.. and give also the low energy solution, the energys of the 4th particle
(the alpha) with the corresponding angels as well as the two Jakobians
for the 14O! All of that at an 14O angle of 2 degree (lots of R-commands)

     elast -qa 2 RE+RE-R4A+R4E+R4A-R4E-RJ+RJ- "1(1H)" 0.1 17F 14O x 60

.. do it for 16 energies between 40 and 65 MeV, showing the enrgy in the
first colum of the resulting table (E-command)! ComString is written 
with dots for better redability.

     elast -qa 2 E.A.RE+.RE-.R4A+.R4E+.R4A-.R4E-.RJ+.RJ- "1(1H)" 0.1 17F 14O x 40 65 16

.. do it for angles between 0 and 6 degree in 0.2 deg. setps at 60 MeV! Give
the anglke in the first colum (A-command)

    elast -qa 0 6 0.2 ARE+RE-R4A+R4E+R4A-R4E-RJ+RJ- "1(1H)" 0.1 17F 14O x 60

.. Generate a table without headers (-Q), that shows the Lab energy of the
product as function of the CM angle (RE0), and output it into a file!

    elast -Qa 0 180 1 ARE0 "1(1H)" 0.1 17F 14O x 60 > outputfile.dat

     NOTE that a combination of the RE0-command with others R- commands
     would cause a confusing output, since the angle in the same table
     would once mean the CM angle, and the lab Angle at the same time!
     However, elast would perform the calculation without complaints...

Another Example: In flight decay of an excited nucleus. A 18Ne-nucleus
traveling at 65 MeV (0.088 c0) in the lab system is excited at 6.1 MeV
and decays in a 14O particle and an alpha. Print a table, that contains
the kinematics of the 14O-particle in the lab system (angle, and the
two energy solutions) as well as the kinematics of the 4th particle,
the alpha, with the two angles and energies.

    elast -qx 6.1 -a 0 4 0.2 ARE+RE-R4A-R4E-R4A+R4E+ DC 0 18Ne 14O x 65

Kinematic Curves for Excited states of the product:

   plot the first halft of the e-theta relation in a file named GS.dat...

     elast -Qa 0 4.4 .1 ARE+ "1(2h)" .1 16O 17F x 70 > GS.dat

   then add the backward solution to the same file. Note, that we made
   the angels run backwards now (-a 4.4 0 -0.1) to allow a plot programm
   to connect the data-points. The ">>" adds to a file instead of
   overwriting is, as ">" would do.

     elast -Qa 4.4 0 -.1 ARE- "1(2h)" .1 16O 17F x 70 >> GS.dat

   in gnuplot, we could no say: 'plot "GS.dat"'

   if we wanted just a scatter plot, we could have instead used:

      elast -Qa 0 4.5 .1 ARE+RE- "1(2h)" .1 16O 17F x 70

   and just plotted the two colums seperately, in gnuplot for example:
         'plot "GS.dat" using 1:2, "GS.dat" using 1:3'

   Now, we make a file for the first excited state at 495 keV = 0.495 MeV,
   using the -X-switch

     elast -QX 0.495  -a 0 4.4 .1 ARE+RE- "1(2h)" .1 16O 17F x 70 > GS.dat

   Do not confuse the -X and -x switch: -x 0.495 would have given you an
   excited 16O-beam.

5. INSTALATION

Copy all files in a temporary directory and compile only elast.c with an
ANSI-C compiler (e.g. gcc). The program uses the floating point library, so 
don't forget the switch to link it (should be "-lm"). Since all data
will be included in the executable file, one needs only the executable file
itself to use the program.

You also can compile elast on a MS-DOS PC. I suceeded using gcc in a suitable
portation.








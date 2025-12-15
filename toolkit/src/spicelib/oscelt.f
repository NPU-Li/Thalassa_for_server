C$Procedure OSCELT ( Determine conic elements from state )

      SUBROUTINE OSCELT ( STATE, ET, MU, ELTS )

C$ Abstract
C
C     Determine the set of osculating conic orbital elements that
C     corresponds to the state (position, velocity) of a body at
C     some epoch.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     CONIC
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      ELTS   ( 8 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   State of body at epoch of elements.
C     ET         I   Epoch of elements.
C     MU         I   Gravitational parameter (GM) of primary body.
C     ELTS       O   Equivalent conic elements.
C
C$ Detailed_Input
C
C     STATE    is the state (position and velocity) of the body
C              at some epoch. Components are x, y, z, dx/dt, dy/dt,
C              dz/dt. STATE must be expressed relative to an
C              inertial reference frame. Units are km and km/sec.
C
C
C     ET       is the epoch of the input state, in ephemeris seconds
C              past J2000.
C
C                                                    3    2
C     MU       is the gravitational parameter (GM, km /sec ) of
C              the primary body.
C
C$ Detailed_Output
C
C     ELTS     are equivalent conic elements describing the orbit
C              of the body around its primary. The elements are,
C              in order:
C
C                 RP      Perifocal distance.
C                 ECC     Eccentricity.
C                 INC     Inclination.
C                 LNODE   Longitude of the ascending node.
C                 ARGP    Argument of periapsis.
C                 M0      Mean anomaly at epoch.
C                 T0      Epoch.
C                 MU      Gravitational parameter.
C
C              The epoch of the elements is the epoch of the input
C              state. Units are km, rad, rad/sec. The same elements
C              are used to describe all three types (elliptic,
C              hyperbolic, and parabolic) of conic orbit.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If MU is not positive, the error SPICE(NONPOSITIVEMASS)
C         is signaled.
C
C     2)  If the specific angular momentum vector derived from STATE
C         is the zero vector, the error SPICE(DEGENERATECASE)
C         is signaled.
C
C     3)  If the position or velocity vectors derived from STATE
C         is the zero vector, the error SPICE(DEGENERATECASE)
C         is signaled.
C
C     4)  If the inclination is determined to be zero or 180 degrees,
C         the longitude of the ascending node is set to zero.
C
C     5)  If the eccentricity is determined to be zero, the argument of
C         periapse is set to zero.
C
C     6)  If the eccentricity of the orbit is very close to but not
C         equal to zero, the argument of periapse may not be accurately
C         determined.
C
C     7)  For inclinations near but not equal to 0 or 180 degrees,
C         the longitude of the ascending node may not be determined
C         accurately. The argument of periapse and mean anomaly may
C         also be inaccurate.
C
C     8)  For eccentricities very close to but not equal to 1, the
C         results of this routine are unreliable.
C
C     9)  If the specific angular momentum vector is non-zero but
C         "close" to zero, the results of this routine are unreliable.
C
C     10) If STATE is expressed relative to a non-inertial reference
C         frame, the resulting elements are invalid. No error checking
C         is done to detect this problem.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The SPICELIB routine CONICS is the inverse of this routine:
C     CONICS maps a set of osculating elements and a time to a state
C     vector.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Determine the osculating elements of Phobos with respect to
C        Mars at some arbitrary time in the J2000 inertial reference
C        frame.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: oscelt_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              mar097.bsp                    Mars satellite ephemeris
C              gm_de431.tpc                  Gravitational constants
C              naif0012.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'mar097.bsp',
C                                  'gm_de431.tpc',
C                                  'naif0012.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM OSCELT_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ELTS   (8)
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      MU     (1)
C              DOUBLE PRECISION      STATE  (6)
C              DOUBLE PRECISION      STEP
C
C              INTEGER               DIM
C
C        C
C        C     Load the meta kernel listing the needed SPK, LSK and
C        C     PCK with gravitational parameters kernels.
C        C
C              CALL FURNSH ( 'oscelt_ex1.tm' )
C
C        C
C        C     Convert the time string to ephemeris time
C        C
C              CALL STR2ET ( 'Dec 25, 2007', ET )
C
C        C
C        C     Retrieve the state of Phobos with respect to Mars in
C        C     J2000.
C        C
C              CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS',
C             .               STATE,   LT                          )
C
C        C
C        C     Read the gravitational parameter for Mars.
C        C
C              CALL BODVRD ( 'MARS', 'GM', 1, DIM, MU )
C
C        C
C        C     Convert the state 6-vector to the elts 8-vector. Note:
C        C     BODVRD returns data as arrays, so to access the
C        C     gravitational parameter (the only value in the array),
C        C     we use MU(1).
C        C
C              CALL OSCELT ( STATE, ET, MU(1), ELTS )
C
C        C
C        C     Output the elts vector.
C        C
C              WRITE(*,'(A,F21.10)')
C             .  'Perifocal distance          (km): ', ELTS(1)
C              WRITE(*,'(A,F21.10)')
C             .  'Eccentricity                    : ', ELTS(2)
C              WRITE(*,'(A,F21.10)')
C             .  'Inclination                (deg): ', ELTS(3) * DPR( )
C              WRITE(*,'(A,F21.10)')
C             .  'Lon of ascending node      (deg): ', ELTS(4) * DPR( )
C              WRITE(*,'(A,F21.10)')
C             .  'Argument of periapsis      (deg): ', ELTS(5) * DPR( )
C              WRITE(*,'(A,F21.10)')
C             .  'Mean anomaly at epoch      (deg): ', ELTS(6) * DPR( )
C              WRITE(*,'(A,F21.10)')
C             .  'Epoch                        (s): ', ELTS(7)
C              WRITE(*,'(A,F21.10)')
C             .  'Gravitational parameter (km3/s2): ', ELTS(8)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Perifocal distance          (km):       9232.5746716211
C        Eccentricity                    :          0.0156113904
C        Inclination                (deg):         38.1225231660
C        Lon of ascending node      (deg):         47.0384055902
C        Argument of periapsis      (deg):        214.1546430017
C        Mean anomaly at epoch      (deg):        340.5048466068
C        Epoch                        (s):  251812865.1837092042
C        Gravitational parameter (km3/s2):      42828.3736206991
C
C
C     2) Calculate the history of Phobos's orbit plane inclination
C        with respect to Mars in the J2000 frame at intervals of six
C        months for a time interval of 10 years.
C
C        Use the meta-kernel from the first example.
C
C
C        Example code begins here.
C
C
C              PROGRAM OSCELT_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      SPD
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 24 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(TIMLEN)    UTCSTR
C
C              DOUBLE PRECISION      ELTS   (8)
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      MU     (1)
C              DOUBLE PRECISION      STATE  (6)
C              DOUBLE PRECISION      STEP
C
C              INTEGER               DIM
C              INTEGER               I
C
C        C
C        C     Load the meta kernel listing the needed SPK, LSK and
C        C     PCK with gravitational parameters kernels.
C        C
C              CALL FURNSH ( 'oscelt_ex1.tm' )
C
C        C
C        C     Read the gravitational parameter for Mars.
C        C
C              CALL BODVRD ( 'MARS', 'GM', 1, DIM, MU )
C
C        C
C        C     Convert the time string to ephemeris time
C        C
C              CALL STR2ET ( 'Jan 1, 2000 12:00:00', ET )
C
C        C
C        C     A step of six months - in seconds.
C        C
C              STEP = 180.0D0 * SPD( )
C
C        C
C        C     10 years in steps of six months starting
C        C     approximately Jan 1, 2000.
C        C
C              WRITE(*,'(A)') '        UCT Time          Inclination'
C              WRITE(*,'(A)') '------------------------  -----------'
C
C              DO I= 1, 20
C
C        C
C        C        Retrieve the state; convert to osculating elements.
C        C
C                 CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS',
C             .                  STATE,   LT                          )
C                 CALL OSCELT ( STATE, ET, MU(1), ELTS )
C
C        C
C        C        Convert the ephemeris time to calendar UTC.
C        C
C                 CALL ET2UTC ( ET, 'C', 3, UTCSTR )
C
C                 WRITE(*,'(A,X,F12.6)') UTCSTR,  ELTS(3) * DPR( )
C
C                 ET = ET + STEP
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C                UCT Time          Inclination
C        ------------------------  -----------
C        2000 JAN 01 12:00:00.000    36.055248
C        2000 JUN 29 12:00:00.000    37.112144
C        2000 DEC 26 12:00:00.000    38.152129
C        2001 JUN 24 12:00:00.000    37.552071
C        2001 DEC 21 12:00:00.000    36.242049
C        2002 JUN 19 11:59:59.999    36.330470
C        2002 DEC 16 12:00:00.000    37.674595
C        2003 JUN 14 11:59:59.999    38.121191
C        2003 DEC 11 12:00:00.001    36.973204
C        2004 JUN 08 11:59:59.999    36.033732
C        2004 DEC 05 12:00:00.001    36.844542
C        2005 JUN 03 11:59:59.999    38.077365
C        2005 NOV 30 12:00:00.001    37.786106
C        2006 MAY 29 11:59:58.999    36.413540
C        2006 NOV 25 11:59:59.001    36.171050
C        2007 MAY 24 11:59:58.999    37.448015
C        2007 NOV 20 11:59:59.001    38.189118
C        2008 MAY 18 11:59:58.999    37.223573
C        2008 NOV 14 11:59:59.001    36.084745
C        2009 MAY 13 11:59:57.999    36.608971
C
C
C$ Restrictions
C
C     1)  The input state vector must be expressed relative to an
C         inertial reference frame.
C
C     2)  Osculating elements are generally not useful for
C         high-accuracy work.
C
C     3)  Accurate osculating elements may be difficult to derive for
C         near-circular or near-equatorial orbits. Osculating elements
C         for such orbits should be used with caution.
C
C     4)  Extracting osculating elements from a state vector is a
C         mathematically simple but numerically challenging task. The
C         mapping from a state vector to equivalent elements is
C         undefined for certain state vectors, and the mapping is
C         difficult to implement with finite precision arithmetic for
C         states near the subsets of R6 where singularities occur.
C
C         In general, the elements found by this routine can have
C         two kinds of problems:
C
C         -  The elements are not accurate but still represent
C            the input state accurately. The can happen in
C            cases where the inclination is near zero or 180
C            degrees, or for near-circular orbits.
C
C         -  The elements are garbage. This can occur when
C            the eccentricity of the orbit is close to but
C            not equal to 1. In general, any inputs that cause
C            great loss of precision in the computation of the
C            specific angular momentum vector or the eccentricity
C            vector will result in invalid outputs.
C
C         For further details, see the $Exceptions section.
C
C         Users of this routine should carefully consider whether
C         it is suitable for their applications. One recommended
C         "sanity check" on the outputs is to supply them to the
C         SPICELIB routine CONICS and compare the resulting state
C         vector with the one supplied to this routine.
C
C$ Literature_References
C
C     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
C          Astrodynamics," Dover Publications Inc., 1971.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.2, 13-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples to $Examples section.
C
C-    SPICELIB Version 1.3.1, 28-FEB-2008 (NJB)
C
C        Updated $Index_Entries header section to use keywords
C        "osculating" and "convert." Updated $Particulars header
C        section to refer to CONICS. Fixed typo in in-line
C        comments.
C
C-    SPICELIB Version 1.3.0, 17-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C        The $Exceptions and $Restrictions header sections were updated.
C
C-    SPICELIB Version 1.2.0, 28-JAN-2003 (NJB) (EDW)
C
C        Bug fixes: routine previously didn't correctly compute
C        the argument of periapse or mean anomaly for some cases.
C        Also, the arguments of the ACOS and DACOSH functions were
C        able to go out of range, causing floating-point exceptions.
C
C        The computations of M0 and INC were re-coded for improved
C        accuracy.
C
C        Also, added error checks for non-positive MU, zero
C        position, velocity, and specific angular momentum vectors.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.2, 06-APR-1995 (WLT)
C
C        A typo was fixed in the description of the node vector
C        in the comments of the routine.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     conic elements from state
C     osculating elements from state
C     convert state to osculating elements
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.3.0, 17-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    SPICELIB Version 1.2.0, 28-JAN-2003 (NJB) (EDW)
C
C        Bug fixes: routine previously didn't correctly compute
C        the argument of periapse or mean anomaly for some cases.
C        Also, the arguments of the ACOS and DACOSH functions were
C        able to go out of range, causing floating-point exceptions.
C
C        The old computation of ARGP did not work for cases where
C        the inclination was 0 or pi: the sign of ARGP was sometimes
C        incorrect.
C
C        The new method uses the criterion: for inclination zero or pi
C        the argument of periapse is between zero and pi radians when
C
C           e  *  ( h x n )  >  0
C           -       -   -    -
C
C        where
C
C           e  is the eccentricity vector,
C           -
C
C           h  is the specific angular momentum vector,
C           -
C
C           n  is the node vector.
C           -
C
C        The computation of M0 was re-coded for improved accuracy.
C        The new computation uses ATAN2 rather than ACOS to find
C        the eccentric anomaly for the ellipse case. The quadrant
C        of M0 is now found by converting the position to the
C        perifocal frame and finding the corresponding longitude.
C
C        The old method, using the sign of <r,v>, did not work
C        for circular orbits and was unreliable for near-circular
C        orbits.
C
C        Inclination is now computed using VSEP.
C
C        Also, added error checks for non-positive MU, zero
C        position, velocity, and specific angular momentum vectors.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.2, 6-APR-1995 (WLT)
C
C        A typo was fixed in the description of the node vector
C        in the comments of the routine.
C
C-    Beta Version 1.0.1, 27-JAN-1989 (IMU)
C
C        $Examples section completed.
C
C-&


C
C     External functions
C
      EXTERNAL              PI

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DACOSH
      DOUBLE PRECISION      EXACT
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C
      DOUBLE PRECISION      CLOSE
      PARAMETER           ( CLOSE = 1.D-10 )

C
C     Local variables
C
      DOUBLE PRECISION      ARGP
      DOUBLE PRECISION      COSEA
      DOUBLE PRECISION      COSHF
      DOUBLE PRECISION      E      ( 3 )
      DOUBLE PRECISION      EA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      H      ( 3 )
      DOUBLE PRECISION      INC
      DOUBLE PRECISION      LNODE
      DOUBLE PRECISION      M0
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      NU
      DOUBLE PRECISION      P
      DOUBLE PRECISION      PERIX  ( 3 )
      DOUBLE PRECISION      PERIY  ( 3 )
      DOUBLE PRECISION      R      ( 3 )
      DOUBLE PRECISION      RMAG
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SINEA
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VMAG
      DOUBLE PRECISION      XPROD  ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

C
C     Saved variables
C
      SAVE                  ZVEC

C
C     Initial values
C
      DATA                  ZVEC / 0.D0, 0.D0, 1.D0 /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'OSCELT' )
      END IF


      IF ( MU .LE. 0.0D0 ) THEN

         CALL SETMSG ( 'MU = #; non-positive gravitational parameter' )
         CALL ERRDP  ( '#',  MU                                       )
         CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'                       )
         CALL CHKOUT ( 'OSCELT'                                       )
         RETURN

      END IF


C
C     In order to convert a position and velocity to an equivalent
C     set of (osculating) orbital elements, we need to determine three
C     principal vectors associated with the orbit:
C
C        h         The angular momentum vector. This is perpendicular
C        -         to the plane of the orbit.
C
C                        h = r X v
C                        -   -   -
C
C        n         The node vector. This is perpendicular to the
C        -         normals of both the reference and orbital planes;
C                  it lies in the intersection of these planes,
C                  pointing toward the ascending node.
C
C                             ^
C                        n  = k X h  = ( -h , h , 0 )
C                        -        -        y   x
C
C        e         The eccentricity vector. This lies in the plane
C        -         of the orbit, and points toward periapse. The
C                  magnitude of this vector is the eccentricity.
C
C                                      2
C                        e = (1/mu)( (v  - mu/r) r  -  <r,v> v )
C                        -                       -      - -  -
C
      CALL VEQU ( STATE(1), R )
      CALL VEQU ( STATE(4), V )

C
C     Check for non-physical cases. Probably due to user
C     input error
C
      IF ( VZERO(R) ) THEN

         CALL SETMSG ( 'Zero vector for input position vector.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'OSCELT'                                 )
         RETURN

      END IF

      IF ( VZERO(V) ) THEN

         CALL SETMSG ( 'Zero vector for input velocity vector.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'OSCELT'                                 )
         RETURN

      END IF


      RMAG   =  VNORM(R)
      VMAG   =  VNORM(V)

      CALL VCRSS ( R, V, H )

C
C     If the specific angular momentum vector is the zero vector,
C     we have a degenerate orbit and cannot proceed.
C
      IF ( VZERO(H) ) THEN

         CALL SETMSG ( 'Input position and velocity are too close '  //
     .                 'to parallel; the specific angular momentum ' //
     .                 'vector is zero.'                              )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                        )
         CALL CHKOUT ( 'OSCELT'                                       )
         RETURN

      END IF


      CALL VPACK  ( -H(2), H(1), 0.D0, N )

      CALL VLCOM  ( VMAG**2 - MU/RMAG,  R,  -VDOT(R,V),  V,  E )
      CALL VSCLIP ( 1.D0/MU, E )


C
C     We begin by determining the size and shape of the orbit.
C
C     The eccentricity of the orbit is the magnitude of the
C     eccentricity vector. If the eccentricity is "close" to one,
C     go ahead and make this a parabola.
C
C     The perifocal distance depends on the eccentricity and the
C     semi-latus rectum, which in turn orbit depends only on the
C     specific angular momentum of the orbiting object.
C

      ECC = EXACT ( VNORM ( E ), 1.D0, CLOSE )

      P   = VDOT  ( H,H ) / MU
      RP  = P / (1.D0 + ECC)

C
C     Next, the orientation of the orbit.
C                                                       ^
C     The inclination of the orbit is the angle between k (which is
C     perpendicular to the equator) and h (which is perpendicular to
C     the orbit.                        -
C
C     If close to zero or pi, make it exact. In either case, the node
C     vector becomes undefined.
C

      INC = VSEP ( H, ZVEC )

      IF ( ABS ( INC - 0.D0 ) .LT. CLOSE ) THEN
         INC = 0.D0
         CALL VPACK ( 1.D0, 0.D0, 0.D0, N )

      ELSE IF ( ABS ( INC - PI() ) .LT. CLOSE ) THEN
         INC = PI()
         CALL VPACK ( 1.D0, 0.D0, 0.D0, N )
      END IF

C
C                                                              ^
C     The longitude of the ascending node is the angle between i
C     (the x-axis) and the node vector, n.
C                                       -
C
      LNODE = ATAN2 ( N(2), N(1) )

      IF ( LNODE .LT. 0.D0 ) THEN
         LNODE = LNODE + TWOPI()
      END IF

C
C     The argument of periapsis is the angle between the node vector
C     n, and the eccentricity vector e. This is not defined for
C     -                              -
C     circular orbits.
C
C
      IF ( ECC .EQ. 0.D0 ) THEN

         ARGP = 0.D0

      ELSE

C
C        Set the magnitude of ARGP; we'll determine the sign next.
C
         ARGP = VSEP ( N, E )


         IF ( ARGP .NE. 0.D0 ) THEN

            IF (  ( INC .EQ. 0.D0 ) .OR. ( INC .EQ. PI() )  ) THEN
C
C              The quadrant of ARGP is determined by the component of E
C              in the direction H x N.
C
               CALL UCRSS ( H, N, XPROD )

               IF (  VDOT( E, XPROD ) .LT. 0.D0 ) THEN

                  ARGP = TWOPI() - ARGP

               END IF

            ELSE IF ( E(3) .LT. 0.D0 ) THEN
C
C              The periapsis is below the reference plane;  the argument
C              of periapsis must be greater than 180 degrees.
C
               ARGP = TWOPI() - ARGP

            END IF

         END IF

      END IF

C
C     And finally, the position of the object within the orbit.
C     The true anomaly, nu, is the angle between the eccentricity
C     and radius vectors, e and r. (For circular orbits, substitute
C     n for e.)           -     -
C     -     -
C
C     This angle increases in the counterclockwise direction about h.
C     We express the position in the perifocal frame in order to
C     extract nu.
C
      IF ( ECC .EQ. 0.D0 ) THEN
C
C        In this case, the argument of periapse is set to zero,
C        so the nu is measured from N.
C
         CALL VHAT  ( N, PERIX )

      ELSE
         CALL VHAT  ( E, PERIX )
      END IF

      CALL UCRSS ( H, PERIX, PERIY )

      NU = ATAN2 ( VDOT(R,PERIY), VDOT(R,PERIX) )


C
C     Unfortunately, the other element routines need the mean
C     anomaly, M. The true and mean anomalies are related through
C     the eccentric anomalies D (parabolas), E (ellipses), and
C     F (hyperbolas), as shown below.
C
C                      e + cos(nu)
C           cos(E)  = ---------------         (ellipse)
C                      1 + e cos(nu)
C
C           M       = E - e sin(E)
C
C
C                       e + cos(nu)
C           cosh(F) = ---------------         (hyperbola)
C                       1 + e cos(nu)
C
C           M       = e sinh(F) - F
C
C
C           D       = tan(nu/2)               (parabola)
C
C                           3
C           M       =  D + D / 3
C
C     For elliptic orbits, the mean anomaly should be in [0,2*pi].
C

      IF ( ECC .LT. 1.D0 ) THEN
C
C        For improved numerical performance, we compute both the
C        sine and cosine of the eccentric anomaly, then let ATAN2
C        find the eccentric anomaly.
C
         COSEA  = ( ECC + COS(NU) )  /  ( 1.D0 + ECC*COS(NU) )

C
C        Here we use the relationships (here b is the length
C        of the semi-minor axis):
C
C           a sin(E) = (a/b) r sin(nu)
C
C           sin(E)   = (r/b) sin(nu)
C                               ______________
C                    = (r/rp) \/ (1-e) / (1+e)  sin(nu)
C
C
         SINEA  = (RMAG/RP) * SQRT( (1.D0-ECC) / (1.D0+ECC) ) * SIN(NU)

         EA     =  ATAN2 ( SINEA, COSEA )

         M0     =  SIGN ( EA - ECC*SIN(EA),  NU )

         IF ( M0 .LT. 0.D0 ) THEN

            M0 = M0 + TWOPI()

         END IF


      ELSE IF ( ECC .GT. 1.D0 ) THEN

         COSHF = (ECC + COS(NU)) / (1.D0 + ECC*COS(NU))

         EA    =  DACOSH (  MAX ( 1.D0, COSHF )  )

         M0    =  SIGN( ECC*SINH(EA) - EA,  NU )


      ELSE

         EA = TAN(NU/2.D0)
         M0 = SIGN ( EA + EA**3/3.D0,  NU )

      END IF

C
C     Return the elements as a vector, suitable for input to CONICS.
C
      ELTS(1) = RP
      ELTS(2) = ECC
      ELTS(3) = INC
      ELTS(4) = LNODE
      ELTS(5) = ARGP
      ELTS(6) = M0
      ELTS(7) = ET
      ELTS(8) = MU


      CALL CHKOUT ( 'OSCELT' )
      RETURN
      END

C$Procedure OSCLTX ( Extended osculating elements from state )

      SUBROUTINE OSCLTX ( STATE, ET, MU, ELTS )

C$ Abstract
C
C     Determine the set of osculating conic orbital elements that
C     corresponds to the state (position, velocity) of a body at some
C     epoch. In additional to the classical elements, return the true
C     anomaly, semi-major axis, and period, if applicable.
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
C     ELEMENTS
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'oscltx.inc'

      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      ELTS   ( *  )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   State of body at epoch of elements.
C     ET         I   Epoch of elements.
C     MU         I   Gravitational parameter (GM) of primary body.
C     ELTS       O   Extended set of classical conic elements.
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
C                 NU      True anomaly at epoch.
C                 A       Semi-major axis. A is set to zero if
C                         it is not computable.
C                 TAU     Orbital period. Applicable only for
C                         elliptical orbits. Set to zero otherwise.
C
C              The epoch of the elements is the epoch of the input
C              state. Units are km, rad, rad/sec. The same elements
C              are used to describe all three types (elliptic,
C              hyperbolic, and parabolic) of conic orbit.
C
C              User applications should declare ELTS using the
C              parameter
C
C                 OSCXSZ
C
C              See the $Parameters section below.
C
C$ Parameters
C
C     OSCXSZ   is the size of the output elements array ELTS. OSCXSZ
C              is declared in the Fortran include file
C
C                 oscltx.inc
C
C              The output array ELTS is intended to contain unused
C              space to hold additional elements that may be added
C              in a later version of this routine. In order to
C              maintain forward compatibility, user applications
C              should declare ELTS as follows:
C
C                 DOUBLE PRECISION   ELTS( OSCXSZ )
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
C     11) The semi-major axis and period may not be computable for
C         orbits having eccentricity too close to 1. If the semi-major
C         axis is not computable, both it and the period are set to
C         zero. If the period is not computable, it is set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns in the first 8 elements of the array ELTS
C     the outputs computed by OSCELT, and in addition returns in
C     elements 9-11 the quantities:
C
C        ELTS(9)   true anomaly at ET, in radians.
C
C        ELTS(10)  orbital semi-major axis at ET, in km. Valid
C                  if and only if this value is non-zero.
C
C                  The semi-major axis won't be computable if the
C                  eccentricity of the orbit is too close to 1.
C                  In this case A is set to zero.
C
C        ELTS(11)  orbital period. If the period is not computable,
C                  TAU is set to zero.
C
C     The SPICELIB routine CONICS is an approximate inverse of this
C     routine: CONICS maps a set of osculating elements and a time to a
C     state vector.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Determine the osculating conic orbital elements of Phobos
C        with respect to Mars at some arbitrary time in the J2000
C        inertial reference frame, including true anomaly, semi-major
C        axis and period.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: oscltx_ex1.tm
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
C              PROGRAM OSCLTX_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'oscltx.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ELTS   ( OSCXSZ )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      MU     ( 1 )
C              DOUBLE PRECISION      STATE  ( 6 )
C              DOUBLE PRECISION      STEP
C
C              INTEGER               DIM
C
C        C
C        C     Load the meta kernel listing the needed SPK, LSK and
C        C     PCK with gravitational parameters kernels.
C        C
C              CALL FURNSH ( 'oscltx_ex1.tm' )
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
C              CALL OSCLTX ( STATE, ET, MU(1), ELTS )
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
C              WRITE(*,'(A,F21.10)')
C             .  'True anomaly at epoch      (deg): ', ELTS(9) * DPR( )
C              WRITE(*,'(A,F21.10)')
C             .  'Orbital semi-major axis     (km): ', ELTS(10)
C              WRITE(*,'(A,F21.10)')
C             .  'Orbital period               (s): ', ELTS(11)
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
C        True anomaly at epoch      (deg):        339.8966628076
C        Orbital semi-major axis     (km):       9378.9938051492
C        Orbital period               (s):      27577.0908930612
C
C
C     2) Calculate the history of Phobos's orbital period at intervals
C        of six months for a time interval of 10 years.
C
C        Use the meta-kernel from the first example.
C
C
C        Example code begins here.
C
C
C              PROGRAM OSCLTX_EX2
C              IMPLICIT NONE
C
C              INCLUDE 'oscltx.inc'
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
C              DOUBLE PRECISION      ELTS   ( OSCXSZ )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      MU     ( 1 )
C              DOUBLE PRECISION      STATE  ( 6 )
C              DOUBLE PRECISION      STEP
C
C              INTEGER               DIM
C              INTEGER               I
C
C        C
C        C     Load the meta kernel listing the needed SPK, LSK and
C        C     PCK with gravitational parameters kernels.
C        C
C              CALL FURNSH ( 'oscltx_ex1.tm' )
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
C              WRITE(*,'(A)') '        UCT Time             Period'
C              WRITE(*,'(A)') '------------------------  ------------'
C
C              DO I= 1, 20
C
C        C
C        C        Retrieve the state; convert to osculating elements.
C        C
C                 CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS',
C             .                  STATE,   LT                         )
C                 CALL OSCLTX ( STATE, ET, MU(1), ELTS )
C
C        C
C        C        Convert the ephemeris time to calendar UTC.
C        C
C                 CALL ET2UTC ( ET, 'C', 3, UTCSTR )
C
C                 WRITE(*,'(A,2X,F12.6)') UTCSTR,  ELTS(11)
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
C                UCT Time             Period
C        ------------------------  ------------
C        2000 JAN 01 12:00:00.000  27575.419249
C        2000 JUN 29 12:00:00.000  27575.124052
C        2000 DEC 26 12:00:00.000  27574.987749
C        2001 JUN 24 12:00:00.000  27574.273163
C        2001 DEC 21 12:00:00.000  27573.096137
C        2002 JUN 19 11:59:59.999  27572.262064
C        2002 DEC 16 12:00:00.000  27572.336386
C        2003 JUN 14 11:59:59.999  27572.576986
C        2003 DEC 11 12:00:00.001  27572.441912
C        2004 JUN 08 11:59:59.999  27572.338535
C        2004 DEC 05 12:00:00.001  27572.964737
C        2005 JUN 03 11:59:59.999  27574.450440
C        2005 NOV 30 12:00:00.001  27575.627595
C        2006 MAY 29 11:59:58.999  27576.174100
C        2006 NOV 25 11:59:59.001  27576.702123
C        2007 MAY 24 11:59:58.999  27577.625008
C        2007 NOV 20 11:59:59.001  27578.959155
C        2008 MAY 18 11:59:58.999  27579.545076
C        2008 NOV 14 11:59:59.001  27578.920610
C        2009 MAY 13 11:59:57.999  27577.800624
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples to $Examples section.
C
C-    SPICELIB Version 1.0.0, 02-FEB-2017 (NJB)
C
C        12-MAR-2015 (NJB)
C
C           Re-arranged test for small E to avoid overflow.
C           Changed definition of B to make the maximum value
C           of TAU equal to LIMIT. Removed test comparing
C           E/LIMIT to RMAG.
C
C        11-NOV-2014 (NJB)
C
C           Original version. Based on OSCELT version 1.3.1,
C           28-FEB-2008
C
C-&


C$ Index_Entries
C
C     extended conic elements from state
C     extended osculating elements from state
C     convert state to extended osculating elements
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      EXACT
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      DOUBLE PRECISION      SMALL
      PARAMETER           ( SMALL  = 1.D-10 )

      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 2.D2 )

C
C     Local variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      E
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      ECCVEC ( 3 )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      MUCUBR
      DOUBLE PRECISION      NU
      DOUBLE PRECISION      POLE   ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RHAT   ( 3 )
      DOUBLE PRECISION      RMAG
      DOUBLE PRECISION      RPERI  ( 3 )
      DOUBLE PRECISION      TAU
      DOUBLE PRECISION      VEL    ( 3 )
      DOUBLE PRECISION      VMAG
      DOUBLE PRECISION      XMAT   ( 3, 3 )

      LOGICAL               CMPTAU
      LOGICAL               PASS1

      SAVE                  LIMIT
      SAVE                  PASS1

      DATA                  PASS1 / .TRUE. /
      DATA                  LIMIT / 0.D0   /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'OSCLTX' )

      IF ( PASS1 ) THEN

         LIMIT = DPMAX() / MARGIN
         PASS1 = .FALSE.

      END IF

C
C     Initialize the members of ELTS that won't be set by OSCELT. This
C     is necessary because this routine may return early if A or TAU
C     cannot be computed.
C
      CALL CLEARD ( OSCXSZ-8, ELTS(9) )
      NU  = 0.D0
      TAU = 0.D0
      A   = 0.D0

C
C     Compute osculating elements using OSCELT. Take advantage
C     of OSCELT's error handling.
C
      CALL OSCELT ( STATE, ET, MU, ELTS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'OSCLTX' )
         RETURN
      END IF

C
C     Due to checks made by OSCELT, we know that MU and RMAG
C     are strictly positive.
C
C
C     Compute the true anomaly at ET. For the non-degenerate case, we
C     use equation 2.4-5 from [1] to compute the eccentricity vector:
C
C        _    1          2   mu    _     _  _  _
C        e = ---- * [ ( v - ---- ) r  - <r, v> v ]                   (1)
C             mu             r
C
C
C     where
C
C        _
C        e      is the eccentricity vector
C
C        _  _
C        r, v   are, respectively, the object's position and
C               velocity vectors expressed in the inertial
C               reference frame of the input state.
C                                                    _     _
C        r, v   are, respectively, the magnitudes of r and v
C
C        mu     is the GM value of the center of motion
C
C
C
      CALL UNORM ( STATE, RHAT, RMAG )
      CALL VEQU  ( STATE(4),    VEL  )

      VMAG = VNORM( VEL )

      ECC  = EXACT ( ELTS(2), 0.D0, SMALL )


      IF ( ECC .NE. 0.D0 ) THEN
C                                 _   _
C        C1 is the coefficient of r/||r|| in (1) above:
C
         C1 =    (1.D0/MU) * ( ( RMAG * VMAG**2 ) - MU )

C                                 _
C        C2 is the coefficient of v in (1) above:
C
         C2 =  - (1.D0/MU) * VDOT( STATE, VEL )

C
C        ECCVEC is the eccentricity vector:
C
         CALL VLCOM ( C1, RHAT, C2, VEL, ECCVEC )

C
C        POLE is the orbital pole unit vector.
C
         CALL UCRSS ( STATE, VEL, POLE )

C
C        Compute the transformation from the frame of the input state
C        to the perifocal frame.
C
         CALL TWOVEC ( ECCVEC, 1, POLE, 3, XMAT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'OSCLTX' )
            RETURN
         END IF

C
C        Transform the input position to the perifocal frame and
C        compute its "right ascension." This is the true anomaly NU,
C        confined to the range [0, 2*pi) radians.
C
         CALL MXV    ( XMAT,  STATE, RPERI     )
         CALL RECRAD ( RPERI, RANGE, NU,   DEC )

      ELSE
C
C        The orbit is circular or nearly so. The mean anomaly can
C        be used as the true anomaly.
C
         NU = ELTS( 6 )

      END IF

      ELTS(9) = NU

C
C     The semi-major axis A satisfies the relationship
C
C                mu
C        E =  - ----                                                 (2)
C                2A
C
C     where E represents the specific energy of the orbit, as long
C     as A is computable.
C
C     If the orbit is not parabolic or too close to parabolic,
C     we can recover A from (2) above:
C
C                mu
C        A =  - ----
C                2E
C
C     To make sure that A is computable, we require
C
C
C
C        |mu/E| < LIMIT
C
C     so
C
C        |mu/LIMIT| < |E|
C
C     We compute E from the specific kinetic and potential
C     energy of the orbit:
C
C               2
C              v     mu
C        E =  --- - ----                                             (3)
C              2     r
C
C     The second term on the right hand side is computable if
C
C        mu/LIMIT < r
C
C
      IF ( ELTS(2) .EQ. 1.D0 ) THEN
C
C        This is the parabolic case.
C
         CALL CHKOUT ( 'OSCLTX' )
         RETURN

      END IF


      IF ( RMAG .LE. ( MU/LIMIT ) ) THEN
C
C        We need RMAG > MU/LIMIT to make E computable.
C
C        We can't safely compute E.
C
         CALL CHKOUT ( 'OSCLTX' )
         RETURN

      END IF

C
C     We assume V and MU are small enough not to cause overflow.
C
      E = ( 0.5D0 * VMAG**2 ) - ( MU / RMAG )


      IF (  ABS(E)  .LT.  ( ABS(MU)/LIMIT )  ) THEN
C
C        We can't safely compute A. The case of E equal to 0
C        gets the code to this point.
C
         CALL CHKOUT ( 'OSCLTX' )
         RETURN

      END IF

C
C     At this point we can compute A (which is stored in ELTS(10)).
C
      A          = - MU / ( 2 * E )
      ELTS( 10 ) = A

C
C     If the orbit is elliptical, compute the period.
C
      ECC = ELTS(2)

      IF ( ECC .LT. 1.D0 ) THEN
C
C        The period is given by the formula
C
C                             3       1/2
C           tau = 2 * pi * ( a  / mu )
C
C        We need to make sure this computation doesn't
C        overflow.
C
C        If mu >= 1 then we can express the constraint
C        on a as
C
C                   1/3                         2/3
C           ( a / mu    )  <  ( LIMIT / (2*pi) )
C
C
C        Otherwise, we require that
C
C                                 2/3     1/3
C           a < ( LIMIT / (2*pi) )    * mu
C
C
C
C        Note that the case of non-positive MU has already
C        been ruled out.
C
         B      = ( LIMIT / TWOPI() )**(2.D0/3.D0)

         MUCUBR = MU ** (1.D0/3.D0 )


         IF ( MU .GE. 1.D0 ) THEN

            CMPTAU =  (  A / MUCUBR  )  .LT.    B
         ELSE
            CMPTAU =     A              .LT.  ( B * MUCUBR )
         END IF

         IF ( CMPTAU ) THEN

            TAU = TWOPI() * ( (A/MUCUBR)**(3.D0/2.D0) )

         END IF

      END IF

C
C     Assign the remaining members of ELTS.
C
      ELTS( 11 ) = TAU

      CALL CHKOUT ( 'OSCLTX' )
      RETURN
      END

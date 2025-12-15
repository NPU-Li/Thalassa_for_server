C$Procedure DRDPGR ( Derivative of rectangular w.r.t. planetographic )

      SUBROUTINE DRDPGR ( BODY, LON, LAT, ALT, RE, F, JACOBI )

C$ Abstract
C
C     Compute the Jacobian matrix of the transformation from
C     planetographic to rectangular coordinates.
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
C     COORDINATES
C     DERIVATIVES
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         BODY
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   Name of body with which coordinates are associated.
C     LON        I   Planetographic longitude of a point (radians).
C     LAT        I   Planetographic latitude of a point (radians).
C     ALT        I   Altitude of a point above reference spheroid.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     BODY     is the name of the body with which the planetographic
C              coordinate system is associated.
C
C              BODY is used by this routine to look up from the
C              kernel pool the prime meridian rate coefficient giving
C              the body's spin sense. See the $Files and $Particulars
C              header sections below for details.
C
C     LON      is the planetographic longitude of the input point. This
C              is the angle between the prime meridian and the meridian
C              containing the input point. For bodies having prograde
C              (aka direct) rotation, the direction of increasing
C              longitude is positive west: from the +X axis of the
C              rectangular coordinate system toward the -Y axis. For
C              bodies having retrograde rotation, the direction of
C              increasing longitude is positive east: from the +X axis
C              toward the +Y axis.
C
C              The earth, moon, and sun are exceptions:
C              planetographic longitude is measured positive east for
C              these bodies.
C
C              The default interpretation of longitude by this
C              and the other planetographic coordinate conversion
C              routines can be overridden; see the discussion in
C              $Particulars below for details.
C
C              Longitude is measured in radians. On input, the range
C              of longitude is unrestricted.
C
C     LAT      is the planetographic latitude of the input point. For a
C              point P on the reference spheroid, this is the angle
C              between the XY plane and the outward normal vector at
C              P. For a point P not on the reference spheroid, the
C              planetographic latitude is that of the closest point
C              to P on the spheroid.
C
C              Latitude is measured in radians. On input, the
C              range of latitude is unrestricted.
C
C     ALT      is the altitude of point above the reference spheroid.
C              Units of ALT must match those of RE.
C
C     RE       is the equatorial radius of a reference spheroid. This
C              spheroid is a volume of revolution: its horizontal
C              cross sections are circular. The shape of the
C              spheroid is defined by an equatorial radius RE and
C              a polar radius RP. Units of RE must match those of
C              ALT.
C
C     F        is the flattening coefficient =
C
C                 (RE-RP) / RE
C
C              where RP is the polar radius of the spheroid, and the
C              units of RP match those of RE.
C
C$ Detailed_Output
C
C     JACOBI   is the matrix of partial derivatives of the conversion
C              from planetographic to rectangular coordinates. It
C              has the form
C
C                 .-                              -.
C                 |  DX/DLON   DX/DLAT   DX/DALT   |
C                 |  DY/DLON   DY/DLAT   DY/DALT   |
C                 |  DZ/DLON   DZ/DLAT   DZ/DALT   |
C                 `-                              -'
C
C              evaluated at the input values of LON, LAT and ALT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the body name BODY cannot be mapped to a NAIF ID code,
C         and if BODY is not a string representation of an integer,
C         the error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the kernel variable
C
C            BODY<ID code>_PGR_POSITIVE_LON
C
C         is present in the kernel pool but has a value other
C         than one of
C
C             'EAST'
C             'WEST'
C
C         the error SPICE(INVALIDOPTION) is signaled. Case
C         and blanks are ignored when these values are interpreted.
C
C     3)  If polynomial coefficients for the prime meridian of BODY
C         are not available in the kernel pool, and if the kernel
C         variable BODY<ID code>_PGR_POSITIVE_LON is not present in
C         the kernel pool, the error SPICE(MISSINGDATA) is signaled.
C
C     4)  If the equatorial radius is non-positive, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     5)  If the flattening coefficient is greater than or equal to one,
C         the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C$ Files
C
C     This routine expects a kernel variable giving BODY's prime
C     meridian angle as a function of time to be available in the
C     kernel pool. Normally this item is provided by loading a PCK
C     file. The required kernel variable is named
C
C        BODY<body ID>_PM
C
C     where <body ID> represents a string containing the NAIF integer
C     ID code for BODY. For example, if BODY is 'JUPITER', then
C     the name of the kernel variable containing the prime meridian
C     angle coefficients is
C
C        BODY599_PM
C
C     See the PCK Required Reading for details concerning the prime
C     meridian kernel variable.
C
C     The optional kernel variable
C
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     also is normally defined via loading a text kernel. When this
C     variable is present in the kernel pool, the prime meridian
C     coefficients for BODY are not required by this routine. See the
C     $Particulars section for details.
C
C$ Particulars
C
C     It is often convenient to describe the motion of an object in the
C     planetographic coordinate system. However, when performing
C     vector computations it's hard to beat rectangular coordinates.
C
C     To transform states given with respect to planetographic
C     coordinates to states with respect to rectangular coordinates,
C     one makes use of the Jacobian of the transformation between the
C     two systems.
C
C     Given a state in planetographic coordinates
C
C        ( lon, lat, alt, dlon, dlat, dalt )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation:
C
C                    t          |                                  t
C        (dx, dy, dz)   = JACOBI|              * (dlon, dlat, dalt)
C                               |(lon,lat,alt)
C
C
C     This routine computes the matrix
C
C              |
C        JACOBI|
C              |(lon,lat,alt)
C
C
C     In the planetographic coordinate system, longitude is defined
C     using the spin sense of the body. Longitude is positive to the
C     west if the spin is prograde and positive to the east if the spin
C     is retrograde. The spin sense is given by the sign of the first
C     degree term of the time-dependent polynomial for the body's prime
C     meridian Euler angle "W":  the spin is retrograde if this term is
C     negative and prograde otherwise. For the sun, planets, most
C     natural satellites, and selected asteroids, the polynomial
C     expression for W may be found in a SPICE PCK kernel.
C
C     The earth, moon, and sun are exceptions: planetographic longitude
C     is measured positive east for these bodies.
C
C     If you wish to override the default sense of positive longitude
C     for a particular body, you can do so by defining the kernel
C     variable
C
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     where <body ID> represents the NAIF ID code of the body. This
C     variable may be assigned either of the values
C
C        'WEST'
C        'EAST'
C
C     For example, you can have this routine treat the longitude
C     of the earth as increasing to the west using the kernel
C     variable assignment
C
C        BODY399_PGR_POSITIVE_LON = 'WEST'
C
C     Normally such assignments are made by placing them in a text
C     kernel and loading that kernel via FURNSH.
C
C     The definition of this kernel variable controls the behavior of
C     the SPICELIB planetographic routines
C
C        PGRREC
C        RECPGR
C        DPGRDR
C        DRDPGR
C
C     It does not affect the other SPICELIB coordinate conversion
C     routines.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C
C     1) Find the planetographic state of the earth as seen from
C        Mars in the J2000 reference frame at January 1, 2005 TDB.
C        Map this state back to rectangular coordinates as a check.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: drdpgr_ex1.tm
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
C              de421.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00008.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM DRDPGR_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      RPD
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      DRECTN ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      F
C              DOUBLE PRECISION      JACOBI ( 3, 3 )
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      PGRVEL ( 3 )
C              DOUBLE PRECISION      RADII  ( 3 )
C              DOUBLE PRECISION      RE
C              DOUBLE PRECISION      RECTAN ( 3 )
C              DOUBLE PRECISION      RP
C              DOUBLE PRECISION      STATE  ( 6 )
C
C              INTEGER               N
C
C        C
C        C     Load SPK, PCK, and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'drdpgr_ex1.tm' )
C
C        C
C        C     Look up the radii for Mars.  Although we
C        C     omit it here, we could first call BADKPV
C        C     to make sure the variable BODY499_RADII
C        C     has three elements and numeric data type.
C        C     If the variable is not present in the kernel
C        C     pool, BODVRD will signal an error.
C        C
C              CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
C
C        C
C        C     Compute flattening coefficient.
C        C
C              RE  =  RADII(1)
C              RP  =  RADII(3)
C              F   =  ( RE - RP ) / RE
C
C        C
C        C     Look up the geometric state of earth as seen from Mars at
C        C     January 1, 2005 TDB, relative to the J2000 reference
C        C     frame.
C        C
C              CALL STR2ET ( 'January 1, 2005 TDB', ET )
C
C              CALL SPKEZR ( 'Earth', ET,    'J2000', 'LT+S',
C             .              'Mars',  STATE, LT               )
C
C        C
C        C     Convert position to planetographic coordinates.
C        C
C              CALL RECPGR ( 'MARS', STATE, RE, F, LON, LAT, ALT )
C
C        C
C        C     Convert velocity to planetographic coordinates.
C        C
C
C              CALL DPGRDR ( 'MARS', STATE(1), STATE(2), STATE(3),
C             .               RE,    F,        JACOBI             )
C
C              CALL MXV ( JACOBI, STATE(4), PGRVEL )
C
C        C
C        C     As a check, convert the planetographic state back to
C        C     rectangular coordinates.
C        C
C              CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN )
C
C              CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
C
C              CALL MXV ( JACOBI, PGRVEL, DRECTN )
C
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  X (km)                 = ', STATE(1)
C              WRITE(*,*) '  Y (km)                 = ', STATE(2)
C              WRITE(*,*) '  Z (km)                 = ', STATE(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular velocity:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  dX/dt (km/s)           = ', STATE(4)
C              WRITE(*,*) '  dY/dt (km/s)           = ', STATE(5)
C              WRITE(*,*) '  dZ/dt (km/s)           = ', STATE(6)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Ellipsoid shape parameters: '
C              WRITE(*,*) ' '
C              WRITE(*,*) '  Equatorial radius (km) = ', RE
C              WRITE(*,*) '  Polar radius      (km) = ', RP
C              WRITE(*,*) '  Flattening coefficient = ', F
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Planetographic coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  Longitude (deg)        = ', LON / RPD()
C              WRITE(*,*) '  Latitude  (deg)        = ', LAT / RPD()
C              WRITE(*,*) '  Altitude  (km)         = ', ALT
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Planetographic velocity:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  d Longitude/dt (deg/s) = ', PGRVEL(1)/RPD()
C              WRITE(*,*) '  d Latitude/dt  (deg/s) = ', PGRVEL(2)/RPD()
C              WRITE(*,*) '  d Altitude/dt  (km/s)  = ', PGRVEL(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates from inverse ' //
C             .           'mapping:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  X (km)                 = ', RECTAN(1)
C              WRITE(*,*) '  Y (km)                 = ', RECTAN(2)
C              WRITE(*,*) '  Z (km)                 = ', RECTAN(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular velocity from inverse mapping:'
C              WRITE(*,*) ' '
C              WRITE(*,*) '  dX/dt (km/s)           = ', DRECTN(1)
C              WRITE(*,*) '  dY/dt (km/s)           = ', DRECTN(2)
C              WRITE(*,*) '  dZ/dt (km/s)           = ', DRECTN(3)
C              WRITE(*,*) ' '
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Rectangular coordinates:
C
C           X (km)                 =    146039733.67043769
C           Y (km)                 =    278546605.40670651
C           Z (km)                 =    119750317.58721757
C
C         Rectangular velocity:
C
C           dX/dt (km/s)           =   -47.043272004450600
C           dY/dt (km/s)           =    9.0732615496727291
C           dZ/dt (km/s)           =    4.7579169009979010
C
C         Ellipsoid shape parameters:
C
C           Equatorial radius (km) =    3396.1900000000001
C           Polar radius      (km) =    3376.1999999999998
C           Flattening coefficient =    5.8860075555255261E-003
C
C         Planetographic coordinates:
C
C           Longitude (deg)        =    297.66765938292673
C           Latitude  (deg)        =    20.844504443932596
C           Altitude  (km)         =    336531825.52621418
C
C         Planetographic velocity:
C
C           d Longitude/dt (deg/s) =   -8.3577066632519065E-006
C           d Latitude/dt  (deg/s) =    1.5935566850478802E-006
C           d Altitude/dt  (km/s)  =   -11.211600779360412
C
C         Rectangular coordinates from inverse mapping:
C
C           X (km)                 =    146039733.67043760
C           Y (km)                 =    278546605.40670651
C           Z (km)                 =    119750317.58721757
C
C         Rectangular velocity from inverse mapping:
C
C           dX/dt (km/s)           =   -47.043272004450600
C           dY/dt (km/s)           =    9.0732615496727167
C           dZ/dt (km/s)           =    4.7579169009978992
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 12-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard..
C        Modified code example to use meta-kernel to load kernels.
C
C-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
C
C        Updated to save the input body name and ZZBODTRN state counter
C        and to do name-ID conversion only if the counter has changed.
C
C        Updated to call LJUCRS instead of CMPRSS/UCASE.
C
C-    SPICELIB Version 1.0.0, 26-DEC-2004 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. planetographic coordinates
C
C-&


C
C     SPICELIB functions
C
      INTEGER               PLNSNS

      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         PMTMP
      PARAMETER           ( PMTMP  = 'BODY#_PM' )

      CHARACTER*(*)         OVRTMP
      PARAMETER           ( OVRTMP = 'BODY#_PGR_POSITIVE_LON' )


      INTEGER               EARTH
      PARAMETER           ( EARTH  = 399 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32  )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80  )

      INTEGER               MOON
      PARAMETER           ( MOON   = 301 )

      INTEGER               SENSLN
      PARAMETER           ( SENSLN = 4 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10  )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )



C
C     Local variables
C
      CHARACTER*(LNSIZE)    KVALUE
      CHARACTER*(KVNMLN)    PMKVAR
      CHARACTER*(SENSLN)    PGRLON

      DOUBLE PRECISION      GEOLON

      INTEGER               BODYID
      INTEGER               I
      INTEGER               N
      INTEGER               SENSE

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVBDID
      LOGICAL               SVFND1

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVBDID
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DRDPGR' )

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

C
C     Convert the body name to an ID code.
C
      CALL ZZBODS2C ( SVCTR1, SVBODY, SVBDID, SVFND1,
     .                BODY, BODYID, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The value of the input argument BODY is #, '
     .   //            'this is not a recognized name of an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', BODY                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'DRDPGR'                                   )
         RETURN

      END IF

C
C     The equatorial radius must be positive. If not, signal an error
C     and check out.
C
      IF ( RE .LE. 0.0D0 ) THEN

         CALL SETMSG ( 'Equatorial radius was #.' )
         CALL ERRDP  ( '#', RE                    )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
         CALL CHKOUT ( 'DRDPGR'                   )
         RETURN

      END IF

C
C     If the flattening coefficient is greater than 1, the polar radius
C     is negative. If F is equal to 1, the polar radius is zero. Either
C     case is a problem, so signal an error and check out.
C
      IF ( F .GE. 1.D0 ) THEN

         CALL SETMSG ( 'Flattening coefficient was #.'  )
         CALL ERRDP  ( '#', F                           )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
         CALL CHKOUT ( 'DRDPGR'                         )
         RETURN

      END IF

C
C     Look up the longitude sense override variable from the
C     kernel pool.
C
      CALL REPMI  ( OVRTMP, '#',     BODYID, PMKVAR )
      CALL GCPOOL ( PMKVAR, 1, 1, N, KVALUE, FOUND  )

      IF ( FOUND ) THEN
C
C        Make sure we recognize the value of PGRLON.
C
         CALL LJUCRS ( 0, KVALUE, PGRLON )

         IF ( PGRLON .EQ. 'EAST' ) THEN

            SENSE =  1

         ELSE IF ( PGRLON .EQ. 'WEST' ) THEN

            SENSE = -1

         ELSE

            CALL SETMSG ( 'Kernel variable # may have the values ' //
     .                    'EAST or WEST.  Actual value was #.'    )
            CALL ERRCH  ( '#', PMKVAR                             )
            CALL ERRCH  ( '#', KVALUE                             )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                  )
            CALL CHKOUT ( 'DRDPGR'                                )
            RETURN

         END IF


      ELSE
C
C        Look up the spin sense of the body's prime meridian.
C
         SENSE = PLNSNS ( BODYID )

C
C        If the required prime meridian rate was not available,
C        PLNSNS returns the code 0.  Here we consider this situation
C        to be an error.
C
         IF ( SENSE .EQ. 0 ) THEN

            CALL REPMI  ( PMTMP, '#', BODYID, PMKVAR )

            CALL SETMSG ( 'Prime meridian rate coefficient defined ' //
     .                    'by kernel variable # is required but '    //
     .                    'not available for body #. '               )
            CALL ERRCH  ( '#', PMKVAR                                )
            CALL ERRCH  ( '#', BODY                                  )
            CALL SIGERR ( 'SPICE(MISSINGDATA)'                       )
            CALL CHKOUT ( 'DRDPGR'                                   )
            RETURN

         END IF

C
C        Handle the special cases:  earth, moon, and sun.
C
         IF (      ( BODYID .EQ. EARTH )
     .        .OR. ( BODYID .EQ. MOON  )
     .        .OR. ( BODYID .EQ. SUN   )  ) THEN

            SENSE = 1

         END IF

      END IF

C
C     At this point, SENSE is set to +/- 1.
C
C     Adjust the longitude according to the sense of the body's
C     spin, or according to the override value if one is provided.
C     We want positive east longitude.
C
      GEOLON = SENSE * LON

C
C     Now that we have geodetic longitude in hand, use the
C     geodetic equivalent of the input coordinates to find the
C     Jacobian matrix of rectangular coordinates with respect
C     to geodetic coordinates.
C
      CALL DRDGEO ( GEOLON, LAT, ALT, RE, F, JACOBI )

C
C     The matrix JACOBI is
C
C        .-                              -.
C        |  DX/DGEOLON  DX/DLAT  DX/DALT  |
C        |  DY/DGEOLON  DY/DLAT  DY/DALT  |
C        |  DZ/DGEOLON  DZ/DLAT  DZ/DALT  |
C        `-                              -'
C
C     which, applying the chain rule to D(*)/DGEOLON, is equivalent to
C
C        .-                                       -.
C        |  (1/SENSE) * DX/DLON  DX/DLAT  DX/DALT  |
C        |  (1/SENSE) * DY/DLON  DY/DLAT  DY/DALT  |
C        |  (1/SENSE) * DZ/DLON  DZ/DLAT  DZ/DALT  |
C        `-                                       -'
C
C     So, multiplying the first column of JACOBI by SENSE gives us the
C     matrix we actually want to compute:  the Jacobian matrix of
C     rectangular coordinates with respect to planetographic
C     coordinates.
C
      DO I = 1, 3

         JACOBI(I,1) = SENSE * JACOBI(I,1)

      END DO

      CALL CHKOUT ( 'DRDPGR' )
      RETURN
      END

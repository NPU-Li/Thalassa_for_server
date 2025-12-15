C$Procedure DRDGEO ( Derivative of rectangular w.r.t. geodetic )

      SUBROUTINE DRDGEO ( LON, LAT, ALT, RE, F, JACOBI )

C$ Abstract
C
C     Compute the Jacobian matrix of the transformation from geodetic
C     to rectangular coordinates.
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
C     LON        I   Geodetic longitude of point (radians).
C     LAT        I   Geodetic latitude of point (radians).
C     ALT        I   Altitude of point above the reference spheroid.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     LON      is the geodetic longitude of point (radians).
C
C     LAT      is the geodetic latitude  of point (radians).
C
C     ALT      is the altitude of point above the reference spheroid.
C
C     RE       is the equatorial radius of the reference spheroid.
C
C     F        is the flattening coefficient = (RE-RP) / RE,  where
C              RP is the polar radius of the spheroid. (More
C              importantly RP = RE*(1-F).)
C
C$ Detailed_Output
C
C     JACOBI   is the matrix of partial derivatives of the conversion
C              between geodetic and rectangular coordinates. It
C              has the form
C
C                 .-                             -.
C                 |  DX/DLON   DX/DLAT  DX/DALT   |
C                 |  DY/DLON   DY/DLAT  DY/DALT   |
C                 |  DZ/DLON   DZ/DLAT  DZ/DALT   |
C                 `-                             -'
C
C              evaluated at the input values of LON, LAT and ALT.
C
C              The formulae for computing X, Y, and Z from
C              geodetic coordinates are given below.
C
C                 X = [ALT +          RE/G(LAT,F)]*COS(LON)*COS(LAT)
C                 Y = [ALT +          RE/G(LAT,F)]*SIN(LON)*COS(LAT)
C                 Z = [ALT + RE*(1-F)**2/G(LAT,F)]*         SIN(LAT)
C
C              where
C
C                 RE is the polar radius of the reference spheroid.
C
C                 F  is the flattening factor (the polar radius is
C                    obtained by multiplying the equatorial radius by
C                    1-F).
C
C                 G( LAT, F ) is given by
C
C                    sqrt ( cos(lat)**2 + (1-f)**2 * sin(lat)**2 )
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the flattening coefficient is greater than or equal to
C         one, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2)  If the equatorial radius is non-positive, the error
C         SPICE(BADRADIUS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     It is often convenient to describe the motion of an object in
C     the geodetic coordinate system. However, when performing
C     vector computations its hard to beat rectangular coordinates.
C
C     To transform states given with respect to geodetic coordinates
C     to states with respect to rectangular coordinates, one makes use
C     of the Jacobian of the transformation between the two systems.
C
C     Given a state in geodetic coordinates
C
C          ( lon, lat, alt, dlon, dlat, dalt )
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
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the geodetic state of the earth as seen from
C        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
C        Map this state back to rectangular coordinates as a check.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: drdgeo_ex1.tm
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
C              pck00010.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00010.tpc',
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
C              PROGRAM DRDGEO_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      RPD
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1 = '(A,E18.8)' )
C
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
C              DOUBLE PRECISION      GEOVEL ( 3 )
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
C              CALL FURNSH ( 'drdgeo_ex1.tm' )
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
C        C     Look up the apparent state of earth as seen from Mars
C        C     at January 1, 2005 TDB, relative to the IAU_MARS
C        C     reference frame.
C        C
C              CALL STR2ET ( 'January 1, 2005 TDB', ET )
C
C              CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
C             .              'Mars',  STATE, LT                )
C
C        C
C        C     Convert position to geodetic coordinates.
C        C
C              CALL RECGEO ( STATE, RE, F, LON, LAT, ALT )
C
C        C
C        C     Convert velocity to geodetic coordinates.
C        C
C
C              CALL DGEODR (  STATE(1), STATE(2), STATE(3),
C             .               RE,       F,        JACOBI   )
C
C              CALL MXV ( JACOBI, STATE(4), GEOVEL )
C
C        C
C        C     As a check, convert the geodetic state back to
C        C     rectangular coordinates.
C        C
C              CALL GEOREC ( LON, LAT, ALT, RE, F, RECTAN )
C
C              CALL DRDGEO ( LON, LAT, ALT, RE, F, JACOBI )
C
C              CALL MXV ( JACOBI, GEOVEL, DRECTN )
C
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X (km)                 = ', STATE(1)
C              WRITE(*,FMT1) '  Y (km)                 = ', STATE(2)
C              WRITE(*,FMT1) '  Z (km)                 = ', STATE(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular velocity:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4)
C              WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5)
C              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Ellipsoid shape parameters: '
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Equatorial radius (km) = ', RE
C              WRITE(*,FMT1) '  Polar radius      (km) = ', RP
C              WRITE(*,FMT1) '  Flattening coefficient = ', F
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Geodetic coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Longitude (deg)        = ', LON / RPD()
C              WRITE(*,FMT1) '  Latitude  (deg)        = ', LAT / RPD()
C              WRITE(*,FMT1) '  Altitude  (km)         = ', ALT
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Geodetic velocity:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
C             .                                         GEOVEL(1)/RPD()
C              WRITE(*,FMT1) '  d Latitude/dt  (deg/s) = ',
C             .                                         GEOVEL(2)/RPD()
C              WRITE(*,FMT1) '  d Altitude/dt  (km/s)  = ', GEOVEL(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates from inverse ' //
C             .           'mapping:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1)
C              WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2)
C              WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular velocity from inverse mapping:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1)
C              WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2)
C              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3)
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
C          X (km)                 =    -0.76096183E+08
C          Y (km)                 =     0.32436380E+09
C          Z (km)                 =     0.47470484E+08
C
C         Rectangular velocity:
C
C          dX/dt (km/s)           =     0.22952075E+05
C          dY/dt (km/s)           =     0.53760111E+04
C          dZ/dt (km/s)           =    -0.20881149E+02
C
C         Ellipsoid shape parameters:
C
C          Equatorial radius (km) =     0.33961900E+04
C          Polar radius      (km) =     0.33762000E+04
C          Flattening coefficient =     0.58860076E-02
C
C         Geodetic coordinates:
C
C          Longitude (deg)        =     0.10320290E+03
C          Latitude  (deg)        =     0.81089876E+01
C          Altitude  (km)         =     0.33653182E+09
C
C         Geodetic velocity:
C
C          d Longitude/dt (deg/s) =    -0.40539288E-02
C          d Latitude/dt  (deg/s) =    -0.33189934E-05
C          d Altitude/dt  (km/s)  =    -0.11211601E+02
C
C         Rectangular coordinates from inverse mapping:
C
C          X (km)                 =    -0.76096183E+08
C          Y (km)                 =     0.32436380E+09
C          Z (km)                 =     0.47470484E+08
C
C         Rectangular velocity from inverse mapping:
C
C          dX/dt (km/s)           =     0.22952075E+05
C          dY/dt (km/s)           =     0.53760111E+04
C          dZ/dt (km/s)           =    -0.20881149E+02
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Changed the input argument name LONG to LON for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT)
C
C-&


C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. geodetic coordinates
C
C-&


C
C     SPICELIB functions
C
      LOGICAL          RETURN

C
C     Local parameters
C
      INTEGER               DX
      PARAMETER           ( DX     = 1 )

      INTEGER               DY
      PARAMETER           ( DY     = 2 )

      INTEGER               DZ
      PARAMETER           ( DZ     = 3 )

      INTEGER               DLON
      PARAMETER           ( DLON   = 1 )

      INTEGER               DLAT
      PARAMETER           ( DLAT   = 2 )

      INTEGER               DALT
      PARAMETER           ( DALT   = 3 )

C
C     Local variables
C
      DOUBLE PRECISION      CLAT
      DOUBLE PRECISION      CLON
      DOUBLE PRECISION      DGDLAT
      DOUBLE PRECISION      G
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLON

      DOUBLE PRECISION      FLAT
      DOUBLE PRECISION      FLAT2
      DOUBLE PRECISION      G2

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DRDGEO' )
      END IF

C
C     If the flattening coefficient is greater than one, the polar
C     radius computed below is negative. If it's equal to one, the
C     polar radius is zero. Either case is a problem, so signal an
C     error and check out.
C
      IF ( F .GE. 1.0D0 ) THEN

          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'DRDGEO'                         )
          RETURN

      END IF

      IF ( RE .LE. 0.0D0 ) THEN

          CALL SETMSG ( 'Equatorial Radius <= 0.0D0. RE = *' )
          CALL ERRDP  ( '*', RE                              )
          CALL SIGERR ( 'SPICE(BADRADIUS)'                   )
          CALL CHKOUT ( 'DRDGEO'                             )
          RETURN

      END IF

C
C     For the record, here is a derivation of the formulae for the
C     values of x, y and z as a function of longitude, latitude and
C     altitude.
C
C     First, let's take the case where the longitude is 0. Moreover,
C     lets assume that the length of the equatorial axis is a and
C     that the polar axis is b:
C
C        a = re
C        b = re * (1-f)
C
C     For any point on the spheroid where y is zero we know that there
C     is a unique q in the range (-Pi, Pi] such that
C
C        x = a cos(q) and z = b sin(q).
C
C     The normal to the surface at such a point is given by
C
C           cos(q)     sin(q)
C        ( ------- ,  ------- )
C             a          b
C
C     The unit vector in the same direction is
C
C                 b cos(q)                         a sin(q)
C        ( --------------------------  ,  -------------------------- )
C             ______________________         ______________________
C            / 2   2        2   2           / 2   2        2   2
C          \/ b cos (q)  + a sin (q)      \/ b cos (q)  + a sin (q)
C
C
C     The first component of this term is by definition equal to the
C     cosine of the geodetic latitude, thus
C
C                                ______________________
C                               / 2   2        2   2
C        b cos(q) = cos(lat)  \/ b cos (q)  + a sin (q)
C
C
C     This can be transformed to the equation
C
C                                ______________________________
C                               /   2    2     2        2
C        b cos(q) = cos(lat)  \/ ( b  - a  )cos (q)  + a
C
C
C     Squaring both sides and rearranging terms gives:
C
C         2   2         2         2   2     2        2    2
C        b cos (q) + cos (lat) ( a - b ) cos (q) =  a  cos (lat)
C
C     Thus
C                           2    2
C           2              a  cos (lat)
C        cos (q)  =  --------------------------
C                     2    2         2   2
C                    b  sin (lat) + a cos (lat)
C
C
C
C                             cos (lat)
C                 =  ------------------------------
C                       _____________________________
C                      /      2    2           2
C                    \/  (b/a)  sin (lat) + cos (lat)
C
C
C
C                             cos (lat)
C                 =  ---------------------------------
C                       _____________________________
C                      /      2    2           2
C                    \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C     From this one can also conclude that
C
C
C                           (1-f) sin (lat)
C        sin(q)   =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C     Thus the point on the surface of the spheroid is given by
C
C                            re * cos (lat)
C        x_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C                                  2
C                        re * (1-f) sin (lat)
C        z_0      =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C     Thus given a point with the same latitude but a non-zero
C     longitude, one can conclude that
C
C                         re * cos (lon) *cos (lat)
C        x_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C
C                         re * sin (lon) cos (lat)
C        y_0      =  ---------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C                                    2
C                          re * (1-f) sin (lat)
C        z_0      =  ----------------------------------
C                        _____________________________
C                       /      2    2           2
C                     \/  (1-f)  sin (lat) + cos (lat)
C
C
C     The unit normal, n, at this point is simply
C
C        ( cos(lon)cos(lat),  sin(lon)cos(lat),  sin(lat) )
C
C
C     Thus for a point at altitude alt, we simply add the vector
C
C        alt*n
C
C     to the vector ( x_0, y_0, z_0 ).  Hence we have
C
C        x = [ alt +          re/g(lat,f) ] * cos(lon) * cos(lat)
C        y = [ alt +          re/g(lat,f) ] * sin(lon) * cos(lat)
C        z = [ alt + re*(1-f)**2/g(lat,f) ] *            sin(lat)
C
C
C     We're going to need the sine and cosine of LAT and LON many
C     times.  We'll just compute them once.
C
      CLAT              = DCOS ( LAT )
      CLON              = DCOS ( LON )
      SLAT              = DSIN ( LAT )
      SLON              = DSIN ( LON )

C
C     Referring to the G given in the header we have...
C
      FLAT              = 1.D0 - F
      FLAT2             = FLAT * FLAT

      G                 = DSQRT( CLAT*CLAT   +   FLAT2 * SLAT*SLAT )
      G2                = G * G
      DGDLAT            = ( -1.0D0  +  FLAT2 ) * SLAT * CLAT / G

C
C     Now simply take the partial derivatives of the x,y,z w.r.t.
C     lon, lat, alt.
C
      JACOBI (DX,DLON)  =  - ( ALT + RE/G ) * SLON * CLAT
      JACOBI (DY,DLON)  =    ( ALT + RE/G ) * CLON * CLAT
      JACOBI (DZ,DLON)  =    0.0D0


      JACOBI (DX,DLAT)  =    (      -  RE*DGDLAT / G2 ) * CLON * CLAT
     .                     - ( ALT  +  RE        / G  ) * CLON * SLAT

      JACOBI (DY,DLAT)  =    (      -  RE*DGDLAT / G2 ) * SLON * CLAT
     .                     - ( ALT  +  RE        / G  ) * SLON * SLAT

      JACOBI (DZ,DLAT)  =    (      -  FLAT2*RE*DGDLAT / G2 ) * SLAT
     .                     + ( ALT  +  FLAT2*RE        / G  ) * CLAT


      JACOBI (DX,DALT)  =  CLON * CLAT
      JACOBI (DY,DALT)  =  SLON * CLAT
      JACOBI (DZ,DALT)  =         SLAT


      CALL CHKOUT ( 'DRDGEO' )
      RETURN
      END

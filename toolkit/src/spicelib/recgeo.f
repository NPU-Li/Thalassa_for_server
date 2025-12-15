C$Procedure RECGEO ( Rectangular to geodetic )

      SUBROUTINE RECGEO ( RECTAN, RE, F,  LON, LAT, ALT )

C$ Abstract
C
C     Convert from rectangular coordinates to geodetic coordinates.
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
C     CONVERSION
C     COORDINATES
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   RECTAN ( 3 )
      DOUBLE PRECISION   RE
      DOUBLE PRECISION   F
      DOUBLE PRECISION   LON
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   ALT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of a point.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     LON        O   Geodetic longitude of the point (radians).
C     LAT        O   Geodetic latitude  of the point (radians).
C     ALT        O   Altitude of the point above reference spheroid.
C
C$ Detailed_Input
C
C     RECTAN   are the rectangular coordinates of a point. RECTAN
C              must be in the same units as RE.
C
C     RE       is the equatorial radius of a reference spheroid. This
C              spheroid is a volume of revolution: its horizontal cross
C              sections are circular. The shape of the spheroid is
C              defined by an equatorial radius RE and a polar radius RP.
C              RE must be in the same units as RECTAN.
C
C     F        is the flattening coefficient = (RE-RP) / RE, where RP is
C              the polar radius of the spheroid.
C
C$ Detailed_Output
C
C     LON      is the geodetic longitude of the input point. This is the
C              angle between the prime meridian and the meridian
C              containing RECTAN. The direction of increasing longitude
C              is from the +X axis towards the +Y axis.
C
C              LON is output in radians. The range of LON is [-pi, pi].
C
C     LAT      is the geodetic latitude of the input point. For a point
C              P on the reference spheroid, this is the angle between
C              the XY plane and the outward normal vector at P. For a
C              point P not on the reference spheroid, the geodetic
C              latitude is that of the closest point to P on the
C              spheroid.
C
C              LAT is output in radians. The range of LAT is
C              [-pi/2, pi/2].
C
C     ALT      is the altitude of point above the reference spheroid.
C
C              The units associated with ALT are those associated with
C              the inputs RECTAN and RE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the equatorial radius is non-positive, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2)  If the flattening coefficient is greater than or equal to
C         one, the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3)  For points inside the reference ellipsoid, the nearest
C         point on the ellipsoid to RECTAN may not be unique, so
C         latitude may not be well-defined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given the body-fixed rectangular coordinates of a point, and the
C     constants describing the reference spheroid,  this routine
C     returns the geodetic coordinates of the point. The body-fixed
C     rectangular frame is that having the x-axis pass through the
C     0 degree latitude 0 degree longitude point. The y-axis passes
C     through the 0 degree latitude 90 degree longitude. The z-axis
C     passes through the 90 degree latitude point. For some bodies
C     this coordinate system may not be a right-handed coordinate
C     system.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the geodetic coordinates of the point having Earth
C        rectangular coordinates:
C
C           X (km) =  -2541.748162
C           Y (km) =   4780.333036
C           Z (km) =   3360.428190
C
C        Use the PCK kernel below to load the required triaxial
C        ellipsoidal shape model and orientation data for the Earth.
C
C           pck00010.tpc
C
C
C        Example code begins here.
C
C
C              PROGRAM RECGEO_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      F
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      RADII  ( 3 )
C              DOUBLE PRECISION      RE
C              DOUBLE PRECISION      RECTAN ( 3 )
C              DOUBLE PRECISION      RP
C
C              INTEGER               N
C
C        C
C        C     Load a PCK file containing a triaxial
C        C     ellipsoidal shape model and orientation
C        C     data for the Earth.
C        C
C              CALL FURNSH ( 'pck00010.tpc' )
C
C        C
C        C     Retrieve the triaxial radii of the Earth
C        C
C              CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII )
C
C        C
C        C     Compute flattening coefficient.
C        C
C              RE  =  RADII(1)
C              RP  =  RADII(3)
C              F   =  ( RE - RP ) / RE
C
C        C
C        C     Set a body-fixed position.
C        C
C              RECTAN(1) =  -2541.748162D0
C              RECTAN(2) =   4780.333036D0
C              RECTAN(3) =   3360.428190D0
C
C        C
C        C     Do the conversion. Output angles in degrees.
C        C
C              CALL RECGEO( RECTAN, RADII(1), F, LON, LAT, ALT )
C
C              WRITE (*,*) 'Rectangular coordinates in km (x, y, z)'
C              WRITE (*,'(A,3F14.6)') ' ', RECTAN
C              WRITE (*,*) 'Geodetic coordinates in deg and '
C             . //         'km (lon, lat, alt)'
C              WRITE (*,'(A,3F14.6)') ' ', LON * DPR(), LAT * DPR(), ALT
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Rectangular coordinates in km (x, y, z)
C           -2541.748162   4780.333036   3360.428190
C         Geodetic coordinates in deg and km (lon, lat, alt)
C             118.000000     31.999957      0.001916
C
C
C     2) Create a table showing a variety of rectangular coordinates
C        and the corresponding Earth geodetic coordinates. The
C        values are computed using the equatorial radius of the Clark
C        66 spheroid and the Clark 66 flattening factor:
C
C           radius: 6378.2064
C           flattening factor: 1./294.9787
C
C        Note: the values shown above may not be current or suitable
C              for your application.
C
C
C        Corresponding rectangular and geodetic coordinates are
C        listed to three decimal places. Output angles are in degrees.
C
C        Example code begins here.
C
C
C              PROGRAM RECGEO_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NREC
C              PARAMETER           ( NREC = 11 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      CLARKR
C              DOUBLE PRECISION      CLARKF
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      RECTAN ( 3, NREC )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the input rectangular coordinates.
C        C
C              DATA                 RECTAN /
C             .                  0.D0,         0.D0,         0.D0,
C             .               6378.2064D0,     0.D0,         0.D0,
C             .                  0.D0,      6378.2064D0,     0.D0,
C             .                  0.D0,         0.D0,      6378.2064D0,
C             .              -6378.2064D0,     0.D0,         0.D0,
C             .                  0.D0,     -6378.2064D0,     0.D0,
C             .                  0.D0,         0.D0,     -6378.2064D0,
C             .               6378.2064D0,  6378.2064D0,     0.D0,
C             .               6378.2064D0,     0.D0,      6378.2064D0,
C             .                  0.D0,      6378.2064D0,  6378.2064D0,
C             .               6378.2064D0,  6378.2064D0,  6378.2064D0 /
C
C        C
C        C     Using the equatorial radius of the Clark66 spheroid
C        C     (CLARKR = 6378.2064 km) and the Clark 66 flattening
C        C     factor (CLARKF = 1.0 / 294.9787 ) convert from
C        C     body fixed rectangular coordinates.
C        C
C              CLARKR = 6378.2064D0
C              CLARKF = 1.0D0 / 294.9787D0
C
C        C
C        C     Print the banner.
C        C
C              WRITE(*,*) ' RECTAN(1)  RECTAN(2)  RECTAN(3) '
C             . //        '   LON      LAT       ALT'
C              WRITE(*,*) ' ---------  ---------  --------- '
C             . //        ' -------  -------  ---------'
C
C        C
C        C     Do the conversion. Output angles in degrees.
C        C
C              DO I = 1, NREC
C
C                 CALL RECGEO( RECTAN(1,I), CLARKR, CLARKF,
C             .                LON,         LAT,    ALT    )
C
C                 WRITE (*,'(3F11.3,2F9.3,F11.3)')
C             .             ( RECTAN(J,I), J=1,3 ), LON * DPR(),
C             .             LAT * DPR(), ALT
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
C          RECTAN(1)  RECTAN(2)  RECTAN(3)    LON      LAT       ALT
C          ---------  ---------  ---------  -------  -------  ---------
C              0.000      0.000      0.000    0.000   90.000  -6356.584
C           6378.206      0.000      0.000    0.000    0.000      0.000
C              0.000   6378.206      0.000   90.000    0.000      0.000
C              0.000      0.000   6378.206    0.000   90.000     21.623
C          -6378.206      0.000      0.000  180.000    0.000      0.000
C              0.000  -6378.206      0.000  -90.000    0.000      0.000
C              0.000      0.000  -6378.206    0.000  -90.000     21.623
C           6378.206   6378.206      0.000   45.000    0.000   2641.940
C           6378.206      0.000   6378.206    0.000   45.137   2652.768
C              0.000   6378.206   6378.206   90.000   45.137   2652.768
C           6378.206   6378.206   6378.206   45.000   35.370   4676.389
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
C          Astrodynamics," Dover Publications Inc., 1971.
C
C$ Author_and_Institution
C
C     C.H. Acton         (JPL)
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR) (NJB)
C
C        Changed the output argument name LONG to LON for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples.
C
C-    SPICELIB Version 1.1.0, 03-AUG-2016 (BVS) (NJB)
C
C        Re-implemented derivation of longitude to improve
C        accuracy.
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.3, 02-JUL-2007 (NJB)
C
C        In $Examples section of header, description of right-hand
C        table was updated to use correct names of columns. Term
C        "bodyfixed" is now hyphenated.
C
C-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity. Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     rectangular to geodetic
C
C-&


C$ Revisions
C
C-    Beta Version 3.0.1, 9-JUN-1989 (HAN)
C
C        Error handling was added to detect and equatorial radius
C        whose value is less than or equal to zero.
C
C-    Beta Version 2.0.0, 21-DEC-1988 (HAN)
C
C        Error handling to detect invalid flattening coefficients
C        was added. Because the flattening coefficient is used to
C        compute the length of an axis, it must be checked so that
C        the length is greater than zero.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION BASE   (3)
      DOUBLE PRECISION NORMAL (3)
      DOUBLE PRECISION RADIUS

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RECGEO' )
      END IF


C
C     The equatorial radius must be positive. If not, signal an error
C     and check out.
C
      IF ( RE. LE. 0.0D0 ) THEN
          CALL SETMSG ( 'Equatorial radius was *.' )
          CALL ERRDP  ( '*', RE                    )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
          CALL CHKOUT ( 'RECGEO'                   )
          RETURN
      END IF

C
C     If the flattening coefficient is greater than one, the length
C     of the 'C' axis computed below is negative. If it's equal to one,
C     the length of the axis is zero. Either case is a problem, so
C     signal an error and check out.
C
      IF ( F .GE. 1 ) THEN
          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'RECGEO'                         )
          RETURN
      END IF

C
C     Determine the lengths of the axes of the reference ellipsoid.
C
      A = RE
      B = RE
      C = RE - F*RE

C
C     Find the point on the reference spheroid closest to the input
C     point. From this closest point determine the surface normal.
C
      CALL NEARPT ( RECTAN, A, B, C, BASE, ALT    )
      CALL SURFNM (         A, B, C, BASE, NORMAL )
C
C     Using the surface normal, determine the latitude and longitude
C     of the input point.
C
      CALL RECLAT ( NORMAL, RADIUS, LON, LAT )

C
C     Compute longitude directly rather than from the normal vector.
C
      IF (       ( RECTAN(1) .EQ.0.D0 )
     .     .AND. ( RECTAN(2) .EQ.0.D0 ) ) THEN

         LON = 0.D0
      ELSE
         LON = DATAN2 ( RECTAN(2), RECTAN(1) )
      END IF

      CALL CHKOUT ( 'RECGEO' )
      RETURN
      END

C$Procedure GEOREC ( Geodetic to rectangular coordinates )

      SUBROUTINE GEOREC ( LON, LAT, ALT,  RE, F,  RECTAN )

C$ Abstract
C
C     Convert geodetic coordinates to rectangular coordinates.
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

      DOUBLE PRECISION   LON
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   ALT
      DOUBLE PRECISION   RE
      DOUBLE PRECISION   F
      DOUBLE PRECISION   RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LON        I   Geodetic longitude of point (radians).
C     LAT        I   Geodetic latitude  of point (radians).
C     ALT        I   Altitude of point above the reference spheroid.
C     RE         I   Equatorial radius of the reference spheroid.
C     F          I   Flattening coefficient.
C     RECTAN     O   Rectangular coordinates of point.
C
C$ Detailed_Input
C
C     LON      is the geodetic longitude of the input point. This is
C              the angle between the prime meridian and the meridian
C              containing RECTAN. The direction of increasing
C              longitude is from the +X axis towards the +Y axis.
C
C              Longitude is measured in radians. On input, the
C              range of longitude is unrestricted.
C
C     LAT      is the geodetic latitude of the input point. For a
C              point P on the reference spheroid, this is the angle
C              between the XY plane and the outward normal vector at
C              P. For a point P not on the reference spheroid, the
C              geodetic latitude is that of the closest point to P on
C              the spheroid.
C
C              Latitude is measured in radians. On input, the
C              range of latitude is unrestricted.
C
C     ALT      is the altitude of point above the reference spheroid.
C              ALT must be in the same units as RE.
C
C     RE       is the equatorial radius of a reference spheroid. This
C              spheroid is a volume of revolution: its horizontal
C              cross sections are circular. The shape of the
C              spheroid is defined by an equatorial radius RE and
C              a polar radius RP. RE must be in the same units
C              as ALT.
C
C     F        is the flattening coefficient = (RE-RP) / RE,  where
C              RP is the polar radius of the spheroid.
C
C$ Detailed_Output
C
C     RECTAN   are the rectangular coordinates of a point.
C
C              The units associated with RECTAN are those associated
C              with the inputs ALT and RE.
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
C     2)  If the equatorial radius is less than or equal to zero,
C         the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given the geodetic coordinates of a point, and the constants
C     describing the reference spheroid,  this routine returns the
C     bodyfixed rectangular coordinates of the point. The bodyfixed
C     rectangular frame is that having the x-axis pass through the
C     0 degree latitude 0 degree longitude point. The y-axis passes
C     through the 0 degree latitude 90 degree longitude. The z-axis
C     passes through the 90 degree latitude point. For some bodies
C     this coordinate system may not be a right-handed coordinate
C     system.
C
C$ Examples
C
C     This routine can be used to convert body fixed geodetic
C     coordinates (such as the used for United States Geological
C     Survey topographic maps) to bodyfixed rectangular coordinates
C     such as the Satellite Tracking and Data Network of 1973.
C
C     1) Find the rectangular coordinates of the point having Earth
C        geodetic coordinates:
C
C           LON (deg) =  118.0
C           LAT (deg) =   32.0
C           ALT (km)  =    0.0
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
C              PROGRAM GEOREC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
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
C        C     Set a geodetic position.
C        C
C              LON = 118.D0 * RPD()
C              LAT =  30.D0 * RPD()
C              ALT =   0.D0
C
C        C
C        C     Do the conversion.
C        C
C              CALL GEOREC( LON, LAT, ALT, RADII(1), F, RECTAN )
C
C              WRITE (*,*) 'Geodetic coordinates in deg and '
C             . //         'km (lon, lat, alt)'
C              WRITE (*,'(A,3F14.6)') ' ', LON * DPR(), LAT * DPR(), ALT
C              WRITE (*,*) 'Rectangular coordinates in km (x, y, z)'
C              WRITE (*,'(A,3F14.6)') ' ', RECTAN
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Geodetic coordinates in deg and km (lon, lat, alt)
C             118.000000     30.000000      0.000000
C         Rectangular coordinates in km (x, y, z)
C           -2595.359123   4881.160589   3170.373523
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
C        listed to three decimal places. Input angles are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM GEOREC_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      RPD
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
C              DOUBLE PRECISION      ALT    ( NREC )
C              DOUBLE PRECISION      CLARKR
C              DOUBLE PRECISION      CLARKF
C              DOUBLE PRECISION      LAT    ( NREC )
C              DOUBLE PRECISION      LON    ( NREC )
C              DOUBLE PRECISION      RECTAN ( 3    )
C              DOUBLE PRECISION      RLAT
C              DOUBLE PRECISION      RLON
C
C              INTEGER               I
C
C        C
C        C     Define the input geodetic coordinates. Angles in
C        C     degrees.
C        C
C              DATA                 LON /  0.D0,   0.D0,  90.D0,
C             .                            0.D0, 180.D0, -90.D0,
C             .                            0.D0,  45.D0,   0.D0,
C             .                           90.D0,  45.D0         /
C
C              DATA                 LAT / 90.D0,     0.D0,     0.D0,
C             .                           90.D0,     0.D0,     0.D0,
C             .                          -90.D0,     0.D0,    88.707D0,
C             .                           88.707D0, 88.1713D0         /
C
C              DATA                 ALT /   -6356.5838D0,     0.D0,
C             .                   0.D0,         0.D0,         0.D0,
C             .                   0.D0,         0.D0,         0.D0,
C             .               -6355.5725D0, -6355.5725D0, -6355.5612D0 /
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
C              WRITE(*,*) '   LON      LAT       ALT    '
C             . //        ' RECTAN(1)  RECTAN(2)  RECTAN(3)'
C              WRITE(*,*) ' -------  -------  --------- '
C             . //        ' ---------  ---------  ---------'
C
C        C
C        C     Do the conversion.
C        C
C              DO I = 1, NREC
C
C                 RLON = LON(I) * RPD()
C                 RLAT = LAT(I) * RPD()
C
C                 CALL GEOREC( RLON,   RLAT,   ALT(I),
C             .                CLARKR, CLARKF, RECTAN )
C
C                 WRITE (*,'(2F9.3,F11.3,3F11.3)')
C             .             LON(I), LAT(I), ALT(I), RECTAN
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
C            LON      LAT       ALT     RECTAN(1)  RECTAN(2)  RECTAN(3)
C          -------  -------  ---------  ---------  ---------  ---------
C            0.000   90.000  -6356.584      0.000      0.000      0.000
C            0.000    0.000      0.000   6378.206      0.000      0.000
C           90.000    0.000      0.000      0.000   6378.206      0.000
C            0.000   90.000      0.000      0.000      0.000   6356.584
C          180.000    0.000      0.000  -6378.206      0.000      0.000
C          -90.000    0.000      0.000      0.000  -6378.206      0.000
C            0.000  -90.000      0.000      0.000      0.000  -6356.584
C           45.000    0.000      0.000   4510.073   4510.073      0.000
C            0.000   88.707  -6355.573      1.000      0.000      1.000
C           90.000   88.707  -6355.573      0.000      1.000      1.000
C           45.000   88.171  -6355.561      1.000      1.000      1.000
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
C     H.A. Neilan        (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 01-OCT-2021 (JDR) (NJB)
C
C        Changed the input argument name LONG to LON for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples.
C
C-    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS)
C
C        Minor headers edits.
C
C-    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity. Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
C
C-&


C$ Index_Entries
C
C     geodetic to rectangular coordinates
C
C-&


C$ Revisions
C
C-    Beta Version 3.0.0, 09-JUN-1989  (HAN)
C
C        Error handling added to detect equatorial radius out of
C        range. If the equatorial radius is less than or equal to
C        zero, an error is signaled.
C
C-    Beta Version 2.0.0, 21-DEC-1988 (HAN)
C
C        Error handling to detect invalid flattening coefficients
C        was added. Because the flattening coefficient is used to
C        compute the polar radius, it must be checked so that the
C        polar radius greater than zero.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL          RETURN


C
C     Local variables
C
      DOUBLE PRECISION HEIGHT

      DOUBLE PRECISION RP
      DOUBLE PRECISION CLMBDA
      DOUBLE PRECISION SLMBDA
      DOUBLE PRECISION CPHI
      DOUBLE PRECISION SPHI
      DOUBLE PRECISION BIG
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION SCALE

      DOUBLE PRECISION BASE   (3)
      DOUBLE PRECISION NORMAL (3)




C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GEOREC' )
      END IF



C
C     The equatorial radius must be greater than zero.
C
      IF ( RE .LE. 0.0D0 ) THEN
          CALL SETMSG ( 'Equatorial radius was *.' )
          CALL ERRDP  ( '*', RE                    )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
          CALL CHKOUT ( 'GEOREC'                   )
          RETURN
      END IF

C
C     If the flattening coefficient is greater than one, the polar
C     radius computed below is negative. If it's equal to one, the
C     polar radius is zero. Either case is a problem, so signal an
C     error and check out.
C
      IF ( F .GE. 1 ) THEN
          CALL SETMSG ( 'Flattening coefficient was *.'  )
          CALL ERRDP  ( '*', F                           )
          CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
          CALL CHKOUT ( 'GEOREC'                         )
          RETURN
      END IF


C
C     Move the altitude to a temporary variable.
C
      HEIGHT = ALT


C
C     Compute the polar radius of the spheroid.
C
      RP     = RE - (F * RE)

C
C     Compute a scale factor needed for finding the rectangular
C     coordinates of a point with altitude 0 but the same geodetic
C     latitude and longitude as the input point.
C
      CPHI   = DCOS(LAT)
      SPHI   = DSIN(LAT)
      CLMBDA = DCOS(LON)
      SLMBDA = DSIN(LON)

      BIG    = MAX ( DABS(RE*CPHI), DABS(RP*SPHI) )

      X      = RE*CPHI / BIG
      Y      = RP*SPHI / BIG

      SCALE  = 1.0D0   / (BIG * DSQRT(X*X + Y*Y) )

C
C     Compute the rectangular coordinates of the point with zero
C     altitude.
C
      BASE(1) = SCALE * RE * RE * CLMBDA * CPHI
      BASE(2) = SCALE * RE * RE * SLMBDA * CPHI
      BASE(3) = SCALE * RP * RP * SPHI

C
C     Fetch the normal to the ellipsoid at this point.
C
      CALL SURFNM ( RE, RE, RP, BASE, NORMAL )

C
C     Move along the normal to the input point.
C
      CALL VLCOM ( 1.0D0, BASE, HEIGHT, NORMAL, RECTAN )



      CALL CHKOUT ( 'GEOREC' )
      RETURN
      END

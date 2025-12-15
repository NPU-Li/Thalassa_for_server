C$Procedure RECLAT ( Rectangular to latitudinal coordinates )

      SUBROUTINE RECLAT ( RECTAN, RADIUS, LON, LAT )

C$ Abstract
C
C     Convert from rectangular coordinates to latitudinal coordinates.
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
      DOUBLE PRECISION   RADIUS
      DOUBLE PRECISION   LON
      DOUBLE PRECISION   LAT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of the point.
C     RADIUS     O   Distance of a point from the origin.
C     LON        O   Longitude of point in radians.
C     LAT        O   Latitude of point in radians.
C
C$ Detailed_Input
C
C     RECTAN   are the rectangular coordinates of a point.
C
C$ Detailed_Output
C
C     RADIUS   is the distance of a point from the origin.
C
C              The units associated with RADIUS are those
C              associated with the input RECTAN.
C
C     LON      is the longitude of the input point. This is the
C              angle between the prime meridian and the meridian
C              containing the point. The direction of increasing
C              longitude is from the +X axis towards the +Y axis.
C
C              LON is output in radians. The range of LON is
C              [ -pi, pi].
C
C     LAT      is the latitude of the input point. This is the angle
C              from the XY plane of the ray from the origin through
C              the point.
C
C              LAT is output in radians. The range of LAT is
C              [-pi/2, pi/2].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the X and Y components of RECTAN are both zero, the
C         longitude is set to zero.
C
C     2)  If RECTAN is the zero vector, longitude and latitude are
C         both set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the latitudinal coordinates of a point
C     whose position is input in rectangular coordinates.
C
C     Latitudinal coordinates are defined by a distance from a central
C     reference point, an angle from a reference meridian, and an angle
C     above the equator of a sphere centered at the central reference
C     point.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the latitudinal coordinates of the position of the
C        Moon as seen from the Earth, and convert them to rectangular
C        coordinates.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: reclat_ex1.tm
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
C              naif0012.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
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
C              PROGRAM RECLAT_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1 = '(A,F20.8)' )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      POS    ( 3 )
C              DOUBLE PRECISION      RADIUS
C              DOUBLE PRECISION      RECTAN ( 3 )
C
C        C
C        C     Load SPK and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'reclat_ex1.tm' )
C
C        C
C        C     Look up the geometric state of the Moon as seen from
C        C     the Earth at 2017 Mar 20, relative to the J2000
C        C     reference frame.
C        C
C              CALL STR2ET ( '2017 Mar 20', ET )
C
C              CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
C             .              'Earth', POS, LT               )
C
C        C
C        C     Convert the position vector POS to latitudinal
C        C     coordinates.
C        C
C              CALL RECLAT ( POS, RADIUS, LON, LAT )
C
C        C
C        C     Convert the latitudinal to rectangular coordinates.
C        C
C
C              CALL LATREC ( RADIUS, LON, LAT, RECTAN )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original rectangular coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X          (km): ', POS(1)
C              WRITE(*,FMT1) '  Y          (km): ', POS(2)
C              WRITE(*,FMT1) '  Z          (km): ', POS(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Latitudinal coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Radius     (km): ', RADIUS
C              WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
C              WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates from LATREC:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
C              WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
C              WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
C              WRITE(*,*) ' '
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Original rectangular coordinates:
C
C          X          (km):      -55658.44323296
C          Y          (km):     -379226.32931475
C          Z          (km):     -126505.93063865
C
C         Latitudinal coordinates:
C
C          Radius     (km):      403626.33912495
C          Longitude (deg):         -98.34959789
C          Latitude  (deg):         -18.26566077
C
C         Rectangular coordinates from LATREC:
C
C          X          (km):      -55658.44323296
C          Y          (km):     -379226.32931475
C          Z          (km):     -126505.93063865
C
C
C     2) Create a table showing a variety of rectangular coordinates
C        and the corresponding latitudinal coordinates.
C
C        Corresponding rectangular and latitudinal coordinates are
C        listed to three decimal places. Output angles are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM RECLAT_EX2
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
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      RADIUS
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
C             .                  1.D0,         0.D0,         0.D0,
C             .                  0.D0,         1.D0,         0.D0,
C             .                  0.D0,         0.D0,         1.D0,
C             .                 -1.D0,         0.D0,         0.D0,
C             .                  0.D0,        -1.D0,         0.D0,
C             .                  0.D0,         0.D0,        -1.D0,
C             .                  1.D0,         1.D0,         0.D0,
C             .                  1.D0,         0.D0,         1.D0,
C             .                  0.D0,         1.D0,         1.D0,
C             .                  1.D0,         1.D0,         1.D0  /
C
C        C
C        C     Print the banner.
C        C
C              WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) '
C             . //        '  RADIUS    LON      LAT   '
C              WRITE(*,*) ' -------  -------  ------- '
C             . //        ' -------  -------  ------- '
C
C        C
C        C     Do the conversion. Output angles in degrees.
C        C
C              DO I = 1, NREC
C
C                 CALL RECLAT( RECTAN(1,I), RADIUS, LON, LAT )
C
C                 WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ),
C             .              RADIUS, LON * DPR(), LAT * DPR()
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
C          RECT(1)  RECT(2)  RECT(3)   RADIUS    LON      LAT
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            0.000    1.000    0.000    1.000   90.000    0.000
C            0.000    0.000    1.000    1.000    0.000   90.000
C           -1.000    0.000    0.000    1.000  180.000    0.000
C            0.000   -1.000    0.000    1.000  -90.000    0.000
C            0.000    0.000   -1.000    1.000    0.000  -90.000
C            1.000    1.000    0.000    1.414   45.000    0.000
C            1.000    0.000    1.000    1.414    0.000   45.000
C            0.000    1.000    1.000    1.414   90.000   45.000
C            1.000    1.000    1.000    1.732   45.000   35.264
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
C     C.H. Acton         (JPL)
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
C
C        Changed the output argument name LONG to LON for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Added complete code examples.
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
C     rectangular to latitudinal coordinates
C
C-&


C
C     Local variables.
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION BIG

C
C     Store rectangular coordinates in temporary variables
C
      BIG = MAX ( DABS(RECTAN(1)), DABS(RECTAN(2)), DABS(RECTAN(3)) )

      IF ( BIG .GT. 0 ) THEN

         X      = RECTAN(1) / BIG
         Y      = RECTAN(2) / BIG
         Z      = RECTAN(3) / BIG

         RADIUS = BIG * DSQRT (X*X + Y*Y + Z*Z)

         LAT    = DATAN2 ( Z, DSQRT(X*X + Y*Y) )

         X      = RECTAN(1)
         Y      = RECTAN(2)

         IF (X.EQ.0.D0 .AND. Y.EQ.0.D0) THEN
            LON = 0.D0
         ELSE
            LON = DATAN2 (Y,X)
         END IF

      ELSE
         RADIUS = 0.0D0
         LAT    = 0.D0
         LON   = 0.D0
      END IF

      RETURN
      END

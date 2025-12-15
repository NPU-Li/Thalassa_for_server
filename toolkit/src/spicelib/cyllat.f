C$Procedure CYLLAT ( Cylindrical to latitudinal )

      SUBROUTINE CYLLAT ( R, CLON, Z,  RADIUS, LON, LAT )

C$ Abstract
C
C     Convert from cylindrical to latitudinal coordinates.
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

      DOUBLE PRECISION    R
      DOUBLE PRECISION    CLON
      DOUBLE PRECISION    Z
      DOUBLE PRECISION    RADIUS
      DOUBLE PRECISION    LON
      DOUBLE PRECISION    LAT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     R          I   Distance of point from Z axis.
C     CLON       I   Cylindrical angle of point from XZ plane(radians).
C     Z          I   Height of point above XY plane.
C     RADIUS     O   Distance of point from origin.
C     LON        O   Longitude of point (radians).
C     LAT        O   Latitude of point (radians).
C
C$ Detailed_Input
C
C     R        is the distance of the input point from Z axis.
C
C     CLON     is the cylindrical angle of the point from XZ plane
C              (radians).
C
C     Z        is the height of the point above XY plane.
C
C$ Detailed_Output
C
C     RADIUS   is the distance of the input point from origin.
C
C     LON      is the longitude (i.e. angle from the XZ plane) of
C              the input point (radians). LON is set equal to CLON.
C
C     LAT      is the latitude (i.e. angle above the XY plane) of the
C              input point (radians). The range of LAT is [-pi, pi].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts coordinates given in cylindrical
C     coordinates to coordinates in latitudinal coordinates.
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
C     1) Compute the cylindrical coordinates of the position of the
C        Moon as seen from the Earth, and convert them to latitudinal
C        and rectangular coordinates.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: cyllat_ex1.tm
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
C              PROGRAM CYLLAT_EX1
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
C              DOUBLE PRECISION      CLON
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      POS    ( 3 )
C              DOUBLE PRECISION      RADIUS
C              DOUBLE PRECISION      RECTAN ( 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      Z
C
C        C
C        C     Load SPK and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'cyllat_ex1.tm' )
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
C        C     Convert the position vector POS to cylindrical
C        C     coordinates.
C        C
C              CALL RECCYL ( POS, R, CLON, Z )
C
C        C
C        C     Convert the cylindrical coordinates to latitudinal.
C        C
C              CALL CYLLAT ( R, CLON, Z, RADIUS, LON, LAT )
C
C        C
C        C     Convert the latitudinal coordinates to rectangular.
C        C
C              CALL LATREC ( RADIUS, LON, LAT, RECTAN )
C
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original rectangular coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X          (km): ', POS(1)
C              WRITE(*,FMT1) '  Y          (km): ', POS(2)
C              WRITE(*,FMT1) '  Z          (km): ', POS(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Cylindrical coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Radius     (km): ', R
C              WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
C              WRITE(*,FMT1) '  Z          (km): ', Z
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
C         Cylindrical coordinates:
C
C          Radius     (km):      383289.01777726
C          Longitude (deg):         261.65040211
C          Z          (km):     -126505.93063865
C
C         Latitudinal coordinates:
C
C          Radius     (km):      403626.33912495
C          Longitude (deg):         261.65040211
C          Latitude  (deg):         -18.26566077
C
C         Rectangular coordinates from LATREC:
C
C          X          (km):      -55658.44323296
C          Y          (km):     -379226.32931475
C          Z          (km):     -126505.93063865
C
C
C     2) Create a table showing a variety of cylindrical coordinates
C        and the corresponding latitudinal coordinates.
C
C        Corresponding latitudinal and cylindrical coordinates are
C        listed to three decimal places. All input and output angles
C        are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM CYLLAT_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
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
C              DOUBLE PRECISION      CLON   ( NREC )
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      R      ( NREC )
C              DOUBLE PRECISION      RADIUS
C              DOUBLE PRECISION      RCLON
C              DOUBLE PRECISION      Z      ( NREC )
C
C              INTEGER               I
C
C        C
C        C     Define the input cylindrical coordinates. Angles
C        C     in degrees.
C        C
C
C              DATA                 R    /  0.D0, 1.D0, 1.D0,
C             .                             0.D0, 1.D0, 1.D0,
C             .                             0.D0, 1.D0, 1.D0,
C             .                             0.D0, 0.D0           /
C
C              DATA                 CLON /  0.D0,   0.D0,  90.D0,
C             .                             0.D0, 180.D0, -90.D0,
C             .                             0.D0,  45.D0, 180.D0,
C             .                           180.D0,  33.D0         /
C
C              DATA                 Z    /  0.D0,   0.D0,  0.D0,
C             .                             1.D0,   1.D0,  0.D0,
C             .                            -1.D0,   0.D0, -1.D0,
C             .                             1.D0,   0.D0         /
C
C        C
C        C     Print the banner.
C        C
C              WRITE(*,*) '    R       CLON      Z    '
C             . //        '  RADIUS     LON     LAT   '
C              WRITE(*,*) ' -------  -------  ------- '
C             . //        ' -------  -------  ------- '
C
C        C
C        C     Do the conversion. Output angles in degrees.
C        C
C              DO I = 1, NREC
C
C                 RCLON = CLON(I) * RPD()
C
C                 CALL CYLLAT( R(I), RCLON, Z(I), RADIUS, LON, LAT )
C
C                 WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I),
C             .                       RADIUS, LON * DPR(), LAT * DPR()
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
C             R       CLON      Z      RADIUS     LON     LAT
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000   90.000    0.000    1.000   90.000    0.000
C            0.000    0.000    1.000    1.000    0.000   90.000
C            1.000  180.000    1.000    1.414  180.000   45.000
C            1.000  -90.000    0.000    1.000  -90.000    0.000
C            0.000    0.000   -1.000    1.000    0.000  -90.000
C            1.000   45.000    0.000    1.000   45.000    0.000
C            1.000  180.000   -1.000    1.414  180.000  -45.000
C            0.000  180.000    1.000    1.000  180.000   90.000
C            0.000   33.000    0.000    0.000   33.000    0.000
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Changed the argument names LONGC and LONG to CLON and LON for
C        consistency with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Added complete code examples.
C
C-    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS)
C
C        Minor headers edits.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
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
C     cylindrical to latitudinal
C
C-&


C
C     Local Variables
C
      DOUBLE PRECISION BIG
      DOUBLE PRECISION X
      DOUBLE PRECISION Y

      DOUBLE PRECISION RHO
      DOUBLE PRECISION LATTUD

C
C     Convert the input cylindrical coordinates to latitudinal
C     coordinates, storing in temporary variables.
C
      BIG = MAX( DABS(R), DABS(Z) )

      IF ( BIG .GT. 0 ) THEN
         X   = R/BIG
         Y   = Z/BIG
         RHO = BIG * DSQRT( X*X + Y*Y )
      ELSE
         RHO = 0.0D0
      END IF


      IF (RHO.EQ.0.D0) THEN
         LATTUD = 0.D0
      ELSE
         LATTUD = DATAN2 (Z,R)
      END IF
C
C     Move results to output variables
C
      LON    = CLON
      RADIUS = RHO
      LAT    = LATTUD
C
      RETURN
      END

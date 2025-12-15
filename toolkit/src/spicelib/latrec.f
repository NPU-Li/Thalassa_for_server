C$Procedure LATREC ( Latitudinal to rectangular coordinates )

      SUBROUTINE LATREC ( RADIUS, LON, LAT, RECTAN )

C$ Abstract
C
C     Convert from latitudinal coordinates to rectangular coordinates.
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

      DOUBLE PRECISION   RADIUS
      DOUBLE PRECISION   LON
      DOUBLE PRECISION   LAT
      DOUBLE PRECISION   RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RADIUS     I   Distance of a point from the origin.
C     LON        I   Longitude of point in radians.
C     LAT        I   Latitude of point in radians.
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     RADIUS   is the distance of a point from the origin.
C
C     LON      is the Longitude of the input point. This is the
C              angle between the prime meridian and the meridian
C              containing the point. The direction of increasing
C              longitude is from the +X axis towards the +Y axis.
C
C              Longitude is measured in radians. On input, the
C              range of longitude is unrestricted.
C
C     LAT      is the latitude of the input point. This is the angle
C              from the XY plane of the ray from the origin through
C              the point.
C
C              Latitude is measured in radians. On input, the range
C              of latitude is unrestricted.
C
C$ Detailed_Output
C
C     RECTAN   are the rectangular coordinates of the input point.
C              RECTAN is a 3-vector.
C
C              The units associated with RECTAN are those
C              associated with the input RADIUS.
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
C     This routine returns the rectangular coordinates of a point
C     whose position is input in latitudinal coordinates.
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
C           File name: latrec_ex1.tm
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
C              PROGRAM LATREC_EX1
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
C              CALL FURNSH ( 'latrec_ex1.tm' )
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
C     2) Create a table showing a variety of latitudinal coordinates
C        and the corresponding rectangular coordinates.
C
C        Corresponding latitudinal and rectangular coordinates are
C        listed to three decimal places. Input angles are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM LATREC_EX2
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
C              DOUBLE PRECISION      LAT    ( NREC )
C              DOUBLE PRECISION      LON    ( NREC )
C              DOUBLE PRECISION      RADIUS ( NREC )
C              DOUBLE PRECISION      RECTAN ( 3    )
C              DOUBLE PRECISION      RLAT
C              DOUBLE PRECISION      RLON
C
C              INTEGER               I
C
C        C
C        C     Define the input latitudinal coordinates. Angles in
C        C     degrees.
C        C
C
C              DATA                 RADIUS / 0.D0,    1.D0,      1.D0,
C             .                              1.D0,     1.D0,     1.D0,
C             .                              1.D0, 1.4142D0, 1.4142D0,
C             .                          1.4142D0, 1.732D0            /
C
C              DATA                 LON    / 0.D0,    0.D0,  90.D0,
C             .                              0.D0,  180.D0, -90.D0,
C             .                              0.D0,   45.D0,   0.D0,
C             .                             90.D0,   45.D0            /
C
C              DATA                 LAT    / 0.D0,     0.D0,   0.D0,
C             .                             90.D0,     0.D0,   0.D0,
C             .                            -90.D0,     0.D0,  45.D0,
C             .                             45.D0, 35.264D0           /
C
C        C
C        C     Print the banner.
C        C
C              WRITE(*,*) '  RADIUS    LON      LAT   '
C             . //        ' RECT(1)  RECT(2)  RECT(3) '
C              WRITE(*,*) ' -------  -------  ------- '
C             . //        ' -------  -------  ------- '
C
C        C
C        C     Do the conversion.
C        C
C              DO I = 1, NREC
C
C                 RLON = LON(I) * RPD()
C                 RLAT = LAT(I) * RPD()
C
C                 CALL LATREC( RADIUS(I), RLON, RLAT, RECTAN )
C
C                 WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I),
C             .                       RECTAN
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
C           RADIUS    LON      LAT    RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000   90.000    0.000    0.000    1.000    0.000
C            1.000    0.000   90.000    0.000    0.000    1.000
C            1.000  180.000    0.000   -1.000    0.000    0.000
C            1.000  -90.000    0.000    0.000   -1.000    0.000
C            1.000    0.000  -90.000    0.000    0.000   -1.000
C            1.414   45.000    0.000    1.000    1.000    0.000
C            1.414    0.000   45.000    1.000    0.000    1.000
C            1.414   90.000   45.000    0.000    1.000    1.000
C            1.732   45.000   35.264    1.000    1.000    1.000
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Changed the input argument name LONG to LON for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code examples.
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     latitudinal to rectangular coordinates
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION   X
      DOUBLE PRECISION   Y
      DOUBLE PRECISION   Z

C
C     Convert to rectangular coordinates, storing the results in
C     temporary variables.
C
      X = RADIUS * DCOS(LON) * DCOS(LAT)
      Y = RADIUS * DSIN(LON) * DCOS(LAT)
      Z = RADIUS * DSIN(LAT)

C
C  Move the results to the output variables.
C
      RECTAN(1) = X
      RECTAN(2) = Y
      RECTAN(3) = Z

      RETURN
      END

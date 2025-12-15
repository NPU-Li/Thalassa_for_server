C$Procedure SPHREC ( Spherical to rectangular coordinates )

      SUBROUTINE SPHREC ( R, COLAT, SLON, RECTAN  )

C$ Abstract
C
C     Convert from spherical coordinates to rectangular coordinates.
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

      DOUBLE PRECISION   R
      DOUBLE PRECISION   COLAT
      DOUBLE PRECISION   SLON
      DOUBLE PRECISION   RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     R          I   Distance of a point from the origin.
C     COLAT      I   Angle of the point from the Z-axis in radians.
C     SLON       I   Angle of the point from the XZ plane in radians.
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     R        is the distance of the point from the origin.
C
C     COLAT    is the angle between the point and the positive
C              Z-axis in radians.
C
C     SLON     is the angle of the projection of the point to the
C              XY plane from the positive X-axis in radians. The
C              positive Y-axis is at longitude PI/2 radians.
C
C$ Detailed_Output
C
C     RECTAN   are the rectangular coordinates of a point.
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
C     whose position is input in spherical coordinates.
C
C     Spherical coordinates are defined by a distance from a central
C     reference point, an angle from a reference meridian, and an angle
C     from the Z-axis. The co-latitude of the positive Z-axis is
C     zero. The longitude of the positive Y-axis is PI/2 radians.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the spherical coordinates of the position of the Moon
C        as seen from the Earth, and convert them to rectangular
C        coordinates.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: sphrec_ex1.tm
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
C              PROGRAM SPHREC_EX1
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
C              DOUBLE PRECISION      COLAT
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      POS    ( 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      RADIUS
C              DOUBLE PRECISION      RECTAN ( 3 )
C              DOUBLE PRECISION      SLON
C              DOUBLE PRECISION      Z
C
C        C
C        C     Load SPK and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'sphrec_ex1.tm' )
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
C        C     Convert the position vector POS to spherical
C        C     coordinates.
C        C
C              CALL RECSPH ( POS, RADIUS, COLAT, SLON )
C
C        C
C        C     Convert the spherical coordinates to rectangular.
C        C
C              CALL SPHREC ( RADIUS, COLAT, SLON, RECTAN )
C
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original rectangular coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X           (km): ', POS(1)
C              WRITE(*,FMT1) '  Y           (km): ', POS(2)
C              WRITE(*,FMT1) '  Z           (km): ', POS(3)
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Spherical coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Radius      (km): ', RADIUS
C              WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
C              WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Rectangular coordinates from SPHREC:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
C              WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
C              WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
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
C          X           (km):      -55658.44323296
C          Y           (km):     -379226.32931475
C          Z           (km):     -126505.93063865
C
C         Spherical coordinates:
C
C          Radius      (km):      403626.33912495
C          Colatitude (deg):         108.26566077
C          Longitude  (deg):         -98.34959789
C
C         Rectangular coordinates from SPHREC:
C
C          X           (km):      -55658.44323296
C          Y           (km):     -379226.32931475
C          Z           (km):     -126505.93063865
C
C
C     2) Create a table showing a variety of spherical coordinates
C        and the corresponding rectangular coordinates.
C
C        Corresponding spherical and rectangular coordinates are
C        listed to three decimal places. Input angles are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPHREC_EX2
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
C              DOUBLE PRECISION      COLAT  ( NREC )
C              DOUBLE PRECISION      RADIUS ( NREC )
C              DOUBLE PRECISION      RCOLAT
C              DOUBLE PRECISION      RSLON
C              DOUBLE PRECISION      SLON   ( NREC )
C              DOUBLE PRECISION      RECTAN ( 3    )
C
C              INTEGER               I
C
C        C
C        C     Define the input spherical coordinates. Angles in
C        C     degrees.
C        C
C              DATA                 RADIUS / 0.D0, 1.D0,         1.D0,
C             .                              1.D0, 1.D0,         1.D0,
C             .                              1.D0, 1.4142D0, 1.4142D0,
C             .                          1.4142D0, 1.7320D0           /
C
C              DATA                 COLAT /  0.D0,   90.D0,  90.D0,
C             .                              0.D0,   90.D0,  90.D0,
C             .                            180.D0,   90.D0,  45.D0,
C             .                             45.D0,   54.7356D0        /
C
C              DATA                 SLON  /  0.D0,    0.D0,  90.D0,
C             .                              0.D0,  180.D0, -90.D0,
C             .                              0.D0,   45.D0,   0.D0,
C             .                              90.D0,  45.D0            /
C
C        C
C        C     Print the banner.
C        C
C              WRITE(*,*) '  RADIUS   COLAT     SLON  '
C             . //        ' RECT(1)  RECT(2)  RECT(3) '
C              WRITE(*,*) ' -------  -------  ------- '
C             . //        ' -------  -------  ------- '
C
C        C
C        C     Do the conversion.
C        C
C              DO I = 1, NREC
C
C                 RCOLAT = COLAT(I) * RPD()
C                 RSLON  = SLON(I)  * RPD()
C
C                 CALL SPHREC( RADIUS(I), RCOLAT, RSLON, RECTAN )
C
C                 WRITE (*,'(6F9.3)') RADIUS(I), COLAT(I), SLON(I),
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
C           RADIUS   COLAT     SLON   RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000   90.000    0.000    1.000    0.000    0.000
C            1.000   90.000   90.000    0.000    1.000    0.000
C            1.000    0.000    0.000    0.000    0.000    1.000
C            1.000   90.000  180.000   -1.000    0.000    0.000
C            1.000   90.000  -90.000    0.000   -1.000    0.000
C            1.000  180.000    0.000    0.000    0.000   -1.000
C            1.414   90.000   45.000    1.000    1.000    0.000
C            1.414   45.000    0.000    1.000    0.000    1.000
C            1.414   45.000   90.000    0.000    1.000    1.000
C            1.732   54.736   45.000    1.000    1.000    1.000
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
C
C        Changed the argument name LONG to SLON for consistency with
C        other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Added complete code examples.
C
C-    SPICELIB Version 1.0.4, 26-JUL-2016 (BVS)
C
C        Minor headers edits.
C
C-    SPICELIB Version 1.0.3, 24-SEP-1997 (WLT)
C
C        The BRIEF I/O section was corrected so that it
C        correctly reflects the inputs and outputs.
C
C-    SPICELIB Version 1.0.2, 12-JUL-1995 (WLT)
C
C        The header documentation was corrected so that longitude
C        now is correctly described as the angle from the
C        XZ plane instead of XY.
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
C     spherical to rectangular coordinates
C
C-&


C
C     Local Variables
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z

C
C     Convert to rectangular coordinates, storing in the results in
C     temporary variables
C
      X = R * DCOS(SLON) * DSIN(COLAT)
      Y = R * DSIN(SLON) * DSIN(COLAT)
      Z = R * DCOS(COLAT)

C
C     Move the results to the output variables
C
      RECTAN(1) = X
      RECTAN(2) = Y
      RECTAN(3) = Z

      RETURN
      END

C$Procedure DRDCYL (Derivative of rectangular w.r.t. cylindrical)

      SUBROUTINE DRDCYL ( R, CLON, Z, JACOBI )

C$ Abstract
C
C     Compute the Jacobian matrix of the transformation from
C     cylindrical to rectangular coordinates.
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

      DOUBLE PRECISION      R
      DOUBLE PRECISION      CLON
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     R          I   Distance of a point from the origin.
C     CLON       I   Angle of the point from the XZ plane in radians.
C     Z          I   Height of the point above the XY plane.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     R        is the distance of the point of interest from Z-axis.
C
C     CLON     is the cylindrical angle (in radians) of the point of
C              interest from the XZ plane. The angle increases in
C              the counterclockwise sense about the +Z axis.
C
C     Z        is the height of the point above XY plane.
C
C$ Detailed_Output
C
C     JACOBI   is the matrix of partial derivatives of the conversion
C              between cylindrical and rectangular coordinates. It
C              has the form
C
C                 .-                                  -.
C                 |  DX/DR     DX/DCLON       DX/DZ    |
C                 |                                    |
C                 |  DY/DR     DY/DCLON       DY/DZ    |
C                 |                                    |
C                 |  DZ/DR     DZ/DCLON       DZ/DZ    |
C                 `-                                  -'
C
C              evaluated at the input values of R, CLON and Z.
C              Here X, Y, and Z are given by the familiar formulae
C
C                 X = R*COS(CLON)
C                 Y = R*SIN(CLON)
C                 Z = Z
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
C     It is often convenient to describe the motion of an object in
C     the cylindrical coordinate system. However, when performing
C     vector computations its hard to beat rectangular coordinates.
C
C     To transform states given with respect to cylindrical coordinates
C     to states with respect to rectangular coordinates, one uses
C     the Jacobian of the transformation between the two systems.
C
C     Given a state in cylindrical coordinates
C
C        ( r, clon, z, dr, dclon, dz )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation:
C                    t          |                           t
C        (dx, dy, dz)   = JACOBI|          * (dr, dclon, dz)
C                               |(r,clon,z)
C
C     This routine computes the matrix
C
C              |
C        JACOBI|
C              |(r,clon,z)
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the cylindrical state of the Earth as seen from
C        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
C        Map this state back to rectangular coordinates as a check.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: drdcyl_ex1.tm
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
C              PROGRAM DRDCYL_EX1
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
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      CLON
C              DOUBLE PRECISION      DRECTN ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      JACOBI ( 3, 3 )
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      CYLVEL ( 3 )
C              DOUBLE PRECISION      RECTAN ( 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      STATE  ( 6 )
C              DOUBLE PRECISION      Z
C
C        C
C        C     Load SPK, PCK and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'drdcyl_ex1.tm' )
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
C        C     Convert position to cylindrical coordinates.
C        C
C              CALL RECCYL ( STATE, R, CLON, Z )
C
C        C
C        C     Convert velocity to cylindrical coordinates.
C        C
C
C              CALL DCYLDR ( STATE(1), STATE(2), STATE(3), JACOBI )
C
C              CALL MXV ( JACOBI, STATE(4), CYLVEL )
C
C        C
C        C     As a check, convert the cylindrical state back to
C        C     rectangular coordinates.
C        C
C              CALL CYLREC ( R, CLON, Z, RECTAN )
C
C              CALL DRDCYL ( R, CLON, Z, JACOBI )
C
C              CALL MXV ( JACOBI, CYLVEL, DRECTN )
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
C              WRITE(*,*) 'Cylindrical coordinates:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  Radius    (km)         = ', R
C              WRITE(*,FMT1) '  Longitude (deg)        = ', CLON/RPD()
C              WRITE(*,FMT1) '  Z         (km)         = ', Z
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Cylindrical velocity:'
C              WRITE(*,*) ' '
C              WRITE(*,FMT1) '  d Radius/dt    (km/s)  = ', CYLVEL(1)
C              WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
C             .                                         CYLVEL(2)/RPD()
C              WRITE(*,FMT1) '  d Z/dt         (km/s)  = ', CYLVEL(3)
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
C         Cylindrical coordinates:
C
C          Radius    (km)         =     0.33317039E+09
C          Longitude (deg)        =     0.10320290E+03
C          Z         (km)         =     0.47470484E+08
C
C         Cylindrical velocity:
C
C          d Radius/dt    (km/s)  =    -0.83496628E+01
C          d Longitude/dt (deg/s) =    -0.40539288E-02
C          d Z/dt         (km/s)  =    -0.20881149E+02
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
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Changed the input argument name LONG to CLON for consistency
C        with other routines.
C
C-    SPICELIB Version 1.0.1, 12-NOV-2013 (EDW)
C
C        Trivial edit to header, deleted trailing whitespace
C        on lines.
C
C-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     Jacobian of rectangular w.r.t. cylindrical coordinates
C
C-&


C
C     Local parameters
C
      INTEGER               DX
      PARAMETER           ( DX     = 1 )

      INTEGER               DY
      PARAMETER           ( DY     = 2 )

      INTEGER               DZ
      PARAMETER           ( DZ     = 3 )

      INTEGER               DR
      PARAMETER           ( DR     = 1 )

      INTEGER               DLON
      PARAMETER           ( DLON   = 2 )



      JACOBI (DX,DR)     =  DCOS( CLON )
      JACOBI (DY,DR)     =  DSIN( CLON )
      JACOBI (DZ,DR)     =  0.0D0

      JACOBI (DX,DLON)   = -DSIN( CLON ) * R
      JACOBI (DY,DLON)   =  DCOS( CLON ) * R
      JACOBI (DZ,DLON)   =  0.0D0

      JACOBI (DX,DZ)     =  0.0D0
      JACOBI (DY,DZ)     =  0.0D0
      JACOBI (DZ,DZ)     =  1.0D0

      RETURN
      END

C$Procedure DCYLDR (Derivative of cylindrical w.r.t. rectangular )

      SUBROUTINE DCYLDR ( X, Y, Z, JACOBI )

C$ Abstract
C
C     Compute the Jacobian matrix of the transformation from
C     rectangular to cylindrical coordinates.
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

      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   X-coordinate of point.
C     Y          I   Y-coordinate of point.
C     Z          I   Z-coordinate of point.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     X,
C     Y,
C     Z        are the rectangular coordinates of the point at
C              which the Jacobian of the map from rectangular
C              to cylindrical coordinates is desired.
C
C$ Detailed_Output
C
C     JACOBI   is the matrix of partial derivatives of the conversion
C              between rectangular and cylindrical coordinates. It
C              has the form
C
C                 .-                               -.
C                 |  dr   /dx   dr   /dy  dr   /dz  |
C                 |  dlong/dx   dlong/dy  dlong/dz  |
C                 |  dz   /dx   dz   /dy  dz   /dz  |
C                 `-                               -'
C
C              evaluated at the input values of X, Y, and Z.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input point is on the Z-axis (X = 0 and Y = 0), the
C         Jacobian is undefined, the error SPICE(POINTONZAXIS) is
C         signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     When performing vector calculations with velocities it is
C     usually most convenient to work in rectangular coordinates.
C     However, once the vector manipulations have been performed,
C     it is often desirable to convert the rectangular representations
C     into cylindrical coordinates to gain insights about phenomena
C     in this coordinate frame.
C
C     To transform rectangular velocities to derivatives of coordinates
C     in a cylindrical system, one uses the Jacobian of the
C     transformation between the two systems.
C
C     Given a state in rectangular coordinates
C
C        ( x, y, z, dx, dy, dz )
C
C     the velocity in cylindrical coordinates is given by the matrix
C     equation:
C
C                       t          |                     t
C        (dr, dlong, dz)   = JACOBI|       * (dx, dy, dz)
C                                  |(x,y,z)
C
C     This routine computes the matrix
C
C              |
C        JACOBI|
C              |(x,y,z)
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
C           File name: dcyldr_ex1.tm
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
C              PROGRAM DCYLDR_EX1
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
C              CALL FURNSH ( 'dcyldr_ex1.tm' )
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT)
C
C-&


C$ Index_Entries
C
C     Jacobian of cylindrical w.r.t. rectangular coordinates
C
C-&


C
C     SPICELIB functions
C
      LOGICAL          RETURN

C
C     Local variables
C
      DOUBLE PRECISION      RECTAN  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      ZZ
      DOUBLE PRECISION      INJACB  ( 3, 3 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DCYLDR' )
      END IF

C
C     There is a singularity of the Jacobian for points on the z-axis.
C
      IF ( ( X .EQ. 0.D0 ) .AND. ( Y .EQ. 0.D0 ) ) THEN

         CALL SETMSG ( 'The Jacobian of the transformation from '     //
     .                 'rectangular to cylindrical coordinates '      //
     .                 'is not defined for points on the z-axis.'      )
         CALL SIGERR ( 'SPICE(POINTONZAXIS)'                           )
         CALL CHKOUT ( 'DCYLDR'                                        )
         RETURN

      END IF

C
C     We will get the Jacobian of rectangular to cylindrical by
C     implicit differentiation.
C
C     First move the X,Y and Z coordinates into a vector.
C
      CALL VPACK  ( X, Y, Z, RECTAN )

C
C     Convert from rectangular to cylindrical coordinates.
C
      CALL RECCYL ( RECTAN, R, LONG, ZZ )

C
C     Get the Jacobian from cylindrical to rectangular coordinates at
C     R, LONG, Z.
C
      CALL DRDCYL ( R,  LONG, ZZ, INJACB )

C
C     Now invert INJACB to get the Jacobian from rectangular to
C     cylindrical coordinates.
C
      CALL INVORT ( INJACB, JACOBI )

      CALL CHKOUT ( 'DCYLDR' )
      RETURN
      END

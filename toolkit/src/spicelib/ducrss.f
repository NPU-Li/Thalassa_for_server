C$Procedure DUCRSS ( Unit Normalized Cross Product and Derivative )

      SUBROUTINE DUCRSS ( S1, S2, SOUT )

C$ Abstract
C
C     Compute the unit vector parallel to the cross product of
C     two 3-dimensional vectors and the derivative of this unit vector.
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
C     DERIVATIVE
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION    S1   ( 6 )
      DOUBLE PRECISION    S2   ( 6 )
      DOUBLE PRECISION    SOUT ( 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1         I   Left hand state for cross product and derivative.
C     S2         I   Right hand state for cross product and derivative.
C     SOUT       O   Unit vector and derivative of the cross product.
C
C$ Detailed_Input
C
C     S1       is any state vector. Typically, this might represent the
C              apparent state of a planet or the Sun, which defines the
C              orientation of axes of some coordinate system.
C
C     S2       is any state vector.
C
C$ Detailed_Output
C
C     SOUT     is the unit vector parallel to the cross product of the
C              position components of S1 and S2 and the derivative of
C              the unit vector.
C
C              If the cross product of the position components is
C              the zero vector, then the position component of the
C              output will be the zero vector. The velocity component
C              of the output will simply be the derivative of the
C              cross product of the position components of S1 and S2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the position components of S1 and S2 cross together to
C         give a zero vector, the position component of the output
C         will be the zero vector. The velocity component of the
C         output will simply be the derivative of the cross product
C         of the position vectors.
C
C     2)  If S1 and S2 are large in magnitude (taken together,
C         their magnitude surpasses the limit allowed by the
C         computer) then it may be possible to generate a
C         floating point overflow from an intermediate
C         computation even though the actual cross product and
C         derivative may be well within the range of double
C         precision numbers.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DUCRSS calculates the unit vector parallel to the cross product
C     of two vectors and the derivative of that unit vector.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) One can construct non-inertial coordinate frames from apparent
C        positions of objects or defined directions. However, if one
C        wants to convert states in this non-inertial frame to states
C        in an inertial reference frame, the derivatives of the axes of
C        the non-inertial frame are required.
C
C        Define a reference frame with the apparent direction of the
C        Sun as seen from Earth as the primary axis X. Use the Earth
C        pole vector to define with the primary axis the XY plane of
C        the frame, with the primary axis Y pointing in the direction
C        of the pole.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: ducrss_ex1.tm
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
C              PROGRAM DUCRSS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      TRANS  ( 6, 6 )
C              DOUBLE PRECISION      X_NEW  ( 6    )
C              DOUBLE PRECISION      Y_NEW  ( 6    )
C              DOUBLE PRECISION      Z      ( 6    )
C              DOUBLE PRECISION      Z_NEW  ( 6    )
C              DOUBLE PRECISION      ZINERT ( 6    )
C
C              INTEGER               I
C
C
C        C
C        C     Define the earth body-fixed pole vector (Z). The pole
C        C     has no velocity in the Earth fixed frame IAU_EARTH.
C        C
C              DATA                  Z  / 0.D0, 0.D0, 1.D0,
C             .                           0.D0, 0.D0, 0.D0  /
C
C        C
C        C     Load SPK, PCK, and LSK kernels, use a meta kernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'ducrss_ex1.tm' )
C
C        C
C        C     Calculate the state transformation between IAU_EARTH and
C        C     J2000 at an arbitrary epoch.
C        C
C              CALL STR2ET ( 'Jan 1, 2009', ET )
C              CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, TRANS )
C
C        C
C        C     Transform the earth pole vector from the IAU_EARTH frame
C        C     to J2000.
C        C
C              CALL MXVG ( TRANS, Z, 6, 6, ZINERT )
C
C        C
C        C     Calculate the apparent state of the Sun from Earth at
C        C     the epoch ET in the J2000 frame.
C        C
C              CALL SPKEZR ( 'Sun',   ET,   'J2000', 'LT+S',
C             .              'Earth', STATE, LT              )
C
C        C
C        C     Define the z axis of the new frame as the cross product
C        C     between the apparent direction of the Sun and the Earth
C        C     pole. Z_NEW cross X_NEW defines the Y axis of the
C        C     derived frame.
C        C
C              CALL DVHAT  ( STATE, X_NEW         )
C              CALL DUCRSS ( STATE, ZINERT, Z_NEW )
C              CALL DUCRSS ( Z_NEW, STATE,  Y_NEW )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A)') 'New X-axis:'
C              WRITE(*,'(A,3F16.12)') '   position:', (X_NEW(I), I=1,3)
C              WRITE(*,'(A,3F16.12)') '   velocity:', (X_NEW(I), I=4,6)
C              WRITE(*,'(A)') 'New Y-axis:'
C              WRITE(*,'(A,3F16.12)') '   position:', (Y_NEW(I), I=1,3)
C              WRITE(*,'(A,3F16.12)') '   velocity:', (Y_NEW(I), I=4,6)
C              WRITE(*,'(A)') 'New Z-axis:'
C              WRITE(*,'(A,3F16.12)') '   position:', (Z_NEW(I), I=1,3)
C              WRITE(*,'(A,3F16.12)') '   velocity:', (Z_NEW(I), I=4,6)
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        New X-axis:
C           position:  0.183446637633 -0.901919663328 -0.391009273602
C           velocity:  0.000000202450  0.000000034660  0.000000015033
C        New Y-axis:
C           position:  0.078846540163 -0.382978080242  0.920386339077
C           velocity:  0.000000082384  0.000000032309  0.000000006387
C        New Z-axis:
C           position: -0.979862518033 -0.199671507623  0.000857203851
C           velocity:  0.000000044531 -0.000000218531 -0.000000000036
C
C
C        Note that these vectors define the transformation between the
C        new frame and J2000 at the given ET:
C
C               .-            -.
C               |       :      |
C               |   R   :  0   |
C           M = | ......:......|
C               |       :      |
C               | dRdt  :  R   |
C               |       :      |
C               `-            -'
C
C        with
C
C           DATA         R     / X_NEW(1:3), Y_NEW(1:3), Z_NEW(1:3)  /
C
C           DATA         dRdt  / X_NEW(4:6), Y_NEW(4:6), Z_NEW(4:6)  /
C
C$ Restrictions
C
C     1)  No checking of S1 or S2 is done to prevent floating point
C         overflow. The user is required to determine that the magnitude
C         of each component of the states is within an appropriate range
C         so as not to cause floating point overflow. In almost every
C         case there will be no problem and no checking actually needs
C         to be done.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code example.
C
C-    SPICELIB Version 1.2.0, 08-APR-2014 (NJB)
C
C        Now scales inputs to reduce chance of numeric
C        overflow.
C
C-    SPICELIB Version 1.1.1, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in DVHAT call.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     Compute a unit cross product and its derivative
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      F1
      DOUBLE PRECISION      F2
      DOUBLE PRECISION      SCLS1  ( 6 )
      DOUBLE PRECISION      SCLS2  ( 6 )
      DOUBLE PRECISION      TMPSTA ( 6 )

C
C     Scale the components of the input states so the states have the
C     same direction and angular rates, but their largest position
C     components have absolute value equal to 1. Do not modify states
C     that have all position components equal to zero.
C
      F1 = MAX( ABS(S1(1)), ABS(S1(2)), ABS(S1(3)) )
      F2 = MAX( ABS(S2(1)), ABS(S2(2)), ABS(S2(3)) )

      IF ( F1 .GT. 0.D0 ) THEN

         CALL VSCLG ( 1.D0/F1, S1, 6, SCLS1 )
      ELSE
         CALL MOVED ( S1,          6, SCLS1 )
      END IF

      IF ( F2 .GT. 0.D0 ) THEN

         CALL VSCLG ( 1.D0/F2, S2, 6, SCLS2 )
      ELSE
         CALL MOVED ( S2,          6, SCLS2 )
      END IF

C
C     Not much to this.  Just get the cross product and its derivative.
C     Using that, get the associated unit vector and its derivative.
C
      CALL DVCRSS ( SCLS1,  SCLS2, TMPSTA )
      CALL DVHAT  ( TMPSTA,        SOUT   )

      RETURN
      END

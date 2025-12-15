C$Procedure DVCRSS ( Derivative of Vector cross product )

      SUBROUTINE DVCRSS ( S1, S2, SOUT )

C$ Abstract
C
C     Compute the cross product of two 3-dimensional vectors
C     and the derivative of this cross product.
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
C     SOUT       O   State associated with cross product of positions.
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
C     SOUT     is the state associated with the cross product of the
C              position components of S1 and S2. In other words, if
C              S1 = (P1,V1) and S2 = (P2,V2) then SOUT is
C              ( P1xP2, d/dt( P1xP2 ) ).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If S1 and S2 are large in magnitude (taken together,
C         their magnitude surpasses the limit allowed by the
C         computer) then it may be possible to generate a
C         floating point overflow from an intermediate
C         computation even though the actual cross product and
C         derivative may be well within the range of double
C         precision numbers.
C
C         DVCRSS does NOT check the magnitude of S1 or S2 to
C         insure that overflow will not occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DVCRSS calculates the three-dimensional cross product of two
C     vectors and the derivative of that cross product according to
C     the definition.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the cross product of two 3-dimensional vectors
C        and the derivative of this cross product.
C
C
C        Example code begins here.
C
C
C              PROGRAM DVCRSS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      S1     ( 6, 2 )
C              DOUBLE PRECISION      S2     ( 6, 2 )
C              DOUBLE PRECISION      SOUT   ( 6    )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Set S1 and S2 vectors.
C        C
C              DATA                  S1 /
C             .                   0.D0, 1.D0, 0.D0, 1.D0, 0.D0, 0.D0,
C             .                   5.D0, 5.D0, 5.D0, 1.D0, 0.D0, 0.D0  /
C              DATA                  S2 /
C             .                 1.D0,  0.D0,  0.D0, 1.D0, 0.D0, 0.D0,
C             .                -1.D0, -1.D0, -1.D0, 2.D0, 0.D0, 0.D0  /
C
C        C
C        C     For each vector S1 and S2, compute their cross product
C        C     and its derivative.
C        C
C              DO I = 1, 2
C
C                 CALL DVCRSS ( S1(1,I), S2(1,I), SOUT)
C
C                 WRITE(*,'(A,6F7.1)') 'S1  :', ( S1(J,I), J=1,6 )
C                 WRITE(*,'(A,6F7.1)') 'S2  :', ( S2(J,I), J=1,6 )
C                 WRITE(*,'(A,6F7.1)') 'SOUT:', ( SOUT(J), J=1,6 )
C                 WRITE(*,*)
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
C        S1  :    0.0    1.0    0.0    1.0    0.0    0.0
C        S2  :    1.0    0.0    0.0    1.0    0.0    0.0
C        SOUT:    0.0    0.0   -1.0    0.0    0.0   -1.0
C
C        S1  :    5.0    5.0    5.0    1.0    0.0    0.0
C        S2  :   -1.0   -1.0   -1.0    2.0    0.0    0.0
C        SOUT:    0.0    0.0    0.0    0.0   11.0  -11.0
C
C
C     2) One can construct non-inertial coordinate frames from apparent
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
C           File name: dvcrss_ex2.tm
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
C              PROGRAM DVCRSS_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      TMPSTA ( 6    )
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
C              CALL FURNSH ( 'dvcrss_ex2.tm' )
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
C        C     Define the X axis of the new frame to aligned with
C        C     the computed state. Calculate the state's unit vector
C        C     and its derivative to get the X axis and its
C        C     derivative.
C        C
C              CALL DVHAT  ( STATE, X_NEW         )
C
C        C
C        C     Define the Z axis of the new frame as the cross product
C        C     between the computed state and the Earth pole.
C        C     Calculate the Z direction in the new reference frame,
C        C     then calculate the this direction's unit vector and its
C        C     derivative to get the Z axis and its derivative.
C        C
C              CALL DVCRSS ( STATE,  ZINERT, TMPSTA )
C              CALL DVHAT  ( TMPSTA, Z_NEW          )
C
C        C
C        C     As for Z_NEW, calculate the Y direction in the new
C        C     reference frame, then calculate this direction's unit
C        C     vector and its derivative to get the Y axis and its
C        C     derivative.
C        C
C              CALL DUCRSS ( Z_NEW,  STATE, TMPSTA )
C              CALL DVHAT  ( TMPSTA, Y_NEW         )
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
C     None.
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples.
C
C-    SPICELIB Version 1.0.1, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     Compute the derivative of a cross product
C
C-&


C
C     Local Variables
C
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      DVTMP1 ( 3 )
      DOUBLE PRECISION      DVTMP2 ( 3 )

C
C     Calculate the cross product of S1 and S2, store it in VTEMP.
C
      CALL VCRSS ( S1(1), S2(1), VTEMP  )

C
C     Calculate the two components of the derivative of S1 x S2.
C
      CALL VCRSS ( S1(4), S2(1), DVTMP1 )
      CALL VCRSS ( S1(1), S2(4), DVTMP2 )

C
C     Put all of the pieces into SOUT.
C
      CALL VEQU  ( VTEMP,          SOUT(1) )
      CALL VADD  ( DVTMP1, DVTMP2, SOUT(4) )

      RETURN
      END

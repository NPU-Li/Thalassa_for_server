C$Procedure ROTVEC ( Transform a vector via a rotation )

      SUBROUTINE ROTVEC ( V1, ANGLE, IAXIS, VOUT )

C$ Abstract
C
C     Transform a vector to a new reference frame rotated by ANGLE
C     radians about axis IAXIS. This transformation rotates V1 by
C     -ANGLE radians about the specified axis.
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
C     ROTATION
C
C$ Keywords
C
C     ROTATION
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   V1    ( 3 )
      DOUBLE PRECISION   ANGLE
      INTEGER            IAXIS
      DOUBLE PRECISION   VOUT  ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector whose coordinate system is to be rotated.
C     ANGLE      I   Angle of rotation in radians.
C     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3).
C     VOUT       O   Resulting vector expressed in the new frame.
C
C$ Detailed_Input
C
C     V1       is a vector (typically representing a vector fixed in
C              inertial space) which is to be expressed in another
C              reference frame. The vector remains fixed but the
C              reference frame changes.
C
C     ANGLE    is an angle given in radians, through which the rotation
C              is performed.
C
C     IAXIS    is the index of the axis of rotation. The X, Y, and Z
C              axes have indices 1, 2 and 3 respectively.
C
C$ Detailed_Output
C
C     VOUT     is the vector expressed in the new reference frame
C              specified by the angle of rotation and axis. If
C
C                 M = [ANGLE]
C                            IAXIS
C
C              represents the rotation matrix described by the ANGLE
C              and IAXIS, (refer to the routine ROTATE) then
C
C                 VOUT =  M * V1  = [ANGLE]      * V1
C                                          IAXIS
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the IAXIS index is not in the range 1 to 3, it will be
C         treated the same as that integer 1, 2, or 3 that is congruent
C         to it mod 3.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A rotation about the first, i.e. X-axis, is described by
C
C        .-                            -.
C        |   1        0          0      |
C        |   0   cos(theta) sin(theta)  |
C        |   0  -sin(theta) cos(theta)  |
C        `-                            -'
C
C     A rotation about the second, i.e. Y-axis, is described by
C
C        .-                            -.
C        |  cos(theta)  0  -sin(theta)  |
C        |      0       1        0      |
C        |  sin(theta)  1   cos(theta)  |
C        `-                            -'
C
C     A rotation about the third, i.e. Z-axis, is described by
C
C        .-                            -.
C        |  cos(theta) sin(theta)   0   |
C        | -sin(theta) cos(theta)   0   |
C        |       0          0       1   |
C        `-                            -'
C
C     ROTVEC decides which form is appropriate according to the value
C     of IAXIS and applies the rotation to the input vector.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Apply a rotation of -45.D0 degrees about the +Z axis to
C        a 3 dimensional vector.
C
C        Example code begins here.
C
C
C              PROGRAM ROTVEC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      PI
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      V1    ( 3 )
C              DOUBLE PRECISION      VOUT  ( 3 )
C
C              INTEGER               I
C              INTEGER               IAXIS
C
C        C
C        C     Input values.
C        C
C              DATA                  V1  / 1.414D0, 0.D0, 0.D0 /
C
C              ANGLE = PI( )/4
C              IAXIS = 3
C
C        C
C        C     Rotate V1 by ANGLE radians about IAXIS
C        C
C              CALL ROTVEC (V1, ANGLE, IAXIS, VOUT)
C
C              WRITE(*,'(A,3F10.3)') 'Input vector  :',
C             .                        ( V1(I), I=1,3 )
C              WRITE(*,'(A,3F10.3)') 'Rotated vector:',
C             .                      ( VOUT(I), I=1,3 )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Input vector  :     1.414     0.000     0.000
C        Rotated vector:     1.000    -1.000     0.000
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Created
C        complete code example from existing code fragments.
C
C        Changed "coordinate system" to "reference frame" to follow
C        NAIF conventions. Added ROTATION required reading.
C
C-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 04-OCT-1999 (NJB)
C
C        Procedure line and abstract were changed to dispel the
C        impression that the input vector is rotated by +ANGLE
C        radians about the specified axis.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
C
C-&


C$ Index_Entries
C
C     rotate a vector
C
C-&


C$ Revisions
C
C-    Beta Version 1.1.0, 04-JAN-1989 (WLT)
C
C        Upgrade the routine to work with negative axis indexes. Also
C        take care of the funky way the indices (other than the input)
C        were obtained via the MOD function. It works but isn't as
C        clear (or fast) as just reading the axes from data.
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      S
      DOUBLE PRECISION      C

      INTEGER               TMP
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3

      DOUBLE PRECISION      TEMP   (3)
      INTEGER               INDEXS (5)
      SAVE                  INDEXS

      DATA                  INDEXS  / 3, 1, 2, 3, 1 /

C
C  Get the sine and cosine of ANGLE
C
      S = DSIN(ANGLE)
      C = DCOS(ANGLE)

C
C  Get indices for axes. The first index is for the axis of rotation.
C  The next two axes follow in right hand order (XYZ).  First get the
C  non-negative value of IAXIS mod 3 .
C
      TMP = MOD ( MOD(IAXIS,3) + 3, 3 )

      I1  = INDEXS( TMP + 1 )
      I2  = INDEXS( TMP + 2 )
      I3  = INDEXS( TMP + 3 )

C
C  The coordinate along the axis of rotation does not change.
C
      TEMP(1) =  V1(I1)
      TEMP(2) =           C*V1(I2) + S*V1(I3)
      TEMP(3) =          -S*V1(I2) + C*V1(I3)

C
C  Move the buffered vector to the output
C
      VOUT(I1) = TEMP(1)
      VOUT(I2) = TEMP(2)
      VOUT(I3) = TEMP(3)

      RETURN
      END

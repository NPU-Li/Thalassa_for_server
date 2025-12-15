C$Procedure AXISAR ( Axis and angle to rotation )

      SUBROUTINE AXISAR ( AXIS, ANGLE, R )

C$ Abstract
C
C     Construct a rotation matrix that rotates vectors by a specified
C     angle about a specified axis.
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
C     MATRIX
C     ROTATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      AXIS  ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      R     ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AXIS       I   Rotation axis.
C     ANGLE      I   Rotation angle, in radians.
C     R          O   Rotation matrix corresponding to AXIS and ANGLE.
C
C$ Detailed_Input
C
C     AXIS,
C     ANGLE    are, respectively, a rotation axis and a rotation
C              angle. AXIS and ANGLE determine a coordinate
C              transformation whose effect on any vector V is to
C              rotate V by ANGLE radians about the vector AXIS.
C
C$ Detailed_Output
C
C     R        is a rotation matrix representing the coordinate
C              transformation determined by AXIS and ANGLE: for
C              each vector V, R*V is the vector resulting from
C              rotating V by ANGLE radians about AXIS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If AXIS is the zero vector, the rotation generated is the
C         identity. This is consistent with the specification of VROTV.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     AXISAR can be thought of as a partial inverse of RAXISA.  AXISAR
C     really is a `left inverse': the code fragment
C
C        CALL RAXISA ( R,    AXIS,  ANGLE )
C        CALL AXISAR ( AXIS, ANGLE, R     )
C
C     preserves R, except for round-off error, as long as R is a
C     rotation matrix.
C
C     On the other hand, the code fragment
C
C        CALL AXISAR ( AXIS, ANGLE, R     )
C        CALL RAXISA ( R,    AXIS,  ANGLE )
C
C     preserves AXIS and ANGLE, except for round-off error, only if
C     ANGLE is in the range (0, pi). So AXISAR is a right inverse
C     of RAXISA only over a limited domain.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute a matrix that rotates vectors by pi/2 radians about
C        the Z-axis, and compute the rotation axis and angle based on
C        that matrix.
C
C
C        Example code begins here.
C
C
C              PROGRAM AXISAR_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      HALFPI
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      ANGOUT
C              DOUBLE PRECISION      AXIS   ( 3    )
C              DOUBLE PRECISION      AXOUT  ( 3    )
C              DOUBLE PRECISION      ROTMAT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define an axis and an angle for rotation.
C        C
C              AXIS(1) = 0.D0
C              AXIS(2) = 0.D0
C              AXIS(3) = 1.D0
C              ANGLE   = HALFPI()
C
C        C
C        C     Determine the rotation matrix.
C        C
C              CALL AXISAR ( AXIS, ANGLE, ROTMAT )
C
C        C
C        C     Now calculate the rotation axis and angle based on
C        C     ROTMAT as input.
C        C
C              CALL RAXISA ( ROTMAT, AXOUT, ANGOUT )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A)') 'Rotation matrix:'
C              WRITE(*,*)
C              DO I = 1, 3
C                 WRITE(*,'(3F10.5)') ( ROTMAT(I,J), J=1,3 )
C              END DO
C              WRITE(*,*)
C              WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXOUT
C              WRITE(*,'(A,F10.5)')  'Rotation angle (deg):',
C             .                                      ANGOUT * DPR()
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Rotation matrix:
C
C           0.00000  -1.00000   0.00000
C           1.00000   0.00000   0.00000
C           0.00000   0.00000   1.00000
C
C        Rotation axis       :   0.00000   0.00000   1.00000
C        Rotation angle (deg):  90.00000
C
C
C     2) Linear interpolation between two rotation matrices.
C
C        Let R(t) be a time-varying rotation matrix; R could be
C        a C-matrix describing the orientation of a spacecraft
C        structure. Given two points in time t1 and t2 at which
C        R(t) is known, and given a third time t3, where
C
C           t1  <  t3  <  t2,
C
C        we can estimate R(t3) by linear interpolation. In other
C        words, we approximate the motion of R by pretending that
C        R rotates about a fixed axis at a uniform angular rate
C        during the time interval [t1, t2]. More specifically, we
C        assume that each column vector of R rotates in this
C        fashion. This procedure will not work if R rotates through
C        an angle of pi radians or more during the time interval
C        [t1, t2]; an aliasing effect would occur in that case.
C
C
C        Example code begins here.
C
C
C              PROGRAM AXISAR_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      HALFPI
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      AXIS   ( 3    )
C              DOUBLE PRECISION      DELTA  ( 3, 3 )
C              DOUBLE PRECISION      FRAC
C              DOUBLE PRECISION      Q      ( 3, 3 )
C              DOUBLE PRECISION      R1     ( 3, 3 )
C              DOUBLE PRECISION      R2     ( 3, 3 )
C              DOUBLE PRECISION      R3     ( 3, 3 )
C              DOUBLE PRECISION      T1
C              DOUBLE PRECISION      T2
C              DOUBLE PRECISION      T3
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Lets assume that R(t) is the matrix that rotates
C        C     vectors by pi/2 radians about the Z-axis every
C        C     minute.
C        C
C        C     Let
C        C
C        C        R1 = R(t1), for t1 =  0", and
C        C        R2 = R(t2), for t1 = 60".
C        C
C        C     Define both matrices and times.
C        C
C              AXIS(1) = 0.D0
C              AXIS(2) = 0.D0
C              AXIS(3) = 1.D0
C
C              T1 =  0.D0
C              T2 = 60.D0
C              T3 = 30.D0
C
C              CALL IDENT  ( R1 )
C              CALL AXISAR ( AXIS, HALFPI(), R2 )
C
C        C
C        C     Lets compute
C        C
C        C                    -1
C        C        Q  = R2 * R1  ,
C        C
C        C     The rotation axis and angle of Q define the rotation
C        C     that each column of R(t) undergoes from time `t1' to
C        C     time `t2'.  Since R(t) is orthogonal, we can find Q
C        C     using the transpose of R1.  We find the rotation axis
C        C     and angle via RAXISA.
C
C              CALL MXMT   ( R2,   R1,    Q      )
C              CALL RAXISA ( Q,    AXIS,  ANGLE  )
C
C        C
C        C     Find the fraction of the total rotation angle that R
C        C     rotates through in the time interval [t1, t3].
C        C
C              FRAC = ( T3 - T1 )  /  ( T2 - T1 )
C
C        C
C        C     Finally, find the rotation DELTA that R(t) undergoes
C        C     during the time interval [t1, t3], and apply that
C        C     rotation to R1, yielding R(t3), which we'll call R3.
C        C
C              CALL AXISAR ( AXIS,   FRAC * ANGLE,  DELTA  )
C              CALL MXM    ( DELTA,  R1,            R3     )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A,F10.5)')  'Time (s)            :', T3
C              WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXIS
C              WRITE(*,'(A,F10.5)')  'Rotation angle (deg):',
C             .                               FRAC * ANGLE * DPR()
C              WRITE(*,'(A)')        'Rotation matrix     :'
C              WRITE(*,*)
C              DO I = 1, 3
C                 WRITE(*,'(3F10.5)') ( R3(I,J), J=1,3 )
C              END DO
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Time (s)            :  30.00000
C        Rotation axis       :   0.00000   0.00000   1.00000
C        Rotation angle (deg):  45.00000
C        Rotation matrix     :
C
C           0.70711  -0.70711   0.00000
C           0.70711   0.70711   0.00000
C           0.00000   0.00000   1.00000
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code examples based on existing code fragments.
C
C-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VROTV call.
C
C        Identity matrix is now obtained from IDENT.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     axis and angle to rotation
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      VTEMP ( 3 )
      INTEGER               I

C
C     First, set R equal to the identity.
C
      CALL IDENT ( R )

C
C     The matrix we want rotates EVERY vector by ANGLE about AXIS.
C     In particular, it does so to our basis vectors.  The columns
C     of R are the images of the basis vectors under this rotation.
C
      DO I = 1, 3
         CALL VROTV ( R(1,I), AXIS, ANGLE, VTEMP  )
         CALL VEQU  ( VTEMP,               R(1,I) )
      END DO

      RETURN
      END

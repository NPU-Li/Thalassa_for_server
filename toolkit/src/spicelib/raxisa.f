C$Procedure RAXISA ( Rotation axis of a matrix )

      SUBROUTINE RAXISA ( MATRIX, AXIS, ANGLE )

C$ Abstract
C
C     Compute the axis of the rotation given by an input matrix
C     and the angle of the rotation about that axis.
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
C     ANGLE
C     MATRIX
C     ROTATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION    MATRIX ( 3, 3 )
      DOUBLE PRECISION    AXIS   (    3 )
      DOUBLE PRECISION    ANGLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     I   3x3 rotation matrix in double precision.
C     AXIS       O   Axis of the rotation.
C     ANGLE      O   Angle through which the rotation is performed.
C
C$ Detailed_Input
C
C     MATRIX   is a 3x3 rotation matrix in double precision.
C
C$ Detailed_Output
C
C     AXIS     is a unit vector pointing along the axis of the
C              rotation. In other words, AXIS is a unit eigenvector
C              of the input matrix, corresponding to the eigenvalue
C              1. If the input matrix is the identity matrix, AXIS
C              will be the vector (0, 0, 1). If the input rotation is
C              a rotation by PI radians, both AXIS and -AXIS may be
C              regarded as the axis of the rotation.
C
C     ANGLE    is the angle between V and MATRIX*V for any non-zero
C              vector V orthogonal to AXIS. Angle is given in
C              radians. The angle returned will be in the range from
C              0 to PI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input matrix is not a rotation matrix (where a fairly
C         loose tolerance is used to check this), an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  If the input matrix is the identity matrix, this routine
C         returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Every rotation matrix has an axis A such any vector, V, parallel
C     to that axis satisfies the equation
C
C        V = MATRIX * V
C
C     This routine returns a unit vector AXIS parallel to the axis of
C     the input rotation matrix. Moreover for any vector W orthogonal
C     to the axis of the rotation
C
C        AXIS  and  W x MATRIX*W
C
C        (where "x" denotes the cross product operation)
C
C     will be positive scalar multiples of one another (at least to
C     within the ability to make such computations with double
C     precision arithmetic, and under the assumption that the MATRIX
C     does not represent a rotation by zero or Pi radians).
C
C     The angle returned will be the angle between W and MATRIX*W for
C     any vector orthogonal to AXIS.
C
C     If the input matrix is a rotation by 0 or PI radians some choice
C     must be made for the AXIS returned. In the case of a rotation by
C     0 radians, AXIS is along the positive z-axis. In the case of a
C     rotation by 180 degrees, two choices are
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given an axis and an angle of rotation about that axis,
C        determine the rotation matrix. Using this matrix as input,
C        compute the axis and angle of rotation, and verify that
C        the later are equivalent by subtracting the original matrix
C        and the one resulting from using the computed axis and angle
C        of rotation on the AXISAR call.
C
C
C        Example code begins here.
C
C
C              PROGRAM RAXISA_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      TWOPI
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      ANGOUT
C              DOUBLE PRECISION      AXIS   ( 3    )
C              DOUBLE PRECISION      AXOUT  ( 3    )
C              DOUBLE PRECISION      R      ( 3, 3 )
C              DOUBLE PRECISION      ROUT   ( 3, 3 )
C
C              INTEGER               I
C
C        C
C        C     Define an axis and an angle for rotation.
C        C
C              DATA                  AXIS  /  1.D0, 2.D0, 3.D0  /
C
C              ANGLE = 0.1D0 * TWOPI()
C
C        C
C        C     Determine the rotation matrix.
C        C
C              CALL AXISAR ( AXIS, ANGLE, R )
C
C        C
C        C     Now calculate the rotation axis and angle based on the
C        C     matrix as input.
C        C
C              CALL RAXISA ( R, AXOUT, ANGOUT )
C
C              WRITE(*,'(A,3F12.8)') 'Axis :', AXOUT
C              WRITE(*,'(A,F12.8)')  'Angle:', ANGOUT
C              WRITE(*,*) ' '
C
C        C
C        C     Now input the AXOUT and ANGOUT to AXISAR to
C        C     compare against the original rotation matrix R.
C        C
C              WRITE(*,'(A)') 'Difference between input and output '
C             .           //  'matrices:'
C
C              CALL AXISAR ( AXOUT, ANGOUT, ROUT )
C
C              DO I = 1, 3
C
C                 WRITE(*,'(3F20.16)') ROUT(I,1) - R(I,1),
C             .                        ROUT(I,2) - R(I,2),
C             .                        ROUT(I,3) - R(I,3)
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
C        Axis :  0.26726124  0.53452248  0.80178373
C        Angle:  0.62831853
C
C        Difference between input and output matrices:
C         -0.0000000000000001  0.0000000000000000  0.0000000000000000
C          0.0000000000000001 -0.0000000000000001  0.0000000000000000
C          0.0000000000000000  0.0000000000000001  0.0000000000000000
C
C
C        Note, the zero matrix is accurate to round-off error. A
C        numerical demonstration of equality.
C
C
C     2) This routine can be used to numerically approximate the
C        instantaneous angular velocity vector of a rotating object.
C
C        Suppose that R(t) is the rotation matrix whose columns
C        represent the inertial pointing vectors of the body-fixed axes
C        of an object at time t.
C
C        Then the angular velocity vector points along the vector given
C        by:
C
C                                T
C            limit  AXIS( R(t+h)R )
C            h-->0
C
C        And the magnitude of the angular velocity at time t is given
C        by:
C
C                               T
C           d ANGLE ( R(t+h)R(t) )
C           ----------------------   at   h = 0
C                     dh
C
C        This code example computes the instantaneous angular velocity
C        vector of the Earth at 2000 Jan 01 12:00:00 TDB.
C
C        Use the PCK kernel below to load the required triaxial
C        ellipsoidal shape model and orientation data for the Earth.
C
C           pck00010.tpc
C
C
C        Example code begins here.
C
C
C              PROGRAM RAXISA_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      ANGVEL ( 3    )
C              DOUBLE PRECISION      AXIS   ( 3    )
C              DOUBLE PRECISION      INFROT ( 3, 3 )
C              DOUBLE PRECISION      H
C              DOUBLE PRECISION      RT     ( 3, 3 )
C              DOUBLE PRECISION      RTH    ( 3, 3 )
C              DOUBLE PRECISION      T
C
C        C
C        C     Load a PCK file containing a triaxial
C        C     ellipsoidal shape model and orientation
C        C     data for the Earth.
C        C
C              CALL FURNSH ( 'pck00010.tpc' )
C
C        C
C        C     Load time into the double precision variable T
C        C     and the delta time (1 ms) into the double precision
C        C     variable H
C        C
C              T = 0.D0
C              H = 1D-3
C
C        C
C        C     Get the rotation matrices from IAU_EARTH to J2000
C        C     at T and T+H.
C        C
C              CALL PXFORM ( 'IAU_EARTH', 'J2000', T,   RT  )
C              CALL PXFORM ( 'IAU_EARTH', 'J2000', T+H, RTH )
C
C        C
C        C     Compute the infinitesimal rotation R(t+h)R(t)**T
C        C
C              CALL MXMT ( RTH, RT, INFROT )
C
C        C
C        C     Compute the AXIS and ANGLE of the infinitesimal rotation
C        C
C              CALL RAXISA ( INFROT, AXIS, ANGLE )
C
C        C
C        C     Scale AXIS to get the angular velocity vector
C        C
C              CALL VSCL ( ANGLE/H, AXIS, ANGVEL )
C
C        C
C        C     Output the results.
C        C
C              WRITE(*,*) 'Instantaneous angular velocity vector:'
C              WRITE(*,'(3F15.10)') ANGVEL
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Instantaneous angular velocity vector:
C           0.0000000000   0.0000000000   0.0000729212
C
C
C$ Restrictions
C
C     1)  If the input matrix is not a rotation matrix but is close
C         enough to pass the tests this routine performs on it, no error
C         will be signaled, but the results may have poor accuracy.
C
C     2)  The input matrix is taken to be an object that acts on
C         (rotates) vectors---it is not regarded as a coordinate
C         transformation. To find the axis and angle of a coordinate
C         transformation, input the transpose of that matrix to this
C         routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 05-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code examples.
C
C-    SPICELIB Version 2.1.2, 02-JAN-2008 (EDW)
C
C        Minor edit to the ANGLE declaration strictly
C        identifying the constant as a double.
C
C        From:
C
C           ANGLE = 2.0 * DATAN2( VNORM(Q(1)), Q(0) )
C
C        To:
C
C           ANGLE = 2.D0 * DATAN2( VNORM(Q(1)), Q(0) )
C
C-    SPICELIB Version 2.1.1, 05-JAN-2005 (NJB)
C
C        Minor edits and formatting changes were made.
C
C-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
C
C        This routine now participates in error handling properly.
C
C-    SPICELIB Version 2.0.0, 19-SEP-1999 (WLT)
C
C        The routine was re-written so as to avoid the numerical
C        instabilities present in the previous implementation for
C        rotations very near zero or 180 degrees.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
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
C     axis and angle of a rotation matrix
C
C-&


C$ Revisions
C
C-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
C
C        Calls to CHKIN and CHKOUT in the standard SPICE error
C        handling style were added. Versions prior to 2.0.0
C        were error free, however the call to M2Q introduced in
C        version 2.0.0 signals an error if the input matrix is
C        not sufficiently close to a rotation.
C
C        Additionally, FAILED is now checked after the call to
C        M2Q. This prevents garbage from being placed into the
C        output arguments.
C
C-    Beta Version 1.1.0, 03-JAN-1989 (WLT)
C
C        Even though the routine stipulates that the input matrix
C        should be a rotation matrix, it might not be. As a result
C        we could have negative numbers showing up where we need
C        to take square roots. This fix simply bounds these values
C        so that Fortran intrinsics always get reasonable input values.
C
C        Add and example to the header.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

      EXTERNAL              PI
      DOUBLE PRECISION      PI

C
C     Local Variables
C
      DOUBLE PRECISION      Q ( 0: 3 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RAXISA' )
      END IF

C
C     Construct the quaternion corresponding to the input rotation
C     matrix
C
      CALL M2Q ( MATRIX, Q )

C
C     Check FAILED and return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'RAXISA' )
         RETURN
      END IF

C
C     The quaternion we've just constructed is of the form:
C
C         cos(ANGLE/2) + sin(ANGLE/2) * AXIS
C
C     We take a few precautions to handle the case of an identity
C     rotation.
C
      IF ( VZERO(Q(1)) ) THEN

         ANGLE = 0
         AXIS(1) = 0.0D0
         AXIS(2) = 0.0D0
         AXIS(3) = 1.0D0

      ELSE IF ( Q(0) .EQ. 0.0D0 ) THEN

         ANGLE   = PI()
         AXIS(1) = Q(1)
         AXIS(2) = Q(2)
         AXIS(3) = Q(3)

      ELSE

         CALL VHAT ( Q(1), AXIS )
         ANGLE = 2.D0 * DATAN2 ( VNORM(Q(1)), Q(0) )

      END IF

      CALL CHKOUT ( 'RAXISA' )
      RETURN

      END

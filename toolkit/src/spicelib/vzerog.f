C$Procedure VZEROG ( Is a vector the zero vector? -- general dim. )

      LOGICAL FUNCTION VZEROG ( V, NDIM )

C$ Abstract
C
C     Indicate whether an n-dimensional vector is the zero vector.
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
C     MATH
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      V    ( * )
      INTEGER               NDIM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V          I   Vector to be tested.
C     NDIM       I   Dimension of V.
C
C     The function returns the value .TRUE. if and only if V is the
C     zero vector.
C
C$ Detailed_Input
C
C     V,
C     NDIM     are, respectively, an n-dimensional vector and its
C              dimension.
C
C$ Detailed_Output
C
C     The function returns the value .TRUE. if and only if V is the
C     zero vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  When NDIM is non-positive, this function returns the value
C         .FALSE. (A vector of non-positive dimension cannot be the
C         zero vector.)
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function has the same truth value as the logical expression
C
C        VNORMG ( V, NDIM )  .EQ.  0.D0
C
C     Replacing the above expression by
C
C        VZEROG ( V, NDIM )
C
C     has several advantages: the latter expresses the test more
C     clearly, looks better, and doesn't go through the work of scaling,
C     squaring, taking a square root, and re-scaling (all of which
C     VNORMG must do) just to find out that a vector is non-zero.
C
C     A related function is VZERO, which accepts three-dimensional
C     vectors.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a set of n-dimensional vectors, check which ones are
C        the zero vector.
C
C
C        Example code begins here.
C
C
C              PROGRAM VZEROG_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              LOGICAL               VZEROG
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 2 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      V    ( NDIM, SETSIZ )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the vector set.
C        C
C              DATA                  V   /
C             .                          0.D0,  0.D0,  0.D0,  2.D-7,
C             .                          0.D0,  0.D0,  0.D0,  0.D0  /
C
C        C
C        C     Check each n-dimensional vector within the set.
C        C
C              DO I=1, SETSIZ
C
C        C
C        C        Check if the I'th vector is the zero vector.
C        C
C                 WRITE(*,*)
C                 WRITE(*,'(A,4F11.7)') 'Input vector: ',
C             .                         ( V(J,I), J=1,NDIM )
C
C                 IF ( VZEROG ( V(1,I), NDIM ) ) THEN
C
C                    WRITE(*,'(A)') '   The zero vector.'
C
C                 ELSE
C
C                    WRITE(*,'(A)') '   Not all elements of the '
C             .                  // 'vector are zero.'
C
C                 END IF
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
C        Input vector:   0.0000000  0.0000000  0.0000000  0.0000002
C           Not all elements of the vector are zero.
C
C        Input vector:   0.0000000  0.0000000  0.0000000  0.0000000
C           The zero vector.
C
C
C     2) Define a unit quaternion and confirm that it is non-zero
C        before converting it to a rotation matrix.
C
C
C        Example code begins here.
C
C
C              PROGRAM VZEROG_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      VNORMG
C              LOGICAL               VZEROG
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      Q    ( 0 : 3 )
C              DOUBLE PRECISION      M    ( 3,  3 )
C              DOUBLE PRECISION      S
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define a unit quaternion.
C        C
C              S = SQRT( 2.D0 ) / 2.D0
C
C              Q(0) = S
C              Q(1) = 0.D0
C              Q(2) = 0.D0
C              Q(3) = -S
C
C              WRITE(*,'(A,4F12.7)') 'Quaternion :', Q
C
C        C
C        C     Confirm that it is non-zero and
C        C
C              IF ( VZEROG ( Q, 4 ) ) THEN
C
C                 WRITE(*,*) '   Quaternion is the zero vector.'
C
C              ELSE
C
C        C
C        C        Confirm q satisfies ||Q|| = 1.
C        C
C                 WRITE(*,'(A,F12.7)') 'Norm       :', VNORMG ( Q, 4 )
C
C        C
C        C        Convert the quaternion to a matrix form.
C        C
C                 CALL Q2M ( Q, M )
C
C                 WRITE(*,'(A)') 'Matrix form:'
C                 DO I = 1, 3
C
C                    WRITE(*,'(3F12.7)') ( M(I,J), J=1,3 )
C
C                 END DO
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Quaternion :   0.7071068   0.0000000   0.0000000  -0.7071068
C        Norm       :   1.0000000
C        Matrix form:
C           0.0000000   1.0000000   0.0000000
C          -1.0000000   0.0000000  -0.0000000
C          -0.0000000   0.0000000   1.0000000
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
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 18-JUL-1990 (NJB) (IMU)
C
C-&


C$ Index_Entries
C
C     test whether an n-dimensional vector is the zero vector
C
C-&


C
C     Local variables
C
      INTEGER               I

C
C     Leave as soon as we find a non-zero component.  If we get through
C     the loop, we have a zero vector, as long as the vector's dimension
C     is valid.
C
      DO I = 1, NDIM

         IF ( V(I) .NE. 0.D0 ) THEN
            VZEROG = .FALSE.
            RETURN
         END IF

      END DO

C
C     We have a zero vector if and only if the vector's dimension is at
C     least 1.
C
      VZEROG  =  ( NDIM .GE. 1 )

      END

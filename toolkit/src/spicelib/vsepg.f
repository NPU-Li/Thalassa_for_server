C$Procedure VSEPG ( Angular separation of vectors, general dimension )

      DOUBLE PRECISION FUNCTION VSEPG ( V1, V2, NDIM )

C$ Abstract
C
C     Find the separation angle in radians between two double precision
C     vectors of arbitrary dimension. This angle is defined as zero if
C     either vector is zero.
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
C     ANGLE
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   V1 ( * )
      DOUBLE PRECISION   V2 ( * )
      INTEGER            NDIM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   First vector.
C     V2         I   Second vector.
C     NDIM       I   The number of elements in V1 and V2.
C
C     The function returns the angle between V1 and V2 expressed in
C     radians.
C
C$ Detailed_Input
C
C     V1,
C     V2       are two double precision vectors of arbitrary dimension.
C              Either V1 or V2, or both, may be the zero vector.
C
C              An implicit assumption exists that V1 and V2 are
C              specified in the same reference space. If this is not
C              the case, the numerical result of this routine has no
C              meaning.
C
C     NDIM     is the dimension of both V1 and V2.
C
C$ Detailed_Output
C
C     The function returns the angle between V1 and V2 expressed in
C     radians.
C
C     VSEPG is strictly non-negative. For input vectors of four or more
C     dimensions, the angle is defined as the generalization of the
C     definition for three dimensions. If either V1 or V2 is the zero
C     vector, then VSEPG is defined to be 0 radians.
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
C     In four or more dimensions this angle does not have a physically
C     realizable interpretation. However, the angle is defined as
C     the generalization of the following definition which is valid in
C     three or two dimensions:
C
C        In the plane, it is a simple matter to calculate the angle
C        between two vectors once the two vectors have been made to be
C        unit length. Then, since the two vectors form the two equal
C        sides of an isosceles triangle, the length of the third side
C        is given by the expression
C
C           LENGTH = 2.0 * SIN ( VSEPG/2.0 )
C
C        The length is given by the magnitude of the difference of the
C        two unit vectors
C
C           LENGTH = NORM ( U1 - U2 )
C
C        Once the length is found, the value of VSEPG may be calculated
C        by inverting the first expression given above as
C
C           VSEPG = 2.0 * ARCSIN ( LENGTH/2.0 )
C
C        This expression becomes increasingly unstable when VSEPG gets
C        larger than PI/2 radians or 90 degrees. In this situation
C        (which is easily detected by determining the sign of the dot
C        product of V1 and V2) the supplementary angle is calculated
C        first and then VSEPG is given by
C
C           VSEPG = PI - SUPPLEMENTARY_ANGLE
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define two sets of n-dimensional vectors and compute the
C        angular separation between each vector in first set and the
C        corresponding vector in the second set.
C
C
C        Example code begins here.
C
C
C              PROGRAM VSEPG_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      VSEPG
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 3 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
C              DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  V1 /
C             .                      1.D0,  0.D0,  0.D0,  0.D0,
C             .                      1.D0,  0.D0,  0.D0,  0.D0,
C             .                      3.D0,  0.D0,  0.D0,  0.D0   /
C
C              DATA                  V2 /
C             .                      1.D0,  0.D0,  0.D0,  0.D0,
C             .                      0.D0,  1.D0,  0.D0,  0.D0,
C             .                     -5.D0,  0.D0,  0.D0,  0.D0  /
C
C        C
C        C     Calculate the angular separation between each pair
C        C     of vectors.
C        C
C              DO I=1, SETSIZ
C
C                 WRITE(*,'(A,4F6.1)')  'First vector            : ',
C             .                        ( V1(J,I), J=1,NDIM )
C                 WRITE(*,'(A,4F6.1)')  'Second vector           : ',
C             .                        ( V2(J,I), J=1,NDIM )
C                 WRITE(*,'(A,F15.10)') 'Angular separation (rad): ',
C             .                VSEPG ( V1(1,I), V2(1,I), NDIM )
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
C        First vector            :    1.0   0.0   0.0   0.0
C        Second vector           :    1.0   0.0   0.0   0.0
C        Angular separation (rad):    0.0000000000
C
C        First vector            :    1.0   0.0   0.0   0.0
C        Second vector           :    0.0   1.0   0.0   0.0
C        Angular separation (rad):    1.5707963268
C
C        First vector            :    3.0   0.0   0.0   0.0
C        Second vector           :   -5.0   0.0   0.0   0.0
C        Angular separation (rad):    3.1415926536
C
C
C$ Restrictions
C
C     1)  The user is required to insure that the input vectors will not
C         cause floating point overflow upon calculation of the vector
C         dot product since no error detection or correction code is
C         implemented. In practice, this is not a significant
C         restriction.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.A. Curzon        (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     H.A. Neilan        (JPL)
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
C        Added complete code example based on existing example.
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (HAN)
C
C-&


C$ Index_Entries
C
C     angular separation of n-dimensional vectors
C
C-&


C
C     SPICELIB functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORMG
      DOUBLE PRECISION      VDOTG

C
C     Local Variables
C
C     The following declarations represent, respectively:
C        Magnitudes of V1, V2
C        Reciprocals of the magnitudes of V1, V2
C        Magnitude of either of the difference vectors: V1-V2 or
C           V1-(-V2)
C
      DOUBLE PRECISION      DMAG1
      DOUBLE PRECISION      DMAG2
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      MAGDIF

      INTEGER               I

C
C     Calculate the magnitudes of V1 and V2; if either is 0, VSEPG = 0
C
      DMAG1 = VNORMG ( V1, NDIM )
      IF ( DMAG1 .EQ. 0.D0 ) THEN
         VSEPG = 0.D0
         RETURN
      END IF

      DMAG2 = VNORMG ( V2, NDIM )
      IF ( DMAG2 .EQ. 0.D0 ) THEN
         VSEPG = 0.D0
         RETURN
      END IF


      IF      ( VDOTG (V1, V2, NDIM) .GT. 0 ) THEN

         R1 = 1.0D0/DMAG1
         R2 = 1.0D0/DMAG2

         MAGDIF = 0.0D0
         DO I = 1, NDIM
            MAGDIF = MAGDIF + ( V1(I)*R1 - V2(I)*R2 )**2
         END DO
         MAGDIF = DSQRT(MAGDIF)

         VSEPG = 2.0D0 * ASIN (0.5D0 * MAGDIF)

      ELSE IF ( VDOTG (V1, V2, NDIM) .LT. 0 ) THEN

         R1 = 1.0D0/DMAG1
         R2 = 1.0D0/DMAG2

         MAGDIF = 0.0D0
         DO I = 1, NDIM
            MAGDIF = MAGDIF + ( V1(I)*R1 + V2(I)*R2 )**2
         END DO
         MAGDIF = DSQRT(MAGDIF)

         VSEPG = PI() - 2.0D0 * ASIN (0.5D0 * MAGDIF)

      ELSE
         VSEPG = PI() / 2.0D0
      END IF

      RETURN
      END

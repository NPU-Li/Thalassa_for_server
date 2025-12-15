C$Procedure VLCOMG ( Vector linear combination, general dimension )

      SUBROUTINE VLCOMG ( N, A, V1, B, V2, SUM )

C$ Abstract
C
C     Compute a vector linear combination of two double precision
C     vectors of arbitrary dimension.
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
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      INTEGER            N
      DOUBLE PRECISION   A
      DOUBLE PRECISION   V1  ( N )
      DOUBLE PRECISION   B
      DOUBLE PRECISION   V2  ( N )
      DOUBLE PRECISION   SUM ( N )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I   Dimension of vector space.
C     A          I   Coefficient of V1.
C     V1         I   Vector in N-space.
C     B          I   Coefficient of V2.
C     V2         I   Vector in N-space.
C     SUM        O   Linear vector combination A*V1 + B*V2.
C
C$ Detailed_Input
C
C     N        is the dimension of V1, V2 and SUM.
C
C     A        is the double precision scalar variable that multiplies
C              V1.
C
C     V1       is an arbitrary, double precision n-dimensional vector.
C
C     B        is the double precision scalar variable that multiplies
C              V2.
C
C     V2       is an arbitrary, double precision n-dimensional vector.
C
C$ Detailed_Output
C
C     SUM      is the double precision n-dimensional vector which
C              contains the linear combination
C
C                 A * V1 + B * V2
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
C     The code reflects precisely the following mathematical expression
C
C        For each value of the index I, from 1 to N:
C
C           SUM(I) = A * V1(I) + B * V2(I)
C
C     No error checking is performed to guard against numeric overflow.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Perform the projection of a 4-dimensional vector into a
C        2-dimensional plane in 4-space.
C
C
C        Example code begins here.
C
C
C              PROGRAM VLCOMG_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      VDOTG
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM = 4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      PUV    ( NDIM )
C              DOUBLE PRECISION      X      ( NDIM )
C              DOUBLE PRECISION      U      ( NDIM )
C              DOUBLE PRECISION      V      ( NDIM )
C
C        C
C        C     Let X be an arbitrary NDIM-vector
C        C
C              DATA                  X  /  4.D0, 35.D0, -5.D0, 7.D0  /
C
C        C
C        C     Let U and V be orthonormal NDIM-vectors spanning the
C        C     plane of interest.
C        C
C              DATA                  U  /  0.D0,  0.D0,  1.D0, 0.D0 /
C
C              V(1) =  SQRT(3.D0)/3.D0
C              V(2) = -SQRT(3.D0)/3.D0
C              V(3) =  0.D0
C              V(4) =  SQRT(3.D0)/3.D0
C
C        C
C        C     Compute the projection of X onto this 2-dimensional
C        C     plane in NDIM-space.
C        C
C              CALL VLCOMG ( NDIM, VDOTG ( X, U, NDIM), U,
C             .                    VDOTG ( X, V, NDIM), V, PUV )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A,4F6.1)') 'Input vector             : ', X
C              WRITE(*,'(A,4F6.1)') 'Projection into 2-d plane: ', PUV
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Input vector             :    4.0  35.0  -5.0   7.0
C        Projection into 2-d plane:   -8.0   8.0  -5.0  -8.0
C
C
C$ Restrictions
C
C     1)  No error checking is performed to guard against numeric
C         overflow or underflow. The user is responsible for insuring
C         that the input values are reasonable.
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
C-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code example based on existing example.
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
C     linear combination of two n-dimensional vectors
C
C-&


C
C     Local variables
C
      INTEGER        I

      DO I = 1,N
         SUM(I) = A*V1(I) + B*V2(I)
      END DO

      RETURN
      END

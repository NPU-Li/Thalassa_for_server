C$Procedure VSCLG ( Vector scaling, general dimension )

      SUBROUTINE VSCLG ( S, V1, NDIM, VOUT )

C$ Abstract
C
C     Multiply a scalar and a double precision vector of arbitrary
C     dimension.
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

      INTEGER            NDIM
      DOUBLE PRECISION   S
      DOUBLE PRECISION   V1   ( NDIM )
      DOUBLE PRECISION   VOUT ( NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S          I   Scalar to multiply a vector.
C     V1         I   Vector to be multiplied.
C     NDIM       I   Dimension of V1 (and also VOUT).
C     VOUT       O   Product vector, S * V1.
C
C$ Detailed_Input
C
C     S        is a double precision scalar.
C
C     V1       is a double precision n-dimensional vector.
C
C     NDIM     is the dimension of V1 (and VOUT).
C
C$ Detailed_Output
C
C     VOUT     is a double precision n-dimensional vector containing
C              the product of the scalar with the vector V1.
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
C     For each value of the index I from 1 to NDIM, this subroutine
C     performs the following multiplication
C
C        VOUT(I) = S * V1(I)
C
C     No error checking is performed to guard against numeric overflow
C     or underflow.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define a sets of scalar double precision values and use them
C        to scale a given n-dimensional vector.
C
C
C        Example code begins here.
C
C
C              PROGRAM VSCLG_EX1
C              IMPLICIT NONE
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
C              DOUBLE PRECISION      S    ( SETSIZ )
C              DOUBLE PRECISION      V1   ( NDIM   )
C              DOUBLE PRECISION      VOUT ( NDIM   )
C
C              INTEGER               I
C
C        C
C        C     Define the set of scalars and the input vector.
C        C
C              DATA                  S    / 3.D0, 0.D0, -1.D0 /
C
C              DATA                  V1   / 1.D0, 2.D0, -3.D0, 4.D0  /
C
C
C              WRITE(*,'(A,4F6.1)') 'Input vector : ', V1
C              WRITE(*,*)
C
C        C
C        C     Calculate product of each scalar and V1.
C        C
C              DO I=1, SETSIZ
C
C                 CALL VSCLG ( S(I), V1, NDIM, VOUT )
C
C                 WRITE(*,'(A,F6.1)')  'Scale factor : ', S(I)
C                 WRITE(*,'(A,4F6.1)') 'Output vector: ', VOUT
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
C        Input vector :    1.0   2.0  -3.0   4.0
C
C        Scale factor :    3.0
C        Output vector:    3.0   6.0  -9.0  12.0
C
C        Scale factor :    0.0
C        Output vector:    0.0   0.0  -0.0   0.0
C
C        Scale factor :   -1.0
C        Output vector:   -1.0  -2.0   3.0  -4.0
C
C
C$ Restrictions
C
C     1)  No error checking is performed to guard against numeric
C         overflow. The programmer is thus required to insure that the
C         values in V1 and S are reasonable and will not cause overflow.
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
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     n-dimensional vector scaling
C
C-&


C
C     Local variables
C
      INTEGER            I

      DO I=1,NDIM
         VOUT(I) = S * V1(I)
      END DO

      RETURN
      END

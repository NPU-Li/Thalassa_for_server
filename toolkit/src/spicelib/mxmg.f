C$Procedure MXMG ( Matrix times matrix, general dimension )

      SUBROUTINE MXMG ( M1, M2, NR1, NC1R2, NC2, MOUT )

C$ Abstract
C
C     Multiply two double precision matrices of arbitrary size.
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
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      INTEGER             NR1
      INTEGER             NC1R2
      INTEGER             NC2
      DOUBLE PRECISION    M1   ( NR1,   NC1R2 )
      DOUBLE PRECISION    M2   ( NC1R2, NC2   )
      DOUBLE PRECISION    MOUT ( NR1,   NC2   )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   NR1   x NC1R2 double precision matrix.
C     M2         I   NC1R2 x NC2   double precision matrix.
C     NR1        I   Row dimension of M1 (and also MOUT).
C     NC1R2      I   Column dimension of M1 and row dimension of M2.
C     NC2        I   Column dimension of M2 (and also MOUT).
C     MOUT       O   NR1 x NC2 double precision matrix.
C
C$ Detailed_Input
C
C     M1       is any double precision matrix of arbitrary size.
C
C     M2       is any double precision matrix of arbitrary size.
C              The number of rows in M2 must match the number of
C              columns in M1.
C
C     NR1      is the number of rows in both M1 and MOUT.
C
C     NC1R2    is the number of columns in M1 and (by necessity)
C              the number of rows of M2.
C
C     NC2      is the number of columns in both M2 and MOUT.
C
C$ Detailed_Output
C
C     MOUT     is a a double precision matrix of dimension
C              NR1 x NC2. MOUT is the product matrix given
C              by MOUT = (M1) x (M2). MOUT must not overwrite
C              M1 or M2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NC1R2 < 1, the elements of the matrix MOUT are set equal to
C         zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The code reflects precisely the following mathematical expression
C
C     For each value of the subscript I from 1 to NC1, and J from 1
C     to NC2:
C
C        MOUT(I,J) = Summation from K=1 to NC1R2 of ( M1(I,K) * M2(K,J)
C
C     Since this subroutine operates on matrices of arbitrary size, it
C     is not feasible to buffer intermediate results. Thus, MOUT
C     should NOT overwrite either M1 or M2.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 3x2 and a 2x3 matrices, multiply the first matrix by
C        the second one.
C
C
C        Example code begins here.
C
C
C              PROGRAM MXMG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M1   ( 3, 2 )
C              DOUBLE PRECISION      M2   ( 2, 3 )
C              DOUBLE PRECISION      MOUT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M1 and M2.
C        C
C              DATA                  M1 /  1.0D0,  2.0D0, 3.0D0,
C             .                            4.0D0,  5.0D0, 6.0D0  /
C
C              DATA                  M2 /  1.0D0,  2.0D0,
C             .                            3.0D0,  4.0D0,
C             .                            5.0D0,  6.0D0  /
C
C        C
C        C     Multiply M1 by M2.
C        C
C              CALL MXMG ( M1, M2, 3, 2, 3, MOUT )
C
C              WRITE(*,'(A)') 'M1 times M2:'
C              DO I = 1, 3
C
C                 WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
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
C        M1 times M2:
C             9.000    19.000    29.000
C            12.000    26.000    40.000
C            15.000    33.000    51.000
C
C
C$ Restrictions
C
C     1)  No error checking is performed to prevent numeric overflow or
C         underflow.
C
C     2)  No error checking performed to determine if the input and
C         output matrices have, in fact, been correctly dimensioned.
C
C     3)  MOUT should not overwrite M1 or M2.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
C
C        Changed input argument names ROW1, COL1 and COL2 to NR1, NC1R2
C        and NC2 for consistency with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example based on the existing example.
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
C     matrix times matrix n-dimensional_case
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      SUM

      INTEGER               I
      INTEGER               J
      INTEGER               K

C
C  Perform the matrix multiplication
C
      DO I = 1, NR1
         DO J = 1, NC2
            SUM = 0.D0
            DO K = 1, NC1R2
               SUM = SUM + M1(I,K)*M2(K,J)
            END DO
            MOUT(I,J) = SUM
         END DO
      END DO
C
      RETURN
      END

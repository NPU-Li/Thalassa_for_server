C$Procedure MTXMG ( Matrix transpose times matrix, general dimension )

      SUBROUTINE MTXMG ( M1, M2, NC1, NR1R2, NC2, MOUT )

C$ Abstract
C
C     Multiply the transpose of a matrix with another matrix,
C     both of arbitrary size. (The dimensions of the matrices must be
C     compatible with this multiplication.)
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

      INTEGER            NC1
      INTEGER            NR1R2
      INTEGER            NC2
      DOUBLE PRECISION   M1   ( NR1R2,NC1 )
      DOUBLE PRECISION   M2   ( NR1R2,NC2 )
      DOUBLE PRECISION   MOUT ( NC1,  NC2 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   Left-hand matrix whose transpose is to be
C                    multiplied.
C     M2         I   Right-hand matrix to be multiplied.
C     NC1        I   Column dimension of M1 and row dimension of MOUT.
C     NR1R2      I   Row dimension of both M1 and M2.
C     NC2        I   Column dimension of both M2 and MOUT.
C     MOUT       O   Product matrix M1**T * M2.
C
C$ Detailed_Input
C
C     M1       is an double precision matrix of arbitrary dimension
C              whose transpose is the left hand multiplier of a
C              matrix multiplication.
C
C     M2       is an double precision matrix of arbitrary dimension
C              whose transpose is the left hand multiplier of a
C              matrix multiplication.
C
C     NC1      is the column dimension of M1 and row dimension of
C              MOUT.
C
C     NR1R2    is the row dimension of both M1 and M2.
C
C     NC2      is the column dimension of both M2 and MOUT.
C
C$ Detailed_Output
C
C     MOUT     is a double precision matrix containing the product
C
C                           T
C                 MOUT =  M1  x  M2
C
C              where the superscript T denotes the transpose of M1.
C
C              MOUT must NOT overwrite either M1 or M2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NR1R2 < 1, the elements of the matrix MOUT are set equal to
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
C     MOUT(I,J) = Summation from K=1 to NR1R2 of  ( M1(K,I) * M2(K,J) )
C
C     Note that the reversal of the K and I subscripts in the left-hand
C     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
C     and not simply of M1 itself.
C
C     Since this subroutine operates on matrices of arbitrary size, it
C     is not possible to buffer intermediate results. Thus, MOUT
C     should NOT overwrite either M1 or M2.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 2x4 and a 2x3 matrices, multiply the transpose of the
C        first matrix by the second one.
C
C
C        Example code begins here.
C
C
C              PROGRAM MTXMG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M1   ( 4, 2 )
C              DOUBLE PRECISION      M2   ( 2, 3 )
C              DOUBLE PRECISION      MOUT ( 4, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M1 and M2.
C        C
C              DATA                  M1 /  1.0D0,  1.0D0,
C             .                            2.0D0,  1.0D0,
C             .                            3.0D0,  1.0D0,
C             .                            0.0D0,  1.0D0  /
C
C              DATA                  M2 /  1.0D0,  0.0D0,
C             .                            2.0D0,  0.0D0,
C             .                            3.0D0,  0.0D0  /
C
C        C
C        C     Multiply the transpose of M1 by M2.
C        C
C              CALL MTXMG ( M1, M2, 4, 2, 3, MOUT )
C
C              WRITE(*,'(A)') 'Transpose of M1 times M2:'
C              DO I = 1, 4
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
C        Transpose of M1 times M2:
C             1.000     2.000     3.000
C             2.000     4.000     6.000
C             3.000     6.000     9.000
C             0.000     0.000     0.000
C
C
C$ Restrictions
C
C     1)  The user is responsible for checking the magnitudes of the
C         elements of M1 and M2 so that a floating point overflow does
C         not occur.
C
C     2)  MOUT must not overwrite M1 or M2 or else the intermediate
C         will affect the final result.
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
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code example based on the existing example.
C
C        Added entry #1 to $Exceptions section.
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
C     matrix_transpose times matrix n-dimensional_case
C
C-&


C
C     Local variables
C
      INTEGER               I
      INTEGER               J
      INTEGER               K

C
C  Perform the matrix multiplication
C
      DO I = 1, NC1
         DO J = 1, NC2

            MOUT(I,J) = 0.D0
            DO K = 1, NR1R2
               MOUT(I,J) = MOUT(I,J) + M1(K,I)*M2(K,J)
            END DO

         END DO
      END DO
C
      RETURN
      END

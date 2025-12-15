C$Procedure MXVG ( Matrix time vector, general dimension )

      SUBROUTINE MXVG ( M1, V2, NR1, NC1R2, VOUT )

C$ Abstract
C
C     Multiply a matrix and a vector of arbitrary size.
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
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      INTEGER            NR1
      INTEGER            NC1R2
      DOUBLE PRECISION   M1   ( NR1,NC1R2 )
      DOUBLE PRECISION   V2   (     NC1R2 )
      DOUBLE PRECISION   VOUT ( NR1       )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   Left-hand matrix to be multiplied.
C     V2         I   Right-hand vector to be multiplied.
C     NR1        I   Row dimension of M1 and length of VOUT.
C     NC1R2      I   Column dimension of M1 and length of V2.
C     VOUT       O   Product vector M1*V2.
C
C$ Detailed_Input
C
C     M1       is a double precision matrix of arbitrary size which
C              forms the left-hand matrix of the multiplication.
C
C     V2       is a double precision vector on the right of the
C              multiplication.
C
C     NR1      is the row dimension of M1 and length of VOUT.
C
C     NC1R2    is the column dimension of M1 and length of V2.
C
C$ Detailed_Output
C
C     VOUT     is the double precision vector which results from
C              the expression VOUT = (M1) x V2.
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
C        For each value of the subscript I from 1 to NR1,
C
C        VOUT(I) = Summation from K=1 to NC1R2 of  ( M1(I,K) * V2(K) )
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 2x3 matrix and a 3-vector, multiply the matrix by
C        the vector.
C
C
C        Example code begins here.
C
C
C              PROGRAM MXVG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M    ( 2, 3 )
C              DOUBLE PRECISION      VIN  ( 3    )
C              DOUBLE PRECISION      VOUT ( 2    )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M and VIN.
C        C
C              DATA                  M    /  1.0D0,  2.0D0,
C             .                              1.0D0,  3.0D0,
C             .                              1.0D0,  4.0D0  /
C
C              DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
C
C        C
C        C     Multiply M by VIN.
C        C
C              CALL MXVG ( M, VIN, 2, 3, VOUT )
C
C              WRITE(*,'(A)') 'M times VIN:'
C              WRITE(*,'(2F10.3)') VOUT
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        M times VIN:
C             6.000    20.000
C
C
C$ Restrictions
C
C     1)  The user is responsible for checking the magnitudes of the
C         elements of M1 and V2 so that a floating point overflow does
C         not occur.
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
C-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example based on the existing example.
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Re-ordered header sections and made minor formatting
C        changes.
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
C     matrix times n-dimensional vector
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      SUM

      INTEGER               I
      INTEGER               K

C
C  Perform the matrix-vector multiplication
C
      DO I = 1, NR1
         SUM = 0.D0
         DO K = 1, NC1R2
            SUM = SUM + M1(I,K)*V2(K)
         END DO
         VOUT(I) = SUM
      END DO

      RETURN
      END

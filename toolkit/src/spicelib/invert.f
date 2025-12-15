C$Procedure INVERT ( Invert a 3x3 matrix )

      SUBROUTINE INVERT ( M, MOUT )

C$ Abstract
C
C     Generate the inverse of a 3x3 matrix.
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
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   M    ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M          I   Matrix to be inverted.
C     MOUT       O   Inverted matrix (M)**-1. If M is singular, then
C                    MOUT will be the zero matrix.
C
C$ Detailed_Input
C
C     M        is an arbitrary 3x3 matrix. The limits on the size of
C              elements of M are determined by the process of
C              calculating the cofactors of each element of the matrix.
C              For a 3x3 matrix this amounts to the differencing of two
C              terms, each of which consists of the multiplication of
C              two matrix elements. This multiplication must not exceed
C              the range of double precision numbers or else an overflow
C              error will occur.
C
C$ Detailed_Output
C
C     MOUT     is the inverse of M and is calculated explicitly using
C              the matrix of cofactors. MOUT is set to be the zero
C              matrix if M is singular.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If M is singular, MOUT is set to be the zero matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     First the determinant is explicitly calculated using the
C     fundamental definition of the determinant. If this value is less
C     that 10**-16 then the matrix is deemed to be singular and the
C     output value is filled with zeros. Otherwise, the output matrix
C     is calculated an element at a time by generating the cofactor of
C     each element. Finally, each element in the matrix of cofactors
C     is multiplied by the reciprocal of the determinant and the result
C     is the inverse of the original matrix.
C
C     NO INTERNAL CHECKING ON THE INPUT MATRIX M IS PERFORMED EXCEPT
C     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE
C     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF
C     CALCULATING THE MATRIX OF COFACTORS.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a double precision 3x3 matrix, compute its inverse. Check
C        that the original matrix times the computed inverse produces
C        the identity matrix.
C
C        Example code begins here.
C
C
C              PROGRAM INVERT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      IMAT ( 3, 3 )
C              DOUBLE PRECISION      M    ( 3, 3 )
C              DOUBLE PRECISION      MOUT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define a matrix to invert.
C        C
C              DATA                  M  /  0.D0,  0.5D0, 0.D0,
C             .                           -1.D0,  0.D0,  0.D0,
C             .                            0.D0,  0.D0,  1.D0 /
C
C              WRITE(*,*) 'Original Matrix:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 )
C
C              END DO
C        C
C        C     Invert the matrix, then output.
C        C
C              CALL INVERT ( M, MOUT )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Inverse Matrix:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
C
C              END DO
C
C        C
C        C     Check the M times MOUT produces the identity matrix.
C        C
C              CALL MXM ( M, MOUT, IMAT )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original times inverse:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 )
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
C         Original Matrix:
C               0.0000000      -1.0000000       0.0000000
C               0.5000000       0.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
C
C         Inverse Matrix:
C               0.0000000       2.0000000      -0.0000000
C              -1.0000000       0.0000000      -0.0000000
C               0.0000000      -0.0000000       1.0000000
C
C         Original times inverse:
C               1.0000000       0.0000000       0.0000000
C               0.0000000       1.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
C
C
C$ Restrictions
C
C     1)  The input matrix must be such that generating the cofactors
C         will not cause a floating point overflow or underflow. The
C         strictness of this condition depends, of course, on the
C         computer installation and the resultant maximum and minimum
C         values of double precision numbers.
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
C        Changed input argument name M1 to M for consistency with other
C        routines.
C
C        Added IMPLICIT NONE statement.
C
C        Updated the header to comply with NAIF standard. Added
C        complete code example to $Examples section.
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
C     invert a 3x3_matrix
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION   DET
      DOUBLE PRECISION   MTEMP(3,3)
      DOUBLE PRECISION   MDET
      DOUBLE PRECISION   INVDET

C
C  Find the determinant of M and check for singularity
C
      MDET = DET(M)
      IF ( DABS(MDET) .LT. 1.D-16 ) THEN
         CALL FILLD ( 0.D0, 9, MOUT )
         RETURN
      END IF

C
C  Get the cofactors of each element of M
C
      MTEMP(1,1) =  (M(2,2)*M(3,3) - M(3,2)*M(2,3))
      MTEMP(1,2) = -(M(1,2)*M(3,3) - M(3,2)*M(1,3))
      MTEMP(1,3) =  (M(1,2)*M(2,3) - M(2,2)*M(1,3))
      MTEMP(2,1) = -(M(2,1)*M(3,3) - M(3,1)*M(2,3))
      MTEMP(2,2) =  (M(1,1)*M(3,3) - M(3,1)*M(1,3))
      MTEMP(2,3) = -(M(1,1)*M(2,3) - M(2,1)*M(1,3))
      MTEMP(3,1) =  (M(2,1)*M(3,2) - M(3,1)*M(2,2))
      MTEMP(3,2) = -(M(1,1)*M(3,2) - M(3,1)*M(1,2))
      MTEMP(3,3) =  (M(1,1)*M(2,2) - M(2,1)*M(1,2))

C
C  Multiply the cofactor matrix by 1/MDET to obtain the inverse
C
      INVDET = 1.D0 / MDET
      CALL VSCLG (INVDET, MTEMP, 9, MOUT)
C
      RETURN
      END

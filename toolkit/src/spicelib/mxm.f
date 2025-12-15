C$Procedure MXM ( Matrix times matrix, 3x3 )

      SUBROUTINE MXM ( M1, M2, MOUT )

C$ Abstract
C
C     Multiply two 3x3 matrices.
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

      DOUBLE PRECISION   M1   ( 3,3 )
      DOUBLE PRECISION   M2   ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   3x3 double precision matrix.
C     M2         I   3x3 double precision matrix.
C     MOUT       O   The 3x3 double precision matrix product M1*M2.
C
C$ Detailed_Input
C
C     M1       is an arbitrary 3x3 double precision matrix.
C
C     M2       is an arbitrary 3x3 double precision matrix.
C
C$ Detailed_Output
C
C     MOUT     is a 3x3 double precision matrix. MOUT is the product
C              M1*M2.
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
C        For each value of the subscripts I and J from 1 to 3:
C
C                          3
C                       .-----
C                        \
C           MOUT(I,J) =   )  M1(I,K) * M2(K,J)
C                        /
C                       '-----
C                         K=1
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given two 3x3 double precision matrices, compute their
C        product.
C
C
C        Example code begins here.
C
C
C              PROGRAM MXM_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M1   ( 3, 3 )
C              DOUBLE PRECISION      M2   ( 3, 3 )
C              DOUBLE PRECISION      MOUT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M1 and M2.
C        C
C              DATA                  M1   /  1.0D0, -1.0D0,  0.0D0,
C             .                              1.0D0,  1.0D0,  0.0D0,
C             .                              0.0D0,  0.0D0,  1.0D0  /
C
C              DATA                  M2   /  1.0D0,  0.0D0,  0.0D0,
C             .                              0.0D0,  1.0D0, -1.0D0,
C             .                              0.0D0,  1.0D0,  1.0D0  /
C
C        C
C        C     Compute M1 times M2.
C        C
C              CALL MXM ( M1, M2, MOUT )
C
C              WRITE(*,'(A)') 'M1:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( M1(I,J), J=1,3 )
C
C              END DO
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'M2:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( M2(I,J), J=1,3 )
C
C              END DO
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'M1 times M2:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
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
C        M1:
C               1.0000000       1.0000000       0.0000000
C              -1.0000000       1.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
C
C        M2:
C               1.0000000       0.0000000       0.0000000
C               0.0000000       1.0000000       1.0000000
C               0.0000000      -1.0000000       1.0000000
C
C        M1 times M2:
C               1.0000000       1.0000000       1.0000000
C              -1.0000000       1.0000000       1.0000000
C               0.0000000      -1.0000000       1.0000000
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
C     matrix times matrix 3x3_case
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      PRODM( 3, 3 )

      INTEGER               I
      INTEGER               J

C
C  Perform the matrix multiplication
C
      DO I=1,3
         DO J=1,3
            PRODM(I,J) = M1(I,1)*M2(1,J)
     .                 + M1(I,2)*M2(2,J)
     .                 + M1(I,3)*M2(3,J)
         END DO
      END DO
C
C  Move the result into MOUT
C
      CALL MOVED (PRODM,9,MOUT)

      RETURN
      END

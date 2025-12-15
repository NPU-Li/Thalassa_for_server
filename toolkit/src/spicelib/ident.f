C$Procedure IDENT ( Return the 3x3 identity matrix )

      SUBROUTINE IDENT ( MATRIX )

C$ Abstract
C
C     Return the 3x3 identity matrix.
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

      DOUBLE PRECISION      MATRIX ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     O   The 3x3 identity matrix.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     MATRIX   is the 3x3 Identity matrix. That MATRIX is
C              the following
C
C                 .-                       -.
C                 |  1.0D0   0.0D0   0.0D0  |
C                 |  0.0D0   1.0D0   0.0D0  |
C                 |  0.0D0   0.0D0   1.0D0  |
C                 `-                       -'
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
C     This is a utility routine for obtaining the 3x3 identity matrix
C     so that you may avoid having to write the loop or assignments
C     needed to get the matrix.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define a 3x3 matrix and compute its inverse using the SPICELIB
C        routine INVERT. Verify the accuracy of the computed inverse
C        using the mathematical identity
C
C                -1
C           M x M   - I = 0
C
C        where I is the 3x3 identity matrix.
C
C
C        Example code begins here.
C
C
C              PROGRAM IDENT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      IDMAT  ( 3, 3 )
C              DOUBLE PRECISION      IMAT   ( 3, 3 )
C              DOUBLE PRECISION      M      ( 3, 3 )
C              DOUBLE PRECISION      MOUT   ( 3, 3 )
C              DOUBLE PRECISION      MZERO  ( 3, 3 )
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
C              CALL IDENT ( IDMAT )
C              CALL MXM   ( M, MOUT, IMAT )
C
C              CALL VSUBG ( IMAT, IDMAT, 9, MZERO )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original times inverse minus identity:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( MZERO(I,J), J=1,3 )
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
C         Original times inverse minus identity:
C               0.0000000       0.0000000       0.0000000
C               0.0000000       0.0000000       0.0000000
C               0.0000000       0.0000000       0.0000000
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C-    SPICELIB Version 1.0.0, 05-FEB-1996 (WLT)
C
C-&


C$ Index_Entries
C
C     Get the 3x3 identity matrix
C
C-&


      MATRIX ( 1, 1 ) = 1.0D0
      MATRIX ( 2, 1 ) = 0.0D0
      MATRIX ( 3, 1 ) = 0.0D0

      MATRIX ( 1, 2 ) = 0.0D0
      MATRIX ( 2, 2 ) = 1.0D0
      MATRIX ( 3, 2 ) = 0.0D0

      MATRIX ( 1, 3 ) = 0.0D0
      MATRIX ( 2, 3 ) = 0.0D0
      MATRIX ( 3, 3 ) = 1.0D0

      RETURN
      END

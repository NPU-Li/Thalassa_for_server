C$Procedure MXV ( Matrix times vector, 3x3 )

      SUBROUTINE MXV ( M, VIN, VOUT )

C$ Abstract
C
C     Multiply a 3x3 double precision matrix with a 3-dimensional
C     double precision vector.
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

      DOUBLE PRECISION   M      ( 3, 3 )
      DOUBLE PRECISION   VIN    (    3 )
      DOUBLE PRECISION   VOUT   (    3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M          I   3x3 double precision matrix.
C     VIN        I   3-dimensional double precision vector.
C     VOUT       O   3-dimensional double precision vector. VOUT is
C                    the product M*VIN.
C
C$ Detailed_Input
C
C     M        is an arbitrary 3x3 double precision matrix.
C
C     VIN      is an arbitrary 3-dimensional double precision vector.
C
C$ Detailed_Output
C
C     VOUT     is a 3-dimensional double precision vector. VOUT is
C              the product M * V.
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
C        For each value of the subscript I from 1 to 3:
C
C                        3
C                     .-----
C                      \
C           VOUT(I) =   )  M(I,K) * VIN(K)
C                      /
C                     '-----
C                       K=1
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 3x3 matrix and a 3-vector, multiply the matrix by
C        the vector.
C
C
C        Example code begins here.
C
C
C              PROGRAM MXV_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M    ( 3, 3 )
C              DOUBLE PRECISION      VIN  ( 3    )
C              DOUBLE PRECISION      VOUT ( 3    )
C
C        C
C        C     Define M and VIN.
C        C
C              DATA                  M    /  0.0D0, -1.0D0,  0.0D0,
C             .                              1.0D0,  0.0D0,  0.0D0,
C             .                              0.0D0,  0.0D0,  1.0D0  /
C
C              DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
C
C        C
C        C     Multiply M by VIN.
C        C
C              CALL MXV ( M, VIN, VOUT )
C
C              WRITE(*,'(A)') 'M times VIN:'
C              WRITE(*,'(3F10.3)') VOUT
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        M times VIN:
C             2.000    -1.000     3.000
C
C
C$ Restrictions
C
C     1)  The user is responsible for checking the magnitudes of the
C         elements of M and VIN so that a floating point overflow does
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
C-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on the existing example.
C
C        Changed input argument name MATRIX to M for consistency with
C        other routines.
C
C        Added IMPLICIT NONE statement.
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
C     matrix times 3-dimensional vector
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION     PRODV  ( 3 )
      INTEGER              I

C
C  Perform the matrix-vector multiplication
C
      DO I = 1, 3
         PRODV(I) = M(I,1)*VIN(1) +
     .              M(I,2)*VIN(2) +
     .              M(I,3)*VIN(3)
      END DO
C
C  Move the buffered vector into the output vector VOUT.
C
      VOUT(1) = PRODV(1)
      VOUT(2) = PRODV(2)
      VOUT(3) = PRODV(3)

      RETURN
      END

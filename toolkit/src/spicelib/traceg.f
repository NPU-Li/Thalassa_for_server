C$Procedure TRACEG ( Trace of a matrix, general dimension )

      DOUBLE PRECISION FUNCTION  TRACEG ( MATRIX, NDIM )

C$ Abstract
C
C     Return the trace of a square matrix of arbitrary dimension.
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

      INTEGER            NDIM
      DOUBLE PRECISION   MATRIX ( NDIM,NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     I     NDIM x NDIM matrix of double precision numbers.
C     NDIM       I     Dimension of the matrix.
C
C     The function returns the trace of the square matrix of arbitrary
C     dimension MATRIX.
C
C$ Detailed_Input
C
C     MATRIX   is a double precision square matrix of arbitrary
C              dimension. The input matrix must be square or else the
C              concept is meaningless.
C
C     NDIM     is the dimension of MATRIX.
C
C$ Detailed_Output
C
C     The function returns the trace of MATRIX, i.e. it is the sum of
C     the diagonal elements of MATRIX.
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
C     The code reflects precisely the following mathematical
C     expression:
C
C                   NDIM
C                 .------
C                  \
C        TRACEG =   ) MATRIX(I,I)
C                  /
C                 '------
C                    I=1
C
C     No error detection or correction is implemented within this
C     function.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 4x4 double precision matrix, compute its trace.
C
C
C        Example code begins here.
C
C
C              PROGRAM TRACEG_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      TRACEG
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      MATRIX ( NDIM, NDIM )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define MATRIX.
C        C
C              DATA                  MATRIX  /
C             .                          3.D0,  0.D0,  4.D0,  0.D0,
C             .                          5.D0, -2.D0,  0.D0,  0.D0,
C             .                          7.D0,  8.D0, -1.D0,  1.D0,
C             .                          3.D0,  1.D0,  0.D0,  0.D0  /
C
C
C              WRITE(*,'(A)') 'Matrix:'
C              DO I=1, NDIM
C
C                 WRITE(*,'(4F6.1)') ( MATRIX(I,J), J=1,NDIM )
C
C              END DO
C
C        C
C        C     Compute the trace of MATRIX and display the result.
C        C
C              WRITE(*,*)
C              WRITE(*,'(A,F4.1)') 'Trace: ', TRACEG ( MATRIX, NDIM )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Matrix:
C           3.0   5.0   7.0   3.0
C           0.0  -2.0   8.0   1.0
C           4.0   0.0  -1.0   0.0
C           0.0   0.0   1.0   0.0
C
C        Trace:  0.0
C
C
C$ Restrictions
C
C     1)  No checking is performed to guard against floating point
C         overflow or underflow. This routine should probably not be
C         used if the input matrix is expected to have large double
C         precision numbers along the diagonal.
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
C-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Created
C        complete code example based on existing fragment. Updated
C        $Particulars to provide mathematical representation of the
C        implemented algorithm.
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
C     trace of a nxn_matrix
C
C-&


      INTEGER         I

      TRACEG = 0.0D0
      DO I = 1,NDIM
         TRACEG = TRACEG + MATRIX(I,I)
      END DO

      RETURN
      END

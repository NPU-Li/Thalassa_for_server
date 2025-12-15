C$Procedure VSUBG ( Vector subtraction, general dimension )

      SUBROUTINE VSUBG ( V1, V2, NDIM, VOUT )

C$ Abstract
C
C     Compute the difference between two double precision vectors of
C     arbitrary dimension.
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
      DOUBLE PRECISION   V1   ( NDIM )
      DOUBLE PRECISION   V2   ( NDIM )
      DOUBLE PRECISION   VOUT ( NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   First vector (minuend).
C     V2         I   Second vector (subtrahend).
C     NDIM       I   Dimension of V1, V2, and VOUT.
C     VOUT       O   Difference vector, V1 - V2.
C
C$ Detailed_Input
C
C     V1       is a double precision vector of arbitrary dimension which
C              is the minuend (i.e. first or left-hand member) in the
C              vector subtraction.
C
C     V2       is a double precision vector of arbitrary dimension which
C              is the subtrahend (i.e. second or right-hand member) in
C              the vector subtraction.
C
C     NDIM     is the dimension of V1 and V2 (and VOUT).
C
C$ Detailed_Output
C
C     VOUT     is a double precision n-dimensional vector which
C              represents the vector difference, V1 - V2.
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
C     For each value of the index I from 1 to NDIM, this routine
C     performs the following subtraction:
C
C        VOUT(I) = V1(I) - V2(I)
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
C     1) Define two sets of n-dimensional vectors and compute the
C        difference from each vector in first set with the
C        corresponding vector in the second set.
C
C
C        Example code begins here.
C
C
C              PROGRAM VSUBG_EX1
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
C              DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
C              DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
C              DOUBLE PRECISION      VOUT ( NDIM         )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  V1 /
C             .                      1.D0,  2.D0,  3.D0,  4.D0,
C             .                      1.D0,  2.D0,  3.D0,  4.D0,
C             .                      1.D0,  2.D0,  3.D0,  4.D0   /
C
C              DATA                  V2 /
C             .                      1.D0,  1.D0,  1.D0,  1.D0,
C             .                     -1.D0, -2.D0, -3.D0, -4.D0,
C             .                     -1.D0,  2.D0, -3.D0,  4.D0  /
C
C        C
C        C     Calculate the difference between each pair of vectors
C        C
C              DO I=1, SETSIZ
C
C                 CALL VSUBG ( V1(1,I), V2(1,I), NDIM, VOUT )
C
C                 WRITE(*,'(A,4F6.1)') 'First vector : ',
C             .                        ( V1(J,I), J=1,NDIM )
C                 WRITE(*,'(A,4F6.1)') 'Second vector: ',
C             .                        ( V2(J,I), J=1,NDIM )
C                 WRITE(*,'(A,4F6.1)') 'Difference   : ', VOUT
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
C        First vector :    1.0   2.0   3.0   4.0
C        Second vector:    1.0   1.0   1.0   1.0
C        Difference   :    0.0   1.0   2.0   3.0
C
C        First vector :    1.0   2.0   3.0   4.0
C        Second vector:   -1.0  -2.0  -3.0  -4.0
C        Difference   :    2.0   4.0   6.0   8.0
C
C        First vector :    1.0   2.0   3.0   4.0
C        Second vector:   -1.0   2.0  -3.0   4.0
C        Difference   :    2.0   0.0   6.0   0.0
C
C
C$ Restrictions
C
C     1)  No error checking is performed to guard against numeric
C         overflow. The programmer is thus required to insure that the
C         values in V1 and V2 are reasonable and will not cause
C         overflow. No error recovery or reporting scheme is
C         incorporated in this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
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
C-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 09-MAY-1990 (HAN)
C
C        Several errors in the header documentation were corrected.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     n-dimensional vector subtraction
C
C-&


C
C     Local variables
C
      INTEGER               I

      DO I=1,NDIM
         VOUT(I) = V1(I) - V2(I)
      END DO

      RETURN
      END

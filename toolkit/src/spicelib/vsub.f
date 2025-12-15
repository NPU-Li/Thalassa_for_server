C$Procedure VSUB ( Vector subtraction, 3 dimensions )

      SUBROUTINE VSUB ( V1, V2, VOUT )

C$ Abstract
C
C     Compute the difference between two double precision 3-dimensional
C     vectors.
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

      DOUBLE PRECISION  V1   ( 3 )
      DOUBLE PRECISION  V2   ( 3 )
      DOUBLE PRECISION  VOUT ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   First vector (minuend).
C     V2         I   Second vector (subtrahend).
C     VOUT       O   Difference vector, V1 - V2.
C
C$ Detailed_Input
C
C     V1       is a double precision 3-dimensional vector which is the
C              minuend (i.e. first or left-hand member) in the vector
C              subtraction.
C
C     V2       is a double precision 3-dimensional vector which is the
C              subtrahend (i.e. second or right-hand member) in the
C              vector subtraction.
C
C$ Detailed_Output
C
C     VOUT     is a double precision 3-dimensional vector which
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
C     For each value of the index I from 1 to 3, this routine performs
C     the following subtraction:
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
C     1) Define two sets of 3-dimensional vectors and compute the
C        difference from each vector in first set with the
C        corresponding vector in the second set.
C
C
C        Example code begins here.
C
C
C              PROGRAM VSUB_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 3 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      V1   ( 3, SETSIZ )
C              DOUBLE PRECISION      V2   ( 3, SETSIZ )
C              DOUBLE PRECISION      VOUT ( 3         )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  V1 / 1.D0,  2.D0,  3.D0,
C             .                           1.D0,  2.D0,  3.D0,
C             .                           1.D0,  2.D0,  3.D0  /
C
C              DATA                  V2 / 1.D0,  1.D0,  1.D0,
C             .                          -1.D0, -2.D0, -3.D0,
C             .                          -1.D0,  2.D0, -3.D0  /
C
C        C
C        C     Calculate the difference between each pair of vectors
C        C
C              DO I=1, SETSIZ
C
C                 CALL VSUB ( V1(1,I), V2(1,I), VOUT )
C
C                 WRITE(*,'(A,3F6.1)') 'First vector : ',
C             .                        ( V1(J,I), J=1,3 )
C                 WRITE(*,'(A,3F6.1)') 'Second vector: ',
C             .                        ( V2(J,I), J=1,3 )
C                 WRITE(*,'(A,3F6.1)') 'Difference   : ', VOUT
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
C        First vector :    1.0   2.0   3.0
C        Second vector:    1.0   1.0   1.0
C        Difference   :    0.0   1.0   2.0
C
C        First vector :    1.0   2.0   3.0
C        Second vector:   -1.0  -2.0  -3.0
C        Difference   :    2.0   4.0   6.0
C
C        First vector :    1.0   2.0   3.0
C        Second vector:   -1.0   2.0  -3.0
C        Difference   :    2.0   0.0   6.0
C
C
C$ Restrictions
C
C     1)  The user is required to determine that the magnitude each
C         component of the vectors is within the appropriate range so as
C         not to cause floating point overflow. No error recovery or
C         reporting scheme is incorporated in this routine.
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.4, 03-JUL-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C-    SPICELIB Version 1.0.3, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW)
C
C        Corrected a mistake in the second example's value
C        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with
C        (-1D24, 0.0, 0.0).
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
C     3-dimensional vector subtraction
C
C-&


      VOUT(1) = V1(1) - V2(1)
      VOUT(2) = V1(2) - V2(2)
      VOUT(3) = V1(3) - V2(3)

      RETURN
      END

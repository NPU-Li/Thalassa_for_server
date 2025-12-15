C$Procedure VADDG ( Vector addition, general dimension )

      SUBROUTINE VADDG ( V1, V2, NDIM, VOUT )

C$ Abstract
C
C     Add two vectors of arbitrary dimension.
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

      INTEGER             NDIM
      DOUBLE PRECISION    V1   ( NDIM )
      DOUBLE PRECISION    V2   ( NDIM )
      DOUBLE PRECISION    VOUT ( NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   First vector to be added.
C     V2         I   Second vector to be added.
C     NDIM       I   Dimension of V1, V2, and VOUT.
C     VOUT       O   Sum vector, V1 + V2.
C
C$ Detailed_Input
C
C     V1,
C     V2       are two arbitrary double precision n-dimensional
C              vectors.
C
C     NDIM     is the dimension of V1, V2 and VOUT.
C
C$ Detailed_Output
C
C     VOUT     is the double precision n-dimensional vector sum of V1
C              and V2.
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
C     This routine simply performs addition between components of V1
C     and V2. No checking is performed to determine whether floating
C     point overflow has occurred.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define two sets of n-dimensional vectors and compute the sum
C        of each vector in first set with the corresponding vector in
C        the second set.
C
C
C        Example code begins here.
C
C
C              PROGRAM VADDG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 2 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
C              DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
C              DOUBLE PRECISION      VOUT ( NDIM )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  SETA /
C             .                      1.D0,  2.D0,   3.D0,  4.D0,
C             .                      1.D-7, 1.D23, 1.D-9,  0.D0   /
C
C              DATA                  SETB /
C             .                      4.D0,  5.D0,   6.D0,  7.D0,
C             .                      1.D24, 1.D23,  0.D0,  3.D-23  /
C
C        C
C        C     Calculate the sum of each pair of vectors
C        C
C              DO I=1, SETSIZ
C
C                 CALL VADDG ( SETA(1,I), SETB(1,I), NDIM, VOUT )
C
C                 WRITE(*,'(A,4E11.2)') 'Vector A  : ',
C             .                        ( SETA(J,I), J=1,NDIM )
C                 WRITE(*,'(A,4E11.2)') 'Vector B  : ',
C             .                        ( SETB(J,I), J=1,NDIM )
C                 WRITE(*,'(A,4E11.2)') 'Sum vector: ', VOUT
C                 WRITE(*,*) ' '
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
C        Vector A  :    0.10E+01   0.20E+01   0.30E+01   0.40E+01
C        Vector B  :    0.40E+01   0.50E+01   0.60E+01   0.70E+01
C        Sum vector:    0.50E+01   0.70E+01   0.90E+01   0.11E+02
C
C        Vector A  :    0.10E-06   0.10E+24   0.10E-08   0.00E+00
C        Vector B  :    0.10E+25   0.10E+24   0.00E+00   0.30E-22
C        Sum vector:    0.10E+25   0.20E+24   0.10E-08   0.30E-22
C
C
C$ Restrictions
C
C     1)  The user is required to determine that the magnitude each
C         component of the vectors is within the appropriate range so as
C         not to cause floating point overflow.
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
C-    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW)
C
C        Corrected a mistake in the second example's value
C        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with
C        (1D24, 2D23).
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
C     n-dimensional vector addition
C
C-&


C
C     Local variables
C
      INTEGER             I

      DO I = 1, NDIM
         VOUT(I) = V1(I) + V2(I)
      END DO

      RETURN
      END

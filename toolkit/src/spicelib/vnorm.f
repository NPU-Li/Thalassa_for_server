C$Procedure VNORM ( Vector norm, 3 dimensions )

      DOUBLE PRECISION FUNCTION VNORM ( V1 )

C$ Abstract
C
C     Compute the magnitude of a double precision 3-dimensional
C     vector.
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

      DOUBLE PRECISION  V1 ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector whose magnitude is to be found.
C
C     The function returns the magnitude of V1.
C
C$ Detailed_Input
C
C     V1       is any double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C     The function returns the magnitude of V1 calculated in a
C     numerically stable way.
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
C     VNORM takes care to avoid overflow while computing the norm of the
C     input vector V1. VNORM finds the component of V1 whose magnitude
C     is the largest. Calling this magnitude V1MAX, the norm is computed
C     using the formula:
C
C                        ||    1         ||
C        VNORM = V1MAX * || ------- * V1 ||
C                        ||  V1MAX       ||
C
C     where the notation ||X|| indicates the norm of the vector X.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define a set of 3-dimensional vectors and compute the
C        magnitude of each vector within.
C
C
C        Example code begins here.
C
C
C              PROGRAM VNORM_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      VNORM
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
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define a set of 3-dimensional vectors.
C        C
C              DATA                  V1  /  1.D0,   2.D0,   2.D0,
C             .                             5.D0,  12.D0,   0.D0,
C             .                            -5.D-17, 0.0D0, 12.D-17  /
C
C        C
C        C     Calculate the magnitude of each vector
C        C
C              DO I=1, SETSIZ
C
C                 WRITE(*,'(A,3E10.2)') 'Input vector: ',
C             .                         ( V1(J,I), J=1,3 )
C                 WRITE(*,'(A,F24.20)') 'Magnitude   : ',
C             .                         VNORM ( V1(1,I) )
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
C        Input vector:   0.10E+01  0.20E+01  0.20E+01
C        Magnitude   :   3.00000000000000000000
C
C        Input vector:   0.50E+01  0.12E+02  0.00E+00
C        Magnitude   :  13.00000000000000000000
C
C        Input vector:  -0.50E-16  0.00E+00  0.12E-15
C        Magnitude   :   0.00000000000000013000
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
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 06-JUL-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
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
C     norm of 3-dimensional vector
C
C-&


      DOUBLE PRECISION V1MAX

C
C  Determine the maximum component of the vector.
C
      V1MAX = MAX (DABS(V1(1)), DABS(V1(2)), DABS(V1(3)))

C
C  If the vector is zero, return zero; otherwise normalize first.
C  Normalizing helps in the cases where squaring would cause overflow
C  or underflow.  In the cases where such is not a problem it not worth
C  it to optimize further.
C
      IF (V1MAX.EQ.0.D0) THEN
         VNORM = 0.D0
      ELSE
         VNORM = V1MAX * DSQRT (  (V1(1)/V1MAX)**2
     .                          + (V1(2)/V1MAX)**2
     .                          + (V1(3)/V1MAX)**2)
      END IF
C
      RETURN
      END

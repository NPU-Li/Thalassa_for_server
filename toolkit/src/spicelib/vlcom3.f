C$Procedure VLCOM3 ( Vector linear combination, 3 dimensions )

      SUBROUTINE VLCOM3 ( A, V1, B, V2, C, V3, SUM )

C$ Abstract
C
C     Compute the vector linear combination of three double precision
C     3-dimensional vectors.
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

      DOUBLE PRECISION   A
      DOUBLE PRECISION   V1  ( 3 )
      DOUBLE PRECISION   B
      DOUBLE PRECISION   V2  ( 3 )
      DOUBLE PRECISION   C
      DOUBLE PRECISION   V3  ( 3 )
      DOUBLE PRECISION   SUM ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   Coefficient of V1.
C     V1         I   Vector in 3-space.
C     B          I   Coefficient of V2.
C     V2         I   Vector in 3-space.
C     C          I   Coefficient of V3.
C     V3         I   Vector in 3-space.
C     SUM        O   Linear vector combination A*V1 + B*V2 + C*V3.
C
C$ Detailed_Input
C
C     A        is the double precision scalar variable that multiplies
C              V1.
C
C     V1       is an arbitrary, double precision 3-dimensional vector.
C
C     B        is the double precision scalar variable that multiplies
C              V2.
C
C     V2       is an arbitrary, double precision 3-dimensional vector.
C
C     C        is the double precision scalar variable that multiplies
C              V3.
C
C     V3       is a double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C     SUM      is the double precision 3-dimensional vector which
C              contains the linear combination
C
C                 A * V1 + B * V2 + C * V3
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
C        For each value of the index I, from 1 to 3:
C
C           SUM(I) = A * V1(I) + B * V2(I) + C * V3(I)
C
C     No error checking is performed to guard against numeric overflow.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose you have an instrument with an elliptical field
C        of view described by its angular extent along the semi-minor
C        and semi-major axes.
C
C        The following code example demonstrates how to create
C        16 vectors aiming at visualizing the field-of-view in
C        three dimensional space.
C
C
C        Example code begins here.
C
C
C              PROGRAM VLCOM3_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      TWOPI
C
C        C
C        C     Local parameters.
C        C
C        C     Define the two angular extends, along the semi-major
C        C     (U) and semi-minor (V) axes of the elliptical field
C        C     of view, in radians.
C        C
C              DOUBLE PRECISION      MAXANG
C              PARAMETER           ( MAXANG = 0.07D0 )
C
C              DOUBLE PRECISION      MINANG
C              PARAMETER           ( MINANG = 0.035D0 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      A
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      THETA
C              DOUBLE PRECISION      U      ( 3 )
C              DOUBLE PRECISION      V      ( 3 )
C              DOUBLE PRECISION      VECTOR ( 3 )
C              DOUBLE PRECISION      Z      ( 3 )
C
C              INTEGER               I
C
C        C
C        C     Let U and V be orthonormal 3-vectors spanning the
C        C     focal plane of the instrument, and Z its
C        C     boresight.
C        C
C              DATA                  U  /  1.D0,  0.D0,  0.D0 /
C              DATA                  V  /  0.D0,  1.D0,  0.D0 /
C              DATA                  Z  /  0.D0,  0.D0,  1.D0 /
C
C        C
C        C     Find the length of the ellipse's axes. Note that
C        C     we are dealing with unitary vectors.
C        C
C              A = TAN ( MAXANG )
C              B = TAN ( MINANG )
C
C        C
C        C     Compute the vectors of interest and display them
C        C
C              THETA = 0.D0
C              STEP  = TWOPI() / 16
C
C              DO I = 1, 16
C
C                 CALL VLCOM3 ( 1.D0,           Z, A * COS(THETA), U,
C             .                 B * SIN(THETA), V, VECTOR            )
C
C                 WRITE(*,'(I2,A,3F10.6)') I, ':', VECTOR
C
C                 THETA = THETA + STEP
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
C         1:  0.070115  0.000000  1.000000
C         2:  0.064777  0.013399  1.000000
C         3:  0.049578  0.024759  1.000000
C         4:  0.026832  0.032349  1.000000
C         5:  0.000000  0.035014  1.000000
C         6: -0.026832  0.032349  1.000000
C         7: -0.049578  0.024759  1.000000
C         8: -0.064777  0.013399  1.000000
C         9: -0.070115  0.000000  1.000000
C        10: -0.064777 -0.013399  1.000000
C        11: -0.049578 -0.024759  1.000000
C        12: -0.026832 -0.032349  1.000000
C        13: -0.000000 -0.035014  1.000000
C        14:  0.026832 -0.032349  1.000000
C        15:  0.049578 -0.024759  1.000000
C        16:  0.064777 -0.013399  1.000000
C
C
C$ Restrictions
C
C     1)  No error checking is performed to guard against numeric
C         overflow or underflow. The user is responsible for insuring
C         that the input values are reasonable.
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Added restriction #1.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 01-NOV-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     linear combination of three 3-dimensional vectors
C
C-&


      SUM(1) = A*V1(1) + B*V2(1) + C*V3(1)
      SUM(2) = A*V1(2) + B*V2(2) + C*V3(2)
      SUM(3) = A*V1(3) + B*V2(3) + C*V3(3)

      RETURN
      END

C$Procedure VPACK ( Pack three scalar components into a vector )

      SUBROUTINE VPACK ( X, Y, Z, V )

C$ Abstract
C
C     Pack three scalar components into a vector.
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

      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION V ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X,
C     Y,
C     Z          I   Scalar components of a vector.
C     V          O   Equivalent vector.
C
C$ Detailed_Input
C
C     X,
C     Y,
C     Z        are the scalar components of a 3-dimensional vector.
C
C$ Detailed_Output
C
C     V        is the equivalent vector, such that
C
C                 V(1) = X
C                 V(2) = Y
C                 V(3) = Z
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
C     Basically, this is just shorthand notation for the common
C     sequence
C
C        V(1) = X
C        V(2) = Y
C        V(3) = Z
C
C     The routine is useful largely for two reasons. First, it
C     reduces the chance that the programmer will make a "cut and
C     paste" mistake, like
C
C        V(1) = X
C        V(1) = Y
C        V(1) = Z
C
C     Second, it makes conversions between equivalent units simpler,
C     and clearer. For instance, the sequence
C
C        V(1) = X * RPD
C        V(2) = Y * RPD
C        V(3) = Z * RPD
C
C     can be replaced by the (nearly) equivalent sequence
C
C        CALL VPACK  ( X,   Y, Z, V )
C        CALL VSCLIP ( RPD, V       )
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input
C     (if any), the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute an upward normal of an equilateral triangle lying
C        in the X-Y plane and centered at the origin.
C
C
C        Example code begins here.
C
C
C              PROGRAM VPACK_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      NORMAL ( 3 )
C              DOUBLE PRECISION      S
C              DOUBLE PRECISION      V1     ( 3 )
C              DOUBLE PRECISION      V2     ( 3 )
C              DOUBLE PRECISION      V3     ( 3 )
C
C
C              S = SQRT(3.D0)/2
C
C        C
C        C     Define the three corners of the triangle.
C        C
C              CALL VPACK (    S,  -0.5D0,  0.D0, V1 )
C              CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 )
C              CALL VPACK (   -S,  -0.5D0,  0.D0, V3 )
C
C        C
C        C     Compute an upward normal of the triangle.
C        C
C              CALL PLTNRM ( V1, V2, V3, NORMAL )
C
C              WRITE (*, '(A,3F17.13)' ) 'NORMAL = ', NORMAL
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        NORMAL =   0.0000000000000  0.0000000000000  2.5980762113533
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
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 16-JUL-2020 (JDR)
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     pack three scalar components into a vector
C
C-&


C
C     Just shorthand, like it says above.
C
      V(1) = X
      V(2) = Y
      V(3) = Z

      RETURN
      END

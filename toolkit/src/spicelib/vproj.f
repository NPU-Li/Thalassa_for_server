C$Procedure VPROJ ( Vector projection, 3 dimensions )

      SUBROUTINE VPROJ ( A, B, P )

C$ Abstract
C
C     Compute the projection of one 3-dimensional vector onto another
C     3-dimensional vector.
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

      DOUBLE PRECISION   A ( 3 )
      DOUBLE PRECISION   B ( 3 )
      DOUBLE PRECISION   P ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   The vector to be projected.
C     B          I   The vector onto which A is to be projected.
C     P          O   The projection of A onto B.
C
C$ Detailed_Input
C
C     A        is a double precision, 3-dimensional vector. This
C              vector is to be projected onto the vector B.
C
C     B        is a double precision, 3-dimensional vector. This
C              vector is the vector which receives the projection.
C
C$ Detailed_Output
C
C     P        is a double precision, 3-dimensional vector containing
C              the projection of A onto B. (P is necessarily parallel
C              to B.) If B is the zero vector then P will be returned
C              as the zero vector.
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
C     Given any vectors A and B, there is a unique decomposition of
C     A as a sum V + P such that V, the dot product of V and B, is zero,
C     and the dot product of P with B is equal the product of the
C     lengths of P and B. P is called the projection of A onto B. It
C     can be expressed mathematically as
C
C        DOT(A,B)
C        -------- * B
C        DOT(B,B)
C
C     (This is not necessarily the prescription used to compute the
C     projection. It is intended only for descriptive purposes.)
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define two sets of vectors and compute the projection of
C        each vector of the first set on the corresponding vector of
C        the second set.
C
C        Example code begins here.
C
C
C              PROGRAM VPROJ_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 3 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
C              DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
C              DOUBLE PRECISION      PVEC ( NDIM )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  SETA / 6.D0,  6.D0,  6.D0,
C             .                             6.D0,  6.D0,  6.D0,
C             .                             6.D0,  6.D0,  0.D0,
C             .                             6.D0,  0.D0,  0.D0  /
C
C              DATA                  SETB / 2.D0,  0.D0,  0.D0,
C             .                            -3.D0,  0.D0,  0.D0,
C             .                             0.D0,  7.D0,  0.D0,
C             .                             0.D0,  0.D0,  9.D0  /
C
C        C
C        C     Calculate the projection
C        C
C              DO I=1, SETSIZ
C
C                 CALL VPROJ ( SETA(1,I), SETB(1,I), PVEC )
C
C                 WRITE(*,'(A,3F5.1)') 'Vector A  : ',
C             .                        ( SETA(J,I), J=1,3 )
C                 WRITE(*,'(A,3F5.1)') 'Vector B  : ',
C             .                        ( SETB(J,I), J=1,3 )
C                 WRITE(*,'(A,3F5.1)') 'Projection: ', PVEC
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
C        Vector A  :   6.0  6.0  6.0
C        Vector B  :   2.0  0.0  0.0
C        Projection:   6.0  0.0  0.0
C
C        Vector A  :   6.0  6.0  6.0
C        Vector B  :  -3.0  0.0  0.0
C        Projection:   6.0 -0.0 -0.0
C
C        Vector A  :   6.0  6.0  0.0
C        Vector B  :   0.0  7.0  0.0
C        Projection:   0.0  6.0  0.0
C
C        Vector A  :   6.0  0.0  0.0
C        Vector B  :   0.0  0.0  9.0
C        Projection:   0.0  0.0  0.0
C
C
C$ Restrictions
C
C     1)  An implicit assumption exists that A and B are specified in
C         the same reference frame. If this is not the case, the
C         numerical result has no meaning.
C
C$ Literature_References
C
C     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
C          7th Edition, Addison Wesley, 1988.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Added entry in $Restrictions section.
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
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
C     3-dimensional vector projection
C
C-&


C$ Revisions
C
C-    Beta Version 1.1.0, 4-JAN-1989 (WLT)
C
C        Upgrade the routine to work with negative axis indexes. Also
C        take care of the funky way the indices (other than the input)
C        were obtained via the MOD function. It works but isn't as
C        clear (or fast) as just reading the axes from data.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT

C
C     Local variables
C
      DOUBLE PRECISION      BIGA
      DOUBLE PRECISION      BIGB
      DOUBLE PRECISION      R(3)
      DOUBLE PRECISION      T(3)
      DOUBLE PRECISION      SCALE


      BIGA = MAX ( DABS(A(1)), DABS(A(2)), DABS(A(3)) )
      BIGB = MAX ( DABS(B(1)), DABS(B(2)), DABS(B(3)) )

      IF ( BIGA .EQ. 0 ) THEN
         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN
      END IF

      IF (  BIGB .EQ. 0 ) THEN
         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN
      END IF

      R(1)  = B(1) / BIGB
      R(2)  = B(2) / BIGB
      R(3)  = B(3) / BIGB

      T(1)  = A(1) / BIGA
      T(2)  = A(2) / BIGA
      T(3)  = A(3) / BIGA

      SCALE = VDOT (T,R) * BIGA  / VDOT (R,R)

      CALL VSCL ( SCALE, R, P )

      RETURN
      END

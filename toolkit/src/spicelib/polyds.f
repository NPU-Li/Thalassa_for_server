C$Procedure POLYDS ( Compute a Polynomial and its Derivatives )

      SUBROUTINE POLYDS ( COEFFS, DEG, NDERIV, T, P )

C$ Abstract
C
C     Compute the value of a polynomial and its first
C     NDERIV derivatives at the value T.
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
C     INTERPOLATION
C     MATH
C     POLYNOMIAL
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      COEFFS ( 0:* )
      INTEGER               DEG
      INTEGER               NDERIV
      DOUBLE PRECISION      T
      DOUBLE PRECISION      P      ( 0:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COEFFS     I   Coefficients of the polynomial to be evaluated.
C     DEG        I   Degree of the polynomial to be evaluated.
C     NDERIV     I   Number of derivatives to compute.
C     T          I   Point to evaluate the polynomial and derivatives
C     P          O   Value of polynomial and derivatives.
C
C$ Detailed_Input
C
C     COEFFS   are the coefficients of the polynomial that is
C              to be evaluated. The first element of this array
C              should be the constant term, the second element the
C              linear coefficient, the third term the quadratic
C              coefficient, and so on. The number of coefficients
C              supplied should be one more than DEG.
C
C                 F(X) =   COEFFS(1) + COEFFS(2)*X + COEFFS(3)*X^2
C
C                        + COEFFS(4)*X^4 + ... + COEFFS(DEG+1)*X^DEG
C
C     DEG      is the degree of the polynomial to be evaluated. DEG
C              should be one less than the number of coefficients
C              supplied.
C
C     NDERIV   is the number of derivatives to compute. If NDERIV
C              is zero, only the polynomial will be evaluated. If
C              NDERIV = 1, then the polynomial and its first
C              derivative will be evaluated, and so on. If the value
C              of NDERIV is negative, the routine returns
C              immediately.
C
C     T        is the point at which the polynomial and its
C              derivatives should be evaluated.
C
C$ Detailed_Output
C
C     P        is an array containing the value of the polynomial and
C              its derivatives evaluated at T. The first element of
C              the array contains the value of P at T. The second
C              element of the array contains the value of the first
C              derivative of P at T and so on. The NDERIV + 1'st
C              element of the array contains the NDERIV'th derivative
C              of P evaluated at T.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NDERIV is less than zero, the routine simply returns.
C
C     2)  If the degree of the polynomial is less than 0, the routine
C         returns the first NDERIV+1 elements of P set to 0.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine uses the user supplied coefficients (COEFFS)
C     to evaluate a polynomial (having these coefficients) and its
C     derivatives at the point T. The zero'th derivative of the
C     polynomial is regarded as the polynomial itself.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) For the polynomial
C
C           F(x) = 1 + 3*x + 0.5*x^2 + x^3 + 0.5*x^4 - x^5 + x^6
C
C        the coefficient set
C
C           Degree  coeffs
C           ------  ------
C           0       1
C           1       3
C           2       0.5
C           3       1
C           4       0.5
C           5      -1
C           6       1
C
C        Compute the value of the polynomial and it's first
C        3 derivatives at the value T = 1.0. We expect:
C
C           Derivative Number     T = 1
C           ------------------    -----
C           F(x)         0        6
C           F'(x)        1        10
C           F''(x)       2        23
C           F'''(x)      3        78
C
C
C        Example code begins here.
C
C
C              PROGRAM POLYDS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER               NDERIV
C              PARAMETER           ( NDERIV = 3 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      COEFFS (7)
C              DOUBLE PRECISION      P      ( NDERIV + 1 )
C              DOUBLE PRECISION      T
C
C              INTEGER               DEG
C              INTEGER               I
C
C              DATA                  COEFFS / 1.D0,   3.D0,
C             .                               0.5D0,  1.D0,
C             .                               0.5D0, -1.D0,
C             .                               1.D0          /
C
C              T = 1.D0
C              DEG = 6
C
C              CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
C
C              DO I= 1, NDERIV + 1
C                 WRITE(*,*) 'P = ', P(I)
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         P =    6.0000000000000000
C         P =    10.000000000000000
C         P =    23.000000000000000
C         P =    78.000000000000000
C
C
C$ Restrictions
C
C     1)  Depending on the coefficients the user should be careful when
C         taking high order derivatives. As the example shows, these
C         can get big in a hurry. In general the coefficients of the
C         derivatives of a polynomial grow at a rate greater
C         than N! (N factorial).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 16-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Updated the header to comply with NAIF standard. Added
C        full code example. Updated Exception #2 to properly describe
C        the routine's behavior.
C
C-    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG)
C
C        Replaced the function calls to DFLOAT with standard conforming
C        calls to DBLE.
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
C     compute a polynomial and its derivatives
C
C-&


C$ Revisions
C
C-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
C
C        The error free specification was added as well as notes
C        on exceptional degree or derivative requests.
C
C-&


C
C     Local variables
C
      INTEGER          I
      INTEGER          K
      DOUBLE PRECISION SCALE


      IF ( NDERIV .LT. 0 ) THEN
         RETURN
      END IF

C
C     The following loops may not look like much, but they compute
C     P(T), P'(T), P''(T), ... etc.
C
C     To see why, recall that if A_0 through A_N are the coefficients
C     of a polynomial, then P(t) can be computed from the sequence
C     of polynomials given by:
C
C        P_0(t) = 0
C        P_1(t) = t*P_0(t)     +   A_N
C        P_2(t) = t*P_1(t)     +   A_[N-1]
C               .
C               .
C               .
C        P_n(t) = t*P_[n-1](t) +   A_0
C
C     The final polynomial in this list is in fact P(t).  From this
C     it follows that P'(t) is given by P_n'(t).  But
C
C        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t)
C
C     and
C
C        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t)
C                    .
C                    .
C                    .
C        P_2'(t)     = t*P_1'(t)     +   P_1(t)
C        P_1'(t)     = t*P_0'(t)     +   P_0(t)
C        P_0'(t)     = 0
C
C     Rearranging the sequence we have a recursive method
C     for computing P'(t).  At the i'th stage we require only the i-1st
C     polynomials P_[i-1] and P_[i-1]' .
C
C        P_0'(t)     = 0
C        P_1'(t)     = t*P_0'(t)     +   P_0(t)
C        P_2'(t)     = t*P_1'(t)     +   P_1(t)
C                    .
C                    .
C                    .
C        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t)
C        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t)
C
C
C     Similarly,
C
C        P_0''(t)     = 0
C        P_1''(t)     = t*P_0''(t)     +   2*P_0'(t)
C        P_2''(t)     = t*P_1''(t)     +   2*P_1'(t)
C                     .
C                     .
C                     .
C        P_[n-1]''(t) = t*P_[n-2]''(t) +   2*P_[n-2]'(t)
C
C
C
C        P_0'''(t)     = 0
C        P_1'''(t)     = t*P_0'''(t)     +   3*P_0''(t)
C        P_2'''(t)     = t*P_1'''(t)     +   3*P_1''(t)
C                      .
C                      .
C                      .
C        P_[n-1]'''(t) = t*P_[n-2]'''(t) +   3*P_[n-2]''(t)
C
C     Thus if P(I) contains the k'th iterations of the i'th derivative
C     computation of P and P(I-1) contains the k'th iteration of the
C     i-1st derivative of P then, t*P(I) + I*P(I-1) is the value of the
C     k+1st iteration of the computation of the i'th derivative of
C     P. This can then be stored in P(I).
C
C     If in a loop we compute in-place k'th iteration of the
C     I'th derivative before we perform the in-place k'th iteration
C     of the I-1st and I-2cnd derivative, then the k-1'th values
C     of the I-1st and I-2cnd will not be altered and will be available
C     for the computation of the k'th iteration of the I-1st
C     derivative.  This observation gives us an economical way to
C     compute all of the derivatives (including the zero'th derivative)
C     in place.  We simply compute the iterates of the high order
C     derivatives first.

C
C     Initialize the polynomial value (and all of its derivatives) to be
C     zero.
C
      DO I = 0, NDERIV
         P(I) = 0.0D0
      END DO


C
C     Set up the loop "counters" (they count backwards) for the first
C     pass through the loop.
C
      K     = DEG
      I     = NDERIV
      SCALE = DBLE ( NDERIV )

      DO WHILE ( K .GE. 0 )

         DO WHILE ( I .GT. 0 )
            P(I)  = T*P(I) + SCALE*P(I-1)
            SCALE = SCALE - 1
            I     = I - 1
         END DO

         P(0)  = T*P(0) + COEFFS(K)
         K     = K - 1
         I     = NDERIV
         SCALE = DBLE( NDERIV )
      END DO

      RETURN
      END

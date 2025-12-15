C$Procedure CHBVAL ( Value of a Chebyshev polynomial expansion )

      SUBROUTINE CHBVAL ( CP, DEGP, X2S, X, P )

C$ Abstract
C
C     Return the value of a polynomial evaluated at the input X using
C     the coefficients for the Chebyshev expansion of the polynomial.
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

      DOUBLE PRECISION   CP  ( * )
      INTEGER            DEGP
      DOUBLE PRECISION   X2S ( 2 )
      DOUBLE PRECISION   X
      DOUBLE PRECISION   P

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CP         I   DEGP+1 Chebyshev polynomial coefficients.
C     DEGP       I   Degree of polynomial.
C     X2S        I   Transformation parameters of polynomial.
C     X          I   Value for which the polynomial is to be evaluated.
C     P          O   Value of the polynomial at X.
C
C$ Detailed_Input
C
C     CP       is an array of coefficients a polynomial with respect
C              to the Chebyshev basis. The polynomial to be
C              evaluated is assumed to be of the form:
C
C                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C              where T(I,S) is the I'th Chebyshev polynomial
C              evaluated at a number S whose double precision
C              value lies between -1 and 1. The value of S is
C              computed from the input variables X2S(1), X2S(2)
C              and X.
C
C     DEGP     is the degree of the Chebyshev polynomial to be
C              evaluated.
C
C     X2S      is an array of two parameters. These parameters are
C              used to transform the domain of the input variable X
C              into the standard domain of the Chebyshev polynomial.
C              X2S(1) should be a reference point in the domain of X;
C              X2S(2) should be the radius by which points are
C              allowed to deviate from the reference point and while
C              remaining within the domain of X. The value of
C              X is transformed into the value S given by
C
C                 S = ( X - X2S(1) ) / X2S(2)
C
C              Typically X2S(1) is the midpoint of the interval over
C              which X is allowed to vary and X2S(2) is the radius of
C              the interval.
C
C              The main reason for doing this is that a Chebyshev
C              expansion is usually fit to data over a span
C              from A to B where A and B are not -1 and 1
C              respectively. Thus to get the "best fit" the
C              data was transformed to the interval [-1,1] and
C              coefficients generated. These coefficients are
C              not rescaled to the interval of the data so that
C              the numerical "robustness" of the Chebyshev fit will
C              not be lost. Consequently, when the "best fitting"
C              polynomial needs to be evaluated at an intermediate
C              point, the point of evaluation must be transformed
C              in the same way that the generating points were
C              transformed.
C
C     X        is the value for which the polynomial is to be
C              evaluated.
C
C$ Detailed_Output
C
C     P        is the value of the polynomial to be evaluated. It
C              is given by
C
C                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C              where T(I,S) is the I'th Chebyshev polynomial
C              evaluated  at a number S = ( X - X2S(1) )/X2S(2)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  No tests are performed for exceptional values (DEGP negative,
C         etc.). This routine is expected to be used at a low level in
C         ephemeris evaluations. For that reason it has been elected as
C         a routine that will not participate in error handling.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the value P given by
C
C        CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                             + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C     where
C
C        S  =  ( X - X2S(1) ) / X2S(2)
C
C     and
C
C        T(I,S) is the I'th Chebyshev polynomial of the first kind
C        evaluated at S.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Depending upon the user's needs, there are 3 routines
C        available for evaluating Chebyshev polynomials.
C
C           CHBVAL   for evaluating a Chebyshev polynomial when no
C                    derivatives are desired.
C
C           CHBINT   for evaluating a Chebyshev polynomial and its
C                    first derivative.
C
C           CHBDER   for evaluating a Chebyshev polynomial and a user
C                    or application dependent number of derivatives.
C
C        Of these 3 the one most commonly employed by SPICE software
C        is CHBINT as it is used to interpolate ephemeris state
C        vectors; this requires the evaluation of a polynomial
C        and its derivative. When no derivatives are desired one
C        should use CHBVAL, or when more than one or an unknown
C        number of derivatives are desired one should use CHBDER.
C
C        The code example below illustrates how this routine might
C        be used to obtain points for plotting a polynomial.
C
C
C        Example code begins here.
C
C
C              PROGRAM CHBVAL_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      CP     (7)
C              DOUBLE PRECISION      X
C              DOUBLE PRECISION      P
C              DOUBLE PRECISION      X2S    (2)
C
C              INTEGER               DEGP
C              INTEGER               I
C
C        C
C        C     Set the coefficients of the polynomial and its
C        C     transformation parameters
C        C
C              DATA                  CP     / 1.D0,  3.D0,  0.5D0,
C             .                               1.D0,  0.5D0, -1.D0,
C             .                               1.D0               /
C              DATA                  X2S    / 0.5D0, 3.D0 /
C
C              DEGP   = 6
C              X      = 1.D0
C
C              CALL CHBVAL ( CP, DEGP, X2S, X, P )
C
C              WRITE(*,'(A,F10.6)')
C             .        'Value of the polynomial at X=1: ', P
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Value of the polynomial at X=1:  -0.340878
C
C
C$ Restrictions
C
C     1)  One needs to be careful that the value
C
C            (X-X2S(1)) / X2S(2)
C
C         lies between -1 and 1. Otherwise, the routine may fail
C         spectacularly (for example with a floating point overflow).
C
C$ Literature_References
C
C     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
C          "Numerical Recipes -- The Art of Scientific Computing,"
C          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
C          Formula," p 161, Cambridge University Press, 1986.
C
C     [2]  T. Rivlin, "The Chebyshev Polynomials," Wisley, 1974.
C
C     [3]  R. Weast and S. Selby, "CRC Handbook of Tables for
C          Mathematics," 4th Edition, CRC Press, 1976.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 16-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Updated the header to comply with NAIF standard. Added
C        full code example.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
C
C-&


C$ Index_Entries
C
C     value of a chebyshev polynomial expansion
C
C-&


C$ Revisions
C
C-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
C
C        The Error free specification was added to the routine as
C        well as an explanation for this designation. Examples added.
C
C-&


C
C     Local variables
C
      INTEGER          J
      DOUBLE PRECISION W(3)
      DOUBLE PRECISION S
      DOUBLE PRECISION S2

C
C     Transform X to S and initialize temporary variables.
C
      S     = (X-X2S(1)) / X2S(2)
      S2    = 2.0D0 * S
      J     = DEGP  + 1
      W(1)  = 0.D0
      W(2)  = 0.D0

C
C     Evaluate the polynomial using recursion.
C
      DO WHILE ( J .GT. 1 )
         W(3) = W (2)
         W(2) = W (1)
         W(1) = CP(J) + (S2*W(2) - W(3))
         J    = J - 1
      END DO

      P     = (S*W(1)-W(2)) + CP(1)

      RETURN
      END

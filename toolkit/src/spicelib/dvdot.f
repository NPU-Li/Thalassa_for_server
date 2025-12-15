C$Procedure DVDOT  ( Derivative of Vector Dot Product, 3-D )

      DOUBLE PRECISION FUNCTION DVDOT ( S1, S2 )

C$ Abstract
C
C     Compute the derivative of the dot product of two double
C     precision position vectors.
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
C     DERIVATIVE
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   S1 ( 6 )
      DOUBLE PRECISION   S2 ( 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1         I   First state vector in the dot product.
C     S2         I   Second state vector in the dot product.
C
C     The function returns the derivative of the dot product <S1,S2>
C
C$ Detailed_Input
C
C     S1       is any state vector. The components are in order
C              (x, y, z, dx/dt, dy/dt, dz/dt )
C
C     S2       is any state vector.
C
C$ Detailed_Output
C
C     The function returns the derivative of the dot product of the
C     position portions of the two state vectors S1 and S2.
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
C     Given two state vectors S1 and S2 made up of position and
C     velocity components (P1,V1) and (P2,V2) respectively,
C     DVDOT calculates the derivative of the dot product of P1 and P2,
C     i.e. the time derivative
C
C           d
C           -- < P1, P2 > = < V1, P2 > + < P1, V2 >
C           dt
C
C     where <,> denotes the dot product operation.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that given two state vectors whose position components
C        are unit vectors, and that we need to compute the rate of
C        change of the angle between the two vectors.
C
C        Example code begins here.
C
C
C              PROGRAM DVDOT_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DVDOT
C              DOUBLE PRECISION      VDOT
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      DTHETA
C              DOUBLE PRECISION      S1     (6)
C              DOUBLE PRECISION      S2     (6)
C
C        C
C        C     Define the two state vectors whose position
C        C     components are unit vectors.
C        C
C              DATA                  S1 /
C             .         7.2459D-01,  6.6274D-01, 1.8910D-01,
C             .        -1.5990D-06,  1.6551D-06, 7.4873D-07 /
C
C              DATA                  S2 /
C             .         8.4841D-01, -4.7790D-01, -2.2764D-01,
C             .         1.0951D-07,  1.0695D-07,  4.8468D-08 /
C
C        C
C        C     We know that the Cosine of the angle THETA between them
C        C     is given by
C        C
C        C        cos(THETA) = VDOT(S1,S2)
C        C
C        C     Thus by the chain rule, the derivative of the angle is
C        C     given by:
C        C
C        C        sin(THETA) dTHETA/dt = DVDOT(S1,S2)
C        C
C        C     Thus for values of THETA away from zero we can compute
C        C     dTHETA/dt as:
C        C
C              DTHETA = DVDOT(S1,S2) / SQRT( 1 - VDOT(S1,S2)**2 )
C
C              WRITE(*,'(A,F18.12)') 'Rate of change of angle '
C             . //                   'between S1 and S2:', DTHETA
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Rate of change of angle between S1 and S2:   -0.000002232415
C
C
C        Note that if the position components of S1 and S2 are parallel,
C        the derivative of the  angle between the positions does not
C        exist. Any code that computes the derivative of the angle
C        between two position vectors should account for the case
C        when the position components are parallel.
C
C$ Restrictions
C
C     1)  The user is responsible for determining that the states S1 and
C         S2 are not so large as to cause numeric overflow. In most
C         cases this won't present a problem.
C
C     2)  An implicit assumption exists that S1 and S2 are specified in
C         the same reference frame. If this is not the case, the
C         numerical result has no meaning.
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
C        code examples. Added entry #2 to $Restrictions.
C
C-    SPICELIB Version 1.0.0, 18-MAY-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     Compute the derivative of a dot product
C
C-&


      DVDOT = S1(1)*S2(4) + S1(2)*S2(5) + S1(3)*S2(6)
     .      + S1(4)*S2(1) + S1(5)*S2(2) + S1(6)*S2(3)

      RETURN
      END

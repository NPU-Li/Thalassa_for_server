C$Procedure ZZDNPT ( Derivative of ellipsoid near point )
 
      SUBROUTINE ZZDNPT ( STATE, NEARP, A, B, C, DNEAR, DALT, FOUND )
 
C$ Abstract
C
C     Compute the velocity of an ellipsoid surface point nearest to a 
C     specified position.
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
C     ELLIPSOID
C     GEOMETRY
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      NEARP ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DNEAR ( 3 )
      DOUBLE PRECISION      DALT 
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   State of an object in body-fixed coordinates.
C     NEARP      I   Near point on ellipsoid.
C     A          I   Length of semi-axis parallel to x-axis.
C     B          I   Length of semi-axis parallel to y-axis.
C     C          I   Length on semi-axis parallel to z-axis.
C     DNEAR      O   Derivative of the nearest point on the ellipsoid.
C     DALT       O   Derivative of altitude.
C     FOUND      O   Tells whether DNEAR is degenerate.
C
C$ Detailed_Input
C
C     STATE    is a 6-vector giving the position and velocity of some
C              object in the body-fixed coordinates of the ellipsoid.
C
C     NEARP    the calculated/derived coordinates of the point on the
C              ellipsoid closest to STATE.
C
C              In body-fixed coordinates, the semi-axes of the ellipsoid
C              are aligned with the X, Y, and Z-axes of the coordinate
C              system.
C
C     A        is the length of the semi-axis of the ellipsoid that is
C              parallel to the X-axis of the body-fixed coordinate
C              system.
C
C     B        is the length of the semi-axis of the ellipsoid that is
C              parallel to the Y-axis of the body-fixed coordinate
C              system.
C
C     C        is the length of the semi-axis of the ellipsoid that is
C              parallel to the Z-axis of the body-fixed coordinate
C              system.
C
C$ Detailed_Output
C
C     DNEAR    is the 3-vector giving the velocity in
C              body-fixed coordinates of the point on the ellipsoid,
C              closest to the object whose position and velocity are
C              represented by STATE.
C
C              While the position component of DNEAR is always
C              meaningful, the velocity component of DNEAR will be
C              meaningless if FOUND if .FALSE. (See the discussion of
C              the meaning of FOUND below.)
C
C     DALT     is rate of change of the altitude, i.e. the range rate.
C
C              Note that the rate of change of altitude is meaningful if
C              and only if FOUND is .TRUE. (See the discussion of the
C              meaning of FOUND below.)
C
C     FOUND    is a logical flag indicating whether or not the velocity
C              portion of DNEAR is meaningful. If the velocity portion
C              of DNEAR is meaningful FOUND will be returned with a
C              value of .TRUE. Under very rare circumstance the velocity
C              of the near point is undefined. Under these circumstances
C              FOUND will be returned with the value .FALSE.
C
C              FOUND can be .FALSE. only for states whose position
C              components are inside the ellipsoid and then only at
C              points on a special surface contained inside the
C              ellipsoid called the focal set of the ellipsoid.
C
C              A point in the interior is on this special surface only
C              if there are two or more points on the ellipsoid that are
C              closest to it. The origin is such a point and the only
C              such point if the ellipsoid is a sphere. For
C              non-spheroidal ellipsoids the focal set contains small
C              portions of the planes of symmetry of the ellipsoid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the axes are non-positive, an error is signaled by a
C         routine in the call tree of this routine.
C
C     2)  If an object is passing through the interior of an ellipsoid
C         there are points at which there is more than 1 point on
C         the ellipsoid that is closest to the object.  At these
C         points the velocity of the near point is undefined. (See
C         the description of the output variable FOUND).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes DNEAR from STATE. In addition it returns
C     the range rate of altitude.
C
C     Note that this routine can compute DNEAR for STATES outside,
C     on, or inside the ellipsoid.  However, DNEAR and derivative
C     of altitude do not exist for a "small" set of STATES in the
C     interior of the ellipsoid. See the discussion of FOUND above
C     for a description of this set of points.
C
C$ Examples
C
C     None.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-SEP-2021 (EDW)
C
C-&

C$ Index_Entries
C
C     Velocity of the nearest point on an ellipsoid
C     Rate of change of the altitude over an ellipsoid
C     Derivative of altitude over an ellipsoid
C     Range rate of altitude over an ellipsoid
C     Velocity of a ground track
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VTMV
 
C
C     Local Variables
C
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DTERM  ( 3 )
      DOUBLE PRECISION      GRAD   ( 3 )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LENGTH
      DOUBLE PRECISION      LPRIME
      DOUBLE PRECISION      NORML  ( 3 )
      DOUBLE PRECISION      TEMP   ( 3 )
      DOUBLE PRECISION      ZENITH ( 3 )
 
 
      DOUBLE PRECISION      GRADM  ( 3, 3 )
      DOUBLE PRECISION      M      ( 3, 3 )
 
      INTEGER               I
C
C     Saved Variables
C
      SAVE                  GRADM
      SAVE                  M
 
C
C     Initial Values
C
      DATA   GRADM / 1.0D0, 0.0D0, 0.0D0,
     .               0.0D0, 1.0D0, 0.0D0,
     .               0.0D0, 0.0D0, 1.0D0  /
 
      DATA   M     / 1.0D0, 0.0D0, 0.0D0,
     .               0.0D0, 1.0D0, 0.0D0,
     .               0.0D0, 0.0D0, 1.0D0  /
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZDNPT' )
C
C     Until we have reason to believe otherwise, we set FOUND to TRUE.
C
      FOUND = .TRUE.

C
C     Now for the work of this routine.  We need to compute the
C     velocity component of NEARP.
C
C     In all of the discussions below we let <,> stand for the
C     dot product (inner product).
C
C     Let P be the position (first three components) of STATE
C     and let N be the position (first three components) of NEARP.
C
C     The surface of the ellipsoid is described as the level set
C     f(x,y,z) = 1 for the function f defined by
C
C                    x**2 + y**2 + z**2
C         f(x,y,z) = ----   ----   ----
C                    A**2   B**2   C**2
C
C     Let GRAD be the "half" gradient of f,
C
C         (NABLA * f)/2
C
C     with NABLA the operator 
C
C         (Dx, Dy, Dz)
C
C     ("D" indicating partial derivative). Then for some L
C
C           N + L * GRAD = P                         ( 1 )
C
C     Solve for L
C
C           L * GRAD = P - N
C
C      Apply <,GRAD> to LHS and RHS of expression
C
C           <L * GRAD, GRAD> = < P - N, GRAD >
C
C           L * < GRAD, GRAD > = < P - N, GRAD >
C
C     So that
C                < P - N, GRAD >
C           L =  --------------
C                < GRAD , GRAD >
C
C      Recall
C
C            < X, X > = |X|**2 , X in Rn, R3 in this case
C
C                          GRAD
C             =  < P - N, ------ >  /  | GRAD |
C                         |GRAD|
C
C     Since GRAD is computed at a point on the level set f(x,y,z) = 1
C     we don't have to worry about the magnitude of |GRAD| being
C     so small that underflow can occur (mostly).
C
C     Note that the half gradient of f can be computed by simple
C     vector multiplication
C
C                       [ 1/A**2    0       0    ] [ x ]
C        GRAD(x,y,z)  = |   0     1/B**2    0    | | y |
C                       [   0       0     1/C**2 ] [ z ]
C
C     We call the matrix above GRADM.  The correct off
C     diagonal values have been established in the data statement
C     following the declaration section of this routine.
C
 
      GRADM(1,1) = 1.0D0/(A*A)
      GRADM(2,2) = 1.0D0/(B*B)
      GRADM(3,3) = 1.0D0/(C*C)
 
      CALL VSUB  ( STATE,  NEARP, ZENITH )
 
      CALL MXV   ( GRADM,  NEARP,    GRAD   )
      CALL UNORM ( GRAD,   NORML,    LENGTH )
 
      L  = VDOT  ( ZENITH, NORML ) / LENGTH
 
C
C     We can rewrite equation (1) as
C
C        P = N + L * GRADM * N
C
C     from this it follows that
C
C        P' =  N' + L' * GRADM * N
C                 + L  * GRADM * N'
C
C           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N
C
C           = ( IDENT + L*GRADM ) * N'   + L' * GRAD
C
C     where IDENT is the 3x3 identity matrix.
C
C     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided
C     of course that all of the diagonal entries are non-zero).
C
C     If we multiply both sides of the equation above by M
C     we have
C
C
C        M*P'  = N'  + L'* M * GRAD                      ( 2 )
C
C
C     Recall now that N' is orthogonal to GRAD (N' lies in the
C     tangent plane to the ellipsoid at N and GRAD is normal
C     to this tangent plane).  Thus
C
C        < GRAD, M*P' > = L' < GRAD, M * GRAD >
C
C     and
C
C                 < GRAD, M*P'   >
C        L'   =   -----------------
C                 < GRAD, M*GRAD >
C
C
C             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD )
C
C     Let's pause now to compute M and L'.
C
C        This is where things could go bad.  M might not exist (which
C        indicates STATE is on the focal set of the ellipsoid).  In
C        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is
C        zero.  This turns out not to be possible.  However, the
C        demonstration of this fact requires delving into the details
C        of how N was computed by NEARPT.  Rather than spending a
C        lot of time explaining the details we will make an
C        unnecessary but inexpensive check that we don't divide by
C        zero when computing L'.
C

      DO I = 1, 3
         DTERM(I) = 1.0D0 + L*GRADM(I,I)
      END DO
 
      DO I = 1, 3
 
         IF ( DTERM(I) .NE. 0.0D0 ) THEN
            M(I,I) = 1.0D0 / DTERM(I)
         ELSE
            FOUND = .FALSE.
            CALL CHKOUT ( 'ZZDNPT' )
            RETURN
         END IF
 
      END DO

      DENOM = VTMV ( GRAD, M, GRAD )
 
      IF ( DENOM .EQ. 0.0D0 ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'ZZDNPT' )
         RETURN
      END IF

      LPRIME = VTMV( GRAD, M, STATE(4) ) / DENOM
 
C
C     Now that we have L' we can easily compute N'. Rewriting
C     equation (2) from above we have.
C
C        N'  = M * ( P' - L'*GRAD )
C
      CALL VLCOM ( 1.0D0, STATE(4), -LPRIME, GRAD, TEMP     )
      CALL MXV   ( M,     TEMP,                    DNEAR )
 

C
C     Only one thing left to do. Compute the derivative
C     of the altitude ALT. This quantity equals the range rate of the
C     vector from the near point, N, to the observer object, P.
C
C                               ^
C     Range rate in R3 equals < r, v >. In this case, NORML defines 
C     the unit vector from N to P. The velocity of P with respect 
C     to N,
C
C        V = d(P - N) = P' - N'
C            --
C            dt
C
C     But as we discussed earlier, N' is orthogonal to NORML (GRAD).
C     Thus
C
C          ^
C        < r, v > = < NORML, P' - N' >
C                 = < NORML, P'> - < NORML, N'>
C                 = < NORML, P'>
C
C        dALT/dt = < NORML, P'>
C
C     Given P' = STATE(4,5,6)
C
      DALT = VDOT ( NORML, STATE(4) )
 
      CALL CHKOUT ( 'ZZDNPT' )

      RETURN
      END

C$Procedure DHFA ( Time derivative of half angle )

      DOUBLE PRECISION FUNCTION DHFA ( STATE, BODYR)

C$ Abstract
C
C     Calculate the value of the time derivative of the
C     half angle of a spherical body given a state vector
C     STATE and body radius BODYR.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      STATE (6)
      DOUBLE PRECISION      BODYR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   SPICE state vector
C     BODYR      I   Radius of body
C
C$ Detailed_Input
C
C     STATE    is the state vector of a target body as seen from an
C              observer.
C
C     BODYR    is the radius of the target body observed from the
C              position in STATE; the target body assumed as a sphere.
C
C$ Detailed_Output
C
C     The function returns the double precision value of the time
C     derivative of the half angle of a spherical body in radians
C     per second.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the radius of the body BODYR is less than zero, the error
C         SPICE(BADRADIUS) is signaled.
C
C     2)  If the position component of STATE equals the zero vector, the
C         error SPICE(DEGENERATECASE) is signaled.
C
C     3)  If the body radius exceeds the distance from the body to the
C         observer, the error SPICE(BADGEOMETRY) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In this discussion, the notation
C
C        < V1, V2 >
C
C     indicates the dot product of vectors V1 and V2.
C
C     The expression
C
C                     body_radius
C        sin(ALPHA) = -----------                                    (1)
C                       range
C
C     describes the half angle (ALPHA) of a spherical body, i.e. the
C     angular radius of the spherical body as viewed by an observer at
C     distance 'range'.
C
C     Solve for ALPHA
C
C                   -1  body_radius
C        ALPHA = sin  ( ----------- )                                (2)
C                         range
C
C     Take the derivative of ALPHA with respect to time
C
C        d                   1                   d    body_radius
C        --(ALPHA) =  --------------------- *    __ (----------- )   (3)
C        dt           1 -   body_radius  2   1/2 dt    range
C                  (      [ ----------- ]   )
C                            range
C
C        d              - body_radius             1      d
C        --(ALPHA) =  --------------------- *   ------ * __(range)   (4)
C        dt           1 -   body_radius  2  1/2      2   dt
C                  (      [ ----------- ]  )    range
C                            range
C
C     With
C                          _  _
C        d               < R, V >              -
C        -- ( range )  = -------- ,  range = ||R||                   (5)
C        dt                 -
C                         ||R||
C
C     Apply (5) to equation (4)
C                                                          _  _
C        d              - body_radius             1      < R, V >
C        --(ALPHA) =  --------------------- *   ------ *  --------   (6)
C        dt           1 -   body_radius  2  1/2     2     range
C                  (      [ ----------- ]  )    range
C                              range
C
C     Carry range through the denominator gives
C
C                                                  _  _
C        d              - body_radius            < R, V >
C        --(ALPHA) =  --------------------- *    --------            (7)
C        dt                 2            2  1/2        2
C                     (range - body_radius )      range
C
C     So since
C                        -    -         _  _
C          ^  -       <  R,   V >     < R, V >
C        < R, V >   =   ---        =  --------
C                        -              range
C                      ||R||
C
C                                                ^  _
C        d              - body_radius            < R, V >
C        --(ALPHA) =  --------------------- *    --------            (8)
C        dt                 2            2  1/2
C                     (range - body_radius )      range
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the half angle derivative at the approximate time
C        corresponding to a maximal angular separation between the
C        Earth and Moon as seen from the Sun, and two weeks later.
C
C        The two derivate values shall have similar magnitudes but
C        opposite signs.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: dhfa_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00010.tpc                  Planet orientation and
C                                            radii
C              naif0012.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00010.tpc',
C                                  'naif0012.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM DHFA_EX
C              IMPLICIT              NONE
C
C              INTEGER               DIM
C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      DHADT
C              DOUBLE PRECISION      RAD   (3)
C              DOUBLE PRECISION      STATE (6)
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 64 )
C
C              CHARACTER*(STRLEN)    BEGSTR
C
C
C              DOUBLE PRECISION      SPD
C              DOUBLE PRECISION      DHFA
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ('dhfa_ex1.tm')
C
C        C
C        C     An approximate time corresponding to a maximal angular
C        C     separation between the Earth and Moon as seen from the
C        C     Sun.
C        C
C              BEGSTR = '2007-DEC-17 04:04:46.935443 (TDB)'
C              CALL STR2ET( BEGSTR, ET )
C
C              CALL BODVRD ('SUN', 'RADII', 3, DIM, RAD )
C
C              CALL SPKEZR ('MOON', ET, 'J2000', 'NONE', 'SUN',
C             .              STATE, LT                         )
C
C        C
C        C     The derivative of the half angle at ET should have a
C        C     near-to maximal value as the Moon velocity vector points
C        C     either towards the Sun or away.
C        C
C              DHADT = DHFA( STATE, RAD(1) )
C              WRITE(*,*) 'Half angle derivative:'
C              WRITE(*,*) '   at begin time  : ', DHADT
C
C        C
C        C     Two weeks later the derivate should have a similar
C        C     magnitude but the opposite sign.
C        C
C              ET = SPD() * 14.D0 + ET
C
C              CALL SPKEZR ('MOON', ET, 'J2000', 'NONE', 'SUN',
C             .              STATE, LT                         )
C
C              DHADT = DHFA( STATE, RAD(1) )
C              WRITE(*,*) '   two weeks later: ', DHADT
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Half angle derivative:
C            at begin time  :   -2.5387993682459762E-011
C            two weeks later:    2.9436205837172777E-011
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-JUN-2020 (JDR)
C
C        Edited the header to comply with NAIF standard. Added problem
C        statement and meta-kernel to the example. Modified output to
C        comply with maximum line length of header comments.
C
C-    SPICELIB Version 1.0.1, 06-JUL-2009 (EDW)
C
C        Rename of the ZZDHA call to DHFA.
C
C-    SPICELIB Version 1.0.0, 10-FEB-2009 (EDW) (NJB)
C
C-&


C$ Index_Entries
C
C     time derivative of half angle
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      LOGICAL               RETURN
      LOGICAL               VZERO

      DOUBLE PRECISION      P     (3)
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RNGRAT
      DOUBLE PRECISION      BASE

      IF ( RETURN () ) THEN
         DHFA = 0.D0
         RETURN
      ELSE
         CALL CHKIN( 'DHFA' )
      END IF

C
C     A zero body radius (point object) returns a zero for the
C     derivative. A negative value indicates an error
C     the caller should diagnose.
C
      IF ( BODYR .EQ. 0 ) THEN

         DHFA = 0.D0
         CALL CHKOUT( 'DHFA' )
         RETURN

      ELSE IF ( BODYR .LT. 0 ) THEN

         DHFA = 0.D0
         CALL SETMSG( 'Non physical case. The input body radius '
     .      //        'has a negative value.'                      )
         CALL SIGERR( 'SPICE(BADRADIUS)'                           )
         CALL CHKOUT( 'DHFA'                                       )
         RETURN

      END IF

C
C     Normalize the position component of STATE. Store the unit vector
C     in P.
C
      CALL UNORM( STATE(1), P, R)

      IF ( VZERO(P) ) THEN

         DHFA = 0.D0
         CALL SETMSG( 'The position component of the input state '
     .      //        'vector equals the zero vector.'             )
         CALL SIGERR( 'SPICE(DEGENERATECASE)'                      )
         CALL CHKOUT( 'DHFA'                                       )
         RETURN

      END IF

C
C     Calculate the range rate.
C
      RNGRAT = VDOT( P, STATE(4) )

C
C     Confirm R > BODYR.
C
      BASE = R**2 - BODYR**2

      IF ( BASE .LE. 0 ) THEN

         DHFA = 0.D0
         CALL SETMSG( 'Invalid case. The body radius, #1, equals '
     .      //        'or exceeds the range to the target, #2.'    )
         CALL ERRDP ( '#1', BODYR                                  )
         CALL ERRDP ( '#2', R                                      )
         CALL SIGERR( 'SPICE(BADGEOMETRY)'                         )
         CALL CHKOUT( 'DHFA'                                       )
         RETURN

      END IF

C
C     Now we safely take the square root of BASE.
C
      BASE = DSQRT( BASE )
      DHFA = -(RNGRAT * BODYR)/(BASE * R)

      CALL CHKOUT( 'DHFA' )
      RETURN

      END

C$Procedure M2EUL ( Matrix to Euler angles )

      SUBROUTINE M2EUL (  R,  AXIS3,   AXIS2,   AXIS1,
     .                        ANGLE3,  ANGLE2,  ANGLE1  )

C$ Abstract
C
C     Factor a rotation matrix as a product of three rotations about
C     specified coordinate axes.
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
C     ROTATION
C
C$ Keywords
C
C     ANGLE
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      R      ( 3, 3 )

      INTEGER               AXIS3
      INTEGER               AXIS2
      INTEGER               AXIS1

      DOUBLE PRECISION      ANGLE3
      DOUBLE PRECISION      ANGLE2
      DOUBLE PRECISION      ANGLE1

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     R          I   A rotation matrix to be factored.
C     AXIS3,
C     AXIS2,
C     AXIS1      I   Numbers of third, second, and first rotation axes.
C     ANGLE3,
C     ANGLE2,
C     ANGLE1     O   Third, second, and first Euler angles, in radians.
C
C$ Detailed_Input
C
C     R        is a 3x3 rotation matrix that is to be factored as
C              a product of three rotations about a specified
C              coordinate axes. The angles of these rotations are
C              called "Euler angles."
C
C     AXIS3,
C     AXIS2,
C     AXIS1    are the indices of the rotation axes of the
C              "factor" rotations, whose product is R. R is
C              factored as
C
C                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
C                               AXIS3           AXIS2           AXIS1
C
C              The axis numbers must belong to the set {1, 2, 3}.
C              The second axis number MUST differ from the first
C              and third axis numbers.
C
C              See the $Particulars section below for details
C              concerning this notation.
C
C$ Detailed_Output
C
C     ANGLE3,
C     ANGLE2,
C     ANGLE1   are the Euler angles corresponding to the matrix
C              R and the axes specified by AXIS3, AXIS2, and
C              AXIS1. These angles satisfy the equality
C
C                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
C                               AXIS3           AXIS2           AXIS1
C
C
C              See the $Particulars section below for details
C              concerning this notation.
C
C              The range of ANGLE3 and ANGLE1 is (-pi, pi].
C
C              The range of ANGLE2 depends on the exact set of
C              axes used for the factorization. For
C              factorizations in which the first and third axes
C              are the same,
C
C                 R = [r]  [s]  [t] ,
C                        a    b    a
C
C              the range of ANGLE2 is [0, pi].
C
C              For factorizations in which the first and third
C              axes are different,
C
C                 R = [r]  [s]  [t] ,
C                        a    b    c
C
C              the range of ANGLE2 is [-pi/2, pi/2].
C
C              For rotations such that ANGLE3 and ANGLE1 are not
C              uniquely determined, ANGLE3 will always be set to
C              zero; ANGLE1 is then uniquely determined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any of AXIS3, AXIS2, or AXIS1 do not have values in
C
C            { 1, 2, 3 }
C
C         the error SPICE(BADAXISNUMBERS) is signaled.
C
C     2)  If AXIS2 is equal to AXIS3 or AXIS1, the error
C         SPICE(BADAXISNUMBERS) is signaled. An arbitrary rotation
C         matrix cannot be expressed using a sequence of Euler angles
C         unless the second rotation axis differs from the other two.
C
C     3)  If the input matrix R is not a rotation matrix, the error
C         SPICE(NOTAROTATION) is signaled.
C
C     4)  If ANGLE3 and ANGLE1 are not uniquely determined, ANGLE3
C         is set to zero, and ANGLE1 is determined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A word about notation: the symbol
C
C        [ x ]
C             i
C
C     indicates a coordinate system rotation of x radians about the
C     ith coordinate axis. To be specific, the symbol
C
C        [ x ]
C             1
C
C     indicates a coordinate system rotation of x radians about the
C     first, or x-, axis; the corresponding matrix is
C
C        .-                    -.
C        |  1      0       0    |
C        |                      |
C        |  0    cos(x)  sin(x) |
C        |                      |
C        |  0   -sin(x)  cos(x) |
C        `-                    -'
C
C     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
C     matrix, when applied to a vector, rotates the vector by -x
C     radians, not x radians. Applying the matrix to a vector yields
C     the vector's representation relative to the rotated coordinate
C     system.
C
C     The analogous rotation about the second, or y-, axis is
C     represented by
C
C        [ x ]
C             2
C
C     which symbolizes the matrix
C
C        .-                    -.
C        | cos(x)   0   -sin(x) |
C        |                      |
C        |  0       1      0    |
C        |                      |
C        | sin(x)   0    cos(x) |
C        `-                    -'
C
C     and the analogous rotation about the third, or z-, axis is
C     represented by
C
C        [ x ]
C             3
C
C     which symbolizes the matrix
C
C        .-                    -.
C        |  cos(x)  sin(x)   0  |
C        |                      |
C        | -sin(x)  cos(x)   0  |
C        |                      |
C        |  0        0       1  |
C        `-                    -'
C
C
C     The input matrix is assumed to be the product of three
C     rotation matrices, each one of the form
C
C        .-                    -.
C        |  1      0       0    |
C        |                      |
C        |  0    cos(r)  sin(r) |     (rotation of r radians about the
C        |                      |      x-axis),
C        |  0   -sin(r)  cos(r) |
C        `-                    -'
C
C
C        .-                    -.
C        | cos(s)   0   -sin(s) |
C        |                      |
C        |  0       1      0    |     (rotation of s radians about the
C        |                      |      y-axis),
C        | sin(s)   0    cos(s) |
C        `-                    -'
C
C     or
C
C        .-                    -.
C        |  cos(t)  sin(t)   0  |
C        |                      |
C        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
C        |                      |      z-axis),
C        |  0        0       1  |
C        `-                    -'
C
C     where the second rotation axis is not equal to the first or
C     third. Any rotation matrix can be factored as a sequence of
C     three such rotations, provided that this last criterion is met.
C
C     This routine is related to the SPICELIB routine EUL2M, which
C     produces a rotation matrix, given a sequence of Euler angles.
C     This routine is a `right inverse' of EUL2M:  the sequence of
C     calls
C
C        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
C       .                 ANGLE3,  ANGLE2,  ANGLE1     )
C
C        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
C       .                 AXIS3,   AXIS2,   AXIS1,   R )
C
C     preserves R, except for round-off error.
C
C
C     On the other hand, the sequence of calls
C
C        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
C       .                 AXIS3,   AXIS2,   AXIS1,   R )
C
C        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
C       .                 ANGLE3,  ANGLE2,  ANGLE1     )
C
C
C     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
C     out in the ranges that M2EUL's outputs are restricted to.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Conversion of instrument pointing from a matrix representation
C        to Euler angles:
C
C        Suppose we want to find camera pointing in alpha, delta, and
C        kappa, given the inertial-to-camera coordinate transformation
C
C
C        .-                                                         -.
C        |  0.491273796781358  0.508726203218642  0.706999085398824  |
C        |                                                           |
C        | -0.508726203218642 -0.491273796781358  0.706999085398824  |
C        |                                                           |
C        |  0.706999085398824 -0.706999085398824  0.017452406437284  |
C        `-                                                         -'
C
C
C        Find angles alpha, delta, kappa such that
C
C           TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
C                              3                 1                 3
C
C        Example code begins here.
C
C
C              PROGRAM M2EUL_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      HALFPI
C              DOUBLE PRECISION      TWOPI
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ALPHA
C              DOUBLE PRECISION      ANG1
C              DOUBLE PRECISION      ANG2
C              DOUBLE PRECISION      DELTA
C              DOUBLE PRECISION      KAPPA
C              DOUBLE PRECISION      TICAM  ( 3, 3 )
C
C
C              DATA TICAM /  0.491273796781358D0,
C             .             -0.508726203218642D0,
C             .              0.706999085398824D0,
C             .              0.508726203218642D0,
C             .             -0.491273796781358D0,
C             .             -0.706999085398824D0,
C             .              0.706999085398824D0,
C             .              0.706999085398824D0,
C             .              0.017452406437284D0  /
C
C
C              CALL M2EUL ( TICAM, 3, 1, 3, KAPPA, ANG2, ANG1 )
C
C              DELTA = HALFPI() - ANG2
C              ALPHA = ANG1     - HALFPI()
C
C              IF ( KAPPA .LT. 0.D0 ) THEN
C                 KAPPA = KAPPA + TWOPI()
C              END IF
C
C              IF ( ALPHA .LT. 0.D0 ) THEN
C                 ALPHA = ALPHA + TWOPI()
C              END IF
C
C              WRITE (*,'(A,F24.14)') 'Alpha (deg): ', DPR() * ALPHA
C              WRITE (*,'(A,F24.14)') 'Delta (deg): ', DPR() * DELTA
C              WRITE (*,'(A,F24.14)') 'Kappa (deg): ', DPR() * KAPPA
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Alpha (deg):       315.00000000000000
C        Delta (deg):         1.00000000000003
C        Kappa (deg):        45.00000000000000
C
C
C     2) Conversion of instrument pointing angles from a non-J2000,
C        not necessarily inertial frame to J2000-relative RA, Dec,
C        and Twist.
C
C        Suppose that we have orientation for the CASSINI Narrow Angle
C        Camera (NAC) frame expressed as
C
C           [ gamma ]  [ beta ]  [ alpha ]
C                    1         2          3
C
C        with respect to the CASSINI spacecraft frame.
C
C        We want to express that orientation with respect to the J2000
C        frame as the sequence of rotations
C
C           [ twist ]  [ dec ]  [ ra ] .
C                    3        1       3
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: m2eul_ex2.tm
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
C              File name                      Contents
C              ---------                      --------
C              naif0010.tls                   Leapseconds
C              cas00145.tsc                   Cassini SCLK
C              cas_v40.tf                     Cassini frames
C              08052_08057ra.bc               Orientation for Cassini
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0010.tls'
C                                  'cas00145.tsc'
C                                  'cas_v40.tf'
C                                  '08052_08057ra.bc')
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM M2EUL_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      HALFPI
C              DOUBLE PRECISION      RPD
C              DOUBLE PRECISION      TWOPI
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   =  'm2eul_ex2.tm' )
C
C              DOUBLE PRECISION      ALPHA
C              PARAMETER           ( ALPHA =  89.9148D0   )
C
C              DOUBLE PRECISION      BETA
C              PARAMETER           ( BETA  = -0.03300D0   )
C
C              DOUBLE PRECISION      GAMMA
C              PARAMETER           ( GAMMA = -90.009796D0 )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      RA
C              DOUBLE PRECISION      ANG1
C              DOUBLE PRECISION      ANG2
C              DOUBLE PRECISION      DEC
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      INST2J ( 3, 3 )
C              DOUBLE PRECISION      INST2S ( 3, 3 )
C              DOUBLE PRECISION      S2J    ( 3, 3 )
C              DOUBLE PRECISION      TWIST
C
C        C
C        C     Load the kernels.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     First, we use subroutine EUL2M to form the
C        C     transformation from instrument coordinates to
C        C     CASSINI spacecraft frame.
C        C
C              CALL EUL2M ( GAMMA * RPD(), BETA * RPD(), ALPHA * RPD(),
C             .             1,             2,            3,    INST2S )
C
C        C
C        C     Now we compute the transformation from CASSINI
C        C     spacecraft frame to J2000, at a given time.
C        C
C              CALL STR2ET ( '2008 Feb 25', ET )
C              CALL PXFORM ( 'CASSINI_SC_COORD', 'J2000', ET, S2J )
C
C        C
C        C     Next, we form the transformation from instrument
C        C     coordinates to J2000 frame.
C        C
C              CALL MXM ( S2J, INST2S, INST2J )
C
C        C
C        C     Finally, we express INST2J using the desired Euler
C        C     angles.
C        C
C              CALL M2EUL ( INST2J, 3, 1, 3, TWIST, ANG1, ANG2 )
C
C              RA   =  ANG2 - HALFPI()
C              DEC  =  HALFPI() - ANG1
C
C        C
C        C     If we wish to make sure that RA, DEC, and TWIST are in
C        C     the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
C        C     respectively, we may add the code
C        C
C              IF ( RA .LT. 0.D0 ) THEN
C                 RA = RA + TWOPI()
C              END IF
C
C              IF ( TWIST .LT. 0.D0 ) THEN
C                 TWIST = TWIST + TWOPI()
C              END IF
C
C        C
C        C     Now RA, DEC, and TWIST express the instrument pointing
C        C     as RA, Dec, and Twist, relative to the J2000 system.
C        C
C              WRITE (*,'(A,F24.14)') 'RA    (deg): ', DPR() * RA
C              WRITE (*,'(A,F24.14)') 'Dec   (deg): ', DPR() * DEC
C              WRITE (*,'(A,F24.14)') 'Twist (deg): ', DPR() * TWIST
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        RA    (deg):        83.77802387778848
C        Dec   (deg):       -14.92925498590898
C        Twist (deg):       294.55732942050986
C
C
C        Note:  more than one definition of RA, Dec, and Twist is
C        extant. Before using this example in an application, check
C        that the definition given here is consistent with that used
C        in your application.
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary entries from $Revisions section.
C
C        Added complete code examples from existing fragments.
C
C        Removed erroneous comments about behavior of ATAN2.
C
C-    SPICELIB Version 1.2.1, 21-DEC-2006 (NJB)
C
C        Error corrected in header example: input matrix
C        previously did not match shown outputs. Offending
C        example now includes complete program.
C
C-    SPICELIB Version 1.2.0, 15-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and MTXM calls. A short error message cited in
C        the $Exceptions section of the header failed to match
C        the actual short message used; this has been corrected.
C
C-    SPICELIB Version 1.1.2, 13-OCT-2004 (NJB)
C
C        Fixed header typo.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
C
C        Header upgraded to describe notation in more detail. Argument
C        names were changed to describe the use of the arguments more
C        accurately. No change in functionality was made; the operation
C        of the routine is identical to that of the previous version.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     matrix to euler angles
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
C
C        Argument names were changed to describe the use of the
C        arguments more accurately. The axis and angle numbers
C        now decrease, rather than increase, from left to right.
C        The current names reflect the order of operator application
C        when the Euler angle rotations are applied to a vector: the
C        rightmost matrix
C
C           [ ANGLE1 ]
C                     AXIS1
C
C        is applied to the vector first, followed by
C
C           [ ANGLE2 ]
C                     AXIS2
C
C        and then
C
C           [ ANGLE3 ]
C                     AXIS3
C
C        Previously, the names reflected the order in which the Euler
C        angle matrices appear on the page, from left to right. This
C        naming convention was found to be a bit obtuse by a various
C        users.
C
C        No change in functionality was made; the operation of the
C        routine is identical to that of the previous version.
C
C        Also, the header was upgraded to describe the notation in more
C        detail. The symbol
C
C           [ x ]
C                i
C
C        is explained at mind-numbing length. An example was added
C        that shows a specific set of inputs and the resulting output
C        matrix.
C
C        The angle sequence notation was changed to be consistent with
C        Rotations required reading.
C
C          1-2-3  and  a-b-c
C
C        have been changed to
C
C          3-2-1  and  c-b-a.
C
C       Also, one `)' was changed to a `}'.
C
C       The phrase `first and third' was changed to `first or third'
C       in the $Particulars section, where the criterion for the
C       existence of an Euler angle factorization is stated.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               ISROT
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     NTOL and DETOL are used to determine whether R is a rotation
C     matrix.
C
C     NTOL is the tolerance for the norms of the columns of R.
C
C     DTOL is the tolerance for the determinant of a matrix whose
C     columns are the unitized columns of R.
C
C
      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL = 0.1D0 )

      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL = 0.1D0 )


C
C     Local variables
C
      DOUBLE PRECISION      CHANGE ( 3, 3 )
      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
      DOUBLE PRECISION      TMPROT ( 3, 3 )

      INTEGER               C
      INTEGER               I
      INTEGER               NEXT   ( 3 )

      LOGICAL               DEGEN

C
C     Saved variables
C
      SAVE                  NEXT

C
C     Initial values
C
      DATA  NEXT / 2, 3, 1 /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'M2EUL' )
      END IF

C
C     The first order of business is to screen out the goofy cases.
C
C     Make sure the axis numbers are all right:  They must belong to
C     the set {1, 2, 3}...
C
      IF (      (  ( AXIS3 .LT. 1 ) .OR. ( AXIS3 .GT. 3 )  )
     .     .OR. (  ( AXIS2 .LT. 1 ) .OR. ( AXIS2 .GT. 3 )  )
     .     .OR. (  ( AXIS1 .LT. 1 ) .OR. ( AXIS1 .GT. 3 )  )   ) THEN

         CALL SETMSG ( 'Axis numbers are #,  #,  #. ' )
         CALL ERRINT ( '#',  AXIS3                    )
         CALL ERRINT ( '#',  AXIS2                    )
         CALL ERRINT ( '#',  AXIS1                    )
         CALL SIGERR ( 'SPICE(BADAXISNUMBERS)'        )
         CALL CHKOUT ( 'M2EUL'                        )
         RETURN

C
C     ...and the second axis number must differ from its neighbors.
C
      ELSE IF ( ( AXIS3 .EQ. AXIS2 ) .OR. ( AXIS1 .EQ. AXIS2 ) ) THEN

         CALL SETMSG ( 'Middle axis matches neighbor: # # #.' )
         CALL ERRINT ( '#',  AXIS3                            )
         CALL ERRINT ( '#',  AXIS2                            )
         CALL ERRINT ( '#',  AXIS1                            )
         CALL SIGERR ( 'SPICE(BADAXISNUMBERS)'                )
         CALL CHKOUT ( 'M2EUL'                                )
         RETURN

C
C     R must be a rotation matrix, or we may as well forget it.
C
      ELSE IF (  .NOT. ISROT ( R, NTOL, DTOL )  ) THEN

         CALL SETMSG ( 'Input matrix is not a rotation.' )
         CALL SIGERR ( 'SPICE(NOTAROTATION)'             )
         CALL CHKOUT ( 'M2EUL'                           )
         RETURN

      END IF


C
C     AXIS3, AXIS2, AXIS1 and R have passed their tests at this
C     point.  We take the liberty of working with TMPROT, a version
C     of R that has unitized columns.
C
      DO I = 1, 3
         CALL VHAT ( R(1,I), TMPROT(1,I) )
      END DO

C
C     We now proceed to recover the promised Euler angles from
C     TMPROT.
C
C        The ideas behind our method are explained in excruciating
C        detail in the ROTATION required reading, so we'll be terse.
C        Nonetheless, a word of explanation is in order.
C
C        The sequence of rotation axes used for the factorization
C        belongs to one of two categories:  a-b-a or c-b-a.  We
C        wish to handle each of these cases in one shot, rather than
C        using different formulas for each sequence to recover the
C        Euler angles.
C
C        What we're going to do is use the Euler angle formula for the
C        3-1-3 factorization for all of the a-b-a sequences, and the
C        formula for the 3-2-1 factorization for all of the c-b-a
C        sequences.
C
C        How can we get away with this?  The Euler angle formulas for
C        each factorization are different!
C
C        Our trick is to apply a change-of-basis transformation to the
C        input matrix R.  For the a-b-a factorizations, we choose a new
C        basis such that a rotation of ANGLE3 radians about the basis
C        vector indexed by AXIS3 becomes a rotation of ANGLE3 radians
C        about the third coordinate axis, and such that a rotation of
C        ANGLE2 radians about the basis vector indexed by AXIS2 becomes
C        a rotation of ANGLE2 radians about the first coordinate axis.
C        So R can be factored as a 3-1-3 rotation relative to the new
C        basis, and the Euler angles we obtain are the exact ones we
C        require.
C
C        The c-b-a factorizations can be handled in an analogous
C        fashion.  We transform R to a basis where the original axis
C        sequence becomes a 3-2-1 sequence.  In some cases, the angles
C        we obtain will be the negatives of the angles we require.  This
C        will happen if and only if the ordered basis (here the e's are
C        the standard basis vectors)
C
C            { e        e        e      }
C               AXIS3    AXIS2    AXIS1
C
C        is not right-handed.  An easy test for this condition is that
C        AXIS2 is not the successor of AXIS3, where the ordering is
C        cyclic.
C


      IF ( AXIS3 .EQ. AXIS1 ) THEN

C
C        The axis order is a-b-a.  We're going to find a matrix CHANGE
C        such that
C
C                 T
C           CHANGE  R  CHANGE
C
C        gives us R in the a useful basis, that is, a basis in which
C        our original a-b-a rotation is a 3-1-3 rotation, but where the
C        rotation angles are unchanged. To achieve this pleasant
C        simplification, we set column 3 of CHANGE to to e(AXIS3),
C        column 1 of CHANGE to e(AXIS2), and column 2 of CHANGE to
C
C          (+/-) e(C),
C
C        (C is the remaining index) depending on whether
C        AXIS3-AXIS2-C is a right-handed sequence of axes:  if it
C        is, the sign is positive.  (Here e(1), e(2), e(3) are the
C        standard basis vectors.)
C
C        Determine the sign of our third basis vector, so that we can
C        ensure that our new basis is right-handed.  The variable NEXT
C        is just a little mapping that takes 1 to 2, 2 to 3, and 3 to
C        1.
C
         IF ( AXIS2 .EQ. NEXT(AXIS3) ) THEN
            SIGN =  1.D0
         ELSE
            SIGN = -1.D0
         END IF

C
C        Since the axis indices sum to 6,
C
         C = 6 - AXIS3 - AXIS2

C
C        Set up the entries of CHANGE:
C
         CALL CLEARD ( 9, CHANGE )

         CHANGE ( AXIS3, 3 ) =         1.D0
         CHANGE ( AXIS2, 1 ) =         1.D0
         CHANGE ( C,     2 ) =  SIGN * 1.D0

C
C        Transform TMPROT.
C
         CALL MXM  ( TMPROT,  CHANGE,  TMPMAT )
         CALL MTXM ( CHANGE,  TMPMAT,  TMPROT )

C
C        Now we're ready to find the Euler angles, using a
C        3-1-3 factorization.  In general, the matrix product
C
C           [ a1 ]   [ a2 ]   [ a3 ]
C                 3        1        3
C
C        has the form
C
C     +-                                                              -+
C     |         cos(a1)cos(a3)          cos(a1)sin(a3)  sin(a1)sin(a2) |
C     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 |
C     |                                                                |
C     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)  cos(a1)sin(a2) |
C     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 |
C     |                                                                |
C     |         sin(a2)sin(a3)         -sin(a2)cos(a3)         cos(a2) |
C     +-                                                              -+
C
C
C        but if a2 is 0 or pi, the product matrix reduces to
C
C
C     +-                                                              -+
C     |         cos(a1)cos(a3)          cos(a1)sin(a3)               0 |
C     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 |
C     |                                                                |
C     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)               0 |
C     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 |
C     |                                                                |
C     |                      0                       0         cos(a2) |
C     +-                                                              -+
C
C
C        In this case, a1 and a3 are not uniquely determined.  If we
C        arbitrarily set a1 to zero, we arrive at the matrix
C
C           +-                                         -+
C           |         cos(a3)         sin(a3)      0    |
C           | -cos(a2)sin(a3)  cos(a2)cos(a3)      0    |
C           |               0            0      cos(a2) |
C           +-                                         -+
C
C        We take care of this case first.  We test three conditions
C        that are mathematically equivalent, but may not be satisfied
C        simultaneously because of round-off:
C
C
         DEGEN =      (        (       TMPROT(1,3)    .EQ. 0.D0  )
     .                   .AND. (       TMPROT(2,3)    .EQ. 0.D0  )  )
     .           .OR. (        (       TMPROT(3,1)    .EQ. 0.D0  )
     .                   .AND. (       TMPROT(3,2)    .EQ. 0.D0  )  )
     .           .OR. (           ABS( TMPROT(3,3) )  .EQ. 1.D0     )


C
C        In the following block of code, we make use of the fact that
C
C           SIN ( ANGLE2 )   >  0
C                            -
C        in choosing the signs of the ATAN2 arguments correctly.
C
         IF ( DEGEN ) THEN

            ANGLE3 = 0.D0
            ANGLE2 = ACOS  (  TMPROT(3,3)                 )
            ANGLE1 = ATAN2 (  TMPROT(1,2),   TMPROT(1,1)  )

         ELSE
C
C           The normal case.
C
            ANGLE3 = ATAN2 (  TMPROT(1,3),   TMPROT(2,3)  )
            ANGLE2 = ACOS  (  TMPROT(3,3)                 )
            ANGLE1 = ATAN2 (  TMPROT(3,1),  -TMPROT(3,2)  )

         END IF



      ELSE

C
C        The axis order is c-b-a.  We're going to find a matrix CHANGE
C        such that
C
C                 T
C           CHANGE  R  CHANGE
C
C        gives us R in the a useful basis, that is, a basis in which
C        our original c-b-a rotation is a 3-2-1 rotation, but where the
C        rotation angles are unchanged, or at worst negated.  To
C        achieve this pleasant simplification, we set column 1 of
C        CHANGE to to e(AXIS3), column 2 of CHANGE to e(AXIS2), and
C        column 3 of CHANGE to
C
C          (+/-) e(AXIS1),
C
C        depending on whether AXIS3-AXIS2-AXIS1 is a right-handed
C        sequence of axes:  if it is, the sign is positive.  (Here
C        e(1), e(2), e(3) are the standard basis vectors.)
C
C        We must be cautious here, because if AXIS3-AXIS2-AXIS1 is a
C        right-handed sequence of axes, all of the rotation angles will
C        be the same in our new basis, but if it's a left-handed
C        sequence, the third angle will be negated.  Let's get this
C        straightened out right now.  The variable NEXT is just a
C        little mapping that takes 1 to 2, 2 to 3, and 3 to 1.
C
         IF ( AXIS2 .EQ. NEXT(AXIS3) ) THEN
            SIGN =  1.D0
         ELSE
            SIGN = -1.D0
         END IF

C
C        Set up the entries of CHANGE:
C
         CALL CLEARD ( 9, CHANGE )

         CHANGE ( AXIS3, 1 ) =         1.D0
         CHANGE ( AXIS2, 2 ) =         1.D0
         CHANGE ( AXIS1, 3 ) =  SIGN * 1.D0

C
C        Transform TMPROT.
C
         CALL MXM  ( TMPROT,  CHANGE,  TMPMAT )
         CALL MTXM ( CHANGE,  TMPMAT,  TMPROT )

C
C        Now we're ready to find the Euler angles, using a
C        3-2-1 factorization.  In general, the matrix product
C
C           [ a1 ]   [ a2 ]   [ a3 ]
C                 1        2        3
C
C        has the form
C
C
C     +-                                                              -+
C     |         cos(a2)cos(a3)          cos(a2)sin(a3)        -sin(a2) |
C     |                                                                |
C     |        -cos(a1)sin(a3)          cos(a1)cos(a3)  sin(a1)cos(a2) |
C     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 |
C     |                                                                |
C     |         sin(a1)sin(a3)         -sin(a1)cos(a3)  cos(a1)cos(a2) |
C     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 |
C     +-                                                              -+
C
C
C        but if a2 is -pi/2 or pi/2, the product matrix reduces to
C
C
C     +-                                                              -+
C     |                      0                       0        -sin(a2) |
C     |                                                                |
C     |        -cos(a1)sin(a3)          cos(a1)cos(a3)               0 |
C     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 |
C     |                                                                |
C     |         sin(a1)sin(a3)         -sin(a1)cos(a3)               0 |
C     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 |
C     +-                                                              -+
C
C
C        In this case, a1 and a3 are not uniquely determined.  If we
C        arbitrarily set a1 to zero, we arrive at the matrix
C
C           +-                                              -+
C           |               0                 0    -sin(a2)  |
C           |        -sin(a3)           cos(a3)          0   |,
C           |  sin(a2)cos(a3)    sin(a2)sin(a3)          0   |
C           +-                                              -+
C
C
C        We take care of this case first.  We test three conditions
C        that are mathematically equivalent, but may not be satisfied
C        simultaneously because of round-off:
C
C
         DEGEN =      (        (       TMPROT(1,1)    .EQ. 0.D0  )
     .                   .AND. (       TMPROT(1,2)    .EQ. 0.D0  )  )
     .           .OR. (        (       TMPROT(2,3)    .EQ. 0.D0  )
     .                   .AND. (       TMPROT(3,3)    .EQ. 0.D0  )  )
     .           .OR. (           ABS( TMPROT(1,3) )  .EQ. 1.D0     )


C
C        In the following block of code, we make use of the fact that
C
C           COS ( ANGLE2 )   >  0
C                            -
C        in choosing the signs of the ATAN2 arguments correctly.
C
         IF ( DEGEN ) THEN

            ANGLE3 =          0.D0
            ANGLE2 =          ASIN  ( -TMPROT(1,3)                 )
            ANGLE1 = SIGN  *  ATAN2 ( -TMPROT(2,1),   TMPROT(2,2)  )

         ELSE
C
C           The normal case.
C
            ANGLE3 =          ATAN2 (  TMPROT(2,3),   TMPROT(3,3)  )
            ANGLE2 =          ASIN  ( -TMPROT(1,3)                 )
            ANGLE1 = SIGN  *  ATAN2 (  TMPROT(1,2),   TMPROT(1,1)  )

         END IF

      END IF

      CALL CHKOUT ( 'M2EUL' )
      RETURN
      END

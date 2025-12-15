C$Procedure DNEARP ( Derivative of near point )

      SUBROUTINE DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )

C$ Abstract
C
C     Compute the state (position and velocity) of an ellipsoid surface
C     point nearest to the position component of a specified state.
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
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DNEAR ( 6 )
      DOUBLE PRECISION      DALT  ( 2 )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   State of an object in body-fixed coordinates.
C     A          I   Length of semi-axis parallel to X-axis.
C     B          I   Length of semi-axis parallel to Y-axis.
C     C          I   Length on semi-axis parallel to Z-axis.
C     DNEAR      O   State of the nearest point on the ellipsoid.
C     DALT       O   Altitude and derivative of altitude.
C     FOUND      O   Flag that indicates whether DNEAR is degenerate.
C
C$ Detailed_Input
C
C     STATE    is a 6-vector giving the position and velocity of some
C              object in the body-fixed coordinates of the ellipsoid.
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
C     DNEAR    is the 6-vector giving the position and velocity in
C              body-fixed coordinates of the point on the ellipsoid,
C              closest to the object whose position and velocity are
C              represented by STATE.
C
C              While the position component of DNEAR is always
C              meaningful, the velocity component of DNEAR will be
C              meaningless if FOUND if .FALSE. (See the discussion of
C              the meaning of FOUND below.)
C
C     DALT     is an array of two double precision numbers. The first
C              gives the altitude of STATE with respect to the
C              ellipsoid. The second gives the rate of change of the
C              altitude.
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
C         there are points at which there is more than 1 point on the
C         ellipsoid that is closest to the object. At these points the
C         velocity of the near point is undefined. (See the description
C         of the output variable FOUND).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If an object is moving relative to some triaxial body along a
C     trajectory C(t) then there is a companion trajectory N(t) that
C     gives the point on the ellipsoid that is closest to C(t) as a
C     function of `t'. The instantaneous position and velocity of C(t),
C     STATE, are sufficient to compute the instantaneous position and
C     velocity of N(t), DNEAR.
C
C     This routine computes DNEAR from STATE. In addition it returns the
C     altitude and rate of change of altitude.
C
C     Note that this routine can compute DNEAR for STATE outside, on,
C     or inside the ellipsoid. However, the velocity of DNEAR and
C     derivative of altitude do not exist for a "small" set of STATE
C     in the interior of the ellipsoid. See the discussion of FOUND
C     above for a description of this set of points.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Suppose you wish to compute the velocity of the ground track
C        of a satellite as it passes over a location on Mars and that
C        the moment of passage has been previously determined. (We
C        assume that the spacecraft is close enough to the surface that
C        light time corrections do not matter.)
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: dnearp_ex1.tm
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
C              File name                        Contents
C              ---------                        --------
C              pck00010.tpc                     Planet orientation and
C                                               radii
C              naif0012.tls                     Leapseconds
C              de430.bsp                        Planetary ephemeris
C              mar097.bsp                       Mars satellite ephemeris
C              mro_psp4_ssd_mro95a.bsp          MRO ephemeris
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'pck00010.tpc',
C                                  'naif0012.tls',
C                                  'de430.bsp',
C                                  'mar097.bsp',
C                                  'mro_psp4_ssd_mro95a.bsp' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM DNEARP_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      VNORM
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         BODYNM
C              PARAMETER           ( BODYNM = 'MARS' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'dnearp_ex1.tm' )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      A
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      C
C              DOUBLE PRECISION      DALT   ( 2 )
C              DOUBLE PRECISION      DNEAR  ( 6 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      RADII  ( 3 )
C              DOUBLE PRECISION      STATE  ( 6 )
C              DOUBLE PRECISION      GTVEL  ( 3 )
C
C              INTEGER               DIM
C
C              LOGICAL               FOUND
C
C        C
C        C     Load kernel files via the meta-kernel.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert the TDB input time string to seconds past
C        C     J2000, TDB.
C        C
C              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
C
C        C
C        C     First get the axes of the body.
C        C
C              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII )
C              CALL VUPACK ( RADII, A, B, C )
C
C        C
C        C     Get the geometric state of the spacecraft with
C        C     respect to BODYNM in the body-fixed reference frame
C        C     at ET and compute the state of the sub-spacecraft point.
C        C
C              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE',
C             .              BODYNM, STATE, LT                  )
C              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
C
C              IF ( FOUND ) THEN
C
C        C
C        C        DNEAR contains the state of the subspacecraft point.
C        C
C                 CALL VEQU ( DNEAR(4), GTVEL )
C
C                 WRITE(*,'(A,3F10.6)')
C             .        'Ground-track velocity (km/s):', GTVEL
C                 WRITE(*,'(A,F10.6)')
C             .        'Ground-track speed    (km/s):', VNORM( GTVEL )
C
C              ELSE
C
C                 WRITE(*,*) 'DNEAR is degenerate.'
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Ground-track velocity (km/s):  0.505252  1.986553 -2.475506
C        Ground-track speed    (km/s):  3.214001
C
C
C     2) Suppose you wish to compute the one-way doppler shift of a
C        radar mounted on board a spacecraft as it passes over some
C        region. Moreover, assume that for your purposes it is
C        sufficient to neglect effects of atmosphere, topography and
C        antenna pattern for the sake of this computation.
C
C        Use the meta-kernel from Example 1 above.
C
C
C        Example code begins here.
C
C
C              PROGRAM DNEARP_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      CLIGHT
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         BODYNM
C              PARAMETER           ( BODYNM = 'MARS' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'dnearp_ex1.tm' )
C
C        C
C        C     Define the central frequency of the radar,
C        C     in megahertz.
C        C
C              DOUBLE PRECISION      RCFRQ
C              PARAMETER           ( RCFRQ  = 20.D0 )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      A
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      C
C              DOUBLE PRECISION      DALT   ( 2 )
C              DOUBLE PRECISION      DNEAR  ( 6 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      RADII  ( 3 )
C              DOUBLE PRECISION      SHIFT
C              DOUBLE PRECISION      STATE  ( 6 )
C
C              INTEGER               DIM
C
C              LOGICAL               FOUND
C
C        C
C        C     Load kernel files via the meta-kernel.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert the TDB input time string to seconds past
C        C     J2000, TDB.
C        C
C              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
C
C        C
C        C     First get the axes of the body.
C        C
C              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII )
C              CALL VUPACK ( RADII, A, B, C )
C
C        C
C        C     Get the geometric state of the spacecraft with
C        C     respect to BODYNM in the body-fixed reference frame
C        C     at ET and compute the state of the sub-spacecraft point.
C        C
C              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE',
C             .              BODYNM, STATE, LT                  )
C              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
C
C              IF ( FOUND ) THEN
C
C        C
C        C        The change in frequency is given by multiplying SHIFT
C        C        times the carrier frequency
C        C
C                 SHIFT = ( DALT(2) / CLIGHT() )
C                 WRITE(*,'(A,F20.16)') 'Central frequency (MHz):',
C             .                          RCFRQ
C                 WRITE(*,'(A,F20.16)') 'Doppler shift     (MHz):',
C             .                          RCFRQ * SHIFT
C
C              ELSE
C
C                 WRITE(*,*) 'DNEAR is degenerate.'
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Central frequency (MHz): 20.0000000000000000
C        Doppler shift     (MHz): -0.0000005500991159
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
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 26-OCT-2021 (JDR) (EDW)
C
C        Reimplemented routine using ZZDNPT.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples, based on the existing code fragments.
C
C-    SPICELIB Version 1.1.2, 26-JUN-2008 (NJB)
C
C        Corrected spelling error in abstract; re-wrote
C        abstract text.
C
C-    SPICELIB Version 1.1.1, 24-OCT-2005 (NJB)
C
C        Header update: changed references to BODVAR to references
C        to BODVCD.
C
C-    SPICELIB Version 1.1.0, 05-MAR-1998 (WLT)
C
C        In the previous version of the routine FOUND could be
C        returned without being set to .TRUE. when the velocity
C        of the near point and rate of change of altitude
C        could be determined. This error has been corrected.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     Velocity of the nearest point on an ellipsoid
C     Rate of change of the altitude over an ellipsoid
C     Derivative of altitude over an ellipsoid
C     Velocity of a ground track
C
C-&


C
C     Spicelib functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED


C
C     Local Variables
C

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DNEARP' )

C
C     Until we have reason to believe otherwise, we set FOUND to TRUE.
C
      FOUND = .TRUE.

C
C     First we need to compute the near point.
C
      CALL NEARPT ( STATE, A, B, C, DNEAR, DALT )

C
C     Make sure nothing went bump in the dark innards of NEARPT.
C
      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'DNEARP' )
         RETURN
      END IF

C
C     Calculate the derivative of the near point and altitude.
C

      CALL ZZDNPT ( STATE, DNEAR, A, B, C, DNEAR(4), DALT(2), FOUND )

C
C     Check error status. Bail out if failure in call.
C
      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'DNEARP' )
         RETURN
      END IF

      CALL CHKOUT ( 'DNEARP' )

      RETURN
      END

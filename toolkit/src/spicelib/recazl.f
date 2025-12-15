C$Procedure RECAZL ( Rectangular coordinates to AZ/EL )

      SUBROUTINE RECAZL ( RECTAN, AZCCW, ELPLSZ, RANGE, AZ, EL )

C$ Abstract
C
C     Convert rectangular coordinates of a point to range, azimuth and
C     elevation.
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
C     CONVERSION
C     COORDINATES
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   RECTAN ( 3 )
      LOGICAL            AZCCW
      LOGICAL            ELPLSZ
      DOUBLE PRECISION   RANGE
      DOUBLE PRECISION   AZ
      DOUBLE PRECISION   EL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of a point.
C     AZCCW      I   Flag indicating how Azimuth is measured.
C     ELPLSZ     I   Flag indicating how Elevation is measured.
C     RANGE      O   Distance of the point from the origin.
C     AZ         O   Azimuth in radians.
C     EL         O   Elevation in radians.
C
C$ Detailed_Input
C
C     RECTAN   are the rectangular coordinates of a point.
C
C     AZCCW    is a flag indicating how azimuth is measured.
C
C              If AZCCW is .TRUE., azimuth increases in the
C              counterclockwise direction; otherwise it increases in
C              the clockwise direction.
C
C     ELPLSZ   is a flag indicating how elevation is measured.
C
C              If ELPLSZ is .TRUE., elevation increases from
C              the XY plane toward +Z; otherwise toward -Z.
C
C$ Detailed_Output
C
C     RANGE    is the distance of the point from the origin.
C
C              The units associated with RANGE are those associated
C              with the input point.
C
C     AZ       is the azimuth of the point. This is the angle between
C              the projection onto the XY plane of the vector from the
C              origin to the point and the +X axis of the reference
C              frame. AZ is zero at the +X axis.
C
C              The way azimuth is measured depends on the value of the
C              logical flag AZCCW. See the description of the argument
C              AZCCW for details.
C
C              AZ is output in radians. The range of AZ is [0, 2*pi].
C
C     EL       is the elevation of the point. This is the angle between
C              the vector from the origin to the point and the XY
C              plane. EL is zero at the XY plane.
C
C              The way elevation is measured depends on the value of
C              the logical flag ELPLSZ. See the description of the
C              argument ELPLSZ for details.
C
C              EL is output in radians. The range of EL is [-pi/2,
C              pi/2].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the X and Y components of RECTAN are both zero, the
C         azimuth is set to zero.
C
C     2)  If RECTAN is the zero vector, azimuth and elevation
C         are both set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the range, azimuth, and elevation of a point
C     specified in rectangular coordinates.
C
C     The output is defined by the distance from the center of the
C     reference frame (range), the angle from a reference vector
C     (azimuth), and the angle above the XY plane of the reference
C     frame (elevation).
C
C     The way azimuth and elevation are measured depends on the values
C     given by the user to the AZCCW and ELPLSZ logical flags. See the
C     descriptions of these input arguments for details.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create four tables showing a variety of rectangular
C        coordinates and the corresponding range, azimuth and
C        elevation, resulting from the different choices of the AZCCW
C        and ELPLSZ flags.
C
C        Corresponding rectangular coordinates and azimuth, elevation
C        and range are listed to three decimal places. Output angles
C        are in degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM RECAZL_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NREC
C              PARAMETER           ( NREC = 11 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(30)        MSG
C
C              DOUBLE PRECISION      AZ
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      RANGE
C              DOUBLE PRECISION      RECTAN ( 3, NREC )
C
C              INTEGER               I
C              INTEGER               J
C              INTEGER               K
C              INTEGER               N
C
C              LOGICAL               AZCCW  ( 2 )
C              LOGICAL               ELPLSZ ( 2 )
C
C        C
C        C     Define the input rectangular coordinates and the
C        C     different choices of the AZCCW and ELPLSZ flags.
C        C
C              DATA                  RECTAN /
C             .                  0.D0,         0.D0,         0.D0,
C             .                  1.D0,         0.D0,         0.D0,
C             .                  0.D0,         1.D0,         0.D0,
C             .                  0.D0,         0.D0,         1.D0,
C             .                 -1.D0,         0.D0,         0.D0,
C             .                  0.D0,        -1.D0,         0.D0,
C             .                  0.D0,         0.D0,        -1.D0,
C             .                  1.D0,         1.D0,         0.D0,
C             .                  1.D0,         0.D0,         1.D0,
C             .                  0.D0,         1.D0,         1.D0,
C             .                  1.D0,         1.D0,         1.D0  /
C
C              DATA                  AZCCW   /  .FALSE.,  .TRUE.  /
C              DATA                  ELPLSZ  /  .FALSE.,  .TRUE.  /
C
C        C
C        C     Create a table for each combination of AZCCW and ELPLSZ.
C        C
C              DO I = 1, 2
C
C                 DO J = 1, 2
C
C        C
C        C           Display the flag settings.
C        C
C                    MSG = 'AZCCW = #; ELPLSZ = #'
C                    CALL REPML ( MSG, '#', AZCCW(I),  'C', MSG )
C                    CALL REPML ( MSG, '#', ELPLSZ(J), 'C', MSG )
C
C                    WRITE(*,*)
C                    WRITE(*,'(A)') MSG
C
C        C
C        C           Print the banner.
C        C
C                    WRITE(*,*)
C                    WRITE(*,'(A)') '  RECT(1)  RECT(2)  RECT(3) '
C             .       //            '  RANGE      AZ       EL'
C                    WRITE(*,'(A)') '  -------  -------  ------- '
C             .       //            ' -------  -------  -------'
C
C        C
C        C           Do the conversion. Output angles in degrees.
C        C
C                    DO N = 1, NREC
C
C                       CALL RECAZL( RECTAN(1,N), AZCCW(I), ELPLSZ(J),
C             .                      RANGE,       AZ,       EL        )
C
C                       WRITE (*,'(6F9.3)') ( RECTAN(K,N), K=1,3 ),
C             .                           RANGE, AZ * DPR(), EL * DPR()
C
C                    END DO
C
C                 END DO
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
C        AZCCW = False; ELPLSZ = False
C
C          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            0.000    1.000    0.000    1.000  270.000    0.000
C            0.000    0.000    1.000    1.000    0.000  -90.000
C           -1.000    0.000    0.000    1.000  180.000    0.000
C            0.000   -1.000    0.000    1.000   90.000    0.000
C            0.000    0.000   -1.000    1.000    0.000   90.000
C            1.000    1.000    0.000    1.414  315.000    0.000
C            1.000    0.000    1.000    1.414    0.000  -45.000
C            0.000    1.000    1.000    1.414  270.000  -45.000
C            1.000    1.000    1.000    1.732  315.000  -35.264
C
C        AZCCW = False; ELPLSZ = True
C
C          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            0.000    1.000    0.000    1.000  270.000    0.000
C            0.000    0.000    1.000    1.000    0.000   90.000
C           -1.000    0.000    0.000    1.000  180.000    0.000
C            0.000   -1.000    0.000    1.000   90.000    0.000
C            0.000    0.000   -1.000    1.000    0.000  -90.000
C            1.000    1.000    0.000    1.414  315.000    0.000
C            1.000    0.000    1.000    1.414    0.000   45.000
C            0.000    1.000    1.000    1.414  270.000   45.000
C            1.000    1.000    1.000    1.732  315.000   35.264
C
C        AZCCW = True; ELPLSZ = False
C
C          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            0.000    1.000    0.000    1.000   90.000    0.000
C            0.000    0.000    1.000    1.000    0.000  -90.000
C           -1.000    0.000    0.000    1.000  180.000    0.000
C            0.000   -1.000    0.000    1.000  270.000    0.000
C            0.000    0.000   -1.000    1.000    0.000   90.000
C            1.000    1.000    0.000    1.414   45.000    0.000
C            1.000    0.000    1.000    1.414    0.000  -45.000
C            0.000    1.000    1.000    1.414   90.000  -45.000
C            1.000    1.000    1.000    1.732   45.000  -35.264
C
C        AZCCW = True; ELPLSZ = True
C
C          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            0.000    1.000    0.000    1.000   90.000    0.000
C            0.000    0.000    1.000    1.000    0.000   90.000
C           -1.000    0.000    0.000    1.000  180.000    0.000
C            0.000   -1.000    0.000    1.000  270.000    0.000
C            0.000    0.000   -1.000    1.000    0.000  -90.000
C            1.000    1.000    0.000    1.414   45.000    0.000
C            1.000    0.000    1.000    1.414    0.000   45.000
C            0.000    1.000    1.000    1.414   90.000   45.000
C            1.000    1.000    1.000    1.732   45.000   35.264
C
C
C     2) Compute the apparent azimuth and elevation of Venus as seen
C        from the DSS-14 station.
C
C        Task Description
C        ================
C
C        In this example, we will obtain the apparent position of
C        Venus as seen from the DSS-14 station in the DSS-14 topocentric
C        reference frame. We will use a station frames kernel and
C        transform the resulting rectangular coordinates to azimuth,
C        elevation and range using AZLREC.
C
C        In order to introduce the usage of the logical flags AZCCW
C        and ELPLSZ, we will request the azimuth to be measured
C        clockwise and the elevation positive towards the +Z
C        axis of the DSS-14_TOPO reference frame.
C
C
C        Kernels
C        =======
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: recazl_ex2.tm
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
C              de430.bsp                        Planetary ephemeris
C              naif0011.tls                     Leapseconds
C              earth_720101_070426.bpc          Earth historical
C                                               binary PCK
C              earthstns_itrf93_050714.bsp      DSN station SPK
C              earth_topo_050714.tf             DSN station FK
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'de430.bsp',
C                               'naif0011.tls',
C                               'earth_720101_070426.bpc',
C                               'earthstns_itrf93_050714.bsp',
C                               'earth_topo_050714.tf'         )
C
C           \begintext
C
C           End of meta-kernel.
C
C
C        Example code begins here.
C
C
C              PROGRAM RECAZL_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT0
C              PARAMETER           ( FMT0   = '(3F21.8)'  )
C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(A,F20.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'recazl_ex2.tm' )
C
C              INTEGER               BDNMLN
C              PARAMETER           ( BDNMLN = 36 )
C
C              INTEGER               CORLEN
C              PARAMETER           ( CORLEN = 10 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(BDNMLN)    OBS
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(FRNMLN)    REF
C              CHARACTER*(BDNMLN)    TARGET
C
C              DOUBLE PRECISION      AZ
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      PTARG  ( 3 )
C              DOUBLE PRECISION      R
C
C              INTEGER               I
C
C              LOGICAL               AZCCW
C              LOGICAL               ELPLSZ
C
C        C
C        C     Load SPICE kernels.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert the observation time to seconds past J2000 TDB.
C        C
C              OBSTIM = '2003 OCT 13 06:00:00.000000 UTC'
C
C              CALL STR2ET ( OBSTIM, ET )
C
C        C
C        C     Set the target, observer, observer frame, and
C        C     aberration corrections.
C        C
C              TARGET = 'VENUS'
C              OBS    = 'DSS-14'
C              REF    = 'DSS-14_TOPO'
C              ABCORR = 'CN+S'
C
C        C
C        C     Compute the observer-target position.
C        C
C              CALL SPKPOS ( TARGET, ET, REF, ABCORR, OBS, PTARG, LT )
C
C        C
C        C     Compute azimuth, elevation and range of Venus
C        C     as seen from DSS-14, with azimuth increasing
C        C     clockwise and elevation positive towards +Z
C        C     axis of the DSS-14_TOPO reference frame
C        C
C              AZCCW  = .FALSE.
C              ELPLSZ = .TRUE.
C
C              CALL RECAZL ( PTARG, AZCCW, ELPLSZ, R, AZ, EL )
C
C        C
C        C     Express both angles in degrees.
C        C
C              EL =   EL * DPR()
C              AZ =   AZ * DPR()
C
C        C
C        C     Display the computed position, the range and
C        C     the angles.
C        C
C              WRITE (*,*)
C              WRITE (*,'(2A)') 'Target:                ', TARGET
C              WRITE (*,'(2A)') 'Observation time:      ', OBSTIM
C              WRITE (*,'(2A)') 'Observer center:       ', OBS
C              WRITE (*,'(2A)') 'Observer frame:        ', REF
C              WRITE (*,'(2A)') 'Aberration correction: ', ABCORR
C              WRITE (*,*)
C              WRITE (*,'(A)')  'Observer-target position (km):'
C              WRITE (*,FMT0)  PTARG
C              WRITE (*,FMT1)  'Light time (s):       ', LT
C              WRITE (*,*)
C              WRITE (*,FMT1) 'Target azimuth          (deg): ', AZ
C              WRITE (*,FMT1) 'Target elevation        (deg): ', EL
C              WRITE (*,FMT1) 'Observer-target distance (km): ', R
C              WRITE (*,*)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Target:                VENUS
C        Observation time:      2003 OCT 13 06:00:00.000000 UTC
C        Observer center:       DSS-14
C        Observer frame:        DSS-14_TOPO
C        Aberration correction: CN+S
C
C        Observer-target position (km):
C            66886767.37916669   146868551.77222887  -185296611.10841593
C        Light time (s):               819.63862811
C
C        Target azimuth          (deg):         294.48543372
C        Target elevation        (deg):         -48.94609726
C        Observer-target distance (km):   245721478.99272084
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
C     S.C. Krening       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 07-SEP-2021 (JDR) (NJB) (SCK) (BVS)
C
C-&


C$ Index_Entries
C
C     rectangular coordinates to range, az and el
C     rectangular to range, azimuth and elevation
C     convert rectangular coordinates to range, az and el
C     convert rectangular to range, azimuth and elevation
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI

C
C     Call the subroutine RECRAD to convert the rectangular coordinates
C     into right ascension and declination.  In RECRAD, the right
C     ascension is measured counterclockwise from +X axis about the +Z
C     axis, and the declination is measured positive from the XY plane
C     towards +Z axis.
C
C     The header of RECRAD says in part:
C
C        The range of RA is [0, 2*pi].
C        The range of DEC is [-pi/2, pi/2].
C
C     The range of AZ in the following call is that of RA.
C     The range of EL is that of DEC.
C
      CALL RECRAD ( RECTAN, RANGE, AZ, EL )

C
C     If AZCCW is set to .FALSE. the azimuth is measured clockwise from
C     the +X axis about the +Z axis.
C
      IF ( .NOT. AZCCW ) THEN
C
C        Azimuth increases in the clockwise direction.
C
C        Map AZ to its 2*pi complement if AZ is non-zero. Don't map
C        zero to 2*pi. The value of AZ returned from RECRAD is never
C        negative, but it may be zero.
C
         IF ( AZ .GT. 0.D0 ) THEN
C
C            Replace AZ with its 2*pi complement.
C
C            This assignment requires that ACOS( -1.D0 ) be exactly
C            equal to DATAN2( 0.D0, <non-zero value> ). This should be
C            true for any correct arithmetic implementation.
C
C            Although we expect the result to always be non-negative,
C            we take no chances.
C
             AZ = MAX( TWOPI() - AZ, 0.D0 )

         END IF

      END IF

C
C     If ELPLSZ is set to .FALSE. the elevation is measured positive
C     from the XY plane toward the -Z axis.
C
      IF ( .NOT. ELPLSZ ) THEN
C
C        Negate only non-zero values of EL. Avoid creating
C        -0.D0 values, which affect printed outputs generated
C        by example programs.
C
         IF ( EL .NE. 0.D0 ) THEN
            EL = -EL
         END IF

      END IF

      RETURN
      END

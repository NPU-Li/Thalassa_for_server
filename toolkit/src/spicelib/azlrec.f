C$Procedure AZLREC ( AZ/EL to rectangular coordinates )

      SUBROUTINE AZLREC ( RANGE, AZ, EL, AZCCW, ELPLSZ, RECTAN )

C$ Abstract
C
C     Convert from range, azimuth and elevation of a point to
C     rectangular coordinates.
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

      DOUBLE PRECISION   RANGE
      DOUBLE PRECISION   AZ
      DOUBLE PRECISION   EL
      LOGICAL            AZCCW
      LOGICAL            ELPLSZ
      DOUBLE PRECISION   RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RANGE      I   Distance of the point from the origin.
C     AZ         I   Azimuth in radians.
C     EL         I   Elevation in radians.
C     AZCCW      I   Flag indicating how azimuth is measured.
C     ELPLSZ     I   Flag indicating how elevation is measured.
C     RECTAN     O   Rectangular coordinates of a point.
C
C$ Detailed_Input
C
C     RANGE    is the distance of the point from the origin. The
C              input should be in terms of the same units in which
C              the output is desired.
C
C              Although negative values for RANGE are allowed, its
C              use may lead to undesired results. See the $Exceptions
C              section for a discussion on this topic.
C
C     AZ       is the azimuth of the point. This is the angle between
C              the projection onto the XY plane of the vector from
C              the origin to the point and the +X axis of the
C              reference frame. AZ is zero at the +X axis.
C
C              The way azimuth is measured depends on the value of
C              the logical flag AZCCW. See the description of the
C              argument AZCCW for details.
C
C              The range (i.e., the set of allowed values) of AZ is
C              unrestricted. See the $Exceptions section for a
C              discussion on the AZ range.
C
C              Units are radians.
C
C     EL       is the elevation of the point. This is the angle
C              between the vector from the origin to the point and
C              the XY plane. EL is zero at the XY plane.
C
C              The way elevation is measured depends on the value of
C              the logical flag ELPLSZ. See the description of the
C              argument ELPLSZ for details.
C
C              The range (i.e., the set of allowed values) of EL is
C              [-pi/2, pi/2], but no error checking is done to ensure
C              that EL is within this range. See the $Exceptions
C              section for a discussion on the EL range.
C
C              Units are radians.
C
C     AZCCW    is a flag indicating how the azimuth is measured.
C
C              If AZCCW is .TRUE., the azimuth increases in the
C              counterclockwise direction; otherwise it increases
C              in the clockwise direction.
C
C     ELPLSZ   is a flag indicating how the elevation is measured.
C
C              If ELPLSZ is .TRUE., the elevation increases from
C              the XY plane toward +Z; otherwise toward -Z.
C
C$ Detailed_Output
C
C     RECTAN   is an array containing the rectangular coordinates of
C              the point.
C
C              The units associated with the point are those
C              associated with the input RANGE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the value of the input argument RANGE is negative
C         the output rectangular coordinates will be negated, i.e.
C         the resulting array will be of the same length
C         but opposite direction to the one that would be obtained
C         with a positive input argument RANGE of value ||RANGE||.
C
C     2)  If the value of the input argument EL is outside the
C         range [-pi/2, pi/2], the results may not be as
C         expected.
C
C     3)  If the value of the input argument AZ is outside the
C         range [0, 2*pi], the value will be mapped to a value
C         inside the range that differs from the input value by an
C         integer multiple of 2*pi.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts the azimuth, elevation, and range
C     of a point into the associated rectangular coordinates.
C
C     The input is defined by the distance from the center of
C     the reference frame (range), the angle from a reference
C     vector (azimuth), and the angle above the XY plane of the
C     reference frame (elevation).
C
C     The way azimuth and elevation are measured depends on the
C     values given by the user to the AZCCW and ELPLSZ logical
C     flags. See the descriptions of these input arguments
C     for details.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create four tables showing a variety of azimuth/elevation
C        coordinates and the corresponding rectangular coordinates,
C        resulting from the different choices of the AZCCW and ELPLSZ
C        flags.
C
C        Corresponding azimuth/elevation and rectangular coordinates
C        are listed to three decimal places. Input angles are in
C        degrees.
C
C
C        Example code begins here.
C
C
C              PROGRAM AZLREC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      RPD
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
C              DOUBLE PRECISION      AZ     ( NREC )
C              DOUBLE PRECISION      EL     ( NREC )
C              DOUBLE PRECISION      RANGE  ( NREC )
C              DOUBLE PRECISION      RAZ
C              DOUBLE PRECISION      REL
C              DOUBLE PRECISION      RECTAN ( 3 )
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
C        C     Define the input azimuth/elevation coordinates and the
C        C     different choices of the AZCCW and ELPLSZ flags.
C        C
C              DATA                  RANGE   /
C             .                            0.D0,    1.D0,    1.D0,
C             .                            1.D0,    1.D0,    1.D0,
C             .                            1.D0,    1.414D0, 1.414D0,
C             .                            1.414D0, 1.732D0           /
C
C              DATA                  AZ     /
C             .                            0.D0,    0.D0,   270.D0,
C             .                            0.D0,  180.D0,    90.D0,
C             .                            0.D0,  315.D0,     0.D0,
C             .                          270.D0,  315.D0            /
C
C              DATA                  EL     /
C             .                            0.D0,    0.D0,     0.D0,
C             .                          -90.D0,    0.D0,     0.D0,
C             .                           90.D0,    0.D0,   -45.D0,
C             .                          -45.D0,  -35.264D0         /
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
C                    WRITE(*,'(A)') '   RANGE      AZ       EL   '
C             .       //            ' RECT(1)  RECT(2)  RECT(3)'
C                    WRITE(*,'(A)') '  -------  -------  ------- '
C             .       //            ' -------  -------  -------'
C
C        C
C        C           Do the conversion. Input angles in degrees.
C        C
C                    DO N = 1, NREC
C
C                       RAZ = AZ(N) * RPD()
C                       REL = EL(N) * RPD()
C
C                       CALL AZLREC ( RANGE(N), RAZ,       REL,
C             .                       AZCCW(I), ELPLSZ(J), RECTAN )
C
C                       WRITE (*,'(6F9.3)')  RANGE(N), AZ(N), EL(N),
C             .                              RECTAN
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
C           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000  270.000    0.000   -0.000    1.000    0.000
C            1.000    0.000  -90.000    0.000    0.000    1.000
C            1.000  180.000    0.000   -1.000   -0.000    0.000
C            1.000   90.000    0.000    0.000   -1.000    0.000
C            1.000    0.000   90.000    0.000    0.000   -1.000
C            1.414  315.000    0.000    1.000    1.000    0.000
C            1.414    0.000  -45.000    1.000    0.000    1.000
C            1.414  270.000  -45.000   -0.000    1.000    1.000
C            1.732  315.000  -35.264    1.000    1.000    1.000
C
C        AZCCW = False; ELPLSZ = True
C
C           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000  270.000    0.000   -0.000    1.000    0.000
C            1.000    0.000  -90.000    0.000    0.000   -1.000
C            1.000  180.000    0.000   -1.000   -0.000    0.000
C            1.000   90.000    0.000    0.000   -1.000    0.000
C            1.000    0.000   90.000    0.000    0.000    1.000
C            1.414  315.000    0.000    1.000    1.000    0.000
C            1.414    0.000  -45.000    1.000    0.000   -1.000
C            1.414  270.000  -45.000   -0.000    1.000   -1.000
C            1.732  315.000  -35.264    1.000    1.000   -1.000
C
C        AZCCW = True; ELPLSZ = False
C
C           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000  270.000    0.000   -0.000   -1.000    0.000
C            1.000    0.000  -90.000    0.000    0.000    1.000
C            1.000  180.000    0.000   -1.000    0.000    0.000
C            1.000   90.000    0.000    0.000    1.000    0.000
C            1.000    0.000   90.000    0.000    0.000   -1.000
C            1.414  315.000    0.000    1.000   -1.000    0.000
C            1.414    0.000  -45.000    1.000    0.000    1.000
C            1.414  270.000  -45.000   -0.000   -1.000    1.000
C            1.732  315.000  -35.264    1.000   -1.000    1.000
C
C        AZCCW = True; ELPLSZ = True
C
C           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
C          -------  -------  -------  -------  -------  -------
C            0.000    0.000    0.000    0.000    0.000    0.000
C            1.000    0.000    0.000    1.000    0.000    0.000
C            1.000  270.000    0.000   -0.000   -1.000    0.000
C            1.000    0.000  -90.000    0.000    0.000   -1.000
C            1.000  180.000    0.000   -1.000    0.000    0.000
C            1.000   90.000    0.000    0.000    1.000    0.000
C            1.000    0.000   90.000    0.000    0.000    1.000
C            1.414  315.000    0.000    1.000   -1.000    0.000
C            1.414    0.000  -45.000    1.000    0.000   -1.000
C            1.414  270.000  -45.000   -0.000   -1.000   -1.000
C            1.732  315.000  -35.264    1.000   -1.000   -1.000
C
C
C     2) Compute the right ascension and declination of the pointing
C        direction of DSS-14 station at a given epoch.
C
C        Task Description
C        ================
C
C        In this example, we will obtain the right ascension and
C        declination of the pointing direction of the DSS-14 station at
C        a given epoch, by converting the station's pointing direction
C        given in azimuth and elevation to rectangular coordinates
C        in the station topocentric reference frame and applying a
C        frame transformation from DSS-14_TOPO to J2000, in order to
C        finally obtain the corresponding right ascension and
C        declination of the pointing vector.
C
C        In order to introduce the usage of the logical flags AZCCW
C        and ELPLSZ, we will assume that the azimuth is measured
C        counterclockwise and the elevation negative towards +Z
C        axis of the DSS-14_TOPO reference frame.
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
C           File name: azlrec_ex2.tm
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
C             File name                        Contents
C             ---------                        --------
C             naif0011.tls                     Leapseconds
C             earth_720101_070426.bpc          Earth historical
C                                              binary PCK
C             earth_topo_050714.tf             DSN station FK
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'naif0011.tls',
C                               'earth_720101_070426.bpc',
C                               'earth_topo_050714.tf'     )
C
C           \begintext
C
C           End of meta-kernel.
C
C
C        Example code begins here.
C
C
C              PROGRAM AZLREC_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT0
C              PARAMETER           ( FMT0   = '(A,3F15.8)' )
C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(A,F15.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'azlrec_ex2.tm' )
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
C              CHARACTER*(40)        MSG
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(FRNMLN)    REF
C
C              DOUBLE PRECISION      AZ
C              DOUBLE PRECISION      AZR
C              DOUBLE PRECISION      DEC
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      ELR
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      JPOS   ( 3 )
C              DOUBLE PRECISION      PTARG  ( 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      RA
C              DOUBLE PRECISION      RANGE
C              DOUBLE PRECISION      ROTATE ( 3, 3 )
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
C        C     Set the local topocentric frame
C        C
C              REF    = 'DSS-14_TOPO'
C
C        C
C        C     Set the station's pointing direction in azimuth and
C        C     elevation. Set arbitrarily the range to 1.0. Azimuth
C        C     and elevation shall be given in radians. Azimuth
C        C     increases counterclockwise and elevation is negative
C        C     towards +Z (above the local horizon)
C        C
C              AZ     =   75.00
C              EL     =  -27.25
C              AZR    =   AZ * RPD()
C              ELR    =   EL * RPD()
C              R      =    1.00
C              AZCCW  = .TRUE.
C              ELPLSZ = .FALSE.
C
C        C
C        C     Obtain the rectangular coordinates of the station's
C        C     pointing direction.
C        C
C              CALL AZLREC ( R, AZR, ELR, AZCCW, ELPLSZ, PTARG )
C
C        C
C        C     Transform the station's pointing vector from the
C        C     local topocentric frame to J2000.
C        C
C              CALL PXFORM ( REF,   'J2000',   ET, ROTATE )
C              CALL MXV    ( ROTATE,  PTARG, JPOS         )
C
C        C
C        C     Compute the right ascension and declination.
C        C     Express both angles in degrees.
C        C
C              CALL RECRAD ( JPOS, RANGE, RA, DEC )
C              RA =   RA * DPR()
C              DEC =   DEC * DPR()
C
C        C
C        C     Display the computed pointing vector, the input
C        C     data and resulting the angles.
C        C
C              WRITE (*,*)
C              WRITE (*,FMT1) 'Pointing azimuth    (deg): ', AZ
C              WRITE (*,FMT1) 'Pointing elevation  (deg): ', EL
C
C              CALL REPML ( 'Azimuth counterclockwise?: #', '#',
C             .              AZCCW, 'C', MSG                    )
C              WRITE (*,'(A)') MSG
C
C              CALL REPML ( 'Elevation positive +Z?   : #', '#',
C             .              ELPLSZ, 'C', MSG                   )
C              WRITE (*,'(A)') MSG
C
C              WRITE (*,'(2A)') 'Observation epoch        : ', OBSTIM
C              WRITE (*,*)
C              WRITE (*,'(A)') 'Pointing direction (normalized):   '
C              WRITE (*,FMT0) '  ', ( PTARG(I), I = 1, 3 )
C              WRITE (*,*)
C              WRITE (*,FMT1) 'Pointing right ascension (deg): ', RA
C              WRITE (*,FMT1) 'Pointing declination (deg):     ', DEC
C              WRITE (*,*)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Pointing azimuth    (deg):     75.00000000
C        Pointing elevation  (deg):    -27.25000000
C        Azimuth counterclockwise?: True
C        Elevation positive +Z?   : False
C        Observation epoch        : 2003 OCT 13 06:00:00.000000 UTC
C
C        Pointing direction (normalized):
C               0.23009457     0.85872462     0.45787392
C
C        Pointing right ascension (deg):    280.06179939
C        Pointing declination (deg):         26.92826084
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 08-SEP-2021 (JDR) (NJB)
C
C-&


C$ Index_Entries
C
C     range, az and el to rectangular coordinates
C     range, azimuth and elevation to rectangular
C     convert range, az and el to rectangular coordinates
C     convert range, azimuth and elevation to rectangular
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      AZIMUT
      DOUBLE PRECISION      ELEVAT

C
C     Error free routine. No check-in.
C
      AZIMUT = AZ
      ELEVAT = EL

C
C     We are going to use RADREC to convert the azimuth and elevation
C     to rectangular coordinates. In RADREC, the right ascension
C     is measured counterclockwise from +X axis about the +Z axis, and
C     the declination is measured positive from the XY plane towards
C     +Z axis.
C
C     Check the AZCCW and ELPLSZ flags and convert AZ and EL to the
C     coordinate system used by RADREC.
C
C     If AZCCW is set to .FALSE. the azimuth is measured clockwise
C     from the +X axis about the +Z axis.
C
      IF ( .NOT. AZCCW ) THEN
C
C        We can simply negate AZ; we don't need to map it into the
C        range [0, 2*pi]. LATREC will accept it as is.
C
C        Negate only non-zero values of AZ. Avoid creating
C        -0.D0 values, which affect printed outputs generated
C        by example programs.
C
         IF ( AZ .NE. 0.D0 ) THEN
            AZIMUT = -AZ
         END IF

      END IF

C
C     If ELPLSZ is set to .FALSE. the elevation is measured positive
C     from the XY plane towards the -Z axis.
C
      IF ( .NOT. ELPLSZ ) THEN
C
C        Negate only non-zero values of EL. Avoid creating
C        -0.D0 values, which affect printed outputs generated
C        by example programs.
C
         IF ( EL .NE. 0.D0 ) THEN
            ELEVAT = -EL
         END IF

      END IF

C
C     In principle, we could call the subroutine RADREC to convert AZ
C     and EL to rectangular coordinates. RADREC simply passes its
C     inputs to LATREC, so we bypass the middleman.
C
C     We rely on LATREC to handle the case of RANGE < 0.
C
      CALL LATREC ( RANGE, AZIMUT, ELEVAT, RECTAN )

      RETURN
      END

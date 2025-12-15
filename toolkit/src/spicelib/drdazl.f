C$Procedure DRDAZL ( Derivative of rectangular w.r.t. AZ/EL )

      SUBROUTINE DRDAZL ( RANGE, AZ, EL, AZCCW, ELPLSZ, JACOBI )

C$ Abstract
C
C     Compute the Jacobian matrix of the transformation from
C     azimuth/elevation to rectangular coordinates.
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
C     COORDINATES
C     DERIVATIVES
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      AZ
      DOUBLE PRECISION      EL
      LOGICAL               AZCCW
      LOGICAL               ELPLSZ
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RANGE      I   Distance of a point from the origin.
C     AZ         I   Azimuth of input point in radians.
C     EL         I   Elevation of input point in radians.
C     AZCCW      I   Flag indicating how azimuth is measured.
C     ELPLSZ     I   Flag indicating how elevation is measured.
C     JACOBI     O   Matrix of partial derivatives.
C
C$ Detailed_Input
C
C     RANGE    is the distance from the origin of the input point
C              specified by RANGE, AZ, and EL.
C
C              Negative values for RANGE are not allowed.
C
C              Units are arbitrary and are considered to match those
C              of the rectangular coordinate system associated with the
C              output matrix JACOBI.
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
C              counterclockwise direction; otherwise AZ increases
C              in the clockwise direction.
C
C     ELPLSZ   if a flag indicating how the elevation is measured.
C
C              If ELPLSZ is .TRUE., the elevation increases from
C              the XY plane toward +Z; otherwise toward -Z.
C
C$ Detailed_Output
C
C     JACOBI   is the matrix of partial derivatives of the
C              transformation from azimuth/elevation to rectangular
C              coordinates. It has the form
C
C                 .-                                  -.
C                 |  DX/DRANGE     DX/DAZ     DX/DEL   |
C                 |                                    |
C                 |  DY/DRANGE     DY/DAZ     DY/DEL   |
C                 |                                    |
C                 |  DZ/DRANGE     DZ/DAZ     DZ/DEL   |
C                 `-                                  -'
C
C              evaluated at the input values of RANGE, AZ and EL.
C
C              X, Y, and Z are given by the familiar formulae
C
C                 X = RANGE * COS( AZ )          * COS( EL )
C                 Y = RANGE * SIN( AZSNSE * AZ ) * COS( EL )
C                 Z = RANGE * SIN( ELDIR  * EL )
C
C              where AZSNSE is +1 when AZCCW is .TRUE. and -1
C              otherwise, and ELDIR is +1 when ELPLSZ is .TRUE.
C              and -1 otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the value of the input parameter RANGE is negative,
C         the error SPICE(VALUEOUTOFRANGE) is signaled.
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
C     It is often convenient to describe the motion of an object
C     in azimuth/elevation coordinates. It is also convenient to
C     manipulate vectors associated with the object in rectangular
C     coordinates.
C
C     The transformation of a azimuth/elevation state into an
C     equivalent rectangular state makes use of the Jacobian matrix
C     of the transformation between the two systems.
C
C     Given a state in latitudinal coordinates,
C
C        ( r, az, el, dr, daz, del )
C
C     the velocity in rectangular coordinates is given by the matrix
C     equation
C                    t          |                             t
C        (dx, dy, dz)   = JACOBI|             * (dr, daz, del)
C                               |(r,az,el)
C
C     This routine computes the matrix
C
C              |
C        JACOBI|
C              |(r,az,el)
C
C     In the azimuth/elevation coordinate system, several conventions
C     exist on how azimuth and elevation are measured. Using the AZCCW
C     and ELPLSZ flags, users indicate which conventions shall be used.
C     See the descriptions of these input arguments for details.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the azimuth/elevation state of Venus as seen from the
C        DSS-14 station at a given epoch. Map this state back to
C        rectangular coordinates as a check.
C
C        Task description
C        ================
C
C        In this example, we will obtain the apparent state of Venus as
C        seen from the DSS-14 station in the DSS-14 topocentric
C        reference frame. We will use a station frames kernel and
C        transform the resulting rectangular coordinates to azimuth,
C        elevation and range and its derivatives using RECAZL and
C        DAZLDR.
C
C        We will map this state back to rectangular coordinates using
C        AZLREC and DRDAZL.
C
C        In order to introduce the usage of the logical flags AZCCW
C        and ELPLSZ, we will request the azimuth to be measured
C        clockwise and the elevation positive towards +Z
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
C           File name: drdazl_ex1.tm
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
C                                                  binary PCK
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
C              PROGRAM DRDAZL_EX1
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
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(A,F20.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'drdazl_ex1.tm' )
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
C              DOUBLE PRECISION      AZLVEL ( 3    )
C              DOUBLE PRECISION      DRECTN ( 3    )
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      JACOBI ( 3, 3 )
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      RECTAN ( 3    )
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
C        C     Compute the observer-target state.
C        C
C              CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBS,
C             .              STATE,  LT                   )
C
C        C
C        C     Convert position to azimuth/elevation coordinates,
C        C     with azimuth increasing clockwise and elevation
C        C     positive towards +Z axis of the DSS-14_TOPO
C        C     reference frame
C        C
C              AZCCW  = .FALSE.
C              ELPLSZ = .TRUE.
C
C              CALL RECAZL ( STATE, AZCCW, ELPLSZ, R, AZ, EL )
C
C        C
C        C     Convert velocity to azimuth/elevation coordinates.
C        C
C              CALL DAZLDR ( STATE(1), STATE(2), STATE(3),
C             .              AZCCW,    ELPLSZ,   JACOBI   )
C
C              CALL MXV ( JACOBI, STATE(4), AZLVEL )
C
C        C
C        C     As a check, convert the azimuth/elevation state back to
C        C     rectangular coordinates.
C        C
C              CALL AZLREC ( R, AZ, EL, AZCCW, ELPLSZ, RECTAN )
C
C              CALL DRDAZL ( R, AZ, EL, AZCCW, ELPLSZ, JACOBI )
C
C              CALL MXV ( JACOBI, AZLVEL, DRECTN )
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'AZ/EL coordinates:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   Range      (km)        = ', R
C              WRITE(*,FMT1) '   Azimuth    (deg)       = ', AZ * DPR()
C              WRITE(*,FMT1) '   Elevation  (deg)       = ', EL * DPR()
C              WRITE(*,*)
C              WRITE(*,'(A)')    'AZ/EL velocity:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   d Range/dt     (km/s)  = ', AZLVEL(1)
C              WRITE(*,FMT1) '   d Azimuth/dt   (deg/s) = ', AZLVEL(2)
C             .                                             * DPR()
C              WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLVEL(3)
C             .                                             * DPR()
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Rectangular coordinates:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   X (km)                 = ', STATE(1)
C              WRITE(*,FMT1) '   Y (km)                 = ', STATE(2)
C              WRITE(*,FMT1) '   Z (km)                 = ', STATE(3)
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Rectangular velocity:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   dX/dt (km/s)           = ', STATE(4)
C              WRITE(*,FMT1) '   dY/dt (km/s)           = ', STATE(5)
C              WRITE(*,FMT1) '   dZ/dt (km/s)           = ', STATE(6)
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Rectangular coordinates from inverse '
C             .    //         'mapping:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   X (km)                 = ', RECTAN(1)
C              WRITE(*,FMT1) '   Y (km)                 = ', RECTAN(2)
C              WRITE(*,FMT1) '   Z (km)                 = ', RECTAN(3)
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Rectangular velocity from inverse '
C             .    //         'mapping:'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   dX/dt (km/s)           = ', DRECTN(1)
C              WRITE(*,FMT1) '   dY/dt (km/s)           = ', DRECTN(2)
C              WRITE(*,FMT1) '   dZ/dt (km/s)           = ', DRECTN(3)
C              WRITE(*,*)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        AZ/EL coordinates:
C
C           Range      (km)        =   245721478.99272084
C           Azimuth    (deg)       =         294.48543372
C           Elevation  (deg)       =         -48.94609726
C
C        AZ/EL velocity:
C
C           d Range/dt     (km/s)  =          -4.68189834
C           d Azimuth/dt   (deg/s) =           0.00402256
C           d Elevation/dt (deg/s) =          -0.00309156
C
C        Rectangular coordinates:
C
C           X (km)                 =    66886767.37916667
C           Y (km)                 =   146868551.77222887
C           Z (km)                 =  -185296611.10841590
C
C        Rectangular velocity:
C
C           dX/dt (km/s)           =        6166.04150307
C           dY/dt (km/s)           =      -13797.77164550
C           dZ/dt (km/s)           =       -8704.32385654
C
C        Rectangular coordinates from inverse mapping:
C
C           X (km)                 =    66886767.37916658
C           Y (km)                 =   146868551.77222890
C           Z (km)                 =  -185296611.10841590
C
C        Rectangular velocity from inverse mapping:
C
C           dX/dt (km/s)           =        6166.04150307
C           dY/dt (km/s)           =      -13797.77164550
C           dZ/dt (km/s)           =       -8704.32385654
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
C     Jacobian matrix of rectangular w.r.t. AZ/EL coordinates
C     range, azimuth and elevation to rectangular derivative
C     Range, AZ and EL to rectangular velocity conversion
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON

      INTEGER               AZSNSE
      INTEGER               ELDIR
      INTEGER               I

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DRDAZL' )

C
C     The input range must be non-negative. If not, signal an error
C     and check out.
C
      IF ( RANGE .LT. 0.0D0 ) THEN

         CALL SETMSG ( 'Input range was #. Negative '
     .   //            'values are not allowed.'  )
         CALL ERRDP  ( '#', RANGE                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'   )
         CALL CHKOUT ( 'DRDAZL'                   )
         RETURN

      END IF

C
C     Convert the input azimuth and elevation to the equivalent
C     latitudinal coordinates, and define the rotation sense for the
C     azimuth and direction for the elevation
C
      IF ( AZCCW ) THEN

         LON    = AZ
         AZSNSE = 1

      ELSE

         LON    = -AZ
         AZSNSE = -1

      END IF

      IF ( ELPLSZ ) THEN

         LAT    = EL
         ELDIR  = 1

      ELSE

         LAT    = -EL
         ELDIR  = -1

      END IF

C
C     Now we have the latitudinal equivalent coordinates, use them to
C     find the Jacobian matrix of rectangular coordinates with respect
C     to latitudinal coordinates.
C
      CALL DRDLAT ( RANGE, LON, LAT, JACOBI )

C
C     The matrix JACOBI is
C
C        .-                             -.
C        |  DX/DRANGE  DX/DLON  DX/DLAT  |
C        |  DY/DRANGE  DY/DLON  DY/DLAT  |
C        |  DZ/DRANGE  DZ/DLON  DZ/DLAT  |
C        `-                             -'
C
C     Given that
C
C        LON = AZSNSE * AZ
C        LAT = ELDIR  * EL
C
C     applying the chain rule to derivative of each Cartesian
C     component with respect to the latitude and longitude, the matrix
C     above is equivalent to
C
C        .-                                                      -.
C        |  DX/DRANGE   (1/AZSNSE) * DX/DAZ   (1/ELDIR) * DX/DEL  |
C        |  DY/DRANGE   (1/AZSNSE) * DY/DAZ   (1/ELDIR) * DY/DEL  |
C        |  DZ/DRANGE   (1/AZSNSE) * DZ/DAZ   (1/ELDIR) * DZ/DEL  |
C        `-                                                      -'
C
C     We have
C
C        AZSNSE = 1 / AZSNSE
C        ELDIR  = 1 / ELDIR
C
C     So, multiplying the second column of JACOBI by AZSNSE and the
C     third column of JACOBI by ELDIR gives us the matrix we actually
C     want to compute: the Jacobian matrix of rectangular
C     coordinates with respect to azimuth/elevation coordinates.
C
      DO I = 1, 3

         JACOBI(I,2) = AZSNSE * JACOBI(I,2)
         JACOBI(I,3) = ELDIR  * JACOBI(I,3)

      END DO

      CALL CHKOUT ( 'DRDAZL' )
      RETURN
      END

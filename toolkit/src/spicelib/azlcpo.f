C$Procedure AZLCPO ( AZ/EL, constant position observer state )

      SUBROUTINE AZLCPO ( METHOD, TARGET, ET,     ABCORR, AZCCW,
     .                    ELPLSZ, OBSPOS, OBSCTR, OBSREF, AZLSTA, LT )

C$ Abstract
C
C     Return the azimuth/elevation coordinates of a specified target
C     relative to an "observer," where the observer has constant
C     position in a specified reference frame. The observer's position
C     is provided by the calling program rather than by loaded SPK
C     files.
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
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     COORDINATES
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      LOGICAL               AZCCW
      LOGICAL               ELPLSZ
      DOUBLE PRECISION      OBSPOS ( 3 )
      CHARACTER*(*)         OBSCTR
      CHARACTER*(*)         OBSREF
      DOUBLE PRECISION      AZLSTA ( 6 )
      DOUBLE PRECISION      LT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     METHOD     I   Method to obtain the surface normal vector.
C     TARGET     I   Name of target ephemeris object.
C     ET         I   Observation epoch.
C     ABCORR     I   Aberration correction.
C     AZCCW      I   Flag indicating how azimuth is measured.
C     ELPLSZ     I   Flag indicating how elevation is measured.
C     OBSPOS     I   Observer position relative to center of motion.
C     OBSCTR     I   Center of motion of observer.
C     OBSREF     I   Body-fixed, body-centered frame of observer's
C                    center.
C     AZLSTA     O   State of target with respect to observer,
C                    in azimuth/elevation coordinates.
C     LT         O   One way light time between target and
C                    observer.
C
C$ Detailed_Input
C
C     METHOD   is a short string providing parameters defining the
C              computation method to be used to obtain the surface
C              normal vector that defines the local zenith. Parameters
C              include, but are not limited to, the shape model used to
C              represent the body's surface of observer's center of
C              motion.
C
C              The only choice currently supported is
C
C                 'ELLIPSOID'        The intercept computation uses
C                                    a triaxial ellipsoid to model
C                                    the body's surface of the
C                                    observer's center of motion.
C                                    The ellipsoid's radii must be
C                                    available in the kernel pool.
C
C              Neither case nor white space are significant in
C              METHOD. For example, the string ' eLLipsoid ' is
C              valid.
C
C              In a later Toolkit release, this argument will be
C              used to invoke a wider range of surface
C              representations. For example, it will be possible to
C              represent the target body's surface using a digital
C              shape model.
C
C     TARGET   is the name of a target body. Optionally, you may supply
C              the ID code of the object as an integer string. For
C              example, both 'EARTH' and '399' are legitimate strings
C              to supply to indicate the target is Earth.
C
C              Case and leading and trailing blanks are not significant
C              in the string TARGET.
C
C     ET       is the ephemeris time at which the state of the
C              target relative to the observer is to be computed. ET
C              is expressed as seconds past J2000 TDB. ET refers to
C              time at the observer's location.
C
C     ABCORR   is a short string that indicates the aberration
C              corrections to be applied to the observer-target state
C              to account for one-way light time and stellar
C              aberration.
C
C              ABCORR may be any of the following:
C
C                 'NONE'     Apply no correction. Return the
C                            geometric state of the target
C                            relative to the observer.
C
C              The following values of ABCORR apply to the
C              "reception" case in which photons depart from the
C              target's location at the light-time corrected epoch
C              ET-LT and *arrive* at the observer's location at ET:
C
C                 'LT'       Correct for one-way light time (also
C                            called "planetary aberration") using a
C                            Newtonian formulation. This correction
C                            yields the state of the target at the
C                            moment it emitted photons arriving at
C                            the observer at ET.
C
C                            The light time correction uses an
C                            iterative solution of the light time
C                            equation. The solution invoked by the
C                            'LT' option uses one iteration.
C
C                 'LT+S'     Correct for one-way light time and
C                            stellar aberration using a Newtonian
C                            formulation. This option modifies the
C                            state obtained with the 'LT' option to
C                            account for the observer's velocity
C                            relative to the solar system
C                            barycenter. The result is the apparent
C                            state of the target---the position and
C                            velocity of the target as seen by the
C                            observer.
C
C                 'CN'       Converged Newtonian light time
C                            correction. In solving the light time
C                            equation, the 'CN' correction iterates
C                            until the solution converges.
C
C                 'CN+S'     Converged Newtonian light time
C                            and stellar aberration corrections.
C
C
C              The following values of ABCORR apply to the
C              "transmission" case in which photons *depart* from
C              the observer's location at ET and arrive at the
C              target's location at the light-time corrected epoch
C              ET+LT:
C
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time using a Newtonian
C                            formulation. This correction yields the
C                            state of the target at the moment it
C                            receives photons emitted from the
C                            observer's location at ET.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation  This option modifies the
C                            state obtained with the 'XLT' option to
C                            account for the observer's velocity
C                            relative to the solar system
C                            barycenter. The position component of
C                            the computed target state indicates the
C                            direction that photons emitted from the
C                            observer's location must be "aimed" to
C                            hit the target.
C
C                 'XCN'      "Transmission" case: converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C
C              Neither special nor general relativistic effects are
C              accounted for in the aberration corrections applied
C              by this routine.
C
C              Case and leading and trailing blanks are not
C              significant in the string ABCORR.
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
C     OBSPOS   is the fixed (constant) geometric position of an
C              observer relative to its center of motion OBSCTR,
C              expressed in the reference frame OBSREF.
C
C              OBSPOS does not need to be located on the surface of
C              the object centered at OBSCTR.
C
C              Units are always km.
C
C     OBSCTR   is the name of the center of motion of OBSPOS. The
C              ephemeris of OBSCTR is provided by loaded SPK files.
C
C              Optionally, you may supply the integer ID code for the
C              object as an integer string. For example both 'MOON' and
C              '301' are legitimate strings that indicate the moon is
C              the center of motion.
C
C              Case and leading and trailing blanks are not significant
C              in the string OBSCTR.
C
C     OBSREF   is the name of the body-fixed, body-centered reference
C              frame associated with the observer's center of motion,
C              relative to which the input position OBSPOS is
C              expressed. The observer has constant position relative
C              to its center of motion in this reference frame.
C
C              Case and leading and trailing blanks are not significant
C              in the string OBSREF.
C
C$ Detailed_Output
C
C     AZLSTA   is a state vector representing the position and
C              velocity of the target relative to the specified
C              observer, corrected for the specified aberrations
C              and expressed in azimuth/elevation coordinates. The
C              first three components of AZLSTA represent the range,
C              azimuth and elevation of the target's position; the
C              last three components form the corresponding velocity
C              vector:
C
C                 AZLSTA = ( R, AZ, EL, dR/dt, dAZ/dt, dEL/dt )
C
C              The position component of AZLSTA points from the
C              observer's location at ET to the aberration-corrected
C              location of the target. Note that the sense of the
C              position vector is independent of the direction of
C              radiation travel implied by the aberration correction.
C
C              The velocity component of AZLSTA is the derivative with
C              respect to time of the position component of AZLSTA.
C
C              Azimuth, elevation and its derivatives are measured with
C              respect to the axes of the local topocentric reference
C              frame. See the $Particulars section for the definition
C              of this reference frame.
C
C              The azimuth is the angle between the projection onto the
C              local topocentric principal (X-Y) plane of the vector
C              from the observer's position to the target and the
C              principal axis of the reference frame. The azimuth is
C              zero on the +X axis.
C
C              The elevation is the angle between the vector from the
C              observer's position to the target and the local
C              topocentric principal plane. The elevation is zero on
C              the plane.
C
C              Units are km for R, radians for AZ and EL, km/sec for
C              dR/dt, and radians/sec for dAZ/dt and dEL/dt. The range
C              of AZ is [0, 2*pi] and the range of EL is [-pi/2, pi/2].
C
C              The way azimuth and elevation are measured depend
C              respectively on the value of the logical flags AZCCW and
C              ELPLSZ. See the description of these input arguments for
C              details.
C
C     LT       is the one-way light time between the observer and
C              target in seconds. If the target state is corrected
C              for aberrations, then LT is the one-way light time
C              between the observer and the light time corrected
C              target location.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either the name of the center of motion or the target
C         cannot be translated to its NAIF ID code, an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  If the reference frame OBSREF is not recognized, the error
C         SPICE(UNKNOWNFRAME) is signaled. A frame name may fail to be
C         recognized because a required frame specification kernel has
C         not been loaded; another cause is a misspelling of the frame
C         name.
C
C     3)  If the reference frame OBSREF is not centered at the
C         observer's center of motion OBSCTR, the error
C         SPICE(INVALIDFRAME) is signaled.
C
C     4)  If the radii of the center of motion body are not available
C         from the kernel pool, an error is signaled by a routine in
C         the call tree of this routine.
C
C     5)  If the size of the OBSCTR body radii kernel variable is not
C         three, an error is signaled by a routine in the call tree of
C         this routine.
C
C     6)  If any of the three OBSCTR body radii is less-than or equal to
C         zero, an error is signaled by a routine in the call tree of
C         this routine.
C
C     7)  If the ratio of the longest to the shortest
C         radii is large enough so that arithmetic expressions
C         involving its squared value may overflow, an error is
C         signaled by a routine in the call tree of this routine.
C
C     8)  If the radii of the center of motion body and the axes of
C         OBSPOS have radically different magnitudes so that arithmetic
C         overflow may occur during the computation of the nearest
C         point of the observer on the center of motion's reference
C         ellipsoid, an error is signaled by a routine in the call tree
C         of this routine. Note that even if there is no overflow, if
C         the ratios of the radii lengths, or the ratio of the
C         magnitude of OBSPOS and the shortest radius vary by many
C         orders of magnitude, the results may have poor precision.
C
C     9)  If the computation METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     10) If the loaded kernels provide insufficient data to compute
C         the requested state vector, an error is signaled by a routine
C         in the call tree of this routine.
C
C     11) If an error occurs while reading an SPK or other kernel file,
C         the error  is signaled by a routine in the call tree of this
C         routine.
C
C     12) If the aberration correction ABCORR is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     13) If TARGET is on the Z-axis ( X = 0 and Y = 0 ) of the local
C         topocentric frame centered at OBSPOS, an error is signaled by
C         a routine in the call tree of this routine. See item 2 in the
C         $Restrictions section for further details.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C     -  SPK data: ephemeris data for the observer center and target
C        must be loaded. If aberration corrections are used, the
C        states of the observer center and target relative to the
C        solar system barycenter must be calculable from the
C        available ephemeris data. Typically ephemeris data are made
C        available by loading one or more SPK files using FURNSH.
C
C     -  Shape and orientation data: if the computation method is
C        specified as "Ellipsoid," triaxial radii for the center body
C        must be loaded into the kernel pool. Typically this is done by
C        loading a text PCK file via FURNSH. Additionally, rotation
C        data for the body-fixed, body-centered frame associated with
C        the observer's center of motion must be loaded. These may be
C        provided in a text or binary PCK file. In some cases these
C        data may be provided by a CK file.
C
C     The following data may be required:
C
C     -  Frame data: if a frame definition not built into SPICE is
C        required, for example to convert the observer-target state
C        to the body-fixed body-centered frame, that definition
C        must be available in the kernel pool. Typically frame
C        definitions are supplied by loading a frame kernel using
C        FURNSH.
C
C     -  Additional kernels: if a CK frame is used in this routine's
C        state computation, then at least one CK and corresponding SCLK
C        kernel is required. If dynamic frames are used, additional
C        SPK, PCK, CK, or SCLK kernels may be required.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine computes azimuth/elevation coordinates of a target
C     as seen from an observer whose trajectory is not provided by SPK
C     files.
C
C     Observers supported by this routine must have constant position
C     with respect to a specified center of motion, expressed in a
C     caller-specified reference frame. The state of the center of
C     motion relative to the target must be computable using
C     loaded SPK data.
C
C     This routine is suitable for computing the azimuth/elevation
C     coordinates and its derivatives of target ephemeris
C     objects, as seen from landmarks on the surface of an extended
C     object, in cases where no SPK data are available for those
C     landmarks.
C
C     The azimuth/elevation coordinates are defined with respect to
C     the observer's local topocentric reference frame. This frame is
C     generally defined as follows:
C
C     -  the +Z axis is aligned with the surface normal outward
C        vector at the observer's location;
C
C     -  the +X axis is aligned with the component of the +Z axis
C        of the body-fixed reference frame orthogonal to the
C        outward normal vector, i.e. the +X axis points towards
C        the body's North pole;
C
C     -  the +Y axis completes the right-handed system.
C
C     For observers located on the +Z axis of the body-fixed frame
C     designated by OBSREF, the following definition of the local
C     topocentric reference frame is used by this routine:
C
C     -  the +Z axis is aligned with the surface normal outward
C        vector at the observer's location;
C
C     -  the +X axis aligned with the +X axis of the body-fixed
C        reference frame;
C
C     -  the +Y axis completes the right-handed system.
C
C     In both cases, the origin of the local topocentric frame is
C     the observer's location.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the azimuth/elevation state of Venus as seen from the
C        DSS-14 station at a given epoch first using the position of
C        the station given as a vector in the ITRF93 frame and then
C        using the data provided in the kernel pool for the DSS-14
C        station.
C
C
C        Task description
C        ================
C
C        In this example, we will obtain the apparent state of Venus as
C        seen from DSS-14 station in the DSS-14 topocentric reference
C        frame. For this computation, we'll use the DSS-14 station's
C        location given as a vector in the ITRF93 frame.
C
C        Then we will compute same apparent state using SPKPOS to
C        obtain a Cartesian state vector, after which we will transform
C        the vector coordinates to azimuth, elevation and range and
C        their derivatives using RECAZL and DAZLDR.
C
C        In order to introduce the usage of the logical flags AZCCW
C        and ELPLSZ, we will request the azimuth to be measured
C        clockwise and the elevation positive towards the +Z
C        axis of the DSS-14_TOPO reference frame.
C
C        Results from the two computations will not agree exactly
C        because of time-dependent differences in the orientation,
C        relative to the ITRF93 frame, of the topocentric frame centered
C        at DSS-14. This orientation varies with time due to movement of
C        the station, which is affected by tectonic plate motion. The
C        computation using AZLCPO evaluates the orientation of this
C        frame using the station location at the observation epoch,
C        while the SPKPOS computation uses the orientation provided by
C        the station frame kernel. The latter is fixed and is derived
C        from the station location at an epoch specified in the
C        documentation of that kernel.
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
C           File name: azlcpo_ex1.tm
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
C              pck00010.tpc                     Planetary constants
C              earth_720101_070426.bpc          Earth historical
C                                                  binary PCK
C              earthstns_itrf93_050714.bsp      DSN station SPK
C              earth_topo_050714.tf             DSN station FK
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'de430.bsp',
C                               'naif0011.tls',
C                               'pck00010.tpc',
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
C              PROGRAM AZLCPO_EX1
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
C              PARAMETER           ( FMT1   = '(A,F20.8)'     )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'azlcpo_ex1.tm' )
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
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 40 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(BDNMLN)    OBS
C              CHARACTER*(BDNMLN)    OBSCTR
C              CHARACTER*(FRNMLN)    OBSREF
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(STRLEN)    METHOD
C              CHARACTER*(FRNMLN)    REF
C              CHARACTER*(BDNMLN)    TARGET
C
C              DOUBLE PRECISION      AZ
C              DOUBLE PRECISION      AZLSTA ( 6    )
C              DOUBLE PRECISION      AZLVEL ( 3    )
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      JACOBI ( 3, 3 )
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      OBSPOS ( 3    )
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
C              OBSTIM = '2003 Jan 01 00:00:00 TDB'
C
C              CALL STR2ET ( OBSTIM, ET )
C
C        C
C        C     Set the method, target, center of motion of the observer,
C        C     frame of observer position, and aberration corrections.
C        C
C              METHOD = 'ELLIPSOID'
C              TARGET = 'VENUS'
C              OBSCTR = 'EARTH'
C              OBSREF = 'ITRF93'
C              ABCORR = 'CN+S'
C
C        C
C        C     Set the position of DSS-14 relative to the earth's
C        C     center at the observation epoch, expressed in the
C        C     ITRF93 reference frame. Values come from the
C        C     earth station SPK specified in the meta-kernel.
C        C
C        C     The actual station velocity is non-zero due
C        C     to tectonic plate motion; we ignore the motion
C        C     in this example.
C        C
C              OBSPOS(1) =  -2353.621419700D0
C              OBSPOS(2) =  -4641.341471700D0
C              OBSPOS(3) =   3677.052317800D0
C
C        C
C        C     We want the azimuth/elevation coordinates to be measured
C        C     with the azimuth increasing clockwise and the
C        C     elevation positive towards +Z axis of the local
C        C     topocentric reference frame
C        C
C              AZCCW  = .FALSE.
C              ELPLSZ = .TRUE.
C
C              CALL AZLCPO ( METHOD, TARGET, ET,     ABCORR,
C             .              AZCCW,  ELPLSZ, OBSPOS, OBSCTR,
C             .              OBSREF, AZLSTA, LT              )
C
C        C
C        C     In order to check the results obtained using AZLCPO
C        C     we are going to compute the same azimuth/elevation state
C        C     using the position of DSS-14 and its local topocentric
C        C     reference frame 'DSS-14_TOPO' from the kernel pool.
C        C
C              OBS    = 'DSS-14'
C              REF    = 'DSS-14_TOPO'
C
C        C
C        C     Compute the observer-target state.
C        C
C              CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBS,
C             .              STATE,  LT                   )
C
C        C
C        C     Convert the position to azimuth/elevation coordinates.
C        C
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
C              WRITE(*,*)
C              WRITE(*,'(A)') 'AZ/EL coordinates (from AZLCPO):'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   Range     (km)         = ', AZLSTA(1)
C              WRITE(*,FMT1) '   Azimuth   (deg)        = ', AZLSTA(2)
C             .                                            * DPR()
C              WRITE(*,FMT1) '   Elevation (deg)        = ', AZLSTA(3)
C             .                                            * DPR()
C              WRITE(*,*)
C              WRITE(*,'(A)') 'AZ/EL coordinates (using kernels):'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   Range     (km)         = ', R
C              WRITE(*,FMT1) '   Azimuth   (deg)        = ', AZ * DPR()
C              WRITE(*,FMT1) '   Elevation (deg)        = ', EL * DPR()
C              WRITE(*,*)
C              WRITE(*,'(A)') 'AZ/EL velocity (from AZLCPO):'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   d Range/dt    (km/s)   = ', AZLSTA(4)
C              WRITE(*,FMT1) '   d Azimuth/dt  (deg/s)  = ', AZLSTA(5)
C             .                                            * DPR()
C              WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLSTA(6)
C             .                                            * DPR()
C              WRITE(*,*)
C              WRITE(*,'(A)') 'AZ/EL velocity (using kernels):'
C              WRITE(*,*)
C              WRITE(*,FMT1) '   d Range/dt     (km/s)  = ', AZLVEL(1)
C              WRITE(*,FMT1) '   d Azimuth/dt   (deg/s) = ', AZLVEL(2)
C             .                                            * DPR()
C              WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLVEL(3)
C             .                                            * DPR()
C              WRITE(*,*)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        AZ/EL coordinates (from AZLCPO):
C
C           Range     (km)         =    89344802.82679011
C           Azimuth   (deg)        =         269.04481881
C           Elevation (deg)        =         -25.63088321
C
C        AZ/EL coordinates (using kernels):
C
C           Range     (km)         =    89344802.82679011
C           Azimuth   (deg)        =         269.04481846
C           Elevation (deg)        =         -25.63088278
C
C        AZ/EL velocity (from AZLCPO):
C
C           d Range/dt    (km/s)   =          13.41734176
C           d Azimuth/dt  (deg/s)  =           0.00238599
C           d Elevation/dt (deg/s) =          -0.00339644
C
C        AZ/EL velocity (using kernels):
C
C           d Range/dt     (km/s)  =          13.41734176
C           d Azimuth/dt   (deg/s) =           0.00238599
C           d Elevation/dt (deg/s) =          -0.00339644
C
C
C        Note the discrepancy in the AZ/EL coordinates found by the two
C        computation methods. Please refer to the task description for
C        an explanation.
C
C$ Restrictions
C
C     1)  This routine may not be suitable for work with stars or other
C         objects having large distances from the observer, due to loss
C         of precision in position vectors.
C
C     2)  The Jacobian matrix of the transformation from rectangular to
C         azimuth/elevation coordinates has a singularity for points
C         located on the Z-axis ( X = 0 and Y = 0 ) of the local
C         topocentric frame centered at OBSPOS; therefore the
C         derivative of the azimuth/elevation coordinates cannot be
C         computed for those points.
C
C         A user who wishes to compute the azimuth/elevation
C         coordinates, without their derivatives, of TARGET as seen
C         from OBSPOS at the input time ET, for those cases when TARGET
C         is located along the local topocentric Z-axis, could do so by
C         executing the following calls:
C
C            CALL SPKCPO ( TARGET, ET,     OBSREF, 'OBSERVER', ABCORR,
C           .              OBSPOS, OBSCTR, OBSREF,  STATE,     LT     )
C
C            RANGE = VNORM( STATE )
C
C         By definition, the azimuth is zero and the elevation is
C         either pi/2 if ELPLSZ is .TRUE., or -pi/2 otherwise.
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
C-    SPICELIB Version 1.0.0, 01-NOV-2021 (JDR) (NJB) (EDW)
C
C-&


C$ Index_Entries
C
C     AZ/EL_coordinates relative to constant_position_observer
C     AZ/EL_coordinates w.r.t. constant_position surface_point
C     AZ/EL_coordinates relative to surface_point extended_object
C     AZ/EL_coordinates relative to landmark on extended_object
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         REFLOC
      PARAMETER           ( REFLOC = 'OBSERVER' )

C
C     Local variables
C
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LHSTA  ( 6    )
      DOUBLE PRECISION      NORMAL ( 3    )
      DOUBLE PRECISION      OBSSPT ( 3    )
      DOUBLE PRECISION      RADII  ( 3    )
      DOUBLE PRECISION      STATE  ( 6    )
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
      DOUBLE PRECISION      XFTOPO ( 3, 3 )
      DOUBLE PRECISION      Z      ( 3    )

      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FRCLSS
      INTEGER               FXFCDE
      INTEGER               OBSCDE

      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE                  Z

C
C     Initial values
C
      DATA                  Z       / 0.00D0,   0.00D0,   1.00D0 /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'AZLCPO' )

C
C     Get the center of motion ID code here, since it will be
C     need later on several calls.
C
      CALL BODS2C ( OBSCTR, OBSCDE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer''s center of motion, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you did not load a '
     .   //            'text kernel containing body-name mapping '
     .   //            'assignments for this name, or that you '
     .   //            'need an updated version of the SPICE '
     .   //            'Toolkit.'                                 )
         CALL ERRCH  ( '#', OBSCTR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'AZLCPO'                                   )
         RETURN

      END IF

C
C     Determine the attributes of the frame designated by OBSREF.
C
      CALL NAMFRM ( OBSREF, FXFCDE )
      CALL FRINFO ( FXFCDE, CENTER, FRCLSS, CLSSID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'AZLCPO' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by '
     .   //            'the SPICE frame subsystem. Possibly '
     .   //            'a required frame definition kernel has '
     .   //            'not been loaded.'                        )
         CALL ERRCH  ( '#',  OBSREF                              )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                     )
         CALL CHKOUT ( 'AZLCPO'                                  )
         RETURN

      END IF

C
C     Make sure that OBSREF is centered at the observer's center of
C     motion.
C
      IF ( CENTER .NE. OBSCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the '
     .   //            'observer''s center of motion #. The ID '
     .   //            'code of the frame center is #.'           )
         CALL ERRCH  ( '#',  OBSREF                               )
         CALL ERRCH  ( '#',  OBSCTR                               )
         CALL ERRINT ( '#',  CENTER                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( 'AZLCPO'                                   )
         RETURN

      END IF

C
C     Construct the local topocentric reference frame. Check
C     first the method to be used.
C
      IF ( EQSTR( METHOD, 'ELLIPSOID' )  ) THEN

C
C        If the input observer position is on the Z-axis of the
C        body-fixed, the local topocentric frame will be defined
C        as follows:
C
C           - the +Z axis aligned with the outward normal vector;
C
C           - the +X axis aligned with the +X axis of the body-fixed
C             reference frame;
C
C           - the +Y axis completes the right-handed system.
C
C        otherwise, the local topocentric frame will be defined as
C        follows:
C
C           - the +Z axis aligned with the outward normal vector;
C
C           - the +X axis aligned with the component of the +Z axis
C             of the body-fixed reference frame orthogonal to the
C             outward normal vector;
C
C           - the +Y axis completes the right-handed frame.
C
         IF (       ( OBSPOS(1) .EQ. 0.0D0 )
     .        .AND. ( OBSPOS(2) .EQ. 0.0D0 ) ) THEN

            CALL IDENT ( XFTOPO )

            IF ( OBSPOS(3) .LT. 0.0D0 ) THEN

               CALL ROTMAT ( XFTOPO, PI(), 1, TMPMAT )
               CALL MOVED  ( TMPMAT, 9,       XFTOPO )

            END IF

         ELSE
C
C           Get the radii of the observer center of motion from the
C           kernel pool.
C
            CALL ZZGFTREB ( OBSCDE, RADII )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'AZLCPO' )
               RETURN
            END IF

C
C           The observer's position does not need to be located on
C           the surface of the reference ellipsoid. Find the nearest
C           point on the ellipsoid to the observer.
C
            CALL NEARPT ( OBSPOS, RADII(1), RADII(2), RADII(3),
     .                    OBSSPT, ALT                          )

C
C           Get the outward-pointing, unit normal vector from the point
C           on the surface of the reference ellipsoid.
C
            CALL SURFNM ( RADII(1), RADII(2), RADII(3), OBSSPT, NORMAL )

C
C           Construct the transformation matrix from the body-fixed
C           reference frame associated with the observer's center of
C           motion and the local topocentric frame at the observer's
C           location.
C
            CALL TWOVEC ( NORMAL, 3, Z, 1, XFTOPO )

         END IF

      ELSE

         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( 'AZLCPO'                                        )
         RETURN

      END IF

C
C     Compute the observer-target position vector. Use as OUTREF the
C     same reference frame used for expressing the OBSPOS vector
C     (OBSREF).
C
      CALL SPKCPO ( TARGET, ET,     OBSREF, REFLOC, ABCORR,
     .              OBSPOS, OBSCTR, OBSREF, STATE,  LT     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'AZLCPO' )
         RETURN
      END IF

C
C     STATE is expressed with respect to the reference frame
C     specified by OBSREF. Convert this vector from OBSREF frame
C     to local-horizon frame.
C
      CALL MXV ( XFTOPO, STATE,    LHSTA    )
      CALL MXV ( XFTOPO, STATE(4), LHSTA(4) )

C
C     Convert LHSTA from rectangular to azimuth/elevation coordinates
C
      CALL RECAZL ( LHSTA,     AZCCW,     ELPLSZ,
     .              AZLSTA(1), AZLSTA(2), AZLSTA(3) )

      CALL DAZLDR ( LHSTA(1),  LHSTA(2),  LHSTA(3),
     .              AZCCW,     ELPLSZ,    JACOBI    )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'AZLCPO' )
         RETURN
      END IF

      CALL MXV ( JACOBI, LHSTA(4), AZLSTA(4) )

      CALL CHKOUT ( 'AZLCPO' )
      RETURN
      END

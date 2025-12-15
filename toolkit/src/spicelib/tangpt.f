C$Procedure TANGPT ( Ray-ellipsoid tangent point )

      SUBROUTINE TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

C$ Abstract
C
C     Compute, for a given observer, ray emanating from the observer,
C     and target, the "tangent point": the point on the ray nearest
C     to the target's surface. Also compute the point on the target's
C     surface nearest to the tangent point.
C
C     The locations of both points are optionally corrected for light
C     time and stellar aberration.
C
C     The surface shape is modeled as a triaxial ellipsoid.
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
C     ABCORR
C     CK
C     FRAMES
C     NAIF_IDS
C     PCK
C     SCLK
C     SPK
C     TIME
C
C$ Keywords
C
C     ELLIPSOID
C     GEOMETRY
C     RAY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         CORLOC
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      TANPT  ( 3 )
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     FIXREF     I   Body-fixed, body-centered target body frame.
C     ABCORR     I   Aberration correction.
C     CORLOC     I   Aberration correction locus: 'TANGENT POINT' or
C                    'SURFACE POINT'.
C     OBSRVR     I   Name of observing body.
C     DREF       I   Reference frame of ray direction vector.
C     DVEC       I   Ray direction vector.
C     TANPT      O   "Tangent point": point on ray nearest to surface.
C     ALT        O   Altitude of tangent point above surface.
C     RANGE      O   Distance of tangent point from observer.
C     SRFPT      O   Point on surface nearest to tangent point.
C     TRGEPC     O   Epoch associated with correction locus.
C     SRFVEC     O   Vector from observer to surface point SRFPT.
C
C$ Detailed_Input
C
C     METHOD   is a short string providing parameters defining
C              the computation method to be used.
C
C              METHOD is currently restricted to the value
C
C                 'ELLIPSOID'
C
C              This value indicates that the target shape is
C              modeled as a triaxial ellipsoid.
C
C              METHOD is case-insensitive, and leading and trailing
C              blanks in METHOD are not significant.
C
C     TARGET   is the name of the target body. TARGET is
C              case-insensitive, and leading and trailing blanks in
C              TARGET are not significant. Optionally, you may
C              supply a string containing the integer ID code
C              for the object. For example both 'MOON' and '301'
C              are legitimate strings that indicate the Moon is the
C              target body.
C
C              If the target is identified by name rather than ID code,
C              the target name must be recognized by SPICE. Radii
C              defining a triaxial ellipsoid target shape model must be
C              available in the kernel pool. See the $Files section
C              below.
C
C     ET       is the epoch associated with the observer, expressed as
C              ephemeris seconds past J2000 TDB. ET is the epoch at
C              which radiation is received by the observer, when an
C              observation is made, or in the case of transmission from
C              the observer, at which radiation is emitted.
C
C              ET is the epoch at which the state of the observer
C              relative to the solar system barycenter is computed.
C
C              When aberration corrections are not used, ET is also
C              the epoch at which the state and orientation of the
C              target body are computed.
C
C              When aberration corrections are used, the position
C              and orientation of the target body are computed at
C              ET-LT or ET+LT, where LT is the one-way light time
C              between the aberration correction locus and the
C              observer. The sign applied to LT depends on the
C              selected correction. See the descriptions of ABCORR
C              and CORLOC below for details.
C
C     FIXREF   is the name of a body-fixed reference frame centered on
C              the target body. FIXREF may be any such frame supported
C              by the SPICE system, including built-in frames
C              (documented in frames.req) and frames defined by a
C              loaded frame kernel (FK). The string FIXREF is
C              case-insensitive, and leading and trailing blanks in
C              FIXREF are not significant.
C
C              The output points TANPT and SRFPT, and the
C              observer-to-surface point vector SRFVEC will be
C              expressed relative to this reference frame.
C
C     ABCORR   indicates the aberration corrections to be applied
C              when computing the target's position and orientation.
C
C              See the description of the aberration correction
C              locus CORLOC for further details on how aberration
C              corrections are applied.
C
C              For remote sensing applications, where the apparent
C              tangent or surface point seen by the observer is
C              desired, normally one of the corrections
C
C                 'CN+S' or 'NONE'
C
C              should be selected. For applications involving
C              transmission from the observer, normally 'XCN+S' or
C              'NONE' should be selected.
C
C              Light-time-only corrections can be useful for
C              testing but generally don't accurately model geometry
C              applicable to remote sensing observations or signal
C              transmission.
C
C              The supported options are described below.
C
C              ABCORR may be any of the following:
C
C                 'NONE'     Compute outputs without applying
C                            aberration corrections.
C
C                            'NONE' may be suitable when the
C                            magnitudes of the aberration
C                            corrections are negligible.
C
C              Let LT represent the one-way light time between the
C              observer and the aberration correction locus specified
C              by CORLOC. The following values of ABCORR apply to the
C              "reception" case in which radiation departs from the
C              aberration correction locus at the light-time corrected
C              epoch ET-LT and arrives at the observer's location at
C              ET:
C
C                 'LT'       Correct for one-way light time between
C                            the aberration correction locus and
C                            the observer, using a Newtonian
C                            formulation. This correction yields the
C                            position of the aberration correction
C                            locus at the moment it emitted radiation
C                            arriving at the observer at ET.
C
C                            The light time correction uses an
C                            iterative solution of the light time
C                            equation. The solution invoked by the
C                            'LT' option uses several iterations
C                            but does not guarantee convergence.
C
C                            Both the target position as seen by the
C                            observer, and rotation of the target
C                            body, are corrected for light time.
C
C                 'LT+S'     Correct for one-way light time and
C                            stellar aberration using a Newtonian
C                            formulation. This option modifies the
C                            aberration correction locus solution
C                            obtained with the 'LT' option to
C                            account for the observer's velocity
C                            relative to the solar system
C                            barycenter. These corrections yield the
C                            apparent aberration correction locus.
C
C                 'CN'       Converged Newtonian light time
C                            correction. In solving the light time
C                            equation, the 'CN' correction iterates
C                            until either the solution converges or
C                            a large iteration limit is reached.
C                            Both the position and orientation of
C                            the target body are corrected for light
C                            time.
C
C                 'CN+S'     Converged Newtonian light time and stellar
C                            aberration corrections. This option
C                            produces a solution that is at least as
C                            accurate at that obtainable with the
C                            'LT+S' option. Whether the 'CN+S' solution
C                            is substantially more accurate depends on
C                            the geometry of the participating objects
C                            and on the accuracy of the input data. In
C                            some cases this routine will execute more
C                            slowly when a converged solution is
C                            computed.
C
C                            For reception-case applications where
C                            aberration corrections are applied, this
C                            option should be used, unless the
C                            magnitudes of the corrections are
C                            negligible.
C
C              The following values of ABCORR apply to the
C              "transmission" case in which radiation *departs* from
C              the observer's location at ET and arrives at the
C              aberration correction locus at the light-time
C              corrected epoch ET+LT:
C
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time between the
C                            aberration correction locus and the
C                            observer, using a Newtonian
C                            formulation. This correction yields the
C                            position of the aberration correction
C                            locus at the moment it receives radiation
C                            emitted from the observer's location at
C                            ET.
C
C                            The light time correction uses an
C                            iterative solution of the light time
C                            equation. The solution invoked by the
C                            'XLT' option uses several iterations
C                            but does not guarantee convergence.
C
C                            Both the target position as seen by the
C                            observer, and rotation of the target
C                            body, are corrected for light time.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation. This option modifies the
C                            aberration correction locus solution
C                            obtained with the 'XLT' option to
C                            account for the observer's velocity
C                            relative to the solar system
C                            barycenter.
C
C                            Stellar aberration is computed for
C                            transmitted, rather than received,
C                            radiation.
C
C                            These corrections yield the analog for
C                            the transmission case of the apparent
C                            aberration correction locus.
C
C                 'XCN'      "Transmission" case: converged Newtonian
C                            light time correction. In solving the
C                            light time equation, the 'XCN' correction
C                            iterates until either the solution
C                            converges or a large iteration limit is
C                            reached. Both the position and rotation of
C                            the target body are corrected for light
C                            time.
C
C                 'XCN+S'    "Transmission" case: converged Newtonian
C                            light time and stellar aberration
C                            corrections. This option produces a
C                            solution that is at least as accurate at
C                            that obtainable with the 'XLT+S' option.
C                            Whether the 'XCN+S' solution is
C                            substantially more accurate depends on the
C                            geometry of the participating objects and
C                            on the accuracy of the input data. In some
C                            cases this routine will execute more
C                            slowly when a converged solution is
C                            computed.
C
C                            For transmission-case applications where
C                            aberration corrections are applied, this
C                            option should be used, unless the
C                            magnitudes of the corrections are
C                            negligible.
C
C              Case and embedded blanks are not significant in
C              ABCORR. For example, the string
C
C                 'Cn + s'
C
C              is valid.
C
C     CORLOC   specifies the aberration correction "locus," which is
C              the fixed point in the frame designated by FIXREF for
C              which light time and stellar aberration corrections are
C              computed.
C
C              Differential aberration effects across the surface of
C              the target body are not considered by this routine. When
C              aberration corrections are used, the effective positions
C              of the observer and target, and the orientation of the
C              target, are computed according to the corrections
C              determined for the aberration correction locus.
C
C              The light time used to determine the position and
C              orientation of the target body is that between the
C              aberration correction locus and the observer.
C
C              The stellar aberration correction applied to the
C              position of the target is that computed for the
C              aberration correction locus.
C
C              The descriptions below apply only when aberration
C              corrections are used.
C
C              The values and meanings of CORLOC are:
C
C                 'TANGENT POINT'    Compute corrections at the
C                                    "tangent point," which is the
C                                    point on the ray, defined by DREF
C                                    and DVEC, nearest to the target's
C                                    surface.
C
C                 'SURFACE POINT'    Compute corrections at the
C                                    point on the target's surface
C                                    nearest to the tangent point.
C
C              Case and leading and trailing blanks are not significant
C              in CORLOC.
C
C     OBSRVR   is the name of the observing body. This is typically
C              a spacecraft or a surface point on an extended
C              ephemeris object. OBSRVR is case-insensitive, and
C              leading and trailing blanks in OBSRVR are not
C              significant. Optionally, you may supply a string
C              containing the integer ID code for the object. For
C              example both 'MOON' and '301' are legitimate strings
C              that indicate the Moon is the observer.
C
C              If the observer is identified by name rather than ID
C              code, the observer name must be recognized by SPICE. See
C              the $Files section below.
C
C     DREF     is the name of the reference frame relative to which
C              the ray direction vector is expressed. This may be
C              any frame supported by the SPICE system, including
C              built-in frames (documented in the Frames Required
C              Reading) and frames defined by a loaded frame kernel
C              (FK). The string DREF is case-insensitive, and
C              leading and trailing blanks in DREF are not
C              significant.
C
C              When DREF designates a non-inertial frame, the
C              orientation of the frame is evaluated at an epoch
C              dependent on the frame's center and, if the center is
C              not the observer, on the selected aberration
C              correction. See the description of the direction
C              vector DVEC for details.
C
C     DVEC     is a ray direction vector emanating from the observer.
C              The tangent point on the ray and the point on the target
C              body's surface nearest to the tangent point are sought.
C
C              DVEC is specified relative to the reference frame
C              designated by DREF.
C
C              Non-inertial reference frames are treated as follows:
C              if the center of the frame is at the observer's
C              location, the frame's orientation is evaluated at ET.
C              If the frame's center is located elsewhere, then
C              letting LTCENT be the one-way light time between the
C              observer and the central body associated with the
C              frame, the orientation of the frame is evaluated at
C              ET-LTCENT, ET+LTCENT, or ET depending on whether the
C              requested aberration correction is, respectively, for
C              received radiation, transmitted radiation, or is
C              omitted. LTCENT is computed using the method
C              indicated by ABCORR.
C
C$ Detailed_Output
C
C     TANPT    is the "tangent point": the point on the ray defined by
C              DREF and DVEC nearest to the target body's surface.
C
C              TANPT is a vector originating at the target body's
C              center, expressed in the reference frame designated
C              by FIXREF, the orientation of which is evaluated at
C              TRGEPC (see description below). Units are km.
C
C              If the ray intersects the surface, TANPT is the
C              nearest point of intersection to the observer.
C
C              If the ray points away from the surface---that is, if
C              the angle between the ray and the outward normal at the
C              target surface point nearest to the observer, computed
C              using the specified aberration corrections, is less than
C              or equal to 90 degrees---then TANPT is set to the
C              position of the observer relative to the target center.
C
C              TANPT is computed using the aberration corrections
C              specified by ABCORR and CORLOC.
C
C              When the aberration correction locus is set to
C              'TANGENT POINT', and the position of TANPT is
C              corrected for aberration as specified by ABCORR, the
C              resulting point will lie on the input ray.
C
C     ALT      is the altitude of the tangent point above the
C              target body's surface. This is the distance between
C              TANPT and SRFPT. Units are km.
C
C              If the ray intersects the surface, ALT is set to the
C              exact double precision value 0.D0. ALT may be used as
C              an indicator of whether a ray-surface intersection
C              exists.
C
C     RANGE    is the distance between the observer and the tangent
C              point. Units are km.
C
C              If the ray points away from the surface (see the
C              description of TANPT above), RANGE is set to the
C              exact double precision value 0.D0. RANGE may be used
C              as an indicator of whether this geometric condition
C              exists.
C
C     SRFPT    is the point on the target body's surface nearest to the
C              tangent point.
C
C              SRFPT is a vector originating at the target body's
C              center, expressed in the reference frame designated
C              by FIXREF, the orientation of which is evaluated at
C              TRGEPC (see description below). Units are km.
C
C              SRFPT is computed using the aberration corrections
C              specified by ABCORR and CORLOC.
C
C              When the aberration correction locus is set to
C              'SURFACE POINT', and the position of SRFPT is
C              corrected for aberration as specified by ABCORR, the
C              resulting point will lie on the ray emanating from
C              the observer and pointing in the direction of SRFVEC.
C
C              If the ray intersects the surface, SRFPT is the point of
C              intersection nearest to the observer.
C
C              If the ray points away from the surface (see the
C              description of TANPT above), SRFPT is set to the target
C              surface point nearest to the observer.
C
C     TRGEPC   is the epoch associated with the aberration correction
C              locus. TRGEPC is defined as follows: letting LT be the
C              one-way light time between the observer and the
C              aberration correction locus, TRGEPC is the epoch ET-LT,
C              ET+LT, or ET depending on whether the requested
C              aberration correction is, respectively, for received
C              radiation, transmitted radiation, or omitted. LT is
C              computed using the method indicated by ABCORR.
C
C              TRGEPC is expressed as seconds past J2000 TDB.
C
C              The name TRGEPC, which stands for "target epoch,"
C              is used for compatibility with other SPICE high-level
C              geometry routines. Note that the epoch it designates
C              is not associated with the target body's center.
C
C     SRFVEC   is the vector from the observer's position at ET to
C              the surface point SRFPT, where the position of SRFPT
C              is corrected for aberrations as specified by ABCORR
C              and CORLOC. SRFVEC is expressed in the target
C              body-fixed reference frame designated by FIXREF,
C              evaluated at TRGEPC. Units are km.
C
C              One can use the SPICELIB function VNORM to obtain the
C              distance between the observer and SRFPT:
C
C                 DIST = VNORM ( SRFVEC )
C
C              The observer's position OBSPOS, relative to the
C              target body's center, where the center's position is
C              corrected for aberration effects as indicated by
C              ABCORR and CORLOC, can be computed via the call:
C
C                 CALL VSUB ( SRFPT, SRFVEC, OBSPOS )
C
C              To transform the vector SRFVEC from the reference frame
C              FIXREF at time TRGEPC to a time-dependent reference
C              frame REF at time ET, the routine PXFRM2 should be
C              called. Let XFORM be the 3x3 matrix representing the
C              rotation from the reference frame FIXREF at time
C              TRGEPC to the reference frame REF at time ET. Then
C              SRFVEC can be transformed to the result REFVEC as
C              follows:
C
C                 CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM )
C                 CALL MXV    ( XFORM,  SRFVEC, REFVEC )
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified aberration correction is unrecognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     2)  If METHOD is not equivalent to 'ELLIPSOID', when case and
C         blanks are ignored in the comparison, the error
C         SPICE(NOTSUPPORTED) is signaled.
C
C     3)  If CORLOC is not equivalent to either 'TANGENT POINT' or
C         'SURFACE POINT', when case and blanks are ignored, the
C         error SPICE(NOTSUPPORTED) is signaled.
C
C     4)  If the direction vector DVEC is the zero vector, the error
C         SPICE(ZEROVECTOR) is signaled.
C
C     5)  If either the target or observer input strings cannot be
C         converted to an integer ID code, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     6)  If OBSRVR and TARGET map to the same NAIF integer ID code,
C         the error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     7)  If triaxial radii of the target body have not been loaded
C         into the kernel pool prior to a call to this routine, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     8)  If the number of radii associated with the target body is not
C         three, the error SPICE(INVALIDCOUNT) is signaled.
C
C     9)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(NOFRAME) is signaled. A frame
C         name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     10) If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     11) If the reference frame designated by DREF is not recognized
C         by the SPICE frame subsystem, the error SPICE(NOFRAME) is
C         signaled.
C
C     12) If insufficient ephemeris data have been loaded prior to
C         calling TANGPT, an error is signaled by a routine in the call
C         tree of this routine. Note that when light time correction is
C         used, sufficient ephemeris data must be available to
C         propagate the states of both observer and target to the solar
C         system barycenter. If light time correction is used and
C         the ray's frame DREF is non-inertial, sufficient ephemeris
C         data must be available to compute the state of that frame's
C         center relative to the solar system barycenter.
C
C     13) If the target and observer have distinct identities but are
C         at the same location (for example, the target is Mars and the
C         observer is the Mars barycenter), the error
C         SPICE(NOSEPARATION) is signaled.
C
C     14) The target must be an extended body: if any of the radii of
C         the target body are non-positive, an error is signaled by a
C         routine in the call tree of this routine.
C
C     15) If the observer does not coincide with the target, but the
C         observer is located inside the ellipsoid modeling the
C         target body's shape, the error SPICE(INVALIDGEOMETRY) is
C         signaled.
C
C     16) If the transformation between the ray frame DREF and the
C         J2000 frame cannot be computed, an error is signaled by a
C         routine in the call tree of this routine.
C
C     17) If the transformation between the J2000 frame and the
C         target body-fixed, body-centered frame FIXREF cannot be
C         computed, an error is signaled by a routine in the call tree
C         of this routine.
C
C     18) If the nearest point to the target on the line containing
C         the input ray cannot be computed, an error is signaled by a
C         routine in the call tree of this routine. This type of error
C         may result from degenerate geometry; for example, if after
C         scaling the reference ellipsoid axes to make the longest
C         semi-axis a unit vector, another scaled axis is so short that
C         its squared length underflows to zero, no result can be
C         computed.
C
C     19) It is not an error for the ray to intersect the target body
C         or to point away from it so that the nearest point
C         to the ellipsoid on the line containing the ray lies behind
C         the ray's vertex.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C     -  SPK data: ephemeris data for target and observer must be
C        loaded. If aberration corrections are used, the states of
C        target and observer relative to the solar system barycenter
C        must be calculable from the available ephemeris data.
C        Typically ephemeris data are made available by loading one or
C        more SPK files via FURNSH.
C
C     -  PCK data: triaxial radii for the target body must be loaded
C        into the kernel pool. Typically this is done by loading a text
C        PCK file via FURNSH.
C
C     -  Target orientation data: rotation data for the target body
C        must be loaded. These may be provided in a text or binary PCK
C        file, or by a CK file.
C
C     The following data may be required:
C
C     -  SPK data: if aberration corrections are used, and if the ray
C        frame DREF is non-inertial, ephemeris data for that frame's
C        center must be loaded. The state of that object relative to
C        the solar system barycenter must be calculable from the
C        available ephemeris data.
C
C     -  Frame specifications: if a frame definition is required to
C        convert the observer and target states to the body-fixed frame
C        of the target, that definition must be available in the kernel
C        pool. Similarly, the frame definition required to map between
C        the frame designated by DREF and the target body-fixed frame
C        must be available. Typically the definitions of frames not
C        already built-in to SPICE are supplied by loading a frame
C        kernel.
C
C     -  Ray frame orientation data: if the frame to which DREF refers
C        is non-inertial, PCK or CK data for the frame's orientation
C        are required. If the frame is fixed to a spacecraft instrument
C        or structure, at least one CK file will be needed to permit
C        transformation of vectors between that frame and both the
C        J2000 and the target body-fixed frames.
C
C     -  Ray direction data: if the ray direction is defined by a
C        vector expressed in a spacecraft reference frame, an IK may be
C        required to provide the coordinates of the ray's direction in
C        that frame.
C
C     -  SCLK data: if a CK file is needed, an associated SCLK kernel
C        is required to enable conversion between encoded SCLK (used to
C        time-tag CK data) and barycentric dynamical time (TDB).
C
C     -  Leapseconds data: if SCLK data are needed, a leapseconds
C        kernel usually is needed as well.
C
C     -  Body name-ID mappings: if the target or observer name is
C        not built into the SPICE software, the mapping between the
C        name and the corresponding ID code must be present in the
C        kernel pool. Such mappings are usually introduced by loading
C        a frame kernel or other text kernel containing them.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     Given an observer, the direction vector of a ray emanating from
C     the observer, and an extended target body represented by a
C     triaxial ellipsoid, TANGPT computes the "tangent point": a point
C     nearest to the target body's surface nearest to the ray. The
C     corresponding surface point nearest to the tangent point is
C     computed as well.
C
C     For remote sensing observations, for maximum accuracy, reception
C     light time and stellar aberration corrections should be used.
C     These corrections model observer-target-ray geometry as it is
C     observed.
C
C     For signal transmission applications, for maximum accuracy,
C     transmission light time and stellar aberration corrections should
C     be used. These corrections model the observer-target-ray geometry
C     that applies to the transmitted signal. For example, these
C     corrections are needed to calculate the minimum altitude of the
C     signal's path over the target body.
C
C     In some cases, the magnitudes of light time and stellar
C     aberration corrections are negligible. When these corrections
C     can be ignored, significantly faster execution can be achieved
C     by setting the input ABCORR to 'NONE'.
C
C     This routine ignores differential aberration effects over the
C     target body's surface: it computes corrections only at a
C     user-specified point, which is called the "aberration correction
C     locus." The user may select either the tangent point or
C     corresponding surface point as the locus. In many cases, the
C     differences between corrections for these points are very small.
C
C     The $Examples header section below presents geometric cases for
C     which aberration correction magnitudes are significant, and cases
C     for which they are not.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following program computes tangent and surface points for
C        the MAVEN IUVS instrument. The observer is the MAVEN
C        spacecraft; the target body is Mars. The ray direction is
C        that of the boresight of the MAVEN IUVS instrument.
C
C        The aberration corrections used in this example are often
C        suitable for remote sensing observations: converged Newtonian
C        light time and stellar aberration "reception" corrections. In
C        some cases it is reasonable to omit aberration corrections;
C        see the second and third example programs below for
C        demonstrations of the effects of different aberration
C        correction choices.
C
C        In this example, the aberration correction locus is the
C        tangent point, meaning that converged light time and stellar
C        aberration corrections are computed for that point. The epoch
C        TRGEPC is used to compute the light time-corrected target
C        position and orientation, and the stellar aberration
C        correction applicable to the tangent point is applied to the
C        observer-target position vector, in order to model apparent
C        observation geometry.
C
C        Three geometric cases are covered by this example:
C
C           - The "normal" case, in which the ray defined by the
C             MAVEN IUVS boresight passes over Mars at low altitude.
C
C             In the example code, there are two computations that fall
C             into this category.
C
C           - The "intercept" case, in which the ray intersects Mars.
C
C           - The "look away" case, in which the elevation of the ray's
C             direction vector, measured from the local level plane at
C             the sub-spacecraft point, is greater than or equal to 0.
C             The aberration corrections used to compute the
C             sub-observer point for this case are those applicable to
C             the aberration correction locus.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: tangpt_ex1.tm
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
C           All kernels referenced by this meta-kernel are available
C           from the MAVEN SPICE PDS archive.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C           File name                          Contents
C           ---------                          --------
C           mar097s.bsp                        Mars satellite ephemeris
C           maven_iuvs_v11.ti                  MAVEN IUVS instrument
C                                              information
C           maven_orb_rec_201001_210101_v1.bsp MAVEN s/c ephemeris
C           mvn_v09.tf                         MAVEN frame
C                                              specifications
C           mvn_app_rel_201005_201011_v01.bc   MAVEN Articulated
C                                              Payload Platform
C                                              attitude
C           mvn_iuvs_rem_201001_201231_v01.bc  MAVEN IUVS instrument
C                                              internal mirror
C                                              attitude
C           mvn_sc_rel_201005_201011_v01.bc    MAVEN s/c attitude
C           mvn_sclkscet_00086.tsc             MAVEN SCLK coefficients
C           naif0012.tls                       Leapseconds
C           pck00010.tpc                       Planet and satellite
C                                              orientation and radii
C
C           \begindata
C
C              KERNELS_TO_LOAD = (
C                 'mar097s.bsp',
C                 'maven_iuvs_v11.ti',
C                 'maven_orb_rec_201001_210101_v1.bsp',
C                 'maven_v09.tf',
C                 'mvn_app_rel_201005_201011_v01.bc',
C                 'mvn_iuvs_rem_201001_201231_v01.bc',
C                 'mvn_sc_rel_201005_201011_v01.bc',
C                 'mvn_sclkscet_00086.tsc',
C                 'naif0012.tls',
C                 'pck00010.tpc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM TANGPT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Parameters
C        C
C              CHARACTER*(*)         FMT1F7
C              PARAMETER           ( FMT1F7 = '(1X,A,F15.7)' )
C
C              CHARACTER*(*)         FMT3F7
C              PARAMETER           ( FMT3F7 = '(1X,A,3F15.7)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'tangpt_ex1.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' )
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
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 72 )
C
C              INTEGER               LOCLEN
C              PARAMETER           ( LOCLEN = 25 )
C
C              INTEGER               NCASE
C              PARAMETER           ( NCASE  =  3 )
C
C              INTEGER               NTIMES
C              PARAMETER           ( NTIMES =  4 )
C
C              INTEGER               ROOM
C              PARAMETER           ( ROOM   = 12 )
C
C              INTEGER               SHPLEN
C              PARAMETER           ( SHPLEN = 25 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 35 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(LNSIZE)    CASENM
C              CHARACTER*(LNSIZE)    CASES  ( NCASE )
C              CHARACTER*(FRNMLN)    FIXREF
C              CHARACTER*(BDNMLN)    INSNAM
C              CHARACTER*(LOCLEN)    LOCUS
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(FRNMLN)    RAYFRM
C              CHARACTER*(SHPLEN)    SHAPE
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(TIMLEN)    UTCTIM ( NTIMES )
C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      BOUNDS ( 3, ROOM )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      RANGE
C              DOUBLE PRECISION      RAYDIR ( 3 )
C              DOUBLE PRECISION      SRFPT  ( 3 )
C              DOUBLE PRECISION      SRFVEC ( 3 )
C              DOUBLE PRECISION      TANPT  ( 3 )
C              DOUBLE PRECISION      TRGEPC
C
C              INTEGER               I
C              INTEGER               NVEC
C
C        C
C        C     Initial values
C        C
C              DATA                  CASES /
C             .                      'Ray slightly above limb',
C             .                      'Intercept',
C             .                      'Look-away'    /
C
C              DATA                  INSNAM / 'MAVEN_IUVS' /
C
C              DATA                  UTCTIM /
C             .            '2020-10-11 16:01:43.000000 UTC',
C             .            '2020-10-11 16:17:43.000000 UTC',
C             .            '2020-10-11 16:49:07.000000 UTC',
C             .            '2020-10-11 17:12:08.000000 UTC' /
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( META )
C
C              WRITE(*,*) ' '
C              WRITE(*, '(A)') 'Instrument: ' // INSNAM
C              WRITE(*,*) ' '
C
C        C
C        C     Get the instrument reference frame name and
C        C     the instrument boresight direction in the
C        C     instrument frame.
C        C
C              CALL GETFVN ( INSNAM, ROOM,   SHAPE,
C             .              RAYFRM, RAYDIR, NVEC, BOUNDS )
C
C        C
C        C     Initialize inputs to TANGPT, except for time.
C        C
C              TARGET = 'MARS'
C              OBSRVR = 'MAVEN'
C              FIXREF = 'IAU_MARS'
C              ABCORR = 'CN+S'
C              LOCUS  = 'TANGENT POINT'
C
C        C
C        C     Compute the apparent tangent point for each time.
C        C
C              WRITE(*,'(A)') 'Aberration correction:       ' // ABCORR
C              WRITE(*,'(A)') 'Aberration correction locus: ' // LOCUS
C
C              DO I = 1, NTIMES
C
C                 CALL STR2ET ( UTCTIM(I), ET )
C
C                 CALL TANGPT ( 'ELLIPSOID',
C             .                 TARGET, ET,     FIXREF, ABCORR,
C             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR,
C             .                 TANPT,  ALT,    RANGE,  SRFPT,
C             .                 TRGEPC, SRFVEC                 )
C
C        C
C        C        Set the label for the geometric case.
C        C
C                 IF ( ALT .EQ. 0 ) THEN
C
C                    CASENM = CASES(2)
C
C                 ELSE IF ( RANGE .EQ. 0.D0 ) THEN
C
C                    CASENM = CASES(3)
C                 ELSE
C                    CASENM = CASES(1)
C                 END IF
C
C        C
C        C        Convert the target epoch to a string for output.
C        C
C                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR )
C
C                 WRITE(*,*) ' '
C
C                 WRITE( *, '(A)') '  Observation Time = ' // UTCTIM(I)
C                 WRITE( *, '(A)') '  Target Time      = ' // TIMSTR
C
C                 WRITE(*, FMT1F7) '   ALT    (km) = ', ALT
C                 WRITE(*, FMT1F7) '   RANGE  (km) = ', RANGE
C                 WRITE(*, FMT3F7) '   TANPT  (km) = ', TANPT
C                 WRITE(*, FMT3F7) '   SRFPT  (km) = ', SRFPT
C                 WRITE(*, FMT3F7) '   SRFVEC (km) = ', SRFVEC
C
C                 WRITE( *, '(A)') '    Geometric case = ' // CASENM
C
C              END DO
C
C              WRITE(*,*) ' '
C
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran/64-bit
C        platform, the output was:
C
C
C        Instrument: MAVEN_IUVS
C
C        Aberration correction:       CN+S
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-10-11 16:01:43.000000 UTC
C          Target Time      = 2020-10-11 16:01:42.983021 UTC
C            ALT    (km) =      99.4262977
C            RANGE  (km) =    5090.1928435
C            TANPT  (km) =   -2273.0408575   1072.4423944  -2415.6104827
C            SRFPT  (km) =   -2208.5678350   1042.0234063  -2346.3031728
C            SRFVEC (km) =   -2138.0677257   3050.4078643   3470.3929222
C            Geometric case = Ray slightly above limb
C
C          Observation Time = 2020-10-11 16:17:43.000000 UTC
C          Target Time      = 2020-10-11 16:17:42.993820 UTC
C            ALT    (km) =       0.0000000
C            RANGE  (km) =    1852.8381880
C            TANPT  (km) =     752.0909507  -1781.3912506  -2775.5390159
C            SRFPT  (km) =     752.0909507  -1781.3912506  -2775.5390159
C            SRFVEC (km) =    -700.9743439   1162.4766255   1261.0679662
C            Geometric case = Intercept
C
C          Observation Time = 2020-10-11 16:49:07.000000 UTC
C          Target Time      = 2020-10-11 16:49:06.998907 UTC
C            ALT    (km) =     218.2661426
C            RANGE  (km) =     327.7912133
C            TANPT  (km) =    2479.8672359  -1772.2350525   1931.8678816
C            SRFPT  (km) =    2330.3561559  -1665.3870838   1814.0966731
C            SRFVEC (km) =      77.3692694    325.9571470   -207.0099587
C            Geometric case = Ray slightly above limb
C
C          Observation Time = 2020-10-11 17:12:08.000000 UTC
C          Target Time      = 2020-10-11 17:12:08.000000 UTC
C            ALT    (km) =     969.2772042
C            RANGE  (km) =       0.0000000
C            TANPT  (km) =     -58.1087763   2034.6474343   3844.2010767
C            SRFPT  (km) =     -45.2530638   1584.5115999   2985.8825113
C            SRFVEC (km) =      12.8557125   -450.1358344   -858.3185654
C            Geometric case = Look-away
C
C
C     2) The following program computes tangent and surface points for
C        the MRO MCS A1 instrument, for a single epoch. The observer is
C        the MRO spacecraft; the target body is Mars. The ray direction
C        is that of the boresight of the MRO MCS A1 instrument.
C
C        The aberration corrections used in this example are converged
C        Newtonian light time and stellar aberration corrections,
C        converged Newtonian light time alone, and "none."
C
C        For remote sensing observations made by a spacecraft in low
C        orbit about Mars, both the combination of light time and
C        stellar aberration corrections and omission of aberration
C        corrections may be valid. See the output of this program and
C        of the third example program below for examples of how results
C        differ due to the choice of aberration corrections.
C
C        Use of light time corrections alone is presented to
C        illustrate, by way of contrast, the effect of this choice.
C        This choice can be useful for testing but is unlikely to be
C        correct for modeling actual observation geometry.
C
C        Separate computations are performed using both the tangent
C        point and the corresponding surface point---the nearest point
C        on the target surface to the tangent point---as the aberration
C        correction locus.
C
C        Three geometric cases are covered by this example:
C
C           - The "normal" case, in which the ray defined by the
C             MRO MCS A1 boresight passes over Mars at low altitude.
C
C             In the example code, there are two computations that fall
C             into this category.
C
C           - The "intercept" case, in which the ray intersects Mars.
C
C           - The "look away" case, in which the elevation of the ray's
C             direction vector, measured from the local level plane at
C             the sub-spacecraft point, is greater than or equal to 0.
C             The target position and orientation used for this
C             computation are the same as those used to compute the
C             aberration correction locus.
C
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: tangpt_ex2.tm
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
C           All kernels referenced by this meta-kernel are available
C           from the MRO SPICE PDS archive.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                       Contents
C              ---------                       --------
C              mar097.bsp                      Mars satellite ephemeris
C              mro_mcs_psp_201001_201031.bc    MRO MCS attitude
C              mro_mcs_v10.ti                  MRO MCS instrument
C                                              information
C              mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris
C              mro_sc_psp_201027_201102.bc     MRO s/c bus attitude
C              mro_sclkscet_00095_65536.tsc    MRO SCLK coefficients
C              mro_v16.tf                      MRO frame specifications
C              naif0012.tls                    Leapseconds
C              pck00008.tpc                    Planet and satellite
C                                              orientation and radii
C
C           \begindata
C
C              KERNELS_TO_LOAD = (
C                 'mar097.bsp',
C                 'mro_mcs_psp_201001_201031.bc',
C                 'mro_mcs_v10.ti',
C                 'mro_psp57_ssd_mro95a.bsp',
C                 'mro_sc_psp_201027_201102.bc',
C                 'mro_sclkscet_00095_65536.tsc',
C                 'mro_v16.tf',
C                 'naif0012.tls',
C                 'pck00008.tpc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM TANGPT_EX2
C              IMPLICIT NONE
C
C        C
C        C     Parameters
C        C
C              CHARACTER*(*)         FMT1F7
C              PARAMETER           ( FMT1F7  = '(1X,A,F15.7)' )
C
C              CHARACTER*(*)         FMT3F7
C              PARAMETER           ( FMT3F7  = '(1X,A,3F15.7)' )
C
C              CHARACTER*(*)         FMT1F4
C              PARAMETER           ( FMT1F4 = '(1X,A,F10.4)' )
C
C              CHARACTER*(*)         FMT3F4
C              PARAMETER           ( FMT3F4 = '(1X,A,3F10.4)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'tangpt_ex2.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' )
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
C              INTEGER               LOCLEN
C              PARAMETER           ( LOCLEN = 25 )
C
C              INTEGER               NCASE
C              PARAMETER           ( NCASE  =  5 )
C
C              INTEGER               ROOM
C              PARAMETER           ( ROOM   =  4 )
C
C              INTEGER               SHPLEN
C              PARAMETER           ( SHPLEN = 25 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 35 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(CORLEN)    CORRS  ( NCASE )
C              CHARACTER*(FRNMLN)    FIXREF
C              CHARACTER*(BDNMLN)    INSNAM
C              CHARACTER*(LOCLEN)    LOCI   ( NCASE )
C              CHARACTER*(LOCLEN)    LOCUS
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(FRNMLN)    RAYFRM
C              CHARACTER*(SHPLEN)    SHAPE
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(TIMLEN)    UTCTIM
C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      BOUNDS ( 3, ROOM )
C              DOUBLE PRECISION      DIFF   ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      RANGE
C              DOUBLE PRECISION      RAYDIR ( 3 )
C              DOUBLE PRECISION      SRFPT  ( 3 )
C              DOUBLE PRECISION      SRFVEC ( 3 )
C              DOUBLE PRECISION      SVALT
C              DOUBLE PRECISION      SVEPOC
C              DOUBLE PRECISION      SVRANG
C              DOUBLE PRECISION      SVSRFP ( 3 )
C              DOUBLE PRECISION      SVSRFV ( 3 )
C              DOUBLE PRECISION      SVTANP ( 3 )
C              DOUBLE PRECISION      TANPT  ( 3 )
C              DOUBLE PRECISION      TRGEPC
C
C              INTEGER               I
C              INTEGER               NVEC
C
C        C
C        C     Initial values
C        C
C              DATA                  CORRS  / 'CN+S', 'CN+S',
C             .                               'CN',   'CN',
C             .                               'NONE'           /
C
C              DATA                  INSNAM / 'MRO_MCS_A1'     /
C
C              DATA                  LOCI   / 'TANGENT POINT',
C             .                               'SURFACE POINT',
C             .                               'TANGENT POINT',
C             .                               'SURFACE POINT',
C             .                               'TANGENT POINT'  /
C
C              DATA                  UTCTIM /
C             .              '2020-10-31 00:01:23.111492 UTC'  /
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( META )
C
C              WRITE( *, * ) ' '
C              WRITE( *, '(A)') 'Instrument: ' // INSNAM
C
C        C
C        C     Get the instrument reference frame name and
C        C     the instrument boresight direction in the
C        C     instrument frame.
C        C
C              CALL GETFVN ( INSNAM, ROOM,   SHAPE,
C             .              RAYFRM, RAYDIR, NVEC, BOUNDS )
C
C        C
C        C     Initialize inputs to TANGPT that are common to all
C        C     cases.
C        C
C              TARGET = 'MARS'
C              OBSRVR = 'MRO'
C              FIXREF = 'IAU_MARS'
C
C        C
C        C     Compute the apparent tangent point for each case.
C        C
C              DO I = 1, NCASE
C
C                 WRITE(*,*) ' '
C
C                 ABCORR = CORRS(I)
C                 LOCUS  = LOCI(I)
C
C                 WRITE(*, '(A)') 'Aberration correction:       '
C             .   //               ABCORR
C
C                 WRITE(*, '(A)') 'Aberration correction locus: '
C             .   //               LOCUS
C
C                 WRITE(*,*) ' '
C
C                 CALL STR2ET ( UTCTIM, ET )
C
C                 CALL TANGPT ( 'ELLIPSOID',
C             .                 TARGET, ET,     FIXREF, ABCORR,
C             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR,
C             .                 TANPT,  ALT,    RANGE,  SRFPT,
C             .                 TRGEPC, SRFVEC                 )
C
C        C
C        C        Convert the target epoch to a string for output.
C        C
C                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR )
C
C                 WRITE(*, '(A)') '  Observation Time = '
C             .   //               UTCTIM
C
C                 WRITE(*, '(A)') '  Target Time      = '
C             .   //                 TIMSTR
C
C                 WRITE(*, FMT1F7) '   ALT    (km) = ', ALT
C                 WRITE(*, FMT1F7) '   RANGE  (km) = ', RANGE
C                 WRITE(*, FMT3F7) '   TANPT  (km) = ', TANPT
C                 WRITE(*, FMT3F7) '   SRFPT  (km) = ', SRFPT
C                 WRITE(*, FMT3F7) '   SRFVEC (km) = ', SRFVEC
C
C                 IF ( I .EQ. 1 ) THEN
C        C
C        C           Save results for comparison.
C        C
C                    SVALT  = ALT
C                    SVEPOC = TRGEPC
C                    SVRANG = RANGE
C                    CALL VEQU( TANPT,  SVTANP )
C                    CALL VEQU( SRFPT,  SVSRFP )
C                    CALL VEQU( SRFVEC, SVSRFV )
C
C                 ELSE
C        C
C        C           Compare results to CN+S, tangent point
C        C           locus case.
C        C
C                    WRITE(*,*) ' '
C
C                    WRITE(*, '(A)')
C             .      '  Differences from case 1 outputs:'
C
C                    WRITE(*, FMT1F4)
C             .      '   Target time delta (ms) = ',
C             .      1.D3 * ( TRGEPC - SVEPOC )
C
C                    WRITE(*, FMT1F4)
C             .      '   ALT    delta (m) = ',
C             .      1.D3 * ( ALT - SVALT )
C
C                    WRITE(*, FMT1F4)
C             .      '   RANGE  delta (m) = ',
C             .      1.D3 * ( RANGE - SVRANG  )
C
C                    CALL VSUB   ( TANPT, SVTANP, DIFF )
C                    CALL VSCLIP ( 1.D3,  DIFF )
C                    WRITE(*, FMT3F4)
C             .      '   TANPT  delta (m) = ', DIFF
C
C                    CALL VSUB   ( SRFPT, SVSRFP, DIFF )
C                    CALL VSCLIP ( 1.D3,  DIFF )
C                    WRITE(*, FMT3F4)
C             .      '   SRFPT  delta (m) = ', DIFF
C
C                    CALL VSUB   ( SRFVEC, SVSRFV, DIFF )
C                    CALL VSCLIP ( 1.D3,   DIFF )
C                    WRITE(*, FMT3F4)
C             .      '   SRFVEC delta (m) = ', DIFF
C
C                 END IF
C
C                 WRITE(*,*) ' '
C
C              END DO
C
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran/64-bit
C        platform, the output was:
C
C
C        Instrument: MRO_MCS_A1
C
C        Aberration correction:       CN+S
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-10-31 00:01:23.111492 UTC
C          Target Time      = 2020-10-31 00:01:23.106946 UTC
C            ALT    (km) =      39.1034486
C            RANGE  (km) =    1362.8659249
C            TANPT  (km) =   -2530.9040220  -1630.9806346   1644.3612074
C            SRFPT  (km) =   -2502.1342299  -1612.4406294   1625.4496512
C            SRFVEC (km) =    -589.3842679   -234.0892764  -1206.9635473
C
C
C        Aberration correction:       CN+S
C        Aberration correction locus: SURFACE POINT
C
C          Observation Time = 2020-10-31 00:01:23.111492 UTC
C          Target Time      = 2020-10-31 00:01:23.106944 UTC
C            ALT    (km) =      39.1014434
C            RANGE  (km) =    1362.8679108
C            TANPT  (km) =   -2530.9025464  -1630.9796845   1644.3602376
C            SRFPT  (km) =   -2502.1342295  -1612.4406300   1625.4496511
C            SRFVEC (km) =    -589.3866439   -234.0905954  -1206.9643086
C
C          Differences from case 1 outputs:
C            Target time delta (ms) =    -0.0019
C            ALT    delta (m) =    -2.0052
C            RANGE  delta (m) =     1.9859
C            TANPT  delta (m) =     1.4757    0.9501   -0.9698
C            SRFPT  delta (m) =     0.0004   -0.0006   -0.0000
C            SRFVEC delta (m) =    -2.3760   -1.3189   -0.7614
C
C
C        Aberration correction:       CN
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-10-31 00:01:23.111492 UTC
C          Target Time      = 2020-10-31 00:01:23.106946 UTC
C            ALT    (km) =      39.1714711
C            RANGE  (km) =    1362.8658567
C            TANPT  (km) =   -2530.9135880  -1631.0820975   1644.3878335
C            SRFPT  (km) =   -2502.0942100  -1612.5090527   1625.4434517
C            SRFVEC (km) =    -589.3346511   -234.0562242  -1206.9963133
C
C          Differences from case 1 outputs:
C            Target time delta (ms) =     0.0000
C            ALT    delta (m) =    68.0225
C            RANGE  delta (m) =    -0.0683
C            TANPT  delta (m) =    -9.5660 -101.4629   26.6261
C            SRFPT  delta (m) =    40.0199  -68.4233   -6.1994
C            SRFVEC delta (m) =    49.6168   33.0522  -32.7661
C
C
C        Aberration correction:       CN
C        Aberration correction locus: SURFACE POINT
C
C          Observation Time = 2020-10-31 00:01:23.111492 UTC
C          Target Time      = 2020-10-31 00:01:23.106944 UTC
C            ALT    (km) =      39.1714973
C            RANGE  (km) =    1362.8658326
C            TANPT  (km) =   -2530.9135902  -1631.0821391   1644.3878436
C            SRFPT  (km) =   -2502.0941931  -1612.5090815   1625.4434492
C            SRFVEC (km) =    -589.3346210   -234.0562071  -1206.9963050
C
C          Differences from case 1 outputs:
C            Target time delta (ms) =    -0.0019
C            ALT    delta (m) =    68.0487
C            RANGE  delta (m) =    -0.0924
C            TANPT  delta (m) =    -9.5682 -101.5045   26.6362
C            SRFPT  delta (m) =    40.0368  -68.4521   -6.2020
C            SRFVEC delta (m) =    49.6469   33.0694  -32.7577
C
C
C        Aberration correction:       NONE
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-10-31 00:01:23.111492 UTC
C          Target Time      = 2020-10-31 00:01:23.111492 UTC
C            ALT    (km) =      39.1090103
C            RANGE  (km) =    1362.9233525
C            TANPT  (km) =   -2530.9082604  -1630.9831041   1644.3638384
C            SRFPT  (km) =   -2502.1343747  -1612.4404639   1625.4495931
C            SRFVEC (km) =    -589.4063032   -234.0970874  -1207.0162978
C
C          Differences from case 1 outputs:
C            Target time delta (ms) =     4.5460
C            ALT    delta (m) =     5.5616
C            RANGE  delta (m) =    57.4276
C            TANPT  delta (m) =    -4.2384   -2.4695    2.6310
C            SRFPT  delta (m) =    -0.1448    0.1655   -0.0581
C            SRFVEC delta (m) =   -22.0352   -7.8109  -52.7505
C
C
C     3) The following program computes tangent and surface points for
C        a ray pointing from the Goldstone DSN station DSS-14 to the
C        location of the MRO spacecraft, for a single epoch. The target
C        body is Mars.
C
C        The aberration corrections used in this example are
C
C           CN+S
C           XCN+S
C           CN
C           NONE
C
C        Results using CN+S corrections are computed for both locus
C        choices: TANGENT POINT and SURFACE POINT.
C
C        For each case other than the one using CN+S corrections for
C        the TANGENT POINT locus, differences between results for the
C        former and latter case are shown.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: tangpt_ex3.tm
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
C           All kernels referenced by this meta-kernel are available
C           from the NAIF SPICE server in the generic kernels area
C           or from the MRO SPICE PDS archive.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                       Contents
C              ---------                       --------
C              mar097.bsp                      Mars satellite ephemeris
C              mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris
C              earthstns_itrf93_201023.bsp     DSN station locations
C              naif0012.tls                    Leapseconds
C              pck00010.tpc                    Planet and satellite
C                                              orientation and radii
C              earth_latest_high_prec.bpc      High accuracy Earth
C                                              attitude
C
C           \begindata
C
C              KERNELS_TO_LOAD = (
C                 'mar097.bsp'
C                 'mro_psp57_ssd_mro95a.bsp'
C                 'earthstns_itrf93_201023.bsp'
C                 'naif0012.tls'
C                 'pck00010.tpc'
C                 'earth_latest_high_prec.bpc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM TANGPT_EX3
C              IMPLICIT NONE
C
C        C
C        C     Parameters
C        C
C              CHARACTER*(*)         FMT1F3
C              PARAMETER           ( FMT1F3  = '(1X,A,F14.3)' )
C
C              CHARACTER*(*)         FMT3F3
C              PARAMETER           ( FMT3F3  = '(1X,A,3F14.3)' )
C
C              CHARACTER*(*)         FMT1F6
C              PARAMETER           ( FMT1F6  = '(1X,A,F14.6)' )
C
C              CHARACTER*(*)         FMTMF3
C              PARAMETER           ( FMTMF3 = '(1X,A,2F13.3,F10.3)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'tangpt_ex3.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' )
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
C              INTEGER               LOCLEN
C              PARAMETER           ( LOCLEN = 25 )
C
C              INTEGER               NCASE
C              PARAMETER           ( NCASE  =  5 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 35 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(CORLEN)    CORRS  ( NCASE )
C              CHARACTER*(FRNMLN)    FIXREF
C              CHARACTER*(LOCLEN)    LOCI   ( NCASE )
C              CHARACTER*(LOCLEN)    LOCUS
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(FRNMLN)    RAYFRM
C              CHARACTER*(BDNMLN)    SC
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(TIMLEN)    UTCTIM
C
C              DOUBLE PRECISION      ALT
C              DOUBLE PRECISION      DIFF   ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      RANGE
C              DOUBLE PRECISION      RAYDIR ( 3 )
C              DOUBLE PRECISION      RAYLT
C              DOUBLE PRECISION      SRFPT  ( 3 )
C              DOUBLE PRECISION      SRFVEC ( 3 )
C              DOUBLE PRECISION      SVALT
C              DOUBLE PRECISION      SVEPOC
C              DOUBLE PRECISION      SVRANG
C              DOUBLE PRECISION      SVSRFP ( 3 )
C              DOUBLE PRECISION      SVSRFV ( 3 )
C              DOUBLE PRECISION      SVTANP ( 3 )
C              DOUBLE PRECISION      TANPT  ( 3 )
C              DOUBLE PRECISION      TRGEPC
C
C              INTEGER               I
C
C        C
C        C     Initial values
C        C
C              DATA                  CORRS  / 'CN+S', 'XCN+S',
C             .                               'CN',   'NONE',
C             .                               'CN+S'          /
C
C              DATA                  LOCI   / 'TANGENT POINT',
C             .                               'TANGENT POINT',
C             .                               'TANGENT POINT',
C             .                               'TANGENT POINT',
C             .                               'SURFACE POINT'  /
C
C              DATA                  UTCTIM /
C             .              '2020-12-30 00:00:00 UTC'  /
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Set name of spacecraft used to define ray direction.
C        C
C              SC = 'MRO'
C
C        C
C        C     Initialize inputs to TANGPT that are common to all
C        C     cases.
C        C
C              TARGET = 'MARS'
C              OBSRVR = 'DSS-14'
C              FIXREF = 'IAU_MARS'
C              RAYFRM = 'J2000'
C
C        C
C        C     Convert observation time to TDB seconds past J2000.
C        C
C              CALL STR2ET ( UTCTIM, ET )
C
C        C
C        C     Generate ray direction vector. Use apparent position
C        C     of the MRO spacecraft.
C        C
C              CALL SPKPOS ( SC,     ET,     RAYFRM,
C             .              'CN+S', OBSRVR, RAYDIR, RAYLT )
C
C              WRITE(*,*) ' '
C              WRITE( *, '(A)') 'Observer:   ' // OBSRVR
C              WRITE( *, '(A)') 'Target:     ' // TARGET
C              WRITE( *, '(A)') 'Spacecraft: ' // SC
C
C        C
C        C     Compute the apparent tangent point for each case.
C        C
C              DO I = 1, NCASE
C
C                 WRITE(*,*) ' '
C
C                 ABCORR = CORRS(I)
C                 LOCUS  = LOCI(I)
C
C                 WRITE(*, '(A)') 'Aberration correction:       '
C             .   //               ABCORR
C
C                 WRITE(*, '(A)') 'Aberration correction locus: '
C             .   //               LOCUS
C
C                 WRITE(*,*) ' '
C
C        C
C        C        Compute tangent point.
C        C
C                 CALL TANGPT ( 'ELLIPSOID',
C             .                 TARGET, ET,     FIXREF, ABCORR,
C             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR,
C             .                 TANPT,  ALT,    RANGE,  SRFPT,
C             .                 TRGEPC, SRFVEC                 )
C
C        C
C        C        Convert the target epoch to a string for output.
C        C
C                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR )
C
C                 WRITE(*, '(A)') '  Observation Time = '
C             .   //               UTCTIM
C
C                 WRITE(*, '(A)') '  Target Time      = '
C             .   //                 TIMSTR
C
C                 WRITE(*, FMT1F3) '   ALT    (km) = ', ALT
C                 WRITE(*, FMT1F3) '   RANGE  (km) = ', RANGE
C                 WRITE(*, FMT3F3) '   TANPT  (km) = ', TANPT
C                 WRITE(*, FMT3F3) '   SRFPT  (km) = ', SRFPT
C                 WRITE(*, FMT3F3) '   SRFVEC (km) = ', SRFVEC
C
C                 IF ( I .EQ. 1 ) THEN
C        C
C        C           Save results for comparison.
C        C
C                    SVALT  = ALT
C                    SVEPOC = TRGEPC
C                    SVRANG = RANGE
C                    CALL VEQU( TANPT,  SVTANP )
C                    CALL VEQU( SRFPT,  SVSRFP )
C                    CALL VEQU( SRFVEC, SVSRFV )
C
C                 ELSE
C        C
C        C           Compare results to CN+S, tangent point
C        C           locus case.
C        C
C                    WRITE(*,*) ' '
C
C                    WRITE(*, '(A)')
C             .      '  Differences from case 1 outputs:'
C
C                    WRITE(*, FMT1F6)
C             .      '   Target time delta (s) = ',
C             .      TRGEPC - SVEPOC
C
C                    WRITE(*, FMTMF3)
C             .      '   ALT    delta (km) = ', ALT - SVALT
C
C                    WRITE(*, FMTMF3)
C             .      '   RANGE  delta (km) = ', RANGE - SVRANG
C
C                    CALL VSUB   ( TANPT, SVTANP, DIFF )
C                    WRITE(*, FMTMF3)
C             .      '   TANPT  delta (km) = ', DIFF
C
C                    CALL VSUB   ( SRFPT, SVSRFP, DIFF )
C                    WRITE(*, FMTMF3)
C             .      '   SRFPT  delta (km) = ', DIFF
C
C                    CALL VSUB   ( SRFVEC, SVSRFV, DIFF )
C                    WRITE(*, FMTMF3)
C             .      '   SRFVEC delta (km) = ', DIFF
C
C                 END IF
C
C                 WRITE(*,*) ' '
C
C              END DO
C
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran/64-bit
C        platform, the output was:
C
C
C        Observer:   DSS-14
C        Target:     MARS
C        Spacecraft: MRO
C
C        Aberration correction:       CN+S
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-12-30 00:00:00 UTC
C          Target Time      = 2020-12-29 23:52:40.613204 UTC
C            ALT    (km) =        140.295
C            RANGE  (km) =  131724847.608
C            TANPT  (km) =       1351.574      1182.155     -3029.495
C            SRFPT  (km) =       1298.181      1135.455     -2908.454
C            SRFVEC (km) =  121233989.354  -5994858.328  51164606.676
C
C
C        Aberration correction:       XCN+S
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-12-30 00:00:00 UTC
C          Target Time      = 2020-12-30 00:07:19.347692 UTC
C            ALT    (km) =       4921.539
C            RANGE  (km) =  131713124.520
C            TANPT  (km) =       -413.404     -8220.856     -1193.471
C            SRFPT  (km) =       -168.808     -3356.879      -483.938
C            SRFVEC (km) =  120615301.766 -13523495.083  51160641.665
C
C          Differences from case 1 outputs:
C            Target time delta (s) =     878.734488
C            ALT    delta (km) =      4781.244
C            RANGE  delta (km) =    -11723.089
C            TANPT  delta (km) =     -1764.978    -9403.011  1836.024
C            SRFPT  delta (km) =     -1466.989    -4492.334  2424.517
C            SRFVEC delta (km) =   -618687.588 -7528636.755 -3965.012
C
C
C        Aberration correction:       CN
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-12-30 00:00:00 UTC
C          Target Time      = 2020-12-29 23:52:40.613219 UTC
C            ALT    (km) =       3409.162
C            RANGE  (km) =  131724843.177
C            TANPT  (km) =       1933.641      5183.696     -3951.091
C            SRFPT  (km) =        965.945      2589.501     -1962.095
C            SRFVEC (km) =  121233070.966  -5997405.747  51166472.910
C
C          Differences from case 1 outputs:
C            Target time delta (s) =       0.000015
C            ALT    delta (km) =      3268.868
C            RANGE  delta (km) =        -4.431
C            TANPT  delta (km) =       582.067     4001.541  -921.596
C            SRFPT  delta (km) =      -332.236     1454.046   946.360
C            SRFVEC delta (km) =      -918.388    -2547.420  1866.234
C
C
C        Aberration correction:       NONE
C        Aberration correction locus: TANGENT POINT
C
C          Observation Time = 2020-12-30 00:00:00 UTC
C          Target Time      = 2020-12-30 00:00:00.000000 UTC
C            ALT    (km) =        781.382
C            RANGE  (km) =  131718986.013
C            TANPT  (km) =        615.190     -3545.867     -2111.285
C            SRFPT  (km) =        500.266     -2883.463     -1713.075
C            SRFVEC (km) =  120983074.323  -9765994.151  51162607.074
C
C          Differences from case 1 outputs:
C            Target time delta (s) =     439.386796
C            ALT    delta (km) =       641.087
C            RANGE  delta (km) =     -5861.595
C            TANPT  delta (km) =      -736.384    -4728.022   918.210
C            SRFPT  delta (km) =      -797.915    -4018.919  1195.379
C            SRFVEC delta (km) =   -250915.031 -3771135.823 -1999.603
C
C
C        Aberration correction:       CN+S
C        Aberration correction locus: SURFACE POINT
C
C          Observation Time = 2020-12-30 00:00:00 UTC
C          Target Time      = 2020-12-29 23:52:40.613204 UTC
C            ALT    (km) =        140.308
C            RANGE  (km) =  131724847.611
C            TANPT  (km) =       1351.579      1182.159     -3029.507
C            SRFPT  (km) =       1298.181      1135.455     -2908.454
C            SRFVEC (km) =  121233989.351  -5994858.332  51164606.689
C
C          Differences from case 1 outputs:
C            Target time delta (s) =       0.000000
C            ALT    delta (km) =         0.013
C            RANGE  delta (km) =         0.003
C            TANPT  delta (km) =         0.005        0.004    -0.012
C            SRFPT  delta (km) =        -0.000        0.000     0.000
C            SRFVEC delta (km) =        -0.003       -0.005     0.013
C
C
C$ Restrictions
C
C     1)  This routine is applicable only to computations for which
C         radiation paths can be modeled as straight lines.
C
C     2)  This routine does not account for differential aberration
C         corrections across the target body surface: when aberration
C         corrections are used, the entire target ellipsoid's position
C         and orientation are modified by the corrections that apply at
C         the aberration correction locus.
C
C     3)  A cautionary note: if aberration corrections are used, and if
C         DREF is the target body-fixed frame, the epoch at which that
C         frame is evaluated is offset from ET by the light time
C         between the observer and the *center* of the target body.
C         This light time normally will differ from the light time
C         between the observer and the tangent or surface point.
C         Consequently the orientation of the target body-fixed frame
C         at TRGEPC will not match that of the target body-fixed frame
C         at the epoch associated with DREF. As a result, various
C         derived quantities may not be as expected: for example,
C         SRFVEC would not be parallel to DVEC.
C
C         In many applications the errors arising from this frame
C         discrepancy may be insignificant; however a safe approach is
C         to always use as DREF a frame other than the target
C         body-fixed frame.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     M. Costa Sitja     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-OCT-2021 (NJB) (MCS)
C
C-&


C$ Index_Entries
C
C     find ray-ellipsoid tangent point
C     find nearest point to ray on ellipsoid
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VREL

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C

C
C     Limit on relative error in light time; this is used to terminate
C     the solution loop.
C
C     This value is meant to ensure timely loop termination in cases
C     where extended precision computations cause successive values to
C     differ by amounts having magnitude less than the minimum
C     relative value representable by IEEE-754 conforming, 64-bit
C     double precision floating point numbers.
C
C     The convergence tests used here are not necessarily sensitive to
C     use of extended precision, but it is possible that future changes
C     to the code could make them so.
C
C     In most situations, use of this value enforces convergence. In
C     rare cases, successive approximate solutions will differ by
C     small, non-zero amounts but will not converge. In those cases,
C     iteration will be terminated when the iteration count limit,
C     which is dependent on the choice of aberration correction, is
C     reached.
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-16 )

C
C     Upper bound on converged solution iterations.
C
      INTEGER               MXCVIT
      PARAMETER           ( MXCVIT = 10 )

C
C     Upper bound on non-converged solution iterations.
C
      INTEGER               MXNCIT
      PARAMETER           ( MXNCIT =  3 )

C
C     Saved body name length.
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Locus string length.
C
      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 15 )

C
C     Code for the frame J2000.
C
      INTEGER               J2CODE
      PARAMETER           ( J2CODE = 1 )

C
C     Local variables
C
      CHARACTER*(LOCLEN)    LOCSTR
      CHARACTER*(CORLEN)    PRVCOR
      CHARACTER*(LOCLEN)    PRVLOC

      DOUBLE PRECISION      CTRPOS ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DVAL
      DOUBLE PRECISION      EPCDIF
      DOUBLE PRECISION      FIXDIR ( 3 )
      DOUBLE PRECISION      FIXOBS ( 3 )
      DOUBLE PRECISION      J2DIR  ( 3 )
      DOUBLE PRECISION      J2FIXM ( 3, 3 )
      DOUBLE PRECISION      J2LCUS ( 3 )
      DOUBLE PRECISION      J2LPOS ( 3 )
      DOUBLE PRECISION      J2OPOS ( 3 )
      DOUBLE PRECISION      J2TPOS ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      PRVEPC
      DOUBLE PRECISION      PRVSRF ( 3 )
      DOUBLE PRECISION      PRVTAN ( 3 )
      DOUBLE PRECISION      R2JMAT ( 3, 3 )
      DOUBLE PRECISION      REFEPC
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLFIX ( 3 )
      DOUBLE PRECISION      STLLOC ( 3 )
      DOUBLE PRECISION      STLOBS ( 3 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      TANOFF ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )

      INTEGER               DCENTR
      INTEGER               DCLASS
      INTEGER               DFRCDE
      INTEGER               DTYPID
      INTEGER               FXCENT
      INTEGER               FXCLSS
      INTEGER               FXFCDE
      INTEGER               FXTYID
      INTEGER               I
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               TRGCDE

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               LTCNV
      LOGICAL               STLCNV
      LOGICAL               TANLOC
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

C
C     Saved body name/ID item declarations.
C
      INTEGER               PRVTCD
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(BDNMLN)    SVTARG
      INTEGER               SVTCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(BDNMLN)    SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFREF
      INTEGER               SVFXFC

      INTEGER               SVCTR4 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVDREF
      INTEGER               SVDFRC

C
C     Saved target radii declarations.
C
      INTEGER               SVCTR5 ( CTRSIZ )
      DOUBLE PRECISION      SVRADI ( 3 )
      INTEGER               SVNRAD


C
C     Saved surface name/ID item declarations. To be used if DSK
C     shapes are supported.
C
C      INTEGER               SVCTR6 ( CTRSIZ )

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  PRVLOC
      SAVE                  PRVTCD
      SAVE                  TANLOC
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  USESTL
      SAVE                  XMIT

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR3
      SAVE                  SVFREF
      SAVE                  SVFXFC

      SAVE                  SVCTR4
      SAVE                  SVDREF
      SAVE                  SVDFRC

C
C     Saved target radii items.
C
      SAVE                  SVCTR5
      SAVE                  SVRADI
      SAVE                  SVNRAD

C
C     To be used if DSK shapes are supported:
C
C     Saved surface name/ID items.
C
C      SAVE                  SVCTR6
C

C
C     Initial values
C
      DATA                  FIRST  / .TRUE.  /
      DATA                  PRVCOR / ' '     /
      DATA                  PRVLOC / ' '     /
      DATA                  PRVTCD / 0       /
      DATA                  TANLOC / .FALSE. /
      DATA                  USECN  / .FALSE. /
      DATA                  USELT  / .FALSE. /
      DATA                  USESTL / .FALSE. /
      DATA                  XMIT   / .FALSE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TANGPT' )

C
C     Counter initialization is done separately.
C
      IF ( FIRST ) THEN
C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )
         CALL ZZCTRUIN( SVCTR4 )
         CALL ZZCTRUIN( SVCTR5 )
C
C        To be used if DSK shapes are supported:
C
C         CALL ZZCTRUIN( SVCTR6 )

      END IF

C
C     Parse the aberration correction specifier, if it's new.
C
      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        PRVCOR is updated only when a valid correction has been
C        recognized. PRVCOR is blank on the first pass; afterward
C        it is always valid.
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any. Analyze the new flag.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF
C
C        Set logical flags indicating the attributes of the requested
C        correction:
C
C           XMIT is .TRUE. when the correction is for transmitted
C           radiation.
C
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USECN indicates converged Newtonian light time correction.
C
C           USESTL indicates stellar aberration corrections.
C
C
C        The above definitions are consistent with those used by
C        ZZVALCOR.
C
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )

C
C        The aberration correction flag is valid; save it.
C
         PRVCOR = ABCORR

C
C        FIRST will be set to .FALSE. later, after all first-pass
C        actions have been performed.
C
      END IF

C
C     Get the sign S prefixing LT in the expression for TRGEPC.
C     When light time correction is not used, setting S = 0
C     allows us to seamlessly set TRGEPC equal to ET.
C
      IF ( USELT ) THEN

         IF ( XMIT ) THEN
            S   =  1.D0
         ELSE
            S   = -1.D0
         END IF

      ELSE
         S = 0.D0
      END IF

C
C     The method cannot be anything other than 'ELLIPSOID'.
C
      IF ( .NOT. EQSTR( METHOD, 'ELLIPSOID') ) THEN

         CALL SETMSG ( 'Method is currently restricted to '
     .   //            'ELLIPSOID, but input value was #.' )
         CALL ERRCH  ( '#', METHOD                         )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'               )
         CALL CHKOUT ( 'TANGPT'                            )
         RETURN

      END IF

C
C     Code from this point onward assumes the target shape is modeled
C     as a triaxial ellipsoid.
C
C     If we're using aberration corrections, the aberration correction
C     locus must be equivalent to one of 'TANGENT POINT' or 'SURFACE
C     POINT'. TANLOC is set to .TRUE. if and only if the locus is the
C     tangent point.
C
      IF (  FIRST  .OR.  ( CORLOC .NE. PRVLOC )  ) THEN
C
C        Left justify the input locus string, convert to upper case,
C        and compress all embedded blanks for comparison.
C
         CALL LJUCRS ( 0, CORLOC, LOCSTR )

         IF ( LOCSTR .EQ. 'TANGENTPOINT' ) THEN

            TANLOC = .TRUE.

         ELSE IF ( LOCSTR .EQ. 'SURFACEPOINT' ) THEN

            TANLOC = .FALSE.

         ELSE

            CALL SETMSG ( 'Aberration correction locus must be one '
     .      //            'of TANGENT POINT or SURFACE POINT but '
     .      //            'was #.'                                  )
            CALL ERRCH  ( '#', CORLOC                               )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
            CALL CHKOUT ( 'TANGPT'                                  )
            RETURN

         END IF

C
C        At this point we have a valid locus. TANLOC is set.
C        Save the input locus string so we can check for
C        a change on the next call.
C
         PRVLOC = CORLOC

      END IF

C
C     Check for a zero ray direction vector.
C
      IF ( VZERO(DVEC) ) THEN

         CALL SETMSG ( 'Input ray direction was the zero '
     .   //            'vector; this vector must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                     )
         CALL CHKOUT ( 'TANGPT'                                )
         RETURN

      END IF

C
C     Obtain integer codes for the target and observer.
C
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1,
     .                TARGET, TRGCDE, FND    )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit, or that you '
     .   //            'failed to load a kernel containing a '
     .   //            'name-ID mapping for this body.'           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'TANGPT'                                   )
         RETURN

      END IF

      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2,
     .                OBSRVR, OBSCDE, FND    )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit, or that you '
     .   //            'failed to load a kernel containing a '
     .   //            'name-ID mapping for this body.'           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'TANGPT'                                   )
         RETURN

      END IF

C
C     Check the input body codes. If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN

         CALL SETMSG ( 'The observing body and target body are the '
     .   //            'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( 'TANGPT'                                     )
         RETURN

      END IF

C
C     Get the target body's ellipsoid radii.
C
      IF (  FIRST  .OR.  ( TRGCDE .NE. PRVTCD )  ) THEN
C
C        This the first pass, or else the target body has changed. We
C        need to get radii for the new target body.
C
C        Re-initialize the counter used to detect changes to the target
C        body radii.
C
         CALL ZZCTRUIN( SVCTR5 )

         PRVTCD = TRGCDE

      END IF

      CALL ZZBODVCD ( TRGCDE, 'RADII', 3, SVCTR5, SVNRAD, SVRADI )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( SVNRAD .NE. 3 ) THEN

         CALL SETMSG ( 'Number of radii associated with target '
     .   //            'body # is #; number must be 3.'         )
         CALL ERRCH  ( '#',  TARGET                             )
         CALL ERRINT ( '#',  SVNRAD                             )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'TANGPT'                                 )
         RETURN

      END IF

C
C     At this point, we've performed all first-pass actions.
C
      FIRST = .FALSE.

C
C     Determine the attributes of the frame designated by FIXREF.
C
      CALL ZZNAMFRM ( SVCTR3, SVFREF, SVFXFC, FIXREF, FXFCDE )

      CALL FRINFO ( FXFCDE, FXCENT, FXCLSS, FXTYID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by '
     .   //            'the SPICE frame subsystem. Possibly '
     .   //            'a required frame definition kernel has '
     .   //            'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( 'TANGPT'                                  )
         RETURN

      END IF

C
C     Make sure that FIXREF is centered at the target body's center.
C
      IF ( FXCENT .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the '
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  FXCENT                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( 'TANGPT'                                   )
         RETURN

      END IF

C
C     Determine the attributes of the frame designated by DREF.
C
      CALL ZZNAMFRM ( SVCTR4, SVDREF, SVDFRC, DREF, DFRCDE )

      CALL FRINFO ( DFRCDE, DCENTR, DCLASS, DTYPID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by '
     .   //            'the SPICE frame subsystem. Possibly '
     .   //            'a required frame definition kernel has '
     .   //            'not been loaded.'                        )
         CALL ERRCH  ( '#',  DREF                                )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( 'TANGPT'                                  )
         RETURN

      END IF

C
C     Get the position of the target relative to the observer. If
C     light time corrections are used, this gives us an initial
C     light time estimate and initial target epoch.
C
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TPOS, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( VZERO(TPOS) ) THEN

         CALL SETMSG ( 'Observer # and target # have distinct '
     .   //            'ID codes but the distance between these '
     .   //            'objects is zero.'                        )
         CALL ERRCH  ( '#',  OBSRVR                              )
         CALL ERRCH  ( '#',  TARGET                              )
         CALL SIGERR ( 'SPICE(NOSEPARATION)'                     )
         CALL CHKOUT ( 'TANGPT'                                  )
         RETURN

      END IF

C
C     Negate the target's position to obtain the position of the
C     observer relative to the target.
C
      CALL VMINUS ( TPOS, FIXOBS )

C
C     Now we can check whether the observer is inside the ellipsoid.
C     Find the point on the ellipsoid that lies on the line between
C     FIXOBS and the ellipsoid's center.
C
      CALL EDPNT ( FIXOBS, SVRADI(1), SVRADI(2), SVRADI(3), P )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( VNORM(P) .GE. VNORM(FIXOBS) ) THEN

         CALL SETMSG ( 'Observer # is inside ellipsoid representing '
     .   //            'target body # shape.'   )
         CALL ERRCH  ( '#', OBSRVR              )
         CALL ERRCH  ( '#', TARGET              )
         CALL SIGERR ( 'SPICE(INVALIDGEOMETRY)' )
         CALL CHKOUT ( 'TANGPT' )
         RETURN

      END IF

C
C     The target epoch is dependent on the aberration correction. The
C     coefficient S has been set to give us the correct answer for each
C     case.
C
      TRGEPC = ET  +  S*LT

C
C     Transform the direction vector from frame DREF to the body-fixed
C     frame associated with the target. The epoch TRGEPC associated
C     with the body-fixed frame has been set already.
C
C     We'll compute the transformation in two parts: first
C     from frame DREF to J2000, then from J2000 to the target
C     frame.
C
C     The orientation of the ray's frame is evaluated at ET in any
C     of the following situations:
C
C        - The frame is inertial
C        - Light time corrections are not used
C        - The frame is centered at the observer
C
C     Let REFEPC be the epoch of participation of the observer.
C
      IF (      ( DCLASS .EQ.  INERTL )
     .     .OR. ( DCENTR .EQ.  OBSCDE )
     .     .OR. (        .NOT. USELT  )  ) THEN

         REFEPC = ET

      ELSE
C
C        The epoch at which the orientation of ray frame is evaluated
C        is the epoch of participation of the center of that frame.
C
C        Find the light time from the observer to the center of
C        frame DREF.
C
         CALL SPKEZP ( DCENTR, ET,     'J2000', ABCORR,
     .                 OBSCDE, CTRPOS, LTCENT          )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF

         REFEPC  =  ET  +  S * LTCENT

      END IF

C
C     The epoch REFEPC associated with frame DREF has been set.
C
C     Compute the ray direction in the J2000 frame.
C
      IF ( DFRCDE .EQ. J2CODE ) THEN

         CALL VEQU ( DVEC, J2DIR )

      ELSE

         CALL REFCHG ( DFRCDE, J2CODE, REFEPC, R2JMAT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF

         CALL MXV ( R2JMAT, DVEC, J2DIR )

      END IF

C
C     Now transform the ray direction from the J2000 frame to the
C     target body-fixed frame at TRGEPC.
C
      CALL REFCHG ( J2CODE, FXFCDE, TRGEPC, J2FIXM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      CALL MXV ( J2FIXM, J2DIR, FIXDIR )

C
C     We have all the inputs needed to make initial estimates of
C     our outputs.
C
      CALL NPEDLN ( SVRADI(1), SVRADI(2), SVRADI(3),
     .              FIXOBS,    FIXDIR,    SRFPT,    ALT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

C
C     Compute the observer-to-surface point vector for the initial
C     estimated solution.
C
      CALL VSUB ( SRFPT, FIXOBS, SRFVEC )

C
C     Now compute the tangent point.
C
C     Start by finding the nearest point to SRFPT on the line
C     containing the input ray. Note that although we're setting the
C     value of the outputs TANPT here, we're not done yet.
C
C     We retain the altitude found by NPEDLN, since the following
C     call can introduce round-off error in the intercept case.
C
      CALL NPLNPT ( FIXOBS, FIXDIR, SRFPT, TANPT, DVAL )

C
C     Note that TANPT might not be valid here, if we're in the look-
C     away case. We'll deal with this case below.
C
C     A SPICE error should not be possible here but we check anyway
C     for safety.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

      IF ( .NOT. USELT ) THEN
C
C        Aberration corrections are not used.
C
         TRGEPC = ET

C
C        If TANPT is on or behind the ray's vertex, reset TANPT to be
C        the vertex, and set the range to zero. Reset the surface point
C        SRFPT to the nearest point on the target to the observer, and
C        set ALT to the altitude of the observer with respect to that
C        point.
C
C        Note that if aberration corrections were used, then the test
C        for the ray pointing away from the target could give a
C        different result. We handle that case separately later on.
C
         CALL VSUB ( TANPT, FIXOBS, TANOFF )

         IF ( VDOT( TANOFF, FIXDIR ) .LE. 0.D0 ) THEN
C
C           TANPT is on or behind the ray's vertex.
C
            CALL VEQU ( FIXOBS, TANPT )
            RANGE = 0.D0

            CALL NEARPT ( FIXOBS, SVRADI(1), SVRADI(2), SVRADI(3),
     .                    SRFPT,  ALT                             )
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TANGPT' )
               RETURN
            END IF

C
C           Compute SRFVEC using our newly computed value of SRFPT.
C
            CALL VSUB ( SRFPT, FIXOBS, SRFVEC )

         ELSE
C
C           The tangent point lies ahead of the observer.
C
            IF ( ALT .EQ. 0.D0 ) THEN
C
C              This is the geometric intercept case. ALT, SRFPT, TANPT,
C              and SRFVEC are already set. To eliminate any error in
C              TANPT, we set it equal to SRFPT.
C
               CALL VEQU ( SRFPT, TANPT )

               RANGE = VNORM ( SRFVEC )

            ELSE
C
C              This is the normal geometric case. All outputs
C              other than range are already set.
C
               RANGE = VDIST ( FIXOBS, TANPT )

            END IF

         END IF

         CALL CHKOUT ( 'TANGPT' )
         RETURN

      END IF

C
C     Still here? Then we are using aberration corrections. The outputs
C     we've computed serve as first estimates for converged values.
C
C     Since we're using light time corrections, we're going to make an
C     estimate of light time to the aberration correction locus, then
C     re-do our computation of the target position and orientation
C     using the new light time value.
C
C     Note that for non-converged light time, we perform several more
C     iterations. The initial light time correction was for the target
C     center.
C
      IF ( USECN ) THEN
         NITR = MXCVIT
      ELSE
         NITR = MXNCIT
      END IF
C
C     Compute new light time estimate and new target epoch.
C
      IF ( TANLOC ) THEN
C
C        Compute distance to the tangent point.
C
         DIST = VDIST ( FIXOBS, TANPT )

      ELSE
C
C        Compute distance to the surface point.
C
         DIST = VNORM ( SRFVEC )

      END IF

C
C     We'll need the state of the observer relative to the solar system
C     barycenter. This state need be computed just once. The position
C     of the target relative to the solar system barycenter will need
C     to be re-computed on each loop iteration.
C
      CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TANGPT' )
         RETURN
      END IF

C
C     Compute light time based on distance to the aberration correction
C     locus; compute the new target epoch based on the light time.
C
      LT     =  DIST / CLIGHT()
      TRGEPC =  ET  +  S * LT

      PREVLT = 0.D0
      PRVEPC = TRGEPC

      I      = 0
      LTDIFF = 1.D0
      EPCDIF = 1.D0

C
C     Initialize STLCNV, the flag that indicates whether stellar
C     aberration corrections have converged.
C
      IF ( USESTL ) THEN
         STLCNV = .FALSE.
      ELSE
         STLCNV = .TRUE.
      END IF

C
C     Initialize LTCNV, the flag that indicates whether light time
C     corrections have converged.
C
      LTCNV  = .FALSE.

C
C     The loop terminates if both light time and stellar aberration
C     correction have converged or if the maximum number of iterations
C     has been reached.
C
C     Light time correction convergence is indicated when either of
C     these conditions are met:
C
C         - The relative difference between successive light time
C           estimates becomes less than CNVLIM
C
C         - The target epoch doesn't change
C
C     Stellar aberration convergence is indicated when both of
C     these conditions are met:
C
C        - The relative difference between successive values of TANPT
C          becomes less than CNVLIM
C
C        - The relative difference between successive values of SRFPT
C          becomes less than CNVLIM
C
C
      DO WHILE (             ( I     .LT.  NITR   )
     .           .AND. .NOT. ( LTCNV .AND. STLCNV ) )

         IF ( USESTL ) THEN
C
C           Track the output points in order to test convergence of
C           the stellar aberration correction.
C
            CALL VEQU ( TANPT, PRVTAN )
            CALL VEQU ( SRFPT, PRVSRF )

         END IF

C
C        Get the J2000-relative state of the target relative to
C        the solar system barycenter at the target epoch. The
C        observer's position doesn't change.
C
         CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF

C
C        Convert the position of the observer relative to the solar
C        system barycenter from the J2000 frame to the target frame at
C        TRGEPC.
C
C        SSBOST contains the J2000-relative state of the observer
C        relative to the solar system barycenter at ET.
C
         CALL VSUB   ( SSBOST,  SSBTST, J2OPOS         )
         CALL PXFORM ( 'J2000', FIXREF, TRGEPC, J2FIXM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF

C
C        If we're using stellar aberration corrections, which we
C        normally should do if we're using light time corrections,
C        compute the stellar aberration correction for the light time
C        corrected aberration correction locus.
C
         IF ( USESTL ) THEN
C
C           Get the position of the aberration correction locus
C           relative to the observer in the J2000 frame. The locus is
C           expressed as an offset from the target center.
C
C           First convert the locus from the target body-fixed frame to
C           the J2000 frame.
C
            IF ( TANLOC ) THEN
               CALL MTXV ( J2FIXM, TANPT, J2LCUS )
            ELSE
               CALL MTXV ( J2FIXM, SRFPT, J2LCUS )
            END IF

C
C           Compute the position of the locus relative to the observer
C           in the J2000 frame.
C
            CALL VMINUS ( J2OPOS, J2TPOS )
            CALL VADD   ( J2TPOS, J2LCUS, J2LPOS )

C
C           Correct the vector from the observer to the aberration
C           correction locus for stellar aberration and retain the
C           offset STLOFF from the uncorrected vector to the corrected
C           vector.
C
            IF ( XMIT ) THEN
               CALL STLABX ( J2LPOS, SSBOST(4), STLLOC )
            ELSE
               CALL STELAB ( J2LPOS, SSBOST(4), STLLOC )
            END IF

            CALL VSUB ( STLLOC, J2LPOS, STLOFF )

C
C           Convert the stellar aberration correction offset to the
C           target body-fixed frame at TRGEPC.
C
            CALL MXV ( J2FIXM, STLOFF, STLFIX )

         ELSE
C
C           We're not using stellar aberration correction, so just
C           zero out the offset.
C
            CALL CLEARD ( 3, STLFIX )

         END IF

C
C        Convert the observer's position relative to the target from
C        the J2000 frame to the target frame at the target epoch. Let
C        TRGPOS be the negative of this vector.
C
         CALL MXV    ( J2FIXM, J2OPOS, FIXOBS )
         CALL VMINUS ( FIXOBS, TRGPOS )

C
C        Convert the ray direction vector from the J2000 frame
C        to the target frame at the target epoch.
C
         CALL MXV ( J2FIXM, J2DIR, FIXDIR )

C
C        The ray-ellipsoid near point computation must be performed
C        using the apparent target. We've accounted for light time,
C        but stellar aberration must be accounted for as well. The
C        apparent target is shifted due to stellar aberration by the
C        body-fixed vector STLFIX. Equivalently, we can shift the
C        observer position by -STLFIX.
C
C        If stellar aberration correction was not commanded, then
C        STLFIX is the zero vector.
C
         CALL VSUB ( FIXOBS, STLFIX, STLOBS )

C
C        Re-compute the surface point and ray altitude.
C
         CALL NPEDLN ( SVRADI(1), SVRADI(2), SVRADI(3),
     .                 STLOBS,    FIXDIR,    SRFPT,    ALT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TANGPT' )
            RETURN
         END IF

C
C        Now compute the tangent point.
C
C        Start by finding the nearest point to SRFPT on the line
C        containing the input ray.
C
C        We retain the altitude found by NPEDLN, since the following
C        call can introduce round-off error in the intercept case.
C
         IF ( ALT .NE. 0.D0 ) THEN
            CALL NPLNPT ( STLOBS, FIXDIR, SRFPT, TANPT, DVAL )
         ELSE
            CALL VEQU ( SRFPT, TANPT )
         END IF

C
C        The output SRFVEC extends from the observer to the apparent
C        position of the surface point.
C
         CALL VSUB ( SRFPT, STLOBS, SRFVEC )

C
C        We may need to update TANPT, SRFPT and SRFVEC if the tangent
C        point is behind the observer (the look-away case).
C
         CALL VSUB ( TANPT, STLOBS, TANOFF )

         IF ( VDOT( TANOFF, FIXDIR ) .LE. 0.D0 ) THEN
C
C           TANPT is on or behind the ray's vertex. Reset TANPT to be
C           the vertex.
C
            CALL VEQU ( STLOBS, TANPT )
            RANGE = 0.D0

C
C           In this case, the surface point is considered to be the
C           nearest point on the target to the observer. The altitude
C           of the observer above this point is the tangent point's
C           altitude.
C
            CALL NEARPT ( STLOBS,    SVRADI(1), SVRADI(2),
     .                    SVRADI(3), SRFPT,     ALT       )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TANGPT' )
               RETURN
            END IF

            CALL VSUB ( SRFPT, TANPT, SRFVEC )

C
C           Set the light time and the target epoch, based on the
C           locus.
C
            IF ( TANLOC ) THEN

               LT     = 0.D0
               TRGEPC = ET

            ELSE

               LT     = ALT / CLIGHT()
               TRGEPC = ET + S*LT

            END IF

         ELSE
C
C           This is the normal case.
C
C           Compute a new light time estimate and new target epoch.
C           Light time estimates are computed using the light-time
C           corrected position of the aberration correction locus;
C           stellar aberration does not apply. Therefore we use FIXOBS
C           as the observer position for the distance computations.
C
            IF ( TANLOC ) THEN
C
C              Compute distance to the tangent point.
C
               DIST = VDIST ( FIXOBS, TANPT )

            ELSE
C
C              Compute distance to the surface point.
C
               DIST = VDIST ( FIXOBS, SRFPT )

            END IF
C
C           Compute a new light time estimate and a new target epoch.
C
            LT     = DIST / CLIGHT()
            TRGEPC = ET  +  S * LT

         END IF

C
C        Compute the changes in the light time and target epoch for
C        this loop pass. Determine whether light time and stellar
C        aberration have converged.
C
         LT      =   TOUCHD( LT )
         LTDIFF  =   ABS( LT     - PREVLT )
         EPCDIF  =   ABS( TRGEPC - PRVEPC )

         PREVLT  =   LT
         PRVEPC  =   TRGEPC
         I       =   I + 1

         LTCNV  =        (  LTDIFF .LT. ( CNVLIM * ABS(LT) )  )
     .             .OR.  (  EPCDIF .EQ.   0.D0                )

         IF ( USESTL ) THEN

            STLCNV =       ( VREL( TANPT, PRVTAN ) .LT. CNVLIM )
     .               .AND. ( VREL( SRFPT, PRVSRF ) .LT. CNVLIM )
         END IF

      END DO

      RANGE = VDIST ( STLOBS, TANPT )

      CALL CHKOUT ( 'TANGPT' )
      RETURN
      END

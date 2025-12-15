C$Procedure GFSNTC (GF, surface intercept vector coordinate search)

      SUBROUTINE GFSNTC (  TARGET,  FIXREF,  METHOD,
     .                     ABCORR,  OBSRVR,  DREF,
     .                     DVEC,    CRDSYS,  COORD,
     .                     RELATE,  REFVAL,  ADJUST,
     .                     STEP,    CNFINE,  MW,
     .                     NW,      WORK,    RESULT )

C$ Abstract
C
C     Determine time intervals for which a coordinate of a
C     surface intercept position vector satisfies a numerical
C     constraint.
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
C     GF
C     SPK
C     CK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     COORDINATE
C     EVENT
C     GEOMETRY
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   (3)
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         COORD
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      STEP
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Convergence tolerance.
C     ZZGET      P   ZZHOLDD retrieves a stored DP value.
C     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance.
C     TARGET     I   Name of the target body.
C     FIXREF     I   Body fixed frame associated with TARGET.
C     METHOD     I   Name of method type for surface intercept
C                    calculation.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     DREF       I   Reference frame of direction vector DVEC.
C     DVEC       I   Pointing direction vector from OBSRVR.
C     CRDSYS     I   Name of the coordinate system containing COORD.
C     COORD      I   Name of the coordinate of interest.
C     RELATE     I   Relational operator.
C     REFVAL     I   Reference value.
C     ADJUST     I   Adjustment value for absolute extrema searches.
C     STEP       I   Step size used for locating extrema and roots.
C     CNFINE     I   SPICE window to which the search is confined.
C     MW         I   Workspace window size.
C     NW         I   The number of workspace windows needed for the
C                    search.
C     WORK       O   Array of workspace windows
C     RESULT    I-O  SPICE window containing results.
C
C$ Detailed_Input
C
C     TARGET   is the string name of a target body. Optionally, you may
C              supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              On calling GFSNTC, the kernel pool must contain the radii
C              data corresponding to TARGET.
C
C     FIXREF   is the string name of the body-fixed, body-centered
C              reference frame associated with the target body TARGET.
C
C              The SPICE frame subsystem must recognize the FIXREF
C              name.
C
C     METHOD   is the string name of the method to use for the surface
C              intercept calculation. The accepted values for METHOD:
C
C                 'Ellipsoid'        The intercept computation uses
C                                    a triaxial ellipsoid to model
C                                    the surface of the target body.
C                                    The ellipsoid's radii must be
C                                    available in the kernel pool.
C
C              The METHOD string lacks sensitivity to case, embedded,
C              leading and trailing blanks.
C
C     ABCORR   is the string description of the aberration corrections
C              to apply to the state evaluations to account for one-way
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
C
C                 'LT'       "Reception" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case: converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case: converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C              *Note*
C
C              When using a reference frame defined as a dynamic frame,
C              the user should realize defining an aberration correction
C              for the search different from that in the frames
C              definition will affect the search results.
C
C              In general, use the same aberration correction for
C              intercept point searches as used in the definition of a
C              dynamic frame (if applicable).
C
C     OBSRVR   is the string name of an observing body. Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C     DREF     is the string name of the reference frame corresponding
C              to DVEC.
C
C              The DREF string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     DVEC     is the pointing or boresight vector from the observer.
C              The intercept of this vector and TARGET is the event of
C              interest.
C
C     CRDSYS   is the string name of the coordinate system for which the
C              coordinate of interest is a member.
C
C     COORD    is the string name of the coordinate of interest in
C              CRDSYS.
C
C              The supported coordinate systems and coordinate names:
C
C                 CRDSYS             COORD               Range
C                 ----------------   -----------------   ------------
C                 'RECTANGULAR'      'X'
C                                    'Y'
C                                    'Z'
C
C                 'LATITUDINAL'      'RADIUS'
C                                    'LONGITUDE'         (-Pi,Pi]
C                                    'LATITUDE'          [-Pi/2,Pi/2]
C
C                 'RA/DEC'           'RANGE'
C                                    'RIGHT ASCENSION'   [0,2Pi)
C                                    'DECLINATION'       [-Pi/2,Pi/2]
C
C                 'SPHERICAL'        'RADIUS'
C                                    'COLATITUDE'        [0,Pi]
C                                    'LONGITUDE'         (-Pi,Pi]
C
C                 'CYLINDRICAL'      'RADIUS'
C                                    'LONGITUDE'         [0,2Pi)
C                                    'Z'
C
C                 'GEODETIC'         'LONGITUDE'         (-Pi,Pi]
C                                    'LATITUDE'          [-Pi/2,Pi/2]
C                                    'ALTITUDE'
C
C                 'PLANETOGRAPHIC'   'LONGITUDE'         [0,2Pi)
C                                    'LATITUDE'          [-Pi/2,Pi/2]
C                                    'ALTITUDE'
C
C              The 'ALTITUDE' coordinates have a constant value of
C              zero +/- roundoff for ellipsoid targets.
C
C              Limit searches for coordinate events in the 'GEODETIC'
C              and 'PLANETOGRAPHIC' coordinate systems to TARGET bodies
C              with axial symmetry in the equatorial plane, i.e.
C              equality of the body X and Y radii (oblate or prolate
C              spheroids).
C
C              Searches on 'GEODETIC' or 'PLANETOGRAPHIC' coordinates
C              requires body shape data, and in the case of
C              'PLANETOGRAPHIC' coordinates, body rotation data.
C
C              The body associated with 'GEODETIC' or 'PLANETOGRAPHIC'
C              coordinates is the center of the frame FIXREF.
C
C     RELATE   is the string or character describing the relational
C              operator used to define a constraint on the selected
C              coordinate of the surface intercept vector. The result
C              window found by this routine indicates the time intervals
C              where the constraint is satisfied. Supported values of
C              RELATE and corresponding meanings are shown below:
C
C                 '>'        The coordinate value is greater than the
C                            reference value REFVAL.
C
C                 '='        The coordinate value is equal to the
C                            reference value REFVAL.
C
C                 '<'        The coordinate value is less than the
C                            reference value REFVAL.
C
C                 'ABSMAX'   The coordinate value is at an absolute
C                            maximum.
C
C                 'ABSMIN'   The coordinate value is at an absolute
C                            minimum.
C
C                 'LOCMAX'   The coordinate value is at a local
C                            maximum.
C
C                 'LOCMIN'   The coordinate value is at a local
C                            minimum.
C
C              The caller may indicate that the region of interest
C              is the set of time intervals where the quantity is
C              within a specified measure of an absolute extremum.
C              The argument ADJUST (described below) is used to
C              specify this measure.
C
C              Local extrema are considered to exist only in the
C              interiors of the intervals comprising the confinement
C              window:  a local extremum cannot exist at a boundary
C              point of the confinement window.
C
C              The RELATE string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     REFVAL   is the double precision reference value used together
C              with the argument RELATE to define an equality or
C              inequality to satisfy by the selected coordinate of the
C              surface intercept vector. See the discussion of RELATE
C              above for further information.
C
C              The units of REFVAL correspond to the type as defined
C              by COORD, radians for angular measures, kilometers for
C              distance measures.
C
C     ADJUST   is a double precision value used to modify searches for
C              absolute extrema: when RELATE is set to 'ABSMAX' or
C              'ABSMIN' and ADJUST is set to a positive value, GFSNTC
C              finds times when the intercept vector coordinate is
C              within ADJUST radians/kilometers of the specified extreme
C              value.
C
C              For RELATE set to 'ABSMAX', the RESULT window contains
C              time intervals when the intercept vector coordinate has
C              values between ABSMAX - ADJUST and ABSMAX.
C
C              For RELATE set to 'ABSMIN', the RESULT window contains
C              time intervals when the intercept vector coordinate has
C              values between ABSMIN and ABSMIN + ADJUST.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions.
C
C     STEP     is the double precision time step size to use in the
C              search.
C
C              Selection of the time step for surface intercept geometry
C              requires consideration of the mechanics of a surface
C              intercept event. In most cases, two distinct searches
C              will be needed, one to determine the windows when the
C              boresight vector intercepts the surface and then the
C              search based on the user defined constraints within those
C              windows. The boresight of nadir pointing instrument may
C              continually intercept a body, but an instrument scanning
C              across a disc will have configurations when the
C              boresight does not intercept the body.
C
C              The step size must be smaller than the shortest interval
C              within the confinement window over which the intercept
C              exists and also smaller than the shortest interval over
C              which the intercept does not exist.
C
C              For coordinates other than LONGITUDE and RIGHT ASCENSION,
C              the step size must be shorter than the shortest interval,
C              within the confinement window, over which the coordinate
C              is monotone increasing or decreasing.
C
C              For LONGITUDE and RIGHT ASCENSION, the step size must
C              be shorter than the shortest interval, within the
C              confinement window, over which either the sine or cosine
C              of the coordinate is monotone increasing or decreasing.
C
C              The choice of STEP affects the completeness but not
C              the precision of solutions found by this routine; the
C              precision is controlled by the convergence tolerance.
C              See the discussion of the parameter CNVTOL for
C              details.
C
C              STEP has units of TDB seconds.
C
C     CNFINE   is a double precision SPICE window that confines the time
C              period over which the specified search is conducted.
C              CNFINE may consist of a single interval or a collection
C              of intervals.
C
C              In some cases the confinement window can be used to
C              greatly reduce the time period that must be searched
C              for the desired solution. See the $Particulars section
C              below for further discussion.
C
C              See the $Examples section below for a code example
C              that shows how to create a confinement window.
C
C              CNFINE must be initialized by the caller using the
C              SPICELIB routine SSIZED.
C
C              In some cases the observer's state may be computed at
C              times outside of CNFINE by as much as 2 seconds. See
C              $Particulars for details.
C
C     MW       is a parameter specifying the length of the SPICE
C              windows in the workspace array WORK (see description
C              below) used by this routine.
C
C              MW should be set to a number at least twice as large
C              as the maximum number of intervals required by any
C              workspace window. In many cases, it's not necessary to
C              compute an accurate estimate of how many intervals are
C              needed; rather, the user can pick a size considerably
C              larger than what's really required.
C
C              However, since excessively large arrays can prevent
C              applications from compiling, linking, or running
C              properly, sometimes MW must be set according to
C              the actual workspace requirement. A rule of thumb
C              for the number of intervals NINTVLS needed is
C
C                  NINTVLS  =  2*N  +  ( M / STEP )
C
C              where
C
C                  N     is the number of intervals in the confinement
C                        window
C
C                  M     is the measure of the confinement window, in
C                        units of seconds
C
C                  STEP  is the search step size in seconds
C
C              MW should then be set to
C
C                  2 * NINTVLS
C
C     NW       is a parameter specifying the number of SPICE windows
C              in the workspace array WORK (see description below)
C              used by this routine. NW should be set to the
C              parameter NWMAX; this parameter is declared in the
C              INCLUDE file gf.inc. (The reason this dimension is
C              an input argument is that this allows run-time
C              error checking to be performed.)
C
C     RESULT   is a double precision SPICE window which will contain
C              the search results. RESULT must be declared and
C              initialized with sufficient size to capture the full
C              set of time intervals within the search region on which
C              the specified condition is satisfied.
C
C              RESULT must be initialized by the caller via the
C              SPICELIB routine SSIZED.
C
C              If RESULT is non-empty on input, its contents will be
C              discarded before GFSNTC conducts its search.
C
C$ Detailed_Output
C
C     WORK     is an array used to store workspace windows.
C
C              This array should be declared by the caller as shown:
C
C                 INCLUDE 'gf.inc'
C                    ...
C
C                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWMAX )
C
C              where MW is a constant declared by the caller and
C              NWMAX is a constant defined in the SPICELIB INCLUDE
C              file gf.inc. See the discussion of MW above.
C
C              WORK need not be initialized by the caller.
C
C              WORK is modified by this routine. The caller should
C              re-initialize this array before attempting to use it for
C              any other purpose.
C
C     RESULT   is the SPICE window of intervals, contained within the
C              confinement window CNFINE, on which the specified
C              constraint is satisfied.
C
C              The endpoints of the time intervals comprising RESULT are
C              interpreted as seconds past J2000 TDB.
C
C              If the search is for local extrema, or for absolute
C              extrema with ADJUST set to zero, then normally each
C              interval of RESULT will be a singleton: the left and
C              right endpoints of each interval will be identical.
C
C              If no times within the confinement window satisfy the
C              search criteria, RESULT will be returned with a
C              cardinality of zero.
C
C$ Parameters
C
C     LBCELL   is the integer value defining the lower bound for
C              SPICE Cell arrays (a SPICE window is a kind of cell).
C
C     CNVTOL   is the convergence tolerance used for finding
C              endpoints of the intervals comprising the result
C              window. CNVTOL is also used for finding intermediate
C              results; in particular, CNVTOL is used for finding the
C              windows on which the specified coordinate is increasing
C              or decreasing. CNVTOL is used to determine when binary
C              searches for roots should terminate: when a root is
C              bracketed within an interval of length CNVTOL; the
C              root is considered to have been found.
C
C              The accuracy, as opposed to precision, of roots found
C              by this routine depends on the accuracy of the input
C              data. In most cases, the accuracy of solutions will be
C              inferior to their precision.
C
C     NWMAX    is the number of workspace windows required by
C              this routine.
C
C     See INCLUDE file gf.inc for declarations and descriptions of
C     parameters used throughout the GF system.
C
C$ Exceptions
C
C     1)  In order for this routine to produce correct results,
C         the step size must be appropriate for the problem at hand.
C         Step sizes that are too large may cause this routine to miss
C         roots; step sizes that are too small may cause this routine
C         to run unacceptably slowly and in some cases, find spurious
C         roots.
C
C         This routine does not diagnose invalid step sizes, except
C         that if the step size is non-positive, an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  Due to numerical errors, in particular,
C
C            - truncation error in time values
C            - finite tolerance value
C            - errors in computed geometric quantities
C
C         it is *normal* for the condition of interest to not always be
C         satisfied near the endpoints of the intervals comprising the
C         RESULT window. One technique to handle such a situation,
C         slightly contract RESULT using the window routine WNCOND.
C
C     3)  If the window size MW is less than 2 or not an even value,
C         the error SPICE(INVALIDDIMENSION) is signaled.
C
C     4)  If the window size of RESULT is less than 2, the error
C         SPICE(INVALIDDIMENSION) is signaled.
C
C     5)  If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         distance condition is met, an error is signaled
C         by a routine in the call tree of this routine.
C
C     6)  If an error (typically cell overflow) occurs during
C         window arithmetic, the error is signaled by a routine
C         in the call tree of this routine.
C
C     7)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     8)  If the size of the workspace WORK is too small, an error is
C         signaled by a routine in the call tree of this routine.
C
C     9)  If the aberration correction specifier contains an
C         unrecognized value, an error is signaled by a routine in the
C         call tree of this routine.
C
C     10) If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     11) If either of the input body names do not map to NAIF ID
C         codes, an error is signaled by a routine in the call tree of
C         this routine.
C
C     12) If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C     13) If the search uses GEODETIC or PLANETOGRAPHIC coordinates, and
C         the center body of the reference frame has unequal equatorial
C         radii, an error is signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     Appropriate SPK and PCK kernels must be loaded by the calling
C     program before this routine is called.
C
C     The following data are required:
C
C     -  SPK data: the calling application must load ephemeris data
C        for the targets, observer, and any intermediate objects in
C        a chain connecting the targets and observer that cover the
C        time period specified by the window CNFINE. If aberration
C        corrections are used, the states of target and observer
C        relative to the solar system barycenter must be calculable
C        from the available ephemeris data. Typically ephemeris data
C        are made available by loading one or more SPK files using
C        FURNSH.
C
C     -  PCK data: bodies modeled as triaxial ellipsoids must have
C        semi-axis lengths provided by variables in the kernel pool.
C        Typically these data are made available by loading a text
C        PCK file using FURNSH.
C
C     -  If non-inertial reference frames are used, then PCK
C        files, frame kernels, C-kernels, and SCLK kernels may be
C        needed.
C
C     -  In some cases the observer's state may be computed at times
C        outside of CNFINE by as much as 2 seconds; data required to
C        compute this state must be provided by loaded kernels. See
C        $Particulars for details.
C
C     Such kernel data are normally loaded once per program run, NOT
C     every time this routine is called.
C
C$ Particulars
C
C     This routine provides a simpler, but less flexible interface
C     than does the routine GFEVNT for conducting searches for
C     surface intercept vector coordinate value events.
C     Applications that require support for progress reporting,
C     interrupt handling, non-default step or refinement functions, or
C     non-default convergence tolerance should call GFEVNT rather than
C     this routine.
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the selected coordinate of
C     the surface intercept position vector satisfies a caller-specified
C     constraint. The resulting set of intervals is returned as a SPICE
C     window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C
C     The Search Process
C     ==================
C
C     Regardless of the type of constraint selected by the caller, this
C     routine starts the search for solutions by determining the time
C     periods, within the confinement window, over which the specified
C     coordinate function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the coordinate
C     function's local extrema within the confinement window are known.
C     Absolute extrema then can be found very easily.
C
C     Within any interval of these "monotone" windows, there will be at
C     most one solution of any equality constraint. Since the boundary
C     of the solution set for any inequality constraint is contained in
C     the union of
C
C     -  the set of points where an equality constraint is met
C
C     -  the boundary points of the confinement window
C
C     the solutions of both equality and inequality constraints can be
C     found easily once the monotone windows have been found.
C
C
C     Step Size
C     =========
C
C     The monotone windows (described above) are found using a two-step
C     search process. Each interval of the confinement window is
C     searched as follows: first, the input step size is used to
C     determine the time separation at which the sign of the rate of
C     change of coordinate will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the
C     coordinate is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the coordinate function is monotone:
C     the step size should be shorter than the shortest of these
C     intervals (within the confinement window).
C
C     The optimal step size is *not* necessarily related to the lengths
C     of the intervals comprising the result window. For example, if
C     the shortest monotone interval has length 10 days, and if the
C     shortest result window interval has length 5 minutes, a step size
C     of 9.9 days is still adequate to find all of the intervals in the
C     result window. In situations like this, the technique of using
C     monotone windows yields a dramatic efficiency improvement over a
C     state-based search that simply tests at each step whether the
C     specified constraint is satisfied. The latter type of search can
C     miss solution intervals if the step size is longer than the
C     shortest solution interval.
C
C     Having some knowledge of the relative geometry of the target and
C     observer can be a valuable aid in picking a reasonable step size.
C     In general, the user can compensate for lack of such knowledge by
C     picking a very short step size; the cost is increased computation
C     time.
C
C     Note that the step size is not related to the precision with which
C     the endpoints of the intervals of the result window are computed.
C     That precision level is controlled by the convergence tolerance.
C
C
C     Convergence Tolerance
C     =====================
C
C     Once a root has been bracketed, a refinement process is used to
C     narrow down the time interval within which the root must lie.
C     This refinement process terminates when the location of the root
C     has been determined to within an error margin called the
C     "convergence tolerance." The default convergence tolerance
C     used by this routine is set by the parameter CNVTOL (defined
C     in gf.inc).
C
C     The value of CNVTOL is set to a "tight" value so that the
C     tolerance doesn't become the limiting factor in the accuracy of
C     solutions found by this routine. In general the accuracy of input
C     data will be the limiting factor.
C
C     The user may change the convergence tolerance from the default
C     CNVTOL value by calling the routine GFSTOL, e.g.
C
C        CALL GFSTOL( tolerance value )
C
C     Call GFSTOL prior to calling this routine. All subsequent
C     searches will use the updated tolerance value.
C
C     Setting the tolerance tighter than CNVTOL is unlikely to be
C     useful, since the results are unlikely to be more accurate.
C     Making the tolerance looser will speed up searches somewhat,
C     since a few convergence steps will be omitted. However, in most
C     cases, the step size is likely to have a much greater effect
C     on processing time than would the convergence tolerance.
C
C
C     The Confinement Window
C     ======================
C
C     The simplest use of the confinement window is to specify a time
C     interval within which a solution is sought. However, the
C     confinement window can, in some cases, be used to make searches
C     more efficient. Sometimes it's possible to do an efficient search
C     to reduce the size of the time period over which a relatively
C     slow search of interest must be performed.
C
C     Practical use of the coordinate search capability would likely
C     consist of searches over multiple coordinate constraints to find
C     time intervals that satisfies the constraints. An
C     effective technique to accomplish such a search is
C     to use the result window from one search as the confinement window
C     of the next.
C
C     Certain types of searches require the state of the observer,
C     relative to the solar system barycenter, to be computed at times
C     slightly outside the confinement window CNFINE. The time window
C     that is actually used is the result of "expanding" CNFINE by a
C     specified amount "T": each time interval of CNFINE is expanded by
C     shifting the interval's left endpoint to the left and the right
C     endpoint to the right by T seconds. Any overlapping intervals are
C     merged. (The input argument CNFINE is not modified.)
C
C     The window expansions listed below are additive: if both
C     conditions apply, the window expansion amount is the sum of the
C     individual amounts.
C
C     -  If a search uses an equality constraint, the time window
C        over which the state of the observer is computed is expanded
C        by 1 second at both ends of all of the time intervals
C        comprising the window over which the search is conducted.
C
C     -  If a search uses stellar aberration corrections, the time
C        window over which the state of the observer is computed is
C        expanded as described above.
C
C     When light time corrections are used, expansion of the search
C     window also affects the set of times at which the light time-
C     corrected state of the target is computed.
C
C     In addition to the possible 2 second expansion of the search
C     window that occurs when both an equality constraint and stellar
C     aberration corrections are used, round-off error should be taken
C     into account when the need for data availability is analyzed.
C
C     Longitude and Right Ascension
C     =============================
C
C     The cyclic nature of the longitude and right ascension coordinates
C     produces branch cuts at +/- 180 degrees longitude and 0-360
C     longitude. Round-off error may cause solutions near these branches
C     to cross the branch. Use of the SPICE routine WNCOND will contract
C     solution windows by some epsilon, reducing the measure of the
C     windows and eliminating the branch crossing. A one millisecond
C     contraction will in most cases eliminate numerical round-off
C     caused branch crossings.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C
C     1) Find the time during 2007 for which the latitude of the
C        intercept point of the vector pointing from the sun towards
C        the earth in the IAU_EARTH frame equals zero i.e. the intercept
C        point crosses the equator.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: gfsntc_ex1.tm
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
C              de414.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0008.tls                  Leapseconds
C
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'naif0008.tls'
C                               'de414.bsp'
C                               'pck00008.tpc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Use the kernel shown below to define a dynamic frame,
C        Sun-Earth Motion.
C
C
C           KPL/FK
C
C           File name: gfsntc_sem.tf
C
C           The Sun-Earth Motion frame is defined by the sun-to-earth
C           direction vector as the X axis. The Y axis in the earth
C           orbital plane, and Z completing the right hand system.
C
C           \begindata
C
C             FRAME_SEM                     =  10100000
C             FRAME_10100000_NAME           = 'SEM'
C             FRAME_10100000_CLASS          =  5
C             FRAME_10100000_CLASS_ID       =  10100000
C             FRAME_10100000_CENTER         =  10
C             FRAME_10100000_RELATIVE       = 'J2000'
C             FRAME_10100000_DEF_STYLE      = 'PARAMETERIZED'
C             FRAME_10100000_FAMILY         = 'TWO-VECTOR'
C             FRAME_10100000_PRI_AXIS       = 'X'
C             FRAME_10100000_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION'
C             FRAME_10100000_PRI_OBSERVER   = 'SUN'
C             FRAME_10100000_PRI_TARGET     = 'EARTH'
C             FRAME_10100000_PRI_ABCORR     = 'NONE'
C             FRAME_10100000_SEC_AXIS       = 'Y'
C             FRAME_10100000_SEC_VECTOR_DEF = 'OBSERVER_TARGET_VELOCITY'
C             FRAME_10100000_SEC_OBSERVER   = 'SUN'
C             FRAME_10100000_SEC_TARGET     = 'EARTH'
C             FRAME_10100000_SEC_ABCORR     = 'NONE'
C             FRAME_10100000_SEC_FRAME      = 'J2000'
C
C           \begintext
C
C           End of frames kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM GFSNTC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include GF parameter declarations:
C        C
C              INCLUDE               'gf.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      SPD
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C        C
C        C     Create 50 windows.
C        C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 1000 )
C
C        C
C        C     One window consists of two intervals.
C        C
C              INTEGER               NINTRVL
C              PARAMETER           ( NINTRVL = MAXWIN *2 )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 64 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(STRLEN)    BEGSTR
C              CHARACTER*(STRLEN)    ENDSTR
C              CHARACTER*(STRLEN)    TARGET
C              CHARACTER*(STRLEN)    OBSRVR
C              CHARACTER*(STRLEN)    DREF
C              CHARACTER*(STRLEN)    ABCORR
C              CHARACTER*(STRLEN)    METHOD
C              CHARACTER*(STRLEN)    FIXREF
C              CHARACTER*(STRLEN)    CRDSYS
C              CHARACTER*(STRLEN)    COORD
C              CHARACTER*(STRLEN)    RELATE
C
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      DVEC   ( 3 )
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2       )
C              DOUBLE PRECISION      RESULT ( LBCELL : NINTRVL )
C              DOUBLE PRECISION      WORK   ( LBCELL : NINTRVL, NWMAX )
C
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      ENDTIM
C              DOUBLE PRECISION      BEG
C              DOUBLE PRECISION      END
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      ADJUST
C
C              INTEGER               COUNT
C              INTEGER               I
C
C        C
C        C     Saved variables
C        C
C        C     The confinement, workspace and result windows CNFINE,
C        C     WORK and RESULT are saved because this practice helps to
C        C     prevent stack overflow.
C        C
C              SAVE                  CNFINE
C              SAVE                  RESULT
C              SAVE                  WORK
C
C        C
C        C     The SEM frame defines the X axis as always earth
C        C     pointing.
C        C
C        C     Define the earth pointing vector in the SEM frame.
C        C
C              DATA                  DVEC   / 1.D0, 0.D0, 0.D0 /
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ('gfsntc_ex1.tm')
C              CALL FURNSH ('gfsntc_sem.tf')
C
C        C
C        C     Initialize windows RESULT and CNFINE.
C        C
C              CALL SSIZED ( NINTRVL, RESULT )
C              CALL SSIZED ( 2,       CNFINE )
C
C        C
C        C     Store the time bounds of our search interval in
C        C     the CNFINE confinement window.
C        C
C              CALL STR2ET ( '2007 JAN 01', BEGTIM )
C              CALL STR2ET ( '2008 JAN 01', ENDTIM )
C
C              CALL WNINSD ( BEGTIM, ENDTIM, CNFINE )
C
C        C
C        C     Search using a step size of 1 day (in units of seconds).
C        C
C              STEP   = SPD()
C
C        C
C        C     Search for a condition where the latitudinal system
C        C     coordinate latitude in the IAU_EARTH frame has value
C        C     zero.  In this case, the pointing vector, 'DVEC',
C        C     defines the vector direction pointing at the earth
C        C     from the sun.
C        C
C              ADJUST = 0.D0
C              REFVAL = 0.D0
C              TARGET = 'EARTH'
C              OBSRVR = 'SUN'
C              DREF   = 'SEM'
C              METHOD = 'Ellipsoid'
C              FIXREF = 'IAU_EARTH'
C              CRDSYS = 'LATITUDINAL'
C              COORD  = 'LATITUDE'
C              RELATE = '='
C
C        C
C        C     Use the same aberration correction flag as that in the
C        C     SEM frame definition.
C        C
C              ABCORR = 'NONE'
C
C              CALL GFSNTC (  TARGET,  FIXREF,
C             .               METHOD,  ABCORR, OBSRVR,
C             .               DREF,    DVEC,
C             .               CRDSYS,  COORD,
C             .               RELATE,  REFVAL,
C             .               ADJUST,  STEP,   CNFINE,
C             .               NINTRVL, NWMAX,  WORK,   RESULT )
C
C        C
C        C     Check the number of intervals in the result window.
C        C
C              COUNT = WNCARD(RESULT)
C
C        C
C        C     List the beginning and ending points in each interval
C        C     if RESULT contains data.
C        C
C              IF ( COUNT .EQ. 0 ) THEN
C
C                 WRITE (*, '(A)') 'Result window is empty.'
C
C              ELSE
C
C                 DO I = 1, COUNT
C
C        C
C        C           Fetch the endpoints of the Ith interval
C        C           of the result window.
C        C
C                    CALL WNFETD ( RESULT, I, BEG, END  )
C
C                    CALL TIMOUT ( BEG,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             .      //        '(TDB) ::TDB ::RND',  BEGSTR )
C                    CALL TIMOUT ( END,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             .      //        '(TDB) ::TDB ::RND',  ENDSTR )
C
C                    WRITE (*,*) 'Interval ',  I
C                    WRITE (*,*) 'Beginning TDB ', BEGSTR
C                    WRITE (*,*) 'Ending TDB    ', ENDSTR
C
C                 END DO
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
C         Interval            1
C         Beginning TDB 2007-MAR-21 00:01:25.495120 (TDB)
C         Ending TDB    2007-MAR-21 00:01:25.495120 (TDB)
C         Interval            2
C         Beginning TDB 2007-SEP-23 09:46:39.574124 (TDB)
C         Ending TDB    2007-SEP-23 09:46:39.574124 (TDB)
C
C
C     2) Find the time during 2007 for which the intercept point on the
C        earth of the sun-to-earth vector as described in Example 1 in
C        the IAU_EARTH frame lies within a geodetic latitude-longitude
C        "box" defined as
C
C            16 degrees <= latitude  <= 17 degrees
C            85 degrees <= longitude <= 86 degrees
C
C        This problem requires four searches, each search on one of the
C        box restrictions. The user needs also realize the temporal
C        behavior of latitude greatly differs from that of the
C        longitude. The intercept latitude varies between approximately
C        23.44 degrees and -23.44 degrees during the year. The intercept
C        longitude varies between -180 degrees and 180 degrees in one
C        day.
C
C        Use the meta-kernel and the frames kernel from the first
C        example.
C
C        Example code begins here.
C
C
C              PROGRAM GFSNTC_EX2
C              IMPLICIT NONE
C
C        C
C        C     Include GF parameter declarations:
C        C
C              INCLUDE               'gf.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      SPD
C              DOUBLE PRECISION      RPD
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C        C
C        C     Create 50 windows.
C        C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 1000 )
C
C        C
C        C     One window consists of two intervals.
C        C
C              INTEGER               NINTRVL
C              PARAMETER           ( NINTRVL = MAXWIN *2 )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 64 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(STRLEN)    BEGSTR
C              CHARACTER*(STRLEN)    ENDSTR
C              CHARACTER*(STRLEN)    TARGET
C              CHARACTER*(STRLEN)    OBSRVR
C              CHARACTER*(STRLEN)    DREF
C              CHARACTER*(STRLEN)    ABCORR
C              CHARACTER*(STRLEN)    METHOD
C              CHARACTER*(STRLEN)    FIXREF
C              CHARACTER*(STRLEN)    CRDSYS
C              CHARACTER*(STRLEN)    COORD
C              CHARACTER*(STRLEN)    RELATE
C
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      DVEC    ( 3 )
C              DOUBLE PRECISION      CNFINE  ( LBCELL : 2       )
C              DOUBLE PRECISION      RESULT1 ( LBCELL : NINTRVL )
C              DOUBLE PRECISION      RESULT2 ( LBCELL : NINTRVL )
C              DOUBLE PRECISION      RESULT3 ( LBCELL : NINTRVL )
C              DOUBLE PRECISION      RESULT4 ( LBCELL : NINTRVL )
C              DOUBLE PRECISION      WORK    ( LBCELL : NINTRVL, NWMAX )
C
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      ENDTIM
C              DOUBLE PRECISION      BEG
C              DOUBLE PRECISION      END
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      ADJUST
C
C              INTEGER               COUNT
C              INTEGER               I
C
C        C
C        C     Saved variables
C        C
C        C     The confinement, workspace and result windows CNFINE,
C        C     WORK, RESULT1, RESULT2, RESULT3 and RESULT4 are saved
C        C     because this practice helps to prevent stack overflow.
C        C
C              SAVE                  CNFINE
C              SAVE                  RESULT1
C              SAVE                  RESULT2
C              SAVE                  RESULT3
C              SAVE                  RESULT4
C              SAVE                  WORK
C
C        C
C        C     The SEM frame defines the X axis as always earth
C        C     pointing.
C        C
C        C     Define the earth pointing vector in the SEM frame.
C        C
C              DATA                  DVEC   / 1.D0, 0.D0, 0.D0 /
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ('gfsntc_ex1.tm')
C              CALL FURNSH ('gfsntc_sem.tf')
C
C        C
C        C     Initialize windows RESULT and CNFINE.
C        C
C              CALL SSIZED ( NINTRVL, RESULT1 )
C              CALL SSIZED ( NINTRVL, RESULT2 )
C              CALL SSIZED ( NINTRVL, RESULT3 )
C              CALL SSIZED ( NINTRVL, RESULT4 )
C              CALL SSIZED ( 2,       CNFINE  )
C
C        C
C        C     Store the time bounds of our search interval in
C        C     the CNFINE confinement window.
C        C
C              CALL STR2ET ( '2007 JAN 01', BEGTIM )
C              CALL STR2ET ( '2008 JAN 01', ENDTIM )
C
C              CALL WNINSD ( BEGTIM, ENDTIM, CNFINE )
C
C        C
C        C     The latitude varies relatively slowly, ~46 degrees during
C        C     the year. The extrema occur approximately every six
C        C     months.  Search using a step size less than half that
C        C     value (180 days). For this example use ninety days (in
C        C     units of seconds).
C        C
C              STEP   = SPD() * 90.D0
C
C        C
C        C     Perform four searches to determine the times when the
C        C     latitude-longitude box restriction conditions apply. In
C        C     this case, the pointing vector, 'DVEC', defines the
C        C     vector direction pointing at the earth from the sun.
C        C
C        C     Use geodetic coordinates.
C        C
C              ADJUST = 0.D0
C              TARGET = 'EARTH'
C              OBSRVR = 'SUN'
C              DREF   = 'SEM'
C              METHOD = 'Ellipsoid'
C              FIXREF = 'IAU_EARTH'
C              CRDSYS = 'GEODETIC'
C
C        C
C        C     Use the same aberration correction flag as that in the
C        C     SEM frame definition.
C        C
C              ABCORR = 'NONE'
C
C        C
C        C     Perform the searches such that the result window of a
C        C     search serves as the confinement window of the subsequent
C        C     search.
C        C
C        C     Since the latitude coordinate varies slowly and is well
C        C     behaved over the time of the confinement window, search
C        C     first for the windows satisfying the latitude
C        C     requirements, then use that result as confinement for
C        C     the longitude search.
C        C
C              COORD  = 'LATITUDE'
C              REFVAL = 16.D0 * RPD()
C              RELATE = '>'
C
C              CALL GFSNTC (  TARGET,  FIXREF,
C             .               METHOD,  ABCORR, OBSRVR,
C             .               DREF,    DVEC,
C             .               CRDSYS,  COORD,
C             .               RELATE,  REFVAL,
C             .               ADJUST,  STEP,   CNFINE,
C             .               NINTRVL, NWMAX,  WORK,   RESULT1 )
C
C              REFVAL = 17.D0 * RPD()
C              RELATE = '<'
C
C              CALL GFSNTC (  TARGET,  FIXREF,
C             .               METHOD,  ABCORR, OBSRVR,
C             .               DREF,    DVEC,
C             .               CRDSYS,  COORD,
C             .               RELATE,  REFVAL,
C             .               ADJUST,  STEP,   RESULT1,
C             .               NINTRVL, NWMAX,  WORK,    RESULT2 )
C
C
C        C
C        C     Now the longitude search.
C        C
C              COORD  = 'LONGITUDE'
C
C        C
C        C     Reset the step size to something appropriate for the 360
C        C     degrees in 24 hours domain. The longitude shows near
C        C     linear behavior so use a step size less than half the
C        C     period of twelve hours. Ten hours will suffice in this
C        C     case.
C        C
C              STEP   = SPD() * (10.D0/24.D0)
C
C              REFVAL = 85.D0 * RPD()
C              RELATE = '>'
C
C              CALL GFSNTC (  TARGET,  FIXREF,
C             .               METHOD,  ABCORR, OBSRVR,
C             .               DREF,    DVEC,
C             .               CRDSYS,  COORD,
C             .               RELATE,  REFVAL,
C             .               ADJUST,  STEP,   RESULT2,
C             .               NINTRVL, NWMAX,  WORK,    RESULT3 )
C
C        C
C        C     Contract the endpoints of each window to account
C        C     for possible round-off error at the -180/180 degree
C        C     branch.
C        C
C        C     A contraction value of a millisecond should eliminate
C        C     any round-off caused branch crossing.
C        C
C              CALL WNCOND ( 1D-3, 1D-3, RESULT3 )
C
C              REFVAL = 86.D0 * RPD()
C              RELATE = '<'
C
C              CALL GFSNTC (  TARGET,  FIXREF,
C             .               METHOD,  ABCORR, OBSRVR,
C             .               DREF,    DVEC,
C             .               CRDSYS,  COORD,
C             .               RELATE,  REFVAL,
C             .               ADJUST,  STEP,   RESULT3,
C             .               NINTRVL, NWMAX,  WORK,    RESULT4 )
C
C        C
C        C     Check the number of intervals in the result window.
C        C
C              COUNT = WNCARD(RESULT4)
C
C        C
C        C     List the beginning and ending points in each interval
C        C     if RESULT contains data.
C        C
C              IF ( COUNT .EQ. 0 ) THEN
C
C                 WRITE(*, '(A)') 'Result window is empty.'
C
C              ELSE
C
C                 DO I = 1, COUNT
C
C        C
C        C           Fetch the endpoints of the Ith interval
C        C           of the result window.
C        C
C                    CALL WNFETD ( RESULT4, I, BEG, END  )
C
C                    CALL TIMOUT ( BEG,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             .      //        '(TDB) ::TDB ::RND',  BEGSTR )
C                    CALL TIMOUT ( END,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             .      //        '(TDB) ::TDB ::RND',  ENDSTR )
C
C                    WRITE(*,*) 'Interval ',  I
C                    WRITE(*,*) 'Beginning TDB ', BEGSTR
C                    WRITE(*,*) 'Ending TDB    ', ENDSTR
C                    WRITE(*,*) ' '
C
C                 END DO
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
C         Interval            1
C         Beginning TDB 2007-MAY-05 06:14:04.637735 (TDB)
C         Ending TDB    2007-MAY-05 06:18:03.621906 (TDB)
C
C         Interval            2
C         Beginning TDB 2007-MAY-06 06:13:59.583483 (TDB)
C         Ending TDB    2007-MAY-06 06:17:58.569238 (TDB)
C
C         Interval            3
C         Beginning TDB 2007-MAY-07 06:13:55.102940 (TDB)
C         Ending TDB    2007-MAY-07 06:17:54.090298 (TDB)
C
C         Interval            4
C         Beginning TDB 2007-AUG-06 06:23:17.282927 (TDB)
C         Ending TDB    2007-AUG-06 06:27:16.264009 (TDB)
C
C         Interval            5
C         Beginning TDB 2007-AUG-07 06:23:10.545441 (TDB)
C         Ending TDB    2007-AUG-07 06:27:09.524924 (TDB)
C
C         Interval            6
C         Beginning TDB 2007-AUG-08 06:23:03.233996 (TDB)
C         Ending TDB    2007-AUG-08 06:27:02.211888 (TDB)
C
C         Interval            7
C         Beginning TDB 2007-AUG-09 06:22:55.351256 (TDB)
C         Ending TDB    2007-AUG-09 06:26:54.327565 (TDB)
C
C
C$ Restrictions
C
C     1)  The kernel files to be used by this routine must be loaded
C         (normally using the SPICELIB routine FURNSH) before this
C         routine is called.
C
C     2)  This routine has the side effect of re-initializing the
C         coordinate quantity utility package. Callers may
C         need to re-initialize the package after calling this routine.
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
C-    SPICELIB Version 1.2.0, 27-OCT-2021 (JDR) (NJB)
C
C        Added initialization of QCPARS(10) to pacify Valgrind.
C
C        Edited the header to comply with NAIF standard.
C
C        Fixed bug in code example #2. Renamed example's meta-kernel.
C        Added SAVE statements for CNFINE, WORK, RESULT, RESULT1,
C        RESULT2, RESULT3 and RESULT4 variables in code examples.
C
C        Added parameter NWMAX's description. Updated $Files section.
C        Added entries #5 and $9 in $Exceptions section.
C
C        Updated description of WORK and RESULT arguments in $Brief_I/O,
C        $Detailed_Input and $Detailed_Output. Extended description of
C        COORD argument.
C
C        Updated header to describe use of expanded confinement window.
C
C-    SPICELIB Version 1.1.0, 05-SEP-2012 (EDW)
C
C        Edit to comments to correct search description.
C
C        Implemented use of ZZHOLDD to allow user to alter convergence
C        tolerance.
C
C        Removed the STEP > 0 error check. The GFSSTP call includes
C        the check.
C
C-    SPICELIB Version 1.0.1, 16-FEB-2010 (NJB) (EDW)
C
C        Edits to and corrections of argument descriptions and
C        header.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW)
C
C-&


C$ Index_Entries
C
C     GF surface intercept coordinate search
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      INTEGER               SIZED
      LOGICAL               EVEN

      EXTERNAL              GFBAIL

C
C     Routines to set step size, refine transition times
C     and report work.
C
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP

C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               QNPARS
      PARAMETER           ( QNPARS = 10 )

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )

      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )

C
C     Local variables
C
      DOUBLE PRECISION      TOL
      LOGICAL               OK

C
C     Quantity definition parameter arrays:
C
      CHARACTER*(LNSIZE)    QCPARS ( QNPARS )
      CHARACTER*(LNSIZE)    QPNAMS ( QNPARS )

      DOUBLE PRECISION      QDPARS ( QNPARS )

      INTEGER               QIPARS ( QNPARS )

      LOGICAL               QLPARS ( QNPARS )


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Check into the error subsystem.
C
      CALL CHKIN( 'GFSNTC' )

C
C     Confirm minimum window sizes.
C
      IF ( MW .LT. 2 .OR. .NOT. EVEN(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFSNTC'                                    )
         RETURN

      END IF

      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2.'                               )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFSNTC'                                    )
         RETURN

      END IF

C
C     Set up a call to GFEVNT specific to the surface intercept
C     coordinate search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  TARGET

      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  OBSRVR

      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  ABCORR

      QPNAMS(4) = 'COORDINATE SYSTEM'
      QCPARS(4) =  CRDSYS

      QPNAMS(5) = 'COORDINATE'
      QCPARS(5) =  COORD

      QPNAMS(6) = 'REFERENCE FRAME'
      QCPARS(6) =  FIXREF

      QPNAMS(7) = 'VECTOR DEFINITION'
      QCPARS(7) =  SINDEF

      QPNAMS(8) = 'METHOD'
      QCPARS(8) =  METHOD

      QPNAMS(9) = 'DREF'
      QCPARS(9) =  DREF

      QPNAMS(10) = 'DVEC'
      QDPARS(1)  = DVEC(1)
      QDPARS(2)  = DVEC(2)
      QDPARS(3)  = DVEC(3)

C
C     Initialize QCPARS(10) since GFEVNT will try to
C     left-justify it and convert it to upper case.
C
      QCPARS(10) = ' '

C
C     Set the step size.
C
      CALL GFSSTP (STEP)

C
C     Retrieve the convergence tolerance, if set.
C
      CALL ZZHOLDD ( ZZGET, GF_TOL, OK, TOL )

C
C     Use the default value CNVTOL if no stored tolerance value.
C
      IF ( .NOT. OK ) THEN

         TOL = CNVTOL

      END IF

C
C     Initialize the RESULT window to empty.
C
      CALL SCARDD ( 0, RESULT)

C
C     Look for solutions.
C
C     Progress report and interrupt options are set to .FALSE.
C
      CALL GFEVNT ( GFSTEP,   GFREFN,  'COORDINATE',
     .              QNPARS,   QPNAMS,   QCPARS,
     .              QDPARS,   QIPARS,   QLPARS,
     .              RELATE,   REFVAL,   TOL,
     .              ADJUST,   CNFINE,   NORPT,
     .              GFREPI,   GFREPU,   GFREPF,
     .              MW,       NW,       WORK,
     .              NOBAIL,  GFBAIL,   RESULT )

      CALL CHKOUT( 'GFSNTC' )
      RETURN

      END

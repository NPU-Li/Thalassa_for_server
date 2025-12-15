C$Procedure GFPA ( GF, phase angle search )

      SUBROUTINE GFPA ( TARGET, ILLMN,  ABCORR, OBSRVR,
     .                  RELATE, REFVAL, ADJUST, STEP,
     .                  CNFINE, MW,     NW,     WORK,
     .                  RESULT )

C$ Abstract
C
C     Determine time intervals for which a specified constraint
C     on the phase angle between an illumination source, a target,
C     and observer body centers is met.
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
C     NAIF_IDS
C     SPK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     EPHEMERIS
C     EVENT
C     GEOMETRY
C     SEARCH
C     WINDOW
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLMN
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Default convergence tolerance.
C     TARGET     I   Name of the target body.
C     ILLMN      I   Name of the illuminating body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     RELATE     I   Relational operator.
C     REFVAL     I   Reference value.
C     ADJUST     I   Adjustment value for absolute extrema searches.
C     STEP       I   Step size used for locating extrema and roots.
C     CNFINE     I   SPICE window to which the search is confined.
C     MW         I   Workspace window size.
C     NW         I   The number of workspace windows needed for
C                    the search.
C     WORK       O   Array of workspace windows.
C     RESULT    I-O  SPICE window containing results.
C
C$ Detailed_Input
C
C     TARGET   is the name of a target body. Optionally, you may supply
C              a string containing the integer ID code for the object.
C              For example both 'MOON' and '301' are legitimate strings
C              that indicate the Moon is the target body.
C
C              Case and leading or trailing blanks are not significant
C              in the string TARGET.
C
C     ILLMN    is the name of the illuminating body. This will normally
C              be 'SUN' but the algorithm can use any ephemeris object.
C
C              Case and leading or trailing blanks are not significant
C              in the string ILLMN.
C
C     ABCORR   is the description of the aberration corrections to apply
C              to the state evaluations to account for one-way light
C              time and stellar aberration.
C
C              This routine accepts only reception mode aberration
C              corrections. See the header of SPKEZR for a detailed
C              description of the aberration correction options. For
C              convenience, the allowed aberration options are listed
C              below:
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
C                'CN+S'      "Reception" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              Case and leading or trailing blanks are not significant
C              in the string ABCORR.
C
C     OBSRVR   is the name of an observing body. Optionally, you may
C              supply a string containing the integer ID code for the
C              object. For example both "MOON" and "301" are legitimate
C              strings that indicate the Moon is the observer.
C
C              Case and leading or trailing blanks are not significant
C              in the string OBSRVR.
C
C     RELATE   is a relational operator used to define a constraint on a
C              specified phase angle. The result window found by this
C              routine indicates the time intervals where the constraint
C              is satisfied. Supported values of RELATE and
C              corresponding meanings are shown below:
C
C                 '>'        The phase angle value is greater than the
C                            reference value REFVAL.
C
C                 '='        The phase angle value is equal to the
C                            reference value REFVAL.
C
C                 '<'        The phase angle value is less than the
C                            reference value REFVAL.
C
C                 'ABSMAX'   The phase angle value is at an absolute
C                            maximum.
C
C                 'ABSMIN'   The phase angle value is at an absolute
C                            minimum.
C
C                 'LOCMAX'   The phase angle value is at a local
C                            maximum.
C
C                 'LOCMIN'   The phase angle value is at a local
C                            minimum.
C
C              RELATE may be used to specify an "adjusted" absolute
C              extremum constraint: this requires the phase angle to be
C              within a specified offset relative to an absolute
C              extremum. The argument ADJUST (described below) is used
C              to specify this offset.
C
C              Local extrema are considered to exist only in the
C              interiors of the intervals comprising the confinement
C              window:  a local extremum cannot exist at a boundary
C              point of the confinement window.
C
C              Case and leading or trailing blanks are not
C              significant in the string RELATE.
C
C     REFVAL   is the double precision reference value used together
C              with the argument RELATE to define an equality or
C              inequality to be satisfied by the phase angle. See the
C              discussion of RELATE above for further information.
C
C              The units of REFVAL are radians.
C
C     ADJUST   is a double precision value used to modify searches for
C              absolute extrema: when RELATE is set to 'ABSMAX' or
C              'ABSMIN' and ADJUST is set to a positive value, GFPA
C              finds times when the phase angle is within ADJUST radians
C              of the specified extreme value.
C
C              For RELATE set to 'ABSMAX', the RESULT window contains
C              time intervals when the phase angle has
C              values between ABSMAX - ADJUST and ABSMAX.
C
C              For RELATE set to 'ABSMIN', the RESULT window contains
C              time intervals when the phase angle has
C              values between ABSMIN and ABSMIN + ADJUST.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions.
C
C     STEP     is the double precision time step size to use in the
C              search.
C
C              STEP must be short enough for a search using this step
C              size to locate the time intervals where the phase angle
C              function is monotone increasing or decreasing. However,
C              STEP must not be *too* short, or the search will take an
C              unreasonable amount of time.
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
C              The endpoints of the time intervals comprising CNFINE are
C              interpreted as seconds past J2000 TDB.
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
C              parameter NWPA; this parameter is declared in the
C              include file gf.inc. (The reason this dimension is
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
C              discarded before GFPA conducts its search.
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
C                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWPA )
C
C              where MW is a constant declared by the caller and
C              NWPA is a constant defined in the SPICELIB INCLUDE
C              file gf.inc. See the discussion of MW above.
C
C              WORK need not be initialized by the caller.
C
C              WORK is modified by this routine. The caller should
C              re-initialize this array before attempting to use it for
C              any other purpose.
C
C     RESULT   is the SPICE window of intervals, contained within the
C              confinement window CNFINE, on which the specified phase
C              angle constraint is satisfied.
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
C     CNVTOL   is the default convergence tolerance used for finding
C              endpoints of the intervals comprising the result
C              window. CNVTOL is also used for finding intermediate
C              results; in particular, CNVTOL is used for finding the
C              windows on which the phase angle is increasing
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
C         This routine does not diagnose invalid step sizes, except that
C         if the step size is non-positive, an error is signaled by a
C         routine in the call tree of this routine.
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
C     3)  If workspace window size, MW, is not at least 2 and an even
C         value, the error SPICE(INVALIDDIMENSION) is signaled is
C         signaled.
C
C     4)  If workspace window count, NW, is not at least NWPA, the error
C         SPICE(INVALIDDIMENSION) is signaled is signaled.
C
C     5)  If result window, RESULT, is not at least 2 and an even value,
C         the error SPICE(INVALIDDIMENSION) is signaled is signaled.
C
C     6)  If RESULT has insufficient capacity to contain the
C         number of intervals on which the specified angle condition
C         is met, an error is signaled by a routine in the call
C         tree of this routine.
C
C     7)  If an error (typically cell overflow) occurs during
C         window arithmetic, the error is signaled by a routine
C         in the call tree of this routine.
C
C     8)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     9)  If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     10) If ADJUST has a non-zero value when RELATE has any value other
C         than 'ABSMIN' or 'ABSMAX', an error is signaled by a routine
C         in the call tree of this routine.
C
C     11) If any of the input body names, TARGET, ILLMN, OBSRVR, do
C         not map to NAIF ID codes, an error is signaled by a routine
C         in the call tree of this routine.
C
C     12) If the input body names, TARGET, ILLMN, OBSRVR, are not
C         distinct, an error is signaled by a routine in the call
C         tree of this routine.
C
C     13) If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C     14) If the aberration correction specifier contains an
C         unrecognized value, an error is signaled by a routine in the
C         call tree of this routine.
C
C     15) If a transmit mode aberration correction is requested, an
C         error is signaled by a routine in the call tree of this
C         routine.
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
C     -  In some cases the observer's state may be computed at times
C        outside of CNFINE by as much as 2 seconds; data required to
C        compute this state must be provided by loaded kernels. See
C        $Particulars for details.
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C                        ILLMN      OBS
C        ILLMN as seen      *       /
C        from TARG at       |      /
C        ET - LT.           |     /
C                          >|..../< phase angle
C                           |   /
C                         . |  /
C                       .   | /
C                      .     *     TARG as seen from OBS
C                SEP   .   TARG    at ET
C                       .  /
C                         /
C                        *
C
C     This routine determines if the caller-specified constraint
C     condition on the geometric event (phase angle) is satisfied for
C     any time intervals within the confinement window CNFINE. If one
C     or more such time intervals exist, those intervals are added
C     to the RESULT window.
C
C     This routine provides a simpler, but less flexible interface
C     than does the routine GFEVNT for conducting searches for
C     illuminator-target-observer phase angle value events.
C     Applications that require support for progress reporting,
C     interrupt handling, non-default step or refinement functions
C     should call GFEVNT rather than this routine.
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
C     periods, within the confinement window, over which the
C     phase angle function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the phase angle
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
C     change of phase angle will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the
C     phase angle is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the phase angle function is monotone:
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
C     Having some knowledge of the relative geometry of the target,
C     illumination source, and observer can be a valuable aid in
C     picking a reasonable step size. In general, the user can
C     compensate for lack of such knowledge by picking a very short
C     step size; the cost is increased computation time.
C
C     Note that the step size is not related to the precision with which
C     the endpoints of the intervals of the result window are computed.
C     That precision level is controlled by the convergence tolerance.
C
C
C     Convergence Tolerance
C     =====================
C
C     As described above, the root-finding process used by this routine
C     involves first bracketing roots and then using a search process
C     to locate them. "Roots" are both times when local extrema are
C     attained and times when the geometric quantity function is equal
C     to a reference value. All endpoints of the intervals comprising
C     the result window are either endpoints of intervals of the
C     confinement window or roots.
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
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Determine the time windows from December 1, 2006 UTC to
C        January 31, 2007 UTC for which the sun-moon-earth configuration
C        phase angle satisfies the relation conditions with respect to a
C        reference value of .57598845 radians (the phase angle at
C        January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also
C        determine the time windows corresponding to the local maximum
C        and minimum phase angles, and the absolute maximum and minimum
C        phase angles during the search interval. The configuration
C        defines the Sun as the illuminator, the Moon as the target,
C        and the Earth as the observer.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: gfpa_ex1.tm
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
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00009.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM GFPA_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include GF parameter declarations:
C        C
C              INCLUDE 'gf.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      SPD
C              DOUBLE PRECISION      PHASEQ
C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C        C
C        C     Use the parameter MAXWIN for both the result window size
C        C     and the workspace size.
C        C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 1000 )
C
C        C
C        C     Length of strings:
C        C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 26 )
C
C              INTEGER               NLOOPS
C              PARAMETER           ( NLOOPS = 7 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(TIMLEN)    RELATE (NLOOPS)
C              CHARACTER*(6)         ABCORR
C              CHARACTER*(6)         ILLMN
C              CHARACTER*(6)         OBSRVR
C              CHARACTER*(6)         TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWPA )
C              DOUBLE PRECISION      ADJUST
C              DOUBLE PRECISION      ET0
C              DOUBLE PRECISION      ET1
C              DOUBLE PRECISION      FINISH
C              DOUBLE PRECISION      PHASE
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      START
C              DOUBLE PRECISION      STEP
C
C              INTEGER               I
C              INTEGER               J
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
C        C     The relation values for the search.
C        C
C              DATA                  RELATE / '=',
C             .                               '<',
C             .                               '>',
C             .                               'LOCMIN',
C             .                               'ABSMIN',
C             .                               'LOCMAX',
C             .                               'ABSMAX'  /
C
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( 'gfpa_ex1.tm' )
C
C        C
C        C     Initialize windows.
C        C
C              CALL SSIZED ( MAXWIN, RESULT )
C              CALL SSIZED ( 2,      CNFINE )
C
C        C
C        C     Store the time bounds of our search interval in
C        C     the confinement window.
C        C
C              CALL STR2ET ( '2006 DEC 01', ET0 )
C              CALL STR2ET ( '2007 JAN 31', ET1 )
C
C              CALL WNINSD ( ET0, ET1, CNFINE )
C
C        C
C        C     Search using a step size of 1 day (in units of seconds).
C        C     The reference value is 0.57598845 radians. We're not
C        C     using the adjustment feature, so we set ADJUST to zero.
C        C
C              STEP   = SPD()
C              REFVAL = 0.57598845D0
C              ADJUST = 0.D0
C
C        C
C        C     Define the values for target, observer, illuminator, and
C        C     aberration correction.
C        C
C              TARGET = 'MOON'
C              ILLMN  = 'SUN'
C              ABCORR = 'LT+S'
C              OBSRVR = 'EARTH'
C
C              DO J=1, NLOOPS
C
C                 WRITE(*,*) 'Relation condition: ', RELATE(J)
C
C        C
C        C        Perform the search. The SPICE window RESULT contains
C        C        the set of times when the condition is met.
C        C
C                 CALL GFPA (  TARGET,    ILLMN,  ABCORR, OBSRVR,
C             .                RELATE(J), REFVAL, ADJUST, STEP,
C             .                CNFINE,    MAXWIN, NWPA,   WORK,
C             .                RESULT )
C
C        C
C        C        Display the results.
C        C
C                 IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                    WRITE (*, '(A)') 'Result window is empty.'
C
C                 ELSE
C
C                    DO I = 1, WNCARD(RESULT)
C        C
C        C              Fetch the endpoints of the Ith interval
C        C              of the result window.
C        C
C                       CALL WNFETD ( RESULT, I, START, FINISH )
C
C                       PHASE = PHASEQ( START, TARGET, ILLMN, OBSRVR,
C             .                         ABCORR )
C                       CALL TIMOUT ( START,
C             .                       'YYYY-MON-DD HR:MN:SC.###',
C             .                       TIMSTR                          )
C
C                       WRITE (*, '(A,F16.9)') 'Start time = '//TIMSTR,
C             .                                                  PHASE
C
C
C                       PHASE = PHASEQ( FINISH, TARGET, ILLMN, OBSRVR,
C             .                         ABCORR )
C                       CALL TIMOUT ( FINISH,
C             .                       'YYYY-MON-DD HR:MN:SC.###',
C             .                       TIMSTR                          )
C
C                       WRITE (*, '(A,F16.9)') 'Stop time  = '//TIMSTR,
C             .                                                  PHASE
C
C                    END DO
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
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Relation condition: =
C        Start time = 2006-DEC-02 13:31:34.414       0.575988450
C        Stop time  = 2006-DEC-02 13:31:34.414       0.575988450
C        Start time = 2006-DEC-07 14:07:55.470       0.575988450
C        Stop time  = 2006-DEC-07 14:07:55.470       0.575988450
C        Start time = 2006-DEC-31 23:59:59.997       0.575988450
C        Stop time  = 2006-DEC-31 23:59:59.997       0.575988450
C        Start time = 2007-JAN-06 08:16:25.512       0.575988450
C        Stop time  = 2007-JAN-06 08:16:25.512       0.575988450
C        Start time = 2007-JAN-30 11:41:32.557       0.575988450
C        Stop time  = 2007-JAN-30 11:41:32.557       0.575988450
C
C         Relation condition: <
C        Start time = 2006-DEC-02 13:31:34.414       0.575988450
C        Stop time  = 2006-DEC-07 14:07:55.470       0.575988450
C        Start time = 2006-DEC-31 23:59:59.997       0.575988450
C        Stop time  = 2007-JAN-06 08:16:25.512       0.575988450
C        Start time = 2007-JAN-30 11:41:32.557       0.575988450
C        Stop time  = 2007-JAN-31 00:00:00.000       0.468279091
C
C         Relation condition: >
C        Start time = 2006-DEC-01 00:00:00.000       0.940714974
C        Stop time  = 2006-DEC-02 13:31:34.414       0.575988450
C        Start time = 2006-DEC-07 14:07:55.470       0.575988450
C        Stop time  = 2006-DEC-31 23:59:59.997       0.575988450
C        Start time = 2007-JAN-06 08:16:25.512       0.575988450
C        Stop time  = 2007-JAN-30 11:41:32.557       0.575988450
C
C         Relation condition: LOCMIN
C        Start time = 2006-DEC-05 00:16:50.317       0.086121423
C        Stop time  = 2006-DEC-05 00:16:50.317       0.086121423
C        Start time = 2007-JAN-03 14:18:31.977       0.079899769
C        Stop time  = 2007-JAN-03 14:18:31.977       0.079899769
C
C         Relation condition: ABSMIN
C        Start time = 2007-JAN-03 14:18:31.977       0.079899769
C        Stop time  = 2007-JAN-03 14:18:31.977       0.079899769
C
C         Relation condition: LOCMAX
C        Start time = 2006-DEC-20 14:09:10.392       3.055062862
C        Stop time  = 2006-DEC-20 14:09:10.392       3.055062862
C        Start time = 2007-JAN-19 04:27:54.600       3.074603891
C        Stop time  = 2007-JAN-19 04:27:54.600       3.074603891
C
C         Relation condition: ABSMAX
C        Start time = 2007-JAN-19 04:27:54.600       3.074603891
C        Stop time  = 2007-JAN-19 04:27:54.600       3.074603891
C
C
C$ Restrictions
C
C     1)  The kernel files to be used by this routine must be loaded
C         (normally using the SPICELIB routine FURNSH) before this
C         routine is called.
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
C-    SPICELIB Version 1.0.1, 27-OCT-2021 (JDR) (NJB)
C
C        Edited the header to comply with NAIF standard.
C
C        Updated description of RELATE, REFVAL, WORK and RESULT
C        arguments in $Brief_I/O, $Detailed_Input and $Detailed_Output.
C
C        Added SAVE statements for CNFINE, WORK and RESULT variables in
C        code example.
C
C        Replaced entry #9 by new entries #9 and #10, and added entry
C        #14 in $Exceptions.
C
C        Updated header to describe use of expanded confinement window.
C
C-    SPICELIB Version 1.0.0, 15-JUL-2014 (EDW) (NJB)
C
C-&


C$ Index_Entries
C
C     GF phase angle search
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      LOGICAL               ODD

      INTEGER               SIZED

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
      PARAMETER           ( QNPARS = 4 )

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
      CALL CHKIN( 'GFPA' )

C
C     Confirm minimum window sizes.
C
      IF ( (MW .LT. 2) .OR. ODD(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                  )
         CALL CHKOUT ( 'GFPA'                                     )
         RETURN

      END IF


      IF ( NW .LT. NWPA ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                )
         CALL ERRINT ( '#',  NW                                     )
         CALL ERRINT ( '#',  NWPA                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                    )
         CALL CHKOUT ( 'GFPA'                                       )
         RETURN

      END IF


C
C     Check the result window size.
C
      IF ( (SIZED(RESULT) .LT. 2) .OR. ODD( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFPA'                                      )
         RETURN

      END IF


C
C     Set up a call to GFEVNT specific to the phase angle search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  TARGET

      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  OBSRVR

      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  ABCORR

      QPNAMS(4) = 'ILLUM'
      QCPARS(4) =  ILLMN

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
      CALL GFEVNT ( GFSTEP,   GFREFN,  'PHASE ANGLE', QNPARS,
     .              QPNAMS,   QCPARS,   QDPARS,       QIPARS,
     .              QLPARS,   RELATE,   REFVAL,       TOL,
     .              ADJUST,   CNFINE,   NORPT,        GFREPI,
     .              GFREPU,   GFREPF,   MW,           NWPA,
     .              WORK,     NOBAIL,   GFBAIL,       RESULT  )


      CALL CHKOUT( 'GFPA' )
      RETURN

      END

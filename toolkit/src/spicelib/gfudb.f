C$Procedure GFUDB ( GF, user defined boolean )

      SUBROUTINE GFUDB ( UDFUNS, UDFUNB, STEP, CNFINE, RESULT )

C$ Abstract
C
C     Perform a GF search on a user defined boolean quantity.
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
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     EPHEMERIS
C     EVENT
C     SEARCH
C     WINDOW
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      EXTERNAL              UDFUNS
      EXTERNAL              UDFUNB
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Convergence tolerance.
C     UDFUNS     I   Name of the routine that computes a scalar
C                    quantity corresponding to an ET.
C     UDFUNB     I   Name of the routine returning the boolean value
C                    corresponding to an ET.
C     STEP       I   Constant step size in seconds for finding geometric
C                    events.
C     CNFINE     I   SPICE window to which the search is restricted.
C     RESULT    I-O  SPICE window containing results.
C
C$ Detailed_Input
C
C     UDFUNS   is the routine that returns the value of the scalar
C              quantity of interest at time ET. The calling sequence for
C              UDFUNC is:
C
C                 CALL UDFUNS ( ET, VALUE )
C
C              where:
C
C                 ET      a double precision value representing
C                         ephemeris time, expressed as seconds past
C                         J2000 TDB at which to evaluate UDFUNS.
C
C                 VALUE   is the value of the scalar quantity
C                         at ET.
C
C     UDFUNB   is the user defined routine returning a boolean value for
C              an epoch ET. The calling sequence for UNFUNB is:
C
C                 CALL UDFUNB ( UDFUNS, ET, BOOL )
C
C              where:
C
C                 UDFUNS   the name of the scalar function as
C                          defined above.
C
C                 ET       a double precision value representing
C                          ephemeris time, expressed as seconds past
C                          J2000 TDB, at which to evaluate UDFUNB.
C
C                 BOOL     the boolean value at ET.
C
C              GFUDB will correctly operate only for boolean functions
C              with true conditions defining non zero measure time
C              intervals.
C
C              Note, UDFUNB need not call UDFUNS. The use of UDFUNS is
C              determined by the needs of the calculation and the user's
C              design.
C
C     STEP     is the step size to be used in the search. STEP must be
C              shorter than any interval, within the confinement window,
C              over which the user defined boolean function is met. In
C              other words, STEP must be shorter than the shortest time
C              interval for which the boolean function is .TRUE.; STEP
C              must also be shorter than the shortest time interval
C              between two boolean function true events occurring within
C              the confinement window (see below). However, STEP must
C              not be *too* short, or the search will take an
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
C     CNFINE   is a SPICE window that confines the time period over
C              which the specified search is conducted. CNFINE may
C              consist of a single interval or a collection of
C              intervals.
C
C              In some cases the confinement window can be used to
C              greatly reduce the time period that must be searched
C              for the desired solution. See the $Particulars section
C              below for further discussion.
C
C              See the $Examples section below for a code example
C              that shows how to create a confinement window.
C
C              CNFINE must be initialized by the caller via the
C              SPICELIB routine SSIZED.
C
C              Certain computations can expand the time window over
C              which UDFUNS and UDFUNB require data. See $Particulars
C              for details.
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
C              discarded before GFUDB conducts its search.
C
C$ Detailed_Output
C
C     RESULT   is a SPICE window containing the time intervals within
C              the confinement window, during which the specified
C              boolean quantity is .TRUE.
C
C              The endpoints of the time intervals comprising RESULT are
C              interpreted as seconds past J2000 TDB.
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
C              window. CNVTOL is used to determine when binary
C              searches for roots should terminate: when a root is
C              bracketed within an interval of length CNVTOL, the
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
C     3)  If an error (typically cell overflow) occurs while performing
C         window arithmetic, the error is signaled by a routine
C         in the call tree of this routine.
C
C     4)  If the size of the SPICE window RESULT is less than 2 or not
C         an even value, the error SPICE(INVALIDDIMENSION) is signaled.
C
C     5)  If RESULT has insufficient capacity to contain the number of
C         intervals on which the specified condition is met, an error is
C         signaled by a routine in the call tree of this routine.
C
C     6)  If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     If the boolean function requires access to ephemeris data:
C
C     -  SPK data: ephemeris data for any body over the
C        time period defined by the confinement window must be
C        loaded. If aberration corrections are used, the states of
C        target and observer relative to the solar system barycenter
C        must be calculable from the available ephemeris data.
C        Typically ephemeris data are made available by loading one
C        or more SPK files via FURNSH.
C
C     -  If non-inertial reference frames are used, then PCK
C        files, frame kernels, C-kernels, and SCLK kernels may be
C        needed.
C
C     -  Certain computations can expand the time window over which
C        UDFUNS and UDFUNB require data; such data must be provided by
C        loaded kernels. See $Particulars for details.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the boolean function
C     evaluates to true. The resulting set of intervals is returned
C     as a SPICE window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C     UDFUNS Default Template
C     =======================
C
C     The boolean function includes an argument for an input scalar
C     function. Use of a scalar function during the evaluation of
C     the boolean function is not required. SPICE provides a no-op
C     scalar routine, UDF, as a dummy argument for instances when
C     the boolean function does not need to call the scalar function.
C
C     The Search Process
C     ==================
C
C     The search for boolean events is treated as a search for state
C     transitions: times are sought when the boolean function value
C     changes from true to false or vice versa.
C
C     Step Size
C     =========
C
C     Each interval of the confinement window is searched as follows:
C     first, the input step size is used to determine the time
C     separation at which the boolean function will be sampled.
C     Starting at the left endpoint of the interval, samples of the
C     boolean function will be taken at each step. If a state change
C     is detected, a root has been bracketed; at that point, the
C     "root"--the time at which the state change occurs---is found by a
C     refinement process, for example, via binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the boolean function is constant:
C     the step size should be shorter than the shortest such interval
C     and the shortest separation between the intervals, within
C     the confinement window.
C
C     Having some knowledge of the relative geometry of the targets and
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
C     interval within which a solution is sought.
C
C     The confinement window also can be used to restrict a search to
C     a time window over which required data are known to be
C     available.
C
C     In some cases, the confinement window can be used to make
C     searches more efficient. Sometimes it's possible to do an
C     efficient search to reduce the size of the time period over
C     which a relatively slow search of interest must be performed.
C     See the "CASCADE" example program in gf.req for a demonstration.
C
C     Certain user-defined computations may expand the window over
C     which computations are performed. Here "expansion" of a window by
C     an amount "T" means that the left endpoint of each interval
C     comprising the window is shifted left by T, the right endpoint of
C     each interval is shifted right by T, and any overlapping
C     intervals are merged. Note that the input window CNFINE itself is
C     not modified.
C
C     Computation of observer-target states by SPKEZR or SPKEZ, using
C     stellar aberration corrections, requires the state of the
C     observer, relative to the solar system barycenter, to be computed
C     at times offset from the input time by +/- 1 second. If the input
C     time ET is used by UDFUNS or UDFUNB to compute such a state, the
C     window over which the observer state is computed is expanded by 1
C     second.
C
C     When light time corrections are used in the computation of
C     observer-target states, expansion of the search window also
C     affects the set of times at which the light time-corrected states
C     of the targets are computed.
C
C     In addition to possible expansion of the search window when
C     stellar aberration corrections are used, round-off error should
C     be taken into account when the need for data availability is
C     analyzed.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Calculate the time intervals when the position of the Moon
C        relative to the Earth in the IAU_EARTH frame has a positive
C        value for the Z position component, also with a positive value
C        for the Vz velocity component.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: gfudb_ex1.tm
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
C              de418.bsp                     Planetary ephemeris
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de418.bsp',
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
C              PROGRAM GFUDB_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               WNCARD
C              DOUBLE PRECISION      SPD
C
C        C
C        C     User defined external routines
C        C
C              EXTERNAL              UDF
C              EXTERNAL              GFB
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
C              PARAMETER           ( MAXWIN = 100 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(32)        UTC
C
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      RIGHT
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      ETS
C              DOUBLE PRECISION      ETE
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      STATE  (6)
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2      )
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C
C              INTEGER               I
C
C        C
C        C     Saved variables
C        C
C        C     The confinement and result windows CNFINE and RESULT are
C        C     saved because this practice helps to prevent stack
C        C     overflow.
C        C
C              SAVE                  CNFINE
C              SAVE                  RESULT
C
C        C
C        C     Load needed kernels.
C        C
C              CALL FURNSH ( 'gfudb_ex1.tm' )
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
C              CALL STR2ET ( 'Jan 1 2011', ETS )
C              CALL STR2ET ( 'Apr 1 2011', ETE )
C              CALL WNINSD ( ETS, ETE, CNFINE  )
C
C        C
C        C     The moon orbit about the earth-moon barycenter is
C        C     twenty-eight days. The event condition occurs
C        C     during (very) approximately a quarter of the orbit. Use
C        C     a step of five days.
C        C
C              STEP = 5.D0 * SPD()
C
C              CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
C
C              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                    WRITE (*, '(A)') 'Result window is empty.'
C
C              ELSE
C
C                 DO I = 1, WNCARD(RESULT)
C
C        C
C        C           Fetch and display each RESULT interval.
C        C
C                    CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C                    WRITE (*,*) 'Interval ', I
C
C                    CALL ET2UTC ( LEFT, 'C', 4, UTC )
C                    WRITE (*, *) '   Interval start: ', UTC
C
C                    CALL SPKEZ ( 301, LEFT, 'IAU_EARTH', 'NONE', 399,
C             .                   STATE, LT )
C                    WRITE (*, *) '                Z= ', STATE(3)
C                    WRITE (*, *) '               Vz= ', STATE(6)
C
C                    CALL ET2UTC ( RIGHT, 'C', 4, UTC )
C                    WRITE (*, *) '   Interval end  : ', UTC
C
C                    CALL SPKEZ ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399,
C             .                   STATE, LT )
C                    WRITE (*, *) '                Z= ', STATE(3)
C                    WRITE (*, *) '               Vz= ', STATE(6)
C                    WRITE (*, *) ' '
C
C                 END DO
C
C              END IF
C
C              END
C
C
C
C        C-Procedure GFB
C        C
C        C     User defined boolean routine.
C        C
C
C              SUBROUTINE GFB ( UDFUNS, ET, BOOL )
C              IMPLICIT NONE
C
C        C- Abstract
C        C
C        C     User defined geometric boolean function:
C        C
C        C        Z >= 0 with dZ/dt > 0.
C        C
C
C              EXTERNAL              UDFUNS
C
C              DOUBLE PRECISION      ET
C              LOGICAL               BOOL
C
C        C
C        C     Local variables.
C        C
C              INTEGER               TARG
C              INTEGER               OBS
C
C              CHARACTER*(12)        REF
C              CHARACTER*(12)        ABCORR
C
C              DOUBLE PRECISION      STATE ( 6 )
C              DOUBLE PRECISION      LT
C
C        C
C        C     Initialization. Retrieve the vector from the earth to
C        C     the moon in the IAU_EARTH frame, without aberration
C        C     correction.
C        C
C              TARG   = 301
C              REF    = 'IAU_EARTH'
C              ABCORR = 'NONE'
C              OBS    = 399
C
C        C
C        C     Evaluate the state of TARG from OBS at ET with
C        C     correction ABCORR.
C        C
C              CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )
C
C        C
C        C     Calculate the boolean value.
C        C
C              BOOL = (STATE(3) .GE. 0.D0) .AND. (STATE(6) .GT. 0.D0 )
C
C              RETURN
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Interval            1
C            Interval start: 2011 JAN 09 15:24:23.4165
C                         Z=   -1.1251040632487275E-007
C                        Vz=   0.39698408454587081
C            Interval end  : 2011 JAN 16 16:08:28.5642
C                         Z=    156247.48804193645
C                        Vz=    4.0992339730983041E-013
C
C         Interval            2
C            Interval start: 2011 FEB 05 23:17:57.3600
C                         Z=   -1.2467506849134224E-007
C                        Vz=   0.39678128284337311
C            Interval end  : 2011 FEB 13 01:38:28.4265
C                         Z=    157016.05500077485
C                        Vz=    1.7374578338558155E-013
C
C         Interval            3
C            Interval start: 2011 MAR 05 06:08:17.6689
C                         Z=   -7.7721836078126216E-008
C                        Vz=   0.39399025363429169
C            Interval end  : 2011 MAR 12 10:27:45.1896
C                         Z=    157503.77377718856
C                        Vz=   -2.9786351336824612E-013
C
C
C     2) Calculate the time intervals when the Z component of the
C        Earth to Moon position vector in the IAU_EARTH frame has
C        value between -1000 km and 1000 km (e.g. above and below
C        the equatorial plane).
C
C        Use the meta-kernel from the first example.
C
C
C        Example code begins here.
C
C
C              PROGRAM GFUDB_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER               WNCARD
C              DOUBLE PRECISION      SPD
C
C        C
C        C     User defined external routines
C        C
C              EXTERNAL              GFB
C              EXTERNAL              GFQ
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
C              PARAMETER           ( MAXWIN = 100 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(32)        UTC
C
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      RIGHT
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      ETS
C              DOUBLE PRECISION      ETE
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      POS (3)
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2      )
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C
C              INTEGER               I
C
C        C
C        C     Saved variables
C        C
C        C     The confinement and result windows CNFINE and RESULT are
C        C     saved because this practice helps to prevent stack
C        C     overflow.
C        C
C              SAVE                  CNFINE
C              SAVE                  RESULT
C
C        C
C        C     Load needed kernels.
C        C
C              CALL FURNSH ( 'gfudb_ex1.tm' )
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
C              CALL STR2ET ( 'Jan 1 2011', ETS )
C              CALL STR2ET ( 'Apr 1 2011', ETE )
C              CALL WNINSD ( ETS, ETE, CNFINE )
C
C        C
C        C     The duration of the event is approximately ninety
C        C     minutes. Use a step of one hour.
C        C
C              STEP = 60.D0*60.D0
C
C              CALL GFUDB ( GFQ, GFB, STEP, CNFINE, RESULT )
C
C              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                    WRITE (*, '(A)') 'Result window is empty.'
C
C              ELSE
C
C                 DO I = 1, WNCARD(RESULT)
C
C        C
C        C           Fetch and display each RESULT interval.
C        C
C                    CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C                    WRITE (*,*) 'Interval ', I
C
C                    CALL ET2UTC ( LEFT, 'C', 4, UTC )
C                    WRITE (*, *) '   Interval start: ', UTC
C
C                    CALL SPKEZP ( 301, LEFT, 'IAU_EARTH', 'NONE', 399,
C             .                   POS, LT )
C                    WRITE (*, *) '                Z= ', POS(3)
C
C                    CALL ET2UTC ( RIGHT, 'C', 4, UTC )
C                    WRITE (*, *) '   Interval end  : ', UTC
C
C                    CALL SPKEZP ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399,
C             .                   POS, LT )
C                    WRITE (*, *) '                Z= ', POS(3)
C                    WRITE (*, *) ' '
C
C                 END DO
C
C              END IF
C
C              END
C
C
C
C        C-Procedure GFQ
C        C
C        C     User defined scalar routine.
C        C
C
C              SUBROUTINE GFQ ( ET, VALUE )
C              IMPLICIT NONE
C
C        C- Abstract
C        C
C        C     Return the Z component of the POS vector.
C        C
C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      VALUE
C
C        C
C        C     Local variables.
C        C
C              INTEGER               TARG
C              INTEGER               OBS
C
C              CHARACTER*(12)        REF
C              CHARACTER*(12)        ABCORR
C
C              DOUBLE PRECISION      POS ( 3 )
C              DOUBLE PRECISION      LT
C
C        C
C        C     Initialization. Retrieve the vector from the earth to
C        C     the moon in the IAU_EARTH frame, without aberration
C        C     correction.
C        C
C              TARG   = 301
C              REF    = 'IAU_EARTH'
C              ABCORR = 'NONE'
C              OBS    = 399
C
C        C
C        C     Evaluate the position of TARG from OBS at ET with
C        C     correction ABCORR.
C        C
C              CALL SPKEZP ( TARG, ET, REF, ABCORR, OBS, POS, LT )
C
C              VALUE = POS(3)
C
C              RETURN
C              END
C
C
C
C        C-Procedure GFB
C        C
C        C     User defined boolean routine.
C        C
C
C              SUBROUTINE GFB ( UDFUNS, ET, BOOL )
C              IMPLICIT NONE
C
C        C- Abstract
C        C
C        C     User defined boolean function:
C        C
C        C        VALUE >= LIM1 with VALUE <= LIM2.
C        C
C
C              EXTERNAL              UDFUNS
C
C              DOUBLE PRECISION      ET
C              LOGICAL               BOOL
C              DOUBLE PRECISION      VALUE
C
C
C              DOUBLE PRECISION      LIM1
C              DOUBLE PRECISION      LIM2
C
C              LIM1 = -1000.D0
C              LIM2 =  1000.D0
C
C              CALL UDFUNS ( ET, VALUE )
C
C        C
C        C     Calculate the boolean value.
C        C
C              BOOL = (VALUE .GE. LIM1) .AND. (VALUE .LE. LIM2 )
C
C              RETURN
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Interval            1
C            Interval start: 2011 JAN 09 14:42:24.4855
C                         Z=   -999.99999984083206
C            Interval end  : 2011 JAN 09 16:06:22.5030
C                         Z=    999.99999987627757
C
C         Interval            2
C            Interval start: 2011 JAN 23 04:07:44.4563
C                         Z=    999.99999992179255
C            Interval end  : 2011 JAN 23 05:23:06.2446
C                         Z=   -1000.0000001340870
C
C         Interval            3
C            Interval start: 2011 FEB 05 22:35:57.1570
C                         Z=   -1000.0000000961383
C            Interval end  : 2011 FEB 05 23:59:57.7497
C                         Z=    999.99999984281567
C
C         Interval            4
C            Interval start: 2011 FEB 19 14:11:28.2944
C                         Z=    1000.0000000983686
C            Interval end  : 2011 FEB 19 15:26:01.7199
C                         Z=   -999.99999985420800
C
C         Interval            5
C            Interval start: 2011 MAR 05 05:25:59.5621
C                         Z=   -1000.0000000277355
C            Interval end  : 2011 MAR 05 06:50:35.8628
C                         Z=    1000.0000000934349
C
C         Interval            6
C            Interval start: 2011 MAR 19 01:30:19.1660
C                         Z=    999.99999982956138
C            Interval end  : 2011 MAR 19 02:45:21.1121
C                         Z=   -1000.0000000146936
C
C
C        Note that the default convergence tolerance for the GF system
C        has value 10^-6 seconds.
C
C$ Restrictions
C
C     1)  Any kernel files required by this routine must be loaded
C         (normally via the SPICELIB routine FURNSH) before this routine
C         is called.
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
C-    SPICELIB Version 1.0.1, 21-OCT-2021 (JDR) (NJB)
C
C        Edited the header to comply with NAIF standard.
C
C        Added "IMPLICIT NONE" to example code and declared "LT"
C        variable. Reduced the search interval to limit the length of
C        the solutions. Added SAVE statements for CNFINE and RESULT
C        variables in code examples.
C
C        Updated description of RESULT argument in $Brief_I/O,
C        $Detailed_Input and $Detailed_Output.
C
C        Added entry #3 in $Exceptions section.
C
C        Updated header to describe use of expanded confinement window.
C
C-    SPICELIB Version 1.0.0, 15-JUL-2014 (EDW) (NJB)
C
C-&


C$ Index_Entries
C
C     GF user defined boolean function search
C
C-&


C
C     SPICELIB functions.
C
      LOGICAL               ODD
      LOGICAL               RETURN
      INTEGER               SIZED


C
C     Local variables.
C
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP

      DOUBLE PRECISION      TOL
      LOGICAL               OK

      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )

      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'GFUDB' )

C
C     Check the result window size.
C
      IF ( (SIZED(RESULT) .LT. 2) .OR. ODD( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFUDB'                                     )
         RETURN

      END IF

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

      CALL ZZGFUDB ( UDFUNS, UDFUNB, TOL,     GFSTEP,  GFREFN,
     .               NORPT,  GFREPI,  GFREPU,  GFREPF,
     .               NOBAIL, GFBAIL,  CNFINE,  RESULT )

      CALL CHKOUT( 'GFUDB' )

      RETURN
      END

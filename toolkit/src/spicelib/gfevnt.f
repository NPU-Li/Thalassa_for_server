C$Procedure GFEVNT ( GF, Geometric event finder )

      SUBROUTINE GFEVNT ( UDSTEP,   UDREFN,   GQUANT,   QNPARS,
     .                    QPNAMS,   QCPARS,   QDPARS,   QIPARS,
     .                    QLPARS,   OP,       REFVAL,   TOL,
     .                    ADJUST,   CNFINE,   RPT,      UDREPI,
     .                    UDREPU,   UDREPF,   MW,       NW,
     .                    WORK,     BAIL,     UDBAIL,   RESULT   )

C$ Abstract
C
C     Determine time intervals when a specified geometric quantity
C     satisfies a specified mathematical condition.
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
C     TIME
C     NAIF_IDS
C     FRAMES
C
C$ Keywords
C
C     EPHEMERIS
C     EVENT
C     GEOMETRY
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzgf.inc'
      INCLUDE 'zzabcorr.inc'

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 10 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      CHARACTER*(*)         GQUANT
      INTEGER               QNPARS
      CHARACTER*(*)         QPNAMS ( * )
      CHARACTER*(*)         QCPARS ( * )
      DOUBLE PRECISION      QDPARS ( * )
      INTEGER               QIPARS ( * )
      LOGICAL               QLPARS ( * )
      CHARACTER*(*)         OP
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      LOGICAL               BAIL
      LOGICAL               UDBAIL
      EXTERNAL              UDBAIL
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MAXPAR     P   Maximum number of parameters required to define
C                    any quantity.
C     CNVTOL     P   Default convergence tolerance.
C     UDSTEP     I   Name of the routine that computes and returns a
C                    time step.
C     UDREFN     I   Name of the routine that computes a refined time.
C     GQUANT     I   Type of geometric quantity.
C     QNPARS     I   Number of quantity definition parameters.
C     QPNAMS     I   Names of quantity definition parameters.
C     QCPARS     I   Array of character quantity definition parameters.
C     QDPARS     I   Array of double precision quantity definition
C                    parameters.
C     QIPARS     I   Array of integer quantity definition parameters.
C     QLPARS     I   Array of logical quantity definition parameters.
C     OP         I   Operator that either looks for an extreme value
C                    (max, min, local, absolute) or compares the
C                    geometric quantity value and a number.
C     REFVAL     I   Reference value.
C     TOL        I   Convergence tolerance in seconds
C     ADJUST     I   Absolute extremum adjustment value.
C     CNFINE     I   SPICE window to which the search is restricted.
C     RPT        I   Progress reporter on (.TRUE.) or off (.FALSE.).
C     UDREPI     I   Function that initializes progress reporting.
C     UDREPU     I   Function that updates the progress report.
C     UDREPF     I   Function that finalizes progress reporting.
C     MW         I   Size of workspace windows.
C     NW         I   The number of workspace windows needed for the
C                    search.
C     WORK       O   Array containing workspace windows.
C     BAIL       I   Logical indicating program interrupt monitoring.
C     UDBAIL     I   Name of a routine that signals a program interrupt.
C     RESULT    I-O  SPICE window containing results.
C
C$ Detailed_Input
C
C     UDSTEP   is the name of the user specified routine that computes a
C              time step in an attempt to find a transition of the state
C              of the specified coordinate. In the context of this
C              routine's algorithm, a "state transition" occurs where
C              the geometric state changes from being in the desired
C              geometric condition event to not, or vice versa.
C
C              This routine relies on UDSTEP returning step sizes small
C              enough so that state transitions within the confinement
C              window are not overlooked. There must never be two roots
C              A and B separated by less than STEP, where STEP is the
C              minimum step size returned by UDSTEP for any value of ET
C              in the interval [A, B].
C
C              The calling sequence for UDSTEP is:
C
C                 CALL UDSTEP ( ET, STEP )
C
C              where:
C
C                 ET      is the input start time from which the
C                         algorithm is to search forward for a state
C                         transition. ET is expressed as seconds past
C                         J2000 TDB.
C
C                 STEP    is the output step size. STEP indicates how
C                         far to advance ET so that ET and ET+STEP may
C                         bracket a state transition and definitely do
C                         not bracket more than one state transition.
C                         Units are TDB seconds.
C
C              If a constant step size is desired, the SPICELIB routine
C
C                 GFSTEP
C
C              may be used as the step size function. This is the
C              default option. If GFSTEP is used, the step size must be
C              set by calling
C
C                 CALL GFSSTP ( STEP )
C
C              prior to calling this routine.
C
C     UDREFN   is the name of the user specified routine that computes a
C              refinement in the times that bracket a transition point.
C              In other words, once a pair of times have been detected
C              such that the system is in different states at each of
C              the two times, UDREFN selects an intermediate time which
C              should be closer to the transition state than one of the
C              two known times.
C
C              The calling sequence for UDREFN is:
C
C                 CALL UDREFN ( T1, T2, S1, S2, T )
C
C              where the inputs are:
C
C                 T1    is a time when the system is in state S1. T1 is
C                       expressed as seconds past J2000 TDB.
C
C                 T2    is a time when the system is in state S2. T2 is
C                       expressed as seconds past J2000 TDB. T2 is
C                       assumed to be larger than T1.
C
C                 S1    is the state of the system at time T1. S1 is a
C                       LOGICAL value.
C
C                 S2    is the state of the system at time T2. S2 is a
C                       LOGICAL value.
C
C              UDREFN may use or ignore the S1 and S2 values.
C
C              The output is:
C
C                 T     is next time to check for a state transition.
C                       T has value between T1 and T2. T is expressed
C                       as seconds past J2000 TDB.
C
C              If a simple bisection method is desired, the SPICELIB
C              routine
C
C                 GFREFN
C
C              may be used as the refinement function. This is the
C              default option.
C
C     GQUANT   is a string containing the name of a geometric quantity.
C              The times when this quantity satisfies a condition
C              specified by the arguments OP and ADJUST (described
C              below) are to be found.
C
C              Each quantity is specified by the quantity name given in
C              argument GQUANT, and by a set of parameters specified by
C              the arguments
C
C                 QNPARS
C                 QPNAMS
C                 QCPARS
C                 QDPARS
C                 QIPARS
C                 QLPARS
C
C              For each quantity listed here, we also show how to set up
C              these input arguments to define the quantity. See the
C              detailed discussion of these arguments below for further
C              information.
C
C              GQUANT may be any of the strings:
C
C                 'ANGULAR SEPARATION'
C                 'COORDINATE'
C                 'DISTANCE'
C                 'ILLUMINATION ANGLE'
C                 'PHASE ANGLE'
C                 'RANGE RATE'
C
C              GQUANT strings are case insensitive. Values, meanings,
C              and associated parameters are discussed below.
C
C              The aberration correction parameter indicates the
C              aberration corrections to be applied to the state of the
C              target body to account for one-way light time and stellar
C              aberration. If relevant, it applies to the rotation of
C              the target body as well.
C
C              Supported aberration correction options for observation
C              (case where radiation is received by observer at ET) are:
C
C                 'NONE'          No correction.
C                 'LT'            Light time only.
C                 'LT+S'          Light time and stellar aberration.
C                 'CN'            Converged Newtonian (CN) light time.
C                 'CN+S'          CN light time and stellar aberration.
C
C              Supported aberration correction options for transmission
C              (case where radiation is emitted from observer at ET)
C              are:
C
C                 'XLT'           Light time only.
C                 'XLT+S'         Light time and stellar aberration.
C                 'XCN'           Converged Newtonian (CN) light time.
C                 'XCN+S'         CN light time and stellar aberration.
C
C              For detailed information, see the geometry finder
C              required reading, gf.req.
C
C              Case, leading and trailing blanks are not significant in
C              aberration correction parameter strings.
C
C
C              ANGULAR SEPARATION
C
C                 is the apparent angular separation of two target
C                 bodies as seen from an observing body.
C
C                 Quantity Parameters:
C
C                    QNPARS    = 8
C                    QPNAMS(1) = 'TARGET1'
C                    QPNAMS(2) = 'FRAME1'
C                    QPNAMS(3) = 'SHAPE1'
C                    QPNAMS(4) = 'TARGET2'
C                    QPNAMS(5) = 'FRAME2'
C                    QPNAMS(6) = 'SHAPE2'
C                    QPNAMS(7) = 'OBSERVER'
C                    QPNAMS(8) = 'ABCORR'
C
C                    QCPARS(1) = <name of first target>
C                    QCPARS(2) = <name of body-fixed frame
C                                          of first target>
C                    QCPARS(3) = <shape of first target>
C                    QCPARS(4) = <name of second target>
C                    QCPARS(5) = <name of body-fixed frame
C                                         of second target>
C                    QCPARS(6) = <shape of second target>
C                    QCPARS(7) = <name of observer>
C                    QCPARS(8) = <aberration correction>
C
C                 The target shape model specifiers may be set to either
C                 of the values
C
C                    'POINT'
C                    'SPHERE'
C
C                 The shape models for the two bodies need not match.
C
C                 Spherical models have radii equal to the longest
C                 equatorial radius of the PCK-based tri-axial
C                 ellipsoids used to model the respective bodies. When
C                 both target bodies are modeled as spheres, the angular
C                 separation between the bodies is the angle between the
C                 closest points on the limbs of the spheres, as viewed
C                 from the vantage point of the observer. If the limbs
C                 overlap, the angular separation is negative.
C
C                 (In this case, the angular separation is the angle
C                 between the centers of the spheres minus the sum of
C                 the apparent angular radii of the spheres.)
C
C
C              COORDINATE
C
C                 is a coordinate of a specified vector in a specified
C                 reference frame and coordinate system. For example, a
C                 coordinate can be the Z component of the earth-sun
C                 vector in the J2000 reference frame, or the latitude
C                 of the nearest point on Mars to an orbiting
C                 spacecraft, expressed relative to the IAU_MARS
C                 reference frame.
C
C                 The method by which the vector is defined is indicated
C                 by the
C
C                    'VECTOR DEFINITION'
C
C                 parameter. Allowed values and meanings of this
C                 parameter are:
C
C                    'POSITION'
C
C                       The vector is defined by the position of a
C                       target relative to an observer.
C
C                    'SUB-OBSERVER POINT'
C
C                       The vector is the sub-observer point on a
C                       specified target body.
C
C                    'SURFACE INTERCEPT POINT'
C
C                       The vector is defined as the intercept point of
C                       a vector from the observer to the target body.
C
C                 Some vector definitions, such as the sub-observer
C                 point, may be specified by a variety of methods, so a
C                 parameter is provided to select the computation
C                 method. The computation method parameter name is
C
C                    'METHOD'
C
C                 If the vector definition is
C
C                    'POSITION'
C
C                 the 'METHOD' parameter must be set to blank:
C
C                    ' '
C
C                 If the vector definition is
C
C                    'SUB-OBSERVER POINT'
C
C                 the 'METHOD' parameter must be set to either:
C
C                    'Near point: ellipsoid'
C                    'Intercept: ellipsoid'
C
C                 If the vector definition is
C
C                    'SURFACE INTERCEPT POINT'
C
C                 the 'METHOD' parameter must be set to:
C
C                    'Ellipsoid'
C
C                       The intercept computation uses a triaxial
C                       ellipsoid to model the surface of the target
C                       body. The ellipsoid's radii must be available in
C                       the kernel pool.
C
C                 The supported coordinate systems and coordinate names:
C
C                    Coordinate System  Coordinates        Range
C                    -----------------  -----------------  ------------
C
C                    'RECTANGULAR'      'X'
C                                       'Y'
C                                       'Z'
C
C                    'LATITUDINAL'      'RADIUS'
C                                       'LONGITUDE'        (-Pi,Pi]
C                                       'LATITUDE'         [-Pi/2,Pi/2]
C
C                    'RA/DEC'           'RANGE'
C                                       'RIGHT ASCENSION'  [0,2Pi)
C                                       'DECLINATION'      [-Pi/2,Pi/2]
C
C                    'SPHERICAL'        'RADIUS'
C                                       'COLATITUDE'       [0,Pi]
C                                       'LONGITUDE'        (-Pi,Pi]
C
C                    'CYLINDRICAL'      'RADIUS'
C                                       'LONGITUDE'        [0,2Pi)
C                                       'Z'
C
C                    'GEODETIC'         'LONGITUDE'        (-Pi,Pi]
C                                       'LATITUDE'         [-Pi/2,Pi/2]
C                                       'ALTITUDE'
C
C                    'PLANETOGRAPHIC'   'LONGITUDE'        [0,2Pi)
C                                       'LATITUDE'         [-Pi/2,Pi/2]
C                                       'ALTITUDE'
C
C                 When geodetic coordinates are selected, the radii used
C                 are those of the central body associated with the
C                 reference frame. For example, if IAU_MARS is the
C                 reference frame, then geodetic coordinates are
C                 calculated using the radii of Mars taken from a SPICE
C                 planetary constants kernel. One cannot ask for
C                 geodetic coordinates for a frame which doesn't have an
C                 extended body as its center.
C
C                 Reference frame names must be recognized by the SPICE
C                 frame subsystem.
C
C                 Quantity Parameters:
C
C                    QNPARS     = 10
C                    QPNAMS(1)  = 'TARGET'
C                    QPNAMS(2)  = 'OBSERVER'
C                    QPNAMS(3)  = 'ABCORR'
C                    QPNAMS(4)  = 'COORDINATE SYSTEM'
C                    QPNAMS(5)  = 'COORDINATE'
C                    QPNAMS(6)  = 'REFERENCE FRAME'
C                    QPNAMS(7)  = 'VECTOR DEFINITION'
C                    QPNAMS(8)  = 'METHOD'
C                    QPNAMS(9)  = 'DREF'
C                    QPNAMS(10) = 'DVEC'
C
C                 Only 'SURFACE INTERCEPT POINT' searches make use of
C                 the 'DREF' and 'DVEC' parameters.
C
C                    QCPARS(1) = <name of target>
C                    QCPARS(2) = <name of observer>
C                    QCPARS(3) = <aberration correction>
C                    QCPARS(4) = <coordinate system name>
C                    QCPARS(5) = <coordinate name>
C                    QCPARS(6) = <target reference frame name>
C                    QCPARS(7) = <vector definition>
C                    QCPARS(8) = <computation method>
C                    QCPARS(9) = <reference frame of DVEC pointing
C                                        vector, defined in QDPARS>
C
C                    QDPARS(1) = <DVEC pointing vector x component
C                                                      from observer>
C                    QDPARS(2) = <DVEC pointing vector y component
C                                                      from observer>
C                    QDPARS(3) = <DVEC pointing vector z component
C                                                      from observer>
C
C              DISTANCE
C
C                 is the apparent distance between a target body and an
C                 observing body. Distances are always measured between
C                 centers of mass.
C
C                 Quantity Parameters:
C
C                    QNPARS    = 3
C                    QPNAMS(1) = 'TARGET'
C                    QPNAMS(2) = 'OBSERVER'
C                    QPNAMS(3) = 'ABCORR'
C
C                    QCPARS(1) = <name of target>
C                    QCPARS(2) = <name of observer>
C                    QCPARS(3) = <aberration correction>
C
C
C              ILLUMINATION ANGLE
C
C                 is any of the illumination angles
C
C                    emission
C                    phase
C                    solar incidence
C
C                 defined at a surface point on a target body. These
C                 angles are defined as in the SPICELIB routine ILUMIN.
C
C                 Quantity Parameters:
C
C                    QNPARS    = 8
C                    QPNAMS(1) = 'TARGET'
C                    QPNAMS(2) = 'ILLUM'
C                    QPNAMS(3) = 'OBSERVER'
C                    QPNAMS(4) = 'ABCORR'
C                    QPNAMS(5) = 'FRAME'
C                    QPNAMS(6) = 'ANGTYP'
C                    QPNAMS(7) = 'METHOD'
C                    QPNAMS(8) = 'SPOINT'
C
C                    QCPARS(1) =  <name of target>
C                    QCPARS(1) =  <name of illumination source>
C                    QCPARS(3) =  <name of observer>
C                    QCPARS(4) =  <aberration correction>
C                    QCPARS(5) =  <target body-fixed frame>
C                    QCPARS(6) =  <type of illumination angle>
C                    QCPARS(7) =  <computation method>
C
C                 The surface point is specified using rectangular
C                 coordinates in the specified body-fixed frame.
C
C                    QDPARS(1) =  <X coordinate of surface point>
C                    QDPARS(2) =  <Y coordinate of surface point>
C                    QDPARS(3) =  <Z coordinate of surface point>
C
C
C              PHASE ANGLE
C
C                 is the apparent phase angle between a target body
C                 center and an illuminating body center as seen from an
C                 observer.
C
C                 Quantity Parameters:
C
C                    QNPARS    = 4
C                    QPNAMS(1) = 'TARGET'
C                    QPNAMS(2) = 'OBSERVER'
C                    QPNAMS(3) = 'ABCORR'
C                    QPNAMS(4) = 'ILLUM'
C
C                    QCPARS(1) =  <name of target>
C                    QCPARS(2) =  <name of observer>
C                    QCPARS(3) =  <aberration correction>
C                    QCPARS(4) =  <name of illuminating body>
C
C
C              RANGE RATE
C
C                 is the apparent range rate between a target body and
C                 an observing body.
C
C                 Quantity Parameters:
C
C                    QNPARS    = 3
C                    QPNAMS(1) = 'TARGET'
C                    QPNAMS(2) = 'OBSERVER'
C                    QPNAMS(3) = 'ABCORR'
C
C                    QCPARS(1) = <name of target>
C                    QCPARS(2) = <name of observer>
C                    QCPARS(3) = <aberration correction>
C
C     QNPARS   is the count of quantity parameter definition parameters.
C              These parameters supply the quantity-specific information
C              needed to fully define the quantity used in the search
C              performed by this routine.
C
C     QPNAMS   is an array of names of quantity definition parameters.
C              The names occupy elements 1:QNPARS of this array. The
C              value associated with the Ith element of QPNAMS is
C              located in element I of the parameter value argument
C              having data type appropriate for the parameter:
C
C                 Data Type                      Argument
C                 ---------                      --------
C                 Character strings              QCPARS
C                 Double precision numbers       QDPARS
C                 Integers                       QIPARS
C                 Logicals                       QLPARS
C
C              The order in which the parameter names are listed is
C              unimportant, as long as the corresponding parameter
C              values are listed in the same order.
C
C              The names in QPNAMS are case-insensitive.
C
C              See the description of the input argument GQUANT for a
C              discussion of the parameter names and values associated
C              with a given quantity.
C
C     QCPARS,
C     QDPARS,
C     QIPARS,
C     QLPARS   are, respectively, parameter value arrays of types
C
C                  CHARACTER*(*)       QCPARS
C                  DOUBLE PRECISION    QDPARS
C                  INTEGER             QIPARS
C                  LOGICAL             QLPARS
C
C              The value associated with the Ith name in the array
C              QPNAMS resides in the Ith element of whichever of these
C              arrays has the appropriate data type.
C
C              All of these arrays should be declared with dimension at
C              least QNPARS.
C
C              The names in the array QCPARS are case-insensitive.
C
C              Note that there is no required order for QPNAMS/Q*PARS
C              pairs.
C
C              See the description of the input argument GQUANT for a
C              discussion of the parameter names and values associated
C              with a given quantity.
C
C     OP       is a scalar string comparison operator indicating the
C              numeric constraint of interest. Values are:
C
C                 '>'        value of geometric quantity greater than
C                            some reference (REFVAL).
C
C                 '='        value of geometric quantity equal to some
C                            reference (REFVAL).
C
C                 '<'        value of geometric quantity less than some
C                            reference (REFVAL).
C
C                 'ABSMAX'   The geometric quantity is at an absolute
C                            maximum.
C
C                 'ABSMIN'   The geometric quantity is at an absolute
C                            minimum.
C
C                 'LOCMAX'   The geometric quantity is at a local
C                            maximum.
C
C                 'LOCMIN'   The geometric quantity is at a local
C                            minimum.
C
C              The caller may indicate that the region of interest is
C              the set of time intervals where the quantity is within a
C              specified distance of an absolute extremum. The argument
C              ADJUST (described below) is used to specified this
C              distance.
C
C              Local extrema are considered to exist only in the
C              interiors of the intervals comprising the confinement
C              window: a local extremum cannot exist at a boundary point
C              of the confinement window.
C
C              Case is not significant in the string OP.
C
C     REFVAL   is the reference value used to define an equality or
C              inequality to be satisfied by the geometric quantity. The
C              units of REFVAL are radians, radians/sec, km, or km/sec
C              as appropriate.
C
C     TOL      is a tolerance value used to determine convergence of
C              root-finding operations. TOL is measured in ephemeris
C              seconds and must be greater than zero.
C
C     ADJUST   is the amount by which the quantity is allowed to vary
C              from an absolute extremum.
C
C              If the search is for an absolute minimum is performed,
C              the resulting window contains time intervals when the
C              geometric quantity GQUANT has values between ABSMIN and
C              ABSMIN + ADJUST.
C
C              If the search is for an absolute maximum, the
C              corresponding range is  between ABSMAX - ADJUST and
C              ABSMAX.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions and must have value
C              zero for such searches. ADJUST must not be negative.
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
C              In some cases the observer's state may be computed at
C              times outside of CNFINE by as much as 2 seconds. See
C              $Particulars for details.
C
C     RPT      is a logical variable which controls whether the progress
C              reporter is enabled. When RPT is .TRUE., progress
C              reporting is enabled and the routines UDREPI, UDREPU, and
C              UDREPF (see descriptions below) are used to report
C              progress.
C
C     UDREPI   is the name of the user specified routine that
C              initializes a progress report. When progress reporting is
C              enabled, UDREPI is called at the start of a search. The
C              calling sequence of UDREPI is
C
C                 UDREPI ( CNFINE, SRCPRE, SRCSUF )
C
C                 DOUBLE PRECISION    CNFINE ( LBCELL : * )
C                 CHARACTER*(*)       SRCPRE
C                 CHARACTER*(*)       SRCSUF
C
C              where
C
C                 CNFINE
C
C              is a confinement window specifying the time period over
C              which a search is conducted, and
C
C                 SRCPRE
C                 SRCSUF
C
C              are prefix and suffix strings used in the progress
C              report: these strings are intended to bracket a
C              representation of the fraction of work done. For example,
C              when the SPICELIB progress reporting functions are used,
C              if SRCPRE and SRCSUF are, respectively,
C
C                 'Occultation/transit search'
C                 'done.'
C
C              the progress report display at the end of the search will
C              be:
C
C                 Occultation/transit search 100.00% done.
C
C              If the user doesn't wish to provide a custom set of
C              progress reporting functions, the SPICELIB routine
C
C                 GFREPI
C
C              may be used.
C
C     UDREPU   is the name of the user specified routine that updates
C              the progress report for a search. The calling sequence of
C              UDREPU is
C
C                 UDREPU (IVBEG, IVEND, ET )
C
C                 DOUBLE PRECISION      ET
C                 DOUBLE PRECISION      IVBEG
C                 DOUBLE PRECISION      IVEND
C
C              where ET is an epoch belonging to the confinement window,
C              IVBEG and IVEND are the start and stop times,
C              respectively of the current confinement window interval.
C              The ratio of the measure of the portion of CNFINE that
C              precedes ET to the measure of CNFINE would be a logical
C              candidate for the searches completion percentage; however
C              the method of measurement is up to the user.
C
C              If the user doesn't wish to provide a custom set of
C              progress reporting functions, the SPICELIB routine
C
C                 GFREPU
C
C              may be used.
C
C     UDREPF   is the name of the user specified routine that finalizes
C              a progress report. UDREPF has no arguments.
C
C              If the user doesn't wish to provide a custom set of
C              progress reporting functions, the SPICELIB routine
C
C                 GFREPF
C
C              may be used.
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
C                 NINTVLS  =  2*N  +  ( M / STEP )
C
C              where
C
C                 N     is the number of intervals in the confinement
C                       window
C
C                 M     is the measure of the confinement window, in
C                       units of seconds
C
C                 STEP  is the search step size in seconds
C
C              MW should then be set to
C
C                 2 * NINTVLS
C
C     NW       is a parameter specifying the number of SPICE windows
C              in the workspace array WORK (see description below)
C              used by this routine. (The reason this dimension is
C              an input argument is that this allows run-time
C              error checking to be performed.)
C
C     BAIL     is a logical flag indicating whether or not interrupt
C              signaling handling is enabled. When BAIL is set to
C              .TRUE., the input function UDBAIL (see description below)
C              is used to determine whether an interrupt has been
C              issued.
C
C     UDBAIL   is the name of the user specified routine that indicates
C              whether an interrupt signal has been issued (for example,
C              from the keyboard). UDBAIL has no arguments and returns
C              a LOGICAL value. The return value is .TRUE. if an
C              interrupt has been issued; otherwise the value is .FALSE.
C
C              GFEVNT uses UDBAIL only when BAIL (see above) is set
C              to .TRUE., indicating that interrupt handling is
C              enabled. When interrupt handling is enabled, GFEVNT
C              and routines in its call tree will call UDBAIL to
C              determine whether to terminate processing and return
C              immediately.
C
C              If interrupt handing is not enabled, a logical
C              function must still be passed as an input argument.
C              The SPICELIB function
C
C                 GFBAIL
C
C              may be used for this purpose.
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
C              discarded before GFEVNT conducts its search.
C
C$ Detailed_Output
C
C     WORK     is an array used to store workspace windows.
C
C              This array should be declared by the caller as shown:
C
C                 DOUBLE PRECISION     WORK ( LBCELL : MW,  NW )
C
C              WORK need not be initialized by the caller.
C
C              WORK is modified by this routine. The caller should
C              re-initialize this array before attempting to use it for
C              any other purpose.
C
C     RESULT   is a SPICE window representing the set of time intervals,
C              within the confinement period, when the specified
C              geometric event occurs.
C
C              The endpoints of the time intervals comprising RESULT
C              are interpreted as seconds past J2000 TDB.
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
C     LBCELL   is the SPICE cell lower bound.
C
C     MAXPAR   is the maximum number of parameters required to define
C              any quantity. MAXPAR may grow if new quantities require
C              more parameters. MAXPAR is currently set to 10.
C
C     CNVTOL   is the default convergence tolerance used by the
C              high-level GF search API routines. This tolerance is
C              used to terminate searches for binary state transitions:
C              when the time at which a transition occurs is bracketed
C              by two times that differ by no more than
C              SPICE_GF_CNVTOL, the transition time is considered
C              to have been found.
C
C$ Exceptions
C
C     1)  There are varying requirements on how distinct the three
C         objects, QCPARS, must be. If the requirements are not met, an,
C         an error is signaled by a routine in the call tree of this
C         routine.
C
C         When GQUANT has value 'ANGULAR SEPARATION' then all three
C         must be distinct.
C
C         When GQUANT has value of either
C
C            'DISTANCE'
C            'COORDINATE'
C            'RANGE RATE'
C
C         the QCPARS(1) and QCPARS(2) objects must be distinct.
C
C     2)  If any of the bodies involved do not have NAIF ID codes, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     3)  If the value of GQUANT is not recognized as a valid value,
C         the error SPICE(NOTRECOGNIZED) is signaled.
C
C     4)  If the number of quantity definition parameters, QNPARS is
C         greater than the maximum allowed value, MAXPAR, the error
C         SPICE(INVALIDCOUNT) is signaled.
C
C     5)  If the proper required parameters are not supplied in QNPARS,
C         the error SPICE(MISSINGVALUE) is signaled.
C
C     6)  If the comparison operator, OP, is not recognized, the error
C         SPICE(NOTRECOGNIZED) is signaled.
C
C     7)  If the window size MW is less than 2, an error is signaled by
C         a routine in the call tree of this routine.
C
C     8)  If the number of workspace windows NW is too small for the
C         required search, an error is signaled by a routine in the call
C         tree of this routine.
C
C     9)  If TOL is not greater than zero, an error is signaled by a
C         routine in the call tree of this routine.
C
C     10) If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     11) If ADJUST has a non-zero value when OP has any value other
C         than 'ABSMIN' or 'ABSMAX', an error is signaled by a routine
C         in the call tree of this routine.
C
C     12) The user must take care when searching for an extremum
C         ('ABSMAX', 'ABSMIN', 'LOCMAX', 'LOCMIN') of an angular
C         quantity. Problems are most common when using the 'COORDINATE'
C         value of GQUANT with 'LONGITUDE' or 'RIGHT ASCENSION' values
C         for the coordinate name. Since these quantities are cyclical,
C         rather than monotonically increasing or decreasing, an
C         extremum may be hard to interpret. In particular, if an
C         extremum is found near the cycle boundary (-Pi for
C         'LONGITUDE', 2*Pi for 'RIGHT ASCENSION') it may not be
C         numerically reasonable. For example, the search for times when
C         a longitude coordinate is at its absolute maximum may result
C         in a time when the longitude value is -Pi, due to roundoff
C         error.
C
C     13) If operation of this routine is interrupted, the output result
C         window will be invalid.
C
C$ Files
C
C     Appropriate SPK and PCK kernels must be loaded by the
C     calling program before this routine is called.
C
C     The following data are required:
C
C     -  SPK data: ephemeris data for target, source and observer that
C        describes the ephemeris of these objects for the period
C        defined by the confinement window, CNFINE must be
C        loaded. If aberration corrections are used, the states of
C        target and observer relative to the solar system barycenter
C        must be calculable from the available ephemeris data.
C        Typically ephemeris data are made available by loading one
C        or more SPK files via FURNSH.
C
C     -  PCK data: bodies are assumed to be spherical and must have a
C        radius loaded from the kernel pool. Typically this is done by
C        loading a text PCK file via FURNSH. If the bodies are
C        triaxial, the largest radius is chosen as that of the
C        equivalent spherical body.
C
C     -  In some cases the observer's state may be computed at times
C        outside of CNFINE by as much as 2 seconds; data required to
C        compute this state must be provided by loaded kernels. See
C        $Particulars for details.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine provides the SPICE GF subsystem's general interface
C     to determines time intervals when the value of some
C     geometric quantity related to one or more objects and an observer
C     satisfies a user specified constraint. It puts these times in a
C     result window called RESULT. It does this by first finding
C     windows when the quantity of interest is either monotonically
C     increasing or decreasing. These windows are then manipulated to
C     give the final result.
C
C     Applications that require do not require support for progress
C     reporting, interrupt handling, non-default step or refinement
C     functions, or non-default convergence tolerance normally should
C     call a high level geometry quantity routine rather than
C     this routine.
C
C     The Search Process
C     ==================
C
C     Regardless of the type of constraint selected by the caller, this
C     routine starts the search for solutions by determining the time
C     periods, within the confinement window, over which the specified
C     geometric quantity function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the quantity
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
C     change of quantity function will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the quantity
C     function is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the quantity function is monotone:
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
C     "convergence tolerance," passed to this routine as 'tol'.
C
C     The GF subsystem defines a parameter, CNVTOL (from gf.inc), as a
C     default tolerance. This represents a "tight" tolerance value
C     so that the tolerance doesn't become the limiting factor in the
C     accuracy of solutions found by this routine. In general the
C     accuracy of input data will be the limiting factor.
C
C     Making the tolerance tighter than CNVTOL is unlikely to
C     be useful, since the results are unlikely to be more accurate.
C     Making the tolerance looser will speed up searches somewhat,
C     since a few convergence steps will be omitted. However, in most
C     cases, the step size is likely to have a much greater affect
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
C     1) Conduct a DISTANCE search using the default GF progress
C        reporting capability.
C
C        The program will use console I/O to display a simple
C        ASCII-based progress report.
C
C        The program will find local maximums of the distance from
C        Earth to Moon with light time and stellar aberration
C        corrections to model the apparent positions of the Moon.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: gfevnt_ex1.tm
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
C              naif0009.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de414.bsp',
C                                  'pck00008.tpc',
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
C              PROGRAM GFEVNT_EX1
C              IMPLICIT              NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      SPD
C              INTEGER               WNCARD
C
C              INCLUDE               'gf.inc'
C
C        C
C        C     Local variables and initial parameters.
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C              INTEGER               MAXPAR
C              PARAMETER           ( MAXPAR = 8 )
C
C              INTEGER               MAXVAL
C              PARAMETER           ( MAXVAL = 20000 )
C
C
C              INTEGER               STRSIZ
C              PARAMETER           ( STRSIZ = 40 )
C
C              INTEGER               I
C
C        C
C        C     Confining window
C        C
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C
C        C
C        C     Confining window beginning and ending time strings.
C        C
C              CHARACTER*(STRSIZ)    BEGSTR
C              CHARACTER*(STRSIZ)    ENDSTR
C
C        C
C        C     Confining window beginning and ending times
C        C
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      ENDTIM
C
C        C
C        C     Result window beginning and ending times for intervals.
C        C
C              DOUBLE PRECISION      BEG
C              DOUBLE PRECISION      END
C
C        C
C        C     Geometric quantity results window, work window,
C        C     bail switch and progress reporter switch.
C        C
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL )
C              DOUBLE PRECISION      WORK   ( LBCELL : MAXVAL, NWDIST )
C
C              LOGICAL               BAIL
C              LOGICAL               GFBAIL
C              EXTERNAL              GFBAIL
C              LOGICAL               RPT
C
C        C
C        C     Step size.
C        C
C              DOUBLE PRECISION      STEP
C
C        C
C        C     Geometric quantity name.
C        C
C              CHARACTER*(LNSIZE)    EVENT
C
C        C
C        C     Relational string
C        C
C              CHARACTER*(STRSIZ)    RELATE
C
C        C
C        C     Quantity definition parameter arrays:
C        C
C              INTEGER               QNPARS
C              CHARACTER*(LNSIZE)    QPNAMS ( MAXPAR )
C              CHARACTER*(LNSIZE)    QCPARS ( MAXPAR )
C              DOUBLE PRECISION      QDPARS ( MAXPAR )
C              INTEGER               QIPARS ( MAXPAR )
C              LOGICAL               QLPARS ( MAXPAR )
C
C        C
C        C     Routines to set step size, refine transition times
C        C     and report work.
C        C
C              EXTERNAL              GFREFN
C              EXTERNAL              GFREPI
C              EXTERNAL              GFREPU
C              EXTERNAL              GFREPF
C              EXTERNAL              GFSTEP
C
C        C
C        C     Reference and adjustment values.
C        C
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      ADJUST
C
C              INTEGER               COUNT
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
C        C     Load leapsecond and spk kernels. The name of the
C        C     meta kernel file shown here is fictitious; you
C        C     must supply the name of a file available
C        C     on your own computer system.
C
C              CALL FURNSH ('gfevnt_ex1.tm')
C
C        C
C        C     Set a beginning and end time for confining window.
C        C
C              BEGSTR = '2001 jan 01 00:00:00.000'
C              ENDSTR = '2001 jun 30 00:00:00.000'
C
C              CALL STR2ET ( BEGSTR, BEGTIM )
C              CALL STR2ET ( ENDSTR, ENDTIM )
C
C        C
C        C     Set condition for extremum.
C        C
C              RELATE  =   'LOCMAX'
C
C        C
C        C     Set reference value (if needed) and absolute extremum
C        C     adjustment (if needed).
C        C
C              REFVAL   =    0.D0
C              ADJUST   =    0.D0
C
C        C
C        C     Set quantity.
C        C
C              EVENT    =   'DISTANCE'
C
C        C
C        C     Turn on progress reporter and initialize the windows.
C        C
C              RPT    = .TRUE.
C              BAIL   = .FALSE.
C
C              CALL SSIZED ( 2,      CNFINE )
C              CALL SSIZED ( MAXVAL, RESULT )
C
C        C
C        C     Add 2 points to the confinement interval window.
C        C
C              CALL WNINSD ( BEGTIM, ENDTIM, CNFINE )
C
C        C
C        C     Define input quantities.
C        C
C              QPNAMS(1) = 'TARGET'
C              QCPARS(1) = 'MOON'
C
C              QPNAMS(2) = 'OBSERVER'
C              QCPARS(2) = 'EARTH'
C
C              QPNAMS(3) = 'ABCORR'
C              QCPARS(3) = 'LT+S'
C
C              QNPARS    = 3
C
C        C
C        C     Set the step size to 1 day and convert to seconds.
C        C
C              STEP = 1.D-3*SPD()
C
C              CALL GFSSTP ( STEP )
C
C        C
C        C    Look for solutions.
C        C
C              CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
C             .              QNPARS,     QPNAMS,   QCPARS,
C             .              QDPARS,     QIPARS,   QLPARS,
C             .              RELATE,     REFVAL,   CNVTOL,
C             .              ADJUST,     CNFINE,   RPT,
C             .              GFREPI,     GFREPU,   GFREPF,
C             .              MAXVAL,     NWDIST,   WORK,
C             .              BAIL,       GFBAIL,   RESULT )
C
C
C        C
C        C     Check the number of intervals in the result window.
C        C
C              COUNT = WNCARD(RESULT)
C
C              WRITE (*,*) 'Found ', COUNT, ' intervals in RESULT'
C              WRITE (*,*) ' '
C
C        C
C        C     List the beginning and ending points in each interval.
C        C
C              DO I = 1, COUNT
C
C                CALL WNFETD ( RESULT, I, BEG, END  )
C
C                CALL TIMOUT ( BEG,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             .  //            '(TDB) ::TDB ::RND',  BEGSTR )
C                CALL TIMOUT ( END,
C             .                'YYYY-MON-DD HR:MN:SC.###### '
C             . //             '(TDB) ::TDB ::RND',  ENDSTR )
C
C                WRITE (*,*) 'Interval ',  I
C                WRITE (*,*) 'Beginning TDB ', BEGSTR
C                WRITE (*,*) 'Ending TDB    ', ENDSTR
C                WRITE (*,*) ' '
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
C        Distance pass 1 of 1 100.00% done.
C
C         Found            6  intervals in RESULT
C
C         Interval            1
C         Beginning TDB 2001-JAN-24 19:22:01.436672 (TDB)
C         Ending TDB    2001-JAN-24 19:22:01.436672 (TDB)
C
C         Interval            2
C         Beginning TDB 2001-FEB-20 21:52:07.914964 (TDB)
C         Ending TDB    2001-FEB-20 21:52:07.914964 (TDB)
C
C         Interval            3
C         Beginning TDB 2001-MAR-20 11:32:03.182345 (TDB)
C         Ending TDB    2001-MAR-20 11:32:03.182345 (TDB)
C
C         Interval            4
C         Beginning TDB 2001-APR-17 06:09:00.877038 (TDB)
C         Ending TDB    2001-APR-17 06:09:00.877038 (TDB)
C
C         Interval            5
C         Beginning TDB 2001-MAY-15 01:29:28.532819 (TDB)
C         Ending TDB    2001-MAY-15 01:29:28.532819 (TDB)
C
C         Interval            6
C         Beginning TDB 2001-JUN-11 19:44:10.855458 (TDB)
C         Ending TDB    2001-JUN-11 19:44:10.855458 (TDB)
C
C
C        Note that the progress report has the format shown below:
C
C           Distance pass 1 of 1   6.02% done.
C
C        The completion percentage was updated approximately once per
C        second.
C
C        When the program was interrupted at an arbitrary time,
C        the output was:
C
C           Distance pass 1 of 1  13.63% done.
C           Search was interrupted.
C
C        This message was written after an interrupt signal
C        was trapped. By default, the program would have terminated
C        before this message could be written.
C
C$ Restrictions
C
C     1)  The kernel files to be used by GFEVNT must be loaded (normally
C         via the SPICELIB routine FURNSH) before GFEVNT is called.
C
C     2)  If using the default, constant step size routine, GFSTEP, the
C         entry point GFSSTP must be called prior to calling this
C         routine. The call syntax for GFSSTP:
C
C            CALL GFSSTP ( STEP )
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 27-OCT-2021 (JDR) (BVS) (NJB)
C
C        Edited the header to comply with NAIF standard.
C
C        Updated description of WORK and RESULT arguments in $Brief_I/O,
C        $Detailed_Input and $Detailed_Output.
C
C        Added SAVE statements for CNFINE, WORK and RESULT variables in
C        code example.
C
C        Added SAVE statement for DREF.
C
C        Fixed typo in $Exceptions entry #5, which referred to a non
C        existing input argument, replaced entry #7 by new entries #7
C        and #8, replaced entry #10 by new entries #10 and #11, and
C        added entry #13.
C
C        Added descriptions of MAXPAR and CNVTOL to the $Brief_I/O and
C        $Parameters sections.
C
C        Moved declaration of MAXPAR into the $Declarations section.
C
C        Updated header to describe use of expanded confinement window.
C
C-    SPICELIB Version 2.0.0, 05-SEP-2012 (EDW) (NJB)
C
C        Edit to comments to correct search description.
C
C        Edit to $Index_Entries.
C
C        Added geometric quantities:
C
C           Phase Angle
C           Illumination Angle
C
C        Code edits to implement use of ZZGFRELX in event calculations:
C
C           Range rate
C           Separation angle
C           Distance
C           Coordinate
C
C        The code changes for ZZGFRELX use should not affect the
C        numerical results of GF computations.
C
C-    SPICELIB Version 1.1.0, 09-OCT-2009 (NJB) (EDW)
C
C        Edits to argument descriptions.
C
C        Added geometric quantities:
C
C           Range Rate
C
C-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB) (EDW)
C
C-&


C$ Index_Entries
C
C     GF mid-level geometric condition solver
C
C-&


C
C     SPICELIB functions
C
      INTEGER               ISRCHC
      LOGICAL               RETURN

      EXTERNAL              ZZGFUDLT


C
C     Angular separation routines.
C
      EXTERNAL              ZZGFSPDC
      EXTERNAL              ZZGFSPGQ

C
C     Distance routines.
C
      EXTERNAL              ZZGFDIDC
      EXTERNAL              ZZGFDIGQ

C
C     Range rate routines.
C
      EXTERNAL              ZZGFRRDC
      EXTERNAL              ZZGFRRGQ

C
C     Phase angle routines.
C
      EXTERNAL              ZZGFPADC
      EXTERNAL              ZZGFPAGQ

C
C     Illumination angle routines.
C
      EXTERNAL              ZZGFILDC
      EXTERNAL              ZZGFILGQ


C
C     Event quantity codes:
C
      INTEGER               SEP
      PARAMETER           ( SEP    = 1 )

      INTEGER               DIST
      PARAMETER           ( DIST   = 2 )

      INTEGER               COORD
      PARAMETER           ( COORD  = 3 )

      INTEGER               RNGRAT
      PARAMETER           ( RNGRAT = 4 )

      INTEGER               PHASE
      PARAMETER           ( PHASE  = 5 )

      INTEGER               ILUANG
      PARAMETER           ( ILUANG = 6 )

      INTEGER               ANGSPD
      PARAMETER           ( ANGSPD = 7 )

      INTEGER               APDIAM
      PARAMETER           ( APDIAM = 8 )


C
C     Number of supported quantities:
C
      INTEGER               NQ
      PARAMETER           ( NQ     = 8 )

C
C     Number of supported comparison operators:
C
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

C
C     Assorted string lengths:
C

C
C     MAXOP is the maximum string length for comparison operators.
C     MAXOP may grow if new comparisons are added.
C
      INTEGER               MAXOP
      PARAMETER           ( MAXOP = 6 )

C
C     MAXCLN is the maximum character string length of the quantity
C     parameter names and character quantity parameters.
C
      INTEGER               MAXCLN
      PARAMETER           ( MAXCLN = 80 )


C
C     Local variables
C
      CHARACTER*(MAXCLN)    ABCORR
      CHARACTER*(MAXCLN)    ANGTYP
      CHARACTER*(MAXCLN)    CNAMES ( NC )
      CHARACTER*(MAXCLN)    CORNAM
      CHARACTER*(MAXCLN)    CORSYS
      CHARACTER*(MAXCLN)    CPARS  ( MAXPAR )
      CHARACTER*(MAXCLN)    DREF
      CHARACTER*(MAXCLN)    FRAME  ( 2 )
      CHARACTER*(MAXCLN)    ILLUM
      CHARACTER*(MAXCLN)    METHOD
      CHARACTER*(MAXCLN)    OBSRVR
      CHARACTER*(MAXCLN)    OF     ( 2 )
      CHARACTER*(MAXCLN)    PNAMES ( MAXPAR )
      CHARACTER*(MAXCLN)    QNAMES ( NQ     )
      CHARACTER*(MAXCLN)    QPARS  ( MAXPAR, NQ )
      CHARACTER*(MAXCLN)    QUANT
      CHARACTER*(MAXCLN)    REF
      CHARACTER*(MAXCLN)    SHAPE  ( 2 )
      CHARACTER*(MAXCLN)    TARGET
      CHARACTER*(MAXCLN)    VECDEF
      CHARACTER*(MAXOP)     UOP
      CHARACTER*(MXBEGM)    RPTPRE ( 2 )
      CHARACTER*(MXBEGM)    SRCPRE ( 2, NQ )
      CHARACTER*(MXENDM)    SRCSUF ( 2, NQ )

      DOUBLE PRECISION      DT

      INTEGER               I
      INTEGER               LOC
      INTEGER               NPASS
      INTEGER               QTNUM

      LOGICAL               FIRST
      LOGICAL               LOCALX
      LOGICAL               NOADJX

      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  CNAMES
      SAVE                  DREF
      SAVE                  QNAMES
      SAVE                  QPARS
      SAVE                  SRCPRE
      SAVE                  SRCSUF


C
C     Initial values
C
      DATA                  DREF   / ' '    /
      DATA                  FIRST  / .TRUE. /

C
C     Below we initialize the list of quantity names. Restrict this list
C     to those events supported with test families.
C
      DATA                  QNAMES / 'ANGULAR SEPARATION',
     .                               'DISTANCE',
     .                               'COORDINATE',
     .                               'RANGE RATE',
     .                               'PHASE ANGLE',
     .                               'ILLUMINATION ANGLE',
     .                               ' ',
     .                               ' '                    /

C
C     Below we initialize the list of comparison operator names.
C
      DATA                  CNAMES / '>',
     .                               '=',
     .                               '<',
     .                               'ABSMAX',
     .                               'ABSMIN',
     .                               'LOCMAX',
     .                               'LOCMIN'   /


C
C     Below we initialize the list of quantity parameter names.
C     Each quantity has its own list of parameter names.
C
C     NOTE:  ALL of the initializers below must be updated when
C     the parameter MAXPAR is increased.  The number blank string
C     initial values must be increased so that the total number
C     of values for each array is MAXPAR.
C
      DATA                ( QPARS(I,SEP), I = 1, MAXPAR )     /
     .                               'TARGET1',
     .                               'FRAME1',
     .                               'SHAPE1',
     .                               'TARGET2',
     .                               'FRAME2',
     .                               'SHAPE2',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                                ' ',
     .                                ' '                     /

      DATA                ( QPARS(I,DIST), I = 1, MAXPAR )    /
     .                               'TARGET',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                                7 * ' '                 /

      DATA                ( QPARS(I,COORD), I = 1, MAXPAR )   /
     .                               'TARGET',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                               'COORDINATE SYSTEM',
     .                               'COORDINATE',
     .                               'REFERENCE FRAME',
     .                               'VECTOR DEFINITION',
     .                               'METHOD',
     .                               'DVEC',
     .                               'DREF'                 /

      DATA                ( QPARS(I,RNGRAT), I = 1, MAXPAR )  /
     .                               'TARGET',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                                7 * ' '                 /

      DATA                ( QPARS(I,ANGSPD), I = 1, MAXPAR )  /
     .                               'TARGET1',
     .                               'TARGET2',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                               'REFERENCE FRAME',
     .                                5 * ' '                 /

      DATA                ( QPARS(I,PHASE), I = 1, MAXPAR )   /
     .                               'TARGET',
     .                               'OBSERVER',
     .                               'ILLUM',
     .                               'ABCORR',
     .                                6 * ' '                 /

      DATA                ( QPARS(I,APDIAM), I = 1, MAXPAR )  /
     .                               'TARGET',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                               'REFERENCE FRAME',
     .                                6 * ' '                 /

      DATA                ( QPARS(I,ILUANG), I = 1, MAXPAR )  /
     .                               'TARGET',
     .                               'ILLUM',
     .                               'OBSERVER',
     .                               'ABCORR',
     .                               'REFERENCE FRAME',
     .                               'ANGTYP',
     .                               'METHOD',
     .                               'SPOINT',
     .                                2 * ' '                 /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'GFEVNT' )

      IF ( FIRST ) THEN

C
C        Set the progress report prefix and suffix strings for
C        each quantity. No need to set coordinate quantity strings.
C        The coordinate solver performs that function.
C
         FIRST = .FALSE.

         SRCPRE ( 1, SEP    ) = 'Angular separation pass 1 of #'
         SRCPRE ( 2, SEP    ) = 'Angular separation pass 2 of #'

         SRCPRE ( 1, DIST   ) = 'Distance pass 1 of # '
         SRCPRE ( 2, DIST   ) = 'Distance pass 2 of # '

         SRCPRE ( 1, ANGSPD ) = 'Angular Rate pass 1 of #'
         SRCPRE ( 2, ANGSPD ) = 'Angular Rate pass 2 of #'

         SRCPRE ( 1, RNGRAT ) = 'Range Rate pass 1 of #'
         SRCPRE ( 2, RNGRAT ) = 'Range Rate pass 2 of #'

         SRCPRE ( 1, PHASE  ) = 'Phase angle search pass 1 of #'
         SRCPRE ( 2, PHASE  ) = 'Phase angle search pass 2 of #'

         SRCPRE ( 1, APDIAM ) = 'Diameter pass 1 of #'
         SRCPRE ( 2, APDIAM ) = 'Diameter pass 2 of #'

         SRCPRE ( 1, ILUANG ) = 'Illumination angle pass 1 of #'
         SRCPRE ( 2, ILUANG ) = 'Illumination angle pass 2 of #'

         SRCSUF ( 1, SEP    ) = 'done.'
         SRCSUF ( 2, SEP    ) = 'done.'

         SRCSUF ( 1, DIST   ) = 'done.'
         SRCSUF ( 2, DIST   ) = 'done.'

         SRCSUF ( 1, ANGSPD ) = 'done.'
         SRCSUF ( 2, ANGSPD ) = 'done.'

         SRCSUF ( 1, RNGRAT ) = 'done.'
         SRCSUF ( 2, RNGRAT ) = 'done.'

         SRCSUF ( 1, PHASE  ) = 'done.'
         SRCSUF ( 2, PHASE  ) = 'done.'

         SRCSUF ( 1, APDIAM ) = 'done.'
         SRCSUF ( 2, APDIAM ) = 'done.'

         SRCSUF ( 1, ILUANG ) = 'done.'
         SRCSUF ( 2, ILUANG ) = 'done.'

      END IF

C
C     Make sure the requested quantity is one we recognize.
C

      CALL LJUST ( GQUANT, QUANT )
      CALL UCASE ( QUANT,  QUANT )

      QTNUM = ISRCHC ( QUANT, NQ, QNAMES )


      IF ( QTNUM .EQ. 0 ) THEN

        CALL SETMSG ('The geometric quantity, # is not '             //
     .               'recognized. Supported quantities are: '        //
     .               'DISTANCE, '                                    //
     .               'PHASE ANGLE, '                                 //
     .               'COORDINATE, '                                  //
     .               'RANGE RATE, '                                  //
     .               'ANGULAR SEPARATION,'                           //
     .               'ILLUMINATION ANGLE.'                            )
         CALL ERRCH  ('#', GQUANT                                     )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ('GFEVNT'                                        )
         RETURN

      END IF

C
C     Check number of quantity definition parameters.
C
      IF (  ( QNPARS .LT. 0 ) .OR. ( QNPARS .GT. MAXPAR )  ) THEN

         CALL SETMSG ( 'Number of quantity definition parameters = ' //
     .                 '#;  must be in range 0:#.'                    )
         CALL ERRINT ( '#', QNPARS                                    )
         CALL ERRINT ( '#', MAXPAR                                    )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                          )
         CALL CHKOUT ( 'GFEVNT'                                       )
         RETURN

      END IF

C
C     Make left-justified, upper case copies of parameter names.
C
      DO I = 1, QNPARS

         CALL LJUST ( QPNAMS(I), PNAMES(I) )
         CALL UCASE ( PNAMES(I), PNAMES(I) )

         CALL LJUST ( QCPARS(I), CPARS(I) )
         CALL UCASE ( CPARS(I),  CPARS(I) )

      END DO

C
C     Make sure all parameters have been supplied for the requested
C     quantity.
C
      DO I = 1, MAXPAR

         IF ( QPARS(I,QTNUM) .NE. ' ' ) THEN
C
C           The Ith parameter must be supplied by the caller.
C
            LOC = ISRCHC ( QPARS(I,QTNUM), QNPARS, PNAMES )


            IF ( LOC .EQ. 0 ) THEN

               CALL SETMSG ( 'The parameter # is required in order ' //
     .                       'to compute events pertaining to the '  //
     .                       'quantity #; this parameter was not '   //
     .                       'supplied.'                             )
               CALL ERRCH  ( '#', QPARS (I,QTNUM)                    )
               CALL ERRCH  ( '#', QNAMES(  QTNUM)                    )
               CALL SIGERR ( 'SPICE(MISSINGVALUE)'                   )
               CALL CHKOUT ( 'GFEVNT'                                )
               RETURN

            END IF

         END IF

      END DO


C
C     Capture as local variables those parameters passed from the
C     callers.
C
C     If the PNAMES array contains any of the parameters
C
C        TARGET
C        OBSERVER
C        ILLUM
C        TARGET1
C        FRAME1
C        SHAPE1
C        TARGET2
C        FRAME2
C        SHAPE2
C        ABCORR
C        REFERENCE FRAME
C        DREF
C        DVEC
C
C     copy the value corresponding to the parameter to a local variable.
C
C     These operations demonstrate the need for associative arrays
C     as part of Fortran.
C

C
C     -TARGET-
C
      LOC = ISRCHC ( 'TARGET', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         TARGET = CPARS(LOC)

      END IF


C
C     -OBSERVER-
C
      LOC = ISRCHC ( 'OBSERVER', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         OBSRVR = CPARS(LOC)

      END IF


C
C     -ILLUM-
C
      LOC = ISRCHC ( 'ILLUM', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         ILLUM = CPARS(LOC)

      END IF


C
C     -TARGET1-
C
      LOC = ISRCHC ( 'TARGET1', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         OF(1) = CPARS(LOC)

      END IF


C
C     -TARGET2-
C
      LOC = ISRCHC ( 'TARGET2', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         OF(2) = CPARS(LOC)

      END IF


C
C     -FRAME1-
C
      LOC = ISRCHC ( 'FRAME1', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         FRAME(1) = CPARS(LOC)

      END IF

C
C     -FRAME2-
C
      LOC = ISRCHC ( 'FRAME2', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         FRAME(2) = CPARS(LOC)

      END IF


C
C     -SHAPE1-
C
      LOC = ISRCHC ( 'SHAPE1', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         SHAPE(1) = CPARS(LOC)

      END IF


C
C     -SHAPE2-
C
      LOC = ISRCHC ( 'SHAPE2', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         SHAPE(2) = CPARS(LOC)

      END IF


C
C     -ABCORR-
C
      LOC = ISRCHC ( 'ABCORR', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         ABCORR = CPARS(LOC)

      END IF


C
C     -REFERENCE FRAME-
C
      LOC = ISRCHC ( 'REFERENCE FRAME', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         REF = CPARS(LOC)

      END IF


C
C     -COORDINATE SYSTEM-
C
      LOC = ISRCHC ( 'COORDINATE SYSTEM',  QNPARS, QPNAMS )

      IF ( LOC .GT. 0 ) THEN

         CORSYS = QCPARS(LOC)

      END IF


C
C     -COORDINATE-
C
      LOC = ISRCHC ( 'COORDINATE', QNPARS, QPNAMS )

      IF ( LOC .GT. 0 ) THEN

         CORNAM = QCPARS(LOC)

      END IF


C
C     -VECTOR DEFINITION-
C
      LOC = ISRCHC ( 'VECTOR DEFINITION',  QNPARS, QPNAMS )

      IF ( LOC .GT. 0 ) THEN

         VECDEF = QCPARS(LOC)

      END IF

C
C     -DVEC-
C
      LOC = ISRCHC ( 'DVEC', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         CALL VEQU( QDPARS(1), DVEC(1)  )

      END IF


C
C     -METHOD-
C
      LOC = ISRCHC ( 'METHOD',  QNPARS, QPNAMS )

      IF ( LOC .GT. 0 ) THEN

         METHOD = QCPARS(LOC)

      END IF


C
C     -DREF-
C
      LOC = ISRCHC ( 'DREF', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         DREF = CPARS(LOC)

      END IF

C
C     -ANGTYP-
C
      LOC = ISRCHC ( 'ANGTYP', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         ANGTYP = CPARS(LOC)

      END IF

C
C     -SPOINT-
C
      LOC = ISRCHC ( 'SPOINT', QNPARS, PNAMES )

      IF ( LOC .GT. 0 ) THEN

         CALL VEQU( QDPARS(1), SPOINT )

      END IF


C
C     Make sure that the requested comparison operation is one we
C     recognize.
C

      CALL LJUST ( OP,   UOP )
      CALL UCASE ( UOP,  UOP )

      LOC = ISRCHC ( UOP, NC, CNAMES )

      IF ( LOC .EQ. 0 ) THEN

        CALL SETMSG ('The comparison operator, # is not '            //
     .               'recognized.  Supported operators are: '        //
     .               '>, '                                           //
     .               '=, '                                           //
     .               '<, '                                           //
     .               'ABSMAX, '                                      //
     .               'ABSMIN, '                                      //
     .               'LOCMAX, '                                      //
     .               'LOCMIN. '                                       )
         CALL ERRCH  ('#', OP                                         )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ('GFEVNT'                                        )
         RETURN

      END IF


C
C     If progress reporting is enabled, set the report prefix array
C     according to the quantity and the relational operator.
C
      IF ( RPT ) THEN
C
C        We'll use the logical flag LOCALX to indicate a local extremum
C        operator and the flag NOADJX to indicate an absolute extremum
C        operator with zero adjustment.
C
         LOCALX = ( UOP .EQ. 'LOCMIN' ) .OR. ( UOP .EQ. 'LOCMAX' )

         NOADJX =  ( ADJUST .EQ.  0.D0    ) .AND.
     .           ( ( UOP    .EQ. 'ABSMIN' ) .OR. ( UOP .EQ. 'ABSMAX' ) )

         IF ( LOCALX .OR. NOADJX ) THEN

C
C           These operators correspond to 1-pass searches.
C
            NPASS = 1

         ELSE

            NPASS = 2

         END IF

C
C        Fill in the prefix strings.
C
C        Note that we've already performed error checks on QTNUM.
C
         DO I = 1, NPASS
            CALL REPMI ( SRCPRE(I,QTNUM), '#', NPASS, RPTPRE(I) )
         END DO

      END IF

C
C     Here's where the real work gets done:  we solve for the
C     result window.  The code below is quantity-specific.  However,
C     in each case, we always initialize the utility routines for
C     the quantity of interest, then call the generic relation
C     pre-image solver ZZGFREL.
C
      IF ( QTNUM .EQ. SEP ) THEN

C
C        Separation condition initializer.
C
         CALL ZZGFSPIN( OF, OBSRVR, SHAPE, FRAME, ABCORR )

         CALL ZZGFRELX( UDSTEP,        UDREFN,   ZZGFSPDC,
     .                  ZZGFUDLT,      ZZGFSPGQ,
     .                  OP,            REFVAL,   TOL,
     .                  ADJUST,        CNFINE,   MW,      NW,
     .                  WORK,          RPT,      UDREPI,
     .                  UDREPU,        UDREPF,   RPTPRE,
     .                  SRCSUF(1,SEP), BAIL,     UDBAIL,
     .                  RESULT  )


      ELSE IF ( QTNUM .EQ. DIST ) THEN

C
C        Distance condition initializer.
C
         CALL ZZGFDIIN ( TARGET, ABCORR, OBSRVR )

         CALL ZZGFRELX( UDSTEP,         UDREFN,   ZZGFDIDC,
     .                  ZZGFUDLT,       ZZGFDIGQ,
     .                  OP,             REFVAL,   TOL,
     .                  ADJUST,         CNFINE,   MW,     NW,
     .                  WORK,           RPT,      UDREPI,
     .                  UDREPU,         UDREPF,   RPTPRE,
     .                  SRCSUF(1,DIST), BAIL,     UDBAIL,
     .                  RESULT  )

      ELSE IF ( QTNUM .EQ. COORD ) THEN

C
C        Solve for a coordinate condition. ZZGFCSLV calls the coordinate
C        event initializer.
C
         CALL  ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                    OBSRVR, DREF,   DVEC,   CORSYS, CORNAM,
     .                    OP,     REFVAL, TOL,    ADJUST, UDSTEP,
     .                    UDREFN, RPT,    UDREPI, UDREPU, UDREPF,
     .                    BAIL,   UDBAIL, MW,     NW,     WORK,
     .                    CNFINE, RESULT                        )

      ELSE IF ( QTNUM .EQ. ANGSPD  ) THEN

C
C        d( sep )
C        --------     ---Not yet implemented---
C        dt
C

      ELSE IF ( QTNUM .EQ. RNGRAT ) THEN

C
C        Range rate condition initializer.
C
C        Default the interval for the QDERIV call in ZZGFRRDC to one
C        TDB second. This should have a function interface to
C        reset.
C
         DT = 1.D0

         CALL ZZGFRRIN ( TARGET, ABCORR, OBSRVR, DT )

         CALL ZZGFRELX( UDSTEP,           UDREFN,   ZZGFRRDC,
     .                  ZZGFUDLT,         ZZGFRRGQ,
     .                  OP,               REFVAL,   TOL,
     .                  ADJUST,           CNFINE,   MW,      NW,
     .                  WORK,             RPT,      UDREPI,
     .                  UDREPU,           UDREPF,   RPTPRE,
     .                  SRCSUF(1,RNGRAT), BAIL,     UDBAIL,
     .                  RESULT  )

      ELSE IF ( QTNUM .EQ. PHASE ) THEN

C
C        Phase angle condition initializer.
C
         CALL ZZGFPAIN ( TARGET, ILLUM, ABCORR, OBSRVR )

         CALL ZZGFRELX( UDSTEP,          UDREFN,   ZZGFPADC,
     .                  ZZGFUDLT,        ZZGFPAGQ,
     .                  OP,              REFVAL,   TOL,
     .                  ADJUST,          CNFINE,   MW,     NW,
     .                  WORK,            RPT,      UDREPI,
     .                  UDREPU,          UDREPF,   RPTPRE,
     .                  SRCSUF(1,PHASE), BAIL,     UDBAIL,
     .                  RESULT  )

      ELSE IF ( QTNUM .EQ. APDIAM ) THEN

C
C                ---Not yet implemented---
C

      ELSE IF ( QTNUM .EQ. ILUANG ) THEN
C
C        Illumination angle condition initializer.
C
         CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,
     .                   REF,    ABCORR, OBSRVR, SPOINT )

         CALL ZZGFRELX( UDSTEP,           UDREFN,   ZZGFILDC,
     .                  ZZGFUDLT,         ZZGFILGQ,
     .                  OP,               REFVAL,   TOL,
     .                  ADJUST,           CNFINE,   MW,       NW,
     .                  WORK,             RPT,      UDREPI,
     .                  UDREPU,           UDREPF,   RPTPRE,
     .                  SRCSUF(1,ILUANG), BAIL,     UDBAIL,
     .                  RESULT                                   )


      ELSE

C
C        QTNUM is not a recognized event code. This block should
C        never execute since we already checked the input quantity
C        name string.
C
         CALL SETMSG ( 'Unknown event ''#''. This error indicates '
     .              // 'a bug. Please contact NAIF.' )
         CALL ERRCH  ( '#', GQUANT            )
         CALL SIGERR ( 'SPICE(BUG)'           )
         CALL CHKOUT ( 'GFEVNT'               )
         RETURN

      END  IF

      CALL CHKOUT (  'GFEVNT' )

      RETURN
      END

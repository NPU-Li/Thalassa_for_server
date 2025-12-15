C$Procedure CKCOV ( CK coverage )

      SUBROUTINE CKCOV ( CKFNM, IDCODE, NEEDAV, LEVEL,
     .                   TOL,   TIMSYS, COVER          )

C$ Abstract
C
C     Find the coverage window for a specified object in a specified CK
C     file.
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
C     CELLS
C     DAF
C     CK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     POINTING
C     TIME
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         CKFNM
      INTEGER               IDCODE
      LOGICAL               NEEDAV
      CHARACTER*(*)         LEVEL
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      COVER ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CKFNM      I   Name of CK file.
C     IDCODE     I   ID code of object.
C     NEEDAV     I   Flag indicating whether angular velocity is needed.
C     LEVEL      I   Coverage level: 'SEGMENT' OR 'INTERVAL'.
C     TOL        I   Tolerance in ticks.
C     TIMSYS     I   Time system used to represent coverage.
C     COVER     I-O  Window giving coverage for IDCODE.
C
C$ Detailed_Input
C
C     CKFNM    is the name of a C-kernel.
C
C     IDCODE   is the integer ID code of an object, normally a
C              spacecraft structure or instrument, for which pointing
C              data are expected to exist in the specified CK file.
C
C     NEEDAV   is a logical variable indicating whether only segments
C              having angular velocity are to be considered when
C              determining coverage. When NEEDAV is .TRUE., segments
C              without angular velocity don't contribute to the coverage
C              window; when NEEDAV is .FALSE., all segments for IDCODE
C              may contribute to the coverage window.
C
C     LEVEL    is the level (granularity) at which the coverage is
C              examined. Allowed values and corresponding meanings are:
C
C                 'SEGMENT'    The output coverage window contains
C                              intervals defined by the start and stop
C                              times of segments for the object
C                              designated by IDCODE.
C
C                 'INTERVAL'   The output coverage window contains
C                              interpolation intervals of segments for
C                              the object designated by IDCODE. For type
C                              1 segments, which don't have
C                              interpolation intervals, each epoch
C                              associated with a pointing instance is
C                              treated as a singleton interval; these
C                              intervals are added to the coverage
C                              window.
C
C                              All interpolation intervals are
C                              considered to lie within the segment
C                              bounds for the purpose of this summary:
C                              if an interpolation interval extends
C                              beyond the segment coverage interval,
C                              only its intersection with the segment
C                              coverage interval is considered to
C                              contribute to the total coverage.
C
C     TOL      is a tolerance value expressed in ticks of the spacecraft
C              clock associated with IDCODE. Before each interval is
C              inserted into the coverage window, the interval is
C              intersected with the segment coverage interval, then if
C              the intersection is non-empty, it is expanded by TOL: the
C              left endpoint of the intersection interval is reduced by
C              TOL and the right endpoint is increased by TOL. Adjusted
C              interval endpoints, when expressed as encoded SCLK, never
C              are less than zero ticks. Any intervals that overlap as a
C              result of the expansion are merged.
C
C              The coverage window returned when TOL > 0 indicates the
C              coverage provided by the file to the CK readers CKGPAV
C              and CKGP when that value of TOL is passed to them as an
C              input.
C
C     TIMSYS   is a string indicating the time system used in the output
C              coverage window. TIMSYS may have the values:
C
C                  'SCLK'    Elements of COVER are expressed in encoded
C                            SCLK ("ticks"), where the clock is
C                            associated with the object designated by
C                            IDCODE.
C
C                  'TDB'     Elements of COVER are expressed as seconds
C                            past J2000 TDB.
C
C
C     COVER    is an initialized SPICE window data structure. COVER
C              optionally may contain coverage data on input; on output,
C              the data already present in COVER will be combined with
C              coverage found for the object designated by IDCODE in the
C              file CKFNM.
C
C              If COVER contains no data on input, its size and
C              cardinality still must be initialized.
C
C$ Detailed_Output
C
C     COVER    is a SPICE window data structure which represents the
C              merged coverage for IDCODE. When the coverage level is
C              'INTERVAL', this is the set of time intervals for which
C              data for IDCODE are present in the file CKFNM, merged
C              with the set of time intervals present in COVER on input.
C              The merged coverage is represented as the union of one or
C              more disjoint time intervals. The window COVER contains
C              the pairs of endpoints of these intervals.
C
C              When the coverage level is 'SEGMENT', COVER is computed
C              in a manner similar to that described above, but the
C              coverage intervals used in the computation are those of
C              segments rather than interpolation intervals within
C              segments.
C
C              When TOL is > 0, the intervals comprising the coverage
C              window for IDCODE are expanded by TOL and any intervals
C              overlapping as a result are merged. The resulting window
C              is returned in COVER. The expanded window in no case
C              extends beyond the segment bounds in either direction by
C              more than TOL.
C
C              The interval endpoints contained in COVER are encoded
C              spacecraft clock times if TIMSYS is 'SCLK'; otherwise the
C              times are converted from encoded spacecraft clock to
C              seconds past J2000 TDB.
C
C              See the $Examples section below for a complete example
C              program showing how to retrieve the endpoints from COVER.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file has transfer format, the error
C         SPICE(INVALIDFORMAT) is signaled.
C
C     2)  If the input file is not a transfer file but has architecture
C         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled.
C
C     3)  If the input file is a binary DAF file of type other than CK,
C         the error SPICE(INVALIDFILETYPE) is signaled.
C
C     4)  If the CK file cannot be opened or read, an error is signaled
C         by a routine in the call tree of this routine. The output
C         window will not be modified.
C
C     5)  If the size of the output window argument COVER is
C         insufficient to contain the actual number of intervals in the
C         coverage window for IDCODE, an error is signaled by a routine
C         in the call tree of this routine.
C
C     6)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
C         signaled.
C
C     7)  If LEVEL is not recognized, the error SPICE(INVALIDOPTION)
C         is signaled.
C
C     8)  If TIMSYS is not recognized, the error SPICE(NOTSUPPORTED)
C         is signaled.
C
C     9)  If a time conversion error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C     10) If the output time system is TDB, the CK subsystem must be
C         able to map IDCODE to the ID code of the associated spacecraft
C         clock. If this mapping cannot be performed, an error is
C         signaled by a routine in the call tree of this routine.
C
C     11) If the input CK type is not one of the supported CK types, the
C         error SPICE(NOTSUPPORTED) is signaled. This problem may
C         indicate the version of the SPICE Toolkit being used is
C         outdated and a new version is required.
C
C$ Files
C
C     This routine reads a C-kernel.
C
C     If the output time system is 'TDB', then a leapseconds kernel
C     and an SCLK kernel for the spacecraft clock associated with
C     IDCODE must be loaded before this routine is called.
C
C     If the ID code of the clock associated with IDCODE is not
C     equal to
C
C        IDCODE / 1000
C
C     then the kernel variable
C
C        CK_<IDCODE>_SCLK
C
C     must be present in the kernel pool to identify the clock
C     associated with IDCODE. This variable must contain the ID code
C     to be used for conversion between SCLK and TDB. Normally this
C     variable is provided in a text kernel loaded via FURNSH.
C
C$ Particulars
C
C     This routine provides an API via which applications can determine
C     the coverage a specified CK file provides for a specified
C     object.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Display the interval-level coverage for each object in a
C        specified CK file. Use tolerance of zero ticks. Do not
C        request angular velocity. Express the results in the TDB time
C        system.
C
C        Find the set of objects in the file. Loop over the contents
C        of the ID code set: find the coverage for each item in the
C        set and display the coverage.
C
C
C        Example code begins here.
C
C
C              PROGRAM CKCOV_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               WNCARD
C              INTEGER               CARDI
C        C
C        C     Local parameters
C        C
C        C
C        C     Declare the coverage window.  Make enough room
C        C     for MAXIV intervals.
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               MAXIV
C              PARAMETER           ( MAXIV  = 100000 )
C
C              INTEGER               WINSIZ
C              PARAMETER           ( WINSIZ = 2 * MAXIV )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 50 )
C
C              INTEGER               MAXOBJ
C              PARAMETER           ( MAXOBJ = 1000 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    CKFNM
C              CHARACTER*(FILSIZ)    LSK
C              CHARACTER*(FILSIZ)    SCLK
C              CHARACTER*(TIMLEN)    TIMSTR
C
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
C              DOUBLE PRECISION      E
C
C              INTEGER               I
C              INTEGER               IDS   ( LBCELL : MAXOBJ )
C              INTEGER               J
C              INTEGER               NIV
C
C        C
C        C     Load a leapseconds kernel and SCLK kernel for output
C        C     time conversion.  Note that we assume a single spacecraft
C        C     clock is associated with all of the objects in the CK.
C        C
C              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK  )
C              CALL FURNSH ( LSK )
C
C              CALL PROMPT ( 'Name of SCLK kernel        > ', SCLK )
C              CALL FURNSH ( SCLK )
C
C        C
C        C     Get name of CK file.
C        C
C              CALL PROMPT ( 'Name of CK file            > ', CKFNM )
C
C        C
C        C     Initialize the set IDS.
C        C
C              CALL SSIZEI ( MAXOBJ, IDS )
C
C        C
C        C     Initialize the window COVER.
C        C
C              CALL SSIZED ( WINSIZ, COVER )
C
C        C
C        C     Find the set of objects in the CK file.
C        C
C              CALL CKOBJ ( CKFNM, IDS )
C
C        C
C        C     We want to display the coverage for each object.  Loop
C        C     over the contents of the ID code set, find the coverage
C        C     for each item in the set, and display the coverage.
C        C
C              DO I = 1, CARDI( IDS )
C        C
C        C        Find the coverage window for the current
C        C        object. Empty the coverage window each time
C        C        so we don't include data for the previous object.
C        C
C                 CALL SCARDD ( 0,   COVER )
C                 CALL CKCOV  ( CKFNM,       IDS(I),  .FALSE.,
C             .                 'INTERVAL',  0.D0,    'TDB',    COVER )
C
C        C
C        C        Get the number of intervals in the coverage
C        C        window.
C        C
C                 NIV = WNCARD( COVER )
C
C        C
C        C        Display a simple banner.
C        C
C                 WRITE (*,*) '========================================'
C                 WRITE (*,*) 'Coverage for object ', IDS(I)
C
C        C
C        C        Convert the coverage interval start and stop
C        C        times to TDB calendar strings.
C        C
C                 DO J = 1, NIV
C        C
C        C           Get the endpoints of the Jth interval.
C        C
C                    CALL WNFETD ( COVER, J, B, E )
C        C
C        C           Convert the endpoints to TDB calendar
C        C           format time strings and display them.
C        C
C                    CALL TIMOUT ( B,
C             .                    'YYYY MON DD HR:MN:SC.###### ' //
C             .                    '(TDB) ::TDB',
C             .                    TIMSTR                           )
C                    WRITE (*,*) ' '
C                    WRITE (*,*) 'Interval: ', J
C                    WRITE (*,*) 'Start:    ', TIMSTR
C
C                    CALL TIMOUT ( E,
C             .                    'YYYY MON DD HR:MN:SC.###### ' //
C             .                    '(TDB) ::TDB',
C             .                    TIMSTR                          )
C                    WRITE (*,*) 'Stop:     ', TIMSTR
C                    WRITE (*,*) ' '
C
C                 END DO
C
C                 WRITE (*,*) '========================================'
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using the LSK file named naif0010.tls, the SCLK file
C        named cas00145.tsc and the CK file named 08052_08057ra.bc, the
C        output was:
C
C
C        Name of leapseconds kernel > naif0010.tls
C        Name of SCLK kernel        > cas00145.tsc
C        Name of CK file            > 08052_08057ra.bc
C         ========================================
C         Coverage for object       -82000
C
C         Interval:            1
C         Start:    2008 FEB 21 00:01:07.771186 (TDB)
C         Stop:     2008 FEB 23 22:53:30.001738 (TDB)
C
C
C         Interval:            2
C         Start:    2008 FEB 23 22:58:13.999732 (TDB)
C         Stop:     2008 FEB 24 02:22:25.913175 (TDB)
C
C
C         Interval:            3
C         Start:    2008 FEB 24 02:27:49.910886 (TDB)
C         Stop:     2008 FEB 24 19:46:33.470587 (TDB)
C
C
C         Interval:            4
C         Start:    2008 FEB 24 19:49:33.469315 (TDB)
C         Stop:     2008 FEB 25 04:25:21.250677 (TDB)
C
C
C         Interval:            5
C         Start:    2008 FEB 25 04:29:33.248897 (TDB)
C         Stop:     2008 FEB 25 15:23:44.971594 (TDB)
C
C
C         Interval:            6
C         Start:    2008 FEB 25 15:24:12.971396 (TDB)
C         Stop:     2008 FEB 25 20:25:04.843864 (TDB)
C
C
C         Interval:            7
C         Start:    2008 FEB 25 20:25:48.843553 (TDB)
C         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
C
C         ========================================
C
C
C     2) Find the segment-level coverage for the object designated by
C        IDCODE provided by the set of CK files loaded via a
C        metakernel. (The metakernel must also specify leapseconds and
C        SCLK kernels.) Use tolerance of zero ticks. Do not request
C        angular velocity. Express the results in the TDB time system.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: ckcov_ex2.tm
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
C             File name                      Contents
C             ---------                      --------
C             naif0010.tls                   Leapseconds
C             cas00145.tsc                   Cassini SCLK
C             08052_08057ra.bc               Orientation for Cassini
C
C           \begindata
C
C             KERNELS_TO_LOAD = ( 'naif0010.tls'
C                                 'cas00145.tsc'
C                                 '08052_08057ra.bc')
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM CKCOV_EX2
C              IMPLICIT NONE
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C              INTEGER               MAXCOV
C              PARAMETER           ( MAXCOV = 100000 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 50 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    FILE
C              CHARACTER*(LNSIZE)    IDCH
C              CHARACTER*(FILSIZ)    META
C              CHARACTER*(FILSIZ)    SOURCE
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(LNSIZE)    TYPE
C
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      COVER  ( LBCELL : 2*MAXCOV )
C              DOUBLE PRECISION      E
C
C              INTEGER               COUNT
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               IDCODE
C              INTEGER               NIV
C
C              LOGICAL               FOUND
C
C        C
C        C     Prompt for the metakernel name; load the metakernel.
C        C     The metakernel lists the CK files whose coverage
C        C     for IDCODE we'd like to determine.  The metakernel
C        C     must also specify a leapseconds kernel and an SCLK
C        C     kernel for the clock associated with IDCODE.
C        C
C              CALL PROMPT ( 'Enter name of metakernel > ', META )
C
C              CALL FURNSH ( META )
C
C        C
C        C     Get the ID code of interest.
C        C
C              CALL PROMPT ( 'Enter ID code            > ', IDCH )
C
C              CALL PRSINT ( IDCH,  IDCODE )
C
C        C
C        C     Initialize the coverage window.
C        C
C              CALL SSIZED ( MAXCOV, COVER )
C
C        C
C        C     Find out how many kernels are loaded.  Loop over the
C        C     kernels:  for each loaded CK file, add its coverage
C        C     for IDCODE, if any, to the coverage window.
C        C
C              CALL KTOTAL ( 'CK', COUNT )
C
C              DO I = 1, COUNT
C
C                 CALL KDATA ( I,       'CK',    FILE,  TYPE,
C             .                SOURCE,  HANDLE,  FOUND       )
C
C                 CALL CKCOV ( FILE,      IDCODE,  .FALSE.,
C             .                'SEGMENT', 0.D0,    'TDB',    COVER )
C
C              END DO
C
C        C
C        C     Display results.
C        C
C        C     Get the number of intervals in the coverage
C        C     window.
C        C
C              NIV = WNCARD( COVER )
C
C        C
C        C     Display a simple banner.
C        C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Coverage for object ', IDCODE
C
C        C
C        C     Convert the coverage interval start and stop
C        C     times to TDB calendar strings.
C        C
C              DO I = 1, NIV
C        C
C        C        Get the endpoints of the Ith interval.
C        C
C                 CALL WNFETD ( COVER, I, B, E )
C        C
C        C        Convert the endpoints to TDB calendar
C        C        format time strings and display them.
C        C
C                 CALL TIMOUT ( B,
C             .                 'YYYY MON DD HR:MN:SC.###### ' //
C             .                 '(TDB) ::TDB',
C             .                 TIMSTR                           )
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Interval: ', I
C                 WRITE (*,*) 'Start:    ', TIMSTR
C
C                 CALL TIMOUT ( E,
C             .                 'YYYY MON DD HR:MN:SC.###### ' //
C             .                 '(TDB) ::TDB',
C             .                 TIMSTR                           )
C                 WRITE (*,*) 'Stop:     ', TIMSTR
C                 WRITE (*,*) ' '
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using the meta-kernel file named ckcov_ex2.tm and
C        the NAIF ID "-82000" (Cassini spacecraft bus), the output was:
C
C
C        Enter name of metakernel > ckcov_ex2.tm
C        Enter ID code            > -82000
C
C         Coverage for object       -82000
C
C         Interval:            1
C         Start:    2008 FEB 21 00:01:07.771186 (TDB)
C         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
C
C
C$ Restrictions
C
C     1)  When this routine is used to accumulate coverage for IDCODE
C         provided by multiple CK files, the inputs NEEDAV, LEVEL, TOL,
C         and TIMSYS  must have the same values for all files in order
C         for the result to be meaningful.
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
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR)
C
C        Bug fix: added call to FAILED after call to GETFAT.
C
C        Changed input argument name "CK" to "CKFNM" for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard. Added solutions
C        using CASSINI data. Fixed a bug on Example #2. Added entry #11
C        in $Exceptions section and corrected short error messages in
C        entry #2 and #3.
C
C-    SPICELIB Version 2.0.0, 05-JAN-2014 (NJB) (BVS)
C
C        Updated index entries.
C
C        Last update was 05-JAN-2014 (NJB) (BVS)
C
C           Updated to support type 6.
C
C-    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB)
C
C        Corrected bug in first program in header $Examples section:
C        program now empties the coverage window prior to collecting
C        data for the current object. Updated examples to use WNCARD
C        rather than CARDD.
C
C-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB)
C
C-&


C$ Index_Entries
C
C     get coverage window for ck_object
C     get coverage start and stop time for ck_object
C     get coverage start and stop time for CK frame
C     get coverage start and stop time for CK instrument
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         KWINTV
      PARAMETER           ( KWINTV = 'INTERVAL' )

      CHARACTER*(*)         KWSEG
      PARAMETER           ( KWSEG  = 'SEGMENT' )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ARCH
      CHARACTER*(LNSIZE)    KERTYP

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DCTOL  ( ND )
      DOUBLE PRECISION      DESCR  ( ND + (NI+1)/2 )
      DOUBLE PRECISION      ET

      INTEGER               CLKID
      INTEGER               DTYPE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               SEGBEG
      INTEGER               SEGEND

      LOGICAL               AVOK
      LOGICAL               FOUND
      LOGICAL               ISTDB
      LOGICAL               SEGLVL

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKCOV' )

C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'CKCOV'                                        )
         RETURN

      END IF

C
C     Use a logical flag to indicate whether this is a segment-level
C     coverage description.
C
      SEGLVL = EQSTR( LEVEL, KWSEG )

C
C     Check coverage level keyword.
C
      IF (  .NOT. ( SEGLVL .OR. EQSTR( LEVEL, KWINTV ) )  ) THEN

         CALL SETMSG ( 'Allowed values of LEVEL are # and #; actual '//
     .                 'value was #.'                                )
         CALL ERRCH  ( '#',  KWSEG                                   )
         CALL ERRCH  ( '#',  KWINTV                                  )
         CALL ERRCH  ( '#',  LEVEL                                   )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                        )
         CALL CHKOUT ( 'CKCOV'                                       )
         RETURN

      END IF

C
C     See whether GETFAT thinks we've got a CK file.
C
      CALL GETFAT ( CKFNM, ARCH, KERTYP )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKCOV' )
         RETURN
      END IF


      IF ( ARCH .EQ. 'XFR' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file '
     .   //            'must be a binary CK file to be readable '
     .   //            'by this routine.  If the input file is an '
     .   //            'CK file in transfer format, run TOBIN on '
     .   //            'the file to convert it to binary format.'  )
         CALL ERRCH  ( '#',  CKFNM                                 )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                      )
         CALL CHKOUT ( 'CKCOV'                                     )
         RETURN

      ELSE IF ( ARCH .NE. 'DAF' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file '
     .   //            'must be a binary CK file to be readable '
     .   //            'by this routine.  Binary CK files have '
     .   //            'DAF architecture.  If you expected the '
     .   //            'file to be a binary CK file, the problem '
     .   //            'may be due to the file being an old '
     .   //            'non-native file lacking binary file format '
     .   //            'information. It''s also possible the file '
     .   //            'has been corrupted.'                       )
         CALL ERRCH  ( '#',  CKFNM                                 )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDARCHTYPE)'                    )
         CALL CHKOUT ( 'CKCOV'                                     )
         RETURN

      ELSE IF ( KERTYP .NE. 'CK' ) THEN

         CALL SETMSG ( 'Input file # has file type #. The file '
     .   //            'must be a binary CK file to be readable '
     .   //            'by this routine. If you expected the '
     .   //            'file to be a binary CK file, the problem '
     .   //            'may be due to the file being an old '
     .   //            'non-native file lacking binary file format '
     .   //            'information. It''s also possible the file '
     .   //            'has been corrupted.'                       )
         CALL ERRCH  ( '#',  CKFNM                                 )
         CALL ERRCH  ( '#',  KERTYP                                )
         CALL SIGERR ( 'SPICE(INVALIDFILETYPE)'                    )
         CALL CHKOUT ( 'CKCOV'                                     )
         RETURN

      END IF


C
C     Set a logical flag indicating whether the time system is SCLK.
C
      ISTDB  = EQSTR ( TIMSYS, 'TDB' )

C
C     Check time system.
C
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' //
     .                    'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
            CALL CHKOUT ( 'CKCOV'                                   )
            RETURN

         END IF

      END IF

C
C     If the output time system is TDB, find the clock ID associated
C     with IDCODE.
C
      IF ( ISTDB ) THEN

         CALL CKMETA ( IDCODE, 'SCLK', CLKID )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKCOV' )
            RETURN
         END IF

      END IF

C
C     Open the file for reading.
C
      CALL DAFOPR ( CKFNM, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKCOV' )
         RETURN
      END IF

C
C     We will examine each segment descriptor in the file, and
C     we'll update our coverage bounds according to the data found
C     in these descriptors.
C
C     If TOL > 0, we'll apply TOL after we've found the coverage
C     for the zero-tolerance case.
C
C     If the time system is TDB, we'll convert the times to TDB
C     at the end of this routine.

C
C     Start a forward search.
C
      CALL DAFBFS ( HANDLE )

C
C     Find the next DAF array.
C
      CALL DAFFNA ( FOUND )


      DO WHILE ( FOUND )
C
C        Note:  we check FAILED() at the bottom of this loop; this
C        routine returns if FAILED() returns .TRUE. at that point.
C
C        Fetch and unpack the segment descriptor.
C
         CALL DAFGS ( DESCR )
         CALL DAFUS ( DESCR, ND, NI, DC, IC )

C
C        Let AVOK indicate whether the segment satisfies the
C        angular velocity restriction.
C
         AVOK = ( IC(4) .EQ. 1 ) .OR. ( .NOT. NEEDAV )


         IF (  ( IC(1) .EQ. IDCODE )  .AND.  AVOK  ) THEN
C
C           This segment is for the body of interest.  If angular
C           velocity is needed, this segment has it.
C
            IF ( SEGLVL ) THEN
C
C              This is a segment-level summary.
C
C              Insert the coverage bounds into the coverage window.
C              Adjust the interval using the tolerance.
C
               DCTOL(1)  =  MAX (  DC(1) - TOL,  0.D0  )
               DCTOL(2)  =         DC(2) + TOL

C
C              Convert the time to TDB if necessary.
C
               IF ( ISTDB ) THEN
C
C                 Convert the time bounds to TDB before inserting
C                 into the window.
C
                  DO I = 1, 2
                     CALL SCT2E ( CLKID, DCTOL(I), ET )
                     DCTOL(I)  =  ET
                  END DO

               END IF

               IF ( DCTOL(1) .LE. DCTOL(2) ) THEN
                  CALL WNINSD (  DCTOL(1),  DCTOL(2),  COVER )
               END IF

            ELSE
C
C              We're looking for an interval-level coverage window.
C              This information must be retrieved in a
C              data-type-dependent fashion.  The coverage routines
C              we'll call will, if necessary, adjust intervals by TOL
C              and convert interval times to TDB.
C
               DTYPE  = IC(3)
               SEGBEG = IC(5)
               SEGEND = IC(6)

               IF ( DTYPE .EQ. 1 ) THEN

                  CALL ZZCKCV01 ( HANDLE, SEGBEG, SEGEND,
     .                            CLKID,  TOL,    TIMSYS, COVER )

               ELSE IF ( DTYPE .EQ. 2 ) THEN

                  CALL ZZCKCV02 ( HANDLE, SEGBEG, SEGEND,
     .                            CLKID,  TOL,    TIMSYS, COVER )

               ELSE IF ( DTYPE .EQ. 3 ) THEN

                  CALL ZZCKCV03 ( HANDLE, SEGBEG, SEGEND,
     .                            CLKID,  TOL,    TIMSYS, COVER )

               ELSE IF ( DTYPE .EQ. 4 ) THEN

                  CALL ZZCKCV04 ( HANDLE, SEGBEG, SEGEND,
     .                            CLKID,  TOL,    TIMSYS, COVER )

               ELSE IF ( DTYPE .EQ. 5 ) THEN

                  CALL ZZCKCV05 ( HANDLE, SEGBEG, SEGEND, CLKID,
     .                            DC,     TOL,    TIMSYS, COVER )

               ELSE IF ( DTYPE .EQ. 6 ) THEN

                  CALL ZZCKCV06 ( HANDLE, SEGBEG, SEGEND, CLKID,
     .                            DC,     TOL,    TIMSYS, COVER )

               ELSE

                  CALL SETMSG ( 'Supported CK data types are 1, 2, ' //
     .                          '3, 4, 5.  Data type of segment: #. '//
     .                          'This problem may indicate that you '//
     .                          'need to update your SPICE Toolkit.' )
                  CALL ERRINT ( '#',  DTYPE                          )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                )
                  CALL CHKOUT ( 'CKCOV'                              )
                  RETURN

               END IF

            END IF

         END IF

         CALL DAFFNA ( FOUND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKCOV' )
            RETURN
         END IF

      END DO

C
C     COVER now represents the coverage of the entire file at the
C     granularity indicated by LEVEL, combined with the coverage
C     contained in COVER on input.
C
C     Release the file.
C
      CALL DAFCLS ( HANDLE )


      CALL CHKOUT ( 'CKCOV' )
      RETURN
      END

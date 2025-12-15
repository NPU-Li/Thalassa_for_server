C$Procedure ZZSWFFET ( Private, switch frame kernel pool data fetch )

      SUBROUTINE ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                      FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Fetch kernel variables for a specified switch frame. Data
C     for the new frame are added to the switch frame database.
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
C     KERNEL
C     NAIF_IDS
C     TIME
C
C$ Keywords
C
C     Private
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzswtchf.inc'

      INTEGER               FRAMID
      INTEGER               HDFRAM ( * )
      INTEGER               FRPOOL ( LBSNGL : * )
      INTEGER               FIDLST ( * )
      INTEGER               BASBEG ( * )
      INTEGER               FREE
      INTEGER               BASCNT ( * )
      LOGICAL               USETIM ( * )
      LOGICAL               BINARY ( * )
      INTEGER               CLSSES ( * )
      INTEGER               CLSIDS ( * )
      INTEGER               BASLST ( * )
      DOUBLE PRECISION      STARTS ( * )
      DOUBLE PRECISION      STOPS ( * )
      INTEGER               FRAMAT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRAMID     I   The frame ID of a switch frame.
C     HDFRAM    I-O  Array of hash collision list head nodes.
C     FRPOOL    I-O  Hash collision list pool.
C     FIDLST    I-O  Frame IDs corresponding to list nodes.
C     BASBEG    I-O  Start indices of base lists of switch frames.
C     FREE      I-O  First free index in base frame arrays.
C     BASCNT    I-O  Counts of base frames for each switch frame.
C     USETIM    I-O  Logical flags indicating presence of time bounds.
C     BINARY    I-O  Logical flags indicating binary search usage.
C     CLSSES    I-O  Classes of base frames.
C     CLSIDS    I-O  Frame class IDs of base frames.
C     BASLST    I-O  Array containing lists of base frame IDs.
C     STARTS    I-O  Lists of interval start times.
C     STOPS     I-O  Lists of interval stop times.
C     FRAMAT     O   Index in FIDLST of input switch frame ID.
C
C$ Detailed_Input
C
C     FRAMID      is the integer ID code for a switch reference frame
C                 for which kernel pool data are to be fetched.
C
C     HDFRAM      is an array containing head nodes of hash collision
C                 lists in FRPOOL.
C
C     FRPOOL      is a singly linked list pool containing collision
C                 lists corresponding to hash values.
C
C     FIDLST      is an array of switch frame ID codes. The Ith element
C                 of FRPOOL corresponds to the ID code at index I in
C                 FIDLST.
C
C     BASBEG      is an array of start indices of base frame lists and
C                 base frame attributes. The Ith element of BASBEG is
C                 the start index in BASLST of the base frame list of
C                 the switch frame having ID at index I in FIDLST. The
C                 attributes of the base frame at a given index in
C                 BASLST are stored at the same index in parallel
C                 arrays.
C
C     FREE        is the index of the first free element in the array
C                 BASLST. This is also the index of the first free
C                 elements in the parallel arrays CLSSES, CLSIDS,
C                 STARTS and STOPS.
C
C     BASCNT      is an array of counts of base frames associated with
C                 switch frames. The Ith element of BASCNT is the base
C                 frame count of the switch frame having ID at index I
C                 in FIDLST.
C
C     USETIM      is an array of logical flags indicating the existence
C                 of time intervals associated with the base frames of
C                 a given switch frame. The flag at index I of USETIM
C                 is .TRUE. if and only if the switch frame having ID
C                 at index I of FIDLST has time intervals associated
C                 with its base frames.
C
C     BINARY      is an array of logical flags indicating the
C                 eligibility of a given switch frame for use of a
C                 binary search to locate the base frame time interval
C                 containing a given request time. Eligibility means
C                 that time intervals exist and that the stop time of
C                 the interval at index J is less than or equal to the
C                 start time at index J+1. The flag at index I of
C                 BINARY is .TRUE. if and only if the switch frame at
C                 index I of FIDLST is eligible for binary search.
C
C     CLSSES      is an array of classes of base frames. The element at
C                 index I is the class of the base frame at index I of
C                 BASLST, if that element of BASLST is defined.
C
C     CLSIDS      is of frame class IDs of base frames. The element
C                 at index I is the frame class ID of the base frame
C                 at index I of BASLST, if that element of BASLST is
C                 defined.
C
C     BASLST      is an array of ID codes of base frames associated
C                 with switch frames. The base frame having ID at index
C                 BASBEG(I)-1+J of BASLST is the Jth base frame of the
C                 switch frame having frame ID at index I of FIDLST.
C
C     STARTS      is an array of base frame applicability interval
C                 start times. The Ith element of STARTS is the start
C                 time of the base frame at index I of BASLST, if that
C                 element is defined and that frame has associated time
C                 intervals. For a given switch frame, all bases have
C                 associated time intervals or none do.
C
C     STOPS       is an array of base frame applicability interval
C                 start times. The Ith element of STARTS is the start
C                 time of the base frame at index I of BASLST, if that
C                 element is defined and that frame has associated time
C                 intervals. For a given switch frame, all bases have
C                 associated time intervals or none do.
C
C$ Detailed_Output
C
C     HDFRAM      is the input collision list head node array, updated
C                 if necessary to reflect the addition of a node for
C                 the switch frame designated by the input frame ID.
C
C     FRPOOL      is the hash collision list pool, updated to reflect
C                 the addition of a node for the switch frame
C                 designated by the input frame ID.
C
C     FIDLST      is the switch frame ID list, updated to reflect the
C                 addition of the input frame ID.
C
C     BASBEG      is the input base frame head list, updated to reflect
C                 the addition of an entry for the switch frame
C                 designated by the input frame ID.
C
C     FREE        is the common index in BASLST and its parallel arrays
C                 of their first free elements, following addition of
C                 data associated with the input frame ID.
C
C     BASCNT      is the input base frame count list, updated to
C                 reflect the addition of the base frames associated
C                 with the input frame ID.
C
C     USETIM      is the input time interval presence flag array,
C                 updated to reflect the addition of the flag for the
C                 the switch frame designated by the input frame ID.
C
C     BINARY      is the input binary search eligibility array, updated
C                 to reflect the addition of the flag for the switch
C                 frame designated by the input frame ID.
C
C     CLSSES      is the input class array, updated to reflect the
C                 addition of base frame classes for the switch frame
C                 designated by the input frame ID.
C
C     CLSIDS      is the input frame class ID array, updated to reflect
C                 the addition of base frame frame class IDs for the
C                 switch frame designated by the input frame ID.
C
C     BASLST      is the input base frame array, updated to reflect
C                 the addition of base frames associated with the
C                 switch frame designated by the input frame ID.
C
C     STARTS      is the input base frame interval start time array,
C                 updated to reflect the addition of base frames
C                 associated with the switch frame designated by the
C                 input frame ID.
C
C     STOPS       is the input base frame interval stop time array,
C                 updated to reflect the addition of base frames
C                 associated with the switch frame designated by the
C                 input frame ID.
C
C     FRAMAT      is the common index in the arrays
C
C                    FIDLST, BASBEG, BASCNT, USETIM, BINARY
C
C                 of information associated with the switch frame
C                 designated by the input frame ID.
C
C$ Parameters
C
C     LBSNGL      is the lower bound of a singly linked list pool
C                 managed by the integer hash subsystem.
C
C     See the include file zzswtchf.inc for SPICE-private parameters
C     defining sizes of buffers used by the switch frame subsystem.
C
C$ Exceptions
C
C     1)  If the input frame ID maps to a frame specification for which
C         the ID doesn't match, the error SPICE(BADFRAMESPEC) is
C         signaled.
C
C     2)  If kernel variables for any of the universal frame
C         specification items
C
C            frame ID
C            frame name
C            frame center
C            frame class
C            frame class ID
C
C         or the switch frame item
C
C            base frame list
C
C        are not found, the error SPICE(MISSINGFRAMEVAR) is signaled.
C
C     3) If the ID of a base frame cannot be mapped to a name,
C        the error SPICE(FRAMENAMENOTFOUND) is signaled.
C
C     4) If the stop time for a given interval is not strictly greater
C        than the corresponding start time, the error
C        SPICE(BADTIMEBOUNDS) is signaled.
C
C     5) If only one of the start time or stop time kernel variables
C        is supplied, the error SPICE(PARTIALFRAMESPEC) is signaled.
C
C     6) If the count of start times doesn't match the count of
C        stop times, or if either count doesn't match the count of
C        base frames, the error SPICE(COUNTMISMATCH) is signaled.
C
C     7) If an error occurs during kernel variable lookup, the error
C        is signaled by a routine in the call tree of this routine.
C
C     8) If an error occurs during time conversion, the error
C        is signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C         - A switch frame specification, normally provided by a
C           frame kernel.
C
C              > Base frames may be specified by name or frame ID.
C
C              > Time interval bounds may be specified by singly
C                quoted strings or by data using the text kernel "@"
C                syntax.
C
C         - Frame specifications for base frames. Normally these
C           specifications are provided by frame kernels.
C
C           Even though specifications of frames identified by ID code
C           are not explicitly required by this routine, they are
C           required by the intended caller of this routine ZZSWFXFM.
C
C         - A leapseconds kernel, if time interval bounds are
C           specified by time strings.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine obtains switch frame specifications from the kernel
C     pool and adds them to the switch frame database.
C
C     If, upon entry, the database already contains the maximum number
C     of switch frames, the database will be cleared in order to make
C     room for the new switch frame specification.
C
C     If, upon entry, the database doesn't have enough room to buffer
C     the base frame data associated with a new switch frame, the
C     database will be cleared in order to make room for the new switch
C     frame specification.
C
C     Complete error checking of the new switch frame specification is
C     performed on each call.
C
C$ Examples
C
C     None. See usage in ZZSWFXFM.
C
C$ Restrictions
C
C     1)  This routine is SPICE-private. User applications must not
C         call it.
C
C     2)  To improve efficiency, this routine buffers switch frame
C         specifications. The buffers are emptied after any update
C         to the kernel pool. Since the process of parsing, and
C         buffering parameters provided by, switch frame specifications
C         is rather slow, applications using switch frames should
C         avoid high-frequency kernel pool updates.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
C
C-&

C$ Index_Entries
C
C     fetch switch frame data from kernel pool
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local parameters
C

C
C     Kernel variable name templates:
C
      CHARACTER*(*)         TMALGN
      PARAMETER           ( TMALGN = 'FRAME_#_ALIGNED_WITH' )

      CHARACTER*(*)         TMCENT
      PARAMETER           ( TMCENT = 'FRAME_#_CENTER' )

      CHARACTER*(*)         TMCLAS
      PARAMETER           ( TMCLAS = 'FRAME_#_CLASS' )

      CHARACTER*(*)         TMCLID
      PARAMETER           ( TMCLID = 'FRAME_#_CLASS_ID' )

      CHARACTER*(*)         TMID
      PARAMETER           ( TMID   = 'FRAME_#' )

      CHARACTER*(*)         TMFNAM
      PARAMETER           ( TMFNAM = 'FRAME_#_NAME' )

      CHARACTER*(*)         TMSTRT
      PARAMETER           ( TMSTRT = 'FRAME_#_START' )

      CHARACTER*(*)         TMSTOP
      PARAMETER           ( TMSTOP = 'FRAME_#_STOP' )

C
C     Other parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 80 )

C
C     Indices of items in the arrays of kernel variables:
C
      INTEGER               IXFNAM
      PARAMETER           ( IXFNAM = 1 )

      INTEGER               IXID
      PARAMETER           ( IXID   = IXFNAM + 1 )

      INTEGER               IXCENT
      PARAMETER           ( IXCENT = IXID    + 1 )

      INTEGER               IXCLAS
      PARAMETER           ( IXCLAS = IXCENT + 1 )

      INTEGER               IXCLID
      PARAMETER           ( IXCLID = IXCLAS + 1 )

      INTEGER               IXALGN
      PARAMETER           ( IXALGN = IXCLID + 1 )

      INTEGER               IXSTRT
      PARAMETER           ( IXSTRT = IXALGN + 1 )

      INTEGER               IXSTOP
      PARAMETER           ( IXSTOP = IXSTRT + 1 )


      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               NKV
      PARAMETER           ( NKV    = 8 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    BASNAM
      CHARACTER*(1)         BASTYP
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVALGN
      CHARACTER*(KVNMLN)    KVCLID
      CHARACTER*(KVNMLN)    KVCENT
      CHARACTER*(KVNMLN)    KVCLAS
      CHARACTER*(KVNMLN)    KVID
      CHARACTER*(KVNMLN)    KVFNAM
      CHARACTER*(KVNMLN)    KVNAMS ( NKV )
      CHARACTER*(KVNMLN)    KVSTRT
      CHARACTER*(KVNMLN)    KVSTOP
      CHARACTER*(1)         STPTYP
      CHARACTER*(1)         STRTYP
      CHARACTER*(TIMLEN)    TIMSTR

      INTEGER               CENTER
      INTEGER               FID
      INTEGER               FRCENT
      INTEGER               FRCLAS
      INTEGER               FRCLID
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NBASES
      INTEGER               NFRAVL
      INTEGER               NSTART
      INTEGER               NSTOP
      INTEGER               ROOM

C
C     KVFND(*) indicates whether a specified kernel variable was found.
C
      LOGICAL               HAVTIM
      LOGICAL               INFFND
      LOGICAL               KVFND ( NKV )
      LOGICAL               NEW

      LOGICAL               STRXST
      LOGICAL               STPXST

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFFET' )

C
C     No result found yet.
C
      FRAMAT = 0

C
C     Create names of kernel variables, using the input frame ID.
C
C     Basic frame specification variables, excluding that for the
C     frame ID. We'll need the frame name before we can create the
C     name of the kernel variable for the frame ID.
C
      CALL REPMI( TMCENT, '#', FRAMID, KVCENT )
      CALL REPMI( TMCLAS, '#', FRAMID, KVCLAS )
      CALL REPMI( TMCLID, '#', FRAMID, KVCLID )
      CALL REPMI( TMFNAM, '#', FRAMID, KVFNAM )

C
C     Switch frame specification variables:
C
      CALL REPMI( TMALGN, '#', FRAMID, KVALGN )
      CALL REPMI( TMSTRT, '#', FRAMID, KVSTRT )
      CALL REPMI( TMSTOP, '#', FRAMID, KVSTOP )

C
C     Below, we use logical flags to describe the status
C     of kernel variable availability:
C
C         KVFND(*) indicates that a variable exists.
C
C     Look up the basic variables.
C
      CALL GIPOOL( KVCLAS, 1, 1, N, FRCLAS, KVFND(IXCLAS) )
      CALL GIPOOL( KVCLID, 1, 1, N, FRCLID, KVFND(IXCLID) )
      CALL GCPOOL( KVFNAM, 1, 1, N, FRNAME, KVFND(IXFNAM) )

      IF ( FAILED() ) THEN

         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

C
C     Fetch the switch frame ID variable.
C
      IF ( KVFND(IXFNAM) ) THEN
C
C        Use the frame name to fetch the switch frame ID variable.
C
         CALL REPMC ( TMID, '#', FRNAME, KVID )

         CALL GIPOOL( KVID, 1, 1, N, FID, KVFND(IXID) )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

C
C        The frame ID of the frame specification had better match
C        the input frame ID.
C
         IF ( KVFND(IXID) .AND. ( FID .NE. FRAMID ) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Input frame ID was #, but ID in frame '
     .      //            'specification from kernel pool was #. ' )
            CALL ERRINT ( '#', FRAMID           )
            CALL ERRINT ( '#', FID              )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC)' )
            CALL CHKOUT ( 'ZZSWFFET'            )
            RETURN

         END IF
C
C        The frame name must always be found. If not, the error
C        will be diagnosed in the block below.
C
      ELSE
C
C        We couldn't find the frame name. Indicate the frame ID
C        variable [sic] wasn't found, since its found flag won't be set
C        by GIPOOL.
C
         KVFND(IXID) = .FALSE.

      END IF

C
C     Look up the central body of the frame. The name of the kernel
C     variable for the body could refer to the frame by name or frame
C     ID; the body itself could be specified by name or body ID.
C
      IF ( KVFND(IXFNAM) ) THEN

         CALL ZZDYNBID ( FRNAME, FRAMID, 'CENTER', FRCENT )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

         KVFND(IXCENT) = .TRUE.
      ELSE
         KVFND(IXCENT) = .FALSE.
      END IF

C
C     Look up the type and count of the base frame list. It
C     may have either string or numeric type.
C
      CALL DTPOOL( KVALGN, KVFND(IXALGN), NBASES, BASTYP )

      IF ( FAILED() ) THEN
C
C        This code should be unreachable but is provided for safety.
C
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

C
C     Store the kernel variable names in order to prepare for
C     checking availability of required variables.
C
      KVNAMS(IXID)   = KVID
      KVNAMS(IXFNAM) = KVFNAM
      KVNAMS(IXCENT) = KVCENT
      KVNAMS(IXCLAS) = KVCLAS
      KVNAMS(IXCLID) = KVCLID
      KVNAMS(IXALGN) = KVALGN

C
C     Check for required variables that haven't been supplied.
C
      DO I = 1, IXALGN
C
C        The first 6 items are needed; start and stop times are needed
C        if at least one is present.
C
         IF ( .NOT. KVFND(I) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Kernel variable #, needed for '
     .      //            'specification of switch frame having '
     .      //            'frame ID #, was not '
     .      //            'found in the kernel pool. This can occur '
     .      //            'when a frame kernel providing the required '
     .      //            'switch frame specification has not been '
     .      //            'loaded, or if the specification is present '
     .      //            'but is incorrect.'                         )
            CALL ERRCH  ( '#', KVNAMS(I)                              )
            CALL ERRINT ( '#', FRAMID                                 )
            CALL SIGERR ( 'SPICE(MISSINGFRAMEVAR)'                    )
            CALL CHKOUT ( 'ZZSWFFET'                                  )
            RETURN

         END IF

      END DO

C
C     Find out whether time bounds are supplied, and if so, which data
C     type is used to represent them.
C
      KVNAMS(IXSTRT) = KVSTRT
      KVNAMS(IXSTOP) = KVSTOP

      CALL DTPOOL( KVSTRT, KVFND(IXSTRT), NSTART, STRTYP )
      CALL DTPOOL( KVSTOP, KVFND(IXSTOP), NSTOP,  STPTYP )

      IF ( FAILED() ) THEN
C
C        This code should be unreachable but is provided for safety.
C
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

C
C     Availability of starts and stops is optional...however both
C     must be provided if either is, and the counts must match
C     those of the base frames.
C
C     Note that it's necessary to perform these checks before buffering
C     base frame data, since that process assumes that the number of
C     start and stop times is either zero or matches the number of
C     base frames.
C
      STRXST = KVFND(IXSTRT)
      STPXST = KVFND(IXSTOP)

      HAVTIM = STRXST .AND. STPXST

      IF ( HAVTIM ) THEN
C
C        Make sure that counts of base frames, start times, and
C        stop times are equal.
C
         IF ( ( NSTART .NE. NSTOP ) .OR. ( NSTART .NE. NBASES ) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Kernel variables for the switch frame '
     .      //            'having frame ID # have mismatched sizes: '
     .      //            'number of base frames = #; number of start '
     .      //            'times = #; number of stop times = #.'      )
            CALL ERRINT ( '#', FRAMID            )
            CALL ERRINT ( '#', NBASES            )
            CALL ERRINT ( '#', NSTART            )
            CALL ERRINT ( '#', NSTOP             )
            CALL SIGERR ( 'SPICE(COUNTMISMATCH)' )
            CALL CHKOUT ( 'ZZSWFFET'             )
            RETURN

         END IF

      ELSE
C
C        Check for inconsistent presence of start and stop times.
C
         IF ( STRXST .OR. STPXST ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

C
C           Create long message template to be filled in.
C
            CALL SETMSG ( 'Kernel variable #, which specifies base '
     .      //            'frame applicability # times, was not '
     .      //            'provided for the switch frame having '
     .      //            'frame ID #, while the kernel variable # '
     .      //            'specifying base frame applicability '
     .      //            '# times was provided. Switch frame '
     .      //            'applicability start and stop times are '
     .      //            'optional, but both must be provided if '
     .      //            'either is.'  )

            IF ( STRXST ) THEN
C
C              Stop times are missing.
C
               CALL ERRCH  ( '#', KVSTOP  )
               CALL ERRCH  ( '#', 'stop'  )
               CALL ERRINT ( '#', FRAMID  )
               CALL ERRCH  ( '#', KVSTRT  )
               CALL ERRCH  ( '#', 'start' )

            ELSE
C
C              Start times are missing.
C
               CALL ERRCH  ( '#', KVSTRT  )
               CALL ERRCH  ( '#', 'start' )
               CALL ERRINT ( '#', FRAMID  )
               CALL ERRCH  ( '#', KVSTOP  )
               CALL ERRCH  ( '#', 'stop'  )

            END IF

            CALL SIGERR ( 'SPICE(PARTIALFRAMESPEC)' )
            CALL CHKOUT ( 'ZZSWFFET'                )
            RETURN

         END IF

      END IF

C
C     Compute room for the output data.
C
      ROOM = MAXBAS + 1 - FREE

C
C     Check the available room in the frame ID pool and in the frame
C     base arrays.
C
      CALL ZZHSIAVL ( FRPOOL, NFRAVL )

      IF ( ( NFRAVL .EQ. 0 ) .OR. ( ROOM .LT. NBASES ) ) THEN
C
C        There's no room for another frame in the frame pool, or
C        there's no room for the new frame's data in the base
C        frame arrays.
C
C        If we can make enough room by re-initializing the whole
C        local frame database, do so. If the new frame has too
C        much data to fit in the empty base frame arrays, that's
C        an error.
C
         IF ( NBASES .LE. MAXBAS ) THEN
C
C           We can fit the frame data in by clearing out the
C           database.
C
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

C
C           The database is now initialized.
C
            FREE = 1
            ROOM = MAXBAS

         ELSE
C
C           We can't make enough room.
C
C           Initialize local data structures, for safety.
C
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'The requested frame # has # associated '
     .      //            'base frames. The maximum number that '
     .      //            'can be supported is #.'   )
            CALL ERRINT ( '#', FRAMID                )
            CALL ERRINT ( '#', NBASES                )
            CALL ERRINT ( '#', MAXBAS                )
            CALL SIGERR ( 'SPICE(TOOMANYBASEFRAMES)' )
            CALL CHKOUT ( 'ZZSWFFET'                 )
            RETURN

         END IF

      END IF

C
C     Add the frame and its data to the local database.
C
C     Start by adding the frame ID to the frame hash structure.
C     The output argument NEW indicates whether the item is new;
C     we don't need to check this argument. We also don't need to
C     check for failure of ZZHSIADD, since we've already ensured
C     there's room in the hash for a new clock.
C
      CALL ZZHSIADD ( HDFRAM, FRPOOL, FIDLST, FRAMID, FRAMAT, NEW )

C
C     At this point, FRAMAT is set.
C
C     Store the frame ID, base start index, base count, and time
C     interval availability attributes for this frame.
C
      FIDLST( FRAMAT ) = FRAMID
      BASBEG( FRAMAT ) = FREE
      BASCNT( FRAMAT ) = NBASES
      USETIM( FRAMAT ) = HAVTIM

C
C     Look up the base frame variables. The base frame list may have
C     either string or numeric type.
C
      KVFND(IXALGN) = .FALSE.

      IF ( BASTYP .EQ. 'N' ) THEN
C
C        The frames are specified by ID code. We can buffer the frame
C        ID codes immediately.
C
         CALL GIPOOL ( KVALGN,       1,            ROOM,
     .                 BASCNT(FREE), BASLST(FREE), KVFND(IXALGN) )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

      ELSE IF ( BASTYP .EQ. 'C' ) THEN
C
C        The frames are specified by name. We must convert the
C        names to frame ID codes before we can can buffer the IDs.
C
         KVFND(IXALGN) = .FALSE.

         DO I = 1, NBASES

            CALL GCPOOL( KVALGN,  I,       1,
     .                   N,       BASNAM,  KVFND(IXALGN) )

            IF ( FAILED() ) THEN
C
C              This code should be unreachable but is provided for
C              safety.
C
               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL CHKOUT( 'ZZSWFFET' )
               RETURN

            END IF

            CALL NAMFRM( BASNAM, BASLST(FREE-1+I) )

            IF ( BASLST(FREE-1+I) .EQ. 0 ) THEN

               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL SETMSG( 'Base frame name # of switch frame '
     .         //           '# could not be translated to a '
     .         //           'frame ID code '           )
               CALL ERRCH ( '#', BASNAM                )
               CALL ERRINT( '#', FRAMID                )
               CALL SIGERR( 'SPICE(FRAMENAMENOTFOUND)' )
               CALL CHKOUT( 'ZZSWFFET'                 )
               RETURN

            END IF

         END DO

      ELSE
C
C        Backstop: this code should be unreachable.
C
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL SETMSG ( 'Base frame kernel variable # exists but '
     .   //            'DTPOOL returned data type # rather than '
     .   //            'one of the expected values: ''C'' or ''N''.' )
         CALL ERRCH  ( '#', KVALGN  )
         CALL ERRCH  ( '#', BASTYP  )
         CALL SIGERR ( 'SPICE(BUG)' )
         CALL CHKOUT( 'ZZSWFFET'    )
         RETURN

      END IF

C
C     Get attributes of each base frame.
C
      DO I = 1, NBASES
C
C        Store the frame class and frame class ID for this base frame.
C
         J = FREE - 1 + I

         CALL FRINFO ( BASLST(J), CENTER,
     .                 CLSSES(J), CLSIDS(J), INFFND )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

         IF ( .NOT. INFFND ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'No specification was found for '
     .      //            'base frame # of switch frame #.' )
            CALL ERRINT ( '#', BASLST(J)                    )
            CALL ERRINT ( '#', FRAMID                       )
            CALL SIGERR ( 'SPICE(FRAMEINFONOTFOUND)'        )
            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

      END DO

C
C     Fetch interval bounds if they're present.
C
      IF ( USETIM(FRAMAT) ) THEN
C
C        Fetch start times.
C
C        Note that DTPOOL sets the type to 'X' if the target
C        kernel variable is not found.
C
         IF ( STRTYP .EQ. 'N' ) THEN
C
C           The start times are represented by double precision
C           numbers.
C
            CALL GDPOOL( KVSTRT, 1,            ROOM,
     .                   NSTART, STARTS(FREE), KVFND(IXSTRT) )

         ELSE IF ( STRTYP .EQ. 'C' ) THEN
C
C           The start times are represented by strings.
C
C           We must convert each string to a numeric value and store
C           the value in the start time buffer.
C
            KVFND(IXSTRT) = .FALSE.

            DO I = 1, NBASES

               CALL GCPOOL( KVSTRT,  I,      1,
     .                      N,       TIMSTR, KVFND(IXSTRT) )

               CALL STR2ET( TIMSTR, STARTS(FREE-1+I) )

            END DO

         ELSE
C
C           Backstop: this code should be unreachable.
C
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Start time kernel variable # exists but '
     .      //            'DTPOOL returned data type # rather than '
     .      //            'one of the expected values: ''C'' '
     .      //            'or ''N''.' )
            CALL ERRCH  ( '#', KVSTRT  )
            CALL ERRCH  ( '#', STRTYP  )
            CALL SIGERR ( 'SPICE(BUG)' )
            CALL CHKOUT( 'ZZSWFFET'    )
            RETURN

         END IF

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

C
C        Fetch stop times.
C
         IF ( STPTYP .EQ. 'N' ) THEN
C
C           The stop times are represented by double precision numbers.
C
            CALL GDPOOL( KVSTOP, 1,           ROOM,
     .                   NSTOP,  STOPS(FREE), KVFND(IXSTOP) )

         ELSE IF ( STPTYP .EQ. 'C' ) THEN
C
C           The stop times are represented by strings.
C
C           We must convert each string to a numeric value and store
C           the value in the stop time buffer.
C
            KVFND(IXSTOP) = .FALSE.

            DO I = 1, NSTOP

               CALL GCPOOL( KVSTOP,  I,      1,
     .                      N,       TIMSTR, KVFND(IXSTOP) )

               CALL STR2ET( TIMSTR, STOPS(FREE-1+I) )

            END DO

         ELSE
C
C           Backstop: this code should be unreachable.
C
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Stop time kernel variable # exists but '
     .      //            'DTPOOL returned data type # rather than '
     .      //            'one of the expected values: ''C'' '
     .      //            'or ''N''.' )
            CALL ERRCH  ( '#', KVSTOP  )
            CALL ERRCH  ( '#', STPTYP  )
            CALL SIGERR ( 'SPICE(BUG)' )
            CALL CHKOUT( 'ZZSWFFET'    )
            RETURN

         END IF

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN
         END IF

C
C        Singleton intervals and out-of-order intervals are not
C        allowed.
C
         DO I = 1, NSTART

            J = FREE - 1 + I

            IF ( STARTS(J) .GE. STOPS(J) ) THEN

               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL SETMSG ( 'Interval time bounds are not strictly '
     .         //            'increasing at interval index # for '
     .         //            'switch frame #. Time bounds are #:# TDB '
     .         //            '(# TDB : # TDB)'      )
               CALL ERRINT ( '#', I                 )
               CALL ERRINT ( '#', FRAMID            )
               CALL ERRDP  ( '#', STARTS(J)         )
               CALL ERRDP  ( '#', STOPS(J)          )
               CALL ETCAL  ( STARTS(J), TIMSTR      )
               CALL ERRCH  ( '#',       TIMSTR      )
               CALL ETCAL  ( STOPS(J),  TIMSTR      )
               CALL ERRCH  ( '#',       TIMSTR      )
               CALL SIGERR ( 'SPICE(BADTIMEBOUNDS)' )
               CALL CHKOUT ( 'ZZSWFFET'             )
               RETURN

            END IF

         END DO

      END IF

C
C     Determine whether binary search on the time intervals is
C     possible.
C
      IF ( USETIM(FRAMAT) ) THEN

         I = 2
         BINARY( FRAMAT ) = .TRUE.

         DO WHILE (  ( I .LE. NBASES ) .AND.  BINARY(FRAMAT)  )
C
C           Note that proper ordering of start and stop times for
C           each interval has already been verified.
C
            J = BASBEG(FRAMAT) - 1 + I

            IF ( STOPS(J-1) .GT. STARTS(J) ) THEN

               BINARY( FRAMAT ) = .FALSE.

            END IF

            I = I + 1

         END DO

      ELSE
         BINARY( FRAMAT ) = .FALSE.
      END IF

C
C     We've found the data. The switch frame database has been
C     updated. FRAMAT is already set.
C
C     Account for the base frame storage used.
C
      FREE = FREE + NBASES

      CALL CHKOUT ( 'ZZSWFFET' )
      RETURN
      END

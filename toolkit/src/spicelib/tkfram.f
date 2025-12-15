C$Procedure TKFRAM ( TK frame, find position rotation )

      SUBROUTINE TKFRAM ( FRCODE, ROT, FRAME, FOUND )

C$ Abstract
C
C     Find the position rotation matrix from a Text Kernel (TK) frame
C     with the specified frame class ID to its base frame.
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
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               FRCODE
      DOUBLE PRECISION      ROT   ( 3, 3 )
      INTEGER               FRAME
      LOGICAL               FOUND

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 200 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ----------------------------------------------
C     FRCODE     I   Frame class ID of a TK frame.
C     ROT        O   Rotation matrix from TK frame to frame FRAME.
C     FRAME      O   Frame ID of the base reference.
C     FOUND      O   .TRUE. if the rotation could be determined.
C
C$ Detailed_Input
C
C     FRCODE   is the unique frame class ID of the TK frame for which
C              data is being requested. For TK frames the frame class
C              ID is always equal to the frame ID.
C
C$ Detailed_Output
C
C     ROT      is a position rotation matrix that converts positions
C              relative to the TK frame given by its frame class ID,
C              FRCODE, to positions relative to the base frame given by
C              its frame ID, FRAME.
C
C              Thus, if a position S has components x,y,z in the TK
C              frame, then S has components x', y', z' in the base
C              frame.
C
C                 .-  -.     .-     -. .- -.
C                 | x' |     |       | | x |
C                 | y' |  =  |  ROT  | | y |
C                 | z' |     |       | | z |
C                 `-  -'     `-     -' `- -'
C
C
C     FRAME    is the ID code of the base reference frame to which ROT
C              will transform positions.
C
C     FOUND    is a logical indicating whether or not a frame definition
C              for the TK frame with the frame class ID, FRCODE, was
C              constructed from kernel pool data. If ROT and FRAME were
C              constructed, FOUND will be returned with the value .TRUE.
C              Otherwise it will be returned with the value .FALSE.
C
C$ Parameters
C
C     BUFSIZ   is the number of rotation, frame class ID pairs that can
C              have their instance data buffered for the sake of
C              improving run-time performance. This value MUST be
C              positive and should probably be at least 10.
C
C$ Exceptions
C
C     1)  If some kernel variable associated with this frame is not
C         present in the kernel pool, or does not have the proper type
C         or dimension, an error is signaled by a routine in the call
C         tree of this routine. In such a case FOUND will be set to
C         .FALSE.
C
C     2)  If the input FRCODE has the value 0, the error
C         SPICE(ZEROFRAMEID) is signaled. FOUND will be set to .FALSE.
C
C     3)  If the name of the frame corresponding to FRCODE cannot be
C         determined, the error SPICE(INCOMPLETEFRAME) is signaled.
C
C     4)  If the frame given by FRCODE is defined relative to a frame
C         that is unrecognized, the error SPICE(BADFRAMESPEC) is
C         signaled. FOUND will be set to .FALSE.
C
C     5)  If the kernel pool specification for the frame given by
C         FRCODE is not one of 'MATRIX', 'ANGLES' or 'QUATERNION',
C         the error SPICE(UNKNOWNFRAMESPEC) is signaled. FOUND will be
C         set to .FALSE.
C
C     6)  If the frame FRCODE is equal to the relative frame ID (i.e.
C         the frame is defined relative to itself), the error
C         SPICE(BADFRAMESPEC2) is signaled. FOUND will be set to .FALSE.
C
C     7)  If name-based and ID-based forms of any TKFRAME_ keyword
C         are detected in the kernel pool at the same time, the error
C         SPICE(COMPETINGFRAMESPEC) is signaled. FOUND will be set to
C         .FALSE.
C
C$ Files
C
C     This routine makes use of the loaded text kernels to determine
C     the rotation from a constant offset TK frame to its base frame.
C
C$ Particulars
C
C     This routine is used to construct the rotation from some frame
C     that is a constant rotation offset from some other reference
C     frame. This rotation is derived from data stored in the kernel
C     pool.
C
C     This routine is intended to be used as a low level routine by the
C     frame system software. However, you could use this routine to
C     directly retrieve the rotation from an fixed offset TK frame to
C     its base frame.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the rotation from the DSS-34 topocentric frame to
C        its base Earth body-fixed frame and use it to determine the
C        geodetic latitude and longitude of the DSS-34 site.
C
C
C        Use the FK kernel below to load the required topocentric
C        reference frame definition for the DSS-34 site.
C
C           earth_topo_050714.tf
C
C
C        Example code begins here.
C
C
C              PROGRAM TKFRAM_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      DPR
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         MYTOPO
C              PARAMETER           ( MYTOPO = 'DSS-34_TOPO' )
C
C              INTEGER               MXFRLN
C              PARAMETER           ( MXFRLN = 26 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(MXFRLN)    FRNAME
C
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      RAD
C              DOUBLE PRECISION      ROT   ( 3, 3 )
C              DOUBLE PRECISION      Z     ( 3    )
C
C              INTEGER               FRAME
C              INTEGER               FRCODE
C
C              LOGICAL               FOUND
C
C        C
C        C     Load the FK that contains the topocentric reference
C        C     frame definition for DSS-34.
C        C
C              CALL FURNSH ( 'earth_topo_050714.tf' )
C
C        C
C        C     The name of the topocentric frame is MYTOPO.
C        C     First we get the ID code of the topocentric frame.
C        C
C              CALL NAMFRM ( MYTOPO, FRCODE )
C
C        C
C        C     Next get the rotation from the topocentric frame to
C        C     the body-fixed frame. We can use the TK frame ID in
C        C     place of the TK frame class ID in this call because
C        C     for TK frames these IDs are identical.
C        C
C              CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND )
C
C        C
C        C     Make sure the topocentric frame is relative to one of
C        C     the Earth fixed frames.
C        C
C              CALL FRMNAM( FRAME, FRNAME )
C
C              IF (       FRNAME .NE. 'IAU_EARTH'
C             .     .AND. FRNAME .NE. 'EARTH_FIXED'
C             .     .AND. FRNAME .NE. 'ITRF93'  ) THEN
C
C                 WRITE (*,*) 'The frame ', MYTOPO,
C             .               ' does not appear to be '
C                 WRITE (*,*) 'defined relative to an '
C             .            // 'Earth fixed frame.'
C                 STOP
C
C              END IF
C
C        C
C        C     Things look ok. Get the location of the Z-axis in the
C        C     topocentric frame.
C        C
C              Z(1) = ROT(1,3)
C              Z(2) = ROT(2,3)
C              Z(3) = ROT(3,3)
C
C        C
C        C     Convert the Z vector to latitude, longitude and radius.
C        C
C              CALL RECLAT ( Z, RAD, LAT, LON )
C
C              WRITE (*,'(A)') 'The geodetic coordinates of the center'
C              WRITE (*,'(A)') 'of the topographic frame are:'
C              WRITE (*,*)
C              WRITE (*,'(A,F20.13)') '   Latitude  (deg): ', LAT*DPR()
C              WRITE (*,'(A,F20.13)') '   Longitude (deg): ', LON*DPR()
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        The geodetic coordinates of the center
C        of the topographic frame are:
C
C           Latitude  (deg):    148.9819650021110
C           Longitude (deg):    -35.3984778756552
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 20-AUG-2021 (JDR) (BVS) (NJB)
C
C        BUG FIX: the routine now signals an error if it detects
C        name-based and ID-based forms of any TKFRAME_ keyword present
C        in the POOL at the same time. This prevents name-based
C        keywords from frame definitions loaded with lower priority
C        from being used instead of ID-based keywords from frame
C        definitions loaded with higher priority.
C
C        BUG FIX: when failing to fetch any frame keywords from the
C        POOL or for any other reason, the routine now always returns
C        FOUND = .FALSE. Previously FOUND could be set to .TRUE. by a
C        DTPOOL call preceding the failure.
C
C        BUG FIX: when failing due to a frame defined relative to
C        itself or due to an unrecognized _SPEC, the routine now always
C        returns FRAME = 0. Previously FRAME was set to the _RELATIVE
C        keyword.
C
C        BUG FIX: the misspelled short error message
C        SPICE(INCOMPLETEFRAME) was corrected. The message had been
C        spelled correctly in header comments but not in the code.
C
C        Changed to return ROT as identity for all failures; previously
C        it was returned this way only for some failures.
C
C        Changed the input argument name ID to FRCODE for consistency
C        with other routines.
C
C        Fixed minor typo on the UNKNOWNFRAMESPEC long error message.
C
C        Edited the header to comply with NAIF standard and modern
C        SPICE CK and frames terminology.
C
C        Added complete code example based on existing fragments.
C
C        Construction of kernel variable names now uses trimmed
C        strings in order to suppress gfortran compile warnings.
C
C        Added DATA statements to initialize BUFFI, BUFFD, and IDENTS.
C        This change suppresses ftnchek warnings for variables possibly
C        not initialized before use. It is not a bug fix.
C
C        Minor inline comment typos were corrected.
C
C-    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS)
C
C        Added an error check for frames defined relative to
C        themselves.
C
C        Increased BUFSIZ from 20 to 200.
C
C-    SPICELIB Version 2.1.0, 23-APR-2009 (NJB)
C
C        Bug fix: watch is deleted only for frames
C        that are deleted from the buffer.
C
C-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
C
C        Bug fix: this routine now deletes watches set on
C        kernel variables of frames that are discarded from
C        the local buffering system.
C
C-    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, UCRSS, VHATG and VSCL calls.
C
C-    SPICELIB Version 1.1.0, 21-NOV-2001 (FST)
C
C        Updated this routine to dump the buffer of frame ID codes
C        it saves when it or one of the modules in its call tree
C        signals an error. This fixes a bug where a frame's ID code is
C        buffered, but the matrix and kernel pool watcher were not set
C        properly.
C
C-    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT)
C
C-&


C$ Index_Entries
C
C     Fetch the rotation and frame of a text kernel frame
C     Fetch the rotation and frame of a constant offset frame
C
C-&


C
C     Spicelib Functions
C
      DOUBLE PRECISION      VDOT

      INTEGER               LNKNFN
      INTEGER               LNKTL
      INTEGER               RTRIM

      LOGICAL               BADKPV
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 14 )

      INTEGER               NVARS
      PARAMETER           ( NVARS = 14 )

      INTEGER               NDPS
      PARAMETER           ( NDPS  =  9 )

      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ = NDPS * BUFSIZ )

      INTEGER               NINTS
      PARAMETER           ( NINTS =  1 )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = NINTS * BUFSIZ )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )


C
C     Local Variables
C
      CHARACTER*(WDSIZE)    AGENT
      CHARACTER*(WDSIZE)    ALT    ( NITEMS )
      CHARACTER*(WDSIZE)    ALTNAT
      CHARACTER*(WDSIZE)    FRNAME
      CHARACTER*(WDSIZE)    IDSTR
      CHARACTER*(WDSIZE)    ITEM   ( NVARS  )
      CHARACTER*(WDSIZE)    OLDAGT
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    SPEC
      CHARACTER*(1)         TYPE
      CHARACTER*(WDSIZE)    UNITS

      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      BUFFD  ( NDPS, BUFSIZ )
      DOUBLE PRECISION      MATRIX ( 3,    3      )
      DOUBLE PRECISION      QUATRN ( 0:3 )
      DOUBLE PRECISION      QTMP   ( 0:3 )
      DOUBLE PRECISION      TEMPD

      INTEGER               AR
      INTEGER               AT
      INTEGER               AXES   ( 3 )
      INTEGER               BUFFI  ( NINTS,     BUFSIZ )
      INTEGER               I
      INTEGER               IDENTS ( 1,         BUFSIZ )
      INTEGER               IDNT   ( 1 )
      INTEGER               N
      INTEGER               OLDID
      INTEGER               POOL   ( 2, LBPOOL: BUFSIZ )
      INTEGER               R
      INTEGER               TAIL

      LOGICAL               BUFFRD
      LOGICAL               FIRST
      LOGICAL               FOUND1
      LOGICAL               FOUND2
      LOGICAL               FND
      LOGICAL               FULL
      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  AT     /  0            /
      DATA                  BUFFD  / DBUFSZ * 0.D0 /
      DATA                  BUFFI  / IBUFSZ * 0    /
      DATA                  FIRST  / .TRUE.        /
      DATA                  IDENTS / BUFSIZ * 0    /


C
C     Programmer's note: this routine makes use of the *implementation*
C     of LOCATI. If that routine is changed, the logic this routine
C     uses to locate buffered, old frame IDs may need to change as well.
C


C
C     Before we even check in, if N is less than 1 we can
C     just return.
C
C
C     Perform any initializations that might be needed for this
C     routine.
C
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CALL LNKINI ( BUFSIZ, POOL )

      END IF

C
C     Now do the standard SPICE error handling.  Sure this is
C     a bit unconventional, but nothing will be hurt by doing
C     the stuff above first.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TKFRAM' )

C
C     So far, we've not FOUND the rotation to the specified frame.
C
      FOUND = .FALSE.

C
C     Check the ID to make sure it is non-zero.
C
      IF (  FRCODE .EQ. 0 ) THEN

         CALL LNKINI ( BUFSIZ, POOL )

         CALL SETMSG ( 'Frame identification codes are '
     .   //            'required to be non-zero.  You''ve '
     .   //            'specified a frame with ID value '
     .   //            'zero. '                            )
         CALL SIGERR ( 'SPICE(ZEROFRAMEID)'                )
         CALL CHKOUT ( 'TKFRAM'                            )
         RETURN

      END IF

C
C     Find out whether our linked list pool is already full.
C     We'll use this information later to decide whether we're
C     going to have to delete a watcher.
C
      FULL = LNKNFN(POOL) .EQ. 0

      IF ( FULL ) THEN
C
C        If the input frame ID is not buffered, we'll need to
C        overwrite an existing buffer entry. In this case
C        the call to LOCATI we're about to make will overwrite
C        the ID code in the slot we're about to use. We need
C        this ID code, so extract it now while we have the
C        opportunity. The old ID sits at the tail of the list
C        whose head node is AT.
C
         TAIL  = LNKTL  ( AT, POOL )

         OLDID = IDENTS ( 1,  TAIL )

C
C        Create the name of the agent associated with the old
C        frame.
C
         OLDAGT = 'TKFRAME_#'
         CALL REPMI ( OLDAGT, '#', OLDID, OLDAGT )

      END IF

C
C     Look up the address of the instance data.
C
      IDNT(1) = FRCODE
      CALL LOCATI ( IDNT, 1, IDENTS, POOL, AT, BUFFRD )


      IF (  FULL  .AND. ( .NOT. BUFFRD )  ) THEN
C
C        Since the buffer is already full, we'll delete the watcher for
C        the kernel variables associated with OLDID, since there's no
C        longer a need for that watcher.
C
C        First clear the update status of the old agent; DWPOOL won't
C        delete an agent with a unchecked update.
C
         CALL CVPOOL ( OLDAGT, UPDATE )
         CALL DWPOOL ( OLDAGT )

      END IF

C
C     Until we have better information we put the identity matrix
C     into the output rotation and set FRAME to zero.
C
      CALL IDENT ( ROT )
      FRAME = 0

C
C     If we have to look up the data for our frame, we do
C     it now and perform any conversions and computations that
C     will be needed when it's time to convert coordinates to
C     directions.
C
C     Construct the name of the agent associated with the
C     requested frame.  (Each frame has its own agent).
C
      CALL INTSTR ( FRCODE, IDSTR  )
      CALL FRMNAM ( FRCODE, FRNAME )

      IF ( FRNAME .EQ. ' ' ) THEN

         CALL LNKINI ( BUFSIZ, POOL )

         CALL SETMSG ( 'The Text Kernel (TK) frame with ID code '
     .   //            '# does not have a recognized name. '      )
         CALL ERRINT ( '#', FRCODE                                )
         CALL SIGERR ( 'SPICE(INCOMPLETEFRAME)'                   )
         CALL CHKOUT ( 'TKFRAM'                                   )
         RETURN

      END IF

      AGENT  = 'TKFRAME_' // IDSTR( :RTRIM(IDSTR) )
      R      =  RTRIM(AGENT)

      ALTNAT = 'TKFRAME_' // FRNAME( :RTRIM(FRNAME) )
      AR     =  RTRIM(ALTNAT)

C
C     If the frame is buffered, we check the kernel pool to
C     see if there has been an update to this frame.
C
      IF ( BUFFRD ) THEN

         CALL CVPOOL ( AGENT(1:R), UPDATE )

      ELSE
C
C        If the frame is not buffered we definitely need to update
C        things.

         UPDATE = .TRUE.

      END IF


      IF ( .NOT. UPDATE ) THEN
C
C        Just look up the rotation matrix and relative-to
C        information from the local buffer.
C
         ROT(1,1)  = BUFFD ( 1, AT )
         ROT(2,1)  = BUFFD ( 2, AT )
         ROT(3,1)  = BUFFD ( 3, AT )
         ROT(1,2)  = BUFFD ( 4, AT )
         ROT(2,2)  = BUFFD ( 5, AT )
         ROT(3,2)  = BUFFD ( 6, AT )
         ROT(1,3)  = BUFFD ( 7, AT )
         ROT(2,3)  = BUFFD ( 8, AT )
         ROT(3,3)  = BUFFD ( 9, AT )

         FRAME     = BUFFI ( 1, AT )

      ELSE
C
C        Determine how the frame is specified and what it
C        is relative to.  The variables that specify
C        how the frame is represented and what it is relative to
C        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is
C        replaced by the text value of ID or the frame name.
C
         ITEM(1) = AGENT(1:R) // '_SPEC'
         ITEM(2) = AGENT(1:R) // '_RELATIVE'

         ALT (1) = ALTNAT(1:AR) // '_SPEC'
         ALT (2) = ALTNAT(1:AR) // '_RELATIVE'
C
C        See if the friendlier version of the kernel pool variables
C        are available.
C
C        If both forms are present, we signal an error.
C
         DO I = 1, 2

            CALL DTPOOL ( ITEM(I), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(I),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    //
     .                       'ID-based text kernel (fixed-'   //
     .                       'offset) frame definition '      //
     .                       'keywords ''#'' and ''#'' are '  //
     .                       'both present in the '           //
     .                       'POOL. Most likely this is '     //
     .                       'because loaded text kernels '   //
     .                       'contain competing definitions ' //
     .                       'of the ''#'' frame using '      //
     .                       'different keyword styles, '     //
     .                       'which is not allowed. '         )
               CALL ERRCH  ( '#', ITEM(I) )
               CALL ERRCH  ( '#', ALT(I) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(I) = ALT(I)
            END IF

         END DO

C
C        If either the SPEC or RELATIVE frame are missing from
C        the kernel pool, we simply return.
C
         IF (     BADKPV ( 'TKFRAM', ITEM(1), '=', 1, 1, 'C' )
     .       .OR. BADKPV ( 'TKFRAM', ITEM(2), '=', 1, 1, 'C' ) ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  (  ROT     )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN

         END IF

C
C        If we make it this far, look up the SPEC and RELATIVE frame.
C
         CALL GCPOOL ( ITEM(1),   1, 1, N, SPEC,  FND )
         CALL GCPOOL ( ITEM(2),   1, 1, N, NAME,  FND )

C
C        Look up the ID code for this frame.
C
         CALL NAMFRM ( NAME, FRAME )

         IF ( FRAME .EQ. 0 ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            CALL IDENT  (  ROT     )
            CALL SETMSG ( 'The frame to which frame # is '
     .      //            'relatively defined is not '
     .      //            'recognized. The kernel pool '
     .      //            'specification of the relative '
     .      //            'frame is ''#''.  This is not a '
     .      //            'recognized frame. '              )
            CALL ERRINT ( '#', FRCODE                       )
            CALL ERRCH  ( '#', NAME                         )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC)'             )
            CALL CHKOUT ( 'TKFRAM'                          )
            RETURN

         END IF

C
C        Make sure that the RELATIVE frame ID is distinct from the
C        frame ID. If they are the same, SPICE will go into an
C        indefinite loop.
C
         IF ( FRAME .EQ. FRCODE ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  (  ROT     )
            CALL SETMSG ( 'Bad fixed offset frame specification: '
     .      //            'the frame ''#'' (frame ID #) is '
     .      //            'defined relative to itself. SPICE '
     .      //            'cannot work with such frames. '         )
            CALL ERRCH  ( '#', NAME                                )
            CALL ERRINT ( '#', FRCODE                              )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC2)'                   )
            CALL CHKOUT ( 'TKFRAM'                                 )
            RETURN

         END IF

C
C        Convert SPEC to upper case so that we can easily check
C        to see if this is one of the expected specification types.
C
         CALL UCASE ( SPEC, SPEC )

         IF ( SPEC .EQ. 'MATRIX' ) THEN
C
C           This is the easiest case.  Just grab the matrix
C           from the kernel pool (and polish it up a bit just
C           to make sure we have a rotation matrix).
C
C           We give preference to the kernel pool variable
C           TKFRAME_<name>_MATRIX if it is available.
C
C           If both forms are present, we signal an error.
C
            ITEM(3) = AGENT (1:R)  // '_MATRIX'
            ALT (3) = ALTNAT(1:AR) // '_MATRIX'

            CALL DTPOOL ( ITEM(3), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(3),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    //
     .                       'ID-based text kernel (fixed-'   //
     .                       'offset) frame definition '      //
     .                       'keywords ''#'' and ''#'' are '  //
     .                       'both present in the '           //
     .                       'POOL. Most likely this is '     //
     .                       'because loaded text kernels '   //
     .                       'contain competing definitions ' //
     .                       'of the ''#'' frame using '      //
     .                       'different keyword styles, '     //
     .                       'which is not allowed. '         )
               CALL ERRCH  ( '#', ITEM(3) )
               CALL ERRCH  ( '#', ALT(3) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(3) = ALT(3)
            END IF

            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 9, 1, 'N' ) ) THEN

               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN

            END IF

C
C           The variable meets current expectations, look it up
C           from the kernel pool.
C
            CALL GDPOOL ( ITEM(3),   1, 9, N, MATRIX,  FND )

C
C           In this case the full transformation matrix has been
C           specified.  We simply polish it up a bit.
C
            CALL MOVED  ( MATRIX, 9, ROT )
            CALL SHARPR (            ROT )

C
C           The matrix might not be right-handed, so correct
C           the sense of the second and third columns if necessary.
C
            IF ( VDOT( ROT(1,2), MATRIX(1,2) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,2) )
            END IF

            IF ( VDOT( ROT(1,3), MATRIX(1,3) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,3) )
            END IF


         ELSE IF ( SPEC .EQ. 'ANGLES' ) THEN
C
C           Look up the angles, their units and axes for the
C           frame specified by ID. (Note that UNITS are optional).
C           As in the previous case we give preference to the
C           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>.
C
            ITEM(3) = AGENT(1:R)   // '_ANGLES'
            ITEM(4) = AGENT(1:R)   // '_AXES'
            ITEM(5) = AGENT(1:R)   // '_UNITS'

            ALT (3) = ALTNAT(1:AR) // '_ANGLES'
            ALT (4) = ALTNAT(1:AR) // '_AXES'
            ALT (5) = ALTNAT(1:AR) // '_UNITS'
C
C           Again, we give preference to the more friendly form
C           of TKFRAME specification.
C
C           If both forms are present, we signal an error.
C
            DO I = 3, 5

               CALL DTPOOL ( ITEM(I), FOUND1, N, TYPE )
               CALL DTPOOL ( ALT(I),  FOUND2, N, TYPE )

               IF ( FOUND1 .AND. FOUND2 ) THEN
                  CALL LNKINI ( BUFSIZ, POOL )
                  FRAME =  0
                  CALL IDENT  ( ROT      )
                  CALL SETMSG ( 'Frame name-based and frame '    //
     .                          'ID-based text kernel (fixed-'   //
     .                          'offset) frame definition '      //
     .                          'keywords ''#'' and ''#'' are '  //
     .                          'both present in the '           //
     .                          'POOL. Most likely this is '     //
     .                          'because loaded text kernels '   //
     .                          'contain competing definitions ' //
     .                          'of the ''#'' frame using '      //
     .                          'different keyword styles, '     //
     .                          'which is not allowed. '         )
                  CALL ERRCH  ( '#', ITEM(I) )
                  CALL ERRCH  ( '#', ALT(I) )
                  CALL ERRCH  ( '#', FRNAME )
                  CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
                  CALL CHKOUT ( 'TKFRAM' )
                  RETURN
               END IF

               IF ( FOUND2 ) THEN
                  ITEM(I) = ALT(I)
               END IF

            END DO


            IF (    BADKPV( 'TKFRAM', ITEM(3), '=', 3, 1, 'N' )
     .         .OR. BADKPV( 'TKFRAM', ITEM(4), '=', 3, 1, 'N' ) )
     .      THEN

               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN

            END IF

            UNITS = 'RADIANS'

            CALL GDPOOL ( ITEM(3),   1, 3, N, ANGLES, FND )
            CALL GIPOOL ( ITEM(4),   1, 3, N, AXES,   FND )
            CALL GCPOOL ( ITEM(5),   1, 1, N, UNITS,  FND )

C
C           Convert angles to radians.
C
            DO I = 1, 3

               CALL CONVRT ( ANGLES(I), UNITS, 'RADIANS', TEMPD )
               ANGLES(I) = TEMPD

            END DO

C
C           Compute the rotation from instrument frame to CK frame.
C
            CALL EUL2M (  ANGLES(1),   ANGLES(2),   ANGLES(3),
     .                    AXES  (1),   AXES  (2),   AXES  (3), ROT )

            IF ( FAILED() ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

         ELSE IF ( SPEC .EQ. 'QUATERNION' ) THEN
C
C           Look up the quaternion and convert it to a rotation
C           matrix. Again there are two possible variables that
C           may point to the quaternion. We give preference to
C           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q.
C
C           If both forms are present, we signal an error.
C
            ITEM(3) = AGENT(1:R)   // '_Q'
            ALT (3) = ALTNAT(1:AR) // '_Q'

            CALL DTPOOL ( ITEM(3), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(3),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    //
     .                       'ID-based text kernel (fixed-'   //
     .                       'offset) frame definition '      //
     .                       'keywords ''#'' and ''#'' are '  //
     .                       'both present in the '           //
     .                       'POOL. Most likely this is '     //
     .                       'because loaded text kernels '   //
     .                       'contain competing definitions ' //
     .                       'of the ''#'' frame using '      //
     .                       'different keyword styles, '     //
     .                       'which is not allowed. '         )
               CALL ERRCH  ( '#', ITEM(3) )
               CALL ERRCH  ( '#', ALT(3) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(3) = ALT(3)
            END IF


            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 4, 1, 'N' ) ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF
C
C           In this case we have the quaternion representation.
C           Again, we do a small amount of polishing of the input.
C
            CALL GDPOOL ( ITEM(3), 1, 4, N, QUATRN, FND )
            CALL VHATG  ( QUATRN,  4,       QTMP   )
            CALL Q2M    ( QTMP,             ROT    )

         ELSE
C
C           We don't recognize the SPEC for this frame.  Say
C           so.  Also note that perhaps the user needs to upgrade
C           the toolkit.
C
            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  ( ROT      )
            CALL SETMSG ( 'The frame specification "# = '
     .      //            '''#''" is not one of the recognized '
     .      //            'means of specifying a text-kernel '
     .      //            'constant offset frame. '
     .      //            'This may reflect a typographical '
     .      //            'error or may indicate that you '
     .      //            'need to consider updating your '
     .      //            'version of the SPICE toolkit. ' )

            CALL ERRCH  ( '#', ITEM(1) )
            CALL ERRCH  ( '#', SPEC    )
            CALL SIGERR ( 'SPICE(UNKNOWNFRAMESPEC)' )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN

         END IF

C
C        Buffer the identifier, relative frame and rotation matrix.
C
         BUFFD ( 1, AT ) = ROT(1,1)
         BUFFD ( 2, AT ) = ROT(2,1)
         BUFFD ( 3, AT ) = ROT(3,1)
         BUFFD ( 4, AT ) = ROT(1,2)
         BUFFD ( 5, AT ) = ROT(2,2)
         BUFFD ( 6, AT ) = ROT(3,2)
         BUFFD ( 7, AT ) = ROT(1,3)
         BUFFD ( 8, AT ) = ROT(2,3)
         BUFFD ( 9, AT ) = ROT(3,3)

         BUFFI ( 1, AT ) = FRAME

C
C        If these were not previously buffered, we need to set
C        a watch on the various items that might be used to define
C        this frame.
C
         IF ( .NOT. BUFFRD ) THEN
C
C           Immediately check for an update so that we will
C           not redundantly look for this item the next time this
C           routine is called.
C
            ITEM( 1) = AGENT(1:R)   // '_RELATIVE'
            ITEM( 2) = AGENT(1:R)   // '_SPEC'
            ITEM( 3) = AGENT(1:R)   // '_AXES'
            ITEM( 4) = AGENT(1:R)   // '_MATRIX'
            ITEM( 5) = AGENT(1:R)   // '_Q'
            ITEM( 6) = AGENT(1:R)   // '_ANGLES'
            ITEM( 7) = AGENT(1:R)   // '_UNITS'

            ITEM( 8) = ALTNAT(1:AR) // '_RELATIVE'
            ITEM( 9) = ALTNAT(1:AR) // '_SPEC'
            ITEM(10) = ALTNAT(1:AR) // '_AXES'
            ITEM(11) = ALTNAT(1:AR) // '_MATRIX'
            ITEM(12) = ALTNAT(1:AR) // '_Q'
            ITEM(13) = ALTNAT(1:AR) // '_ANGLES'
            ITEM(14) = ALTNAT(1:AR) // '_UNITS'

            CALL SWPOOL ( AGENT, 14, ITEM )
            CALL CVPOOL ( AGENT, UPDATE   )

         END IF

      END IF

      IF ( FAILED() ) THEN
         CALL LNKINI ( BUFSIZ, POOL )
         FRAME =  0
         CALL IDENT  ( ROT      )
         CALL CHKOUT ( 'TKFRAM' )
         RETURN
      END IF

C
C     All errors cause the routine to exit before we get to this
C     point.  If we reach this point we didn't have an error and
C     hence did find the rotation from ID to FRAME.
C
      FOUND = .TRUE.

C
C     That's it
C
      CALL CHKOUT ( 'TKFRAM' )
      RETURN

      END

C$Procedure ZZSWFXFM ( Private, get switch frame transformation )

      SUBROUTINE ZZSWFXFM ( INFRM, ET, NDIM, XFORM, OUTFRM, FOUND )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return switch frame state or position transformation matrix.
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
C     CK
C     FRAMES
C     NAIF_IDS
C     TIME
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'
      INCLUDE              'zzswtchf.inc'
      INCLUDE              'zzctr.inc'

      INTEGER               INFRM
      DOUBLE PRECISION      ET
      INTEGER               NDIM
      DOUBLE PRECISION      XFORM ( NDIM, NDIM )
      INTEGER               OUTFRM
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INFRM      I   Frame ID code of switch frame.
C     ET         I   Time, expressed as seconds past J2000 TDB.
C     NDIM       I   Dimension: 3 or 6.
C     XFORM      O   Transformation from switch to selected base frame.
C     OUTFRM     O   ID code of selected base frame.
C     FOUND      O   Flag indicating whether transformation was found.
C
C$ Detailed_Input
C
C     INFRM       is the frame ID code of switch frame for which
C                 the transformation to a base frame at time ET is
C                 sought.
C
C     ET          is a frame orientation evaluation epoch, expressed
C                 as seconds past J2000 TDB.
C
C     NDIM        is the common row and column dimension of the output
C                 matrix: 6 or 3. When NDIM is set to 6, a 6x6 state
C                 transformation matrix is returned; when NDIM is set
C                 to 3, a 3x3 position transformation matrix is
C                 returned.
C
C$ Detailed_Output
C
C     XFORM       is a matrix that transforms vectors of dimension NDIM
C                 from the switch frame designated by INFRM to the
C                 frame designated by OUTFRM, at time ET.
C
C                 By definition, a switch frame is aligned with a
C                 selected base frame at time ET. However, depending on
C                 the class of the selected base frame, OUTFRM may
C                 be set to that base frame, the base frame at ET of
C                 that base, or the frame J2000. See the description of
C                 OUTFRM below.
C
C                 XFORM is defined if and only if FOUND is .TRUE.
C
C
C     OUTFRM      is the frame ID code of a frame selected by this
C                 routine, which may be a base frame of the switch
C                 frame designated by INFRM, or which may be another
C                 frame, selected as described below. The output matrix
C                 XFORM transforms vectors of dimension NDIM from the
C                 switch frame designated by INFRM to the frame
C                 designated by OUTFRM.
C
C                 OUTFRM is defined if and only if FOUND is .TRUE.
C
C                 OUTFRM is selected as follows:
C
C                    - Base frames are obtained from the kernel
C                      variable
C
C                         FRAME_< INFRM >_ALIGNED_WITH
C
C                      Base frames are listed in order of ascending
C                      priority. A given frame may occur at multiple
C                      indices of the base frame array.
C
C                    - If time intervals are included in the switch
C                      frame specification, the highest priority
C                      base frame for which the associated time
C                      interval contains ET, and in which data are
C                      available at ET, is chosen.
C
C                      If time intervals are not provided, the highest
C                      priority base frame for which data are available
C                      at ET is chosen.
C
C                      Data availability may depend on NDIM. This is
C                      because 6x6 state transformations that rely on
C                      CK data require angular velocity to be present;
C                      3x3 position transformations don't.
C
C                      If no base frame providing data at ET is found
C                      by the process described above, FOUND is set to
C                      .FALSE. and OUTFRM is undefined.
C
C                    - If the process succeeds in finding a base frame
C                      providing data at ET, OUTFRM is set according to
C                      the class of the base frame:
C
C                         > Base is class INERTL: OUTFRM is set to
C                           the ID code of J2000.
C
C                         > Base is class PCK: OUTFRM is set to the ID
C                           code of J2000.
C
C                         > Base is class CK: OUTFRM is set to the ID
C                           code of the CK frame's base frame at time
C                           ET. This is the base returned by FRMGET for
C                           ET, if NDIM is 6, or the base returned by
C                           ROTGET at ET if NDIM is 3.
C
C                         > Base is class TK: OUTFRM is set to the ID
C                           code of the TK frame's base frame. This is
C                           the frame specified by the
C                           TKFRAME_<ID>_RELATIVE keyword of the TK
C                           frame's specification.
C
C                         > Base is class DYN: OUTFRM is set to the ID
C                           code of the base frame.
C
C                         > Base is class SWTCH: OUTFRM is set to the
C                           ID code of the base frame.
C
C
C     FOUND       is a logical flag indicating whether a frame
C                 transformation was found at time ET. XFORM and OUTFRM
C                 are valid if and only if FOUND is .TRUE.
C
C$ Parameters
C
C     See the include file zzswtchf.inc for SPICE-private parameters
C     defining sizes of buffers used by the switch frame subsystem.
C
C$ Exceptions
C
C     1)  If NDIM is a value other than 6 or 3, the error
C         SPICE(BADDIMENSION) will be signaled.
C
C     2)  If the switch frame designated by INFRM has more than MAXBAS
C         associated base frames, the error is signaled by a routine in
C         the call tree of this routine.
C
C     3)  If a frame specification cannot be found for a base frame of
C         the frame designated by INFRM, the error is signaled by a
C         routine in the call tree of this routine.
C
C     4)  If an error is found in the specification of the switch frame
C         designated by INFRM, the error is signaled by a routine in
C         the call tree of this routine.
C
C     5)  If an error occurs while computing the transformation from
C         the frame designated by INFRM to that designated by OUTFRM,
C         the error is signaled by a routine in the call tree of this
C         routine.

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
C         - Frame specifications for all base frames. Normally these
C           specifications are provided by frame kernels.
C
C     The following data may be needed:
C
C         - A leapseconds kernel
C
C         - SCLK kernel parameters and data for each CK base frame.
C           This information is normally provided by SCLK kernels.
C
C         - CK data for each CK base frame. These data are provided
C           by C-kernels.
C
C         - PCK data for each PCK base frame.
C
C         - Any kernels required to specify any dynamic base frame.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     The switch frame subsystem does not participate in fake
C     recursion, hence there are no shadow routines corresponding to
C     ZZSWFXFM and ZZSWFFET.
C
C     The only frame orientation evaluations performed in the process
C     of computing the output transformation XFORM are those that
C     don't require recursive use of the frame subsystem. The frame
C     classes in this category are
C
C        Inertial
C        PCK
C        CK
C        TK
C
C     For dynamic and switch base frames, no orientation is computed.
C     Instead, the identity matrix is returned.
C
C$ Examples
C
C     None. See the routines FRMGET and ROTGET for examples of usage.
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
C     return switch frame transformation and base frame
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

      INTEGER               LSTLED

C
C     Local parameters
C

C
C     ID code of the J2000 frame:
C
      INTEGER               J2CODE
      PARAMETER           ( J2CODE = 1 )

C
C     Size of linked list pool:
C
C     The add-only hash pool for frame IDs is singly linked.
C
      INTEGER               FPLSIZ
      PARAMETER           ( FPLSIZ = MAXFRM - LBSNGL + 1 )

C
C     Dimensions
C
      INTEGER               D3
      PARAMETER           ( D3 = 3 )

      INTEGER               D6
      PARAMETER           ( D6 = 6 )

C
C     Matrix sizes.
C
      INTEGER               SIZE33
      PARAMETER           ( SIZE33 =  9 )

      INTEGER               SIZE66
      PARAMETER           ( SIZE66 = 36 )

C
C     Local variables
C

C
C     Overview of frame storage data structures
C     =========================================
C
C     This routine uses an add-only hash to index switch frames
C     known to this routine. The hash maps input frame ID codes to
C     head nodes of collision lists stored in the array HDFRAM.
C     The collision lists are stored in the singly linked list pool
C     FRPOOL.
C
C     Each node of a collision list is the index in the array FIDLST of
C     a switch frame ID. The array FIDLST has the following associated,
C     parallel arrays:
C
C        BASBEG       Start indices in the base frame attribute arrays
C                     of values associated with specific switch frames.
C                     For example, BASLST( BASBEG(FRAMAT) ) is the
C                     index of the first base frame associated with
C                     the switch frame having ID FIDLST(FRAMAT).
C
C        BASCNT       Base count for each switch frame
C
C        USETIM       Indicates whether time intervals are associated
C                     with that frame's base frames
C
C        BINARY       Indicates whether time intervals are eligible for
C                     binary search. To be eligible, the intervals
C                     must have start times in ascending order and
C                     must be disjoint or have singleton overlap.
C
C     Base frame attributes are stored in a set of parallel arrays.
C     Attributes for bases of a given switch frame are listed in
C     contiguous elements. The arrays and their contents are:
C
C        BASLST       Base frame ID codes. The base frames of a given
C                     switch frame are listed in ascending priority
C                     order in consecutive elements of BASLST.
C
C        STARTS       Base frame interval start times, expressed as
C                     seconds past J2000 TDB. Valid for all bases of
C                     a given switch frame, or unused for that frame.
C
C        STOPS        Base frame interval start times, expressed as
C                     seconds past J2000 TDB. Valid for all bases of
C                     a given switch frame, or unused for that frame.
C
C        CLSSES       Base frame classes.
C
C        CLSIDS       Base frame class IDs.
C
C     Each base frame list for a given switch frame is ordered
C     according to ascending priority.
C
      DOUBLE PRECISION      STARTS ( MAXBAS )
      DOUBLE PRECISION      STOPS  ( MAXBAS )

      INTEGER               BASBEG ( MAXFRM )
      INTEGER               BASCNT ( MAXFRM )
      INTEGER               BASLST ( MAXBAS )
      INTEGER               CLSIDS ( MAXBAS )
      INTEGER               CLSSES ( MAXBAS )
      INTEGER               FIDLST ( MAXFRM )
      INTEGER               FRPOOL ( LBSNGL : MAXFRM )
      INTEGER               HDFRAM ( MAXFRM )

      LOGICAL               BINARY ( MAXFRM )
      LOGICAL               USETIM ( MAXFRM )

C
C     Other declarations
C
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      XIDENT ( 6, 6 )

      INTEGER               BASIDX
      INTEGER               FRAMAT
      INTEGER               FREE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LB
      INTEGER               LSTBEG
      INTEGER               POLCTR ( CTRSIZ )
      INTEGER               PRVAT
      INTEGER               PRVFRM

      LOGICAL               PASS1
      LOGICAL               SAMFRM
      LOGICAL               TIMEOK
      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE                  BASBEG
      SAVE                  BASCNT
      SAVE                  BASLST
      SAVE                  CLSIDS
      SAVE                  CLSSES
      SAVE                  BINARY
      SAVE                  FIDLST
      SAVE                  FREE
      SAVE                  FRPOOL
      SAVE                  HDFRAM
      SAVE                  PASS1
      SAVE                  POLCTR
      SAVE                  PRVAT
      SAVE                  PRVFRM
      SAVE                  STARTS
      SAVE                  STOPS
      SAVE                  USETIM
      SAVE                  XIDENT

C
C     Initial values
C
      DATA                  BASBEG / MAXFRM * 0       /
      DATA                  BASCNT / MAXFRM * 0       /
      DATA                  BASLST / MAXBAS * 0       /
      DATA                  CLSIDS / MAXBAS * 0       /
      DATA                  CLSSES / MAXBAS * 0       /
      DATA                  BINARY / MAXFRM * .FALSE. /
      DATA                  FIDLST / MAXFRM * 0       /
      DATA                  FREE   / 1                /
      DATA                  FRPOOL / FPLSIZ * 0       /
      DATA                  HDFRAM / MAXFRM * 0       /
      DATA                  PASS1  / .TRUE.           /
      DATA                  PRVAT  / 0                /
      DATA                  PRVFRM / 0                /
      DATA                  STARTS / MAXBAS * 0.D0    /
      DATA                  STOPS  / MAXBAS * 0.D0    /
      DATA                  USETIM / MAXFRM * .FALSE. /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFXFM' )

C
C     No frame found yet.
C
      FOUND = .FALSE.

      IF ( PASS1 ) THEN
C
C        Initialize pool data structures.
C
         CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                   FREE,   PRVAT,  PRVFRM, SAMFRM  )
C
C        Initialize the local pool counter.
C
         CALL ZZCTRUIN ( POLCTR )
C
C        Note: no FAILED() check is needed after the two calls above.
C
C        Create a 6x6 identity matrix.
C
         CALL FILLD( 0.D0, SIZE66, XIDENT )

         DO I = 1, 6
            XIDENT(I,I) = 1.D0
         END DO

         PASS1 = .FALSE.

      END IF

C
C     Check the transformation dimension.
C
      IF (  ( NDIM .NE. D3 ) .AND. ( NDIM .NE. D6 )  ) THEN

         CALL SETMSG ( 'Transformation dimension must '
     .   //            'be 3 or 6 but was #.'           )
         CALL ERRINT ( '#', NDIM                        )
         CALL SIGERR ( 'SPICE(BADDIMENSION)'            )
         CALL CHKOUT ( 'ZZSWFXFM'                       )
         RETURN

      END IF

C
C     Reinitialize local database if the kernel pool has been
C     updated. The call below syncs POLCTR.
C
      CALL ZZPCTRCK ( POLCTR, UPDATE )

      IF ( UPDATE ) THEN
C
C        Initialize local data structures.
C
         CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                   FREE,   PRVAT,  PRVFRM, SAMFRM  )

C
C        SAMFRM is set to .FALSE. and PRVFRM is set to 0 at this point.
C
      ELSE
C
C        SAMFRM indicates that INFRM is the same switch frame we saw on
C        the previous call, no kernel pool update has occurred since
C        that call, and that the call was successful. We don't save the
C        previous switch frame information for unsuccessful calls: for
C        those, we set PRVFRM to zero so that it won't match any valid
C        frame ID.
C
         SAMFRM = ( INFRM .NE. 0 ) .AND. ( INFRM .EQ. PRVFRM )

      END IF

      IF ( SAMFRM ) THEN
C
C        We can use information saved from the previous call. FRAMAT is
C        the index of the switch frame ID in the switch frame ID array.
C
         FRAMAT = PRVAT

      ELSE
C
C        Look up the input frame.
C
         CALL ZZHSICHK ( HDFRAM, FRPOOL, FIDLST, INFRM, FRAMAT )

      END IF

      IF ( FRAMAT .EQ. 0 ) THEN
C
C        This frame is unknown to the switch frame subsystem. Try to
C        look up the frame's specification from the kernel pool.
C        ZZSWFFET checks and returns the frame specification.
C
         CALL ZZSWFFET ( INFRM,  HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                   FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                   CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

         IF (  FAILED()  .OR.  ( FRAMAT .EQ. 0 )  ) THEN
C
C           We were unable to obtain information for the requested
C           frame. Initialize all information about the previous frame.
C           For safety, initialize the local database.
C
            CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                      FREE,   PRVAT,  PRVFRM, SAMFRM  )

            CALL CHKOUT ( 'ZZSWFXFM' )
            RETURN

         END IF

      END IF

C
C     At this point, FRAMAT is set.
C
C     Below, HEAD is the common index in each parallel array of base
C     frame data of the first item for a given switch frame. BASIDX is
C     the current index. The index obtained by adding a switch frame's
C     base frame count to HEAD is the successor of the last index of
C     base frame data for that switch frame.
C
      HEAD = BASBEG( FRAMAT )

C
C     At this point, the frame's information is present in the
C     local database. FRAMAT is the index of the frame in the hash
C     data structure. HEAD is the start index of the base frame data
C     for the input switch frame.
C
C     Search for the highest-priority frame that provides data
C     for the request time.
C
C     For safety, reset FOUND.
C
      FOUND = .FALSE.

      IF ( BINARY(FRAMAT) ) THEN
C
C        We'll do a binary search. Suppress follow-on linear searching
C        and frame data lookup by default: when BASIDX is zero, the
C        linear search and data fetch loop won't execute. We'll update
C        BASIDX if an interval is found.
C
         BASIDX = 0

C
C        Find the last interval, if any, that contains ET. Start by
C        finding the last start time that's less than or equal to ET.
C
C        Here we rely on the assumption that start and stop times for
C        each frame are stored in increasing time order.
C
         LSTBEG = LSTLED( ET, BASCNT(FRAMAT), STARTS(HEAD) )

C
C        If LSTBEG is zero, then ET is strictly less than all of the
C        start times, in which case there's no match.
C
         IF ( LSTBEG .GT. 0 ) THEN
C
C           ET could belong to the interval having start time stored at
C           relative index LSTBEG, which is equivalent to the base
C           frame array index HEAD+LSTBEG-1.
C
            IF (  ET .LE. STOPS( HEAD+LSTBEG-1 )  ) THEN
C
C              This interval contains ET. Prepare to fetch data for
C              the base frame corresponding to this interval.
C
               BASIDX = HEAD + LSTBEG - 1

            END IF

         END IF

      ELSE
C
C        Prepare for a linear search and fetching base frame data.
C
         BASIDX = HEAD + BASCNT(FRAMAT) - 1

      END IF

C
C     Search the base list in order of decreasing priority. The search
C     will terminate once we've examined the base frame at index LB,
C     if not before.
C
      LB = HEAD

      DO WHILE (  ( BASIDX .GE. LB ) .AND. ( .NOT. FOUND )  )

         IF (  USETIM( FRAMAT )  ) THEN

            TIMEOK =       ( ET .GE. STARTS(BASIDX) )
     .               .AND. ( ET .LE. STOPS (BASIDX) )
         ELSE
            TIMEOK = .FALSE.
         END IF

         IF (  TIMEOK .OR. ( .NOT. USETIM(FRAMAT) )  ) THEN
C
C           This base frame is applicable.
C
            IF ( CLSSES(BASIDX) .EQ. CK ) THEN
C
C              We can't know whether there are CK data for the request
C              time without looking up the data, so that's what we do.
C              If we find data, we return the transformation to the CK
C              frame's base frame and the frame ID of the CK frame's
C              base frame.
C
               IF ( NDIM .EQ. D6 ) THEN

                  CALL CKFXFM ( CLSIDS(BASIDX), ET,
     .                          XFORM, OUTFRM,  FOUND )
               ELSE

                  CALL CKFROT ( CLSIDS(BASIDX), ET,
     .                          XFORM, OUTFRM,  FOUND )
               END IF

               IF ( FAILED() ) THEN

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                            FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

C
C              If we got this far and we're in binary search mode, we
C              won't find a result, except possibly when ET is equal to
C              the interval start time.
C
               IF ( BINARY(FRAMAT) .AND. ( .NOT. FOUND ) ) THEN

                  IF ( ET .GT. STARTS(BASIDX) ) THEN
C
C                    The next lowest-indexed interval doesn't cover ET,
C                    so no data can be found to satisfy this request.
C
                     PRVFRM = 0
                     PRVAT  = 0

                     CALL CHKOUT ( 'ZZSWFXFM' )
                     RETURN

                  END IF

               END IF
C
C              FOUND is set. If we did find data, XFORM and OUTFRM
C              are set. If FOUND is .FALSE., we'll look at the
C              remaining lower-priority bases, if any, if we're not in
C              binary search mode. If we're in binary search mode,
C              we'll look at the next lower-priority base only if ET is
C              equal to the interval start time.
C

            ELSE IF ( CLSSES(BASIDX) .EQ. TK ) THEN

               CALL TKFRAM ( CLSIDS(BASIDX), RMAT, OUTFRM, FOUND )

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                            FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               IF ( NDIM .EQ. D6 ) THEN

                  DO I = 1,3
                     DO J = 1,3
                        XFORM(I,  J  ) = RMAT(I,J)
                        XFORM(I+3,J+3) = RMAT(I,J)
                        XFORM(I+3,J  ) = 0.0D0
                        XFORM(I,  J+3) = 0.0D0
                     END DO
                  END DO

               ELSE
                  CALL MOVED( RMAT, SIZE33, XFORM )
               END IF
C
C              OUTFRM and FOUND have been set by TKFRAM.
C

            ELSE IF ( CLSSES(BASIDX) .EQ. PCK ) THEN

               IF ( NDIM .EQ. D6 ) THEN

                  CALL TISBOD ( 'J2000', CLSIDS(BASIDX), ET, TSIPM )
                  CALL INVSTM ( TSIPM, XFORM )
               ELSE
                  CALL TIPBOD ( 'J2000', CLSIDS(BASIDX), ET, RMAT  )
                  CALL XPOSE  ( RMAT,  XFORM )
               END IF

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                            FREE,   PRVAT,  PRVFRM, SAMFRM  )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               OUTFRM = J2CODE
               FOUND  = .TRUE.


            ELSE IF ( CLSSES(BASIDX) .EQ. INERTL ) THEN

               CALL IRFROT ( BASLST(BASIDX), J2CODE, RMAT )

               IF ( FAILED() ) THEN

                  FOUND  = .FALSE.

                  CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                            FREE,   PRVAT,  PRVFRM, SAMFRM )

                  CALL CHKOUT ( 'ZZSWFXFM' )
                  RETURN

               END IF

               IF ( NDIM .EQ. D6 ) THEN

                  DO I = 1,3
                     DO J = 1,3
                        XFORM(I,  J  ) = RMAT(I,J)
                        XFORM(I+3,J+3) = RMAT(I,J)
                        XFORM(I+3,J  ) = 0.0D0
                        XFORM(I,  J+3) = 0.0D0
                     END DO
                  END DO

               ELSE
                  CALL MOVED( RMAT, SIZE33, XFORM )
               END IF

               OUTFRM = J2CODE
               FOUND  = .TRUE.

            ELSE
C
C              For other non-CK frames---switch and dynamic---we just
C              return the base frame and the identity matrix.
C
               IF ( NDIM .EQ. D6 ) THEN

                  CALL MOVED ( XIDENT, SIZE66, XFORM )
               ELSE
                  CALL IDENT ( XFORM )
               END IF

               OUTFRM = BASLST( BASIDX )
               FOUND  = .TRUE.

            END IF

         END IF

C
C        If we got this far and we're in binary search mode,
C        we've already found a result. We'll exit the loop at
C        the next loop condition test.
C
C        Check the next lower priority base frame and interval.
C
         BASIDX = BASIDX - 1

      END DO

C
C     FOUND is set.
C
C     We either found data, or we ran out of base frames to try.
C     If we found data, the outputs XFORM and OUTFRM are set.
C
      IF ( FOUND ) THEN
C
C        Save values for re-use.
C
         PRVFRM = INFRM
         PRVAT  = FRAMAT

      ELSE
C
C        Ensure the saved values won't be re-used.
C
         PRVFRM = 0

      END IF

      CALL CHKOUT ( 'ZZSWFXFM' )
      RETURN
      END

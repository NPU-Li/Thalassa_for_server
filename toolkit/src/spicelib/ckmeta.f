C$Procedure CKMETA ( CK ID to associated SCLK )

      SUBROUTINE CKMETA ( CKID,  META, IDCODE )

C$ Abstract
C
C     Return (depending upon the user's request) the ID code of either
C     the spacecraft or spacecraft clock associated with a C-Kernel ID
C     code.
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
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      INTEGER               CKID
      CHARACTER*(*)         META
      INTEGER               IDCODE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CKID       I   The ID code for some C kernel object.
C     META       I   The kind of meta data requested 'SPK' or 'SCLK'
C     IDCODE     O   The requested SCLK or spacecraft ID code.
C
C$ Detailed_Input
C
C     CKID     is the ID code for some object whose attitude
C              and possibly angular velocity are stored in
C              some C-kernel.
C
C     META     is a character string that indicates which piece
C              of meta data to fetch. Acceptable values are
C              'SCLK' and 'SPK'. The routine is case insensitive.
C              Leading and trailing blanks are insignificant.
C              However, blanks between characters are regarded
C              as being significant.
C
C$ Detailed_Output
C
C     IDCODE   if META is 'SCLK' then the value returned in IDCODE
C              is the ID code of the spacecraft clock used for
C              converting ET to TICKS and TICKS to ET for the
C              C-kernel used to represent the attitude of the
C              object with ID code CKID.
C
C              If META is 'SPK' then the value returned in IDCODE is the
C              ID code of the spacecraft on which the platform indicated
C              by CKID is mounted.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the variable META is not recognized to be one of the
C         inputs 'SPK' or 'SCLK', the error SPICE(UNKNOWNCKMETA)
C         is signaled.
C
C     2)  If CKID is greater than -1000, the associated SCLK and SPK
C         IDs must be in the kernel pool. If they are not present
C         a value of zero is returned for the requested item. Zero
C         is never the valid ID of a spacecraft clock.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine for mapping C-kernels to associated
C     spacecraft clocks.
C
C     An association of an SCLK ID and spacecraft ID with a CK frame
C     class ID may be made by placing in a text kernel the kernel
C     variable assignments
C
C        CK_<ck_frame_class_ID_code>_SCLK = <ID code of SCLK>
C        CK_<ck_frame_class_ID_code>_SPK  = <SPK ID code>
C
C     See the Frames Required Reading section on CK frames.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose you would like to look up the attitude of an object
C        in a C-kernel but have ET and seconds as your input time and
C        tolerance.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: ckmeta_ex1.tm
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
C              File name              Contents
C              --------------------   -----------------------
C              cas00071.tsc           CASSINI SCLK
C              naif0012.tls           Leapseconds
C              04153_04182ca_ISS.bc   CASSINI image navigated
C                                     spacecraft CK
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0012.tls',
C                                  'cas00071.tsc'
C                                  '04153_04182ca_ISS.bc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM CKMETA_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C        C     -- The code for CASSINI spacecraft reference frame is
C        C        -82000.
C        C
C        C     -- The reference frame we want is J2000.
C        C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF = 'J2000' )
C
C              INTEGER               CKID
C              PARAMETER           ( CKID = -82000  )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      AV     ( 3 )
C              DOUBLE PRECISION      CLKOUT
C              DOUBLE PRECISION      CMAT   ( 3, 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      ETOUT
C              DOUBLE PRECISION      SECTOL
C              DOUBLE PRECISION      TICKS
C              DOUBLE PRECISION      TICK2
C              DOUBLE PRECISION      TOL
C
C              INTEGER               IDCODE
C
C              LOGICAL               FOUND
C
C        C
C        C     Initial values.
C        C
C              DATA                  ET      / 141162208.034340D0 /
C              DATA                  SECTOL  / 0.5D0 /
C
C
C        C
C        C     First load the CK, LSK and SCLK files.
C        C
C              CALL FURNSH ( 'ckmeta_ex1.tm' )
C
C        C
C        C     Get the SCLK identifier of the spacecraft clock required
C        C     to convert from ET to TICKS.
C        C
C              CALL CKMETA ( CKID, 'SCLK', IDCODE )
C
C        C
C        C     Convert ET and ET+SECTOL to spacecraft clock ticks.
C        C
C              CALL SCE2C  ( IDCODE, ET,        TICKS  )
C              CALL SCE2C  ( IDCODE, ET+SECTOL, TICK2  )
C
C
C        C
C        C     Compute the tolerance in spacecraft clock ticks.
C        C
C              TOL = TICK2 - TICKS
C
C        C
C        C     Look the attitude up.
C        C
C              CALL CKGPAV ( CKID, TICKS, TOL,    REF,
C             .              CMAT, AV,    CLKOUT, FOUND )
C
C              WRITE(*,'(A,F20.6)')    'Input ET:            ', ET
C
C              IF ( FOUND ) THEN
C
C                 CALL SCT2E ( IDCODE, CLKOUT, ETOUT )
C                 WRITE(*,'(A,F20.6)') 'Attitude found at ET:', ETOUT
C
C              ELSE
C
C                 WRITE(*,'(A)') 'No attitude found at ET.'
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
C        Input ET:                141162208.034340
C        Attitude found at ET:    141162208.034586
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 07-JUN-2021 (JDR) (NJB)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing fragment.
C
C-    SPICELIB Version 1.2.0, 06-SEP-2013 (BVS)
C
C        BUG FIX: the POOL agents now watch both variables --
C        CK_<ID>_SCLK and CK_<ID>_SPK. Before they watched only
C        CK_<ID>_SCLK.
C
C        BUG FIX: if a previously available CK_<ID>_SCLK or CK_<ID>_SPK
C        variable that was used to populate a saved value disappears,
C        the routine now resets and returns the value based on the
C        default rule rather than keeping and returning the stale
C        POOL-based saved value.
C
C        BUG FIX: the routine now deletes watchers for the CK IDs that
C        were bumped from the local buffer.
C
C        Updated to keep track of agent-specific POOL counters and call
C        ZZCVPOOL to make use of them.
C
C-    SPICELIB Version 1.1.0, 05-MAR-2009 (NJB)
C
C        This routine now keeps track of whether its kernel pool
C        look-up failed. If so, a kernel pool lookup is attempted on
C        the next call to this routine. This change is an enhancement,
C        not a bug fix (unlike similar modifications in SCLK routines).
C
C        Header sections were put in correct order.
C
C-    SPICELIB Version 1.0.1, 09-MAR-1999 (NJB)
C
C        Comments referring to SCE2T have been updated to refer to
C        SCE2C. Occurrences of "id" replaced by "ID."
C
C-    SPICELIB Version 1.0.0, 04-OCT-1994 (WLT)
C
C-&


C$ Index_Entries
C
C     Map C-kernel ID to SCLK and SPK ID
C
C-&


C
C     SPICELIB Functions
C
      INTEGER               BSCHOI

      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NCK
      PARAMETER           ( NCK = 30 )

      INTEGER               SCLK
      PARAMETER           ( SCLK   =          1 )

      INTEGER               SPK
      PARAMETER           ( SPK    = SCLK   + 1 )

C
C     Local variables
C
      CHARACTER*(7)         BASE
      CHARACTER*(7)         MYMETA
      CHARACTER*(WDSIZE)    LOOKUP ( 2, NCK )
      CHARACTER*(WDSIZE)    AGENT  (    NCK )


      INTEGER               CKS    (    NCK )
      INTEGER               CKSORD (    NCK )
      INTEGER               CURRNT
      INTEGER               LAST
      INTEGER               N
      INTEGER               SCLKS  (    NCK )
      INTEGER               SPKS   (    NCK )
      INTEGER               THIS
      INTEGER               USRCTR ( CTRSIZ, NCK )

      LOGICAL               FIRST
      LOGICAL               FOUND  (    2   )
      LOGICAL               NODATA
      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  BASE   / 'CKMETA.' /
      DATA                  CURRNT / 0         /
      DATA                  FIRST  / .TRUE.    /
      DATA                  LAST   / 0         /
      DATA                  NODATA / .TRUE.    /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKMETA' )

      IF ( FIRST ) THEN

C
C        Initialize all agent-specific POOL counters to user value.
C
         DO N = 1, NCK
            CALL ZZCTRUIN( USRCTR( 1, N ) )
         END DO

C
C        Clear AGENTS array. We will use a non-blank AGENT value as the
C        flag to delete previously set watchers.
C
         CALL CLEARC ( NCK, AGENT )

         FIRST = .FALSE.

      END IF

C
C     Get an upper-case, left-justified copy of the metadata
C     type ('SCLK' or 'SPK').
C
      CALL LJUCRS ( 1, META, MYMETA )

C
C     See if we already have this CK ID in hand.
C
      THIS = BSCHOI ( CKID, CURRNT, CKS, CKSORD )

      IF ( THIS .GT. 0 ) THEN
C
C        We've got it.  Check to see if its value has been updated.
C        (Note that every CK ID  has its own agent and saved POOL
C        counter.)
C
         CALL ZZCVPOOL ( AGENT( THIS ), USRCTR( 1, THIS ), UPDATE )

         IF ( UPDATE .OR. NODATA ) THEN

            CALL GIPOOL( LOOKUP(SCLK,THIS), 1, 1, N,
     .                   SCLKS(THIS),       FOUND(SCLK) )

            CALL GIPOOL( LOOKUP(SPK, THIS), 1, 1, N,
     .                   SPKS (THIS),       FOUND(SPK) )

            IF ( FAILED() ) THEN

               NODATA = .TRUE.

               CALL CHKOUT ( 'CKMETA' )
               RETURN

            END IF

C
C           Note that failure to find data is not an error in this
C           routine; it's just SPICE errors that are a problem.
C
            NODATA = .FALSE.

         ELSE

C
C           The POOL variables did not change since the last check and
C           we have already buffered IDs for this CK ID. Set found
C           flags to make use of saved values.
C
            FOUND(SCLK) = .TRUE.
            FOUND(SPK)  = .TRUE.

         END IF


      ELSE

C
C        We don't have this on our handy list. Find a place to put it.
C
         IF ( CURRNT .LT. NCK ) THEN

            CURRNT = CURRNT + 1
            LAST   = CURRNT

         ELSE

            LAST   = LAST + 1

            IF ( LAST .GT. NCK ) THEN
               LAST = 1
            END IF

         END IF

         THIS = LAST

C
C        If we already have a watcher at this index, delete it. Note
C        we may have an update pending for this watcher, so we will
C        check it first to clear it.
C
         IF ( AGENT( THIS ) .NE. ' ' ) THEN
            CALL CVPOOL ( AGENT( THIS ), UPDATE )
            CALL DWPOOL ( AGENT( THIS ) )
         END IF

C
C        Recompute the order vector for the CKS; construct the
C        kernel pool variable names and the agent name.
C
         CKS(THIS) = CKID

         CALL ORDERI (  CKS,    CURRNT, CKSORD            )

         CALL INTSTR (  CKID,           LOOKUP(SCLK,THIS) )
         CALL PREFIX ( 'CK_',   0,      LOOKUP(SCLK,THIS) )

         AGENT (     THIS) =  BASE   // LOOKUP(SCLK,THIS)
         LOOKUP(SPK, THIS) =            LOOKUP(SCLK,THIS)

         CALL SUFFIX ( '_SCLK',  0,     LOOKUP(SCLK,THIS) )
         CALL SUFFIX ( '_SPK',   0,     LOOKUP(SPK, THIS) )

C
C        Set a watch for this item and fetch the current value
C        from the kernel pool (if there is a value there).
C
         CALL SWPOOL ( AGENT( THIS ), 2, LOOKUP(1,THIS) )

         CALL CVPOOL ( AGENT( THIS ), UPDATE )

         CALL GIPOOL (LOOKUP(SCLK,THIS), 1, 1, N,
     .                SCLKS(THIS),       FOUND(SCLK) )

         CALL GIPOOL (LOOKUP(SPK, THIS), 1, 1, N,
     .                SPKS (THIS),       FOUND(SPK ) )


         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'CKMETA' )
            RETURN

         END IF

C
C        Note that failure to find data is not an error in this
C        routine; it's just SPICE errors that are a problem.
C
C        At this point, kernel data checks are done.
C
         NODATA = .FALSE.

      END IF

C
C     If we didn't find either _SCLK or _SPK variable, we manufacture
C     an ID code based upon the "convention" used for all CKS so far.
C     However, the convention assumes that the CK ID will be less than
C     -1000 if it's not there is no sensible ID to return.  We return
C     zero in that case.
C
      IF ( .NOT. FOUND(SCLK) ) THEN

         IF ( CKS(THIS) .LE. -1000 ) THEN

            SCLKS(THIS) = CKS(THIS)/1000

         ELSE

            SCLKS(THIS) = 0

         END IF

      END IF

      IF ( .NOT. FOUND(SPK) ) THEN

         IF ( CKS(THIS) .LE. -1000 ) THEN

            SPKS(THIS) = CKS(THIS)/1000

         ELSE

            SPKS(THIS) = 0

         END IF

      END IF

C
C     Set output ID.
C
      IF ( MYMETA .EQ. 'SPK' ) THEN

         IDCODE = SPKS (THIS)

      ELSE IF ( MYMETA .EQ. 'SCLK' ) THEN

         IDCODE = SCLKS(THIS)

      ELSE

         IDCODE = 0

         CALL SETMSG ( 'The CK meta data item "#" is not a '
     .   //            'recognized meta data item for the '
     .   //            'routine CKMETA. The recognized value '
     .   //            'are "SPK" and "SCLK". ' )

         CALL ERRCH  ( '#', META )
         CALL SIGERR ( 'SPICE(UNKNOWNCKMETA)'  )
         CALL CHKOUT ( 'CKMETA' )
         RETURN

      END IF

      CALL CHKOUT ( 'CKMETA' )

      RETURN
      END

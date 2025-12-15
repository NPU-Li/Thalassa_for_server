C$Procedure ZZSWFINI ( Private, switch frame initialization )

      SUBROUTINE ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                      FREE,   PRVAT,  PRVFRM, SAMFRM  )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize switch frame bookkeeping data structures.
C
C     Arrays of base frame attributes are not initialized by this
C     routine.
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

      INCLUDE 'zzswtchf.inc'

      INTEGER               HDFRAM ( * )
      INTEGER               FRPOOL ( LBSNGL : * )
      INTEGER               FIDLST ( * )
      INTEGER               BASBEG ( * )
      INTEGER               FREE
      INTEGER               PRVAT
      INTEGER               PRVFRM
      LOGICAL               SAMFRM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HDFRAM     O   Array of hash collision list head nodes.
C     FRPOOL     O   Hash collision list pool.
C     FIDLST     O   Frame IDs corresponding to list nodes.
C     BASBEG     O   Start indices of base lists of switch frames.
C     FREE       O   First free index in base and time lists.
C     PRVAT      O   Index of previous input frame in frame list.
C     PRVFRM     O   Previous input frame ID.
C     SAMFRM     O   Flag indicating input frame matches previous one.
C     LBSNGL     P   Lower bound of singly linked list pool.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HDFRAM      is the initialized head node array.
C
C     FRPOOL      is the initialized hash collision list pool.
C
C     FIDLST      is the initialized switch frame ID list.
C
C     BASBEG      is the initialized base frame start index list.
C
C     FREE        is the common index of the first free entries in the
C                 arrays of base frame attributes. The index is set to
C                 1.
C
C     PRVAT       is the index of the previous ZZSWFXFM input frame
C                 in the frame list.
C
C     PRVFRM      is the frame ID of the previous ZZSWFXFM input frame.
C
C     SAMFRM      is a logical flag indicating whether the current
C                 ZZSWFXFM input frame matches the previous one.
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
C     1)  If integer hash initialization fails, the error is signaled
C         by a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine centralizes initialization of bookkeeping data
C     structures maintained by ZZSWFXFM.
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
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB)
C
C-&

C$ Index_Entries
C
C     initialize switch frame bookkeeping data structures
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFINI' )

C
C     Initialize the frame hash head node array, hash collision list
C     pool, and parallel frame ID list. Initialize the array of start
C     indices of associated base frame information. Indicate the base
C     frame data buffers are empty by setting FREE to 1.
C
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )

      IF ( FAILED() ) THEN
C
C        This code should be unreachable. It's provided for safety.
C
         CALL CHKOUT ( 'ZZSWFINI' )
         RETURN

      END IF

      CALL CLEARI ( MAXFRM, FIDLST )
      CALL CLEARI ( MAXFRM, BASBEG )

      FREE = 1

C
C     Initialize all saved frame identity information.
C
      PRVFRM = 0
      PRVAT  = 0
      SAMFRM = .FALSE.

      CALL CHKOUT ( 'ZZSWFINI' )
      RETURN
      END

C$Procedure ZZSWFCLN ( Private, switch frame clean up )

      SUBROUTINE ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Perform partial data structure clean up in response to failures
C     detected in the switch frame data fetch routine ZZSWFFET.
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
      INTEGER               BASBEG ( * )
      INTEGER               FRAMAT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HDFRAM     O   Array of hash collision list head nodes.
C     FRPOOL     O   Hash collision list pool.
C     BASBEG     O   Start indices of base lists of switch frames.
C     FRAMAT     O   Switch frame index in switch frame ID array.
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
C     BASBEG      is the initialized base frame start index list.
C
C     FRAMAT      is the initialized frame ID index. This index is
C                 set to zero, which is never valid.
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
C     1)  If integer hash initialization fails, the error will be
C         signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is essentially a macro that centralizes
C     initialization of bookkeeping data structures maintained by
C     ZZSWFFET. Only those initializations that are required by
C     ZZSWFFET are performed. See ZZSWFINI for the complete set of
C     initialization actions.
C
C$ Examples
C
C     None. See usage in ZZSWFFET.
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
C     perform switch frame clean up
C
C-&

C
C     This routine must perform its function after an error has been
C     signaled, so it does not return upon entry after a SPICE error
C     occurs.
C
      CALL CHKIN ( 'ZZSWFCLN' )

      FRAMAT = 0

C
C     Both of the following routines will execute even when a SPICE
C     error condition exists.
C
      CALL CLEARI   ( MAXFRM, BASBEG )
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )

      CALL CHKOUT ( 'ZZSWFCLN' )
      RETURN
      END

C$Procedure SPKUDS ( SPK - unpack segment descriptor )

      SUBROUTINE SPKUDS ( DESCR,
     .                    BODY,  CENTER, FRAME,  TYPE,
     .                    FIRST, LAST,   BADDRS, EADDRS )

C$ Abstract
C
C     Unpack the contents of an SPK segment descriptor.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      DESCR ( * )
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               FRAME
      INTEGER               TYPE
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               BADDRS
      INTEGER               EADDRS

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DESCR      I   An SPK segment descriptor.
C     BODY       O   The NAIF ID code for the body of the segment.
C     CENTER     O   The center of motion for BODY.
C     FRAME      O   The code for the frame of this segment.
C     TYPE       O   The type of SPK segment.
C     FIRST      O   The first epoch for which the segment is valid.
C     LAST       O   The last  epoch for which the segment is valid.
C     BADDRS     O   Beginning DAF address of the segment.
C     EADDRS     O   Ending DAF address of the segment.
C
C$ Detailed_Input
C
C     DESCR    is an SPK segment descriptor.
C
C$ Detailed_Output
C
C     BODY     is the NAIF ID code for the body of the segment.
C
C     CENTER   is the center of motion for BODY.
C
C     FRAME    is the SPICE integer code for the frame to which states
C              for the body are be referenced.
C
C     TYPE     is the type of SPK segment.
C
C     FIRST    is the first epoch for which the segment has
C              ephemeris data.
C
C     LAST     is the last epoch for which the segment has
C              ephemeris data.
C
C     BADDRS   is the starting address of the data associated
C              with this descriptor.
C
C     EADDRS   is the last address of the data associated with
C              this descriptor.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the input descriptor DESCR is invalid, it's possible for
C         the output times to contain bit patterns that don't represent
C         normal double precision values. This error is not diagnosed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine extracts the contents of an SPK segment
C     descriptor into the components needed for reading and
C     evaluating the data stored in the segment. It serves
C     as a macro for expanding the SPK segment descriptor.
C
C$ Examples
C
C     Suppose you wished to summarize a particular SPK segment
C     and that you have the descriptor for that segment in hand.
C     The following code fragment shows how you might use this
C     routine to create a summary message concerning the segment.
C
C        CALL SPKUDS ( DESCR, BODY,  CENTER, FRAME,
C       .              TYPE,  FIRST, LAST,   BADDR, EADDR )
C
C     Convert the start and stop times to ephemeris calendar strings
C
C        CALL ETCAL ( FIRST, FSTCAL )
C        CALL ETCAL ( LAST,  LSTCAL )
C
C        WRITE (*,*)
C        WRITE (*,*) 'Body     : ', BODY
C        WRITE (*,*) 'Center   : ', CENTER
C        WRITE (*,*) 'Frame ID : ', FRAME
C        WRITE (*,*) 'Data Type: ', TYPE
C        WRITE (*,*)
C        WRITE (*,*) 'Segment Start : ', FSTCAL
C        WRITE (*,*) 'Segment Stop  : ', LSTCAL
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
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 02-OCT-2021 (JDR) (NJB)
C
C        Changed output argument names "BEGIN" and "END" to "BADDRS" and
C        "EADDRS" for consistency with other routines.
C
C        Edited the header to comply with NAIF standard. Added entry
C        #1 to $Exceptions section and declared the routine error free.
C
C-    SPICELIB Version 1.0.0, 04-JAN-1994 (WLT) (KRG)
C
C-&


C$ Index_Entries
C
C     Unpack and SPK segment descriptor
C
C-&


C
C     Spicelib Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
C     Values of ND and NI for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

C
C     Local Variables
C
      DOUBLE PRECISION      DPPART ( ND )

      INTEGER               IPART  ( NI )

C
C     Standard introductory error handling preparations.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
      CALL CHKIN ( 'SPKUDS' )

C
C     No judgments are made about the descriptor when we
C     unpack it.  If things were done right when the descriptor
C     was created, it should be fine now.
C
      CALL DAFUS ( DESCR, ND, NI, DPPART, IPART )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKUDS' )
         RETURN
      END IF

      BODY   = IPART(1)
      CENTER = IPART(2)
      FRAME  = IPART(3)
      TYPE   = IPART(4)
      BADDRS = IPART(5)
      EADDRS = IPART(6)

      FIRST  = DPPART(1)
      LAST   = DPPART(2)

      CALL CHKOUT ( 'SPKUDS' )
      RETURN

      END

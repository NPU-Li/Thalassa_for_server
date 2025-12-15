C$Procedure SPKCLS ( SPK, Close file )

      SUBROUTINE SPKCLS ( HANDLE )

C$ Abstract
C
C     Close an open SPK file.
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

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of the SPK file to be closed.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of the SPK file that is to be closed.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If there are no segments in the file, the error
C         SPICE(NOSEGMENTSFOUND) is signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     Close the SPK file attached to HANDLE. The close operation tests
C     the file to ensure the presence of data segments.
C
C     A SPKCLS call should balance each call to SPKOPN.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to create an SPK type 8 kernel
C        containing only one segment, given a time-ordered set of
C        discrete states and epochs.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKCLS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 42 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK8
C              PARAMETER           ( SPK8   = 'spkcls_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
C
C              INTEGER               DEGREE
C              PARAMETER           ( DEGREE = 3  )
C
C              INTEGER               NSTATS
C              PARAMETER           ( NSTATS = 9  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      STATES ( 6, NSTATS )
C              DOUBLE PRECISION      STEP
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Set the array of discrete states to write to the SPK
C        C     segment.
C        C
C              DATA                  STATES /
C             .        101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
C             .        102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
C             .        103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
C             .        104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
C             .        105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
C             .        106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
C             .        107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
C             .        108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
C             .        109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment, and the time step separating epochs of states.
C        C
C              FIRST = 100.D0
C              LAST  = 900.D0
C              STEP  = 100.D0
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, so set to zero.
C        C
C              NCOMCH = 0
C
C        C
C        C     Internal file name and segment ID.
C        C
C              IFNAME = 'Type 8 SPK internal file name.'
C              SEGID  = 'SPK type 8 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK8, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Set the epoch of first state in STATES array to be
C        C     the start time of the interval covered by the segment.
C        C
C              BEGTIM = FIRST
C
C        C
C        C     Create a type 8 segment.
C        C
C              CALL SPKW08 (  HANDLE,  BODY,    CENTER,  REF,
C             .               FIRST,   LAST,    SEGID,   DEGREE,
C             .               NSTATS,  STATES,  BEGTIM,  STEP     )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 8 exists in
C        the output directory.
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
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     F.S. Turner        (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 28-MAY-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example, based on the SPKW08 example.
C
C        Updated $Particulars section. Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Removed DAFHLU call; replaced ERRFN call with ERRHAN.
C
C-    SPICELIB Version 1.1.0, 17-FEB-2000 (FST)
C
C        Removed the call to ZZFIXID. This will make all SPK files
C        created with future versions of the toolkit possess the
C        unambiguous ID word 'DAF/SPK '.
C
C-    SPICELIB Version 1.0.0, 27-JAN-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     close an SPK file
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 5 )
C
C     Local Variables
C
      CHARACTER*(ACCLEN)    ACCESS

      LOGICAL               FOUND

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKCLS' )

C
C     Get the access method for the file. Currently, if HANDLE < 0, the
C     access method is 'WRITE'. If HANDLE > 0, the access method is
C     'READ'.  In the future this should make use of the private entry
C     in the handle manager umbrella, ZZDDHNFO.
C
      IF ( HANDLE .LT. 0 ) THEN
         ACCESS = 'WRITE'
      ELSE IF ( HANDLE .GT. 0 ) THEN
         ACCESS = 'READ'
      END IF

C
C     If the file is open for writing and there are segments in the file
C     fix the ID word and close the file, or just close the file.
C
      IF ( ACCESS .EQ. 'WRITE' ) THEN
C
C        Check to see if there are any segments in the file. If there
C        are no segments, we signal an error. This probably indicates a
C        programming error of some sort anyway. Why would you create a
C        file and put nothing in it?
C
         CALL DAFBFS ( HANDLE )
         CALL DAFFNA ( FOUND )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'SPKCLS' )
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'No segments were found in the SPK file'
     .      //            ' ''#''. There must be at least one segment'
     .      //            ' in the file when this subroutine is'
     .      //            ' called.'                                   )
            CALL ERRHAN ( '#', HANDLE                                  )
            CALL SIGERR ( 'SPICE(NOSEGMENTSFOUND)'                     )
            CALL CHKOUT ( 'SPKCLS'                                     )
            RETURN

         END IF

      END IF
C
C     Close the file.
C
      CALL DAFCLS   ( HANDLE )
C
C     No need to check FAILED() here, since we only call spicelib
C     subroutines and return. The caller should check it though.
C
      CALL CHKOUT ( 'SPKCLS' )
      RETURN

      END

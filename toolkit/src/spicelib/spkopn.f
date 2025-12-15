C$Procedure SPKOPN ( SPK, open new file. )

      SUBROUTINE SPKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )

C$ Abstract
C
C     Create a new SPK file, returning the handle of the opened file.
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

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         IFNAME
      INTEGER               NCOMCH
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   The name of the new SPK file to be created.
C     IFNAME     I   The internal filename for the SPK file.
C     NCOMCH     I   The number of characters to reserve for comments.
C     HANDLE     O   The handle of the opened SPK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of the new SPK file to be created.
C
C     IFNAME   is the internal filename for the SPK file that is
C              being created. The internal filename may be up to 60
C              characters long. If you do not have any conventions
C              for tagging your files, an internal filename of
C              'SPK_file' is perfectly acceptable. You may also leave
C              it blank if you like.
C
C     NCOMCH   is the space, measured in characters, to be initially
C              set aside for the comment area when a new SPK file
C              is opened. The amount of space actually set aside may
C              be greater than the amount requested, due to the
C              manner in which comment records are allocated in an
C              SPK file. However, the amount of space set aside for
C              comments will always be at least the amount that was
C              requested.
C
C              The value of NCOMCH should be greater than or equal to
C              zero, i.e., 0 <= NCOMCH. A negative value, should one
C              occur, will be assumed to be zero.
C
C$ Detailed_Output
C
C     HANDLE   is the handle of the opened SPK file. If an error
C              occurs when opening the file, the value of this
C              variable should not be used, as it will not represent
C              a valid handle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the value of NCOMCH is negative, a value of zero (0) will
C         be used for the number of comment characters to be set aside
C         for comments.
C
C     2)  If an error occurs while attempting to open the SPK file, the
C         value of HANDLE will not represent a valid file handle.
C
C$ Files
C
C     See FNAME and HANDLE.
C
C$ Particulars
C
C     Open a new SPK file, reserving room for comments if requested.
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
C              PROGRAM SPKOPN_EX1
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
C              PARAMETER           ( SPK8   = 'spkopn_ex1.bsp' )
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 05-AUG-2021 (JDR)
C
C        Changed the output argument name NAME to FNAME for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code example based on SPKW08 example.
C
C        Fixed typo in $Exceptions entry #2.
C
C-    SPICELIB Version 2.0.0, 09-NOV-2006 (NJB)
C
C        Routine has been upgraded to support comment
C        area allocation using NCOMCH.
C
C-    SPICELIB Version 1.0.0, 26-JAN-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     open a new SPK file
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         TYPE
      PARAMETER           ( TYPE = 'SPK'    )

C
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

C
C     Length of a DAF comment record, in characters.
C
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )

C
C     Local variables
C
      INTEGER               NCOMR

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKOPN' )
C
C     Compute the number of comment records that we want to allocate, if
C     the number of comment characters requested is greater than zero,
C     we always allocate an extra record to account for the end of line
C     marks in the comment area.
C
C
      IF ( NCOMCH .GT. 0 ) THEN
         NCOMR = ( NCOMCH - 1 )/MXCREC  +  1
      ELSE
         NCOMR = 0
      END IF

C
C     Just do it. All of the error handling is taken care of for us.
C
      CALL DAFONW( FNAME, TYPE, ND, NI, IFNAME, NCOMR, HANDLE )

      IF ( FAILED() ) THEN
C
C        If we failed, make sure that HANDLE does not contain a value
C        that represents a valid DAF file handle.
C
         HANDLE = 0

      END IF

      CALL CHKOUT ( 'SPKOPN' )
      RETURN
      END

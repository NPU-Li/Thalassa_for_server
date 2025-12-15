C$Procedure SPKOPA ( SPK open for addition )

      SUBROUTINE SPKOPA ( FILE, HANDLE )

C$ Abstract
C
C     Open an existing SPK file for subsequent write.
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

      CHARACTER*(*)         FILE
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of an existing SPK file.
C     HANDLE     O   Handle attached to the SPK file opened to append.
C
C$ Detailed_Input
C
C     FILE     is the name of an existing SPK file to which
C              you wish to append additional SPK segments.
C
C$ Detailed_Output
C
C     HANDLE   is the DAF integer handle that refers to the SPK file
C              opened for appending. HANDLE is required by any of the
C              SPK writing routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If any of the following exceptions occur, HANDLE will be returned
C     with the value 0.
C
C     1)  If the file specified does not exist, the error
C         SPICE(FILENOTFOUND) is signaled.
C
C     2)  If the file specified is not an SPK file, the error
C         SPICE(FILEISNOTSPK) is signaled.
C
C     3)  If the specified SPK file cannot be opened for writing, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     4)  If the specified SPK file uses a non-native binary file
C         format, an error is signaled by a routine in the call tree of
C         this routine.
C
C     5)  If the specified SPK file is corrupted or otherwise invalid,
C         an error is signaled by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This file provides an interface for opening existing SPK
C     files for the addition of SPK segments. If you need
C     to open an new SPK file for writing, call the routine SPKOPN.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to add a new segment to a
C        new and to an existing SPK file.
C
C        For this example, we will first create an SPK file containing
C        only one type 5 segment, given a time-ordered set of
C        discrete states and epochs, and the gravitational parameter
C        of a central body.
C
C        Then, we will reopen the SPK and add another type 5 segment.
C        The example below shows one set of calls that you could
C        perform to make the addition. Obviously, there is no need to
C        close and re-open the file in order to add multiple segments.
C        It is done in this example to demonstrate the use of SPKOPA.
C
C        Note that you could add segments of other data types by
C        replacing the call to SPKW05 with a suitably modified call to
C        another SPK writing routine.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKOPA_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK5
C              PARAMETER           ( SPK5  = 'spkopa_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              DOUBLE PRECISION      GMSUN
C              PARAMETER           ( GMSUN  = 132712440023.310D0 )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
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
C              DOUBLE PRECISION      EPOCH1 (    NSTATS )
C              DOUBLE PRECISION      EPOCH2 (    NSTATS )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      STATES ( 6, NSTATS )
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Define the states and epochs.
C        C
C              DATA                  STATES /
C             .       101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
C             .       102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
C             .       103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
C             .       104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
C             .       105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
C             .       106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
C             .       107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
C             .       108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
C             .       109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0 /
C
C              DATA                  EPOCH1 / 100.D0, 200.D0, 300.D0,
C             .                               400.D0, 500.D0, 600.D0,
C             .                               700.D0, 800.D0, 900.D0 /
C
C              DATA                  EPOCH2 /
C             .                            1100.D0, 1200.D0, 1300.D0,
C             .                            1400.D0, 1500.D0, 1600.D0,
C             .                            1700.D0, 1800.D0, 1900.D0 /
C
C        C
C        C     Set the start and stop times of interval covered by
C        C     the first segment.
C        C
C              FIRST  = EPOCH1(1)
C              LAST   = EPOCH1(NSTATS)
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
C              IFNAME = 'Type 5 SPK internal file name.'
C              SEGID  = 'SPK type 5 test segment #1'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK5, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Write the segment.
C        C
C              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF,
C             .              FIRST,  LAST,   SEGID,  GMSUN,
C             .              NSTATS, STATES, EPOCH1        )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C        C
C        C     At this point we have an existing SPK type 5 kernel
C        C     that contains a single segment. Let's now demonstrate
C        C     the use of SPKOPA.
C        C
C        C     Open the an existing SPK file for subsequent write.
C        C
C              CALL SPKOPA ( SPK5, HANDLE )
C
C        C
C        C     Set the start and stop times of interval covered by
C        C     the second segment, and the segment ID.
C        C
C              FIRST  = EPOCH2(1)
C              LAST   = EPOCH2(NSTATS)
C
C              SEGID  = 'SPK type 5 test segment #2'
C
C        C
C        C     Now write the second segment. Use the same set of
C        C     states time-ordered set of discrete states and the
C        C     gravitational parameter. Set the epochs to be EPOCH2.
C        C
C              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF,
C             .              FIRST,  LAST,   SEGID,  GMSUN,
C             .              NSTATS, STATES, EPOCH2        )
C
C        C
C        C     Finally, close the file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 5, with two
C        segments, exists in the output directory.
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Updated the contents of $Detailed_Output and $Exceptions.
C
C-    SPICELIB Version 1.0.0, 10-MAR-1999 (WLT)
C
C-&


C$ Index_Entries
C
C     Open an existing SPK file for adding segments
C
C-&


C
C     SPICELIB Functions
C
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 8 )

C
C     Local variables
C
      CHARACTER*(SMWDSZ)    ARCH
      CHARACTER*(SMWDSZ)    TYPE

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKOPA')

C
C     Until we get a legitimate handle we set HANDLE to zero.
C
      HANDLE = 0

C
C     First make sure the file exists.
C
      IF ( .NOT. EXISTS(FILE) ) THEN

         CALL SETMSG ( 'The file ''#'' is not recognized as an '
     .   //            'existing file. ' )
         CALL ERRCH  ( '#', FILE )
         CALL SIGERR ( 'SPICE(FILENOTFOUND)'  )
         CALL CHKOUT ( 'SPKOPA' )

         RETURN
      END IF

C
C     Next make sure it is an SPK file.
C
      CALL GETFAT ( FILE, ARCH, TYPE )

      IF ( FAILED() )THEN
         CALL CHKOUT ( 'SPKOPA' )
         RETURN
      END IF

      IF ( ARCH .NE. 'DAF' .OR. TYPE .NE. 'SPK' ) THEN

         CALL SETMSG ( 'The file ''#'' was not an SPK file.  The '
     .   //            'architecture and type of the file were '
     .   //            'found to be ''#'' and ''#'' respectively. ' )

         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', ARCH )
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(FILEISNOTSPK)'  )
         CALL CHKOUT ( 'SPKOPA' )
         RETURN
      END IF

C
C     That's the limit of the checks performed here.  We let DAFOPW
C     handle the remaining checks.
C
      CALL DAFOPW ( FILE, HANDLE )

      IF ( FAILED() ) THEN
         HANDLE = 0
      END IF

      CALL CHKOUT ( 'SPKOPA' )
      RETURN

      END

C$Procedure CKOPN ( CK, open new file. )

      SUBROUTINE CKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )

C$ Abstract
C
C     Open a new CK file, returning the handle of the opened file.
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
C
C$ Keywords
C
C     CK
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
C     FNAME      I   The name of the CK file to be opened.
C     IFNAME     I   The internal filename for the CK.
C     NCOMCH     I   The number of characters to reserve for comments.
C     HANDLE     O   The handle of the opened CK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of the CK file to be opened.
C
C     IFNAME   is the internal filename for the CK file that is being
C              created. The internal filename may be up to 60
C              characters long. If you do not have any conventions
C              for tagging your files, an internal filename of
C              'CK_file' is perfectly acceptable. You may also leave
C              it blank if you like.
C
C     NCOMCH   is the space, measured in characters, to be initially
C              set aside for the comment area when a new CK file
C              is opened. The amount of space actually set aside may
C              be greater than the amount requested, due to the
C              manner in which comment records are allocated in an CK
C              file. However, the amount of space set aside for
C              comments will always be at least the amount that was
C              requested.
C
C              The value of NCOMCH should be greater than or equal to
C              zero, i.e., 0 <= NCOMCH. A negative value, should one
C              occur, will be assumed to be zero.
C
C$ Detailed_Output
C
C     HANDLE   is the handle of the opened CK file. If an error
C              occurs the value of this variable will not represent a
C              valid handle.
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
C     2)  If an error occurs while attempting to open a CK file the
C         value of HANDLE will not represent a valid file handle.
C
C$ Files
C
C     See FNAME and HANDLE.
C
C$ Particulars
C
C     Open a new CK file, reserving room for comments if requested.
C
C     A CKCLS call should balance every CKOPN call.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a CK type 3 segment; fill with data for a simple time
C        dependent rotation and angular velocity, and reserve room in
C        the CK comments area for 5000 characters.
C
C        Example code begins here.
C
C
C              PROGRAM CKOPN_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         CK3
C              PARAMETER           ( CK3 = 'ckopn_ex1.bc' )
C
C              DOUBLE PRECISION      SPTICK
C              PARAMETER           ( SPTICK = 0.001D0 )
C
C              INTEGER               INST
C              PARAMETER           ( INST = -77703 )
C
C              INTEGER               MAXREC
C              PARAMETER           ( MAXREC = 201 )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 42 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    REF
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      AVVS   (   3,MAXREC )
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      ENDTIM
C              DOUBLE PRECISION      QUATS  ( 0:3,MAXREC )
C              DOUBLE PRECISION      RATE
C              DOUBLE PRECISION      RWMAT  ( 3, 3 )
C              DOUBLE PRECISION      SPACES
C              DOUBLE PRECISION      SCLKDP (     MAXREC )
C              DOUBLE PRECISION      STARTS (    MAXREC/2)
C              DOUBLE PRECISION      STICKS
C              DOUBLE PRECISION      THETA
C              DOUBLE PRECISION      WMAT   ( 3, 3 )
C              DOUBLE PRECISION      WQUAT  ( 0:3 )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               NCOMCH
C              INTEGER               NINTS
C
C              LOGICAL               AVFLAG
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, but it reserves room for 5000 characters.
C        C
C              NCOMCH = 5000
C
C        C
C        C     The base reference from for the rotation data.
C        C
C              REF = 'J2000'
C
C        C
C        C     Time spacing in encoded ticks and in seconds
C        C
C              STICKS = 10.D0
C              SPACES = STICKS * SPTICK
C
C        C
C        C     Declare an angular rate in radians per sec.
C        C
C              RATE = 1.D-2
C
C        C
C        C     Internal file name and segment ID.
C        C
C              SEGID  = 'Test type 3 CK segment'
C              IFNAME = 'Test CK type 3 segment created by CKW03'
C
C
C        C
C        C     Open a new kernel.
C        C
C              CALL CKOPN ( CK3, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Create a 3x3 double precision identity matrix.
C        C
C              CALL IDENT ( WMAT )
C
C        C
C        C     Convert the matrix to quaternion.
C        C
C              CALL M2Q ( WMAT, WQUAT )
C
C        C
C        C     Copy the work quaternion to the first row of
C        C     QUATS.
C        C
C              CALL MOVED ( WQUAT, 4, QUATS(0,1) )
C
C        C
C        C     Create an angular velocity vector. This vector is in the
C        C     REF reference frame and indicates a constant rotation
C        C     about the Z axis.
C        C
C              CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,1) )
C
C        C
C        C     Set the initial value of the encoded ticks.
C        C
C              SCLKDP(1) = 1000.D0
C
C        C
C        C     Fill the rest of the AVVS and QUATS matrices
C        C     with simple data.
C        C
C              DO I = 2, MAXREC
C
C        C
C        C        Create the corresponding encoded tick value in
C        C        increments of STICKS with an initial value of
C        C        1000.0 ticks.
C        C
C                 SCLKDP(I) = 1000.D0 + (I-1) * STICKS
C
C        C
C        C        Create the transformation matrix for a rotation of
C        C        THETA about the Z axis. Calculate THETA from the
C        C        constant angular rate RATE at increments of SPACES.
C        C
C                 THETA = (I-1) * RATE * SPACES
C                 CALL ROTMAT ( WMAT, THETA, 3, RWMAT )
C
C        C
C        C        Convert the RWMAT matrix to SPICE type quaternion.
C        C
C                 CALL M2Q ( RWMAT, WQUAT )
C
C        C
C        C        Store the quaternion in the QUATS matrix.
C        C        Store angular velocity in AVVS.
C        C
C                 CALL MOVED ( WQUAT, 4, QUATS(0,I) )
C                 CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,I) )
C
C              END DO
C
C        C
C        C     Create an array start times for the interpolation
C        C     intervals. The end time for a particular interval is
C        C     determined as the time of the final data value prior in
C        C      time to the next start time.
C        C
C              NINTS = MAXREC/2
C              DO I = 1, NINTS
C
C                 STARTS(I) = SCLKDP(I*2 - 1)
C
C              END DO
C
C        C
C        C     Set the segment boundaries equal to the first and last
C        C     time for the data arrays.
C        C
C              BEGTIM = SCLKDP(1)
C              ENDTIM = SCLKDP(MAXREC)
C
C        C
C        C     This segment contains angular velocity.
C        C
C              AVFLAG = .TRUE.
C
C        C
C        C     All information ready to write. Write to a CK type 3
C        C     segment to the file indicated by HANDLE.
C        C
C              CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,   REF,
C             .             AVFLAG, SEGID,  MAXREC, SCLKDP, QUATS,
C             .             AVVS,   NINTS,  STARTS                )
C
C        C
C        C     SAFELY close the file.
C        C
C              CALL CKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new CK file exists in the
C        output directory.
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
C-    SPICELIB Version 2.1.0, 02-JUL-2021 (JDR)
C
C        Changed the output argument name NAME to FNAME for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard. Added
C        complete code example based on existing fragment.
C
C        Extended $Parameters section.
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
C     open a new CK file
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
      PARAMETER           ( TYPE = 'CK'    )
C
C     DAF ND and NI values for CK files.
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

      CALL CHKIN ( 'CKOPN' )
C
C     Compute the number of comment records that we want to allocate, if
C     the number of comment characters requested is greater than zero,
C     we always allocate an extra record to account for the end of line
C     marks in the comment area.
C
      IF ( NCOMCH .GT. 0 ) THEN
         NCOMR = ( NCOMCH - 1 )/MXCREC + 1
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

      CALL CHKOUT ( 'CKOPN' )
      RETURN
      END

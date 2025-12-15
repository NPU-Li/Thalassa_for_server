C$Procedure CKCLS ( CK, Close file )

      SUBROUTINE CKCLS ( HANDLE )

C$ Abstract
C
C     Close an open CK file.
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
C     None.
C
C$ Keywords
C
C     CK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of the CK file to be closed.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of the CK file that is to be closed.
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
C     None.
C
C$ Particulars
C
C     Close the CK file attached to HANDLE.
C
C     The close operation tests the file to ensure the presence of data
C     segments.
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
C              PROGRAM CKCLS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         CK2
C              PARAMETER           ( CK2 = 'ckcls_ex1.bc' )
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
C              CALL CKOPN ( CK2, IFNAME, NCOMCH, HANDLE )
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
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     F.S. Turner        (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 26-MAY-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added
C        complete code example based on existing fragment.
C
C        Re-ordered header sections and extended the $Particulars
C        section.
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Removed DAFHLU call; replaced ERRFNM call with ERRHAN.
C
C-    SPICELIB Version 1.1.0, 17-FEB-2000 (FST)
C
C        Removed the call to ZZFIXID. This will make all C-kernels
C        created with future versions of the toolkit possess the
C        unambiguous ID word 'DAF/CK  '.
C
C-    SPICELIB Version 1.0.0, 27-JAN-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     close a CK file
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
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN= 5 )
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

      CALL CHKIN ( 'CKCLS' )

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
C     Fix the ID word if the file is open for writing and close the
C     file, or just close the file.
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
            CALL CHKOUT ( 'CKCLS' )
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'No segments were found in the CK file'
     .      //            ' ''#''. There must be at least one segment'
     .      //            ' in the file when this subroutine is'
     .      //            ' called.'                                   )
            CALL ERRHAN ( '#', HANDLE                                  )
            CALL SIGERR ( 'SPICE(NOSEGMENTSFOUND)'                     )
            CALL CHKOUT ( 'CKCLS'                                      )
            RETURN

         END IF

      END IF
C
C     Close the file.
C
      CALL DAFCLS   ( HANDLE )
C
C     No need to check FAILED() here, since we just return. The caller
C     should check it though.
C
      CALL CHKOUT ( 'CKCLS' )
      RETURN

      END

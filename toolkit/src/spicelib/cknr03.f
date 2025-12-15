C$Procedure CKNR03 ( C-kernel, number of records, type 03 )

      SUBROUTINE CKNR03 ( HANDLE, DESCR, NREC )

C$ Abstract
C
C     Return the number of pointing instances in a CK type 03 segment.
C     The segment is identified by a CK file handle and segment
C     descriptor.
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
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               NREC

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the CK file containing the segment.
C     DESCR      I   The descriptor of the type 3 segment.
C     NREC       O   The number of pointing instances in the segment.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of the binary CK file containing the
C              segment. The file should have been opened for read
C              or write access, by CKLPF, DAFOPR or DAFOPW.
C
C     DESCR    is the packed descriptor of a data type 3 CK segment.
C
C$ Detailed_Output
C
C     NREC     is the number of pointing instances in the type 3
C              segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 3 segment,
C         the error SPICE(CKWRONGDATATYPE) is signaled.
C
C     2)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is signaled by a
C         routine in the call tree of this routine.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The CK file specified by HANDLE should be open for read or
C     write access.
C
C$ Particulars
C
C     For a complete description of the internal structure of a type 3
C     segment, see the CK required reading.
C
C     This routine returns the number of discrete pointing instances
C     contained in the specified segment. It is normally used in
C     conjunction with CKGR03 which returns the Ith pointing instance
C     in the segment.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following code example extracts the SCLK time, boresight
C        vector, and angular velocity vector for each pointing instance
C        in the first segment in a CK file that contains segments of
C        data type 3.
C
C        Use the CK kernel below, available in the Venus Express PDS
C        archives, as input for the code example.
C
C           VEX_BOOM_V01.BC
C
C        Example code begins here.
C
C
C              PROGRAM CKNR03_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      QUAT    ( 4 )
C              DOUBLE PRECISION      AV      ( 3 )
C              DOUBLE PRECISION      BORE    ( 3 )
C              DOUBLE PRECISION      CMAT    ( 3, 3 )
C              DOUBLE PRECISION      DCD     ( 2 )
C              DOUBLE PRECISION      DESCR   ( 5 )
C              DOUBLE PRECISION      RECORD  ( 8 )
C              DOUBLE PRECISION      SCLKDP
C
C              INTEGER               I
C              INTEGER               ICD     ( 6 )
C              INTEGER               HANDLE
C              INTEGER               NREC
C
C              LOGICAL               AVSEG
C              LOGICAL               FOUND
C
C        C
C        C     First load the file (it may also be opened by
C        C     using CKLPF).
C        C
C              CALL DAFOPR ( 'VEX_BOOM_V01.BC', HANDLE )
C
C        C
C        C     Begin forward search.  Find the first array.
C        C
C              CALL DAFBFS ( HANDLE )
C              CALL DAFFNA ( FOUND  )
C
C        C
C        C     Get segment descriptor.
C        C
C              CALL DAFGS ( DESCR )
C
C        C
C        C     Unpack the segment descriptor into its double precision
C        C     and integer components.
C        C
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C        C
C        C     The data type for a segment is located in the third
C        C     integer component of the descriptor.
C        C
C              IF ( ICD( 3 ) .EQ. 3 ) THEN
C
C        C
C        C        Does the segment contain AV data?
C        C
C                 AVSEG =  ( ICD(4) .EQ. 1 )
C
C        C
C        C        How many records does this segment contain?
C        C
C                 CALL CKNR03 ( HANDLE, DESCR, NREC )
C
C                 DO I = 1, NREC
C
C        C
C        C           Get the Ith pointing instance in the segment.
C        C
C                    CALL CKGR03 ( HANDLE, DESCR, I, RECORD )
C
C        C
C        C           Unpack RECORD into the time, quaternion, and av.
C        C
C                    SCLKDP = RECORD ( 1 )
C
C                    CALL MOVED ( RECORD(2), 4, QUAT )
C
C                    IF  ( AVSEG )  THEN
C                       CALL MOVED ( RECORD(6), 3, AV   )
C                    END IF
C
C        C
C        C           The boresight vector is the third row of the
C        C           C-matrix.
C        C
C                    CALL Q2M ( QUAT, CMAT )
C
C                    BORE(1) = CMAT(3,1)
C                    BORE(2) = CMAT(3,2)
C                    BORE(3) = CMAT(3,3)
C
C        C
C        C           Write out the results.
C        C
C                    WRITE (*,'(A,I2)') 'Record: ', I
C                    WRITE (*,'(A,F25.6)')  '   SCLK time       :',
C             .                               SCLKDP
C                    WRITE (*,'(A,3F14.9)') '   Boresight       :',
C             .                               BORE
C
C                    IF ( AVSEG ) THEN
C                       WRITE (*,'(A,3F14.9)') '   Angular velocity:',
C             .                                  AV
C                    END IF
C                    WRITE (*,*)
C
C                 END DO
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
C        Record:  1
C           SCLK time       :           2162686.710986
C           Boresight       :  -0.999122830   0.000000000   0.041875654
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  2
C           SCLK time       :       54160369751.715164
C           Boresight       :  -0.999122830   0.000000000   0.041875654
C           Angular velocity:   0.000000000   1.176083393   0.000000000
C
C        Record:  3
C           SCLK time       :       54160454948.487686
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  4
C           SCLK time       :      299264885854.937805
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  5
C           SCLK time       :     2366007685832.532227
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  6
C           SCLK time       :     4432750485810.126953
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  7
C           SCLK time       :     6505155594828.757812
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  8
C           SCLK time       :     8571898394806.352539
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record:  9
C           SCLK time       :    10638641194783.947266
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record: 10
C           SCLK time       :    12705383994761.541016
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record: 11
C           SCLK time       :    14777789103780.169922
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record: 12
C           SCLK time       :    16844531903757.763672
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C        Record: 13
C           SCLK time       :    18911274703735.359375
C           Boresight       :   0.000000000   0.000000000   1.000000000
C           Angular velocity:   0.000000000   0.000000000   0.000000000
C
C
C$ Restrictions
C
C     1)  The binary CK file containing the segment whose descriptor was
C         passed to this routine must be opened for read or write access
C         by CKLPF, DAFOPR or DAFOPW.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     J.M. Lynch         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added
C        reference to required CK in example's problem statement.
C
C        Fixed minor language issues in $Abstract, $Brief_I/O,
C        $Detailed_Input, $Files and $Restrictions sections.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&


C$ Index_Entries
C
C     number of CK type_3 records
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN


C
C     Local parameters
C
C        NDC        is the number of double precision components in an
C                   unpacked C-kernel descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel descriptor.
C
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C

      INTEGER               NDC
      PARAMETER           ( NDC = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC = 6 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE = 3 )

C
C     Local variables
C
      INTEGER               ICD    ( NIC )

      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      NPOINT




C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR03' )
      END IF


C
C     The number of discrete pointing instances contained in a data
C     type 3 segment is stored in the last double precision word of
C     the segment.  Since the address of the last word is stored in
C     the sixth integer component of the segment descriptor, it is
C     a trivial matter to extract the count.
C
C     The unpacked descriptor contains the following information
C     about the segment:
C
C        DCD(1)  Initial encoded SCLK
C        DCD(2)  Final encoded SCLK
C        ICD(1)  Instrument
C        ICD(2)  Inertial reference frame
C        ICD(3)  Data type
C        ICD(4)  Angular velocity flag
C        ICD(5)  Initial address of segment data
C        ICD(6)  Final address of segment data
C
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

C
C     If this segment is not of data type 3, then signal an error.
C

      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 3: Passed '//
     .                 'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', ICD ( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKNR03'                                       )
         RETURN
      END IF

C
C     The number of records is the final word in the segment.
C
      CALL DAFGDA ( HANDLE, ICD(6), ICD(6), NPOINT )

      NREC = NINT ( NPOINT )

      CALL CHKOUT ( 'CKNR03' )
      RETURN
      END

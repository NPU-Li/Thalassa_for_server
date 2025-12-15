C$Procedure CKNR02 ( C-kernel, number of records, type 02 )

      SUBROUTINE CKNR02 ( HANDLE, DESCR, NREC )

C$ Abstract
C
C     Return the number of pointing records in a CK type 02 segment.
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
C     DESCR      I   The descriptor of the type 2 segment.
C     NREC       O   The number of records in the segment.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of the binary CK file containing the
C              segment. The file should have been opened for read or
C              write access, by CKLPF, DAFOPR or DAFOPW.
C
C     DESCR    is the packed descriptor of a data type 2 CK segment.
C
C$ Detailed_Output
C
C     NREC     is the number of pointing records in the type 2 segment
C              associated with HANDLE and DESCR.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 2 segment,
C         the error SPICE(CKWRONGDATATYPE) is signaled.
C
C     2)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is signaled by a routine
C         in the call tree of this routine.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The CK file specified by HANDLE should be open for read or write
C     access.
C
C$ Particulars
C
C     For a complete description of the internal structure of a type 2
C     segment, see the CK required reading.
C
C     This routine returns the number of pointing records contained
C     in the specified segment. It is normally used in conjunction
C     with CKGR02, which returns the Ith record in the segment.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following code example extracts the start and end SCLK
C        time, seconds per tick rate, platform's +Z axis direction,
C        and angular velocity vector for each pointing instance in
C        the first segment in a CK file that contains segments of data
C        type 2.
C
C        Use the CK kernel below, available in the Viking Orbiter PDS
C        archives, as input for the code example.
C
C           vo2_swu_ck2.bc
C
C        Example code begins here.
C
C
C              PROGRAM CKNR02_EX1
C              IMPLICIT NONE
C
C              DOUBLE PRECISION      AV      ( 3 )
C              DOUBLE PRECISION      CMAT    ( 3, 3 )
C              DOUBLE PRECISION      DCD     ( 2  )
C              DOUBLE PRECISION      DESCR   ( 5  )
C              DOUBLE PRECISION      QUAT    ( 4 )
C              DOUBLE PRECISION      RECORD  ( 10 )
C              DOUBLE PRECISION      SCLKE
C              DOUBLE PRECISION      SCLKR
C              DOUBLE PRECISION      SCLKS
C              DOUBLE PRECISION      Z       ( 3 )
C
C              INTEGER               ICD     ( 6 )
C              INTEGER               HANDLE
C              INTEGER               NREC
C              INTEGER               I
C
C              LOGICAL               FOUND
C
C        C
C        C     First load the file. (The file may also be opened by
C        C     using CKLPF).
C        C
C              CALL DAFOPR ( 'vo2_swu_ck2.bc', HANDLE )
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
C              IF ( ICD( 3 ) .EQ. 2 ) THEN
C
C        C
C        C        How many records does this segment contain?
C        C
C                 CALL CKNR02 ( HANDLE, DESCR, NREC )
C
C                 DO I = 1, NREC
C
C        C
C        C           Get the Ith record in the segment.
C        C
C                    CALL CKGR02 ( HANDLE, DESCR, I, RECORD )
C
C        C
C        C           Unpack RECORD into the start and end time, rate in
C        C           TDB seconds/tick, quaternion, and av.
C        C
C                    SCLKS = RECORD ( 1 )
C                    SCLKE = RECORD ( 2 )
C                    SCLKR = RECORD ( 3 )
C
C                    CALL MOVED ( RECORD(4), 4, QUAT )
C                    CALL MOVED ( RECORD(8), 3, AV   )
C
C        C
C        C           The +Z axis direction is the third row of the
C        C           C-matrix.
C        C
C                    CALL Q2M ( QUAT, CMAT )
C
C                    Z(1) = CMAT(3,1)
C                    Z(2) = CMAT(3,2)
C                    Z(3) = CMAT(3,3)
C
C        C
C        C           Write out the results.
C        C
C                    WRITE (*,'(A,I2)') 'Record: ', I
C                    WRITE (*,'(A,F21.6)') '   Start encoded SCLK:',
C             .                              SCLKS
C                    WRITE (*,'(A,F21.6)') '   End encoded SCLK  :',
C             .                              SCLKE
C                    WRITE (*,'(A,F21.6)') '   TDB Seconds/tick  :',
C             .                              SCLKR
C                    WRITE (*,'(A,3F13.8)') '   +Z axis           :',
C             .                              Z
C                    WRITE (*,'(A,3F13.8)') '   Angular velocity  :',
C             .                              AV
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
C           Start encoded SCLK:   32380393707.000015
C           End encoded SCLK  :   32380395707.000015
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.62277152  -0.29420673   0.72498141
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  2
C           Start encoded SCLK:   32380402605.999947
C           End encoded SCLK  :   32380404605.999947
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.62172600  -0.27894910   0.73187716
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  3
C           Start encoded SCLK:   32380412542.000053
C           End encoded SCLK  :   32380414542.000053
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.62183610  -0.26307233   0.73764003
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  4
C           Start encoded SCLK:   32827264875.000000
C           End encoded SCLK  :   32827266875.000000
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.82984105  -0.44796078   0.33270853
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  5
C           Start encoded SCLK:   32827403805.999992
C           End encoded SCLK  :   32827405805.999992
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.80812500  -0.44911178   0.38109395
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  6
C           Start encoded SCLK:   32827412705.000042
C           End encoded SCLK  :   32827414705.000042
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.81120505  -0.43593555   0.38975193
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  7
C           Start encoded SCLK:   32827417284.000038
C           End encoded SCLK  :   32827419284.000038
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.81313834  -0.42861613   0.39382008
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  8
C           Start encoded SCLK:   33793314593.000053
C           End encoded SCLK  :   33793316593.000053
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.79860617  -0.37840751   0.46801275
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record:  9
C           Start encoded SCLK:   33793332478.000046
C           End encoded SCLK  :   33793334478.000046
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.77861783  -0.39171670   0.49021658
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 10
C           Start encoded SCLK:   33793341463.000061
C           End encoded SCLK  :   33793343463.000061
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.76852381  -0.39797437   0.50098659
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 11
C           Start encoded SCLK:   33793350363.000034
C           End encoded SCLK  :   33793352363.000034
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.75779934  -0.40484364   0.51170478
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 12
C           Start encoded SCLK:   33984028250.000000
C           End encoded SCLK  :   33984030250.000000
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.77099184  -0.39926339   0.49614546
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 13
C           Start encoded SCLK:   33984046134.999992
C           End encoded SCLK  :   33984048134.999992
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.75024440  -0.41218993   0.51694564
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 14
C           Start encoded SCLK:   33984055121.000053
C           End encoded SCLK  :   33984057121.000053
C           TDB Seconds/tick  :             0.001000
C           +Z axis           :   0.73947359  -0.41886863   0.52699894
C           Angular velocity  :   0.00000000   0.00000000   0.00000000
C
C        Record: 15
C           Start encoded SCLK:   33984220835.999966
C
C        [...]
C
C
C        Warning: incomplete output. Only 100 out of 875 lines have been
C        provided.
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Created
C        complete code example from existing code fragment.
C
C        Improved text in the $Abstract section.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&


C$ Index_Entries
C
C     number of CK type_2 records
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
C        DTYPE      is the data type.
C

      INTEGER               NDC
      PARAMETER           ( NDC   = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC   = 6 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE = 2 )

C
C     Local variables
C
      INTEGER               BEG
      INTEGER               END
      INTEGER               ARRSIZ
      INTEGER               ICD    ( NIC )

      DOUBLE PRECISION      DCD    ( NDC )




C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR02' )
      END IF


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
C     If this segment is not of data type 2, then signal an error.
C

      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 2: Passed '//
     .                 'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', ICD ( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKNR02'                                       )
         RETURN
      END IF

C
C     The beginning and ending addresses of the segment are in the
C     descriptor.
C
      BEG = ICD( 5 )
      END = ICD( 6 )

C
C     Calculate the number of pointing records in the segment from
C     the physical size of the segment and knowledge of its structure.
C
C        Based on the structure of a type 2 segment, the size of a
C        segment with N pointing intervals is given as follows:
C
C           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1)
C
C        In the above equation PSIZ is eight and integer arithmetic is
C        used.  This equation is equivalent to:
C
C
C           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2)
C                                        -------
C                                          100
C
C        If we can eliminate the integer division then, since all of
C        the other values represent whole numbers, we can solve the
C        equation for N in terms of ARRSIZ by using double precision
C        arithmetic and then rounding the result to the nearest integer.
C
C        This next equation uses double precision arithmetic and is
C        equivalent to (2):
C
C           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3)
C
C        Which means:
C
C           100 * ARRSIZ + 1     ( N-1 ) MOD 100
C           ----------------  +  ---------------   =   N          (4)
C                1001                 1001
C
C         Since the second term on the left side of (4) is always less
C         than 0.1, the first term will always round to the correct
C         value of N.
C
      ARRSIZ = END - BEG + 1

      NREC   = NINT (  ( 100.D0 * (DBLE(ARRSIZ)) + 1.D0 ) / 1001.D0  )


      CALL CHKOUT ( 'CKNR02' )
      RETURN
      END

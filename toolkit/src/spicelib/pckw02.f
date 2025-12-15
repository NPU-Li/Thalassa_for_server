C$Procedure PCKW02 ( PCK, write type 2 segment )

      SUBROUTINE PCKW02 (  HANDLE,  CLSSID,  FRAME,   FIRST,
     .                     LAST,    SEGID,   INTLEN,  N,
     .                     POLYDG,  CDATA,   BTIME          )

C$ Abstract
C
C     Write a type 2 segment to a PCK binary file given the file
C     handle, body-fixed frame class ID, frame, time range covered by
C     the segment, and the Chebyshev polynomial coefficients.
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
C     NAIF_IDS
C     SPC
C     PCK
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               CLSSID
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA (*)
      DOUBLE PRECISION      BTIME

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of binary PCK file open for writing.
C     CLSSID     I   Frame class ID of body-fixed frame.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     INTLEN     I   Length of time covered by logical record.
C     N          I   Number of logical records in segment.
C     POLYDG     I   Chebyshev polynomial degree.
C     CDATA      I   Array of Chebyshev coefficients.
C     BTIME      I   Begin time of first logical record.
C
C$ Detailed_Input
C
C     HANDLE   is the DAF handle of an PCK file to which a type 2
C              segment is to be added. The PCK file must be open
C              for writing.
C
C     CLSSID   is the frame class ID of a body-fixed reference
C              frame whose orientation is described by the
C              segment to be created.
C
C     FRAME    is the NAIF name for a reference frame relative to
C              which the orientation information for the frame
C              designated by CLSSID is specified. The frame
C              designated by FRAME is called the "base frame."
C
C     FIRST,
C     LAST     are, respectively, the start and stop times of
C              the time interval over which the segment defines
C              the orientation of body.
C
C     SEGID    is the segment identifier. A PCK segment
C              identifier may contain up to 40 characters.
C
C     INTLEN   is the length of time, in seconds, covered by each set of
C              Chebyshev polynomial coefficients (each logical record).
C              Each set of Chebyshev coefficients must cover this fixed
C              time interval, INTLEN.
C
C     N        is the number of sets of Chebyshev polynomial
C              coefficients (number of logical records)
C              to be stored in the segment. There is one set
C              of Chebyshev coefficients for each time period.
C
C     POLYDG   is the degree of each set of Chebyshev
C              polynomials.
C
C     CDATA    is an array containing all the sets of Chebyshev
C              polynomial coefficients to be contained in the
C              segment of the PCK file. The coefficients are
C              stored in CDATA in order as follows:
C
C                 the (degree + 1) coefficients for the first
C                 Euler angle of the first logical record,
C
C                 the coefficients for the second Euler angle,
C
C                 the coefficients for the third Euler angle,
C
C                 the coefficients for the first Euler angle for
C                 the second logical record, ...
C
C                 and so on.
C
C     BTIME    is the begin time (seconds past J2000 TDB) of
C              first set of Chebyshev polynomial coefficients
C              (first logical record).
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
C     1)  If the number of sets of coefficients is not positive,
C         the error SPICE(NUMCOEFFSNOTPOS) is signaled.
C
C     2)  If the interval length is not positive, the error
C         SPICE(INTLENNOTPOS) is signaled.
C
C     3)  If the integer code for the reference frame is not recognized,
C         the error SPICE(INVALIDREFFRAME) is signaled.
C
C     4)  If segment stop time is not greater then the begin time,
C         the error SPICE(BADDESCRTIMES) is signaled.
C
C     5)  If the time of the first record is not greater than or equal
C         to the descriptor begin time, the error SPICE(BADDESCRTIMES)
C         is signaled.
C
C     6)  If the end time of the last record is not greater than or
C         equal to the descriptor end time, the error
C         SPICE(BADDESCRTIMES) is signaled.
C
C$ Files
C
C     A new type 2 PCK segment is written to the PCK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an PCK type 2 data segment to the designated
C     PCK file, according to the format described in the PCK Required
C     Reading.
C
C     Each segment can contain data for only one body-fixed frame and
C     reference frame. The Chebyshev polynomial degree and length of
C     time covered by each logical record are also fixed. However, an
C     arbitrary number of logical records of Chebyshev polynomial
C     coefficients can be written in each segment. Minimizing the
C     number of segments in a PCK file will help optimize how the SPICE
C     system accesses the file.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that you have sets of Chebyshev polynomial
C        coefficients in an array pertaining to the orientation of
C        the Moon body-fixed frame with the frame class ID 301
C        relative to the J2000 reference frame, and want
C        to put these into a type 2 segment PCK file. The following
C        example could be used to add one new type 2 segment. To add
C        multiple segments, put the call to PCKW02 in a loop.
C
C
C        Example code begins here.
C
C
C              PROGRAM PCKW02_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'pckw02_ex1.bpc' )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 301  )
C
C              INTEGER               IIDLEN
C              PARAMETER           ( IIDLEN = 40   )
C
C              INTEGER               POLYDG
C              PARAMETER           ( POLYDG = 9    )
C
C              INTEGER               SZCDAT
C              PARAMETER           ( SZCDAT = 60   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(IIDLEN)    IFNAME
C              CHARACTER*(IIDLEN)    SEGID
C
C              DOUBLE PRECISION      BTIME
C              DOUBLE PRECISION      CDATA  ( SZCDAT )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      INTLEN
C              DOUBLE PRECISION      LAST
C
C              INTEGER               HANDLE
C              INTEGER               N
C              INTEGER               NRESVC
C
C        C
C        C     Set the input data: RA/DEC/W coefficients,
C        C     begin time for the first record, start/end times
C        C     for the segment, length of the time covered by
C        C     each record, and number of logical records.
C        C
C        C     CDATA contains the RA/DEC/W coefficients: first the
C        C     the POLYDEG + 1 for the RA first record, then the
C        C     POLYDEG + 1 for the DEC first record, then the
C        C     POLYDEG +1 for W first record, then the POLYDEG + 1
C        C     for the RA second record, and so on.
C        C
C              DATA                  CDATA /
C             .   -5.4242086033301107D-002, -5.2241405162792561D-005,
C             .    8.9751456289930307D-005, -1.5288696963234620D-005,
C             .    1.3218870864581395D-006,  5.9822156790328180D-007,
C             .   -6.5967702052551211D-008, -9.9084309118396298D-009,
C             .    4.9276055963541578D-010,  1.1612267413829385D-010,
C             .    0.42498898565916610D0,    1.3999219324235620D-004,
C             .   -1.8855140511098865D-005, -2.1964684808526649D-006,
C             .    1.4229817868138752D-006, -1.6991716166847001D-007,
C             .   -3.4824688140649506D-008,  2.9208428745895990D-009,
C             .    4.4217757657060300D-010, -3.9211207055305402D-012,
C             .    2565.0633504619473D0,     0.92003769451305328D0,
C             .   -8.0503797901914501D-005,  1.1960860244433900D-005,
C             .   -1.2237900518372542D-006, -5.3651349407824562D-007,
C             .    6.0843372260403005D-008,  9.0211287487688797D-009,
C             .   -4.6460429330339309D-010, -1.0446918704281774D-010,
C             .   -5.3839796353225056D-002,  4.3378021974424991D-004,
C             .    4.8130091384819459D-005, -1.2283066272873327D-005,
C             .   -5.4099296265403208D-006, -4.4237368347319652D-007,
C             .    1.3004982445546169D-007,  1.9017128275284284D-008,
C             .   -7.0368223839477803D-011, -1.7119414526133175D-010,
C             .    0.42507987850614548D0,   -7.1844899448557937D-005,
C             .   -5.1052122872412865D-005, -8.9810401387721321D-006,
C             .   -1.4611718567948972D-007,  4.0883847771062547D-007,
C             .    4.6812854485029333D-008, -4.5698075960784951D-009,
C             .   -9.8679875320349531D-010, -7.9392503778178240D-011,
C             .    2566.9029069934054D0,     0.91952244801740568D0,
C             .   -6.0426151041179828D-005,  1.0850559330577959D-005,
C             .    5.1756033678137497D-006,  4.2127585555214782D-007,
C             .   -1.1774737441872970D-007, -1.7397191490163833D-008,
C             .    5.8908810244396165D-012,  1.4594279337955166D-010 /
C
C              FIRST  =   -43200.D0
C              LAST   =  1339200.D0
C              BTIME  =  FIRST
C              INTLEN =   691200.D0
C              N      =  2
C
C        C
C        C     Open a new PCK file.  For simplicity, we will not
C        C     reserve any space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The variable IFNAME is the internal file name.
C        C
C              NRESVC  =  0
C              IFNAME  =  'Test PCK/Created 04-SEP-2019'
C
C              CALL PCKOPN ( FNAME, IFNAME, NRESVC, HANDLE    )
C
C        C
C        C     Create a segment identifier.
C        C
C              SEGID = 'MY_SAMPLE_PCK_TYPE_2_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL PCKW02 (  HANDLE, BODY,  'J2000',
C             .               FIRST,  LAST,   SEGID,   INTLEN,
C             .               N,      POLYDG, CDATA,   BTIME)
C
C        C
C        C     Close the file.
C        C
C              CALL PCKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new PCK type 2 exists in
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
C     E.D. Wright        (JPL)
C     K.S. Zukor         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 17-AUG-2021 (JDR)
C
C        Changed the input argument name BODY to CLSSID for consistency
C        with other routines, and updated its description.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing fragment.
C
C-    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to $Procedure; clean trailing whitespace.
C        Removed unneeded $Revisions section.
C
C-    SPICELIB Version 2.0.0, 01-AUG-1995 (KSZ)
C
C        The calling sequence was corrected so that REF is
C        a character string and BTIME contains only the start
C        time of the first record. Comments updated, and new
C        routine CHCKID is called to check segment identifier.
C
C-    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ)
C
C-&


C$ Index_Entries
C
C     write PCK type_2 data segment
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
C     DTYPE is the PCK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   2 )
C
C     NS is the size of a packed PCK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
C
C     ND is the number of double precision components in an PCK
C     segment descriptor. PCK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
C
C     NI is the number of integer components in an PCK segment
C     descriptor. PCK uses NI = 5.
C
      INTEGER               NI
      PARAMETER           ( NI      =   5 )
C
C     SIDLEN is the maximum number of characters allowed in an
C     PCK segment identifier.
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

C
C     Local variables
C
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR

      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RSIZE

      INTEGER               I
      INTEGER               ICD   ( NI )
      INTEGER               K
      INTEGER               NINREC
      INTEGER               REFCOD


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKW02' )
      END IF

C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of Euler angle'  //
     .                 'coefficients is not positive. N = #'       )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(NUMCOEFFSNOTPOS)'                    )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     The interval length must be positive.
C
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.'  //
     .                 'N = #'                                     )
         CALL ERRDP  ( '#', INTLEN                                 )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                       )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     Get the NAIF integer code for the reference frame.
C
      CALL IRFNUM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     The segment stop time must be greater than the begin time.
C
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than ' //
     .                 'the segment end time: #'                   )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LAST,  NETSTR                               )
         CALL ERRCH  ( '#',   NETSTR                               )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     The begin time of the first record must be less than or equal
C     to the begin time of the segment.
C
      IF ( FIRST .LT. BTIME ) THEN

         CALL SETMSG ( 'The segment descriptor start time: # is '//
     .                 'less than the beginning time of the '//
     .                 'segment data: #'                           )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( BTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      LTIME = BTIME + N * INTLEN
      IF ( LAST .GT. LTIME ) THEN

         CALL SETMSG ( 'The segment descriptor end time: # is '//
     .                 'greater than the end time of the segment '//
     .                 'data: #'  )
         CALL ETCAL  ( LAST,  ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'PCK segment identifier', SIDLEN, SEGID )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKW02' )
         RETURN
      END IF

C
C     Store the start and end times to be associated
C     with this segment.
C
      DCD(1) = FIRST
      DCD(2) = LAST

C
C     Create the integer portion of the descriptor.
C
      ICD(1) = CLSSID
      ICD(2) = REFCOD
      ICD(3) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment of PCK type 2 form:
C
C        Record 1
C        Record 2
C        ...
C        Record N
C        INIT       ( initial epoch of first record )
C        INTLEN     ( length of interval covered by each record )
C        RSIZE      ( number of data elements in each record )
C        N          ( number of records in segment )
C
C     Each record will have the form:
C
C        MID        ( midpoint of time interval )
C        RADIUS     ( radius of time interval )
C        X coefficients, Y coefficients, Z coefficients
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

C
C     Calculate the number of entries in a record.
C
      NINREC = ( POLYDG + 1 ) * 3

C
C     Fill segment with N records of data.
C
      DO I = 1, N

C
C        Calculate the midpoint and radius of the time of each
C        record, and put that at the beginning of each record.
C
         RADIUS = INTLEN / 2
         MID    = BTIME + RADIUS + ( I - 1 ) * INTLEN

         CALL DAFADA ( MID,    1)
         CALL DAFADA ( RADIUS, 1)

C
C        Put one set of coefficients into the segment.
C
         K = 1 + (I - 1) * NINREC

         CALL DAFADA ( CDATA(K), NINREC )

      END DO
C
C     Store the initial epoch of the first record.
C
      CALL DAFADA ( BTIME, 1 )

C
C     Store the length of interval covered by each record.
C
      CALL DAFADA ( INTLEN, 1 )

C
C     Store the size of each record (total number of array elements).
C
      RSIZE = 2 + NINREC
      CALL DAFADA ( RSIZE, 1 )

C
C     Store the number of records contained in the segment.
C
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )

C
C     End this segment.
C
      CALL DAFENA

      CALL CHKOUT ( 'PCKW02' )
      RETURN
      END

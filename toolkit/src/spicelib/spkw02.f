C$Procedure SPKW02 ( SPK, write segment, type 2 )

      SUBROUTINE SPKW02 (  HANDLE,  BODY,    CENTER,  FRAME,
     .                     FIRST,   LAST,    SEGID,   INTLEN,
     .                     N,       POLYDG,  CDATA,   BTIME  )

C$ Abstract
C
C     Write a type 2 segment to an SPK file.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'spk02.inc'

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
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
C     MAXDEG     P   Maximum degree of Chebyshev expansions.
C     TOLSCL     P   Scale factor used to compute time bound tolerance.
C     HANDLE     I   Handle of SPK file open for writing.
C     BODY       I   NAIF code for ephemeris object.
C     CENTER     I   NAIF code for the center of motion of the body.
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
C     HANDLE   is the DAF handle of an SPK file to which a type 2
C              segment is to be added. The SPK file must be open for
C              writing.
C
C     BODY     is the NAIF integer code for an ephemeris object whose
C              state relative to another body is described by the
C              segment to be created.
C
C     CENTER   is the NAIF integer code for the center of motion of the
C              object identified by BODY.
C
C     FRAME    is the NAIF name for a reference frame relative to which
C              the state information for BODY is specified.
C
C     FIRST,
C     LAST     are the start and stop times of the time interval over
C              which the segment defines the state of body.
C
C     SEGID    is the segment identifier. An SPK segment identifier may
C              contain up to 40 characters.
C
C     INTLEN   is the length of time, in seconds, covered by each set of
C              Chebyshev polynomial coefficients (each logical record).
C              Each set of Chebyshev coefficients must cover this fixed
C              time interval, INTLEN.
C
C     N        is the number of sets of Chebyshev polynomial
C              coefficients for coordinates (number of logical records)
C              to be stored in the segment. There is one set of
C              Chebyshev coefficients for each time period.
C
C     POLYDG   is the degree of each set of Chebyshev polynomials used
C              to represent the ephemeris information. That is, the
C              number of Chebyshev coefficients per coordinate minus
C              one. POLYDG must not exceed MAXDEG (see $Parameters
C              below).
C
C     CDATA    is a time-ordered array of N sets of Chebyshev polynomial
C              coefficients to be placed in the segment of the SPK file.
C              Each set has size SETSZ = 3*(POLYDG+1). The coefficients
C              are stored in CDATA in order as follows:
C
C                 the (POLYDG + 1) coefficients for the first
C                 coordinate of the first logical record,
C
C                 the coefficients for the second coordinate,
C
C                 the coefficients for the third coordinate,
C
C                 the coefficients for the first coordinate for
C                 the second logical record, ...
C
C                 and so on.
C
C     BTIME    is the begin time (seconds past J2000 TDB) of first set
C              of Chebyshev polynomial coefficients (first logical
C              record). FIRST is an appropriate value for BTIME.
C
C$ Detailed_Output
C
C     None.
C
C     The routine writes to the SPK file referred to by HANDLE a type 02
C     SPK segment containing the data in CDATA.
C
C     See the $Particulars section for details about the structure of a
C     type 02 SPK segment.
C
C$ Parameters
C
C     See the include file spk02.inc for declarations of the
C     parameters described below.
C
C     TOLSCL   is a tolerance scale for coverage gap at endpoints
C              of the segment coverage interval.
C
C     MAXDEG   is the maximum allowed degree of the input
C              Chebyshev expansions.
C
C$ Exceptions
C
C     1)  If the number of sets of coefficients is not positive,
C         the error SPICE(NUMCOEFFSNOTPOS) is signaled.
C
C     2)  If the interval length is not positive, the error
C         SPICE(INTLENNOTPOS) is signaled.
C
C     3)  If the name of the reference frame is not recognized,
C         the error SPICE(INVALIDREFFRAME) is signaled.
C
C     4)  If segment stop time is not greater then the begin time,
C         the error SPICE(BADDESCRTIMES) is signaled.
C
C     5)  If the start time of the first record exceeds the descriptor
C         begin time by more than a computed tolerance, or if the end
C         time of the last record precedes the descriptor end time by
C         more than a computed tolerance, the error SPICE(COVERAGEGAP)
C         is signaled. See the $Parameters section above for a
C         description of the tolerance.
C
C     6)  If the input degree POLYDG is less than 0 or greater than
C         MAXDEG, the error SPICE(INVALIDDEGREE) is signaled.
C
C     7)  If the last non-blank character of SEGID occurs past index 40,
C         or if SEGID contains any nonprintable characters, an error is
C         signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     A new type 2 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 2 data segment to the designated
C     SPK file, according to the format described in the SPK Required
C     Reading.
C
C     Each segment can contain data for only one target, central body,
C     and reference frame. The Chebyshev polynomial degree and length
C     of time covered by each logical record are also fixed. However,
C     an arbitrary number of logical records of Chebyshev polynomial
C     coefficients can be written in each segment. Minimizing the
C     number of segments in an SPK file will help optimize how the SPICE
C     system accesses the file.
C
C     The ephemeris data supplied to the type 2 SPK writer is packed
C     into an array as a sequence of records. The logical data records
C     are stored contiguously:
C
C        +----------+
C        | Record 1 |
C        +----------+
C        | Record 2 |
C        +----------+
C            ...
C        +----------+
C        | Record N |
C        +----------+
C
C     The contents of an individual record are:
C
C        +--------------------------------------+
C        | Coeff set for X position component   |
C        +--------------------------------------+
C        | Coeff set for Y position component   |
C        +--------------------------------------+
C        | Coeff set for Z position component   |
C        +--------------------------------------+
C
C     Each coefficient set has the structure:
C
C        +--------------------------------------+
C        | Coefficient of T_0                   |
C        +--------------------------------------+
C        | Coefficient of T_1                   |
C        +--------------------------------------+
C                          ...
C        +--------------------------------------+
C        | Coefficient of T_POLYDG              |
C        +--------------------------------------+
C
C     Where T_n represents the Chebyshev polynomial
C     of the first kind of degree n.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to create an SPK type 2 kernel
C        containing only one segment, given a set of Chebyshev
C        coefficients and their associated epochs.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKW02_EX1
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
C              CHARACTER*(*)         SPK2
C              PARAMETER           ( SPK2  = 'spkw02_ex1.bsp' )
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
C              INTEGER               CHBDEG
C              PARAMETER           ( CHBDEG = 2  )
C
C              INTEGER               NRECS
C              PARAMETER           ( NRECS  = 4  )
C
C              INTEGER               RECSIZ
C              PARAMETER           ( RECSIZ = 3*(CHBDEG+1) )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      INTLEN
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Define the coefficients.
C        C
C              DATA                  RECRDS /
C             .                      1.0101D0, 1.0102D0, 1.0103D0,
C             .                      1.0201D0, 1.0202D0, 1.0203D0,
C             .                      1.0301D0, 1.0302D0, 1.0303D0,
C             .                      2.0101D0, 2.0102D0, 2.0103D0,
C             .                      2.0201D0, 2.0202D0, 2.0203D0,
C             .                      2.0301D0, 2.0302D0, 2.0303D0,
C             .                      3.0101D0, 3.0102D0, 3.0103D0,
C             .                      3.0201D0, 3.0202D0, 3.0203D0,
C             .                      3.0301D0, 3.0302D0, 3.0303D0,
C             .                      4.0101D0, 4.0102D0, 4.0103D0,
C             .                      4.0201D0, 4.0202D0, 4.0203D0,
C             .                      4.0301D0, 4.0302D0, 4.0303D0 /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment, and the length of time covered by logical
C        C     record.
C        C
C              FIRST  = 100.D0
C              LAST   = 500.D0
C              INTLEN = 100.D0
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
C              IFNAME = 'Type 2 SPK internal file name.'
C              SEGID  = 'SPK type 2 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK2, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Write the segment.
C        C
C              CALL SPKW02 ( HANDLE, BODY,   CENTER, REF,
C             .              FIRST,  LAST,   SEGID,  INTLEN,
C             .              NRECS,  CHBDEG, RECRDS, FIRST  )
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
C        screen. After run completion, a new SPK type 2 exists in
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
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C     K.S. Zukor         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 20-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        example code from existing fragment.
C
C        Extended POLYDG and CDATA arguments description to provide the
C        size of the Chebyshev polynomials sets. Moved the details of
C        the SPK structure from CDATA argument description to
C        $Particulars section.
C
C-    SPICELIB Version 2.0.0, 18-JAN-2014 (NJB)
C
C        Relaxed test on relationship between the time bounds of the
C        input record set (determined by BTIME, INTLEN, and N) and the
C        descriptor bounds FIRST and LAST. Now the descriptor bounds
C        may extend beyond the time bounds of the record set by a ratio
C        computed using the parameter TOLSCL (see $Parameters above for
C        details). Added checks on input polynomial degree.
C
C-    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS)
C
C        Removed restriction that the input reference frame should be
C        inertial by changing the routine that determines the frame ID
C        from the name from IRFNUM to NAMFRM.
C
C-    SPICELIB Version 1.0.1, 24-AUG-1998 (EDW)
C
C        Changed a 2 to 2.D0 for a double precision computation. Added
C        some comments to the header. Corrected spelling mistakes.
C
C-    SPICELIB Version 1.0.0, 01-AUG-1995 (KSZ)
C
C-&


C$ Index_Entries
C
C     write SPK type_2 data segment
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
C     DTYPE is the SPK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   2 )
C
C     ND is the number of double precision components in an SPK
C     segment descriptor. SPK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
C
C     NI is the number of integer components in an SPK segment
C     descriptor. SPK uses NI = 6.
C
      INTEGER               NI
      PARAMETER           ( NI      =   6 )
C
C     NS is the size of a packed SPK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
C
C     SIDLEN is the maximum number of characters allowed in an
C     SPK segment identifier.
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
      DOUBLE PRECISION      TOL

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
      END IF

      CALL CHKIN ( 'SPKW02' )

C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of coordinate'
     .   //             'coefficients is not positive. N = #' )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(NUMCOEFFSNOTPOS)'               )
         CALL CHKOUT ( 'SPKW02'                               )
         RETURN

      END IF

C
C     Make sure that the degree of the interpolating polynomials is
C     in range.
C
      IF (  ( POLYDG .LT. 0 ) .OR. ( POLYDG .GT. MAXDEG )  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '
     .   //            'the valid degree range is [0, #].'             )
         CALL ERRINT ( '#', POLYDG                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW02'                                        )
         RETURN

      END IF

C
C     The interval length must be positive.
C
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.'
     .   //            'N = #'                               )
         CALL ERRDP  ( '#', INTLEN                           )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                 )
         CALL CHKOUT ( 'SPKW02'                              )
         RETURN

      END IF

C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', FRAME                                )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'SPKW02'                                  )
         RETURN

      END IF

C
C     The segment stop time must be greater than the begin time.
C
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than '
     .   //            'the segment end time: #'                   )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LAST,  NETSTR                               )
         CALL ERRCH  ( '#',   NETSTR                               )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW02'                                    )
         RETURN

      END IF

C
C     Compute the tolerance to use for descriptor time bound checks.
C
      TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )

      IF ( FIRST .LT. BTIME - TOL ) THEN

         CALL SETMSG ( 'The segment descriptor start time # is too '
     .   //            'much less than the beginning time of the  '
     .   //            'segment data # (in seconds past J2000: #). '
     .   //            'The difference is # seconds; the  '
     .   //            'tolerance is # seconds.'                     )
         CALL ETCAL  ( FIRST, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( BTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   FIRST                                  )
         CALL ERRDP  ( '#',   BTIME-FIRST                            )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW02'                                      )
         RETURN

      END IF

C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      LTIME = BTIME  +  ( N * INTLEN )

      IF ( LAST .GT. LTIME + TOL ) THEN

         CALL SETMSG ( 'The segment descriptor end time # is too '
     .   //            'much greater than the end time of the segment '
     .   //            'data # (in seconds past J2000: #). The '
     .   //            'difference is # seconds; the tolerance is # '
     .   //            'seconds.'                                    )
         CALL ETCAL  ( LAST,  ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( LTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   LAST                                   )
         CALL ERRDP  ( '#',   LAST-LTIME                             )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW02'                                      )
         RETURN

      END IF

C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'SPK segment identifier', SIDLEN, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW02' )
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
      ICD(1) = BODY
      ICD(2) = CENTER
      ICD(3) = REFCOD
      ICD(4) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment of SPK type 2 form:
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
         RADIUS = INTLEN / 2.D0
         MID    = BTIME + RADIUS + ( I - 1 ) * INTLEN

         CALL DAFADA ( MID,    1)
         CALL DAFADA ( RADIUS, 1)

C
C        Put one set of coefficients into segment.
C
         K = 1 + ( I - 1 ) * NINREC

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

C
C     We're done.  Checkout of error trace.
C
      CALL CHKOUT ( 'SPKW02' )
      RETURN
      END

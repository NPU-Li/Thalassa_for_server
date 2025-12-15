C$Procedure WNSUMD ( Summary of a double precision window )

      SUBROUTINE WNSUMD ( WINDOW,
     .                    MEAS,   AVG,   STDDEV,
     .                    IDXSML, IDXLON         )

C$ Abstract
C
C     Summarize the contents of a double precision window.
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
C     WINDOWS
C
C$ Keywords
C
C     WINDOWS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      WINDOW    ( LBCELL:* )
      DOUBLE PRECISION      MEAS
      DOUBLE PRECISION      AVG
      DOUBLE PRECISION      STDDEV
      INTEGER               IDXSML
      INTEGER               IDXLON

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WINDOW     I   Window to be summarized.
C     MEAS       O   Total measure of intervals in WINDOW.
C     AVG        O   Average measure.
C     STDDEV     O   Standard deviation.
C     IDXSML,
C     IDXLON     O   Locations of shortest, longest intervals.
C
C$ Detailed_Input
C
C     WINDOW   is a window containing zero or more intervals.
C
C$ Detailed_Output
C
C     MEAS     is the total measure of the intervals in the input
C              window. This is just the sum of the measures of the
C              individual intervals.
C
C     AVG      is the average of the measures of the intervals in the
C              input window.
C
C     STDDEV   is the standard deviation of the measures of the
C              intervals in the input window.
C
C     IDXSML,
C     IDXLON   are the locations of the shortest and longest intervals
C              in the input window. The shortest interval is
C
C                 [ WINDOW(IDXSML), WINDOW(IDXSML+1) ]
C
C              and the longest is
C
C                 [ WINDOW(IDXLON), WINDOW(IDXLON+1) ]
C
C              IDXSML and IDXLON are both zero if the input window
C              contains no intervals.
C
C              If WINDOW contains multiple intervals having the shortest
C              length, IDXSML is the index of the first such interval.
C              Likewise for the longest length.
C
C              Indices range from 1 to 2*N-1, where N is the number of
C              intervals in the window.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If WINDOW has odd cardinality, the error
C         SPICE(INVALIDCARDINALITY) is signaled.
C
C     2)  Left endpoints of stored intervals must be strictly greater
C         than preceding right endpoints. Right endpoints must be
C         greater than or equal to corresponding left endpoints.
C         Invalid window data are not diagnosed by this routine and may
C         lead to unpredictable results.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a summary of the input window, consisting
C     of the following items:
C
C     -  The measure of the window.
C
C     -  The average and standard deviation of the measures
C        of the individual intervals in the window.
C
C     -  The indices of the left endpoints of the shortest
C        and longest intervals in the window.
C
C     All of these quantities are zero if the window contains no
C     intervals.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Define a window with six intervals, and calculate the
C        summary for that window.
C
C
C        Example code begins here.
C
C
C              PROGRAM WNSUMD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(A,F11.6)' )
C
C              CHARACTER*(*)         FMT2
C              PARAMETER           ( FMT2   = '(A,I4)' )
C
C              CHARACTER*(*)         FMT3
C              PARAMETER           ( FMT3   = '(A,2(F6.3,A))' )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               WNSIZE
C              PARAMETER           ( WNSIZE = 12 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      AVG
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      MEAS
C              DOUBLE PRECISION      RIGHT
C              DOUBLE PRECISION      STDDEV
C              DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
C
C              INTEGER               IDXLON
C              INTEGER               IDXSML
C              INTEGER               INTLON
C              INTEGER               INTSML
C
C        C
C        C     Validate the WINDOW with size WNSIZE and zero elements.
C        C
C              CALL WNVALD ( WNSIZE, 0, WINDOW )
C
C        C
C        C     Insert the intervals
C        C
C        C        [  1,  3 ] [  7, 11 ] [ 18, 18 ]
C        C        [ 23, 27 ] [ 30, 69 ] [ 72, 80 ]
C        C
C        C     into WINDOW.
C        C
C              CALL WNINSD (  1.0D0,  3.0D0, WINDOW )
C              CALL WNINSD (  7.0D0, 11.0D0, WINDOW )
C              CALL WNINSD ( 18.0D0, 18.0D0, WINDOW )
C              CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
C              CALL WNINSD ( 30.0D0, 69.0D0, WINDOW )
C              CALL WNINSD ( 72.0D0, 80.0D0, WINDOW )
C
C        C
C        C     Calculate the summary for WINDOW.
C        C
C              CALL WNSUMD ( WINDOW, MEAS,   AVG,
C             .              STDDEV, IDXSML, IDXLON )
C
C        C
C        C     IDXSML and IDXLON refer to the indices of
C        C     the SPICE Cell data array.
C        C
C              INTSML = (IDXSML+1)/2
C              INTLON = (IDXLON+1)/2
C
C              WRITE(*,FMT1) 'Measure           : ', MEAS
C              WRITE(*,FMT1) 'Average           : ', AVG
C              WRITE(*,FMT1) 'Standard Dev      : ', STDDEV
C              WRITE(*,FMT2) 'Index shortest    : ', IDXSML
C              WRITE(*,FMT2) 'Index longest     : ', IDXLON
C              WRITE(*,FMT2) 'Interval shortest : ', INTSML
C              WRITE(*,FMT2) 'Interval longest  : ', INTLON
C
C        C
C        C     Output the shortest and longest intervals.
C        C
C              CALL WNFETD ( WINDOW, INTSML, LEFT, RIGHT )
C              WRITE(*,FMT3) 'Shortest interval : [ ', LEFT, ', ',
C             .                                        RIGHT, ' ]'
C              CALL WNFETD ( WINDOW, INTLON, LEFT, RIGHT )
C              WRITE(*,FMT3) 'Longest interval  : [ ', LEFT, ', ',
C             .                                        RIGHT, ' ]'
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Measure           :   57.000000
C        Average           :    9.500000
C        Standard Dev      :   13.413302
C        Index shortest    :    5
C        Index longest     :    9
C        Interval shortest :    3
C        Interval longest  :    5
C        Shortest interval : [ 18.000, 18.000 ]
C        Longest interval  : [ 30.000, 69.000 ]
C
C
C     2) Let A contain the intervals
C
C           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C        Let B contain the singleton intervals
C
C           [ 2, 2 ]  [ 9, 9 ]  [ 27, 27 ]
C
C        The measures of A and B are
C
C           (3-1) + (11-7) + (27-23) = 10
C
C        and
C
C           (2-2) + (9-9) + (27-27) = 0
C
C        respectively. Each window has three intervals; thus, the
C        average measures of the windows are 10/3 and 0. The standard
C        deviations are
C
C             .-----------------------------------------
C             |       2         2          2
C             |  (3-1)  + (11-7)  + (27-23)           2           1/2
C             |  ---------------------------  - (10/3)     = (8/9)
C             |             3
C           \ |
C            \|
C
C        and 0. Neither window has one "shortest" interval or "longest"
C        interval; so the first ones found are returned: IDXSML and
C        IDXLON are 1 and 3 for A, 1 and 1 for B.
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR) (NJB)
C
C        Changed output argument names SMALL and LONG to IDXSML and
C        IDXLON for consistency with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Added entry #2 in $Exceptions section.
C
C        Improved description of arguments IDXSML and IDXLON in
C        $Detailed_Output.
C
C        Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 1.1.0, 25-FEB-2009 (EDW)
C
C        Added error test to confirm input window has even cardinality.
C        Corrected section order to match NAIF standard.
C
C-    SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
C
C        Corrected error in example section: changed claimed value
C        of longest interval for window A from 2 to 3.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     summary of a d.p. window
C
C-&


C
C     SPICELIB functions
C
      INTEGER               CARDD
      LOGICAL               RETURN
      LOGICAL               EVEN


C
C     Local variables
C
      DOUBLE PRECISION      M
      DOUBLE PRECISION      SUM
      DOUBLE PRECISION      SUMSQR
      DOUBLE PRECISION      MSHORT
      DOUBLE PRECISION      MLONG

      INTEGER               I
      INTEGER               CARD


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Get the cardinality (number of endpoints) of the window.
C
      CARD = CARDD ( WINDOW )

C
C     Confirm evenness of CARD.
C
      IF ( .NOT. EVEN( CARD ) ) THEN

         CALL CHKIN  ( 'WNSUMD' )
         CALL SETMSG ( 'Input window has odd cardinality. A valid '
     .            //   'SPICE window must have even element '
     .            //   'cardinality.' )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'WNSUMD' )
         RETURN

      END IF

C
C     Trivial case: no intervals. Return all zeros.
C
      IF ( CARD .EQ. 0 ) THEN

         MEAS   = 0.D0
         AVG    = 0.D0
         STDDEV = 0.D0
         IDXSML  = 0
         IDXLON   = 0

C
C     Collect the sum of the measures and the squares of the measures
C     for each of the intervals in the window. At the same time, keep
C     track of the shortest and longest intervals encountered.
C
      ELSE

         SUM    = 0.D0
         SUMSQR = 0.D0

         IDXSML  = 1
         MSHORT = WINDOW(2) - WINDOW(1)

         IDXLON   = 1
         MLONG  = WINDOW(2) - WINDOW(1)

         DO I = 1, CARD, 2

            M       = WINDOW(I+1) - WINDOW(I)
            SUM     = SUM         + M
            SUMSQR  = SUMSQR      + M*M

            IF ( M .LT. MSHORT ) THEN
               IDXSML  = I
               MSHORT = M
            END IF

            IF ( M .GT. MLONG ) THEN
               IDXLON  = I
               MLONG = M
            END IF

         END DO

C
C        The envelope please?
C
         MEAS   = SUM
         AVG    = MEAS * 2.D0 / DBLE ( CARD )
         STDDEV = DSQRT ( ( SUMSQR * 2.D0 / DBLE ( CARD ) ) - AVG*AVG )

      END IF

      RETURN
      END

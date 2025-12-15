C$Procedure WNFETD ( Fetch an interval from a DP window )

      SUBROUTINE WNFETD ( WINDOW, N, LEFT, RIGHT )

C$ Abstract
C
C     Fetch a particular interval from a double precision window.
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

      DOUBLE PRECISION      WINDOW      ( LBCELL:* )
      INTEGER               N
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WINDOW     I   Input window.
C     N          I   Index of interval to be fetched.
C     LEFT,
C     RIGHT      O   Left, right endpoints of the Nth interval.
C
C$ Detailed_Input
C
C     WINDOW   is a window containing zero or more intervals.
C
C     N        is the index of a particular interval within the window.
C              Indices range from 1 to NINT, where NINT is the number of
C              intervals in the window (CARDD(WINDOW)/2).
C
C$ Detailed_Output
C
C     LEFT,
C     RIGHT    are the left and right endpoints of the N'th interval in
C              the input window. If the interval is not found, LEFT and
C              RIGHT are not defined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If N is less than one, the error SPICE(NOINTERVAL) is
C         signaled.
C
C     2)  If the interval does not exist, i.e. N > CARDD(WINDOW)/2, the
C         error SPICE(NOINTERVAL) is signaled.
C
C     3)  The cardinality of the input WINDOW must be even. Left
C         endpoints of stored intervals must be strictly greater than
C         preceding right endpoints. Right endpoints must be greater
C         than or equal to corresponding left endpoints. Invalid window
C         data are not diagnosed by this routine and may lead to
C         unpredictable results.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) The following code example demonstrates how to insert an
C        interval into an existing double precision SPICE window, and
C        how to loop over all its intervals to extract their left and
C        right points.
C
C
C        Example code begins here.
C
C
C              PROGRAM WNFETD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               WNSIZE
C              PARAMETER           ( WNSIZE = 10 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      RIGHT
C
C              INTEGER               I
C
C        C
C        C     Validate the window with size WNSIZE and zero elements.
C        C
C              CALL WNVALD( WNSIZE, 0, WINDOW )
C
C        C
C        C     Insert the intervals
C        C
C        C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C        C
C        C     into WINDOW.
C        C
C              CALL WNINSD(  1.D0,  3.D0, WINDOW )
C              CALL WNINSD(  7.D0, 11.D0, WINDOW )
C              CALL WNINSD( 23.D0, 27.D0, WINDOW )
C
C        C
C        C     Loop over the number of intervals in WINDOW, output
C        C     the LEFT and RIGHT endpoints for each interval.
C        C
C              DO I=1, WNCARD(WINDOW)
C
C                 CALL WNFETD( WINDOW, I, LEFT, RIGHT )
C
C                 WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
C             .                  ' [', LEFT,',',  RIGHT, ']'
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Interval 1 [   1.000,   3.000]
C        Interval 2 [   7.000,  11.000]
C        Interval 3 [  23.000,  27.000]
C
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
C-    SPICELIB Version 1.1.0, 15-MAR-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example, problem statement and solution. Added entry #3 in
C        $Exceptions section.
C
C        Improved description of argument N in $Detailed_Input.
C
C-    SPICELIB Version 1.0.2, 30-JUL-2007 (EDW)
C
C        Removed erroneous description in the $Examples section
C        indicating "Undefined" as a return state after an error
C        event caused by an invalid value of N.
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
C     fetch an interval from a d.p. window
C
C-&


C
C     SPICELIB functions
C
      INTEGER               CARDD
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               CARD
      INTEGER               END


C
C     Set up the error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'WNFETD' )

C
C
C     How many endpoints in the window? Enough? Normally, endpoints
C     of the Nth interval are stored in elements 2N and 2N-1.
C
      CARD = CARDD ( WINDOW )
      END  = 2*N

      IF ( N .LT. 1  .OR.  CARD .LT. END ) THEN
         CALL SETMSG ( 'WNFETD: No such interval.' )
         CALL SIGERR ( 'SPICE(NOINTERVAL)' )
      ELSE
         LEFT  = WINDOW(END-1)
         RIGHT = WINDOW(END  )
      END IF

      CALL CHKOUT ( 'WNFETD' )

      RETURN
      END

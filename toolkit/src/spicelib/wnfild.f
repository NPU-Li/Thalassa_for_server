C$Procedure WNFILD ( Fill small gaps in a DP window )

      SUBROUTINE WNFILD ( SMLGAP, WINDOW )

C$ Abstract
C
C     Fill small gaps between adjacent intervals of a double precision
C     window.
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

      DOUBLE PRECISION      SMLGAP
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SMLGAP     I   Limiting measure of small gaps.
C     WINDOW    I-O  Window to be filled.
C
C$ Detailed_Input
C
C     SMLGAP   is the limiting measure of the small gaps to be filled.
C              Adjacent intervals separated by gaps of measure less than
C              or equal to SMLGAP are merged. The measure SMLGAP is
C              signed, and is used as is---the absolute value of SMLGAP
C              is not used for in place of negative input values.
C
C     WINDOW   on input, is a window containing zero or more
C              intervals.
C
C$ Detailed_Output
C
C     WINDOW   on output, is the original window, after adjacent
C              intervals separated by small gaps have been merged.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  The cardinality of the input WINDOW must be even. Left
C         endpoints of stored intervals must be strictly greater than
C         preceding right endpoints. Right endpoints must be greater
C         than or equal to corresponding left endpoints. Invalid window
C         data are not diagnosed by this routine and may lead to
C         unpredictable results.
C
C     2)  If SMLGAP is less than or equal to zero, this routine has
C         no effect on the window.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine removes small gaps between adjacent intervals
C     by merging intervals separated by gaps of measure less than
C     or equal to the limiting measure SMLGAP.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Given a double precision window, containing the following four
C        intervals:
C
C           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
C
C        merge any adjacent intervals separated by a gap equal to or
C        less than 3.0.
C
C
C        Example code begins here.
C
C
C              PROGRAM WNFILD_EX1
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
C              CHARACTER*(*)         FMT
C              PARAMETER           ( FMT    = '(A,I2,A,2(F7.3,A))' )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               WNSIZE
C              PARAMETER           ( WNSIZE = 10 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      RIGHT
C              DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
C
C              INTEGER               I
C
C        C
C        C     Validate the WINDOW with size WNSIZE and zero elements.
C        C
C              CALL WNVALD ( WNSIZE, 0, WINDOW )
C
C        C
C        C     Insert the intervals
C        C
C        C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
C        C
C        C     into WINDOW.
C        C
C              CALL WNINSD ( 1.0D0,  3.0D0,  WINDOW )
C              CALL WNINSD ( 7.0D0,  11.0D0, WINDOW )
C              CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
C              CALL WNINSD ( 29.0D0, 29.0D0, WINDOW )
C
C        C
C        C     Loop over the number of intervals in WINDOW, output
C        C     the LEFT and RIGHT endpoints for each interval.
C        C
C              WRITE(*,*) 'Initial WINDOW:'
C
C              DO I= 1, WNCARD( WINDOW )
C
C                 CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
C
C                 WRITE(*,FMT) '   Interval ', I, ': [',
C             .                LEFT, ',', RIGHT, ' ]'
C
C              END DO
C
C        C
C        C     Fill the gaps smaller than or equal to 3.0
C        C
C              CALL WNFILD ( 3.0D0, WINDOW )
C
C        C
C        C     Output the intervals.
C        C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Window after filling gaps <= 3.0:'
C
C              DO I= 1, WNCARD( WINDOW )
C
C                 CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
C
C                 WRITE(*,FMT) '   Interval ', I, ': [',
C             .                LEFT, ',', RIGHT, ' ]'
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
C         Initial WINDOW:
C           Interval  1: [  1.000,  3.000 ]
C           Interval  2: [  7.000, 11.000 ]
C           Interval  3: [ 23.000, 27.000 ]
C           Interval  4: [ 29.000, 29.000 ]
C
C         Window after filling gaps <= 3.0:
C           Interval  1: [  1.000,  3.000 ]
C           Interval  2: [  7.000, 11.000 ]
C           Interval  3: [ 23.000, 29.000 ]
C
C
C     2) Using the same window from the first example:
C
C           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
C
C        Then the following series of calls
C
C           CALL WNFILD (  1.D0, WINDOW )                           (1)
C           CALL WNFILD (  2.D0, WINDOW )                           (2)
C           CALL WNFILD (  3.D0, WINDOW )                           (3)
C           CALL WNFILD ( 12.D0, WINDOW )                           (4)
C
C        produces the following series of windows
C
C           [1.0,  3.0]  [7.0, 11.0]  [23.0, 27.0]  [29.0, 29.0]    (1)
C           [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (2)
C           [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (3)
C           [1.0, 29.0]                                             (4)
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) (NJB)
C
C        Changed the argument name SMALL to SMLGAP for consistency with
C        other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code example. Added entry #1 and #2 in
C        $Exceptions section. Extended SMLGAP description in
C        $Detailed_Input.
C
C        Updated code to remove unnecessary lines of code in the
C        Standard SPICE error handling CHKIN statements.
C
C-    SPICELIB Version 1.0.3, 29-JUL-2007 (NJB)
C
C        Corrected typo in the previous Version line date string,
C        "29-JUL-20022" to "29-JUL-2002."
C
C-    SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
C
C        Changed gap size from 10 to 12 to correct erroneous example.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
C
C-&


C$ Index_Entries
C
C     fill small gaps in a d.p. window
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
      INTEGER               I
      INTEGER               J

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'WNFILD' )

C
C     Get the cardinality of the window. (The size is not important;
C     this routine can't create any new intervals.)
C
      CARD = CARDD ( WINDOW )

C
C     Step through the window, looking for the next right endpoint
C     more than SMLGAP away from the following left endpoint. This marks
C     the end of the new first interval, and the beginning of the new
C     second interval. Keep this up until the last right endpoint has
C     been reached. This remains the last right endpoint.
C
      IF ( CARD .GT. 0 ) THEN

         I = 2
         J = 2

         DO WHILE ( J .LT. CARD )

            IF ( ( WINDOW(J) + SMLGAP )  .LT. WINDOW(J+1) ) THEN
               WINDOW(I  ) = WINDOW(J  )
               WINDOW(I+1) = WINDOW(J+1)
               I           = I + 2
            END IF

            J = J + 2

         END DO

         WINDOW(I) = WINDOW(J)
         CALL SCARDD ( I, WINDOW )

      END IF

      CALL CHKOUT ( 'WNFILD' )
      RETURN
      END

C$Procedure WNINSD ( Insert an interval into a DP window )

      SUBROUTINE WNINSD ( LEFT, RIGHT, WINDOW )

C$ Abstract
C
C     Insert an interval into a double precision window.
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

      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WINDOW    ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LEFT,
C     RIGHT      I   Left, right endpoints of new interval.
C     WINDOW    I-O  Input, output window.
C
C$ Detailed_Input
C
C     LEFT,
C     RIGHT    are the left and right endpoints of the interval to be
C              inserted.
C
C     WINDOW   on input, is a SPICE window containing zero or more
C              intervals.
C
C$ Detailed_Output
C
C     WINDOW   on output, is the original window following the insertion
C              of the interval from LEFT to RIGHT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS)
C         is signaled.
C
C     2)  If the insertion of the interval causes an excess of elements,
C         the error SPICE(WINDOWEXCESS) is signaled.
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
C     This routine inserts the interval from LEFT to RIGHT into the
C     input window. If the new interval overlaps any of the intervals
C     in the window, the intervals are merged. Thus, the cardinality
C     of the input window can actually decrease as the result of an
C     insertion. However, because inserting an interval that is
C     disjoint from the other intervals in the window can increase the
C     cardinality of the window, the routine signals an error.
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
C              PROGRAM WNINSD_EX1
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
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 25-AUG-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement.
C
C        Updated to remove unnecessary lines of code in the
C        Standard SPICE error handling CHKIN statements.
C
C        Edited the header to comply to NAIF standard. Added complete
C        code example, problem statement and solution. Added entry #3 in
C        $Exceptions section.
C
C        Removed irrelevant information related to other unary window
C        routines from $Particulars section.
C
C-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
C
C        There was a bug when moving the intervals in the cell
C        to the right when inserting a new interval to the left
C        of the left most interval. The incrementing in the DO
C        loop was incorrect.
C
C        The loop used to read:
C
C           DO J = I-1, CARD
C              WINDOW(J+2) = WINDOW(J)
C           END DO
C
C        which squashed everything to the right of the first interval
C        with the values of the first interval.
C
C        The loop now reads:
C
C           DO J = CARD, I-1, -1
C              WINDOW(J+2) = WINDOW(J)
C           END DO
C
C        which correctly scoots the elements in reverse order,
C        preserving their values.
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
C     insert an interval into a d.p. window
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
C
C        There was a bug when moving the intervals in the cell
C        to the right when inserting a new interval to the left
C        of the left most interval. the incrementing in the DO
C        loop was incorrect.
C
C        The loop used to read:
C
C           DO J = I-1, CARD
C              WINDOW(J+2) = WINDOW(J)
C           END DO
C
C        which squashed everything to the right of the first interval
C        with the values of the first interval.
C
C        The loop now reads:
C
C           DO J = CARD, I-1, -1
C              WINDOW(J+2) = WINDOW(J)
C           END DO
C
C        which correctly scoots the elements in reverse order,
C        preserving their values.
C
C-    Beta Version 1.2.0, 27-FEB-1989 (HAN)
C
C        Due to the calling sequence and functionality changes
C        in the routine EXCESS, the method of signaling an
C        excess of elements needed to be changed.
C
C-    Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
C
C        Contents of the $Required_Reading section was
C        changed from "None." to "WINDOWS".  Also, the
C        declaration of the unused variable K was removed.
C
C-&


C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN

C
C     Local Variables
C
      INTEGER               SIZE
      INTEGER               CARD

      INTEGER               I
      INTEGER               J



C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'WNINSD' )

C
C     Get the size and cardinality of the window.
C
      SIZE = SIZED ( WINDOW )
      CARD = CARDD ( WINDOW )

C
C     Let's try the easy cases first. No input interval? No change.
C     Signal that an error has occurred and set the error message.
C
      IF ( LEFT .GT. RIGHT ) THEN
         CALL SETMSG ( 'Left endpoint was *. Right endpoint was *.' )
         CALL ERRDP  ( '*', LEFT  )
         CALL ERRDP  ( '*', RIGHT )
         CALL SIGERR ( 'SPICE(BADENDPOINTS)' )
         CALL CHKOUT ( 'WNINSD' )
         RETURN

C
C     Empty window? Input interval later than the end of the window?
C     Just insert the interval, if there's room.
C
      ELSE IF ( CARD .EQ. 0  .OR.  LEFT .GT. WINDOW(CARD) ) THEN

         IF ( SIZE .GE. CARD+2 ) THEN

            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(CARD+1) = LEFT
            WINDOW(CARD+2) = RIGHT

         ELSE

            CALL EXCESS ( 2, 'window' )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )

         END IF

         CALL CHKOUT ( 'WNINSD' )
         RETURN

      END IF


C
C     Now on to the tougher cases.
C
C     Skip intervals which lie completely to the left of the input
C     interval. (The index I will always point to the right endpoint
C     of an interval).
C
      I = 2

      DO WHILE ( I .LE. CARD  .AND.  WINDOW(I) .LT. LEFT )
         I = I + 2
      END DO

C
C     There are three ways this can go. The new interval can:
C
C        1) lie entirely between the previous interval and the next.
C
C        2) overlap the next interval, but no others.
C
C        3) overlap more than one interval.
C
C     Only the first case can possibly cause an overflow, since the
C     other two cases require existing intervals to be merged.
C

C
C     Case (1). If there's room, move succeeding intervals back and
C     insert the new one. If there isn't room, signal an error.
C
      IF ( RIGHT .LT. WINDOW(I-1) ) THEN

         IF ( SIZE .GE. CARD+2 ) THEN

            DO J = CARD, I-1, -1
               WINDOW(J+2) = WINDOW(J)
            END DO

            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(I-1) = LEFT
            WINDOW(I  ) = RIGHT

         ELSE

            CALL EXCESS ( 2, 'window' )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )
            CALL CHKOUT ( 'WNINSD' )
            RETURN

         END IF

C
C     Cases (2) and (3).
C
      ELSE

C
C        The left and right endpoints of the new interval may or
C        may not replace the left and right endpoints of the existing
C        interval.
C
         WINDOW(I-1) = MIN ( LEFT,  WINDOW(I-1) )
         WINDOW(I  ) = MAX ( RIGHT, WINDOW(I  ) )

C
C        Skip any intervals contained in the one we modified.
C        (Like I, J always points to the right endpoint of an
C        interval.)
C
         J = I + 2

         DO WHILE ( J .LE. CARD  .AND.  WINDOW(J) .LE. WINDOW(I) )
            J = J + 2
         END DO

C
C        If the modified interval extends into the next interval,
C        merge the two. (The modified interval grows to the right.)
C
         IF ( J .LE. CARD  .AND.  WINDOW(I) .GE. WINDOW(J-1) ) THEN
            WINDOW(I) = WINDOW(J)
            J         = J + 2
         END IF

C
C        Move the rest of the intervals forward to take up the
C        spaces left by the absorbed intervals.
C
         DO WHILE ( J .LE. CARD )
            I           = I + 2
            WINDOW(I-1) = WINDOW(J-1)
            WINDOW(I  ) = WINDOW(J  )
            J           = J + 2
         END DO

         CALL SCARDD ( I, WINDOW )

      END IF

      CALL CHKOUT ( 'WNINSD' )
      RETURN
      END

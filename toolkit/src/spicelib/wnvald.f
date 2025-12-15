C$Procedure WNVALD ( Validate a DP window )

      SUBROUTINE WNVALD ( SIZE, N, WINDOW )

C$ Abstract
C
C     Form a valid double precision window from the contents
C     of a window array.
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

      INTEGER               SIZE
      INTEGER               N
      DOUBLE PRECISION      WINDOW ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SIZE       I   Size of window.
C     N          I   Original number of endpoints.
C     WINDOW    I-O  Input, output window.
C
C$ Detailed_Input
C
C     SIZE     is the size of the window to be validated. This is the
C              maximum number of endpoints that the cell used to
C              implement the window is capable of holding at any one
C              time.
C
C     N        is the original number of endpoints in the input cell.
C
C     WINDOW   on input is a (possibly uninitialized) cell array of
C              maximum size SIZE containing N endpoints of (possibly
C              unordered and non-disjoint) intervals.
C
C$ Detailed_Output
C
C     WINDOW   on output is a validated window, in which any overlapping
C              input intervals have been merged and the resulting set of
C              intervals is arranged in increasing order.
C
C              WINDOW is ready for use with any of the window routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the original number of endpoints N is odd, the error
C         SPICE(UNMATCHENDPTS) is signaled.
C
C     2)  If the original number of endpoints of the window exceeds its
C         size, the error SPICE(WINDOWTOOSMALL) is signaled.
C
C     3)  If the left endpoint is greater than the right endpoint, the
C         error SPICE(BADENDPOINTS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine takes as input a cell array containing pairs of
C     endpoints and validates it to form a window.
C
C     On input, WINDOW is a cell of size SIZE containing N endpoints.
C     During validation, the intervals are ordered, and overlapping
C     intervals are merged. On output, the cardinality of WINDOW is
C     the number of endpoints remaining, and it is ready for use with
C     any of the window routines.
C
C     Because validation is done in place, there is no chance of
C     overflow.
C
C     Validation is primarily useful for ordering and merging
C     intervals read from input files or initialized in DATA
C     statements.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Define an array containing a set of unordered and possibly
C        overlapping intervals, and validate the array as a SPICE
C        window.
C
C
C        Example code begins here.
C
C
C              PROGRAM WNVALD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER               CARDD
C              INTEGER               SIZED
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               WINSIZ
C              PARAMETER           ( WINSIZ = 20 )
C
C              INTEGER               DATSIZ
C              PARAMETER           ( DATSIZ = 16 )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      WINDOW  ( LBCELL : WINSIZ )
C              DOUBLE PRECISION      WINDAT  ( DATSIZ )
C
C              INTEGER               I
C
C
C              DATA                  WINDAT  /  0,  0,
C             .                                10, 12,
C             .                                 2,  7,
C             .                                13, 15,
C             .                                 1,  5,
C             .                                23, 29,  4*0 /
C
C
C        C
C        C     Insert the data into the SPICE cell array.
C        C
C              CALL MOVED ( WINDAT, WINSIZ, WINDOW(1) )
C
C        C
C        C     Validate the input WINDOW array as a SPICE window.
C        C
C              CALL WNVALD ( WINSIZ, DATSIZ, WINDOW )
C
C              WRITE (*,*) 'Current intervals: ', CARDD ( WINDOW ) / 2
C              WRITE (*,*) 'Maximum intervals: ', SIZED ( WINDOW ) / 2
C              WRITE (*,*)
C              WRITE (*,*) 'Intervals:'
C              WRITE (*,*)
C
C              DO I = 1, CARDD ( WINDOW ), 2
C                 WRITE (*,*) WINDOW(I), WINDOW(I+1)
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Current intervals:            5
C         Maximum intervals:           10
C
C         Intervals:
C
C           0.0000000000000000        0.0000000000000000
C           1.0000000000000000        7.0000000000000000
C           10.000000000000000        12.000000000000000
C           13.000000000000000        15.000000000000000
C           23.000000000000000        29.000000000000000
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 16-MAR-2021 (JDR)
C
C        Changed argument name A to WINDOW for consistency with other
C        routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply to NAIF standard. Created complete
C        code example from code fragment and added example's problem
C        statement.
C
C        Improved description of argument WINDOW in $Detailed_Output.
C
C        Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 1.1.1, 30-JUL-2002 (NJB)
C
C        Fixed bugs in example program.
C
C-    SPICELIB Version 1.1.0, 14-AUG-1995 (HAN)
C
C        Fixed a character string that continued over two lines.
C        The "//" characters were missing. The Alpha/OpenVMS compiler
C        issued a warning regarding this incorrect statement syntax.
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
C     validate a d.p. window
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               ODD
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      INTEGER               I

C
C     Setting up error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'WNVALD' )

C
C     First, some error checks. The number of endpoints must be even,
C     and smaller than the reported size of the window.
C
      IF ( ODD ( N ) ) THEN

         CALL SETMSG ( 'WNVALD: Unmatched endpoints' )
         CALL SIGERR ( 'SPICE(UNMATCHENDPTS)' )
         CALL CHKOUT ( 'WNVALD' )
         RETURN

      ELSE IF ( N .GT. SIZE ) THEN

         CALL SETMSG ( 'WNVALD: Inconsistent value for SIZE.' )
         CALL SIGERR ( 'SPICE(WINDOWTOOSMALL)' )
         CALL CHKOUT ( 'WNVALD' )
         RETURN

      END IF

C
C     Taking the easy way out, we will simply insert each new interval
C     as we happen upon it. We can do this safely in place. The output
C     window can't possibly contain more intervals than the input array.
C
C     What can go wrong is this: a left endpoint might be greater than
C     the corresponding left endpoint. This is a boo-boo, and should be
C     reported.
C
      CALL SSIZED ( SIZE, WINDOW )
      CALL SCARDD (    0, WINDOW )

      I = 1

      DO WHILE  ( I .LT. N )

         LEFT  = WINDOW(I)
         RIGHT = WINDOW(I+1)

         IF ( LEFT .GT. RIGHT ) THEN
           CALL SETMSG ( 'WNVALD: Left endpoint may not exceed ' //
     .                   'right endpoint.'                        )
           CALL SIGERR ( 'SPICE(BADENDPOINTS)'                    )
           CALL CHKOUT ( 'WNVALD'                                 )
           RETURN
         END IF

         CALL WNINSD ( LEFT, RIGHT, WINDOW )
         I = I + 2

      END DO

      CALL CHKOUT ( 'WNVALD' )

      RETURN
      END

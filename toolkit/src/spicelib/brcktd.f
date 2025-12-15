C$Procedure BRCKTD ( Bracket a d.p. value within an interval )

      DOUBLE PRECISION FUNCTION BRCKTD ( NUMBER, END1, END2 )

C$ Abstract
C
C     Bracket a double precision number. That is, given a number and an
C     acceptable interval, make sure that the number is contained in the
C     interval. (If the number is already in the interval, leave it
C     alone. If not, set it to the nearest endpoint of the interval.)
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
C     INTERVALS
C     NUMBERS
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   NUMBER
      DOUBLE PRECISION   END1
      DOUBLE PRECISION   END2

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   Number to be bracketed.
C     END1       I   One of the bracketing endpoints for NUMBER.
C     END2       I   The other bracketing endpoint for NUMBER.
C
C     The function returns the bracketed number.
C
C$ Detailed_Input
C
C     NUMBER   is the number to be bracketed. That is, the
C              value of NUMBER is constrained to lie in the
C              interval bounded by END1 and END2.
C
C     END1,
C     END2     are the lower and upper bounds for NUMBER. The
C              order is not important.
C
C$ Detailed_Output
C
C     The function returns the bracketed number. That is NUMBER, if it
C     was already in the interval provided. Otherwise the returned
C     value is the nearest bound of the interval.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a shorthand notation for code fragments
C     like the following:
C
C        IF ( END1 .LT. END2 ) THEN
C           IF      ( NUMBER .LT. END1 ) THEN
C              NUMBER = END1
C           ELSE IF ( NUMBER .GT. END2 ) THEN
C              NUMBER = END2
C           END IF
C        ELSE
C           IF      ( NUMBER .LT. END2 ) THEN
C              NUMBER = END2
C           ELSE IF ( NUMBER .GT. END1 ) THEN
C              NUMBER = END1
C           END IF
C        END IF
C
C     which occur frequently during the processing of program inputs.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following code example illustrates the operation of
C        BRCKTD.
C
C        Example code begins here.
C
C
C              PROGRAM BRCKTD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION        BRCKTD
C
C        C
C        C     Local parameters.
C        C
C              INTEGER                 LISTSZ
C              PARAMETER             ( LISTSZ = 4  )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        END1    ( LISTSZ )
C              DOUBLE PRECISION        END2    ( LISTSZ )
C              DOUBLE PRECISION        NUMBER  ( LISTSZ )
C
C              INTEGER                 I
C
C        C
C        C     Set the values for the example.
C        C
C              DATA                    END1   /  1.D0,   1.D0,
C             .                                 10.D0, -10.D0  /
C              DATA                    END2   / 10.D0,  10.D0,
C             .                                -10.D0,  -1.D0  /
C              DATA                    NUMBER / -1.D0,  29.D0,
C             .                                  3.D0,   3.D0  /
C
C
C              WRITE(*,'(A)') ' Number  End1   End2   Bracketed'
C              WRITE(*,'(A)') ' ------  -----  -----  ---------'
C
C              DO I = 1, LISTSZ
C
C                 WRITE(*,'(3F7.1,F11.1)') NUMBER(I), END1(I), END2(I),
C             .                 BRCKTD ( NUMBER(I), END1(I), END2(I) )
C
C              END DO
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Number  End1   End2   Bracketed
C         ------  -----  -----  ---------
C           -1.0    1.0   10.0        1.0
C           29.0    1.0   10.0       10.0
C            3.0   10.0  -10.0        3.0
C            3.0  -10.0   -1.0       -1.0
C
C
C     2) The following code example illustrates a typical use for
C        BRCKTD: force a star magnitude limit to be within a range.
C        Note that this code assumes that the user provided value
C        is a valid double precision number.
C
C
C        Example code begins here.
C
C
C              PROGRAM BRCKTD_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION        BRCKTD
C
C        C
C        C     Local parameters.
C        C
C              INTEGER                 KWDSZ
C              PARAMETER             ( KWDSZ = 30   )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(KWDSZ)       USRIN
C
C              DOUBLE PRECISION        MAGLIN
C              DOUBLE PRECISION        MAGLOK
C
C        C
C        C     Prompt the user for the star magnitude.
C        C
C              CALL PROMPT ( 'Enter star magnitude: ', USRIN )
C
C        C
C        C     Convert the user input to double precision.
C        C
C              CALL PRSDP ( USRIN, MAGLIN )
C
C        C
C        C     Star magnitude must be in the range 0-10.
C        C
C              MAGLOK = BRCKTD ( MAGLIN, 0.D0, 10.D0 )
C
C        C
C        C     Display confirmation message.
C        C
C              IF ( MAGLIN .NE. MAGLOK ) THEN
C
C                 WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ',
C             .                 MAGLIN, ' is out of range (0-10).'
C
C              ELSE
C
C                 WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ',
C             .                 MAGLIN, ' is in range (0-10).'
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using '10.1' as user provided input, the output was:
C
C
C        Enter star magnitude: 10.1
C        Provided star magnitude 10.1 is out of range (0-10).
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
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 08-AUG-2021 (JDR) (BVS)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples based on existing code fragments.
C
C        Updated code fragment in $Particulars to show that the
C        order of endpoints is not important.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
C
C-&


C$ Index_Entries
C
C     bracket a d.p. value within an interval
C
C-&


C$ Revisions
C
C-    Beta Version 1.1.0, 30-DEC-1988 (WLT)
C
C        The routine was modified so that the order of the endpoints
C        of the bracketing interval is not needed. The routine now
C        determines which is the left endpoint and which is the
C        right and acts appropriately.
C
C-&


C
C     What else is there to say?
C
      IF ( END1 .LT. END2 ) THEN

         BRCKTD = MAX (  END1, MIN(END2,NUMBER)  )

      ELSE

         BRCKTD = MAX (  END2, MIN(END1,NUMBER)  )

      END IF

      RETURN
      END

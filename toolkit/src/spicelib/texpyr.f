C$Procedure TEXPYR ( Time --- Expand year )

      SUBROUTINE TEXPYR ( YEAR )

C$ Abstract
C
C     Expand an abbreviated year to a full year specification.
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
C     TIME
C
C$ Keywords
C
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               YEAR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR      I-O  The year of some epoch abbreviated/expanded.
C
C$ Detailed_Input
C
C     YEAR     is an "abbreviated year." In other words the 98 of
C              1998,  05 of 2005, etc.
C
C$ Detailed_Output
C
C     YEAR     is the expansion of the abbreviated year according
C              to the lower bound established in the entry point
C              TSETYR. By default if YEAR is 69 to 99, the output
C              is 1900 + the input value of YEAR. If YEAR is 0 to 68
C              the output value of YEAR is 2000 + the input value of
C              YEAR.
C
C              See the entry point TSETRY to modify this behavior.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If on input YEAR is not in the inclusive interval from
C         0 to 99, YEAR is returned unchanged.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows all of the SPICE time subsystem to handle
C     uniformly the expansion of "abbreviated" years.  (i.e. the
C     remainder after dividing the actual year by 100).
C
C     By using this routine together with the routine TSETYR you
C     can recover the actual year to associate with an abbreviation.
C
C     The default behavior is as follows
C
C        YEAR Input      YEAR Output
C        ----------      -----------
C            00              2000
C            01              2001
C             .                .
C             .                .
C             .                .
C            66              2066
C            67              2067
C            68              2068
C            69              1969
C             .                .
C             .                .
C             .                .
C            99              1999
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Demonstrate the default behavior of routine TEXPYR and then
C        modify it in order to set the lower bound of the expansion to
C        1980.
C
C        Example code begins here.
C
C
C              PROGRAM TEXPYR_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NYEARS
C              PARAMETER           ( NYEARS = 10 )
C
C        C
C        C     Local variables.
C        C
C              INTEGER               I
C              INTEGER               EXYEAR
C              INTEGER               YEARS  ( NYEARS )
C
C        C
C        C     Set the input years.
C        C
C              DATA                  YEARS /  0,  1, 68, 69, 70,
C             .                              78, 79, 80, 81, 99  /
C
C        C
C        C     Display the default behavior.
C        C
C              WRITE(*,'(A)') 'Default behavior:'
C              WRITE(*,*)
C
C              WRITE(*,'(A)') 'In  Expansion'
C              WRITE(*,'(A)') '--  ---------'
C
C              DO I=1, NYEARS
C
C                 EXYEAR = YEARS(I)
C                 CALL TEXPYR ( EXYEAR )
C
C                 WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR
C
C              END DO
C
C        C
C        C     Set up the lower bound for the expansion of abbreviated
C        C     years to 1980.
C        C
C              CALL TSETYR ( 1980 )
C
C        C
C        C     Display the new behavior.
C        C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Lower bound for expansion set to 1980:'
C              WRITE(*,*)
C
C              WRITE(*,'(A)') 'In  Expansion'
C              WRITE(*,'(A)') '--  ---------'
C
C              DO I=1, NYEARS
C
C                 EXYEAR = YEARS(I)
C                 CALL TEXPYR ( EXYEAR )
C
C                 WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR
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
C        Default behavior:
C
C        In  Expansion
C        --  ---------
C        00     2000
C        01     2001
C        68     2068
C        69     1969
C        70     1970
C        78     1978
C        79     1979
C        80     1980
C        81     1981
C        99     1999
C
C        Lower bound for expansion set to 1980:
C
C        In  Expansion
C        --  ---------
C        00     2000
C        01     2001
C        68     2068
C        69     2069
C        70     2070
C        78     2078
C        79     2079
C        80     1980
C        81     1981
C        99     1999
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 23-SEP-2020 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        example code.
C
C        Added TIME to the list of Required Readings.
C
C-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
C
C        The default century was changed from 1950-2049 to 1969-2068
C
C-    SPICELIB Version 1.0.0, 08-APR-1996 (WLT)
C
C-&


C$ Index_Entries
C
C     Expand an abbreviated year to a fully specified year.
C
C-&


      INTEGER               CENTRY
      INTEGER               LBOUND
      SAVE

      DATA                  CENTRY  / 1900 /
      DATA                  LBOUND  / 1969 /

      IF ( YEAR .GE. 100 .OR. YEAR .LT. 0 ) THEN
         RETURN
      END IF

      YEAR = YEAR + CENTRY

      IF ( YEAR .LT. LBOUND ) THEN
         YEAR = YEAR + 100
      END IF

      RETURN




C$Procedure TSETYR ( Time --- set year expansion boundaries )

      ENTRY TSETYR ( YEAR )

C$ Abstract
C
C     Set the lower bound on the 100 year range.
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
C     TIME
C
C$ Keywords
C
C     TIME
C
C$ Declarations
C
C     INTEGER               YEAR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR       I   Lower bound on the 100 year interval of expansion
C
C$ Detailed_Input
C
C     YEAR     is the year associated with the lower bound on all year
C              expansions computed by the SPICELIB routine TEXPYR. For
C              example if YEAR is 1980, then the range of years that can
C              be abbreviated is from 1980 to 2079.
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
C     Error free.
C
C     1)  If YEAR is less than 1, no action is taken.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to set the range to which years
C     abbreviated to the last two digits will be expanded, allowing all
C     of the SPICE time subsystem routines to handle uniformly the
C     expansion those "abbreviated" years (i.e. the remainder after
C     dividing the actual year by 100.) The input supplied to this
C     routine represents the lower bound of the expansion interval. The
C     upper bound of the expansion interval is YEAR + 99.
C
C     The default expansion interval is from 1969 to 2068.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that you need to manipulate time strings and that
C        you want to treat years components in the range from 0 to 99
C        as being abbreviations for years in the range from
C        1980 to 2079 (provided that the years are not modified by
C        an ERA substring). The example code below shows how you
C        could go about this.
C
C        Use the LSK kernel below to load the leap seconds and time
C        constants required for the conversions.
C
C           naif0012.tls
C
C
C        Example code begins here.
C
C
C              PROGRAM TSETYR_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               DATELN
C              PARAMETER           ( DATELN = 11 )
C
C              INTEGER               NTSTRS
C              PARAMETER           ( NTSTRS = 7 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(DATELN)    DATE   (NTSTRS)
C              CHARACTER*(DATELN)    TIMSTR
C
C              DOUBLE PRECISION      ET
C
C              INTEGER               I
C
C        C
C        C     Assign an array of calendar dates.
C        C
C              DATA                  DATE   / '00 JAN 21',
C             .                               '01 FEB 22',
C             .                               '48 MAR 23',
C             .                               '49 APR 24',
C             .                               '79 JUL 14',
C             .                               '80 FEB 02',
C             .                               '99 DEC 31' /
C
C        C
C        C     Load the required LSK.
C        C
C              CALL FURNSH ( 'naif0012.tls' )
C
C        C
C        C     Set up the lower bound for the
C        C     expansion of abbreviated years.
C        C
C              CALL TSETYR ( 1980 )
C
C        C
C        C     Expand the years in input time strings.
C        C
C              WRITE(*,*) 'Time string    Expansion'
C              WRITE(*,*) '-----------    -----------'
C
C              DO I = 1, NTSTRS
C
C                 CALL STR2ET ( DATE(I), ET )
C                 CALL TIMOUT ( ET, 'YYYY MON DD', TIMSTR )
C
C                 WRITE(*,*) DATE(I), '    ', TIMSTR
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
C         Time string    Expansion
C         -----------    -----------
C         00 JAN 21      2000 JAN 21
C         01 FEB 22      2001 FEB 22
C         48 MAR 23      2048 MAR 23
C         49 APR 24      2049 APR 24
C         79 JUL 14      2079 JUL 14
C         80 FEB 02      1980 FEB 02
C         99 DEC 31      1999 DEC 31
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 23-SEP-2020 (JDR)
C
C        Fixed bug: Added check for "YEAR" to be positive in order to
C        update the lower bound for the expansion.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Added TIME to the list of Required Readings. Extended
C        description in $Particulars to further describe the intended
C        use of this routine.
C
C-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
C
C        The default century was change from 1950-2049 to 1969-2068.
C
C-    SPICELIB Version 1.0.0, 08-APR-1996 (WLT)
C
C-&


C$ Index_Entries
C
C     Set the interval of expansion for abbreviated years
C
C-&


      IF ( YEAR .GT. 0 ) THEN

         CENTRY = ( YEAR/100 ) * 100
         LBOUND =   YEAR

      END IF

      RETURN
      END

C$Procedure SCFMT ( Convert SCLK "ticks" to character clock format )

      SUBROUTINE SCFMT ( SC, TICKS, CLKSTR )

C$ Abstract
C
C     Convert encoded spacecraft clock ticks to character clock format.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               SC
      DOUBLE PRECISION      TICKS
      CHARACTER*(*)         CLKSTR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     TICKS      I   Spacecraft clock count encoded representation.
C     CLKSTR     O   Character representation of a clock count.
C
C$ Detailed_Input
C
C     SC       is the NAIF ID number for the spacecraft whose clock's
C              time is being decoded.
C
C     TICKS    is the double precision encoding of a clock time in
C              units of ticks. Partition information is not reflected
C              in this value.
C
C              An analogy may be drawn between a spacecraft clock and
C              a standard wall clock. The number of ticks
C              corresponding to the wall clock string
C
C                 hh:mm:ss
C
C              would be the number of seconds represented by that
C              time.
C
C              For example,
C
C                 Clock string    Number of ticks
C                 ------------    ---------------
C                   00:00:10             10
C                   00:01:00             60
C                   00:10:00            600
C                   01:00:00           3600
C                   01:01:00           3660
C
C              If TICKS contains a fractional part the result is the
C              same as if TICKS had been rounded to the nearest whole
C              number.
C
C              See the $Examples section below for examples of
C              actual spacecraft clock conversions.
C
C$ Detailed_Output
C
C     CLKSTR   is the spacecraft clock character string
C              corresponding to TICKS. Partition information is
C              not included in CLKSTR.
C
C              Using Galileo as an example, the full format clock
C              string is
C
C                 wwwwwwww:xx:y:z
C
C              where z is a mod-8 counter (values 0-7) which
C              increments approximately once every 8 1/3 ms., y is a
C              mod-10 counter (values 0-9) which increments once
C              every time z turns over, i.e., approximately once every
C              66 2/3 ms., xx is a mod-91 (values 0-90) counter
C              which increments once every time y turns over, i.e.,
C              once every 2/3 seconds. wwwwwwww is the Real-Time Image
C              Count (RIM), which increments once every time xx turns
C              over, i.e., once every 60 2/3 seconds. The roll-over
C              expression for the RIM is 16777215, which corresponds
C              to approximately 32 years.
C
C              wwwwwwww, xx, y, and z are referred to interchangeably
C              as the fields or components of the spacecraft clock.
C              SCLK components may be separated by any of these five
C              characters: ' '  ':'  ','  '-'  '.'
C              The delimiter used is determined by a kernel pool
C              variable and can be adjusted by the user.
C
C              Some spacecraft clock components have offset, or
C              starting, values different from zero. For example,
C              with an offset value of 1, a mod 20 counter would
C              cycle from 1 to 20 instead of from 0 to 19.
C
C              See the SCLK required reading for a detailed
C              description of the Voyager and Mars Observer clock
C              formats.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the data type for the spacecraft is not supported,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     2)  If the value for TICKS is negative, an error is signaled
C         by a routine in the call tree of this routine.
C
C     3)  If the SCLK kernel file does not contain data for the
C         spacecraft specified by SC, an error is signaled by a routine
C         in the call tree of this routine.
C
C     4)  If the declared length of CLKSTR is not large enough to
C         contain the output clock string, an error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The routine SCTIKS performs the inverse operation to SCFMT,
C     converting from clock format to number of ticks.
C
C     Note the important difference between SCFMT and SCDECD. SCDECD
C     converts some number of ticks since the spacecraft clock start
C     time to a character string which includes a partition number.
C     SCFMT, which is called by SCDECD, does not make use of partition
C     information.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) The following code example finds partition start and stop
C        times for the Stardust spacecraft from a spacecraft clock
C        kernel file. Since those times are always returned in units
C        of ticks, the program uses SCFMT to print the times in
C        Stardust clock format.
C
C        Use the SCLK kernel below to load the Stardust time
C        correlation data and spacecraft clock partition information.
C
C           sdu_sclkscet_00074.tsc
C
C
C        Example code begins here.
C
C
C              PROGRAM SCFMT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include the parameters that define the sizes and limits
C        C     of the SCLK system.
C        C
C              INCLUDE 'sclk.inc'
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               CLKLEN
C              PARAMETER           ( CLKLEN = 30 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(CLKLEN)    START
C              CHARACTER*(CLKLEN)    STOP
C
C              DOUBLE PRECISION      PSTART (MXPART)
C              DOUBLE PRECISION      PSTOP  (MXPART)
C
C              INTEGER               I
C              INTEGER               NPARTS
C              INTEGER               SC
C
C        C
C        C     Assign the value for the Stardust spacecraft ID.
C        C
C              SC = -29
C
C        C
C        C     Load the SCLK file.
C        C
C              CALL FURNSH ( 'sdu_sclkscet_00074.tsc' )
C
C        C
C        C     Retrieve the arrays for PSTART and PSTOP and the
C        C     number of partitions within the SCLK.
C        C
C              CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
C
C        C
C        C     Loop over each array value.
C        C
C              DO I= 1, NPARTS
C
C                 CALL SCFMT ( SC, PSTART( I ), START )
C                 CALL SCFMT ( SC, PSTOP ( I ), STOP )
C
C                 WRITE(*,*)
C                 WRITE(*,*) 'Partition: ', I
C                 WRITE(*,*) '   Start : ', START
C                 WRITE(*,*) '   Stop  : ', STOP
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
C         Partition:            1
C            Start : 0000000000.000
C            Stop  : 0602741011.080
C
C         Partition:            2
C            Start : 0602741014.217
C            Stop  : 0605660648.173
C
C         Partition:            3
C            Start : 0605660649.000
C            Stop  : 0631375256.224
C
C         Partition:            4
C            Start : 0631375257.000
C            Stop  : 0633545577.218
C
C         Partition:            5
C            Start : 0633545578.000
C            Stop  : 0644853954.043
C
C         Partition:            6
C            Start : 0644853954.000
C            Stop  : 0655316480.089
C
C         Partition:            7
C            Start : 0655316480.000
C            Stop  : 0660405279.066
C
C         Partition:            8
C            Start : 0660405279.000
C            Stop  : 0670256568.229
C
C         Partition:            9
C            Start : 0670256569.000
C            Stop  : 0674564039.091
C
C         Partition:           10
C            Start : 0674564040.000
C            Stop  : 4294537252.255
C
C
C     2) Below are some examples illustrating various input numbers of
C        ticks and the resulting clock string outputs for the Galileo
C        spacecraft.
C
C           TICKS                CLKSTR
C           ----------------     --------------------
C           -1                   Error: Ticks must be a positive number
C           0                    '00000000:00:0:0'
C           1                    '00000000:00:0:1'
C           1.3                  '00000000:00:0:1'
C           1.5                  '00000000:00:0:2'
C           2                    '00000000:00:0:2'
C           7                    '00000000:00:0:7'
C           8                    '00000000:00:1:0'
C           80                   '00000000:01:0:0'
C           88                   '00000000:01:1:0'
C           7279                 '00000000:90:9:7'
C           7280                 '00000001:00:0:0'
C           1234567890           '00169583:45:6:2'
C
C        The following examples are for the Voyager 2 spacecraft.
C        Note that the third component of the Voyager clock has an
C        offset value of one.
C
C           TICKS                CLKSTR
C           ----------------     --------------------
C           -1                   Error: Ticks must be a positive number
C           0                    '00000:00:001'
C           1                    '00000:00:002'
C           1.3                  '00000:00:002'
C           1.5                  '00000:00:003'
C           2                    '00000:00:003'
C           799                  '00000:00:800'
C           800                  '00000:01:001'
C           47999                '00000:59:800'
C           48000                '00001:00:001'
C           3145727999           '65535:59:800'
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
C     J.M. Lynch         (JPL)
C     W.L. Taber         (JPL)
C     R.E. Thurman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing code fragment.
C
C        Moved implementation details from $Particulars section to a
C        comment in the appropriate part of the code where it applies.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references
C        to FURNSH.
C
C-    SPICELIB Version 1.0.1, 17-APR-1992 (JML) (WLT)
C
C        The $Exceptions section was updated to state that an error
C        is signaled if SCLKCH is not declared big enough to
C        contain the output spacecraft clock string.
C
C        The wording to exception number three was changed.
C
C        Miscellaneous minor updates to the header were performed.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
C
C-&


C$ Index_Entries
C
C     convert spacecraft_clock ticks to character clock format
C
C-&


C
C     SPICELIB functions
C
      INTEGER               SCTYPE
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               TYPE


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCFMT' )
      END IF

C
C     Determine which data type the spacecraft clock belongs to and
C     calls SCFMnn, where nn corresponds to the data type code. SCFMTnn
C     handles the actual conversion from ticks to clock string format.
C
      TYPE  =  SCTYPE( SC )

      IF  ( TYPE .EQ. 1 )  THEN
         CALL SCFM01( SC, TICKS, CLKSTR )
      ELSE
         CALL SETMSG ( 'Clock type # is not supported. ' )
         CALL ERRINT ( '#', TYPE                         )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
         CALL CHKOUT ( 'SCFMT'                           )
         RETURN
      END IF


      CALL CHKOUT ( 'SCFMT' )
      RETURN
      END

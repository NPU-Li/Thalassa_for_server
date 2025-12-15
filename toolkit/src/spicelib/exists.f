C$Procedure EXISTS ( Does the file exist? )

      LOGICAL FUNCTION  EXISTS ( FNAME )

C$ Abstract
C
C     Determine whether a file exists.
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
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    FNAME

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of the file in question.
C
C     The function returns the value .TRUE. if the file exists, .FALSE.
C     otherwise.
C
C$ Detailed_Input
C
C     FNAME    is the name of the file in question. This may be any
C              unambiguous file name valid on the user's computer, for
C              example
C
C                 '/usr/dir1/dir2/DATA.DAT'
C                 './DATA.DAT'
C                 'c:\usr\dir1\dir2\data.dat'
C
C              Environment or shell variables may not be used.
C
C$ Detailed_Output
C
C     The function returns the value .TRUE. if the file exists, .FALSE.
C     otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is
C         signaled.
C
C     2)  If an I/O error occurs while checking the existence of the
C         indicated file, the error SPICE(INQUIREFAILED) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Use the Fortran INQUIRE statement to determine the existence
C     of FNAME.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given two arbitrary files (one of them the actual code example
C        source file), determine if they exists.
C
C        Example code begins here.
C
C
C              PROGRAM EXISTS_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              LOGICAL                 EXISTS
C              INTEGER                 RTRIM
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 FILEN
C              PARAMETER             ( FILEN = 14 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(FILEN)       FNAME  ( 2 )
C
C              INTEGER                 I
C
C        C
C        C     Define an array of file names.
C        C
C              DATA                    FNAME / 'exists_ex1.txt',
C             .                                'exists_ex1.pgm' /
C
C              DO I = 1, 2
C
C                 IF ( EXISTS ( FNAME(I) ) ) THEN
C
C                    WRITE(*,*) 'The file ', FNAME(I)(:RTRIM(FNAME(I))),
C             .                 ' exists.'
C
C                 ELSE
C
C                    WRITE(*,*) 'Cannot find the file ', FNAME(I)
C
C                 END IF
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
C         Cannot find the file exists_ex1.txt
C         The file exists_ex1.pgm exists.
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 17-JUN-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Changed input argument name FILE to FNAME for consistency with
C        other routines.
C
C        Added IMPLICIT NONE statement.
C
C-    SPICELIB Version 2.2.1, 01-JUL-2014 (NJB)
C
C        VAX examples were deleted from the header.
C
C-    SPICELIB Version 2.2.0, 09-DEC-1999 (WLT)
C
C        The input file name is now "trimmed" of trailing blanks
C        before checking its existence.
C
C-    SPICELIB Version 2.1.0, 04-MAR-1996 (KRG)
C
C        Added a local logical variable that is used as temporary
C        storage for the results from the INQUIRE statement rather
C        than using the function name. This solved a problem on the
C        macintosh.
C
C-    SPICELIB Version 2.0.0, 04-AUG-1994 (KRG)
C
C        Added a test to see if the filename was blank before the
C        INQUIRE statement. This allows a meaningful error message to
C        be presented.
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is .TRUE. upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     does the file exist
C
C-&


C$ Revisions
C
C-    Beta Version 2.0.0, 29-DEC-1988 (HAN)
C
C        The IOSTAT specifier was added to the INQUIRE statement.
C        If the value of IOSTAT is not equal to zero, an error
C        occurred during the execution of the INQUIRE statement.
C        In this case, a SPICELIB error is signaled and the routine
C        checks out.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM

C
C     Local variables
C
      INTEGER               IOSTAT
      INTEGER               R

      LOGICAL               MYEXST

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         EXISTS = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'EXISTS' )
      END IF
C
C     Initialize the local variable MYEXST to be .FALSE.
C
      MYEXST = .FALSE.

C
C     First we test to see if the filename is blank.
C
      IF ( FNAME .EQ. ' ' ) THEN

         EXISTS = .FALSE.
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'EXISTS'                   )
         RETURN

      END IF

      R = RTRIM(FNAME)
C
C     So simple, it defies explanation.
C
      INQUIRE ( FILE = FNAME(1:R), EXIST = MYEXST, IOSTAT = IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         EXISTS = .FALSE.
         CALL SETMSG ( 'Value of IOSTAT was *.' )
         CALL ERRINT ( '*', IOSTAT              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'   )
         CALL CHKOUT ( 'EXISTS'                 )
         RETURN

      END IF
C
C     Set the value of the function, check out and return.
C
      EXISTS = MYEXST

      CALL CHKOUT ( 'EXISTS' )
      RETURN
      END

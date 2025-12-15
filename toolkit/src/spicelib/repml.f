C$Procedure REPML ( Replace marker with logical value text )

      SUBROUTINE REPML ( IN, MARKER, VALUE, RTCASE, OUT )

C$ Abstract
C
C     Replace a marker with the text representation of a logical value.
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
C     CHARACTER
C     CONVERSION
C     STRING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         IN
      CHARACTER*(*)         MARKER
      LOGICAL               VALUE
      CHARACTER*1           RTCASE
      CHARACTER*(*)         OUT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     MARKER     I   Marker to be replaced.
C     VALUE      I   Replacement logical value.
C     RTCASE     I   Case of replacement text.
C     OUT        O   Output string.
C
C$ Detailed_Input
C
C     IN       is an arbitrary character string.
C
C     MARKER   is an arbitrary character string. The first occurrence
C              of MARKER in the input string is to be replaced by
C              VALUE.
C
C              MARKER is case-sensitive.
C
C              Leading and trailing blanks in MARKER are NOT
C              significant. In particular, no substitution is
C              performed if MARKER is blank.
C
C     VALUE    is an arbitrary logical value, either .TRUE. or
C              .FALSE.
C
C     RTCASE   indicates the case of the replacement text. RTCASE may
C              be any of the following:
C
C                 RTCASE    Meaning        Output values
C                 ------    -----------    ---------------
C                 U, u      Uppercase      'TRUE', 'FALSE'
C
C                 L, l      Lowercase      'true', 'false'
C
C                 C, c      Capitalized    'True', 'False'
C
C$ Detailed_Output
C
C     OUT      is the string obtained by substituting the text
C              representation of VALUE for the first occurrence of
C              MARKER in the input string.
C
C              OUT and IN must be disjoint.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If OUT does not have sufficient length to accommodate the
C         result of the substitution, the result will be truncated on
C         the right.
C
C     2)  If MARKER is blank, or if MARKER is not a substring of IN,
C         no substitution is performed. (OUT and IN are identical.)
C
C     3)  If the value of RTCASE is not recognized, the error
C         SPICE(INVALIDCASE) is signaled. OUT is not changed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is one of a family of related routines for inserting values
C     into strings. They are typically used to construct messages that
C     are partly fixed, and partly determined at run time. For example,
C     a message like
C
C        'Fifty-one pictures were found in directory [USER.DATA].'
C
C     might be constructed from the fixed string
C
C        '#1 pictures were found in directory #2.'
C
C     by the calls
C
C        CALL REPMCT ( STRING, '#1',  51,           'C', TMPSTR )
C        CALL REPMC  ( TMPSTR, '#2', '[USER.DATA]',      STRING )
C
C     which substitute the cardinal text 'Fifty-one' and the character
C     string '[USER.DATA]' for the markers '#1' and '#2' respectively.
C
C     The complete list of routines is shown below.
C
C        REPMC    ( Replace marker with character string value )
C        REPMD    ( Replace marker with double precision value )
C        REPMF    ( Replace marker with formatted d.p. value   )
C        REPMI    ( Replace marker with integer value          )
C        REPML    ( Replace marker with logical value          )
C        REPMCT   ( Replace marker with cardinal text          )
C        REPMOT   ( Replace marker with ordinal text           )
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following example illustrates the use of REPML to replace
C        a marker within a string with the text representation of a
C        logical value.
C
C
C        Example code begins here.
C
C
C              PROGRAM REPML_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 80 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)      INSTR
C              CHARACTER*(STRLEN)      MARKER
C              CHARACTER*(STRLEN)      OUTSTR
C
C
C        C
C        C     1. Uppercase
C        C
C              MARKER = '#'
C              INSTR  = 'Invalid value. The value was:  #.'
C
C              CALL REPML ( INSTR, MARKER, .FALSE. , 'U' , OUTSTR )
C
C              WRITE(*,*) 'Case 1: Replacement text in uppercase.'
C              WRITE(*,*) '   Input : ', INSTR
C              WRITE(*,*) '   Output: ', OUTSTR
C              WRITE(*,*) ' '
C
C        C
C        C     2. Lowercase
C        C
C              MARKER = ' XX '
C              INSTR  = 'Invalid value. The value was:  XX.'
C
C              CALL REPML ( INSTR, MARKER, .TRUE. , 'l' , OUTSTR )
C
C              WRITE(*,*) 'Case 2: Replacement text in lowercase.'
C              WRITE(*,*) '   Input : ', INSTR
C              WRITE(*,*) '   Output: ', OUTSTR
C              WRITE(*,*) ' '
C
C        C
C        C     2. Capitalized
C        C
C              MARKER = '#'
C              INSTR  = 'Invalid value. The value was:  #.'
C
C              CALL REPML ( INSTR, MARKER, .FALSE. , 'c' , OUTSTR )
C
C              WRITE(*,*) 'Case 3: Replacement text capitalized.'
C              WRITE(*,*) '   Input : ', INSTR
C              WRITE(*,*) '   Output: ', OUTSTR
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Case 1: Replacement text in uppercase.
C            Input : Invalid value. The value was:  #.
C            Output: Invalid value. The value was:  FALSE.
C
C         Case 2: Replacement text in lowercase.
C            Input : Invalid value. The value was:  XX.
C            Output: Invalid value. The value was:  true.
C
C         Case 3: Replacement text capitalized.
C            Input : Invalid value. The value was:  #.
C            Output: Invalid value. The value was:  False.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 08-JAN-2021 (JDR) (NJB)
C
C-&


C$ Index_Entries
C
C     replace marker with logical value
C
C-&


C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               ISRCHC
      INTEGER               LASTNB

      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               MAXLLT
      PARAMETER           ( MAXLLT = 5 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 3 )

C
C     Local variables
C
      CHARACTER*(MAXLLT)    LVALUE
      CHARACTER*(MAXLLT)    VALSTR ( NCASE, 2 )
      CHARACTER*(1)         TMPCAS
      CHARACTER*(1)         CASSTR ( NCASE )

      INTEGER               CASIDX
      INTEGER               MRKNBF
      INTEGER               MRKNBL
      INTEGER               MRKPSB
      INTEGER               MRKPSE
      INTEGER               VALIDX

C
C     Saved variables
C
      SAVE                  CASSTR
      SAVE                  VALSTR

C
C     Initial values
C
      DATA                  CASSTR / 'U', 'L', 'C' /

      DATA                  VALSTR / 'TRUE',  'true',  'True',
     .                               'FALSE', 'false', 'False'  /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'REPML' )

C
C     Identify the case string and find its index in the
C     array of uppercase case strings. Bail out if RTCASE is not
C     recognized.
C
C     RTCASE has length 1, so we need not be concerned with leading
C     blanks.
C
      CALL UCASE ( RTCASE, TMPCAS )

      CASIDX = ISRCHC ( TMPCAS, NCASE, CASSTR )

      IF ( CASIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Case (#) must be U, L, or C.' )
         CALL ERRCH  ( '#', RTCASE                    )
         CALL SIGERR ( 'SPICE(INVALIDCASE)'           )
         CALL CHKOUT ( 'REPML'                        )
         RETURN

      END IF

C
C     If MARKER is blank, no substitution is possible.
C
      IF ( MARKER .EQ. ' ' ) THEN

         OUT = IN

         CALL CHKOUT ( 'REPML' )
         RETURN

      END IF

C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks). If MARKER is not
C     a substring of IN, no substitution can be performed.
C
      MRKNBF = FRSTNB(MARKER)
      MRKNBL = LASTNB(MARKER)

C
C     MARKER is non-blank, so the index range below is valid.
C
      MRKPSB = INDEX ( IN, MARKER( MRKNBF : MRKNBL )  )

      IF ( MRKPSB .EQ. 0 ) THEN

         OUT = IN

         CALL CHKOUT ( 'REPML' )
         RETURN

      END IF

      MRKPSE = MRKPSB + MRKNBL - MRKNBF

C
C     Okay, MARKER is non-blank and has been found.
C
      IF ( VALUE ) THEN
         VALIDX = 1
      ELSE
         VALIDX = 2
      END IF

C
C     Set the value string based on the case specification and
C     the input logical value.
C
      LVALUE = VALSTR( CASIDX, VALIDX )

C
C     Replace MARKER with LVALUE.
C
      CALL REPSUB ( IN,
     .              MRKPSB,
     .              MRKPSE,
     .              LVALUE( : LASTNB(LVALUE) ),
     .              OUT )

      CALL CHKOUT ( 'REPML' )
      RETURN
      END

C$Procedure ERRINT ( Insert Integer into Error Message Text )

      SUBROUTINE ERRINT ( MARKER, INTNUM )

C$ Abstract
C
C     Substitute an integer for the first occurrence of a marker found
C     in the current long error message.
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
C     ERROR
C
C$ Keywords
C
C     CONVERSION
C     ERROR
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'

      CHARACTER*(*)        MARKER
      INTEGER              INTNUM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MARKER     I   A substring of the error message to be replaced.
C     INTNUM     I   The integer to substitute for MARKER.
C
C$ Detailed_Input
C
C     MARKER   is a character string which marks a position in
C              the long error message where a character string
C              representing an integer is to be substituted.
C              Leading and trailing blanks in MARKER are not
C              significant.
C
C              Case IS significant;  'XX' is considered to be
C              a different marker from 'xx'.
C
C     INTNUM   is an integer whose character representation will
C              be substituted for the first occurrence of MARKER
C              in the long error message. This occurrence of the
C              substring indicated by MARKER will be removed, and
C              replaced by a character string, with no leading or
C              trailing blanks, representing INTNUM.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LMSGLN   is the maximum length of the long error message. See
C              the include file errhnd.inc for the value of LMSGLN.
C
C$ Exceptions
C
C     1)  This routine does not detect any errors.
C
C         However, this routine is part of the SPICELIB error
C         handling mechanism.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine updates the current long error message. If no marker
C     is found, (e.g., in the case that the long error message is
C     blank), the routine has no effect. If multiple instances of the
C     marker designated by MARKER are found, only the first one is
C     replaced.
C
C     If the character string resulting from the substitution
C     exceeds the maximum length of the long error message, the
C     characters on the right are lost. No error is signaled.
C
C     This routine has no effect if changes to the long message
C     are not allowed.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a user-defined error message, including both the
C        short and long messages, providing the value of two integer
C        variables within the long message, and signal the error.
C
C
C        Example code begins here.
C
C
C              PROGRAM ERRINT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Set long error message, with two different MARKER
C        C     strings where the value of the integer variables
C        C     will go.  Our markers are '#' and 'XX'.
C        C
C              CALL SETMSG ( 'LONG MESSAGE.  Invalid operation value. '
C             .         //   '  The value was #.  Left endpoint '
C             .         //   'exceeded right endpoint.  The left '
C             .         //   'endpoint was:  XX.  The right endpoint '
C             .         //   'was:  XX.' )
C
C        C
C        C     Insert the integer number where the # is now.
C        C
C              CALL ERRINT ( '#',  5  )
C
C        C
C        C     Insert now an integer variable in the long message where
C        C     the first XX is now.
C        C
C              CALL ERRINT ( 'XX', 910 )
C
C        C
C        C     Signal the error.
C        C
C              CALL SIGERR ( 'SPICE(USERDEFINED)' )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        ============================================================***
C
C        Toolkit version: N0066
C
C        SPICE(USERDEFINED) --
C
C        LONG MESSAGE. Invalid operation value. The value was 5. Left***
C        exceeded right endpoint. The left endpoint was: 910. The right
C        endpoint was: XX.
C
C        Oh, by the way:  The SPICELIB error handling actions are USER-
C        TAILORABLE.  You can choose whether the Toolkit aborts or co***
C        when errors occur, which error messages to output, and where***
C        the output.  Please read the ERROR "Required Reading" file, ***
C        the routines ERRACT, ERRDEV, and ERRPRT.
C
C        ============================================================***
C
C
C        Warning: incomplete output. 6 lines extended past the right
C        margin of the header and have been truncated. These lines are
C        marked by "***" at the end of each line.
C
C
C        Note that the execution of this program produces the error
C        SPICE(USERDEFINED), which follows the NAIF standard as
C        described in the ERROR required reading.
C
C$ Restrictions
C
C     1)  The caller must ensure that the message length, after
C         substitution is performed, doesn't exceed LMSGLN characters.
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
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 17-JUN-2021 (JDR)
C
C        Changed input argument name INTEGR to INTNUM consistency with
C        other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Added complete code example
C        based on existing fragments.
C
C-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN. Miscellaneous format changes to the
C        header, code and in-line comments were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     insert integer into error message text
C
C-&


C
C     SPICELIB functions
C
      INTEGER               LASTNB
      INTEGER               FRSTNB
      LOGICAL               ALLOWD

C
C     Local Variables:
C
      CHARACTER*(LMSGLN)    LNGMSG
      CHARACTER*(LMSGLN)    TMPMSG
      CHARACTER*(11)        ISTRNG

      INTEGER               STRPOS


C
C     Changes to the long error message have to be allowed, or we
C     do nothing.
C
      IF ( .NOT. ALLOWD() ) THEN
         RETURN
      END IF

C
C     MARKER has to have some non-blank characters, or we do nothing.
C
      IF ( LASTNB(MARKER) .EQ. 0 ) THEN
         RETURN
      END IF

C
C     Get a copy of the current long error message.  Convert INTNUM
C     to a character string.
C
      CALL GETLMS ( LNGMSG )
      CALL INTSTR ( INTNUM, ISTRNG )

C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks):
C
      STRPOS = INDEX (  LNGMSG,
     .                  MARKER ( FRSTNB(MARKER):LASTNB(MARKER) )  )

      IF  ( STRPOS .EQ. 0 ) THEN

         RETURN

      ELSE
C
C        We put together TMPMSG, a copy of LNGMSG with MARKER
C        replaced by the character representation of INTNUM:
C

         IF ( STRPOS .GT. 1 ) THEN

            IF (     ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) )
     .          .LT.   LASTNB ( LNGMSG )                          ) THEN
C
C              There's more of the long message after the marker...
C
               TMPMSG =
     .         LNGMSG ( :STRPOS - 1 )                                 //
     .         ISTRNG ( :LASTNB(ISTRNG) )                             //
     .         LNGMSG ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) + 1: )

            ELSE

               TMPMSG =  LNGMSG ( :STRPOS - 1     )                   //
     .                   ISTRNG ( :LASTNB(ISTRNG) )

            END IF


         ELSE
C
C           We're starting with the integer, so we know it fits...
C
            IF (      ( LASTNB(MARKER) - FRSTNB(MARKER) )
     .           .LT.   LASTNB(LNGMSG)                    ) THEN
C
C              There's more of the long message after the marker...
C
               TMPMSG = ISTRNG ( :LASTNB(ISTRNG)                 ) //
     .                  LNGMSG ( STRPOS + LASTNB(MARKER)
     .                                  - FRSTNB(MARKER)  + 1 :  )
            ELSE
C
C              The marker's the whole string:
C
               TMPMSG = ISTRNG

            END IF


         END IF
C
C        Update the long message:
C
         CALL PUTLMS ( TMPMSG )

      END IF

      END

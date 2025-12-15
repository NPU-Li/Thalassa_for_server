C$Procedure GETMSG ( Get Error Message )

      SUBROUTINE GETMSG ( OPTION, MSG )

C$ Abstract
C
C     Retrieve the current short error message, the explanation of the
C     short error message, or the long error message.
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
C     ERROR
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)          OPTION
      CHARACTER*(*)          MSG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OPTION     I   Indicates type of error message.
C     MSG        O   The error message to be retrieved.
C
C$ Detailed_Input
C
C     OPTION   is a string that indicates the type of error message to
C              be retrieved.
C
C              Possible values of OPTION are:
C
C                 'SHORT'     indicates that the short message is to be
C                             retrieved.
C
C                 'EXPLAIN'   indicates that the explanation of the
C                             short message is to be retrieved.
C
C                 'LONG'      indicates that the long message is to be
C                             retrieved.
C
C              The input strings indicating the choice of option may be
C              in mixed case. Leading and trailing blanks in OPTION are
C              not significant.
C
C$ Detailed_Output
C
C     MSG      is the error message to be retrieved. Its value depends
C              on OPTION, and on whether an error condition exists.
C
C              When there is no error condition, MSG is blank.
C
C              If an error condition does exist, and OPTION is
C
C                'SHORT'        MSG is the current short error message.
C                               This is a very condensed, 25-character
C                               description of the error.
C
C                'EXPLAIN'      MSG is the explanation of the current
C                               short error message. This is a one-line
C                               expansion of the text of the short
C                               message.
C
C                               All SPICELIB short error messages
C                               do have corresponding explanation text.
C                               For other short error messages, if
C                               there is no explanation text, MSG
C                               will be blank.
C
C                'LONG'         MSG is the current long error message.
C                               The long error message is a detailed
C                               explanation of the error, possibly
C                               containing data specific to the
C                               particular occurrence of the error.
C                               Not all errors have long error messages.
C                               If there is none, MSG will be blank.
C                               Long error messages are no longer than
C                               320 characters.
C
C              If OPTION is invalid, MSG will remain unchanged from its
C              value on input.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input OPTION is invalid, the error
C         SPICE(INVALIDMSGTYPE) is signaled. In that case no messages
C         are returned; MSG retains the value it had on input.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Please read the "required reading" first!
C
C     A good time to call this routine would be when an error
C     condition exists, as indicated by the SPICELIB function,
C     FAILED.
C
C     See the example below for a serving suggestion.
C
C     GETMSG isn't too useful if an error condition doesn't
C     exist, since it will return a blank string in that case.
C
C$ Examples
C
C     Here's an example of a real-life call to GETMSG to get the
C     explanation of the current short error message.
C
C     In this example, a SPICELIB routine, RDTEXT, is called.
C     Following the return from RDTEXT, the logical function,
C     FAILED, is tested to see whether an error occurred.
C     If it did, the message is retrieved and output via
C     a user-defined output routine:
C
C
C     C
C     C     We call RDTEXT; then test for errors...
C     C
C           CALL RDTEXT ( FILE, LINE, EOF )
C
C           IF ( FAILED ) THEN
C
C     C
C     C        Get explanation text for the current short message
C     C        and print it:
C     C
C
C              CALL GETMSG ( 'EXPLAIN', TEXT )
C
C              CALL USER_DEFINED_OUTPUT ( TEXT )
C
C                    .
C                    .   [Do more stuff here]
C                    .
C
C           END IF
C
C$ Restrictions
C
C     1)  This routine is part of the interface to the SPICELIB error
C         handling mechanism. For this reason, this routine does not
C         participate in the trace scheme, even though it has external
C         references.
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
C-    SPICELIB Version 1.1.0, 20-APR-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Moved
C        disclaimer on participation of this routine in the SPICELIB
C        trace scheme from $Exceptions to $Restrictions section.
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
C     get error message
C
C-&


C
C     Local Variables:
C

C
C     Length of short error message:
C

      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN = 25 )

      CHARACTER*(SHRTLN)    SHRTMS

C
C     Upper case version of the option:
C

      INTEGER               OPTLEN
      PARAMETER           ( OPTLEN = 10 )

      CHARACTER*(OPTLEN)    UPOPT
      CHARACTER*(OPTLEN)    LOCOPT

C
C     Heeeeeeeeeeeeeeeeeeeeer's the code!
C

C
C     We only speak upper case in this routine,
C     so convert any lower case letters in OPTION
C     to upper case.  We save the original OPTION
C     string just in case we need to echo it in
C     an error message.
C
      CALL LJUST ( OPTION, UPOPT )
      CALL UCASE ( UPOPT,  UPOPT )

      IF ( UPOPT .EQ. 'SHORT' ) THEN

C
C        Retrieve short message:
C
         CALL GETSMS ( MSG )


      ELSE IF ( UPOPT .EQ. 'EXPLAIN' ) THEN

C
C        Get current short message; then get explanation
C        corresponding to current short error message:
C

         CALL GETSMS  ( SHRTMS )
         CALL EXPLN   ( SHRTMS, MSG )


      ELSE IF ( UPOPT .EQ. 'LONG' ) THEN

C
C        Grab long error message:
C
         CALL GETLMS  ( MSG )

      ELSE

C
C        Invalid value of OPTION!!  Signal error, and set long
C        error message as well:
C
         LOCOPT  =  OPTION

         CALL SETMSG (
     .                'GETMSG: An invalid value of OPTION was'        //
     .                ' input.  Valid choices are ''SHORT'','         //
     .                '       ''EXPLAIN'', or ''LONG''.  The value'   //
     .                ' that was input was:  '                        //
     .                 LOCOPT                                      )

         CALL SIGERR ( 'SPICE(INVALIDMSGTYPE)' )



      END IF


      END

C$Procedure SETMSG  ( Set Long Error Message )

      SUBROUTINE SETMSG ( MSG )

C$ Abstract
C
C     Set the value of the current long error message.
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

      CHARACTER*(*)                 MSG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MSG        I   A long error message.
C
C$ Detailed_Input
C
C     MSG      is a "long" error message.
C
C              MSG is a detailed description of the error.
C              MSG is supposed to start with the name of the
C              module which detected the error, followed by a
C              colon. Example:
C
C                 'RDTEXT:  There are no more free logical units'
C
C              Only the first LMSGLN characters of MSG are stored;
C              any further characters are truncated.
C
C              Generally, MSG will be stored internally by the SPICELIB
C              error handling mechanism. The only exception
C              is the case in which the user has commanded the
C              toolkit to ``ignore'' the error indicated by MSG.
C
C              As a default, MSG will be output to the screen.
C              See the required reading file for a discussion of how
C              to customize toolkit error handling behavior, and
C              in particular, the disposition of MSG.
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
C     Error free.
C
C     1)  This routine does not detect any errors.
C
C         However, this routine is part of the interface to the
C         SPICELIB error handling mechanism. For this reason,
C         this routine does not participate in the trace scheme,
C         even though it has external references.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The SPICELIB routine SIGERR should always be called
C     AFTER this routine is called, when an error is detected.
C
C     The effects of this routine are:
C
C        1. If acceptance of a new long error message is
C            allowed:
C
C            MSG will be stored internally. As a result,
C            The SPICELIB routine, GETMSG, will be able to
C            retrieve MSG, until MSG has been ``erased''
C            by a call to RESET, or overwritten by another
C            call to SETMSG.
C
C
C        2. If acceptance of a new long error message is not allowed,
C            a call to this routine has no effect.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a user-defined error message, including both the
C        short and long messages, providing the value of an integer
C        and a double precision variables within the long message,
C        and signal the error.
C
C
C        Example code begins here.
C
C
C              PROGRAM SETMSG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Set long error message, with two different MARKER
C        C     strings where the value of the variables will go.
C        C     Our markers are '#' and 'XX'.
C        C
C              CALL SETMSG ( 'LONG MESSAGE. Invalid operation value. '
C             .         //   '  The value was #.  Left endpoint '
C             .         //   'exceeded right endpoint.  The left '
C             .         //   'endpoint was:  XX.'                     )
C
C        C
C        C     Insert the integer number where the # is now.
C        C
C              CALL ERRINT ( '#',  5  )
C
C        C
C        C     Insert a double precision number where the XX is now.
C        C
C              CALL ERRDP  ( 'XX', 910.26111991D0 )
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
C        exceeded right endpoint. The left endpoint was: 9.1026111991***
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
C        Warning: incomplete output. 7 lines extended past the right
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
C     1)  SIGERR must be called once after each call to this routine.
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
C-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example based on existing fragments.
C
C-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN. Miscellaneous header fixes were
C        made. Some indentation and vertical white space abnormalities
C        in the code were fixed. Some dubious comments were deleted
C        from the code.
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
C     set long error message
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN. Miscellaneous header fixes were
C        made. Some indentation and vertical white space abnormalities
C        in the code were fixed. Some dubious comments were deleted
C        from the code.
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Declarations of the unused variable STAT and unused function
C         ACCEPT removed.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               ALLOWD

C
C     We store the long error message only when updates
C     of the long message are allowed:
C
      IF ( ALLOWD() ) THEN

         CALL PUTLMS ( MSG )

      END IF

      END

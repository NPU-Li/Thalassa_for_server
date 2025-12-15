C$Procedure SIGERR ( Signal Error Condition )

      SUBROUTINE SIGERR ( MSG )

C$ Abstract
C
C     Inform the SPICELIB error processing mechanism that an error has
C     occurred, and specify the type of error.
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
C     MSG        I   A short error message.
C
C$ Detailed_Input
C
C     MSG      is a ``short'' error message.
C
C              MSG indicates the type of error that has occurred.
C
C              The exact format that MSG must follow is
C              described in the required reading file, error.req.
C              Only the first 25 characters of MSG will be stored;
C              additional characters will be truncated.
C
C              Generally, MSG will be stored internally by the SPICELIB
C              error handling mechanism. The only exception
C              is the case in which the user has commanded the error
C              handling mechanism to ``ignore'' the error indicated by
C              MSG.
C
C              As a default, MSG will be output to the screen.
C              See the required reading file for a discussion of how
C              to customize SPICELIB error handling behavior, and
C              in particular, the disposition of MSG.
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
C     First of all, please read the ``required reading'' file.
C     The information below will make a lot more sense if you do.
C
C     This is the routine used by SPICELIB to signal the detection
C     of errors.
C
C     Making a call to SIGERR is the way to inform the error
C     handling mechanism that an error has occurred.
C
C     Specifically, the effects of this routine are:
C
C     1. If responding to the error indicated by MSG has
C         not been disabled:
C
C         a. MSG will be stored internally. As a result,
C            The SPICELIB routine, GETMSG, will be able to
C            retrieve MSG, until MSG has been ``erased''
C            by a call to RESET, or overwritten by another
C            call to SIGERR.
C
C         b. An indication of an ``error condition'' will
C            be set internally. The SPICELIB logical
C            function, FAILED, will take the value, .TRUE.,
C            as a result, until the error condition is
C            negated by a call to RESET.
C
C         c. All of the error messages that have been selected
C            for automatic output via ERRPRT will be output.
C            The set of messages is some subset of { short message,
C            long message, explanation of short message,
C            traceback, and default message }.
C
C         d. If the error response mode is not 'RETURN',
C            Setting of the long error message is enabled.
C            You can't re-set the long error message, once
C            it has been set, without first signaling an error.
C
C         e. In 'RETURN' mode, further signaling of error
C            messages, and setting of the long message, are disabled.
C            (These capabilities can be re-enabled by calling RESET).
C
C
C     2. If the error handling mechanism has been commanded to
C         ``ignore'' the error indicated by MSG, the call to SIGERR
C         has no effect.
C
C     If you wish to set the long error message, call
C     SETMSG BEFORE calling SIGERR.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
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
C              PROGRAM SIGERR_EX1
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
C
C     2) Create a user-defined error message, including only the
C        short messages, and signal the error.
C
C
C        Example code begins here.
C
C
C              PROGRAM SIGERR_EX2
C              IMPLICIT NONE
C
C        C
C        C     Signal the error; the short message is given by
C        C     SIGERR input argument.
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
C          Oh, by the way:  The SPICELIB error handling actions are U***
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
C        Note that the execution of this program produces the same
C        SPICE(USERDEFINED) error as in Example #1, but in this case,
C        only the short message is presented.
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 13-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code examples.
C
C-    SPICELIB Version 2.1.1, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 2.1.0, 26-JUL-1996 (KRG)
C
C        The STOP statement in this subroutine has been replaced
C        with a call to the subroutine BYEBYE which passes a failure
C        status to the operating system or command shell/environment
C        on all platforms which support this capability.
C
C-    SPICELIB Version 2.0.0, 22-APR-1996 (KRG)
C
C        This subroutine has been modified in an attempt to improve
C        the general performance of the SPICELIB error handling
C        mechanism. The specific modification has been to change the
C        type of the error action from a short character string to an
C        integer. This change is backwardly incompatible because the
C        type has changed.
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
C     signal error condition
C
C-&


C
C     SPICELIB functions:
C

      LOGICAL               FAILED
      LOGICAL               SETERR
      LOGICAL               ACCEPT

C
C     Local Parameters
C
C     Define mnemonics for the integer action codes used by the error
C     handling. See ERRACT for the character string equivalents used.
C
      INTEGER               IABRT
      PARAMETER           ( IABRT =          1 )

      INTEGER               IREPRT
      PARAMETER           ( IREPRT = IABRT + 1 )

      INTEGER               IRETRN
      PARAMETER           ( IRETRN = IREPRT + 1 )

      INTEGER               IIGNOR
      PARAMETER           ( IIGNOR = IRETRN + 1 )

      INTEGER               IDEFLT
      PARAMETER           ( IDEFLT = IIGNOR + 1 )
C
C     Length for output messages default settings.
C
      INTEGER               DEFLEN
      PARAMETER           ( DEFLEN = 40)
C
C     Local Variables:
C
      CHARACTER*(DEFLEN)    DEFMSG
      CHARACTER*(DEFLEN)    ERRMSG

      INTEGER               ACTION

      LOGICAL               STAT
      SAVE
C
C     Initial Values
C
C     Define the default error message strings for OUTMSG.
C
      DATA       DEFMSG / 'SHORT, EXPLAIN, LONG, TRACEBACK, DEFAULT' /
      DATA       ERRMSG / 'SHORT, EXPLAIN, LONG, TRACEBACK'          /
C
C     We must first check whether the error indicated by
C     MSG is one we're supposed to ignore...
C
C     There are two cases in which we do not want to respond
C     to the signaled error.
C
C     1.  When the error action is 'IGNORE'.  The user has
C         commanded that all messages be ignored.
C
C     2.  When the error action is 'RETURN', and an error
C         condition already exists.  We wish to preserve the
C         error data from the FIRST error until the user/
C         user's program has reset the error status via
C         a call to RESET.
C

      CALL GETACT ( ACTION )

      IF ( ACTION .NE. IIGNOR ) THEN

         IF ( ( ACTION .NE. IRETRN ) .OR. ( .NOT. FAILED() ) ) THEN
C
C           This one's for real.  Indicate an error condition, and
C           store the short error message.
C
C           Note:  the following strange -- looking function
C           reference sets the toolkit error status.  STAT
C           doesn't have any meaning.
C
            STAT = SETERR ( .TRUE. )

            CALL PUTSMS ( MSG )
C
C           Create a frozen copy of the traceback:
C
            CALL FREEZE

C
C           Now we output the error data that are available at this
C           time, and whose output has been enabled.  The choice of
C           data is any combination of the following:
C
C              1. The short error message
C              2. The explanation of the short error message
C              3. The traceback
C              4. The long error message
C              5. The default message
C
C           Note that OUTMSG outputs only those messages which have
C           been SELECTED for output, via a call to ERRPRT, except
C           if the error action is DEFAULT.  In that case, the
C           default message selection applies.
C
            IF (  ACTION .NE. IDEFLT ) THEN

               CALL OUTMSG ( ERRMSG )

            ELSE

               CALL OUTMSG ( DEFMSG )

            END IF

            IF (  ACTION .EQ. IRETRN )  THEN
C
C              Don't accept new long error messages or updates
C              to current long error message:
C              (STAT has no meaning).
C
               STAT  =  ACCEPT ( .FALSE. )

            ELSE

               STAT  =  ACCEPT ( .TRUE. )

            END IF

         ELSE

            STAT  =  ACCEPT ( .FALSE. )

         END IF

      END IF
C
C     We could be in ABORT or DEFAULT mode.
C

      IF ( ( ACTION .EQ. IDEFLT ) .OR. ( ACTION .EQ. IABRT ) ) THEN

         CALL BYEBYE ( 'FAILURE' )

      END IF

C
C     That's all, folks!
C
      RETURN
      END

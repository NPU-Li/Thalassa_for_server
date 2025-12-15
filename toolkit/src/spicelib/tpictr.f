C$Procedure TPICTR ( Create a Time Format Picture )

      SUBROUTINE TPICTR ( SAMPLE, PICTUR, OK, ERRMSG )

C$ Abstract
C
C     Create a time format picture suitable for use by the routine
C     TIMOUT from a given sample time string.
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
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         SAMPLE
      CHARACTER*(*)         PICTUR
      LOGICAL               OK
      CHARACTER*(*)         ERRMSG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SAMPLE     I   is a sample date time string
C     PICTUR     O   is a format picture that describes SAMPLE
C     OK         O   indicates success or failure to parse SAMPLE
C     ERRMSG     O   a diagnostic returned if SAMPLE cannot be parsed
C
C$ Detailed_Input
C
C     SAMPLE   is a representative time string that to use
C              as a model to format time strings.
C
C$ Detailed_Output
C
C     PICTUR   is a format picture suitable for use with the SPICE
C              routine TIMOUT. This picture when used to format
C              the appropriate  epoch via TIMOUT will yield the same
C              time components in the same order as the components
C              in SAMPLE.
C
C              Picture should be declared to be at least 80 characters
C              in length. If Picture is not sufficiently large
C              to contain the format picture, the picture will
C              be truncated on the right.
C
C     OK       is a logical flag. If all of the components of SAMPLE
C              are recognizable, OK will be returned with the value
C              .TRUE. If some part of PICTUR cannot be parsed,
C              OK will be returned with the value .FALSE.
C
C     ERRMSG   is a diagnostic message  that indicates what part of
C              SAMPLE was not recognizable. If SAMPLE can be
C              successfully parsed, OK will be .TRUE. and ERRMSG will
C              be returned as a blank string. If ERRMSG does not
C              have sufficient room (up to 400 characters) to
C              contain the full message, the message will be truncated
C              on the right.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  All problems with the inputs are reported via OK and ERRMSG.
C
C     2)  If a format picture can not be created from the sample
C         time string, PICTUR is returned as a blank string.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Although the routine TIMOUT provides SPICE users with a great
C     deal of flexibility in formatting time strings, users must
C     master the means by which a time picture is constructed
C     suitable for use by TIMOUT.
C
C     This routine allows SPICE users to supply a sample time string
C     from which a corresponding time format picture can be created,
C     freeing users from the task of mastering the intricacies of
C     the routine TIMOUT.
C
C     Note that TIMOUT can produce many time strings whose patterns
C     can not be discerned by this routine. When such outputs are
C     called for, the user must consult TIMOUT and construct the
C     appropriate format picture "by hand." However, these exceptional
C     formats are not widely used and are not generally recognizable
C     to an uninitiated reader.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Given a sample with the format of the UNIX date string
C        local to California, create a SPICE time picture for use
C        in TIMOUT.
C
C        Using that SPICE time picture, convert a series of ephemeris
C        times to that picture format.
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
C              PROGRAM TPICTR_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               ERRLEN
C              PARAMETER           ( ERRLEN  = 400 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN  = 64  )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(ERRLEN)    ERR
C              CHARACTER*(TIMLEN)    PICTUR
C              CHARACTER*(TIMLEN)    SAMPLE
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(TIMLEN)    UTCSTR
C
C              DOUBLE PRECISION      ET
C
C              LOGICAL               OK
C
C        C
C        C     Load LSK file.
C        C
C              CALL FURNSH ( 'naif0012.tls' )
C
C        C
C        C     Create the required time picture.
C        C
C              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111'
C
C              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR )
C
C              IF ( .NOT. OK ) THEN
C
C                 WRITE(*,*) 'Invalid time picture.'
C                 WRITE(*,*) ERR
C
C              ELSE
C
C        C
C        C        Convert the input UTC time to ephemeris time.
C        C
C                 UTCSTR = '24 Mar 2018  16:23:00 UTC'
C                 CALL STR2ET ( UTCSTR, ET )
C
C        C
C        C         Now convert ET to the desired output format.
C        C
C                  CALL TIMOUT ( ET, PICTUR, TIMSTR )
C                  WRITE (*,*) 'Sample format: ', SAMPLE
C                  WRITE (*,*) 'Time picture : ', PICTUR
C                  WRITE (*,*)
C                  WRITE (*,*) 'Input UTC    : ', UTCSTR
C                  WRITE (*,*) 'Output       : ', TIMSTR
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Sample format: Thu Oct 01 11:11:11 PDT 1111
C         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7
C
C         Input UTC    : 24 Mar 2018  16:23:00 UTC
C         Output       : Sat Mar 24 09:23:00 PDT 2018
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
C-    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR)
C
C        Changed output argument name ERROR to ERRMSG for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard.
C        Converted the existing code fragments into complete example
C        and added reference to required LSK.
C
C-    SPICELIB Version 1.0.1, 16-MAR-1999 (WLT)
C
C        Corrected a minor spelling error in the header comments.
C
C-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
C
C-&


C$ Index_Entries
C
C     Use a sample time string to produce a time format picture
C
C-&


C
C     Local variables
C
      CHARACTER*(5)         TYPE
      CHARACTER*(8)         MODIFY   ( 5 )

      DOUBLE PRECISION      TVEC     ( 10 )

      INTEGER               NTVEC

      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV

C
C     This routine is really just a front for one aspect of
C     the routine TPARTV.
C
      ERRMSG  = ' '

      CALL TPARTV ( SAMPLE,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, ERRMSG )


      IF ( PICTUR .EQ. ' ' ) THEN
         OK = .FALSE.
      ELSE
         OK    = .TRUE.
         ERRMSG = ' '
      END IF

      RETURN
      END

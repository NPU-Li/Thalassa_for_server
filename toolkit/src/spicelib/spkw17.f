C$Procedure SPKW17 ( SPK, write a type 17 segment )

      SUBROUTINE SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST,
     .                    SEGID,  EPOCH, EQEL,   RAPOL, DECPOL  )

C$ Abstract
C
C     Write an SPK segment of type 17 given a type 17 data record.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EQEL  ( 9 )
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      DECPOL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an SPK file open for writing.
C     BODY       I   Body code for ephemeris object.
C     CENTER     I   Body code for the center of motion of the body.
C     FRAME      I   The reference frame of the states.
C     FIRST      I   First valid time for which states can be computed.
C     LAST       I   Last valid time for which states can be computed.
C     SEGID      I   Segment identifier.
C     EPOCH      I   Epoch of elements in seconds past J2000.
C     EQEL       I   Array of equinoctial elements.
C     RAPOL      I   Right Ascension of the reference plane's pole.
C     DECPOL     I   Declination of the reference plane's pole.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing.
C
C     BODY     is the NAIF ID for the body whose states are
C              to be recorded in the SPK file.
C
C     CENTER   is the NAIF ID for the center of motion associated
C              with BODY.
C
C     FRAME    is the reference frame that states are referenced to,
C              for example 'J2000'.
C
C     FIRST,
C     LAST     are the bounds on the ephemeris times, expressed as
C              seconds past J2000.
C
C     SEGID    is the segment identifier. An SPK segment identifier
C              may contain up to 40 characters.
C
C     EPOCH    is the epoch of equinoctial elements in seconds
C              past the J2000 epoch.
C
C     EQEL     is an array of 9 double precision numbers that
C              are the equinoctial elements for some orbit relative
C              to the equatorial frame of a central body.
C
C                 Note: The Z-axis of the equatorial frame is the
C                 direction of the pole of the central body relative
C                 to FRAME. The X-axis is given by the cross product of
C                 the Z-axis of FRAME with the direction of the pole of
C                 the central body. The Y-axis completes a right handed
C                 frame.
C
C              The specific arrangement of the elements is spelled
C              out below. The following terms are used in the
C              discussion of elements of EQEL:
C
C                 INC  --- inclination of the orbit
C                 ARGP --- argument of periapse
C                 NODE --- longitude of the ascending node
C                 E    --- eccentricity of the orbit
C                 M0   --- mean anomaly
C
C              EQEL(1)   is the semi-major axis (A) of the orbit in km.
C
C              EQEL(2)   is the value of H at the specified epoch:
C
C                           H =  E * SIN( ARGP + NODE )
C
C              EQEL(3)   is the value of K at the specified epoch:
C
C                           K =  E * COS( ARGP + NODE )
C
C              EQEL(4)   is the mean longitude at the epoch of the
C                        elements measured in radians:
C
C                           ( M0 + ARGP + NODE )
C
C              EQEL(5)   is the value of P at the specified epoch:
C
C                           P =  TAN( INC/2 ) * SIN( NODE )
C
C              EQEL(6)   is the value of Q at the specified epoch:
C
C                           Q =  TAN( INC/2 ) * COS( NODE )
C
C              EQEL(7)   is the rate of the longitude of periapse
C                        at the epoch of the elements.
C
C                           ( dARGP/dt + dNODE/dt )
C
C                        This rate is assumed to hold for all time. The
C                        rate is measured in radians per second.
C
C              EQEL(8)   is the derivative of the mean longitude:
C
C                           ( dM0/dt + dARGP/dt + dNODE/dt )
C
C                        This rate is assumed to be constant and is
C                        measured in radians/second.
C
C              EQEL(9)   is the rate of the longitude of the ascending
C                        node:
C
C                           ( dNODE/dt )
C
C                        This rate is measured in radians per second.
C
C     RAPOL    is the Right Ascension of the pole of the reference
C              plane relative to FRAME measured in radians.
C
C     DECPOL   is the declination of the pole of the reference plane
C              relative to FRAME measured in radians.
C
C$ Detailed_Output
C
C     None.
C
C     The routine writes an SPK type 17 segment to the file attached to
C     HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the semi-major axis is less than or equal to zero, the
C         error SPICE(BADSEMIAXIS) is signaled.
C
C     2)  If the eccentricity of the orbit corresponding to the values
C         of H and K ( EQEL(2) and EQEL(3) ) is greater than 0.9, the
C         error SPICE(ECCOUTOFRANGE) is signaled.
C
C     3)  If the segment identifier has more than 40 non-blank
C         characters, the error SPICE(SEGIDTOOLONG) is signaled.
C
C     4)  If the segment identifier contains non-printing characters,
C         the error SPICE(NONPRINTABLECHARS) is signaled.
C
C     5)  If there are inconsistencies in the BODY, CENTER, FRAME or
C         FIRST and LAST times, an error is signaled by a routine in
C         the call tree of this routine.
C
C$ Files
C
C     A new type 17 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 17 data segment to the open SPK
C     file according to the format described in the type 17 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that at a given time you have the classical elements
C        of Daphnis relative to the equatorial frame of Saturn. These
C        can be converted to equinoctial elements and stored in an SPK
C        file as a type 17 segment so that Daphnis can be used within
C        the SPK subsystem of the SPICE system.
C
C        The example code shown below creates an SPK type 17 kernel
C        with a single segment using such data.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKW17_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40 )
C
C              CHARACTER*(*)         SPK17
C              PARAMETER           ( SPK17  = 'spkw17_ex1.bsp' )
C
C        C
C        C     The SPK type 17 segment will contain data for Daphnis
C        C     (ID 635) with respect to Saturn (ID 699) in the J2000
C        C     reference frame.
C        C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 635     )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 699     )
C
C              CHARACTER*(*)         FRMNAM
C              PARAMETER           ( FRMNAM = 'J2000' )
C
C        C
C        C     This is the list of parameters used to represent the
C        C     classical elements:
C        C
C        C        Variable     Meaning
C        C        --------     ---------------------------------------
C        C        A            Semi-major axis in km.
C        C        ECC          Eccentricity of orbit.
C        C        INC          Inclination of orbit.
C        C        NODE         Longitude of the ascending node at
C        C                     epoch.
C        C        OMEGA        Argument of periapse at epoch.
C        C        M            Mean anomaly at epoch.
C        C        DMDT         Mean anomaly rate in radians/second.
C        C        DNODE        Rate of change of longitude of
C        C                     ascending node in radians/second.
C        C        DOMEGA       Rate of change of argument of periapse
C        C                     in radians/second.
C        C        EPOCH        The epoch of the elements in seconds
C        C                     past the J2000 epoch.
C        C
C              DOUBLE PRECISION      A
C              PARAMETER           ( A      =  1.36505608D+05  )
C
C              DOUBLE PRECISION      ECC
C              PARAMETER           ( ECC    = -2.105898062D-05 )
C
C              DOUBLE PRECISION      INC
C              PARAMETER           ( INC    = -3.489710429D-05 )
C
C              DOUBLE PRECISION      NODE
C              PARAMETER           ( NODE   = -3.349237456D-02 )
C
C              DOUBLE PRECISION      OMEGA
C              PARAMETER           ( OMEGA  =  1.52080206722D0 )
C
C              DOUBLE PRECISION      M
C              PARAMETER           ( M      =  1.21177109734D0 )
C
C              DOUBLE PRECISION      DMDT
C              PARAMETER           ( DMDT   =  1.218114014D-04 )
C
C              DOUBLE PRECISION      DNODE
C              PARAMETER           ( DNODE  = -5.96845468D-07  )
C
C              DOUBLE PRECISION      DOMEGA
C              PARAMETER           ( DOMEGA =  1.196601093D-06 )
C
C              DOUBLE PRECISION      EPOCH
C              PARAMETER           ( EPOCH  = 0.D0             )
C
C        C
C        C     In addition, the SPKW17 routine requires the Right
C        C     Ascension and Declination of the pole of the
C        C     reference plane relative to the J2000 frame, in radians.
C        C
C              DOUBLE PRECISION      RAPOL
C              PARAMETER           ( RAPOL  = 7.08332284D-01 )
C
C              DOUBLE PRECISION      DECPOL
C              PARAMETER           ( DECPOL = 1.45800286D0   )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      EQEL   ( 9 )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment.
C        C
C              FIRST  =  504878400.D0
C              LAST   = 1578657600.D0
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, so set to zero.
C        C
C              NCOMCH = 0
C
C        C
C        C     Internal file name and segment ID.
C        C
C              IFNAME = 'Test for type 17 SPK internal file name'
C              SEGID  = 'SPK type 17 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK17, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Convert the classical elements to equinoctial elements
C        C     (in the order compatible with type 17).
C        C
C              EQEL(1) = A
C              EQEL(2) = ECC * SIN ( OMEGA + NODE )
C              EQEL(3) = ECC * COS ( OMEGA + NODE )
C
C              EQEL(4) = M + OMEGA + NODE
C
C              EQEL(5) = TAN( INC/2.D0 ) * SIN( NODE )
C              EQEL(6) = TAN( INC/2.D0 ) * COS( NODE )
C
C              EQEL(7) =        DOMEGA + DNODE
C              EQEL(8) = DMDT + DOMEGA + DNODE
C              EQEL(9) = DNODE
C
C        C
C        C     Now add the segment.
C        C
C              CALL SPKW17 ( HANDLE, BODY,  CENTER, FRMNAM,
C             .              FIRST,  LAST,  SEGID,  EPOCH,
C             .              EQEL,   RAPOL, DECPOL          )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 17 exists in
C        the output directory.
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C-    SPICELIB Version 1.0.1, 24-JUN-1999 (WLT)
C
C        Corrected typographical errors in the header.
C
C-    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT)
C
C-&


C$ Index_Entries
C
C     Write a type 17 SPK segment
C
C-&


C
C     SPICELIB Functions
C
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Variables
C
C
C     Segment descriptor size
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )

C
C     Segment identifier size
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

C
C     SPK data type
C
      INTEGER               TYPE
      PARAMETER           ( TYPE    = 17 )

C
C     Range of printing characters
C
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )

C
C     Number of items in a segment
C
      INTEGER               DATSIZ
      PARAMETER           ( DATSIZ  = 12 )



      DOUBLE PRECISION      A
      DOUBLE PRECISION      DESCR  ( NS )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      RECORD ( DATSIZ )

      INTEGER               I
      INTEGER               VALUE


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW17')

C
C     Fetch the various entities from the inputs and put them into
C     the data record, first the epoch.
C
      RECORD(1) = EPOCH

C
C     The trajectory pole vector.
C
      CALL MOVED ( EQEL, 9, RECORD(2) )


      RECORD(11) = RAPOL
      RECORD(12) = DECPOL


      A      = RECORD(2)
      H      = RECORD(3)
      K      = RECORD(4)
      ECC    = DSQRT( H*H + K*K )

C
C     Check all the inputs here for obvious failures.  It's much
C     better to check them now and quit than it is to get a bogus
C     segment into an SPK file and diagnose it later.
C

      IF ( A .LE. 0 ) THEN

         CALL SETMSG ( 'The semimajor axis supplied to the '
     .   //            'SPK type 17 evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.' )
         CALL ERRDP  ( '#', A                  )
         CALL SIGERR ( 'SPICE(BADSEMIAXIS)' )
         CALL CHKOUT ( 'SPKW17'                )
         RETURN

      ELSE IF ( ECC .GT. 0.9D0 ) THEN

         CALL SETMSG ( 'The eccentricity supplied for a type 17 '
     .   //            'segment is greater than 0.9.  It must be '
     .   //            'less than 0.9.'
     .   //            'The value supplied '
     .   //            'to the type 17 evaluator was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKW17'                  )
         RETURN

      END IF

C
C     Make sure the segment identifier is not too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than '
     .   //            '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW17'                                  )
         RETURN

      END IF
C
C     Make sure the segment identifier has only printing characters.
C
      DO I = 1, LASTNB(SEGID)

         VALUE = ICHAR( SEGID(I:I) )

         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '
     .      //            'the nonprintable character having ascii '
     .      //            'code #.'                               )
            CALL ERRINT ( '#',   VALUE                            )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW17'                                )
            RETURN

         END IF

      END DO
C
C     All of the obvious checks have been performed on the input
C     record.  Create the segment descriptor. (FIRST and LAST are
C     checked by SPKPDS as well as consistency between BODY and CENTER).
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, TYPE, FIRST, LAST,
     .              DESCR        )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW17' )
         RETURN
      END IF

C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW17' )
         RETURN
      END IF

      CALL DAFADA ( RECORD, DATSIZ )

      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF

      CALL CHKOUT ( 'SPKW17' )
      RETURN
      END

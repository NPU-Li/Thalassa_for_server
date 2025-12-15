C$Procedure SPKW10 (SPK - write a type 10 segment )

      SUBROUTINE SPKW10 ( HANDLE, BODY,   CENTER, FRAME,  FIRST, LAST,
     .                    SEGID,  CONSTS, N,      ELEMS,  EPOCHS      )

C$ Abstract
C
C     Write an SPK type 10 segment to the file specified by
C     the input HANDLE.
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
C     NAIF_IDS
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'sgparam.inc'


      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      CONSTS ( * )
      INTEGER               N
      DOUBLE PRECISION      ELEMS  ( * )
      DOUBLE PRECISION      EPOCHS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a DAF file open for writing.
C     BODY       I   The NAIF ID code for the body of the segment.
C     CENTER     I   The center of motion for BODY.
C     FRAME      I   The reference frame for this segment.
C     FIRST      I   The first epoch for which the segment is valid.
C     LAST       I   The last  epoch for which the segment is valid.
C     SEGID      I   The string to use for segment identifier.
C     CONSTS     I   Array of geophysical constants for the segment.
C     N          I   The number of element/epoch pairs to be stored
C     ELEMS      I   The collection of "two-line" element sets.
C     EPOCHS     I   The epochs associated with the element sets.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been opened
C              for writing.
C
C     BODY     is the NAIF ID for the body whose states are
C              to be recorded in an SPK file.
C
C     CENTER   is the NAIF ID for the center of motion associated
C              with BODY.
C
C     FRAME    is the reference frame that states are referenced to,
C              for example 'J2000'.
C
C     FIRST,
C     LAST     are the bounds on the ephemeris times, expressed as
C              seconds past J2000, for which the states can be used
C              to interpolate a state for BODY.
C
C     SEGID    is the segment identifier. An SPK segment identifier
C              may contain up to 40 characters.
C
C     CONSTS   are the geophysical constants needed for evaluation
C              of the two line elements sets. The order of these
C              constants must be:
C
C                 CONSTS(1) = J2 gravitational harmonic for Earth.
C                 CONSTS(2) = J3 gravitational harmonic for Earth.
C                 CONSTS(3) = J4 gravitational harmonic for Earth.
C
C              These first three constants are dimensionless.
C
C                 CONSTS(4) = KE: Square root of the GM for Earth where
C                             GM is expressed in Earth radii cubed
C                             per minutes squared.
C
C                 CONSTS(5) = QO: High altitude bound for atmospheric
C                             model in km.
C
C                 CONSTS(6) = SO: Low altitude bound for atmospheric
C                             model in km.
C
C                 CONSTS(7) = RE: Equatorial radius of the earth in km.
C
C                 CONSTS(8) = AE: Distance units/earth radius
C                             (normally 1).
C
C              Below are currently recommended values for these
C              items:
C
C                 J2 =    1.082616D-3
C                 J3 =   -2.53881D-6
C                 J4 =   -1.65597D-6
C
C              The next item is the square root of GM for the Earth
C              given in units of earth-radii**1.5/Minute
C
C                 KE =    7.43669161D-2
C
C              The next two items define the top and bottom of the
C              atmospheric drag model used by the type 10 ephemeris
C              type. Don't adjust these unless you understand the full
C              implications of such changes.
C
C                 QO =  120.0D0
C                 SO =   78.0D0
C
C              The ER value is the equatorial radius in km of the Earth
C              as used by NORAD.
C
C                 ER = 6378.135D0
C
C              The value of AE is the number of distance units per
C              Earth radii used by the NORAD state propagation
C              software. The value should be 1 unless you've got a very
C              good understanding of the NORAD routine SGP4 and the
C              affect of changing this value.
C
C                 AE =    1.0D0
C
C     N        is the number of "two-line" element sets and epochs
C              to be stored in the segment.
C
C     ELEMS    is a time-ordered array of two-line elements as supplied
C              in NORAD two-line element files. The I'th set of
C              elements should be stored as shown here:
C
C                 BASE = (I-1)*10
C
C                 ELEMS( BASE + 1  )  = NDT2O in radians/minute**2
C                 ELEMS( BASE + 2  )  = NDD6O in radians/minute**3
C                 ELEMS( BASE + 3  )  = BSTAR
C                 ELEMS( BASE + 4  )  = INCL  in radians
C                 ELEMS( BASE + 5  )  = NODE0 in radians
C                 ELEMS( BASE + 6  )  = ECC
C                 ELEMS( BASE + 7  )  = OMEGA in radians
C                 ELEMS( BASE + 8  )  = M0    in radians
C                 ELEMS( BASE + 9  )  = N0    in radians/minute
C                 ELEMS( BASE + 10 )  = EPOCH of the elements in seconds
C                                       past ephemeris epoch J2000.
C
C              The meaning of these variables is defined by the
C              format of the two-line element files available from
C              NORAD.
C
C     EPOCHS   is an n-dimensional array that contains the epochs
C              (ephemeris seconds past J2000) corresponding to the
C              elements in ELEMS. The I'th epoch must equal the epoch
C              of the I'th element set. EPOCHS must form a strictly
C              increasing sequence.
C
C$ Detailed_Output
C
C     None.
C
C     The routine writes an SPK type 10 segment to the file attached to
C     HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the structure or content of the inputs are invalid, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     2)  If any file access error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine writes a type 10 SPK segment to the SPK file open
C     for writing that is attached to HANDLE.
C
C     The routine GETELM reads two-line element sets, as those
C     distributed by NORAD, and converts them to the elements in units
C     suitable for use in this routine.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that you have collected the two-line element data
C        for a spacecraft with NORAD ID 18123. The following example
C        code demonstrates how you could go about creating a type 10
C        SPK segment.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: spkw10_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name           Contents
C              ---------           ------------------------------------
C              naif0012.tls        Leapseconds
C              geophysical.ker     geophysical constants for evaluation
C                                  of two-line element sets.
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0012.tls',
C                                  'geophysical.ker'  )
C
C           \begintext
C
C           The geophysical.ker is a PCK file that is provided with the
C           SPICE toolkit under the "/data" directory.
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKW10_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      SPD
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40               )
C
C              INTEGER               PNAMLN
C              PARAMETER           ( PNAMLN = 2                )
C
C              CHARACTER*(*)         SPK10
C              PARAMETER           ( SPK10  = 'spkw10_ex1.bsp' )
C
C              INTEGER               TLELLN
C              PARAMETER           ( TLELLN = 69               )
C
C        C
C        C     The SPK type 10 segment will contain 18 two-line
C        C     elements sets for the NORAD spacecraft 18123 with
C        C     respect to the Earth (ID 399) in the J2000 reference
C        C     frame.
C        C
C        C     As stated in the naif_ids required reading, for Earth
C        C     orbiting spacecraft lacking a DSN identification code,
C        C     the NAIF ID is derived from the tracking ID assigned to
C        C     it by NORAD via:
C        C
C        C        NAIF ID = -100000 - NORAD ID code
C        C
C              INTEGER               TLESSZ
C              PARAMETER           ( TLESSZ = 9       )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = -118123 )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 399     )
C
C              CHARACTER*(*)         FRMNAM
C              PARAMETER           ( FRMNAM = 'J2000' )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(PNAMLN)    NOADPN ( 8           )
C              CHARACTER*(NAMLEN)    SEGID
C              CHARACTER*(TLELLN)    TLE    ( 2  * TLESSZ )
C
C              DOUBLE PRECISION      CONSTS ( 8           )
C              DOUBLE PRECISION      ELEMS  ( 10 * TLESSZ )
C              DOUBLE PRECISION      EPOCHS (      TLESSZ )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               N
C              INTEGER               NCOMCH
C
C        C
C        C     These are the variables that will hold the constants
C        C     required by SPK type 10. These constants are available
C        C     from the loaded PCK file, which provides the actual
C        C     values and units as used by NORAD propagation model.
C        C
C        C        Constant   Meaning
C        C        --------   ------------------------------------------
C        C        J2         J2 gravitational harmonic for Earth.
C        C        J3         J3 gravitational harmonic for Earth.
C        C        J4         J4 gravitational harmonic for Earth.
C        C        KE         Square root of the GM for Earth.
C        C        QO         High altitude bound for atmospheric model.
C        C        SO         Low altitude bound for atmospheric model.
C        C        ER         Equatorial radius of the Earth.
C        C        AE         Distance units/earth radius.
C        C
C              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE',
C             .                         'QO', 'SO', 'ER', 'AE'  /
C
C        C
C        C     Define the Two-Line Element sets.
C        C
C              TLE(1)  = '1 18123U 87 53  A 87324.61041692 -.00000023'
C             .      //                   '  00000-0 -75103-5 0 00675'
C              TLE(2)  = '2 18123  98.8296 152.0074 0014950 168.7820 '
C             .      //                   '191.3688 14.12912554 21686'
C              TLE(3)  = '1 18123U 87 53  A 87326.73487726  .00000045'
C             .      //                   '  00000-0  28709-4 0 00684'
C              TLE(4)  = '2 18123  98.8335 154.1103 0015643 163.5445 '
C             .      //                   '196.6235 14.12912902 21988'
C              TLE(5)  = '1 18123U 87 53  A 87331.40868801  .00000104'
C             .      //                   '  00000-0  60183-4 0 00690'
C              TLE(6)  = '2 18123  98.8311 158.7160 0015481 149.9848 '
C             .      //                   '210.2220 14.12914624 22644'
C              TLE(7)  = '1 18123U 87 53  A 87334.24129978  .00000086'
C             .      //                   '  00000-0  51111-4 0 00702'
C              TLE(8)  = '2 18123  98.8296 161.5054 0015372 142.4159 '
C             .      //                   '217.8089 14.12914879 23045'
C              TLE(9)  = '1 18123U 87 53  A 87336.93227900 -.00000107'
C             .      //                   '  00000-0 -52860-4 0 00713'
C              TLE(10) = '2 18123  98.8317 164.1627 0014570 135.9191 '
C             .      //                   '224.2321 14.12910572 23425'
C              TLE(11) = '1 18123U 87 53  A 87337.28635487  .00000173'
C             .      //                   '  00000-0  10226-3 0 00726'
C              TLE(12) = '2 18123  98.8284 164.5113 0015289 133.5979 '
C             .      //                   '226.6438 14.12916140 23475'
C              TLE(13) = '1 18123U 87 53  A 87339.05673569  .00000079'
C             .      //                   '  00000-0  47069-4 0 00738'
C              TLE(14) = '2 18123  98.8288 166.2585 0015281 127.9985 '
C             .      //                   '232.2567 14.12916010 24908'
C              TLE(15) = '1 18123U 87 53  A 87345.43010859  .00000022'
C             .      //                   '  00000-0  16481-4 0 00758'
C              TLE(16) = '2 18123  98.8241 172.5226 0015362 109.1515 '
C             .      //                   '251.1323 14.12915487 24626'
C              TLE(17) = '1 18123U 87 53  A 87349.04167543  .00000042'
C             .      //                   '  00000-0  27370-4 0 00764'
C              TLE(18) = '2 18123  98.8301 176.1010 0015565 100.0881 '
C             .      //                   '260.2047 14.12916361 25138'
C
C        C
C        C     Load the PCK file that provides the geophysical
C        C     constants required for the evaluation of the two-line
C        C     elements sets. Load also an LSK, as it is required by
C        C     GETELM to perform time conversions. Use a metakernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'spkw10_ex1.tm' )
C
C        C
C        C     Retrieve the data from the kernel, and place it on
C        C     the CONSTS array.
C        C
C              DO I = 1, 8
C
C                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, CONSTS(I) )
C
C              END DO
C
C        C
C        C     Convert the Two Line Elements lines to the
C        C     element sets.
C        C
C              DO I = 1, TLESSZ
C
C                 CALL GETELM ( 1950,      TLE( (I-1)*2 + 1 ),
C             .                 EPOCHS(I), ELEMS( (I-1)*10 + 1 ) )
C
C              END DO
C
C        C
C        C     Define the beginning and end of the segment to be
C        C     -/+ 12 hours from the first and last epochs,
C        C     respectively.
C        C
C              FIRST = EPOCHS(1     ) - 0.5D0 * SPD()
C              LAST  = EPOCHS(TLESSZ) + 0.5D0 * SPD()
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
C              IFNAME = 'Test for type 10 SPK internal file name'
C              SEGID  = 'SPK type 10 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK10, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Now add the segment.
C        C
C              CALL SPKW10 ( HANDLE, BODY,  CENTER, FRMNAM,
C             .              FIRST,  LAST,  SEGID,  CONSTS,
C             .              TLESSZ, ELEMS, EPOCHS         )
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
C        screen. After run completion, a new SPK type 10 exists in
C        the output directory.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
C          Propagation of the NORAD Element Sets," U.S. Air Force
C          Aerospace Defense Command, Colorado Springs, CO, 1980.
C
C     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
C          Space Command Element Sets,"  U.S. Air Force Aerospace
C          Defense Command, Colorado Springs, CO, 1986.
C
C     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
C          Orbit Modeling in the U. S. Space Surveillance System,"
C          Journal of Guidance, Control, and Dynamics. 27(2):174-185,
C          2004.
C
C     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
C          Spacetrack Report #3," paper AIAA 2006-6753 presented at the
C          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
C          August 21-24, 2006.
C
C$ Author_and_Institution
C
C     M. Costa Sitja     (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-NOV-2021 (JDR) (MCS)
C
C        Added IMPLICIT NONE statement.
C
C        Corrected the expected order of QO, SO and ER in the detailed
C        description of the input argument GEOPHS and the input element
C        names in ELEMS.
C
C        Added Spacetrack Report #3 to literature references and
C        NAIF_IDS to the list of required readings.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C-    SPICELIB Version 1.0.2, 30-OCT-2006 (BVS)
C
C        Deleted "inertial" from the FRAME description in the $Brief_I/O
C        section of the header.
C
C-    SPICELIB Version 1.0.1, 21-JUN-1999 (WLT)
C
C        Cleaned up the header.
C
C-    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT)
C
C-&


C$ Index_Entries
C
C     Write a type 10 SPK segment
C
C-&


C
C     Spicelib functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Variables
C

C
C     The type of this segment
C
      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = 10 )
C
C     The number of geophysical constants:
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 8 )
C
C     The number of elements per two-line set:
C
      INTEGER               NELEMS
      PARAMETER           ( NELEMS = 10 )

      INTEGER               NUOBL
      PARAMETER           ( NUOBL  = 11 )

      INTEGER               NULON
      PARAMETER           ( NULON  = 12 )

      INTEGER               DNUOBL
      PARAMETER           ( DNUOBL = 13 )

      INTEGER               DNULON
      PARAMETER           ( DNULON = 14 )

      INTEGER               PKTSIZ
      PARAMETER           ( PKTSIZ = DNULON )


      DOUBLE PRECISION      DESCR ( 6 )
      DOUBLE PRECISION      PACKET( PKTSIZ )
      DOUBLE PRECISION      DNUT  ( 4 )

      INTEGER               BASE
      INTEGER               I
      INTEGER               NPKTS
      INTEGER               NEPOCH



C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW10' )
C
C     First we need to create a descriptor for the segment
C     we are about to write.
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, SPKTYP, FIRST, LAST,
     .              DESCR  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW10' )
         RETURN
      END IF

C
C     We've got a valid descriptor, write the data to a DAF
C     segment using the generic segment writer.
C
      NPKTS  = N
      NEPOCH = N


      CALL SGBWFS ( HANDLE, DESCR,   SEGID,
     .              NCONST, CONSTS,  PKTSIZ, EXPCLS          )

      DO I = 1, NEPOCH
C
C        Move the elements into the next packet.
C
         BASE = (I-1)*NELEMS

         CALL MOVED ( ELEMS(BASE + 1), 10, PACKET )
C
C        For each epoch, we need to get the nutation in obliquity,
C        nutation in longitude and mean obliquity.
C
         CALL ZZWAHR  ( EPOCHS(I), DNUT )

         PACKET(NULON)  = DNUT(1)
         PACKET(NUOBL)  = DNUT(2)
         PACKET(DNULON) = DNUT(3)
         PACKET(DNUOBL) = DNUT(4)

C
C        Now write the packet into the generic segment.
C
         CALL SGWFPK ( HANDLE, 1, PACKET, 1, EPOCHS(I) )

      END DO

      CALL SGWES  ( HANDLE )

      CALL CHKOUT ( 'SPKW10' )

      RETURN

      END

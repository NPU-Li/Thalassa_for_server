C$Procedure GETELM ( Get the components from two-line elements )

      SUBROUTINE GETELM ( FRSTYR, LINES, EPOCH, ELEMS )

C$ Abstract
C
C     Parse the "lines" of a two-line element set, returning the
C     elements in units suitable for use in SPICE software.
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
C     PARSING
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               FRSTYR
      CHARACTER*(*)         LINES ( 2  )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      ELEMS ( 10 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRSTYR     I   Year of earliest representable two-line elements.
C     LINES      I   A pair of "lines" containing two-line elements.
C     EPOCH      O   The epoch of the elements in seconds past J2000.
C     ELEMS      O   The elements converted to SPICE units.
C
C$ Detailed_Input
C
C     FRSTYR   is the first year possible for two-line elements.
C              Since two-line elements allow only two digits for
C              the year, some conventions must be followed concerning
C              which century the two digits refer to. FRSTYR
C              is the year of the earliest representable elements.
C              The two-digit year is mapped to the year in
C              the interval from FRSTYR to FRSTYR + 99 that
C              has the same last two digits as the two digit
C              year in the element set. For example if FRSTYR
C              is set to 1960  then the two digit years are mapped
C              as shown in the table below:
C
C                 Two-line         Maps to
C                 element year
C                    00            2000
C                    01            2001
C                    02            2002
C                     .              .
C                     .              .
C                     .              .
C                    58            2058
C                    59            2059
C                   --------------------
C                    60            1960
C                    61            1961
C                    62            1962
C                     .              .
C                     .              .
C                     .              .
C                    99            1999
C
C              Note that if Space Command should decide to represent
C              years in 21st century as 100 + the last two digits
C              of the year (for example: 2015 is represented as 115)
C              instead of simply dropping the first two digits of
C              the year, this routine will correctly map the year
C              as long as you set FRSTYR to some value between 1900
C              and 1999.
C
C     LINES    is a pair of lines of text that comprise a Space
C              command "two-line element" set. These text lines
C              should be the same as they are presented in the
C              two-line element files available from Space Command
C              (formerly NORAD). See $Particulars for a detailed
C              description of the format.
C
C$ Detailed_Output
C
C     EPOCH    is the epoch of the two-line elements supplied via
C              the input array LINES. EPOCH is returned in TDB
C              seconds past J2000.
C
C     ELEMS    is an array containing the elements from the two-line
C              set supplied via the array LINES. The elements are
C              in units suitable for use by the SPICE routines
C              EV2LIN and SPKW10.
C
C              Also note that the elements XNDD6O and BSTAR
C              incorporate the exponential factor present in the
C              input two-line elements in LINES. (See $Particulars
C              below).
C
C                 ELEMS (  1 ) = NDT20 in radians/minute**2
C                 ELEMS (  2 ) = NDD60 in radians/minute**3
C                 ELEMS (  3 ) = BSTAR
C                 ELEMS (  4 ) = INCL  in radians
C                 ELEMS (  5 ) = NODE0 in radians
C                 ELEMS (  6 ) = ECC
C                 ELEMS (  7 ) = OMEGA in radians
C                 ELEMS (  8 ) = M0    in radians
C                 ELEMS (  9 ) = N0    in radians/minute
C                 ELEMS ( 10 ) = EPOCH of the elements in seconds
C                                past ephemeris epoch J2000.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If an error occurs while trying to parse the two-line element
C         set, the error SPICE(BADTLE) is signaled and a description of
C         the detected issue in the "two-line element" set is reported
C         on the long error message.
C
C$ Files
C
C     You must have loaded a SPICE leapseconds kernel into the
C     kernel pool prior to calling this routine.
C
C$ Particulars
C
C     This routine parses a Space Command Two-line element set and
C     returns the orbital elements properly scaled and in units
C     suitable for use by other SPICE software. Input elements
C     are provided in two-lines in accordance with the format
C     required by the two-line element sets available from Space
C     Command (formerly NORAD). See [1] and [2] for details.
C
C     Each of these lines is 69 characters long. The following table
C     define each of the individual fields for lines 1 and 2.
C
C        Line  Column  Type  Description
C        ----  ------  ----  ------------------------------------------
C          1      01     N   Line number of Element Data (always 1)
C          1    03-07    N   Satellite number (NORAD catalog number)
C          1      08     A   Classification (U:Unclassified; S:Secret)
C          1    10-11    N   International designator (last two digits
C                            of launch year).
C          1    12-14    N   International designator (launch number of
C                            the year).
C          1    15-17    A   International designator (piece of the
C                            launch)
C          1    19-20    N   Epoch year (last two digits of year).
C          1    21-32    N   Epoch (day of the year and portion of the
C                            day)
C          1    34-43    N   NDT20: first time derivative of Mean
C                                   Motion
C          1    45-52    N   NDD60: Second time derivative of Mean
C                                   Motion (decimal point assumed)
C          1    54-61    N   BSTAR drag term (decimal point assumed)
C          1      63     N   Ephemeris type
C          1    65-68    N   Element number
C          1      69     N   Checksum.
C
C          2      01     N   Line number of Element Data (always 2)
C          2    03-07    N   Satellite number (must be the same as in
C                            line 1)
C          2    09-16    N   INCL: Inclination, in degrees
C          2    18-25    N   NODE0: Right Ascension of the Ascending
C                                   Node, in degrees
C          2    27-33    N   ECC: Eccentricity (decimal point assumed)
C          2    35-42    N   OMEGA: Argument of Perigee, in degrees
C          2    44-51    N   M0: Mean Anomaly, in degrees
C          2    53-63    N   N0: Mean Motion (revolutions per day)
C          2    64-68    N   Revolution number at epoch
C          2      69     N   Checksum
C
C     The column type A indicates "characters A-Z", the type N means
C     "numeric."
C
C     Column refers to the substring within the line, e.g.
C
C
C  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
C  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
C  ^
C  123456789012345678901234567890123456789012345678901234567890123456789
C           1         2         3         4         5         6
C
C
C        In this example, the satellite number (column 03-07) is 22076.
C
C
C     The "raw" elements used by this routine in the first lines are
C     described in detail below as in several instances exponents and
C     decimal points are implied. Note that the input units are
C     degrees, degrees/day**n and revolutions/day.
C
C     The epoch (column 19-32; line 1) has a format NNNNN.NNNNNNNN,
C     where:
C
C                Fraction
C            DOY  of day
C            --- --------
C          NNNNN.NNNNNNNN
C          --
C        Year
C
C     An epoch of 00001.00000000 corresponds to 00:00:00 UTC on
C     2000 January 01.
C
C     The first derivative of Mean Motion (column 34-43, line 1), has
C     a format +.NNNNNNNN, where the first character could be either a
C     plus sign, a minus sign or a space.
C
C     The second derivative of Mean Motion (column 45-52, line 1) and
C     the BSTAR drag term (see [1] for details -- column 54-61, line 1)
C     have a format +NNNNN-N, where the first character could be either
C     a plus sign, a minus sign or a space, the decimal point is
C     assumed, and the exponent is marked by the sign (+/-) at
C     character 6 (column 51 and 60 for the second derivative and BSTAR
C     drag term respectively).
C
C     The "raw" elements in the second line consists primarily of mean
C     elements calculated using the SGP4/SDP4 orbital model (See [1]).
C     The Inclination, the Right Ascension of the Ascending Node, the
C     Argument of Perigee and the Mean Anomaly have units of degrees
C     and can range from 0 up to 360 degrees, except for the
C     Inclination that ranges from 0 to 180 degrees. The Eccentricity
C     value is provided with an assumed leading decimal point. For
C     example, a value of 9790714 corresponds to an eccentricity of
C     0.9790714. The Mean motion is measured in revolutions per day and
C     its format is NN.NNNNNNN.
C
C     This routine extracts these values, "inserts" the implied
C     decimal points and exponents and then converts the inputs
C     to units of radians, radians/minute, radians/minute**2, and
C     radians/minute**3
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
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
C           File name: getelm_ex1.tm
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
C           The geophysical.ker is a PCK file that is provided with the
C           SPICE toolkit under the "/data" directory.
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0012.tls',
C                                  'geophysical.ker'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM GETELM_EX1
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
C              PARAMETER           ( SPK10  = 'getelm_ex1.bsp' )
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
C              CALL FURNSH ( 'getelm_ex1.tm' )
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
C
C     2) Suppose you have a set of two-line elements for the LUME 1
C        cubesat. This example shows how you can use this routine
C        together with the routine EVSGP4 to propagate a state to an
C        epoch of interest.
C
C        Use the meta-kernel from the previous example to load the
C        required SPICE kernels.
C
C
C        Example code begins here.
C
C
C              PROGRAM GETELM_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         TIMSTR
C              PARAMETER           ( TIMSTR = '2020-05-26 02:25:00' )
C
C              INTEGER               PNAMLN
C              PARAMETER           ( PNAMLN = 2  )
C
C              INTEGER               TLELLN
C              PARAMETER           ( TLELLN = 69 )
C
C        C
C        C     The LUME-1 cubesat is an Earth orbiting object; set
C        C     the center ID to the Earth ID.
C        C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER  = 399     )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(PNAMLN)    NOADPN ( 8  )
C              CHARACTER*(TLELLN)    TLE    ( 2  )
C
C              DOUBLE PRECISION      ELEMS  ( 10 )
C              DOUBLE PRECISION      EPOCH
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      GEOPHS ( 8  )
C              DOUBLE PRECISION      STATE  ( 6  )
C
C              INTEGER               I
C              INTEGER               N
C
C        C
C        C     These are the variables that will hold the constants
C        C     required by EV2LIN. These constants are available from
C        C     the loaded PCK file, which provides the actual values
C        C     and units as used by NORAD propagation model.
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
C        C     Define the Two-Line Element set for LUME-1.
C        C
C              TLE(1)  = '1 43908U 18111AJ  20146.60805006  .00000806'
C             .      //                   '  00000-0  34965-4 0  9999'
C              TLE(2)  = '2 43908  97.2676  47.2136 0020001 220.6050 '
C             .      //                   '139.3698 15.24999521 78544'
C
C        C
C        C     Load the PCK file that provides the geophysical
C        C     constants required for the evaluation of the two-line
C        C     elements sets. Load also an LSK, as it is required by
C        C     GETELM to perform time conversions. Use a metakernel for
C        C     convenience.
C        C
C              CALL FURNSH ( 'getelm_ex1.tm' )
C
C        C
C        C     Retrieve the data from the kernel, and place it on
C        C     the GEOPHS array.
C        C
C              DO I = 1, 8
C
C                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) )
C
C              END DO
C
C        C
C        C     Convert the Two Line Elements lines to the element sets.
C        C     Set the lower bound for the years to be the beginning
C        C     of the space age.
C        C
C              CALL GETELM ( 1957, TLE, EPOCH, ELEMS )
C
C        C
C        C     Now propagate the state using EV2LIN to the epoch of
C        C     interest.
C        C
C              CALL STR2ET ( TIMSTR, ET )
C              CALL EVSGP4 ( ET, GEOPHS, ELEMS, STATE )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(2A)')       'Epoch   : ', TIMSTR
C              WRITE(*,'(A,3F16.8)') 'Position:', (STATE(I), I=1,3)
C              WRITE(*,'(A,3F16.8)') 'Velocity:', (STATE(I), I=4,6)
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Epoch   : 2020-05-26 02:25:00
C        Position:  -4644.60403398  -5038.95025539   -337.27141116
C        Velocity:     -0.45719025      0.92884817     -7.55917355
C
C
C$ Restrictions
C
C     1)  The format of the two-line elements suffer from a "millennium"
C         problem --- only two digits are used for the year of the
C         elements. It is not clear how Space Command will deal with
C         this problem. NAIF hopes that by adjusting the input FRSTYR
C         you should be able to use this routine well into the 21st
C         century.
C
C         The approach taken to mapping the two-digit year to the
C         full year is given by the code below. Here, YR is the
C         integer obtained by parsing the two-digit year from the first
C         line of the elements.
C
C            BEGYR = (FRSTYR/100)*100
C            YEAR  = BEGYR + YR
C
C            IF ( YEAR .LT. FRSTYR ) THEN
C               YEAR = YEAR + 100
C            END IF
C
C         This mapping will be changed if future two-line element
C         representations make this method of computing the full year
C         inaccurate.
C
C$ Literature_References
C
C     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
C          Propagation of the NORAD Element Sets," U.S. Air Force
C          Aerospace Defense Command, Colorado Springs, CO, 1980.
C
C     [2]  "SDC/SCC Two Card Element Set - Transmission Format,"
C          ADCOM/DO Form 12.
C
C     [3]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
C          Space Command Element Sets,"  U.S. Air Force Aerospace
C          Defense Command, Colorado Springs, CO, 1986.
C
C     [4]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
C          Orbit Modeling in the U. S. Space Surveillance System,"
C          Journal of Guidance, Control, and Dynamics. 27(2):174-185,
C          2004.
C
C     [5]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
C          Spacetrack Report #3," paper AIAA 2006-6753 presented at the
C          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
C          August 21-24, 2006.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 06-NOV-2021 (JDR) (MCS)
C
C        Changed the output array declaration from assumed-size array
C        to a constant size array.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples; second one based on existing code fragments.
C
C        Corrected the input element names in ELEMS.
C
C        Updated $Particulars to describe in detail the TLE format,
C        $Restrictions to provide indications on the "millennium"
C        problem of TLE data, and $Literature_References to point to
C        the sources of the detailed documentation.
C
C-    SPICELIB Version 3.0.0, 30-MAR-2004 (EDW)
C
C        Routine now passes inputs to ZZGETELM then responds to
C        any error condition.
C
C-    SPICELIB Version 2.0.0, 03-MAR-2000 (WLT)
C
C        The routine was modified to check that all of the terms
C        in the two-line element set are parsed correctly.
C
C-    SPICELIB Version 1.0.0, 26-JUN-1997 (WLT)
C
C-&


C$ Index_Entries
C
C     Parse two-line elements
C
C-&


C
C     Spicelib functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      LOGICAL               OK
      CHARACTER*(256)       ERROR

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GETELM' )

C
C     Pass the input to the parse routine...
C
      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, OK, ERROR )

C
C     ...check for an error parsing the TLE pair. Signal an
C     error if OK equals .FALSE.
C
      IF ( .NOT. OK ) THEN

         CALL SETMSG ( 'Error in TLE set. #' )
         CALL ERRCH  ( '#', ERROR  )
         CALL SIGERR ( 'SPICE(BADTLE)' )
         CALL CHKOUT ( 'GETELM' )
         RETURN

      END IF

      CALL CHKOUT ( 'GETELM' )
      RETURN
      END

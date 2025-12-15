C$Procedure EVSGP4 ( Evaluate "two-line" element data )

      SUBROUTINE EVSGP4 ( ET, GEOPHS, ELEMS, STATE )

C$ Abstract
C
C     Evaluate NORAD two-line element data for earth orbiting
C     spacecraft. This evaluator uses algorithms as described
C     in Vallado 2006 [4].
C
C     This routine supersedes SPICELIB routines EV2LIN and DPSPCE.
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
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzsgp4.inc'

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GEOPHS (  8 )
      DOUBLE PRECISION      ELEMS  ( 10 )
      DOUBLE PRECISION      STATE  (  6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch in seconds past ephemeris epoch J2000.
C     GEOPHS     I   Geophysical constants
C     ELEMS      I   Two-line element data
C     STATE      O   Evaluated state
C
C$ Detailed_Input
C
C     ET       is the epoch in seconds past ephemeris epoch J2000
C              at which a state should be produced from the
C              input elements.
C
C     GEOPHS   is a collection of 8 geophysical constants needed
C              for computing a state. The order of these
C              constants must be:
C
C                 GEOPHS(1) = J2 gravitational harmonic for Earth.
C                 GEOPHS(2) = J3 gravitational harmonic for Earth.
C                 GEOPHS(3) = J4 gravitational harmonic for Earth.
C
C              These first three constants are dimensionless.
C
C                 GEOPHS(4) = KE: Square root of the GM for Earth where
C                             GM is expressed in Earth radii cubed per
C                             minutes squared.
C
C                 GEOPHS(5) = QO: High altitude bound for atmospheric
C                             model in km.
C
C                 GEOPHS(6) = SO: Low altitude bound for atmospheric
C                             model in km.
C
C                 GEOPHS(7) = RE: Equatorial radius of the earth in km.
C
C                 GEOPHS(8) = AE: Distance units/earth radius
C                             (normally 1)
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
C     ELEMS    is an array containing two-line element data
C              as prescribed below. The elements NDD6O and BSTAR
C              must already be scaled by the proper exponent stored
C              in the two line elements set. Moreover, the
C              various items must be converted to the units shown
C              here.
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
C$ Detailed_Output
C
C     STATE    is the state produced by evaluating the input elements
C              at the input epoch ET. Units are km and km/sec relative
C              to the TEME reference frame.
C
C$ Parameters
C
C     AFSPC    set the SGP4 propagator to use the original
C              Space Track #3 GST algorithm as described in Hoots [1];
C              value defined in zzsgp4.inc.
C
C$ Exceptions
C
C     1)  No checks are made on the reasonableness of the inputs.
C
C     2)  If a problem occurs when evaluating the elements, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine evaluates any NORAD two-line element sets for
C     near-earth orbiting satellites using the algorithms described in
C     Vallado 2006 [4].
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose you have a set of two-line elements for the LUME 1
C        cubesat. This example shows how you can use this routine
C        together with the routine GETELM to propagate a state to an
C        epoch of interest.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: evsgp4_ex1.tm
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
C              PROGRAM EVSGP4_EX1
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
C        C     required by EVSGP4. These constants are available from
C        C     the loaded PCK file, which provides the actual values
C        C     and units as used by NORAD propagation model.
C        C
C        C     Constant   Meaning
C        C     --------   ------------------------------------------
C        C     J2         J2 gravitational harmonic for Earth.
C        C     J3         J3 gravitational harmonic for Earth.
C        C     J4         J4 gravitational harmonic for Earth.
C        C     KE         Square root of the GM for Earth.
C        C     QO         High altitude bound for atmospheric model.
C        C     SO         Low altitude bound for atmospheric model.
C        C     ER         Equatorial radius of the Earth.
C        C     AE         Distance units/earth radius.
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
C        C     Load the MK file that includes the PCK file that provides
C        C     the geophysical constants required for the evaluation of
C        C     the two-line elements sets and the LSK, as it is required
C        C     by GETELM to perform time conversions.
C        C
C              CALL FURNSH ( 'evsgp4_ex1.tm' )
C
C        C
C        C     Retrieve the data from the kernel, and place it on
C        C     the GEOPHS array.
C        C
C              DO I = 1, 8
C                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) )
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
C        C     Now propagate the state using EVSGP4 to the epoch of
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
C        When this program was executed on a PC/Linux/gfortran/64-bit
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 02-NOV-2021 (EDW) (MCS)
C
C-&


C$ Index_Entries
C
C     Evaluate NORAD two-line element data using SGP4.
C
C-&


      DOUBLE PRECISION      T1

      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'EVSGP4' )

C
C     Evaluate TLE.
C

C
C     Initialize.
C
      CALL XXSGP4I ( GEOPHS, ELEMS, AFSPC )

      IF ( FAILED() ) THEN
          CALL CHKOUT ( 'EVSGP4' )
          RETURN
      END IF

C
C     Calculate time from epoch in minutes.
C
      T1 = ELEMS ( KEPOCH )
      T1 = (ET - T1)/60.D0

C
C     Compute state.
C
      CALL XXSGP4E ( T1, STATE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EVSGP4' )
         RETURN
      END IF

C
C     Checkout, then return.
C
      CALL CHKOUT ( 'EVSGP4' )
      RETURN

      END

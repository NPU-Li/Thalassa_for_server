C$Procedure SRFREC ( Surface to rectangular coordinates )

      SUBROUTINE SRFREC ( BODY, LON, LAT, RECTAN )

C$ Abstract
C
C     Convert planetocentric latitude and longitude of a surface
C     point on a specified body to rectangular coordinates.
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
C     KERNEL
C     NAIF_IDS
C
C$ Keywords
C
C     CONVERSION
C     COORDINATES
C     TRANSFORMATION
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               BODY
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF integer code of an extended body.
C     LON        I   Longitude of point in radians.
C     LAT        I   Latitude of point in radians.
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     BODY     is the NAIF integer code of an extended body on which
C              a surface point of interest is located. The body is
C              modeled as a triaxial ellipsoid.
C
C     LON      is the longitude of the input point. This is the
C              angle between the prime meridian and the meridian
C              containing the point. The direction of increasing
C              longitude is from the +X axis towards the +Y axis.
C
C              Longitude is measured in radians. On input, the
C              range of longitude is unrestricted.
C
C     LAT      is the latitude of the input point. This is the angle
C              from the XY plane of the ray from the origin through
C              the point.
C
C              Latitude is measured in radians. On input, the range
C              of latitude is unrestricted.
C
C$ Detailed_Output
C
C     RECTAN   are the rectangular coordinates of the input surface
C              point. Units are the same as those used to define the
C              radii of BODY. Normally, these units are km.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If radii for BODY are not found in the kernel pool, an error
C         is signaled by a routine in the call tree of this routine.
C
C     2)  If the size of the BODY body radii kernel variable is not
C         three, an error is signaled by a routine in the call tree of
C         this routine.
C
C     3)  If any of the three BODY body radii is less-than or equal to
C         zero, an error is signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the rectangular coordinates of a surface
C     point on an extended body with known radii, where the location
C     of the surface point is specified in planetocentric latitudinal
C     coordinates.
C
C     Latitudinal coordinates are defined by a distance from a central
C     reference point, an angle from a reference meridian, and an angle
C     above the equator of a sphere centered at the central reference
C     point. In this case, the distance from the central reference
C     point is not required as an input because the fact that the
C     point is on the body's surface allows one to deduce this quantity.
C
C     Below are two tables that demonstrate by example the relationship
C     between rectangular and latitudinal coordinates.
C
C     Listed in the first table (under R, LON and LAT) are
C     latitudinal coordinate triples that approximately represent
C     points whose rectangular coordinates are taken from the set
C     {-1, 0, 1}.  (Angular quantities are given in degrees.)
C
C
C          R          LON       LAT    RECTAN(1)   RECTAN(2)  RECTAN(3)
C         --------------------------   --------------------------------
C         0.0000    0.0000    0.0000      0.0000      0.0000     0.0000
C         1.0000    0.0000    0.0000      1.0000      0.0000     0.0000
C         1.0000   90.0000    0.0000      0.0000      1.0000     0.0000
C         1.0000    0.0000   90.0000      0.0000      0.0000     1.0000
C         1.0000  180.0000    0.0000     -1.0000      0.0000     0.0000
C         1.0000  -90.0000    0.0000      0.0000     -1.0000     0.0000
C         1.0000    0.0000  -90.0000      0.0000      0.0000    -1.0000
C         1.4142   45.0000    0.0000      1.0000      1.0000     0.0000
C         1.4142    0.0000   45.0000      1.0000      0.0000     1.0000
C         1.4142   90.0000   45.0000      0.0000      1.0000     1.0000
C         1.7320   45.0000   35.2643      1.0000      1.0000     1.0000
C
C
C     This routine is related to the SPICELIB routine LATREC, which
C     accepts a radius, longitude, and latitude as inputs and produces
C     equivalent rectangular coordinates as outputs.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the rectangular coordinates of the point
C
C           100 degrees planetocentric longitude
C           -35 degrees planetocentric latitude
C
C        on the Earth; then convert these coordinates back to
C        latitudinal coordinates. We should be able to recover
C        our original longitude and latitude values.
C
C        Use the PCK kernel below to load the required triaxial
C        ellipsoidal shape model and orientation data for the Earth.
C
C           pck00008.tpc
C
C
C        Example code begins here.
C
C
C              PROGRAM SRFREC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      X     ( 3 )
C              DOUBLE PRECISION      RADIUS
C
C        C
C        C     Load the kernel pool with a PCK file that contains
C        C     values for the radii of the Earth.
C        C
C              CALL FURNSH ( 'pck00008.tpc' )
C
C        C
C        C     Find X, the rectangular coordinates of the
C        C     surface point defined by LAT and LON.  The
C        C     NAIF integer code for the Earth is 399.
C        C     (See the NAIF_IDS required reading file for
C        C     the complete set of codes.)
C        C
C              LON  =  100.D0
C              LAT   =  -35.D0
C
C              WRITE (*,*) 'Original latitudinal coordinates'
C              WRITE (*,*) ' '
C              WRITE (*,*) '  Longitude (deg) = ', LON
C              WRITE (*,*) '  Latitude  (deg) = ', LAT
C
C        C
C        C     Convert angles to radians on input to SRFREC.
C        C
C              CALL SRFREC ( 399, LON*RPD(), LAT*RPD(), X )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Rectangular coordinates '
C              WRITE (*,*) ' '
C              WRITE (*,*) '  X (km)          = ', X(1)
C              WRITE (*,*) '  Y (km)          = ', X(2)
C              WRITE (*,*) '  Z (km)          = ', X(3)
C
C        C
C        C     Now try to recover the original latitudinal
C        C     coordinates from the rectangular coordinates
C        C     found by SRFREC.
C        C
C              CALL RECLAT ( X, RADIUS, LON, LAT )
C
C        C
C        C     Convert angles to degrees for display.
C        C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Latitudinal coordinates recovered '   //
C             .            'from rectangular coordinates'
C              WRITE (*,*) ' '
C              WRITE (*,*) '  Longitude (deg) = ', LON * DPR()
C              WRITE (*,*) '  Latitude  (deg) = ', LAT * DPR()
C              WRITE (*,*) '  Radius    (km)  = ', RADIUS
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Original latitudinal coordinates
C
C           Longitude (deg) =    100.00000000000000
C           Latitude  (deg) =   -35.000000000000000
C
C         Rectangular coordinates
C
C           X (km)          =   -906.24942866761364
C           Y (km)          =    5139.5959088415748
C           Z (km)          =   -3654.3008396462560
C
C         Latitudinal coordinates recovered from rectangular coordinates
C
C           Longitude (deg) =    100.00000000000000
C           Latitude  (deg) =   -35.000000000000000
C           Radius    (km)  =    6371.0790891167535
C
C
C$ Restrictions
C
C     1)  A PCK text kernel containing the body radius definitions
C         required by this routine must be loaded into the kernel
C         pool prior to any calls to this routine.
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 01-NOV-2021 (JDR) (EDW)
C
C        Body radii accessed from kernel pool using ZZGFTREB.
C
C        Changed the input argument name LONG to LON for consistency
C        with other routines.
C
C        Edited the header to comply with NAIF standard. Modified code
C        example output format. Minor edits to header.
C
C-    SPICELIB Version 1.2.0, 19-APR-2016 (NJB)
C
C        Re-implemented ellipsoid surface point computation
C        using EDPNT.
C
C-    SPICELIB Version 1.1.0, 03-NOV-2005 (NJB)
C
C        Call to BODVAR was replaced with call to BODVCD.
C
C        Various header updates were made to clarify description
C        of routine's functionality. Example program was updated
C        as well.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     convert body-fixed latitudinal coordinates to rectangular
C     convert surface latitudinal coordinates to rectangular
C     surface point latitudinal coordinates to rectangular
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      UVEC   ( 3 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SRFREC' )

C
C     Look up the body's radii.
C
      CALL ZZGFTREB ( BODY, RADII )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFREC' )
         RETURN
      END IF

C
C     Find the unit vector pointing from the body center to the
C     input surface point.
C
      CALL LATREC ( 1.D0, LON, LAT, UVEC )

C
C     Find out where the ray defined by this vector intersects the
C     surface.  This intercept is the point we're looking for.
C
      CALL EDPNT ( UVEC, RADII(1), RADII(2), RADII(3), RECTAN )

      CALL CHKOUT ( 'SRFREC' )
      RETURN
      END

C$Procedure RADREC ( Range, RA and DEC to rectangular coordinates )

      SUBROUTINE RADREC ( RANGE, RA, DEC, RECTAN )

C$ Abstract
C
C     Convert from range, right ascension, and declination to
C     rectangular coordinates.
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
C     CONVERSION
C     COORDINATES
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      RECTAN ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     RANGE      I   Distance of a point from the origin.
C     RA         I   Right ascension of point in radians.
C     DEC        I   Declination of point in radians.
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     RANGE    is the distance of the point from the origin. Input
C              should be in terms of the same units in which the
C              output is desired.
C
C     RA       is the right ascension of the point. This is the angular
C              distance measured toward the east from the prime
C              meridian to the meridian containing the input point.
C              The direction of increasing right ascension is from
C              the +X axis towards the +Y axis.
C
C              The range (i.e., the set of allowed values) of
C              RA is unrestricted. Units are radians.
C
C     DEC      is the declination of the point. This is the angle from
C              the XY plane of the ray from the origin through the
C              point.
C
C              The range (i.e., the set of allowed values) of
C              DEC is unrestricted. Units are radians.
C
C$ Detailed_Output
C
C     RECTAN   is the array containing the rectangular coordinates of
C              the point.
C
C              The units associated with RECTAN are those
C              associated with the input RANGE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts the right ascension, declination, and range
C     of a point into the associated rectangular coordinates.
C
C     The input is defined by a distance from a central reference point,
C     an angle from a reference meridian, and an angle above the equator
C     of a sphere centered at the central reference point.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Convert to the J2000 frame the right ascension and declination
C        of an object initially expressed with respect to the B1950
C        reference frame.
C
C
C        Example code begins here.
C
C
C              PROGRAM RADREC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      DECB
C              DOUBLE PRECISION      DECJ
C              DOUBLE PRECISION      MTRANS ( 3, 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      RAB
C              DOUBLE PRECISION      RAJ
C              DOUBLE PRECISION      V1950  ( 3    )
C              DOUBLE PRECISION      V2000  ( 3    )
C
C        C
C        C     Set the initial right ascension and declination
C        C     coordinates of the object, given with respect
C        C     to the B1950 reference frame.
C        C
C              RAB  = 135.88680896D0
C              DECB =  17.50151037D0
C
C        C
C        C     Convert RAB and DECB to a 3-vector expressed in
C        C     the B1950 frame.
C        C
C              CALL RADREC ( 1.D0, RAB * RPD(), DECB * RPD(), V1950 )
C
C        C
C        C     We use the SPICELIB routine PXFORM to obtain the
C        C     transformation  matrix for converting vectors between
C        C     the B1950 and J2000 reference frames.  Since
C        C     both frames are inertial, the input time value we
C        C     supply to PXFORM is arbitrary.  We choose zero
C        C     seconds past the J2000 epoch.
C        C
C              CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS )
C
C        C
C        C     Transform the vector to the J2000 frame.
C        C
C              CALL MXV ( MTRANS, V1950, V2000 )
C
C        C
C        C     Find the right ascension and declination of the
C        C     J2000-relative vector.
C        C
C              CALL RECRAD ( V2000, R, RAJ, DECJ )
C
C        C
C        C     Output the results.
C        C
C              WRITE(*,*) 'Right ascension (B1950 frame): ', RAB
C              WRITE(*,*) 'Declination (B1950 frame)    : ', DECB
C
C              WRITE(*,*) 'Right ascension (J2000 frame): ',
C             .                                       RAJ * DPR()
C              WRITE(*,*) 'Declination (J2000 frame)    : ',
C             .                                      DECJ * DPR()
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Right ascension (B1950 frame):    135.88680896000000
C         Declination (B1950 frame)    :    17.501510369999998
C         Right ascension (J2000 frame):    136.58768235448090
C         Declination (J2000 frame)    :    17.300442748830637
C
C
C     2) Define a set of 15 right ascension-declination data pairs for
C        the Earth's pole at different ephemeris epochs, convert them
C        to rectangular coordinates and compute the angular separation
C        between these coordinates and the IAU_EARTH pole given by a
C        PCK kernel.
C
C        Use the PCK kernel below to load the required triaxial
C        ellipsoidal shape model and orientation data for the Earth.
C
C           pck00010.tpc
C
C
C        Example code begins here.
C
C
C              PROGRAM RADREC_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
C              DOUBLE PRECISION      VSEP
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NCOORD
C              PARAMETER           ( NCOORD = 15 )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      DEC    ( NCOORD )
C              DOUBLE PRECISION      ET     ( NCOORD )
C              DOUBLE PRECISION      POLE   ( 3      )
C              DOUBLE PRECISION      MTRANS ( 3, 3   )
C              DOUBLE PRECISION      RA     ( NCOORD )
C              DOUBLE PRECISION      V2000  ( 3      )
C              DOUBLE PRECISION      Z      ( 3      )
C
C              INTEGER               I
C
C        C
C        C     Define a set of 15 right ascension-declination
C        C     coordinate pairs (in degrees) for the Earth's pole
C        C     and the array of corresponding ephemeris times in
C        C     J2000 TDB seconds.
C        C
C              DATA                  RA   /
C             .           180.003739D0, 180.003205D0, 180.002671D0,
C             .           180.002137D0, 180.001602D0, 180.001068D0,
C             .           180.000534D0, 360.000000D0, 359.999466D0,
C             .           359.998932D0, 359.998397D0, 359.997863D0,
C             .           359.997329D0, 359.996795D0, 359.996261D0  /
C
C              DATA                  DEC  /
C             .            89.996751D0,  89.997215D0,  89.997679D0,
C             .            89.998143D0,  89.998608D0,  89.999072D0,
C             .            89.999536D0,  90.000000D0,  89.999536D0,
C             .            89.999072D0,  89.998607D0,  89.998143D0,
C             .            89.997679D0,  89.997215D0,  89.996751D0  /
C
C              DATA                  ET   /   -18408539.52023917D0,
C             .         -15778739.49107254D0, -13148939.46190590D0,
C             .         -10519139.43273926D0,  -7889339.40357262D0,
C             .          -5259539.37440598D0,  -2629739.34523934D0,
C             .                60.68392730D0,   2629860.71309394D0,
C             .           5259660.74226063D0,   7889460.77142727D0,
C             .          10519260.80059391D0,  13149060.82976055D0,
C             .          15778860.85892719D0,  18408660.88809383D0  /
C
C              DATA                  Z    /  0.D0, 0.D0, 1.D0  /
C
C        C
C        C     Load a PCK kernel.
C        C
C              CALL FURNSH ( 'pck00010.tpc' )
C
C        C
C        C     Print the banner out.
C        C
C              WRITE(*,'(A)') '       ET           Angular difference'
C              WRITE(*,'(A)') '------------------  ------------------'
C
C              DO I = 1, NCOORD
C
C        C
C        C        Convert the right ascension and declination
C        C        coordinates (in degrees) to rectangular.
C        C
C                 CALL RADREC ( 1.D0, RA(I) * RPD(), DEC(I) * RPD(),
C             .                 V2000                               )
C
C        C
C        C        Retrieve the transformation matrix from the J2000
C        C        frame to the IAU_EARTH frame.
C        C
C                 CALL PXFORM ( 'J2000', 'IAU_EARTH', ET(I), MTRANS )
C
C        C
C        C        Rotate the V2000 vector into IAU_EARTH. This vector
C        C        should equal (round-off) the Z direction unit vector.
C        C
C                 CALL MXV ( MTRANS, V2000, POLE )
C
C        C
C        C        Output the ephemeris time and the angular separation
C        C        between the rotated vector and the Z direction unit
C        C        vector.
C        C
C                 WRITE(*,'(F18.8,2X,F18.16)') ET(I),
C             .                                VSEP( POLE, Z ) * DPR()
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C               ET           Angular difference
C        ------------------  ------------------
C        -18408539.52023917  0.0000001559918278
C        -15778739.49107254  0.0000000106799881
C        -13148939.46190590  0.0000001773517911
C        -10519139.43273926  0.0000003440236194
C         -7889339.40357262  0.0000004893045693
C         -5259539.37440598  0.0000003226327536
C         -2629739.34523934  0.0000001559609507
C               60.68392730  0.0000000107108706
C          2629860.71309394  0.0000001773826862
C          5259660.74226063  0.0000003440544891
C          7889460.77142727  0.0000004892736740
C         10519260.80059391  0.0000003226018712
C         13149060.82976055  0.0000001559300556
C         15778860.85892719  0.0000000107417474
C         18408660.88809383  0.0000001774135760
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  L. Taff, "Celestial Mechanics, A Computational Guide for the
C          Practitioner," Wiley, 1985
C
C$ Author_and_Institution
C
C     C.H. Acton         (JPL)
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONTE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete  code example based on existing code fragment
C        and a second example.
C
C-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity. Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
C
C-&


C$ Index_Entries
C
C     range ra and dec to rectangular coordinates
C     right_ascension and declination to rectangular
C
C-&


C
C     Convert from range, right ascension, and declination to
C     rectangular coordinates by calling the routine LATREC.
C
      CALL LATREC ( RANGE, RA, DEC, RECTAN )

      RETURN
      END

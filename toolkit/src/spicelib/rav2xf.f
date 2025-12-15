C$Procedure RAV2XF ( Rotation and angular velocity to transform )

      SUBROUTINE RAV2XF ( ROT, AV, XFORM )

C$ Abstract
C
C     Determine a state transformation matrix from a rotation matrix
C     and the angular velocity of the rotation.
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
C     ROTATION
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROT        I   Rotation matrix.
C     AV         I   Angular velocity vector.
C     XFORM      O   State transformation associated with ROT and AV.
C
C$ Detailed_Input
C
C     ROT      is a rotation matrix that gives the transformation from
C              some frame FRAME1 to another frame FRAME2.
C
C     AV       is the angular velocity of the transformation.
C              In other words, if P is the position of a fixed
C              point in FRAME2, then from the point of view of
C              FRAME1, P rotates (in a right handed sense) about
C              an axis parallel to AV. Moreover the rate of rotation
C              in radians per unit time is given by the length of
C              AV.
C
C              More formally, the velocity V of P in FRAME1 is
C              given by
C                                 T
C                 V  =  AV x ( ROT  * P )
C
C$ Detailed_Output
C
C     XFORM    is a state transformation matrix associated
C              with ROT and AV. If S1 is the state of an object
C              with respect to FRAME1, then the state S2 of the
C              object with respect to FRAME2 is given by
C
C                 S2  =  XFORM * S1
C
C              where "*" denotes Matrix-Vector multiplication.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  No checks are performed on ROT to ensure that it is indeed
C         a rotation matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is essentially a macro routine for converting
C     a rotation and angular velocity of the rotation to the
C     equivalent state transformation matrix.
C
C     This routine is an inverse of XF2RAV.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following example program uses CKGPAV to get C-matrix
C        and associated angular velocity vector for an image whose
C        SCLK count (un-encoded character string version) is known.
C
C        From that matrix and angular velocity vector, the associated
C        state transformation matrix is obtained.
C
C        Note that we need to load a SCLK kernel to convert from clock
C        string to "ticks." Although not required for older spacecraft
C        clocks, most modern spacecraft ones require a leapseconds
C        kernel to be loaded in addition to a SCLK kernel.
C
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: rav2xf_ex1.tm
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
C              File name              Contents
C              --------------------   -----------------------
C              cas00071.tsc           CASSINI SCLK
C              04161_04164ra.bc       CASSINI spacecraft
C                                     reconstructed CK
C
C           \begindata
C
C             KERNELS_TO_LOAD = ( 'cas00071.tsc'
C                                 '04161_04164ra.bc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM RAV2XF_EX1
C              IMPLICIT NONE
C
C        C
C        C     Constants for this program.
C        C
C        C     -- The code for the CASSINI spacecraft clock is -82.
C        C
C        C     -- The code for CASSINI spacecraft reference frame is
C        C        -82000.
C        C
C        C    --  Spacecraft clock tolerance is 1.0 seconds. This may
C        C        not be an acceptable tolerance for some applications.
C        C        It must be converted to "ticks" (units of encoded
C        C        SCLK) for input to CKGPAV.
C        C
C        C     -- The reference frame we want is J2000.
C        C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'rav2xf_ex1.tm' )
C
C              CHARACTER*(*)         REFFRM
C              PARAMETER           ( REFFRM = 'J2000' )
C
C              CHARACTER*(*)         SCLKCH
C              PARAMETER           ( SCLKCH = '1/1465476046.160' )
C
C              CHARACTER*(*)         SCLTOL
C              PARAMETER           ( SCLTOL = '1.0' )
C
C              INTEGER               SCID
C              PARAMETER           ( SCID   = -82    )
C
C              INTEGER               INSTID
C              PARAMETER           ( INSTID = -82000 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      AV     ( 3 )
C              DOUBLE PRECISION      CLKOUT
C              DOUBLE PRECISION      CMAT   ( 3, 3 )
C              DOUBLE PRECISION      FXMAT  ( 6, 6 )
C              DOUBLE PRECISION      SCLKDP
C              DOUBLE PRECISION      TOLTIK
C
C              INTEGER               I
C              INTEGER               J
C
C              LOGICAL               FOUND
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert tolerance from CASSINI formatted character
C        C     string SCLK to ticks which are units of encoded SCLK.
C        C
C              CALL SCTIKS ( SCID, SCLTOL, TOLTIK )
C
C        C
C        C     CKGPAV requires encoded spacecraft clock.
C        C
C              CALL SCENCD ( SCID, SCLKCH, SCLKDP )
C
C              CALL CKGPAV ( INSTID, SCLKDP, TOLTIK, REFFRM,
C             .              CMAT,   AV,     CLKOUT, FOUND )
C
C        C
C        C     Recall that CMAT and AV are the rotation and angular
C        C     velocity of the transformation from J2000 to the
C        C     spacecraft frame.
C        C
C              IF ( FOUND ) THEN
C
C        C
C        C        Display CMAT and AV.
C        C
C                 WRITE(*,'(A)') 'Rotation matrix:'
C                 DO I = 1, 3
C
C                    WRITE(*,'(3F10.6)') (CMAT(I,J), J=1,3 )
C
C                 END DO
C
C                 WRITE(*,'(A)') 'Angular velocity:'
C                 WRITE(*,'(3F20.16)') AV
C
C        C
C        C        Get state transformation from J2000 to the spacecraft
C        C        frame.
C        C
C                 CALL RAV2XF ( CMAT,  AV, FXMAT )
C
C        C
C        C        Display the results.
C        C
C                 WRITE(*,*)
C                 WRITE(*,'(A)') 'State transformation matrix:'
C                 DO I = 1, 6
C
C                    WRITE(*,'(6F10.6)') (FXMAT(I,J), J=1,6 )
C
C                 END DO
C
C              ELSE
C
C                 WRITE(*,*) 'No rotation matrix/angular velocity '
C             .          //  'found for ', SCLKCH
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
C        Rotation matrix:
C         -0.604984  0.796222 -0.005028
C         -0.784160 -0.596891 -0.169748
C         -0.138158 -0.098752  0.985475
C        Angular velocity:
C          0.0000032866819065 -0.0000099372638338  0.0000197597699770
C
C        State transformation matrix:
C         -0.604984  0.796222 -0.005028  0.000000  0.000000  0.000000
C         -0.784160 -0.596891 -0.169748  0.000000  0.000000  0.000000
C         -0.138158 -0.098752  0.985475  0.000000  0.000000  0.000000
C         -0.000016 -0.000012 -0.000003 -0.604984  0.796222 -0.005028
C          0.000013 -0.000015 -0.000010 -0.784160 -0.596891 -0.169748
C         -0.000008 -0.000006 -0.000002 -0.138158 -0.098752  0.985475
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
C-    SPICELIB Version 1.1.1, 04-JUL-2021 (JDR)
C
C        Corrected $Abstract section, which described XF2RAV instead of
C        this routine.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based existing fragment.
C
C         Added ROTATION to the required readings.
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (WLT)
C
C        The example in version 1.0.0 was incorrect. The example
C        in version 1.1.0 fixes the previous problem.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     State transformation to rotation and angular velocity
C
C-&


      INTEGER               I
      INTEGER               J

      DOUBLE PRECISION      OMEGAT ( 3, 3 )
      DOUBLE PRECISION      DROTDT ( 3, 3 )

C
C     A state transformation matrix XFORM has the following form
C
C
C         [      |     ]
C         |  R   |  0  |
C         |      |     |
C         | -----+-----|
C         |  dR  |     |
C         |  --  |  R  |
C         [  dt  |     ]
C
C
C     where R is a rotation and dR/dt is the time derivative of that
C     rotation.  From this we can immediately fill in most of the
C     state transformation matrix.
C
      DO I = 1, 3
         DO J = 1,3
            XFORM(I,  J  ) = ROT(I,J)
            XFORM(I+3,J+3) = ROT(I,J)
            XFORM(I,  J+3) = 0.0D0
         END DO
      END DO

C
C     Now for the rest.
C
C     Recall that ROT is a transformation that converts positions
C     in some frame FRAME1 to positions in a second frame FRAME2.
C
C     The angular velocity matrix OMEGA (the cross product matrix
C     corresponding to AV) has the following property.
C
C     If P is the position of an object that is stationary with
C     respect to FRAME2 then the velocity V of that object in FRAME1
C     is given by:
C                          t
C         V  =  OMEGA * ROT  *  P
C
C     But V is also given by
C
C                    t
C               d ROT
C         V =   -----  * P
C                 dt
C
C     So that
C                                  t
C                    t        d ROT
C         OMEGA * ROT    =   -------
C                               dt
C
C     Hence
C
C          d ROT                 t
C          -----   =  ROT * OMEGA
C            dt
C
C
C     From this discussion we can see that we need OMEGA transpose.
C     Here it is.
C
      OMEGAT(1,1) =  0.0D0
      OMEGAT(2,1) = -AV(3)
      OMEGAT(3,1) =  AV(2)

      OMEGAT(1,2) =  AV(3)
      OMEGAT(2,2) =  0.0D0
      OMEGAT(3,2) = -AV(1)

      OMEGAT(1,3) = -AV(2)
      OMEGAT(2,3) =  AV(1)
      OMEGAT(3,3) =  0.0D0

      CALL MXM ( ROT, OMEGAT, DROTDT )

      DO I = 1, 3
         DO J = 1,3
            XFORM(I+3,J) = DROTDT(I,J)
         END DO
      END DO

      RETURN
      END

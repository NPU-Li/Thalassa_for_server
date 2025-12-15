C$Procedure TISBOD ( Transformation, inertial state to bodyfixed )

      SUBROUTINE TISBOD ( REF, BODY, ET, TSIPM )

C$ Abstract
C
C     Return a 6x6 matrix that transforms states in inertial
C     coordinates to states in body-equator-and-prime-meridian
C     coordinates.
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
C     FRAMES
C     PCK
C     NAIF_IDS
C     ROTATION
C     TIME
C
C$ Keywords
C
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'errhnd.inc'
      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         REF
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TSIPM   ( 6,6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     REF        I   ID of inertial reference frame to transform from
C     BODY       I   ID code of body
C     ET         I   Epoch of transformation
C     TSIPM      O   Transformation (state), inertial to prime meridian
C
C$ Detailed_Input
C
C     REF      is the NAIF name for an inertial reference frame.
C              Acceptable names include:
C
C                 Name       Description
C                 --------   --------------------------------
C                 'J2000'    Earth mean equator, dynamical
C                            equinox of J2000
C
C                 'B1950'    Earth mean equator, dynamical
C                            equinox of B1950
C
C                 'FK4'      Fundamental Catalog (4)
C
C                 'DE-118'   JPL Developmental Ephemeris (118)
C
C                 'DE-96'    JPL Developmental Ephemeris ( 96)
C
C                 'DE-102'   JPL Developmental Ephemeris (102)
C
C                 'DE-108'   JPL Developmental Ephemeris (108)
C
C                 'DE-111'   JPL Developmental Ephemeris (111)
C
C                 'DE-114'   JPL Developmental Ephemeris (114)
C
C                 'DE-122'   JPL Developmental Ephemeris (122)
C
C                 'DE-125'   JPL Developmental Ephemeris (125)
C
C                 'DE-130'   JPL Developmental Ephemeris (130)
C
C                 'GALACTIC' Galactic System II
C
C                 'DE-200'   JPL Developmental Ephemeris (200)
C
C                 'DE-202'   JPL Developmental Ephemeris (202)
C
C              See the Frames Required Reading frames.req for a full
C              list of inertial reference frame names built into
C              SPICE.
C
C              The output TSIPM will give the transformation
C              from this frame to the bodyfixed frame specified by
C              BODY at the epoch specified by ET.
C
C     BODY     is the integer ID code of the body for which the
C              state transformation matrix is requested. Bodies
C              are numbered according to the standard NAIF numbering
C              scheme. The numbering scheme is explained in the NAIF
C              IDs Required Reading naif_ids.req.
C
C     ET       is the epoch at which the state transformation
C              matrix is requested. (This is typically the
C              epoch of observation minus the one-way light time
C              from the observer to the body at the epoch of
C              observation.)
C
C$ Detailed_Output
C
C     TSIPM    is a 6x6 transformation matrix. It is used to
C              transform states from inertial coordinates to body
C              fixed (also called equator and prime meridian ---
C              PM) coordinates.
C
C              Given a state S in the inertial reference frame
C              specified by REF, the corresponding bodyfixed state
C              is given by the matrix vector product:
C
C                 TSIPM * S
C
C              The X axis of the PM system is directed  to the
C              intersection of the equator and prime meridian.
C              The Z axis points along  the spin axis and points
C              towards the same side of the invariable plane of
C              the solar system as does earth's north pole.
C
C              NOTE: The inverse of TSIPM is NOT its transpose.
C              The matrix, TSIPM, has a structure as shown below:
C
C                 .-            -.
C                 |       :      |
C                 |   R   :  0   |
C                 | ......:......|
C                 |       :      |
C                 | dR/dt :  R   |
C                 |       :      |
C                 `-            -'
C
C              where R is a time varying rotation matrix and dR/dt is
C              its derivative. The inverse of this matrix is:
C
C                 .-              -.
C                 |     T  :       |
C                 |    R   :  0    |
C                 | .......:.......|
C                 |        :       |
C                 |      T :   T   |
C                 | dR/dt  :  R    |
C                 |        :       |
C                 `-              -'
C
C              The SPICELIB routine INVSTM is available for producing
C              this inverse.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If data required to define the body-fixed frame associated
C         with BODY are not found in the binary PCK system or the kernel
C         pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In
C         the case of IAU style body-fixed frames, the absence of
C         prime meridian polynomial data (which are required) is used
C         as an indicator of missing data.
C
C     2)  If the test for exception (1) passes, but in fact requested
C         data are not available in the kernel pool, an error is
C         signaled by a routine in the call tree of this routine.
C
C     3)  If the kernel pool does not contain all of the data required
C         to define the number of nutation precession angles
C         corresponding to the available nutation precession
C         coefficients, the error SPICE(INSUFFICIENTANGLES) is
C         signaled.
C
C     4)  If the reference frame REF is not recognized, an error is
C         signaled by a routine in the call tree of this routine.
C
C     5)  If the specified body code BODY is not recognized, an error is
C         signaled by a routine in the call tree of this routine.
C
C     6)  If, for a given body, both forms of the kernel variable names
C
C            BODY<body ID>_CONSTANTS_JED_EPOCH
C            BODY<body ID>_CONSTS_JED_EPOCH
C
C         are found in the kernel pool, the error
C         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done
C         regardless of whether the values assigned to the kernel
C         variable names match.
C
C     7)  If, for a given body, both forms of the kernel variable names
C
C            BODY<body ID>_CONSTANTS_REF_FRAME
C            BODY<body ID>_CONSTS_REF_FRAME
C
C         are found in the kernel pool, the error
C         SPICE(COMPETINGFRAMESPEC) is signaled. This is done
C         regardless of whether the values assigned to the kernel
C         variable names match.
C
C     8)  If the central body associated with the input BODY, whether
C         a system barycenter or BODY itself, has associated phase
C         angles (aka nutation precession angles), and the kernel
C         variable BODY<body ID>_MAX_PHASE_DEGREE for the central
C         body is present but has a value outside the range 1:3,
C         the error SPICE(DEGREEOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Note: NAIF recommends the use of SPKEZR with the appropriate
C     frames kernels when possible over TISBOD.
C
C     The matrix for transforming inertial states to bodyfixed
C     states is the 6x6 matrix shown below as a block structured
C     matrix.
C
C        .-            -.
C        |       :      |
C        | TIPM  :  0   |
C        | ......:......|
C        |       :      |
C        | DTIPM : TIPM |
C        |       :      |
C        `-            -'
C
C     This can also be expressed in terms of Euler angles
C     PHI, DELTA and W. The transformation from inertial to
C     bodyfixed coordinates is represented in the SPICE kernel
C     pool as:
C
C        TIPM   = [W]  [DELTA]  [PHI]
C                    3        1      3
C     Thus
C
C        DTIPM  = d[W] /dt [DELTA]  [PHI]
C                     3           1      3
C
C               + [W]  d[DELTA] /dt  [PHI]
C                    3             1      3
C
C               + [W]  [DELTA]  d[PHI] /dt
C                    3        1           3
C
C
C     If a binary PCK file record can be used for the time and
C     body requested, it will be used. The most recently loaded
C     binary PCK file has first priority, followed by previously
C     loaded binary PCK files in backward time order. If no
C     binary PCK file has been loaded, the text P_constants
C     kernel file is used.
C
C     If there is only text PCK kernel information, it is
C     expressed in terms of RA, DEC and W, where
C
C        RA  = PHI - HALFPI
C        DEC = HALFPI - DELTA
C        W   = W
C
C     The angles RA, DEC, and W are defined as follows in the
C     text PCK file:
C
C                                      2    .-----
C                      RA1*t      RA2*t      \
C        RA  = RA0  + -------  + -------   +  )  a(i) * sin( theta(i) )
C                        T          2        /
C                                  T        '-----
C                                              i
C
C                                       2   .-----
C                      DEC1*t     DEC2*t     \
C        DEC = DEC0 + -------- + --------  +  )  d(i) * cos( theta(i) )
C                        T           2       /
C                                   T       '-----
C                                              i
C
C                                     2     .-----
C                       W1*t      W2*t       \
C        W   = W0   +  ------  + -------   +  )  w(i) * sin( theta(i) )
C                        d          2        /
C                                  d        '-----
C                                              i
C
C
C     where `d' is in seconds/day; T in seconds/Julian century;
C     a(i), d(i), and w(i) arrays apply to satellites only; and
C     theta(i), defined as
C
C                                THETA1(i)*t
C        theta(i) = THETA0(i) + -------------
C                                     T
C
C     are specific to each planet.
C
C     These angles ---typically nodal rates--- vary in number and
C     definition from one planetary system to the next.
C
C     Thus
C
C                                   .-----
C                 RA1     2*RA2*t    \   a(i)*THETA1(i)*cos(theta(i))
C     dRA/dt   = ----- + --------- +  ) ------------------------------
C                  T          2      /                 T
C                            T      '-----
C                                      i
C
C                                     .-----
C                 DEC1     2*DEC2*t    \   d(i)*THETA1(i)*sin(theta(i))
C      dDEC/dt = ------ + ---------- -  ) ------------------------------
C                   T          2       /                 T
C                             T       '-----
C                                        i
C
C                                 .-----
C                 W1     2*W2*t    \   w(i)*THETA1(i)*cos(theta(i))
C      dW/dt   = ---- + -------- +  ) ------------------------------
C                 d         2      /                 T
C                          d      '-----
C                                    i
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Calculate the matrix to transform a state vector from the
C        J2000 frame to the Saturn fixed frame at a specified
C        time, and use it to compute the geometric position and
C        velocity of Titan in Saturn's body-fixed frame.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: tisbod_ex1.tm
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
C              File name                     Contents
C              ---------                     --------
C              sat375.bsp                    Saturn satellite ephemeris
C              pck00010.tpc                  Planet orientation and
C                                            radii
C              naif0012.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'sat375.bsp',
C                                  'pck00010.tpc',
C                                  'naif0012.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM TISBOD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      SATVEC ( 6    )
C              DOUBLE PRECISION      TSIPM  ( 6, 6 )
C
C              INTEGER               I
C              INTEGER               SATID
C
C        C
C        C     Load the kernels.
C        C
C              CALL FURNSH ( 'tisbod_ex1.tm' )
C
C        C
C        C     The body ID for Saturn.
C        C
C              SATID = 699
C
C        C
C        C     Retrieve the transformation matrix at some time.
C        C
C              CALL STR2ET ( 'Jan 1 2005',   ET        )
C              CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
C
C        C
C        C     Retrieve the state of Titan as seen from Saturn
C        C     in the J2000 frame at ET.
C        C
C              CALL SPKEZR ( 'TITAN',  ET,    'J2000', 'NONE',
C             .              'SATURN', STATE, LT              )
C
C              WRITE(*,'(A)') 'Titan as seen from Saturn '
C             .            // '(J2000 frame):'
C              WRITE(*,'(A,3F13.3)') '   position   (km):',
C             .               ( STATE(I), I=1,3 )
C              WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
C             .               ( STATE(I), I=4,6 )
C
C        C
C        C     Rotate the 6-vector STATE into the
C        C     Saturn body-fixed reference frame.
C        C
C              CALL MXVG ( TSIPM, STATE, 6, 6, SATVEC )
C
C              WRITE(*,'(A)') 'Titan as seen from Saturn '
C             .            // '(IAU_SATURN frame):'
C              WRITE(*,'(A,3F13.3)') '   position   (km):',
C             .               ( SATVEC(I), I=1,3 )
C              WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
C             .               ( SATVEC(I), I=4,6 )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Titan as seen from Saturn (J2000 frame):
C           position   (km):  1071928.661  -505781.970   -60383.976
C           velocity (km/s):        2.404        5.176       -0.560
C        Titan as seen from Saturn (IAU_SATURN frame):
C           position   (km):   401063.338 -1116965.364    -5408.806
C           velocity (km/s):     -177.547      -63.745        0.028
C
C
C        Note that the complete example could be replaced by a single
C        SPKEZR call:
C
C           CALL SPKEZR ( 'TITAN',  ET,    'IAU_SATURN', 'NONE',
C          .              'SATURN', STATE, LT                   )
C
C
C     2) Use TISBOD is used to compute the angular velocity vector (with
C        respect to the J2000 inertial frame) of the specified body at
C        given time.
C
C        Use the meta-kernel from Example 1 above.
C
C
C        Example code begins here.
C
C
C              PROGRAM TISBOD_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      AV     ( 3    )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      DTIPM  ( 3, 3 )
C              DOUBLE PRECISION      OMEGA  ( 3, 3 )
C              DOUBLE PRECISION      ROT    ( 3, 3 )
C              DOUBLE PRECISION      TIPM   ( 3, 3 )
C              DOUBLE PRECISION      TSIPM  ( 6, 6 )
C              DOUBLE PRECISION      V      ( 3    )
C
C              INTEGER               I
C              INTEGER               J
C              INTEGER               SATID
C
C        C
C        C     Load the kernels.
C        C
C              CALL FURNSH ( 'tisbod_ex1.tm' )
C
C        C
C        C     The body ID for Saturn.
C        C
C              SATID = 699
C
C        C
C        C     First get the state transformation matrix.
C        C
C              CALL STR2ET ( 'Jan 1 2005',   ET        )
C              CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
C
C        C
C        C     This matrix has the form:
C        C
C        C          .-            -.
C        C          |       :      |
C        C          | TIPM  :  0   |
C        C          | ......:......|
C        C          |       :      |
C        C          | DTIPM : TIPM |
C        C          |       :      |
C        C          `-            -'
C        C
C        C     We extract TIPM and DTIPM
C        C
C              DO  I = 1,3
C                 DO  J = 1,3
C
C                    TIPM  ( I, J ) = TSIPM ( I,   J )
C                    DTIPM ( I, J ) = TSIPM ( I+3, J )
C
C                 END DO
C              END DO
C
C        C
C        C     The transpose of TIPM and DTIPM, (TPMI and DTPMI), gives
C        C     the transformation from bodyfixed coordinates to inertial
C        C     coordinates.
C        C
C        C     Here is a fact about the relationship between angular
C        C     velocity associated with a time varying rotation matrix
C        C     that gives the orientation of a body with respect to
C        C     an inertial frame.
C        C
C        C        The angular velocity vector can be read from the off
C        C        diagonal components of the matrix product:
C        C
C        C                                t
C        C        OMEGA =     DTPMI * TPMI
C        C
C        C                         t
C        C              =     DTIPM * TIPM
C        C
C        C        the components of the angular velocity V will appear
C        C        in this matrix as:
C        C
C        C            .-                   -.
C        C            |                     |
C        C            |   0    -V(3)  V(2)  |
C        C            |                     |
C        C            |  V(3)    0   -V(1)  |
C        C            |                     |
C        C            | -V(2)   V(1)   0    |
C        C            |                     |
C        C            `-                   -'
C        C
C        C
C              CALL MTXM ( DTIPM, TIPM, OMEGA )
C
C              V(1) = OMEGA (3,2)
C              V(2) = OMEGA (1,3)
C              V(3) = OMEGA (2,1)
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A)') 'Angular velocity (km/s):'
C              WRITE(*,'(3F16.9)') V
C
C        C
C        C     It is possible to compute the angular velocity using
C        C     a single call to XF2RAV.
C        C
C              CALL XF2RAV ( TSIPM, ROT, AV )
C
C              WRITE(*,'(A)') 'Angular velocity using XF2RAV (km/s):'
C              WRITE(*,'(3F16.9)') AV
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Angular velocity (km/s):
C             0.000014001     0.000011995     0.000162744
C        Angular velocity using XF2RAV (km/s):
C             0.000014001     0.000011995     0.000162744
C
C
C$ Restrictions
C
C     1)  The kernel pool must be loaded with the appropriate
C         coefficients (from a text or binary PCK file) prior to calling
C         this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     K.S. Zukor         (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.5.1, 16-DEC-2021 (NJB) (JDR)
C
C        The routine was updated to support user-defined maximum phase
C        angle degrees. The additional text kernel kernel variable name
C        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle
C        polynomials have degree higher than 1. The maximum allowed
C        degree is 3.
C
C        The kernel variable names
C
C           BODY#_CONSTS_REF_FRAME
C           BODY#_CONSTS_JED_EPOCH
C
C        are now recognized.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Added note to $Particulars section.
C
C-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
C
C        The routine was updated to be more efficient by using a hash
C        and buffers so save text PCK data instead of doing kernel POOL
C        look-ups over an over again. The routine now checks the POOL
C        state counter and dumps all buffered data if it changes.
C
C        BUG FIX: changed available room in the BODVCD call
C        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2.
C
C-    SPICELIB Version 4.4.0, 01-FEB-2008 (NJB)
C
C        The routine was updated to improve the error messages created
C        when required PCK data are not found. Now in most cases the
C        messages are created locally rather than by the kernel pool
C        access routines. In particular missing binary PCK data will
C        be indicated with a reasonable error message.
C
C-    SPICELIB Version 4.3.0, 13-DEC-2005 (NJB)
C
C        Bug fix: previous update introduced bug in state
C        transformation when REF was unequal to PCK native frame.
C
C-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
C
C        Re-wrote portions of algorithm to simplify source code.
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and VADDG calls.
C
C        Replaced calls to ZZBODVCD with calls to BODVCD.
C
C-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.
C
C-    SPICELIB Version 4.0.0, 12-FEB-2004 (NJB)
C
C        Code has been updated to support satellite ID codes in the
C        range 10000 to 99999 and to allow nutation precession angles
C        to be associated with any object.
C
C        Implementation changes were made to improve robustness
C        of the code.
C
C-    SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
C
C        Properly initialized the variable NPAIRS.
C
C-    SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C        Changed to call PCKMAT rather than PCKEUL.
C
C-    SPICELIB Version 3.1.0, 18-OCT-1994 (KSZ)
C
C        Fixed bug which incorrectly modded DW by two pi.
C
C-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        Changed to look for binary PCK file, and used this
C        to find Euler angles, if such data has been loaded.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
C
C        Updated to handle P_constants referenced to different epochs
C        and inertial reference frames.
C
C        $Required_Reading and $Literature_References sections were
C        updated.
C
C-    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     transformation from inertial state to bodyfixed
C
C-&


C$ Revisions
C
C-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
C
C        The routine was updated to be more efficient by using a hash
C        and buffers so save text PCK data instead of doing kernel POOL
C        look-ups over an over again. The routine now checks the POOL
C        state counter and dumps all buffered data if it changes.
C
C-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
C
C        Re-wrote portions of algorithm to simplify source code.
C        The routine now takes advantage of EUL2XF, which wasn't
C        available when the first version of this routine was written.
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and VADDG calls.
C
C        Replaced calls to ZZBODVCD with calls to BODVCD.
C
C-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added. The new checks
C        are intended to prevent arithmetic operations from
C        being performed with uninitialized or invalid data.
C
C-     SPICELIB Version 4.0.0, 27-JAN-2004 (NJB)
C
C         Code has been updated to support satellite ID codes in the
C         range 10000 to 99999 and to allow nutation precession angles
C         to be associated with any object.
C
C         Calls to deprecated kernel pool access routine RTPOOL
C         were replaced by calls to GDPOOL.
C
C         Calls to BODVAR have been replaced with calls to
C         ZZBODVCD.
C
C-     SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
C
C        The variable NPAIRS is now initialized
C        at the same point as NA, NTHETA, ND, and NW to be
C        zero. This prevents the routine from performing
C        needless calculations for planets and avoids possible
C        floating point exceptions.
C
C-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C        TISBOD now gets the TSIPM matrix from PCKMAT.
C        Reference frame calculation moved to end.
C
C-     SPICELIB Version 3.0.1, 07-OCT-1994 (KSZ)
C
C        TISBOD bug which mistakenly moded DW by 2PI
C        was removed.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        TISBOD now uses new software to check for the
C        existence of binary PCK files, search the for
C        data corresponding to the requested body and time,
C        and return the appropriate Euler angles. Otherwise
C        the code calculates the Euler angles from the
C        P_constants kernel file.
C
C-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
C
C         Updated to handle P_constants referenced to different epochs
C         and inertial reference frames.
C
C         TISBOD now checks the kernel pool for presence of the
C         variables
C
C            BODY#_CONSTANTS_REF_FRAME
C
C         and
C
C            BODY#_CONSTANTS_JED_EPOCH
C
C         where # is the NAIF integer code of the barycenter of a
C         planetary system or of a body other than a planet or
C         satellite. If either or both of these variables are
C         present, the P_constants for BODY are presumed to be
C         referenced to the specified inertial frame or epoch.
C         If the epoch of the constants is not J2000, the input
C         time ET is converted to seconds past the reference epoch.
C         If the frame of the constants is not the frame specified
C         by REF, the rotation from the P_constants' frame to
C         body-fixed coordinates is transformed to the rotation from
C         the requested frame to body-fixed coordinates. The same
C         transformation is applied to the derivative of this
C         rotation.
C
C         Due to the prescience of the original author, the code
C         was already prepared to handle the possibility of
C         specification of a P_constants inertial reference frame via
C         kernel pool variables.
C
C
C         Also, the $Required_Reading and $Literature_References
C         sections were updated. The SPK required reading has been
C         deleted from the $Literature_References section, and the
C         NAIF_IDS, KERNEL, and TIME Required Reading files have
C         been added in the $Required_Reading section.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDOTG

      INTEGER               ZZBODBRY

      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 200 )

C
C     Maximum number of coefficients per phase angle polynomial.
C
      INTEGER               MXPHAS
      PARAMETER           ( MXPHAS = 4 )

      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

C
C     Local variables
C
      CHARACTER*(1)         DTYPE
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(FRNMLN)    FIXFRM
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(NAMLEN)    ITEM2
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      AC     (    MAXANG )
      DOUBLE PRECISION      COSTH  (    MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     (    MAXANG )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DCOSTH (    MAXANG )
      DOUBLE PRECISION      DDEC
      DOUBLE PRECISION      DDELTA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      DRA
      DOUBLE PRECISION      DSINTH (    MAXANG )
      DOUBLE PRECISION      DTIPM  (     3,  3 )
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      DW
      DOUBLE PRECISION      EULSTA (     6     )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      PCKEPC
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      REQ2PC (     3, 3  )
      DOUBLE PRECISION      SINTH  (    MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TC
      DOUBLE PRECISION      TD
      DOUBLE PRECISION      TCOEF  ( MXPHAS * MAXANG )
      DOUBLE PRECISION      TIPM   (     3, 3  )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TMPEPC
      DOUBLE PRECISION      W
      DOUBLE PRECISION      WC     (    MAXANG )
      DOUBLE PRECISION      WCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      XDTIPM (     3, 3  )
      DOUBLE PRECISION      XTIPM  (     3, 3  )

      INTEGER               CENT
      INTEGER               DEG
      INTEGER               DIM
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               J
      INTEGER               J2CODE
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               NA
      INTEGER               ND
      INTEGER               NPHASE
      INTEGER               NPHSCO
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               PCREF
      INTEGER               REFID
      INTEGER               REQREF

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               FOUND2

C
C     POOL state counter.
C
      INTEGER               PULCTR ( CTRSIZ )

      LOGICAL               UPDATE

C
C     ID-based hash for text PCK data. KIDLST, KIDPOL, and KIDIDS
C     provide the index in the body PCK data arrays at which the data
C     of the body with a given ID are stored.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBOD
      PARAMETER           ( MAXBOD = 157 )

      INTEGER               BIDLST (             MAXBOD )
      INTEGER               BIDPOL ( LBPOOL :    MAXBOD )
      INTEGER               BIDIDS (             MAXBOD )

      DOUBLE PRECISION      BAC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BDC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BDCOEF ( 0:MXPOLY-1, MAXBOD )
      DOUBLE PRECISION      BPCKEP (             MAXBOD )
      DOUBLE PRECISION      BRCOEF ( 0:MXPOLY-1, MAXBOD )
      DOUBLE PRECISION      BTCOEF ( MXPHAS * MAXANG, MAXBOD )
      DOUBLE PRECISION      BWC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BWCOEF ( 0:MXPOLY-1, MAXBOD )

      INTEGER               BNA    (             MAXBOD )
      INTEGER               BND    (             MAXBOD )
      INTEGER               BNPHAS (             MAXBOD )
      INTEGER               BNPHCO (             MAXBOD )
      INTEGER               BNW    (             MAXBOD )
      INTEGER               BPCREF (             MAXBOD )

      DOUBLE PRECISION      SINTMP
      DOUBLE PRECISION      COSTMP

      INTEGER               AT
      INTEGER               AVAIL

      LOGICAL               NEW

C
C     Saved variables
C
C     Because we need to save almost everything, we save everything
C     rather than taking a chance and accidentally leaving something
C     off the list.
C
      SAVE

C
C     Initial values
C
      DATA  BPCREF / MAXBOD * 0 /
      DATA  FIRST  / .TRUE.  /
      DATA  FOUND  / .FALSE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TISBOD' )

C
C     Perform any needed first pass initializations.
C
      IF ( FIRST ) THEN

C
C        Initialize POOL state counter to the user value.
C
         CALL ZZCTRUIN ( PULCTR )

C
C        Initialize kernel POOL frame hashes.
C
         CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )

C
C        Get the code for the J2000 frame.
C
         CALL IRFNUM ( 'J2000', J2CODE )

C
C        Set seconds per day and per century.
C
         D = SPD()
         T = D  *  36525.D0

         FIRST = .FALSE.

      END IF

      CALL IRFNUM  ( REF, REQREF )

C
C     Get state transformation matrix from high precision PCK file, if
C     available.
C
      CALL PCKMAT ( BODY, ET, PCREF, TSIPM, FOUND )

      IF ( .NOT. FOUND ) THEN
C
C        The data for the frame of interest are not available in a
C        loaded binary PCK file. This is not an error: the data may be
C        present in the kernel pool.
C
C        Check the POOL counter. If it changed, dump all buffered
C        constants data.
C
         CALL ZZPCTRCK ( PULCTR, UPDATE )

         IF ( UPDATE ) THEN
            CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )
         END IF

C
C        Check if we have data for this body in our buffers.
C
         CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, BODY, AT )

         IF ( AT .NE. 0 ) THEN
C
C           Set PCREF as it is used after the text PCK "IF".
C
            PCREF  = BPCREF ( AT )

         ELSE
C
C           Conduct a non-error-signaling check for the presence of a
C           kernel variable that is required to implement an IAU style
C           body-fixed reference frame. If the data aren't available,
C           we don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND)
C           error; we want to issue the error signal locally, with a
C           better error message.
C
            ITEM = 'BODY#_PM'
            CALL REPMI  ( ITEM, '#',   BODY, ITEM  )
            CALL DTPOOL ( ITEM, FOUND, NW,   DTYPE )

            IF ( .NOT. FOUND ) THEN
C
C              Now we do have an error.
C
C              We don't have the data we'll need to produced the
C              requested state transformation matrix. In order to
C              create an error message understandable to the user,
C              find, if possible, the name of the reference frame
C              associated with the input body. Note that the body is
C              really identified by a PCK frame class ID code, though
C              most of the documentation just calls it a body ID code.
C
               CALL CCIFRM ( PCK, BODY,  FRCODE, FIXFRM, CENT, FOUND )
               CALL ETCAL  ( ET,  TIMSTR )

               ERRMSG = 'PCK data required to compute the orientation '
     .      //       'of the # # for epoch # TDB were not found. '
     .      //       'If these data were to be provided by a binary '
     .      //       'PCK file, then it is possible that the PCK '
     .      //       'file does not have coverage for the specified '
     .      //       'body-fixed frame at the time of interest. If the '
     .      //       'data were to be provided by a text PCK file, '
     .      //       'then possibly the file does not contain data '
     .      //       'for the specified body-fixed frame. In '
     .      //       'either case it is possible that a required '
     .      //       'PCK file was not loaded at all.'

C
C              Fill in the variable data in the error message.
C
               IF ( FOUND ) THEN
C
C                 The frame system knows the name of the body-fixed
C                 frame.
C
                  CALL SETMSG ( ERRMSG                  )
                  CALL ERRCH  ( '#', 'body-fixed frame' )
                  CALL ERRCH  ( '#', FIXFRM             )
                  CALL ERRCH  ( '#', TIMSTR             )

               ELSE
C
C                 The frame system doesn't know the name of the
C                 body-fixed frame, most likely due to a missing
C                 frame kernel.
C
                  CALL SUFFIX ( '#', 1, ERRMSG                      )
                  CALL SETMSG ( ERRMSG                              )
                  CALL ERRCH  ( '#',
     .                       'body-fixed frame associated with '
     .         //            'the ID code'                       )
                  CALL ERRINT ( '#', BODY                           )
                  CALL ERRCH  ( '#', TIMSTR                         )
                  CALL ERRCH  ( '#',
     .                       'Also, a frame kernel defining the '
     .         //            'body-fixed frame associated with '
     .         //            'body # may need to be loaded.'     )
                  CALL ERRINT ( '#', BODY                           )

               END IF

               CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)' )
               CALL CHKOUT ( 'TISBOD'                   )
               RETURN

            END IF

C
C           Find the body code used to label the reference frame and
C           epoch specifiers for the orientation constants for BODY.
C
C           For planetary systems, the reference frame and epoch for
C           the orientation constants is associated with the system
C           barycenter, not with individual bodies in the system. For
C           any other bodies, (the Sun or asteroids, for example) the
C           body's own code is used as the label.
C
            REFID = ZZBODBRY ( BODY )

C
C           Look up the epoch of the constants. The epoch is specified
C           as a Julian ephemeris date. The epoch defaults to J2000.
C
C           Look for both forms of the JED epoch kernel variable. At
C           most one is allowed to be present.
C
            ITEM   =  'BODY#_CONSTANTS_JED_EPOCH'
            CALL REPMI ( ITEM, '#', REFID, ITEM )

            ITEM2  =  'BODY#_CONSTS_JED_EPOCH'
            CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

            CALL GDPOOL ( ITEM, 1, 1, DIM, PCKEPC, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL GDPOOL ( ITEM2, 1, 1, DIM, PCKEPC, FOUND2 )

               IF ( .NOT. FOUND2 ) THEN
                  PCKEPC = J2000()
               END IF

            ELSE
C
C              Check for presence of both forms of the variable names.
C
               CALL DTPOOL ( ITEM2, FOUND2, DIM, DTYPE )

               IF ( FOUND2 ) THEN

                  CALL SETMSG ( 'Both kernel variables # and # are '
     .            //            'present in the kernel pool. At most '
     .            //            'one form of the kernel variable name '
     .            //            'may be present.'           )
                  CALL ERRCH  ( '#', ITEM                   )
                  CALL ERRCH  ( '#', ITEM2                  )
                  CALL SIGERR ( 'SPICE(COMPETINGEPOCHSPEC)' )
                  CALL CHKOUT ( 'TISBOD'                    )
                  RETURN

               END IF

            END IF

C
C           Look up the reference frame of the constants. The reference
C           frame is specified by a code recognized by CHGIRF. The
C           default frame is J2000, symbolized by the code J2CODE.
C
            ITEM   =  'BODY#_CONSTANTS_REF_FRAME'
            CALL REPMI ( ITEM, '#', REFID, ITEM )

            ITEM2  =  'BODY#_CONSTS_REF_FRAME'
            CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

            CALL REPMI  ( ITEM, '#', REFID, ITEM              )
            CALL GIPOOL ( ITEM,  1,  1,     DIM, PCREF, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL GIPOOL ( ITEM2, 1, 1, DIM, PCREF, FOUND )

               IF ( .NOT. FOUND ) THEN
                  PCREF  =  J2CODE
               END IF

            ELSE
C
C              Check for presence of both forms of the variable names.
C
               CALL DTPOOL ( ITEM2, FOUND, DIM, DTYPE )

               IF ( FOUND ) THEN

                  CALL SETMSG ( 'Both kernel variables # and # are '
     .            //            'present in the kernel pool. At most '
     .            //            'one form of the kernel variable name '
     .            //            'may be present.'           )
                  CALL ERRCH  ( '#', ITEM                   )
                  CALL ERRCH  ( '#', ITEM2                  )
                  CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
                  CALL CHKOUT ( 'TISBOD'                    )
                  RETURN

               END IF

            END IF

C
C           Whatever the body, it has quadratic time polynomials for
C           the RA and Dec of the pole, and for the rotation of the
C           Prime Meridian.
C
            ITEM = 'POLE_RA'
            CALL CLEARD (             MXPOLY,     RCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, NA, RCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

            ITEM = 'POLE_DEC'
            CALL CLEARD (             MXPOLY,     DCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, ND, DCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

            ITEM = 'PM'
            CALL CLEARD (             MXPOLY,     WCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, NW, WCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

C
C           Whether or not the body is a satellite, there may be
C           additional nutation and libration (THETA) terms.
C
            NTHETA = 0
            NPHASE = 0
            NPHSCO = 0
            NA     = 0
            ND     = 0
            NW     = 0

            ITEM = 'NUT_PREC_ANGLES'

            IF ( BODFND ( REFID, ITEM ) ) THEN

C              Find out whether the maximum phase angle degree
C              has been explicitly set.
C
               ITEM2 = 'MAX_PHASE_DEGREE'

               IF ( BODFND(REFID, ITEM2) ) THEN

                  CALL BODVCD ( REFID, ITEM2, 1, DIM, DPVAL )

                  DEG = NINT( DPVAL )

                  IF ( ( DEG .LT. 1 ) .OR. ( DEG .GT. MXPHAS-1 ) ) THEN

                     CALL SETMSG ( 'Maximum phase angle degree for '
     .               //            'body # must be in the range 1:# '
     .               //            'but was #.'                      )
                     CALL ERRINT ( '#', REFID                        )
                     CALL ERRINT ( '#', MXPHAS-1                     )
                     CALL ERRINT ( '#', DEG                          )
                     CALL SIGERR ( 'SPICE(DEGREEOUTOFRANGE)'         )
                     CALL CHKOUT ( 'TISBOD'                          )
                     RETURN

                  END IF

                  NPHSCO = DEG + 1

               ELSE
C
C                 The default degree is 1, yielding two coefficients.
C
                  NPHSCO = 2

               END IF
C
C              There is something a bit obscure going on below. BODVCD
C              loads the array TCOEF in the following order
C
C                 TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO),
C                 TCOEF(1,2), TCOEF(2,2), ...
C
C              The NTHETA that comes back is the total number of items
C              loaded, but we will need the actual limit on the second
C              dimension. That is --- NTHETA / NPHSCO.
C
               CALL BODVCD ( REFID, ITEM, MAXANG*MXPHAS, NTHETA, TCOEF )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

C
C              NPHSCO is at least 1; this division is safe.
C
               NPHASE = NTHETA / NPHSCO

            END IF

C
C           Look up the right ascension nutations in the precession of
C           the pole. NA is the number of Ascension coefficients. AC
C           are the Ascension coefficients.
C
            ITEM = 'NUT_PREC_RA'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, NA, AC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

C
C           Look up the declination nutations in the precession of the
C           pole. ND is the number of Declination coefficients. DC are
C           the Declination coefficients.
C
            ITEM = 'NUT_PREC_DEC'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, ND, DC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

C
C           Finally look up the prime meridian nutations. NW is the
C           number of coefficients. WC is the array of coefficients.
C
            ITEM = 'NUT_PREC_PM'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, NW, WC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

C
C           The number of coefficients returned had better not be
C           bigger than the number of angles we are going to compute.
C           If it is we simply signal an error and bag it, for sure.
C
            IF ( MAX( NA, ND, NW ) .GT. NPHASE ) THEN
               CALL SETMSG ( 'Insufficient number of nutation/'
     .         //            'precession angles for body * at time #. '
     .         //            'Number of angles is #; number required '
     .         //            'is #.'                )
               CALL ERRINT ( '*', BODY              )
               CALL ERRDP  ( '#', ET                )
               CALL ERRINT ( '#', NPHASE            )
               CALL ERRINT ( '#', MAX( NA, ND, NW ) )
               CALL SIGERR ( 'SPICE(INSUFFICIENTANGLES)' )
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

C
C           We succeeded to fetch all data for this body. Save it in
C           the buffers. First check if there is room. If not, dump the
C           buffers to make room.
C
            CALL ZZHSIAVL( BIDPOL, AVAIL )
            IF ( AVAIL .LE. 0 ) THEN
               CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )
            END IF

C
C           Add this body to the hash and save its data in the buffers.
C
            CALL ZZHSIADD ( BIDLST, BIDPOL, BIDIDS, BODY, AT, NEW )

            BPCKEP ( AT ) = PCKEPC
            BPCREF ( AT ) = PCREF
            BNPHAS ( AT ) = NPHASE
            BNPHCO ( AT ) = NPHSCO
            BNA    ( AT ) = NA
            BND    ( AT ) = ND
            BNW    ( AT ) = NW

            CALL MOVED( RCOEF( 0 ), MXPOLY,        BRCOEF( 0, AT ) )
            CALL MOVED( DCOEF( 0 ), MXPOLY,        BDCOEF( 0, AT ) )
            CALL MOVED( WCOEF( 0 ), MXPOLY,        BWCOEF( 0, AT ) )
            CALL MOVED( TCOEF( 1 ), MAXANG*NPHSCO, BTCOEF( 1, AT ) )
            CALL MOVED( AC   ( 1 ), MAXANG,        BAC   ( 1, AT ) )
            CALL MOVED( DC   ( 1 ), MAXANG,        BDC   ( 1, AT ) )
            CALL MOVED( WC   ( 1 ), MAXANG,        BWC   ( 1, AT ) )

         END IF

C
C        The reference epoch in the PCK is given as JED. Convert to
C        ephemeris seconds past J2000. Then convert the input ET to
C        seconds past the reference epoch.
C
         TMPEPC  =  SPD() * ( BPCKEP(AT) - J2000() )
         EPOCH   =  ET    -   TMPEPC
         TD      =  EPOCH / D
         TC      =  EPOCH / T

C
C        Evaluate the time polynomials and their derivatives w.r.t.
C        EPOCH at EPOCH.
C
C        Evaluate the time polynomials at EPOCH.
C
         RA  = BRCOEF(0,AT) + TC * ( BRCOEF(1,AT) + TC * BRCOEF(2,AT) )
         DEC = BDCOEF(0,AT) + TC * ( BDCOEF(1,AT) + TC * BDCOEF(2,AT) )
         W   = BWCOEF(0,AT) + TD * ( BWCOEF(1,AT) + TD * BWCOEF(2,AT) )

         DRA  = ( BRCOEF(1,AT)  + 2.0D0*TC*BRCOEF(2,AT) ) / T
         DDEC = ( BDCOEF(1,AT)  + 2.0D0*TC*BDCOEF(2,AT) ) / T
         DW   = ( BWCOEF(1,AT)  + 2.0D0*TD*BWCOEF(2,AT) ) / D

C
C        Compute the nutations and librations (and their derivatives)
C        as appropriate.
C
         DO I = 1, BNPHAS(AT)

            IF ( BNPHCO(AT) .EQ. 2 ) THEN
C
C              This case is so common that we'll deal with it
C              separately. We'll avoid unnecessary arithmetic
C              operations.
C
               K = 1 + 2*(I-1)
               M = 1 + K

               THETA  = ( BTCOEF(K,AT) + TC * BTCOEF(M,AT) ) * RPD()
               DTHETA = ( BTCOEF(M,AT) / T ) * RPD()

            ELSE
C
C              THETA and DTHETA have higher-order terms; there are
C              BNPHCO(AT) coefficients for each angle.
C
               THETA = 0.D0

               DO J = 1, BNPHCO(AT)
C
C                 K is the start index for the coefficients of the
C                 Ith angle.
C
                  K     = J + BNPHCO(AT)*(I-1)

                  THETA = THETA  + ( TC**(J-1) * BTCOEF(K,AT) )

               END DO

               THETA  = THETA * RPD()

               K = 1 + BNPHCO(AT)*(I-1)
               M = 1 + K

               DTHETA = BTCOEF(M,AT) / T

               DO J = 3, BNPHCO(AT)

                  K      = J + BNPHCO(AT)*(I-1)
                  L      = J - 1
C
C                 Differentiate with respect to EPOCH the Jth
C                 term of the expression for the Ith angle; add
C                 the result to the current sum.
C
                  DTHETA = DTHETA +
     .                     ( L * TC**(L-1) * BTCOEF(K,AT) ) / T**(L-1)
               END DO

               DTHETA = DTHETA * RPD()

            END IF

            SINTMP    =  SIN ( THETA )
            COSTMP    =  COS ( THETA )

            SINTH(I)  =  SINTMP
            COSTH(I)  =  COSTMP
            DSINTH(I) =  COSTMP * DTHETA
            DCOSTH(I) = -SINTMP * DTHETA

         END DO

C
C        Adjust RA, DEC, W and their derivatives by the librations
C        and nutations.
C
         RA  = RA    + VDOTG ( BAC(1,AT), SINTH,  BNA(AT) )
         DEC = DEC   + VDOTG ( BDC(1,AT), COSTH,  BND(AT) )
         W   = W     + VDOTG ( BWC(1,AT), SINTH,  BNW(AT) )

         DRA  = DRA  + VDOTG ( BAC(1,AT), DSINTH, BNA(AT) )
         DDEC = DDEC + VDOTG ( BDC(1,AT), DCOSTH, BND(AT) )
         DW   = DW   + VDOTG ( BWC(1,AT), DSINTH, BNW(AT) )

C
C        Convert from degrees to radians
C
         RA      = RA   * RPD()
         DEC     = DEC  * RPD()
         W       = W    * RPD()

         DRA     = DRA  * RPD()
         DDEC    = DDEC * RPD()
         DW      = DW   * RPD()
C
C        Convert to Euler angles.
C
         W       = MOD ( W,  TWOPI() )
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC

         DPHI    =   DRA
         DDELTA  = - DDEC

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

C
C        Pack the Euler angles and their derivatives into
C        a state vector.
C
         CALL VPACK ( W,  DELTA,  PHI,  EULSTA    )
         CALL VPACK ( DW, DDELTA, DPHI, EULSTA(4) )

C
C        Find the state transformation defined by the Euler angle
C        state vector. The transformation matrix TSIPM has the
C        following structure:
C
C            -            -
C           |       :      |
C           | TIPM  :  0   |
C           | ......:......|
C           |       :      |
C           | DTIPM : TIPM |
C           |       :      |
C            -            -
C
         CALL EUL2XF ( EULSTA, 3, 1, 3, TSIPM )

      END IF

C
C     At this point the base frame PCREF has been determined.
C
C     If the requested base frame is not base frame associated with the
C     PCK data, adjust the transformation matrix TSIPM to map from the
C     requested frame to the body-fixed frame.
C
      IF ( REQREF .NE. PCREF ) THEN
C
C        Next get the position transformation from the user specified
C        inertial frame to the native PCK inertial frame.
C
         CALL IRFROT ( REQREF, PCREF, REQ2PC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

C
C        Since we're applying an inertial transformation to TSIPM,
C        we can rotate the non-zero blocks of TSIPM. This saves
C        a bunch of double precision multiplications.
C
C        Extract the upper and lower left blocks of TSIPM.
C
         DO I = 1, 3

            DO J = 1, 3

               TIPM (I,J) =  TSIPM( I,   J )
               DTIPM(I,J) =  TSIPM( I+3, J )

            END DO

         END DO

C
C        Rotate the blocks. Note this is a right multiplication.
C
         CALL MXM ( TIPM,  REQ2PC, XTIPM  )
         CALL MXM ( DTIPM, REQ2PC, XDTIPM )

C
C        Replace the non-zero blocks of TSIPM. This gives us the
C        transformation from the requested frame to the
C        bodyfixed frame.
C
         DO I = 1, 3

            DO J = 1, 3

               TSIPM( I,   J   ) = XTIPM (I,J)
               TSIPM( I+3, J+3 ) = XTIPM (I,J)
               TSIPM( I+3, J   ) = XDTIPM(I,J)

            END DO

         END DO

      END IF
C
C     That's all folks. Check out and get out.
C
      CALL CHKOUT ( 'TISBOD' )
      RETURN
      END

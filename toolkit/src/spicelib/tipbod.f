C$Procedure TIPBOD ( Transformation, inertial position to bodyfixed )

      SUBROUTINE TIPBOD ( REF, BODY, ET, TIPM )

C$ Abstract
C
C     Return a 3x3 matrix that transforms positions in inertial
C     coordinates to positions in body-equator-and-prime-meridian
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

      CHARACTER*(*)         REF
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TIPM   ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     REF        I   ID of inertial reference frame to transform from.
C     BODY       I   ID code of body.
C     ET         I   Epoch of transformation.
C     TIPM       O   Position transformation matrix, inertial to prime
C                    meridian.
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
C              The output TIPM will give the transformation
C              from this frame to the bodyfixed frame specified by
C              BODY at the epoch specified by ET.
C
C     BODY     is the integer ID code of the body for which the
C              position transformation matrix is requested. Bodies
C              are numbered according to the standard NAIF numbering
C              scheme. The numbering scheme is explained in the NAIF
C              IDs Required Reading naif_ids.req.
C
C     ET       is the epoch at which the position transformation
C              matrix is requested. (This is typically the
C              epoch of observation minus the one-way light time
C              from the observer to the body at the epoch of
C              observation.)
C
C$ Detailed_Output
C
C     TIPM     is a 3x3 coordinate transformation matrix. It is
C              used to transform positions from inertial
C              coordinates to body fixed (also called equator and
C              prime meridian --- PM) coordinates.
C
C              Given a position P in the inertial reference frame
C              specified by REF, the corresponding bodyfixed
C              position is given by the matrix vector product:
C
C                 TIPM * S
C
C              The X axis of the PM system is directed to the
C              intersection of the equator and prime meridian.
C              The Z axis points along  the spin axis and points
C              towards the same side of the invariable plane of
C              the solar system as does earth's north pole.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the kernel pool does not contain all of the data required
C         for computing the transformation matrix, TIPM, the error
C         SPICE(INSUFFICIENTANGLES) is signaled.
C
C     2)  If the reference frame, REF, is not recognized, an error is
C         signaled by a routine in the call tree of this routine.
C
C     3)  If the specified body code, BODY, is not recognized, an error
C         is signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TIPBOD takes PCK information as input, either in the
C     form of a binary or text PCK file. High precision
C     binary files are searched for first (the last loaded
C     file takes precedence); then it defaults to the text
C     PCK file. If binary information is found for the
C     requested body and time, the Euler angles are
C     evaluated and the transformation matrix is calculated
C     from them. Using the Euler angles PHI, DELTA and W
C     we compute
C
C        TIPM = [W]  [DELTA]  [PHI]
C                  3        1      3
C
C     If no appropriate binary PCK files have been loaded,
C     the text PCK file is used. Here information is found
C     as RA, DEC and W (with the possible addition of nutation
C     and libration terms for satellites). Again, the Euler
C     angles are found, and the transformation matrix is
C     calculated from them. The transformation from inertial to
C     body-fixed coordinates is represented as:
C
C        TIPM = [W]  [HALFPI-DEC]  [RA+HALFPI]
C                  3             1            3
C
C     These are basically the Euler angles, PHI, DELTA and W:
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
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Calculate the matrix to rotate a position vector from the
C        J2000 frame to the Saturn fixed frame at a specified
C        time, and use it to compute the position of Titan in
C        Saturn's body-fixed frame.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: tipbod_ex1.tm
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
C              PROGRAM TIPBOD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      POS    ( 3    )
C              DOUBLE PRECISION      SATVEC ( 3    )
C              DOUBLE PRECISION      TIPM   ( 3, 3 )
C
C              INTEGER               SATID
C
C        C
C        C     Load the kernels.
C        C
C              CALL FURNSH ( 'tipbod_ex1.tm' )
C
C        C
C        C     The body ID for Saturn.
C        C
C              SATID = 699
C
C        C
C        C     Retrieve the transformation matrix at some time.
C        C
C              CALL STR2ET ( 'Jan 1 2005',   ET       )
C              CALL TIPBOD ( 'J2000', SATID, ET, TIPM )
C
C        C
C        C     Retrieve the position of Titan as seen from Saturn
C        C     in the J2000 frame at ET.
C        C
C              CALL SPKPOS ( 'TITAN',  ET, 'J2000', 'NONE',
C             .              'SATURN', POS, LT            )
C
C              WRITE(*,'(A)')        'Titan as seen from Saturn:'
C              WRITE(*,'(A,3F13.3)') '   in J2000 frame     :', POS
C
C        C
C        C     Rotate the position 3-vector POS into the
C        C     Saturn body-fixed reference frame.
C        C
C              CALL MXV ( TIPM, POS, SATVEC )
C
C              WRITE(*,'(A,3F13.3)') '   in IAU_SATURN frame:', SATVEC
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Titan as seen from Saturn:
C           in J2000 frame     :  1071928.661  -505781.970   -60383.976
C           in IAU_SATURN frame:   401063.338 -1116965.364    -5408.806
C
C
C        Note that the complete example could be replaced by a single
C        SPKPOS call:
C
C           CALL SPKPOS ( 'TITAN',  ET, 'IAU_SATURN', 'NONE',
C          .              'SATURN', POS, LT                   )
C
C$ Restrictions
C
C     1)  The kernel pool must be loaded with the appropriate
C         coefficients (from a text or binary PCK file) prior to
C         calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     K.S. Zukor         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary entries in $Revisions section.
C
C        Added complete code example.
C
C        Added frames.req to $Required_Reading.
C
C-    SPICELIB Version 1.3.0, 02-MAR-2016 (BVS)
C
C        Updated to use the 3x3 top-left corner of the 6x6 matrix
C        returned by TISBOD instead of fetching kernel data and doing
C        computations in-line.
C
C        Fixed indentation of some header sections.
C
C-    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM call. Replaced header references to LDPOOL with
C        references to FURNSH.
C
C-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ)
C
C        Underlying BODMAT code changed to look for binary PCK
C        data files, and use them to get orientation information if
C        they are available. Only the comments to TIPBOD changed.
C
C-    SPICELIB Version 1.0.2, 06-JUL-1993 (HAN)
C
C        Example in header was corrected. Previous version had
C        incorrect matrix dimension specifications passed to MXVG.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-AUG-1991 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     transformation from inertial position to bodyfixed
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added. The new checks
C        are intended to prevent arithmetic operations from
C        being performed with uninitialized or invalid data.
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
      DOUBLE PRECISION      TSIPM   ( 6, 6 )

      INTEGER               I
      INTEGER               J

C
C     Standard SPICE Error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TIPBOD' )
      END IF

C
C     Get 6x6 state transformation from TISBOD. If succeeded, pull out
C     left-top 3x3 matrix.
C
      CALL TISBOD ( REF, BODY, ET, TSIPM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TIPBOD' )
         RETURN
      END IF

      DO  I = 1, 3
         DO   J  = 1, 3
            TIPM  ( I, J ) = TSIPM  ( I, J )
         END DO
      END DO

      CALL CHKOUT ( 'TIPBOD' )
      RETURN
      END

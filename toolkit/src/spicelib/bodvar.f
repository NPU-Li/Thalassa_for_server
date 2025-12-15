C$Procedure BODVAR ( Return values from the kernel pool )

      SUBROUTINE BODVAR ( BODY, ITEM, DIM, VALUES )

C$ Abstract
C
C     Deprecated: This routine has been superseded by BODVCD and
C     BODVRD. This routine is supported for purposes of backward
C     compatibility only.
C
C     Return the values of some item for any body in the
C     kernel pool.
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
C     PCK
C     SPK
C
C$ Keywords
C
C     CONSTANTS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               BODY
      CHARACTER*(*)         ITEM
      INTEGER               DIM
      DOUBLE PRECISION      VALUES  ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of body.
C     ITEM       I   Item for which values are desired.
C     DIM        O   Number of values returned.
C     VALUES     O   Values.
C
C$ Detailed_Input
C
C     BODY     is the ID code of the body for which ITEM is
C              requested.
C
C     ITEM     is the item to be returned. Together, the body and
C              item name combine to form a variable name, e.g.,
C
C                 'BODY599_RADII'
C                 'BODY401_POLE_RA'
C
C$ Detailed_Output
C
C     DIM      is the number of values associated with the variable.
C
C     VALUES   are the values associated with the variable.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the requested item is not found, the error
C         SPICE(KERNELVARNOTFOUND) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Retrieve the Earth's radii values from the kernel pool
C
C        Use the PCK kernel below to load the required triaxial
C        ellipsoidal shape model for the Earth.
C
C           pck00008.tpc
C
C
C        Example code begins here.
C
C
C              PROGRAM BODVAR_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              LOGICAL               BODFND
C
C        C
C        C     Local constants.
C        C
C              INTEGER               BODYID
C              PARAMETER           ( BODYID = 399 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      RADII  (3)
C
C              INTEGER               DIM
C
C              LOGICAL               FOUND
C
C        C
C        C     Load a PCK file.
C        C
C              CALL FURNSH ( 'pck00008.tpc' )
C
C        C
C        C     Test if Earth's radii values exist in the
C        C     kernel pool.
C        C
C        C     The procedure searches for the kernel variable
C        C     BODY399_RADII.
C        C
C              FOUND = BODFND( BODYID, 'RADII' )
C
C        C
C        C     If found, retrieve the values.
C        C
C              IF ( FOUND ) THEN
C
C                 CALL BODVAR ( BODYID, 'RADII', DIM, RADII )
C
C                 WRITE(*,'(I3,A,3F11.3)') BODYID, ' RADII:', RADII(1),
C             .                            RADII(2), RADII(3)
C
C              ELSE
C
C                 WRITE(*,*) 'No RADII data found for object ', BODYID
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
C        399 RADII:   6378.140   6378.140   6356.750
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.6, 17-JUN-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added
C        complete code example. Updated input argument BODY
C        detailed description. Added SPK to the list of required
C        readings.
C
C-    SPICELIB Version 1.0.5, 18-MAY-2010 (BVS)
C
C        Index lines now state that this routine is deprecated.
C
C-    SPICELIB Version 1.0.4, 27-OCT-2005 (NJB)
C
C        Routine is now deprecated.
C
C-    SPICELIB Version 1.0.3, 08-JAN-2004 (EDW)
C
C        Trivial typo corrected.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
C
C        Detailed Input section of the header was updated. The
C        description for the variable BODY was incorrect.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     DEPRECATED fetch constants for a body from the kernel pool
C     DEPRECATED physical constants for a body
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      CHARACTER*16          CODE
      CHARACTER*32          VARNAM
      LOGICAL               FOUND



C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
        RETURN
      ELSE
        CALL CHKIN ( 'BODVAR' )
      END IF

C
C     Construct the variable name from BODY and ITEM.
C
      VARNAM = 'BODY'

      CALL INTSTR ( BODY, CODE )
      CALL SUFFIX ( CODE, 0, VARNAM )
      CALL SUFFIX ( '_',  0, VARNAM )
      CALL SUFFIX ( ITEM, 0, VARNAM )

C
C     Grab the items. Complain if they aren't there.
C
      CALL RTPOOL ( VARNAM, DIM, VALUES, FOUND )

      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'The variable # could not be found in the ' //
     .                 'kernel pool.'                             )
         CALL ERRCH  ( '#', VARNAM                                )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                 )
      END IF

      CALL CHKOUT ( 'BODVAR' )
      RETURN
      END

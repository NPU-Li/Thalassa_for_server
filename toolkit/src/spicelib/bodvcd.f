C$Procedure BODVCD ( Return d.p. values from the kernel pool )

      SUBROUTINE BODVCD ( BODYID, ITEM, MAXN, DIM, VALUES )

C$ Abstract
C
C     Fetch from the kernel pool the double precision values
C     of an item associated with a body, where the body is
C     specified by an integer ID code.
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
C     CONSTANTS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               BODYID
      CHARACTER*(*)         ITEM
      INTEGER               MAXN
      INTEGER               DIM
      DOUBLE PRECISION      VALUES  ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODYID     I   Body ID code.
C     ITEM       I   Item for which values are desired. ('RADII',
C                    'NUT_PREC_ANGLES', etc. )
C     MAXN       I   Maximum number of values that may be returned.
C     DIM        O   Number of values returned.
C     VALUES     O   Values.
C
C$ Detailed_Input
C
C     BODYID   is the NAIF integer ID code for a body of interest.
C              For example, if the body is the earth, the code is
C              399.
C
C     ITEM     is the item to be returned. Together, the NAIF ID
C              code of the body and the item name combine to form a
C              kernel variable name, e.g.,
C
C                    'BODY599_RADII'
C                    'BODY401_POLE_RA'
C
C              The values associated with the kernel variable having
C              the name constructed as shown are sought. Below
C              we'll take the shortcut of calling this kernel variable
C              the "requested kernel variable."
C
C              Note that ITEM *is* case-sensitive. This attribute
C              is inherited from the case-sensitivity of kernel
C              variable names.
C
C     MAXN     is the maximum number of values that may be returned.
C              The output array VALUES must be declared with size at
C              least MAXN. It's an error to supply an output array
C              that is too small to hold all of the values associated
C              with the requested kernel variable.
C
C$ Detailed_Output
C
C     DIM      is the number of values returned; this is always the
C              number of values associated with the requested kernel
C              variable unless an error has been signaled.
C
C     VALUES   is the array of values associated with the requested
C              kernel variable. If VALUES is too small to hold all
C              of the values associated with the kernel variable, the
C              returned values of DIM and VALUES are undefined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the requested kernel variable is not found in the kernel
C         pool, the error SPICE(KERNELVARNOTFOUND) is signaled.
C
C     2)  If the requested kernel variable is found but the associated
C         values aren't numeric, the error SPICE(TYPEMISMATCH) is
C         signaled.
C
C     3)  If the dimension of VALUES indicated by MAXN is too small to
C         contain the requested values, the error SPICE(ARRAYTOOSMALL)
C         is signaled. The output array VALUES must be declared with
C         sufficient size to contain all of the values associated with
C         the requested kernel variable.
C
C     4)  If the input dimension MAXN indicates there is more room
C         in VALUES than there really is---for example, if MAXN is
C         10 but values is declared with dimension 5---and the dimension
C         of the requested kernel variable is larger than the actual
C         dimension of VALUES, then this routine may overwrite
C         memory. The results are unpredictable.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine simplifies looking up PCK kernel variables by
C     constructing names of requested kernel variables and by
C     performing error checking.
C
C     This routine is intended for use in cases where the maximum number
C     of values that may be returned is known at compile time. The
C     caller fetches all of the values associated with the specified
C     kernel variable via a single call to this routine. If the number
C     of values to be fetched cannot be known until run time, the
C     lower-level routine GDPOOL should be used instead. GDPOOL
C     supports fetching arbitrary amounts of data in multiple "chunks."
C
C     This routine is intended for use in cases where the requested
C     kernel variable is expected to be present in the kernel pool. If
C     the variable is not found or has the wrong data type, this
C     routine signals an error. In cases where it is appropriate to
C     indicate absence of an expected kernel variable by returning a
C     boolean "found flag" with the value .FALSE., again the routine
C     GDPOOL should be used.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Retrieve the radii of the Earth from the kernel pool, using
C        both 'RADII' and 'radii' as the item name to return. Since
C        the ITEM variable possesses case sensitivity, the later case
C        should fail. Trap the error and print it to the output.
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
C              PROGRAM BODVCD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NVALS
C              PARAMETER           ( NVALS = 3 )
C
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      VALUES (NVALS)
C
C              INTEGER               DIM
C
C        C
C        C     Load a PCK.
C        C
C              CALL FURNSH ( 'pck00008.tpc' )
C
C        C
C        C     When the kernel variable
C        C
C        C        BODY399_RADII
C        C
C        C     is present in the kernel pool---normally because a PCK
C        C     defining this variable has been loaded (as is the case
C        C     here)---the call
C        C
C              CALL BODVCD ( 399, 'RADII', 3, DIM, VALUES )
C
C        C
C        C     returns the dimension and values associated with the
C        C     variable 'BODY399_RADII'
C        C
C              WRITE(*,'(A,3F10.3)') '399 RADII: ', VALUES
C
C        C
C        C     The ITEM variable possesses case sensitivity. This
C        C     call should cause an error.
C        C
C              CALL BODVRD ( 'EARTH', 'radii', 3, DIM, VALUES )
C              WRITE(*,'(A,3F10.3)') '399 radii: ', VALUES
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        399 RADII:   6378.140  6378.140  6356.750
C
C        ============================================================***
C
C        Toolkit version: N0066
C
C        SPICE(KERNELVARNOTFOUND) -- The Variable Was not Found in th***
C        Pool.
C
C        The variable BODY399_radii could not be found in the kernel ***
C
C        A traceback follows.  The name of the highest level module i***
C        BODVRD
C
C        Oh, by the way:  The SPICELIB error handling actions are USER-
C        TAILORABLE.  You can choose whether the Toolkit aborts or co***
C        when errors occur, which error messages to output, and where***
C        the output.  Please read the ERROR "Required Reading" file, ***
C        the routines ERRACT, ERRDEV, and ERRPRT.
C
C        ============================================================***
C
C
C        Warning: incomplete output. 8 lines extended past the right
C        margin of the header and have been truncated. These lines are
C        marked by "***" at the end of each line.
C
C
C        Note that, usually, the last call will cause a
C        SPICE(KERNELVARNOTFOUND) error to be signaled, because this
C        call will attempt to look up the values associated with a
C        kernel variable of the name
C
C           'BODY399_radii'
C
C        Since kernel variable names are case sensitive, this
C        name is not considered to match the name
C
C           'BODY399_RADII'
C
C        which normally would be present after a text PCK
C        containing data for all planets and satellites has
C        been loaded.
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 07-SEP-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete example code based on the existing fragments.
C
C        Removed note about GDPOOL being entry point of POOL from
C        $Particulars section.
C
C-    SPICELIB Version 1.0.0, 24-OCT-2004 (NJB) (BVS) (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     fetch constants for a body from the kernel pool
C     physical constants for a body
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               CDELEN
      PARAMETER           ( CDELEN = 16 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Local variables
C
      CHARACTER*(CDELEN)    CODE
      CHARACTER*1           TYPE
      CHARACTER*(NAMLEN)    VARNAM

      LOGICAL               FOUND



C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
        RETURN
      ELSE
        CALL CHKIN ( 'BODVCD' )
      END IF

C
C     Construct the variable name from BODY and ITEM.
C
      VARNAM = 'BODY'

      CALL INTSTR ( BODYID,    CODE   )
      CALL SUFFIX ( CODE,   0, VARNAM )
      CALL SUFFIX ( '_',    0, VARNAM )
      CALL SUFFIX ( ITEM,   0, VARNAM )

C
C     Make sure the item is present in the kernel pool.
C
      CALL DTPOOL ( VARNAM, FOUND, DIM, TYPE )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The variable # could not be found in the ' //
     .                 'kernel pool.'                              )
         CALL ERRCH  ( '#', VARNAM                                 )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                  )
         CALL CHKOUT ( 'BODVCD'                                    )
         RETURN

      END IF

C
C     Make sure the item's data type is numeric.
C
      IF ( TYPE .NE. 'N' ) THEN

         CALL SETMSG ( 'The data associated with variable # ' //
     .                 'are not of numeric type.'             )
         CALL ERRCH  ( '#', VARNAM                            )
         CALL SIGERR ( 'SPICE(TYPEMISMATCH)'                  )
         CALL CHKOUT ( 'BODVCD'                               )
         RETURN

      END IF

C
C     Make sure there's enough room in the array VALUES to hold
C     the requested data.
C
      IF ( MAXN .LT. DIM ) THEN

         CALL SETMSG ( 'The data array associated with variable # ' //
     .                 'has dimension #, which is larger than the ' //
     .                 'available space # in the output array.'    )
         CALL ERRCH  ( '#', VARNAM                                 )
         CALL ERRINT ( '#', DIM                                    )
         CALL ERRINT ( '#', MAXN                                   )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                      )
         CALL CHKOUT ( 'BODVCD'                                    )
         RETURN

      END IF

C
C     Grab the values.  We know at this point they're present in
C     the kernel pool, so we don't check the FOUND flag.
C
      CALL GDPOOL ( VARNAM, 1, MAXN, DIM, VALUES, FOUND )

      CALL CHKOUT ( 'BODVCD' )
      RETURN
      END

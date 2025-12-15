C$Procedure SPK14B ( SPK type 14: Begin a segment.)

      SUBROUTINE SPK14B ( HANDLE, SEGID,
     .                    BODY,   CENTER, FRAME,
     .                    FIRST,  LAST,   CHBDEG  )

C$ Abstract
C
C     Begin a type 14 SPK segment in the SPK file associated with
C     HANDLE.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'sgparam.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               CHBDEG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an SPK file open for writing.
C     SEGID      I   The string to use for segment identifier.
C     BODY       I   The NAIF ID code for the body of the segment.
C     CENTER     I   The center of motion for BODY.
C     FRAME      I   The reference frame for this segment.
C     FIRST      I   The first epoch for which the segment is valid.
C     LAST       I   The last epoch for which the segment is valid.
C     CHBDEG     I   The degree of the Chebyshev Polynomial used.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing.
C
C     SEGID    is the segment identifier. An SPK segment identifier
C              may contain up to 40 printing ASCII characters.
C
C     BODY     is the SPICE ID for the body whose states are
C              to be recorded in an SPK file.
C
C     CENTER   is the SPICE ID for the center of motion associated
C              with BODY.
C
C     FRAME    is the reference frame that states are referenced to,
C              for example 'J2000'.
C
C     FIRST    is the starting epoch, in seconds past J2000, for
C              the ephemeris data to be placed into the segment.
C
C     LAST     is the ending epoch, in seconds past J2000, for
C              the ephemeris data to be placed into the segment.
C
C     CHBDEG   is the degree of the Chebyshev Polynomials used to
C              represent the ephemeris information stored in the
C              segment.
C
C$ Detailed_Output
C
C     None. The input data is used to create the segment summary for the
C     segment being started in the SPK file associated with HANDLE.
C
C     See the $Particulars section for details about the structure of a
C     type 14 SPK segment.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1)  If the degree of the Chebyshev Polynomial to be used for this
C         segment is negative, the error SPICE(INVALIDARGUMENT) is
C         signaled.
C
C     2)  If there are issues in the structure or content of the inputs
C         other than the degree of the Chebyshev Polynomial, an error is
C         signaled by a routine in the call tree of this routine.
C
C     3)  If a file access error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     See HANDLE in the $Detailed_Input section.
C
C$ Particulars
C
C     This routine begins writing a type 14 SPK segment to the open SPK
C     file that is associated with HANDLE. The file must have been
C     opened with write access.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 14 SPK segments. These routines are:
C
C        SPK14B: Begin a type 14 SPK segment. This routine must be
C                called before any data may be added to a type 14
C                segment.
C
C        SPK14A: Add data to a type 14 SPK segment. This routine may be
C                called any number of times after a call to SPK14B to
C                add type 14 records to the SPK segment that was
C                started.
C
C        SPK14E: End a type 14 SPK segment. This routine is called to
C                make the type 14 segment a permanent addition to the
C                SPK file. Once this routine is called, no further type
C                14 records may be added to the segment. A new segment
C                must be started.
C
C     A type 14 SPK segment consists of coefficient sets for fixed order
C     Chebyshev polynomials over consecutive time intervals, where the
C     time intervals need not all be of the same length. The Chebyshev
C     polynomials represent the position, X, Y, and Z coordinates, and
C     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to
C     CENTER.
C
C     The ephemeris data supplied to the type 14 SPK writer is packed
C     into an array as a sequence of records,
C
C        -----------------------------------------------------
C        | Record 1 | Record 2 | ... | Record N-1 | Record N |
C        -----------------------------------------------------
C
C     with each record has the following format.
C
C           ------------------------------------------------
C           |  The midpoint of the approximation interval  |
C           ------------------------------------------------
C           |  The radius of the approximation interval    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z velocity    |
C           ------------------------------------------------
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to create an SPK type 14 kernel
C        containing only one segment, given a set of Chebyshev
C        coefficients and their associated epochs.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPK14B_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 42 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK14
C              PARAMETER           ( SPK14  = 'spk14b_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
C
C              INTEGER               CHBDEG
C              PARAMETER           ( CHBDEG = 2  )
C
C              INTEGER               NRECS
C              PARAMETER           ( NRECS  = 4  )
C
C              INTEGER               RECSIZ
C              PARAMETER           ( RECSIZ = 2 + 6*(CHBDEG+1) )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      EPOCHS ( NRECS + 1 )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Define the epochs and coefficients.
C        C
C              DATA                  EPOCHS /
C             .                100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /
C
C              DATA                  RECRDS /
C             .     150.0D0, 50.0D0, 1.0101D0, 1.0102D0, 1.0103D0,
C             .                      1.0201D0, 1.0202D0, 1.0203D0,
C             .                      1.0301D0, 1.0302D0, 1.0303D0,
C             .                      1.0401D0, 1.0402D0, 1.0403D0,
C             .                      1.0501D0, 1.0502D0, 1.0503D0,
C             .                      1.0601D0, 1.0602D0, 1.0603D0,
C             .     250.0D0, 50.0D0, 2.0101D0, 2.0102D0, 2.0103D0,
C             .                      2.0201D0, 2.0202D0, 2.0203D0,
C             .                      2.0301D0, 2.0302D0, 2.0303D0,
C             .                      2.0401D0, 2.0402D0, 2.0403D0,
C             .                      2.0501D0, 2.0502D0, 2.0503D0,
C             .                      2.0601D0, 2.0602D0, 2.0603D0,
C             .     350.0D0, 50.0D0, 3.0101D0, 3.0102D0, 3.0103D0,
C             .                      3.0201D0, 3.0202D0, 3.0203D0,
C             .                      3.0301D0, 3.0302D0, 3.0303D0,
C             .                      3.0401D0, 3.0402D0, 3.0403D0,
C             .                      3.0501D0, 3.0502D0, 3.0503D0,
C             .                      3.0601D0, 3.0602D0, 3.0603D0,
C             .     450.0D0, 50.0D0, 4.0101D0, 4.0102D0, 4.0103D0,
C             .                      4.0201D0, 4.0202D0, 4.0203D0,
C             .                      4.0301D0, 4.0302D0, 4.0303D0,
C             .                      4.0401D0, 4.0402D0, 4.0403D0,
C             .                      4.0501D0, 4.0502D0, 4.0503D0,
C             .                      4.0601D0, 4.0602D0, 4.0603D0 /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment.
C        C
C              FIRST = EPOCHS(1)
C              LAST  = EPOCHS(NRECS + 1)
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
C              IFNAME = 'Type 14 SPK internal file name.'
C              SEGID  = 'SPK type 14 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK14, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Begin the segment.
C        C
C              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REF,
C             .              FIRST,  LAST,  CHBDEG            )
C
C        C
C        C     Add the data to the segment all at once.
C        C
C              CALL SPK14A ( HANDLE, NRECS, RECRDS, EPOCHS )
C
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the SPK file.
C        C
C              CALL SPK14E ( HANDLE )
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
C        screen. After run completion, a new SPK type 14 exists in
C        the output directory.
C
C$ Restrictions
C
C     1)  The SPK file must be open with write access.
C
C     2)  Only one segment may be written to a particular SPK file at a
C         time. All of the data for the segment must be written and the
C         segment must be ended before another segment may be started in
C         the file.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 27-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added
C        complete example code from existing fragment.
C
C        Removed references to other routines from $Abstract section
C        (present in $Particulars).
C
C-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
C
C        Removed comments from the $Declarations section.
C
C-    SPICELIB Version 1.0.1, 30-OCT-2006 (BVS)
C
C        Deleted "inertial" from the FRAME description in the $Brief_I/O
C        section of the header.
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     begin writing a type_14 SPK segment
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Parameters
C
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
C
C     Length of an SPK descriptor.
C
      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + (NI+1)/2 )
C
C     Length of a state.
C
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )
C
C     The type of this segment
C
      INTEGER               TYPE
      PARAMETER           ( TYPE = 14 )
C
C     The number of constants:
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 1 )
C
C     Local variables
C
      DOUBLE PRECISION      DCOEFF
      DOUBLE PRECISION      DESCR(NDESCR)

      INTEGER               NCOEFF
      INTEGER               PKTSIZ
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPK14B' )
      END IF
C
C     First, check the degree of the polynomial to be sure that it is
C     not negative.
C
      IF ( CHBDEG .LT. 0 ) THEN

         CALL SETMSG ( 'The degree of the Chebyshev Polynomial'  //
     .                 ' was negative, #. The degree of the'     //
     .                 ' polynomial must be greater than or'     //
     .                 ' equal to zero.'                          )
         CALL ERRINT ( '#', CHBDEG                                )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'SPK14B'                                   )
         RETURN

      END IF
C
C     Create a descriptor for the segment we are about to write.
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, TYPE, FIRST, LAST, DESCR )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'SPK14B' )
         RETURN

      END IF
C
C     We've got a valid descriptor, so compute a few things and begin
C     the segment.
C
      NCOEFF = CHBDEG + 1
      PKTSIZ = NSTATE * NCOEFF + 2
      DCOEFF = DBLE ( NCOEFF )
C
C     For this data type, we want to use an explicit reference value
C     index where the reference epochs are in increasing order. We also
C     want to have as the index for a particular request epoch the index
C     of the greatest reference epoch less than or equal to the request
C     epoch. These characteristics are prescribed by the mnemonic EXPLE.
C     See the include file 'sgparam.inc' for more details.
C
      CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST,
     .                      DCOEFF, PKTSIZ, EXPLE   )

C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.
C

      CALL CHKOUT ( 'SPK14B' )
      RETURN

      END

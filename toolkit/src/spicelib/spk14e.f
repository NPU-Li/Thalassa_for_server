C$Procedure SPK14E ( SPK type 14: End a segment. )

      SUBROUTINE SPK14E ( HANDLE )

C$ Abstract
C
C     End the type 14 SPK segment currently being written to the SPK
C     file associated with HANDLE.
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
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an SPK file open for writing.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing, and to which a type 14 segment is
C              being written.
C
C$ Detailed_Output
C
C     None. The type 14 segment in the SPK file associated with HANDLE
C     will be ended, making the addition of the data to the file
C     permanent.
C
C     See the $Particulars section for details about the structure of a
C     type 14 SPK segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If there are no segments currently being written to the file
C         associated with HANDLE, an error is signaled by a routine in
C         the call tree of this routine.
C
C     2)  If any file access error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     See the argument HANDLE.
C
C$ Particulars
C
C     This routine ends a type 14 SPK segment which is being written to
C     the SPK file associated with HANDLE. Ending the SPK segment is a
C     necessary step in the process of making the data a permanent part
C     of the SPK file.
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
C     into an array as a sequence of logical records,
C
C        -----------------------------------------------------
C        | Record 1 | Record 2 | ... | Record N-1 | Record N |
C        -----------------------------------------------------
C
C     with each record has the following format.
C
C           ------------------------------------------------
C           |  the midpoint of the approximation interval  |
C           ------------------------------------------------
C           |   the radius of the approximation interval   |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z coordinate  |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the X velocity   |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the Y velocity   |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the Z velocity   |
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
C              PROGRAM SPK14E_EX1
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
C              PARAMETER           ( SPK14  = 'spk14e_ex1.bsp' )
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
C     1)  The type 14 SPK segment being closed must have been started by
C         the routine SPK14B, the routine which begins a type 14 SPK
C         segment.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 27-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added
C        complete example code from existing fragment.
C
C        Updated $Exceptions section.
C
C        Re-ordered header sections. Removed references to other
C        routines from $Abstract section (present in $Particulars).
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     end a type_14 SPK segment
C
C-&


C
C     Spicelib functions
C
      LOGICAL               RETURN

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPK14E' )
      END IF

C
C     This is simple, just call the routine which ends a generic
C     segment.
C
      CALL SGWES ( HANDLE )

C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.

      CALL CHKOUT ( 'SPK14E' )
      RETURN

      END

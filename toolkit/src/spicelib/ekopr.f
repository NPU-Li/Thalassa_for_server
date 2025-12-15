C$Procedure EKOPR ( EK, open file for reading )

      SUBROUTINE EKOPR ( FNAME, HANDLE )

C$ Abstract
C
C     Open an existing E-kernel file for reading.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FNAME
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of EK file.
C     HANDLE     O   Handle attached to EK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of an existing E-kernel file to be
C              opened for read access.
C
C$ Detailed_Output
C
C     HANDLE   is the EK file handle of the file designated by
C              FNAME. This handle is used to identify the file
C              to other EK routines.
C
C$ Parameters
C
C     FTSIZE   is the maximum number of DAS files that a user can
C              have open simultaneously. This includes any files used
C              by the DAS system.
C
C              See the include file das.inc for the actual value of
C              this parameter.
C
C$ Exceptions
C
C     1)  If the indicated file cannot be opened, an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  If the indicated file has the wrong architecture version, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     3)  If an I/O error occurs while reading the indicated file, the
C         error is signaled by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     See the EK Required Reading ek.req for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine should be used to open an EK file for read access.
C     EKs opened for read access may not be modified.
C
C     Opening an EK file with this routine makes the EK accessible to
C     the SPICELIB EK readers
C
C        EKRCEC
C        EKRCED
C        EKRCEI
C
C     all of which expect an EK file handle as an input argument. These
C     readers allow a caller to read individual EK column entries.
C
C     To make an EK available to the EK query system, the file must be
C     loaded via EKLEF, rather than by this routine. See the EK
C     Required Reading for further information.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Open an EK for read access and find the number of segments in
C        it.
C
C        Use the EK kernel below as test input file for loading the
C        experiment database. This kernel contains the Deep
C        Impact spacecraft sequence data based on the integrated
C        Predicted Events File covering the whole primary mission,
C        split into two segments.
C
C           dif_seq_050112_050729.bes
C
C
C        Example code begins here.
C
C
C              PROGRAM EKOPR_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               EKNSEG
C
C        C
C        C     Local variables.
C        C
C              INTEGER               HANDLE
C              INTEGER               NSEG
C
C        C
C        C     Open the EK file, returning the file handle
C        C     associated with the open file to the variable named
C        C     HANDLE.
C        C
C              CALL EKOPR ( 'dif_seq_050112_050729.bes', HANDLE )
C
C
C        C
C        C     Return the number of segments in the EK.
C        C
C              NSEG = EKNSEG( HANDLE )
C              WRITE(*,'(A,I3)') 'Number of segments =', NSEG
C
C        C
C        C     Close the file.
C        C
C              CALL EKCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Number of segments =  2
C
C
C$ Restrictions
C
C     1)  No more than FTSIZE DAS files may be opened simultaneously.
C         See the include file das.inc for the value of FTSIZE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example and updated $Parameters section.
C
C        Corrected $Exceptions #1: this routine does not delete the
C        input file if the file cannot be opened.
C
C-    SPICELIB Version 1.0.0, 26-AUG-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     open EK for reading
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPR' )
      END IF

C
C     Open the file as a DAS file.
C
      CALL DASOPR ( FNAME, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPR' )
         RETURN
      END IF

C
C     Nothing doing unless the architecture is correct.  This file
C     should be a paged DAS EK.
C
      CALL ZZEKPGCH ( HANDLE, 'READ' )


      CALL CHKOUT ( 'EKOPR' )
      RETURN
      END

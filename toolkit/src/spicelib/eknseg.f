C$Procedure EKNSEG ( EK, number of segments in file )

      INTEGER FUNCTION EKNSEG ( HANDLE )

C$ Abstract
C
C     Return the number of segments in a specified EK.
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
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'ekfilpar.inc'

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C
C     The function returns the number of segments in the specified
C     E-kernel.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of an EK file opened for read access.
C
C$ Detailed_Output
C
C     The function returns the number of segments in the EK identified
C     by HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, an error is signaled by a routine in the
C         call tree of this routine. EKNSEG will return the value zero.
C
C     2)  If an I/O error occurs while trying to read the EK, the error
C         is signaled by a routine in the call tree of this routine.
C         EKNSEG will return the value zero.
C
C$ Files
C
C     See the description of HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine is used to support the function of summarizing an
C     EK file. Given the number of segments in the file, a program
C     can use EKSSUM in a loop to summarize each of them.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the number of segments on an EK.
C
C        Use the EK kernel below as test input file for loading the
C        experiment database. This kernel contains the Deep
C        Impact spacecraft sequence data based on the integrated
C        Predicted Events File covering the whole primary mission.
C
C           dif_seq_050112_050729.bes
C
C
C        Example code begins here.
C
C
C              PROGRAM EKNSEG_EX1
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
C              WRITE(*,*) 'Number of segments = ', NSEG
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
C         Number of segments =            2
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     return number of segments in an E-kernel
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRSZ

C
C     Local variables
C
      INTEGER               BASE
      INTEGER               TREE

C
C     Set a default value for EKNSEG.
C
      EKNSEG  =  0

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKNSEG' )
      END IF

C
C     Make sure this is a paged DAS EK.
C
      CALL ZZEKPGCH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKNSEG' )
         RETURN
      END IF

C
C     Obtain the base address of the first integer page.
C
      BASE = ZZEKTRBS ( 1 )

C
C     Look up the head node of the segment tree.
C
      CALL DASRDI ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, TREE )

C
C     Get the entry count for the segment tree.
C
      EKNSEG  =  ZZEKTRSZ ( HANDLE, TREE )

      CALL CHKOUT ( 'EKNSEG' )
      RETURN
      END

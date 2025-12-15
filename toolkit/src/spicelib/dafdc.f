C$Procedure DAFDC ( DAF delete comments )

      SUBROUTINE DAFDC ( HANDLE )

C$ Abstract
C
C     Delete the entire comment area of a previously opened binary
C     DAF attached to HANDLE.
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
C     DAF
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a binary DAF opened for writing.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a binary DAF that is to have its entire
C              comment area deleted. The DAF must have been opened
C              with write access.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the binary DAF attached to HANDLE is not open with write
C         access, an error is signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     See argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     A binary DAF contains an area which is reserved for storing
C     annotations or descriptive textual information about the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAF is a line
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored, so that the appearance of the of
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will delete the entire comment area from the binary
C     DAF attached to HANDLE. The size of the binary DAF will remain
C     unchanged. The space that was used by the comment records
C     is reclaimed.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Delete the entire comment area of a DAF file. Note that this
C        action should only be performed if fresh new comments are to
C        be placed within the DAF file.
C
C        Use the SPK kernel below as input DAF file for the program.
C
C           earthstns_itrf93_201023.bsp
C
C
C        Example code begins here.
C
C
C              PROGRAM DAFDC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               RTRIM
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         KERNEL
C              PARAMETER           ( KERNEL =
C             .                         'earthstns_itrf93_201023.bsp' )
C
C              INTEGER               BUFFSZ
C              PARAMETER           ( BUFFSZ = 10 )
C
C              INTEGER               LINLEN
C              PARAMETER           ( LINLEN = 1000 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(LINLEN)    BUFFER ( BUFFSZ )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               N
C
C              LOGICAL               DONE
C
C        C
C        C     Open a DAF for write. Return a HANDLE referring to the
C        C     file.
C        C
C              CALL DAFOPW ( KERNEL, HANDLE )
C
C        C
C        C     Print the first 10 lines of comments from the DAF file.
C        C
C              WRITE(*,'(A)') 'Comment area of input DAF file '
C             .            // '(max. 10 lines): '
C              WRITE(*,'(A)') '---------------------------------------'
C             .            // '-----------------------'
C
C              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
C
C              DO I = 1, N
C
C                 WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
C
C              END DO
C
C              WRITE(*,'(A)') '---------------------------------------'
C             .            // '-----------------------'
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Deleting entire comment area...'
C
C        C
C        C     Delete all the comments from the DAF file.
C        C
C              CALL DAFDC ( HANDLE )
C
C        C
C        C     Close the DAF file and re-open it for read
C        C     access to work around the DAFEC restriction
C        C     on comments not to be modified while they are
C        C     being extracted.
C        C
C              CALL DAFCLS( HANDLE  )
C
C              CALL DAFOPR( KERNEL, HANDLE  )
C
C        C
C        C     Check if the comments have indeed been deleted.
C        C
C              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
C
C              IF ( DONE .AND. N .EQ. 0 ) THEN
C
C                 WRITE(*,*) ' '
C                 WRITE(*,*) '   Successful operation.'
C
C              ELSE
C
C                 WRITE(*,*) ' '
C                 WRITE(*,*) '   Operation failed.'
C
C              END IF
C
C        C
C        C     Safely close the DAF.
C        C
C              CALL DAFCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Comment area of input DAF file (max. 10 lines):
C        --------------------------------------------------------------
C
C            SPK for DSN Station Locations
C            ========================================================***
C
C            Original file name:                   earthstns_itrf93_2***
C            Creation date:                        2020 October 28 12:30
C            Created by:                           Nat Bachman  (NAIF***
C
C
C            Introduction
C        --------------------------------------------------------------
C
C         Deleting entire comment area...
C
C            Successful operation.
C
C
C        Warning: incomplete output. 3 lines extended past the right
C        margin of the header and have been truncated. These lines are
C        marked by "***" at the end of each line.
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
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.0, 23-SEP-1994 (KRG)
C
C-&


C$ Index_Entries
C
C     delete DAF comment area
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Length of a DAF file internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Local variables
C
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               NCOMR
      INTEGER               ND
      INTEGER               NI

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFDC' )
      END IF
C
C     Verify that the DAF attached to HANDLE was opened with write
C     access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFDC' )
         RETURN

      END IF
C
C     Read the file record to obtain the current number of comment
C     records in the DAF attached to HANDLE. We will also get back some
C     extra stuff that we do not use.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      NCOMR = FWARD - 2

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFDC' )
         RETURN

      END IF
C
C     Now we will attempt to remove the comment records, if there are
C     any, otherwise we do nothing.
C
      IF ( NCOMR .GT. 0 ) THEN
C
C        We have some comment records, so remove them.
C
         CALL DAFRRR ( HANDLE, NCOMR )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'DAFDC' )
            RETURN

         END IF

      END IF
C
C     We're done now, so goodbye.
C
      CALL CHKOUT ( 'DAFDC' )
      RETURN
      END

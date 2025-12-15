C$Procedure DASCLS ( DAS, close file )

      SUBROUTINE DASCLS ( HANDLE )

C$ Abstract
C
C     Close a DAS file.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'das.inc'

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an open DAS file.
C     FTSIZE     P   Maximum number of simultaneously open DAS files.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an open DAS file.
C
C$ Detailed_Output
C
C     None.
C
C     See $Particulars for a description of the effect of this routine.
C
C$ Parameters
C
C     All parameters described here are declared in the SPICELIB
C     include file das.inc. See that file for parameter values.
C
C     FTSIZE   is the maximum number of DAS files that can be
C              open at any one time.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If HANDLE is not the handle of an open DAS file, no error
C         is signaled.
C
C$ Files
C
C     See the description of input argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine provides the primary recommended method of closing an
C     open DAS file. It is also possible to close a DAS file without
C     segregating it by calling DASWBR and DASLLC. Closing a DAS file by
C     any other means may cause the DAS mechanism for keeping track of
C     which files are open to fail. Closing a DAS file that has been
C     opened for writing by any other means may result in the production
C     of something other than a DAS file.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Open a new DAS file called TEST.DAS, add 100 d.p. numbers
C        to it, and then close the file.
C
C        Example code begins here.
C
C
C              PROGRAM DASCLS_EX1
C              IMPLICIT NONE
C
C              INTEGER               NMAX
C              PARAMETER           ( NMAX   = 100 )
C
C              CHARACTER*(15)        FNAME
C              CHARACTER*(5)         FTYPE
C              CHARACTER*(15)        IFNAME
C
C              DOUBLE PRECISION      DDATA  ( NMAX )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               N
C              INTEGER               NCOMCH
C
C        C
C        C     We'll give the file the same internal file name
C        C     as the file's actual name.  We don't require any
C        C     comment records.
C        C
C              FNAME  = 'dascls_ex1.das'
C              FTYPE  = 'TEST'
C              IFNAME = FNAME
C              NCOMCH = 0
C
C              WRITE(*,*) 'Opening the DAS file for writing...'
C              CALL DASONW ( FNAME, FTYPE,  FNAME, NCOMCH, HANDLE )
C
C              DO I = 1, NMAX
C                 DDATA(I)  =  DBLE(I)
C              END DO
C
C              N = NMAX
C
C              WRITE(*,*) 'Adding the double precision numbers...'
C              CALL DASADD ( HANDLE, N, DDATA )
C
C              WRITE(*,*) 'Closing the DAS file...'
C              CALL DASCLS ( HANDLE )
C
C              WRITE(*,*) 'All ok.'
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Opening the DAS file for writing...
C         Adding the double precision numbers...
C         Closing the DAS file...
C         All ok.
C
C
C        Note that after run completion, a new DAS file exists in
C        the output directory.
C
C
C     2) Dump several parameters from the first DLA segment of a DSK
C        file. Note that DSK files are based on DAS. The segment is
C        assumed to be of type 2.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASCLS_EX2
C              IMPLICIT NONE
C
C              INCLUDE 'dla.inc'
C              INCLUDE 'dskdsc.inc'
C              INCLUDE 'dsk02.inc'
C
C        C
C        C     Local parameters
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    DSK
C              CHARACTER*(LNSIZE)    OUTLIN
C
C              DOUBLE PRECISION      VOXORI ( 3 )
C              DOUBLE PRECISION      VOXSIZ
C              DOUBLE PRECISION      VTXBDS ( 2, 3 )
C
C              INTEGER               CGSCAL
C              INTEGER               DLADSC ( DLADSZ )
C              INTEGER               HANDLE
C              INTEGER               NP
C              INTEGER               NV
C              INTEGER               NVXTOT
C              INTEGER               VGREXT ( 3 )
C              INTEGER               VOXNPL
C              INTEGER               VOXNPT
C              INTEGER               VTXNPL
C
C              LOGICAL               FOUND
C
C
C        C
C        C     Prompt for the name of the DSK to read.
C        C
C              CALL PROMPT ( 'Enter DSK name > ', DSK )
C        C
C        C     Open the DSK file for read access.
C        C     We use the DAS-level interface for
C        C     this function.
C        C
C              CALL DASOPR ( DSK, HANDLE )
C
C        C
C        C     Begin a forward search through the
C        C     kernel, treating the file as a DLA.
C        C     In this example, it's a very short
C        C     search.
C        C
C              CALL DLABFS ( HANDLE, DLADSC, FOUND )
C
C              IF ( .NOT. FOUND ) THEN
C        C
C        C        We arrive here only if the kernel
C        C        contains no segments.  This is
C        C        unexpected, but we're prepared for it.
C        C
C                 CALL SETMSG ( 'No segments found '
C             .   //            'in DSK file #.'    )
C                 CALL ERRCH  ( '#',  DSK           )
C                 CALL SIGERR ( 'SPICE(NODATA)'     )
C
C              END IF
C
C        C
C        C     If we made it this far, DLADSC is the
C        C     DLA descriptor of the first segment.
C        C
C        C     Read and display type 2 bookkeeping data.
C        C
C              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT,
C             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL,
C             .              VTXNPL, VOXNPT, VOXNPL                 )
C
C        C
C        C     Show vertex and plate counts.
C        C
C              OUTLIN = 'Number of vertices:                 #'
C              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Number of plates:                   #'
C              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Voxel edge length (km):             #'
C              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Number of voxels:                   #'
C              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C        C
C        C     Close the kernel.  This isn't necessary in a stand-
C        C     alone program, but it's good practice in subroutines
C        C     because it frees program and system resources.
C        C
C              CALL DASCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using the DSK file named phobos512.bds, the output
C        was:
C
C
C        Enter DSK name > phobos512.bds
C        Number of vertices:                 1579014
C        Number of plates:                   3145728
C        Voxel edge length (km):             1.04248E-01
C        Number of voxels:                   11914500
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
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 12-OCT-2021 (JDR) (NJB)
C
C        Bug fix: added call to FAILED following call to DASHAM.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Created
C        complete code example #1 from existing code fragment
C        and added code example #2 using example from DSKB02.
C
C        Updated entries in the $Revisions section.
C
C-    SPICELIB Version 2.0.0, 10-FEB-2017 (NJB)
C
C        Updated to use DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 1.3.3, 05-OCT-2006 (NJB)
C
C        Corrected DASADD calling sequence error in code example.
C        Updated $Particulars header section to mention closing DAS
C        files without segregation via calls to DASWBR and DASLLC.
C
C-    SPICELIB Version 1.3.2, 24-MAR-2003 (NJB)
C
C        DASWBR call has been reinstated for scratch DAS case.
C        This call has the side effect of freeing buffer records
C        owned by the file DASWBR writes to. Failing to free these
C        records can cause write errors on HP/Fortran systems.
C
C-    SPICELIB Version 1.2.2, 27-FEB-2003 (NJB)
C
C        Tests whether file to be closed is a scratch DAS; if
C        so, buffer flushes and record segregation are omitted.
C
C-    SPICELIB Version 1.1.1, 26-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Modified the $Examples section to demonstrate the new ID word
C        format which includes a file type and to include a call to the
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C        Changed the value of the parameter FTSIZE from 20 to 21. This
C        change makes the value of FTSIZE in DASCLS compatible with the
C        value in DASFM. See DASFM for a discussion of the reasons for
C        the increase in the value.
C
C-    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB)
C
C        FHSET is now saved.
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     close an open DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.3.2, 24-MAR-2003 (NJB)
C
C        DASWBR call has been reinstated for scratch DAS case.
C        This call has the side effect of freeing buffer records
C        owned by the file DASWBR writes to. Failing to free these
C        records can cause write errors on HP/Fortran systems.
C
C-    SPICELIB Version 1.2.2, 27-FEB-2003 (NJB)
C
C        Tests whether file to be closed is a scratch DAS; if
C        so, buffer flushes and record segregation are omitted.
C
C-    SPICELIB Version 1.1.1, 26-OCT-1993 (KRG)
C
C        Changed the value of the parameter FTSIZE from 20 to 21. This
C        change makes the value of FTSIZE in DASCLS compatible with the
C        value in DASFM. See DASFM for a discussion of the reasons for
C        the increase in the value.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               ELEMI
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

C
C     Local variables
C
      CHARACTER*(10)        METHOD

      INTEGER               IOSTAT
      INTEGER               UNIT

      LOGICAL               NOTSCR

C
C     Saved variables
C
      INTEGER               FHSET ( LBCELL : FTSIZE )
      SAVE                  FHSET

      LOGICAL               PASS1
      SAVE                  PASS1

C
C     Initial values
C
      DATA                  PASS1  / .TRUE.  /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASCLS' )

      IF ( PASS1 ) THEN
         CALL SSIZEI ( FTSIZE, FHSET )
         PASS1 = .FALSE.
      END IF

C
C     There are only four items on our worklist:
C
C        1)  Determine whether the file open for reading or writing,
C            and if it's open for writing, whether it's a scratch
C            file.
C
C        2)  If the DAS file is open for writing, flush any updated
C            records from the data buffers to the file.
C
C        3)  If the DAS file is open for writing, re-order the records
C            in the file so that the data is segregated by data type.
C
C        4)  Close the file.
C

C
C     See whether the input handle designates an open DAS file.  If not,
C     return now.
C
      CALL DASHOF ( FHSET )

      IF (  .NOT.  ELEMI( HANDLE, FHSET )  )  THEN
         CALL CHKOUT ( 'DASCLS' )
         RETURN
      END IF

C
C     If the file is open for writing, flush any buffered
C     records that belong to it.
C
      CALL DASHAM ( HANDLE, METHOD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASCLS' )
         RETURN
      END IF

      IF ( METHOD .EQ. 'WRITE ') THEN
C
C        Make sure that all buffered records belonging to the
C        indicated file are written out.
C
         CALL DASWBR ( HANDLE )

C
C        We cannot directly test the status of the file, but if
C        the file is unnamed, it must be a scratch file.
C
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASCLS' )
            RETURN
         END IF

         INQUIRE ( UNIT   = UNIT,
     .             NAMED  = NOTSCR,
     .             IOSTAT = IOSTAT )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Error occurred while performing an  '//
     .                    'INQUIRE on a DAS file about to be '  //
     .                    'closed.  IOSTAT = #. File handle '   //
     .                    'was #.  Logical unit was #.'         )
            CALL ERRINT ( '#',  IOSTAT                          )
            CALL ERRINT ( '#',  HANDLE                          )
            CALL ERRINT ( '#',  UNIT                            )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'                )
            CALL CHKOUT ( 'DASCLS'                              )
            RETURN

         END IF


         IF ( NOTSCR ) THEN
C
C           Segregate the data records in the file according to data
C           type.
C
            CALL DASSDR ( HANDLE )

         END IF


      END IF

C
C     Close the file.
C
      CALL DASLLC ( HANDLE )

      CALL CHKOUT ( 'DASCLS' )
      RETURN
      END

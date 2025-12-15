C$Procedure DLABBS ( DLA, begin backward search )

      SUBROUTINE DLABBS ( HANDLE, DLADSC, FOUND )

C$ Abstract
C
C     Begin a backward segment search in a DLA file.
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
C     DLA
C
C$ Keywords
C
C     DAS
C     DLA
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'

      INTEGER               HANDLE
      INTEGER               DLADSC  ( * )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of open DLA file.
C     DLADSC     O   Descriptor of last segment in DLA file.
C     FOUND      O   Flag indicating whether a segment was found.
C
C$ Detailed_Input
C
C     HANDLE   is the integer handle associated with the file to be
C              searched. This handle is used to identify the file in
C              subsequent calls to other DLA or DAS routines.
C
C$ Detailed_Output
C
C     DLADSC   is the descriptor of the last DLA segment in the
C              file associated with HANDLE.
C
C              The segment descriptor layout is:
C
C                 +---------------+
C                 | BACKWARD PTR  | Linked list backward pointer
C                 +---------------+
C                 | FORWARD PTR   | Linked list forward pointer
C                 +---------------+
C                 | BASE INT ADDR | Base DAS integer address
C                 +---------------+
C                 | INT COMP SIZE | Size of integer segment component
C                 +---------------+
C                 | BASE DP ADDR  | Base DAS d.p. address
C                 +---------------+
C                 | DP COMP SIZE  | Size of d.p. segment component
C                 +---------------+
C                 | BASE CHR ADDR | Base DAS character address
C                 +---------------+
C                 | CHR COMP SIZE | Size of character segment component
C                 +---------------+
C
C              DLADSC is valid only if the output argument FOUND is
C              .TRUE.
C
C     FOUND    is a logical flag indicating whether a segment was
C              found. FOUND has the value .TRUE. if the file
C              contains at least one segment; otherwise FOUND is
C              .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, an error is
C         signaled by a routine in the call tree of this routine.
C
C     2)  If an error occurs while reading the DLA file, the error
C         is signaled by a routine in the call tree of this routine.
C
C     3)  If the input descriptor is invalid, this routine will
C         fail in an unpredictable manner.
C
C$ Files
C
C     See description of input argument HANDLE.
C
C$ Particulars
C
C     DLA files are built using the DAS low-level format; DLA files are
C     a specialized type of DAS file in which data are organized as a
C     doubly linked list of segments. Each segment's data belong to
C     contiguous components of character, double precision, and integer
C     type.
C
C     This routine supports backward traversal of a DLA file's segment
C     list. Note that it is not necessary to call this routine to
C     conduct a backward traversal; all that is necessary is to have
C     access to the last descriptor in the file, which this routine
C     provides.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Open a DLA file for read access, traverse the segment
C        list from back to front, and display segment address
C        and size attributes.
C
C        Example code begins here.
C
C
C              PROGRAM DLABBS_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'dla.inc'
C
C        C
C        C     Local parameters
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    FNAME
C
C              INTEGER               CURRNT ( DLADSZ )
C              INTEGER               DLADSC ( DLADSZ )
C              INTEGER               HANDLE
C              INTEGER               NSEGS
C              INTEGER               SEGNO
C
C              LOGICAL               FOUND
C
C        C
C        C     Prompt for the name of the file to search.
C        C
C              CALL PROMPT ( 'Name of DLA file > ', FNAME )
C
C        C
C        C     Open the DLA file for read access.  Since DLA
C        C     files use the DAS architecture, we can use DAS
C        C     routines to open and close the file.
C        C
C              CALL DASOPR ( FNAME, HANDLE )
C
C        C
C        C     Count the segments in the file; this allows us
C        C     to label the segments in our display.
C        C
C              NSEGS = 0
C              CALL DLABBS ( HANDLE, DLADSC, FOUND )
C
C              DO WHILE ( FOUND )
C
C                 NSEGS = NSEGS + 1
C                 CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
C                 CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
C
C              END DO
C
C        C
C        C     Begin a backward search.  Let DLADSC contain
C        C     the descriptor of the last segment.
C        C
C              SEGNO = NSEGS + 1
C
C              CALL DLABBS ( HANDLE, DLADSC, FOUND )
C
C              DO WHILE ( FOUND )
C        C
C        C        Display the contents of the current segment
C        C        descriptor.
C        C
C                 SEGNO = SEGNO - 1
C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Segment number = ', SEGNO
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Backward segment pointer         = ',
C             .               DLADSC(BWDIDX)
C                 WRITE (*,*) 'Forward segment pointer          = ',
C             .               DLADSC(FWDIDX)
C                 WRITE (*,*) 'Character component base address = ',
C             .               DLADSC(CBSIDX)
C                 WRITE (*,*) 'Character component size         = ',
C             .               DLADSC(CSZIDX)
C                 WRITE (*,*) 'D.p. base address                = ',
C             .               DLADSC(DBSIDX)
C                 WRITE (*,*) 'D.p. component size              = ',
C             .               DLADSC(DSZIDX)
C                 WRITE (*,*) 'Integer base address             = ',
C             .               DLADSC(IBSIDX)
C                 WRITE (*,*) 'Integer component size           = ',
C             .               DLADSC(ISZIDX)
C                 WRITE (*,*) ' '
C
C        C
C        C        Find the previous segment.
C        C
C        C        To avoid using DLADSC as both input and output
C        C        in the following call (this use is not allowed
C        C        by the ANSI Fortran 77 standard), we copy DLADSC
C        C        into the variable CURRNT.  We then find the
C        C        segment preceding CURRNT.
C        C
C                 CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
C                 CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
C
C              END DO
C
C        C
C        C     Close the file using the DAS close routine.
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
C        Name of DLA file > phobos512.bds
C
C
C         Segment number =            1
C
C         Backward segment pointer         =           -1
C         Forward segment pointer          =           -1
C         Character component base address =            0
C         Character component size         =            0
C         D.p. base address                =            0
C         D.p. component size              =      4737076
C         Integer base address             =           11
C         Integer component size           =     29692614
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Changed output argument name DESCR to DLADSC for consistency
C        with other routines.
C
C        Edits the header comments to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.0, 21-APR-2010 (NJB)
C
C-&


C$ Index_Entries
C
C     begin backward search in DLA file
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
      INTEGER               THIS

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DLABBS' )

C
C     Nothing found yet.
C
      FOUND = .FALSE.

C
C     Look up the pointer to the last DLA segment descriptor in the
C     file.  Then look up the segment descriptor itself.
C
      CALL DASRDI ( HANDLE, LLEIDX, LLEIDX, THIS )

      IF (  FAILED()  .OR.  ( THIS .EQ. NULPTR )  ) THEN
C
C        If the pointer THIS is null, there are no segments in the
C        file.
C
         CALL CHKOUT ( 'DLABBS' )
         RETURN

      END IF

C
C     Return the last descriptor.
C
      CALL DASRDI ( HANDLE, THIS, THIS+DLADSZ-1, DLADSC )

      FOUND = .TRUE.

      CALL CHKOUT ( 'DLABBS' )
      RETURN
      END

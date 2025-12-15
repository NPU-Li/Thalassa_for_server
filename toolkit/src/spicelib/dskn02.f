C$Procedure DSKN02 ( DSK, type 2, compute normal vector for plate )

      SUBROUTINE DSKN02 ( HANDLE, DLADSC, PLID, NORMAL )

C$ Abstract
C
C     Compute the unit normal vector for a specified plate from a type
C     2 DSK segment.
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
C     DSK
C
C$ Keywords
C
C     DAS
C     DSK
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'

      INTEGER               HANDLE
      INTEGER               DLADSC ( * )
      INTEGER               PLID
      DOUBLE PRECISION      NORMAL ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DSK file handle.
C     DLADSC     I   DLA descriptor.
C     PLID       I   Plate ID.
C     NORMAL     O   Plate's unit normal vector.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a DSK file containing a type 2
C              segment from which data are to be fetched.
C
C     DLADSC   is the DLA descriptor associated with the segment
C              from which data are to be fetched.
C
C     PLID     is the plate ID. Plate IDs range from 1 to NP
C              (the number of plates).
C
C$ Detailed_Output
C
C     NORMAL   is the normal vector associated with the plate
C              designated by PLID. The direction of NORMAL is
C              determined by the order of the plate's vertices;
C              the vertices are presumed to be ordered in the
C              right-handed (counterclockwise) sense about the
C              normal direction. If the plate's vertices are
C              V1, V2, V3, then NORMAL points in the direction
C
C                 (V2 - V1) x ( V3 - V2 )
C
C              where "x" represents the cross product operator.
C
C              The vector NORMAL is expressed in the body-fixed
C              reference frame of the segment designated by DLADSC.
C              The center of this frame is the origin of the cartesian
C              coordinate system in which the vertices are expressed.
C              Note that the frame center need not coincide with the
C              central body of the segment. Units are km.
C
C              The vector has magnitude 1.
C
C              If an error occurs on the call, NORMAL is undefined.
C
C$ Parameters
C
C     See the include file
C
C        dla.inc
C
C     for declarations of DLA descriptor sizes and documentation of the
C     contents of DLA descriptors.
C
C     See the include file
C
C        dskdsc.inc
C
C     for declarations of DSK descriptor sizes and documentation of the
C     contents of DSK descriptors.
C
C     See the include file
C
C        dsk02.inc
C
C     for declarations of DSK data type 2 (plate model) parameters.
C
C$ Exceptions
C
C     1)  If the input handle is invalid, an error is signaled by a
C         routine in the call tree of this routine.
C
C     2)  If a file read error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C     3)  If the input DLA descriptor is invalid, the effect of this
C         routine is undefined. The error *may* be diagnosed by
C         routines in the call tree of this routine, but there are no
C         guarantees.
C
C     4)  If PLID is less than 1 or greater than the number of plates
C         in the segment, the error SPICE(INDEXOUTOFRANGE) is signaled.
C
C     5)  This routine does not check for linear independence of the
C         plate's edges. The plate model is assumed to be geometrically
C         valid.
C
C$ Files
C
C     See input argument HANDLE.
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
C     1) Look up all the vertices associated with each plate
C        of the model contained in a specified type 2 segment. For each
C        of the first 5 plates, display the plate's vertices and normal
C        vector.
C
C        For this example, we'll show the context of this look-up:
C        opening the DSK file for read access, traversing a trivial,
C        one-segment list to obtain the segment of interest.
C
C
C        Example code begins here.
C
C
C              PROGRAM DSKN02_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'dla.inc'
C              INCLUDE 'dsk02.inc'
C
C
C              CHARACTER*(*)         FMT
C              PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
C
C
C              INTEGER               BUFSIZ
C              PARAMETER           ( BUFSIZ = 10000 )
C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C
C              CHARACTER*(FILSIZ)    DSK
C
C              DOUBLE PRECISION      NORMAL ( 3 )
C              DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
C
C              INTEGER               DLADSC ( DLADSZ )
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C              INTEGER               N
C              INTEGER               NNORM
C              INTEGER               NP
C              INTEGER               NREAD
C              INTEGER               NV
C              INTEGER               NVTX
C              INTEGER               PLATES  ( 3, BUFSIZ )
C              INTEGER               PLIX
C              INTEGER               REMAIN
C              INTEGER               START
C
C              LOGICAL               FOUND
C
C        C
C        C     Prompt for name of DSK and open file for reading.
C        C
C              CALL PROMPT ( 'Enter DSK name > ', DSK )
C
C              CALL DASOPR ( DSK, HANDLE )
C
C              CALL DLABFS ( HANDLE, DLADSC, FOUND )
C
C              IF ( .NOT. FOUND ) THEN
C
C                 CALL SETMSG ( 'No segment found in file #.' )
C                 CALL ERRCH  ( '#',  DSK                     )
C                 CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
C
C              END IF
C
C        C
C        C     Get segment vertex and plate counts.
C        C
C              CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Number of vertices: ', NV
C              WRITE (*,*) 'Number of plates:   ', NP
C        C
C        C     Display the vertices of the first 5 plates.
C        C
C              REMAIN = MIN(5, NP)
C              START  = 1
C
C              DO WHILE ( REMAIN .GT. 0 )
C        C
C        C        NREAD is the number of plates we'll read on this
C        C        loop pass.
C        C
C                 NREAD  = MIN ( BUFSIZ, REMAIN )
C
C                 CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
C             .                 PLATES                          )
C
C                 DO I = 1, N
C
C                    PLIX = START + I - 1
C        C
C        C           Read the vertices of the current plate.
C        C
C                    DO J = 1, 3
C                       CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
C             .                       1,      NVTX,   VERTS (1,J)  )
C                    END DO
C        C
C        C           Display the vertices of the current plate:
C        C
C                    WRITE (*,*  ) ' '
C                    WRITE (*,*  ) 'Plate number: ', PLIX
C                    WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
C                    WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
C                    WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
C
C        C
C        C           Display the normal vector of the current plate:
C        C
C                    CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
C
C                    WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
C
C                 END DO
C
C                 START  = START  + NREAD
C                 REMAIN = REMAIN - NREAD
C
C              END DO
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
C
C         Number of vertices:      1579014
C         Number of plates:        3145728
C
C         Plate number:            1
C            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
C            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
C            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
C            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
C
C         Plate number:            2
C            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
C            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
C            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
C            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
C
C         Plate number:            3
C            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
C            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
C            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
C            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
C
C         Plate number:            4
C            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
C            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
C            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
C            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
C
C         Plate number:            5
C            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
C            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
C            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
C            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
C
C
C        Note that only the vertex information for first 5 plates is
C        provided.
C
C$ Restrictions
C
C     See $Exceptions section.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) (BVS)
C
C        Edited the header to comply with NAIF standard. Modified code
C        example to reduce the output.
C
C        Extended detailed description of NORMAL output argument and
C        added an additional record to $Index_Entries.
C
C-    SPICELIB Version 1.0.0, 17-MAR-2016 (NJB)
C
C        Now calls ZZDDHHLU.
C
C        Deleted references to unused parameter. Updated
C        $Examples section.
C
C        DSKLIB Version 1.0.0, 02-JUN-2010 (NJB)
C
C-&


C$ Index_Entries
C
C     compute normal vector for a type 2 DSK plate
C     compute normal vector from DSK type 2 plate id
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
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      EDGE1  ( 3 )
      DOUBLE PRECISION      EDGE2  ( 3 )
      DOUBLE PRECISION      VERTS  ( 3, 3 )

      INTEGER               I
      INTEGER               N
      INTEGER               NP
      INTEGER               NV
      INTEGER               PLATE  ( 3 )
      INTEGER               START
      INTEGER               UNIT

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DSKN02' )

C
C     Look up the DSK descriptor for this segment.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )

C
C     Get the plate model size parameters for this segment.
C     Note that we get a segment data type check for free from
C     DSKZ02.
C
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )

C
C     Check START.
C
      IF (  ( PLID .LT. 1 ) .OR. ( PLID .GT. NP )  ) THEN

         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

         CALL SETMSG ( 'Segment in DSK file # with DAS base '
     .   //            'addresses INT = #, DP = #, CHR = # '
     .   //            'contains # plates, so PLID must '
     .   //            'be in the range 1:#; actual value '
     .   //            'was #.'                              )
         CALL ERRFNM ( '#', UNIT                             )
         CALL ERRINT ( '#', DLADSC(IBSIDX)                   )
         CALL ERRINT ( '#', DLADSC(DBSIDX)                   )
         CALL ERRINT ( '#', DLADSC(CBSIDX)                   )
         CALL ERRINT ( '#', NP                               )
         CALL ERRINT ( '#', NP                               )
         CALL ERRINT ( '#', PLID                             )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'              )
         CALL CHKOUT ( 'DSKN02'                              )
         RETURN

      END IF

C
C     Look up the plate and its vertices.
C
      START = (PLID-1)*3  +  1

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START, 3, N, PLATE )

      DO I = 1, 3

         START = ( PLATE(I) - 1 )*3  + 1

         CALL DSKD02 ( HANDLE, DLADSC, KWVERT, START, 3, N, VERTS(1,I) )

      END DO

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DSKN02' )
         RETURN
      END IF

C
C     Use the right-handed order of the vertices to determine the
C     correct choice of normal direction.
C
      CALL VSUB ( VERTS(1,2), VERTS(1,1), EDGE1 )
      CALL VSUB ( VERTS(1,3), VERTS(1,1), EDGE2 )

      CALL UCRSS ( EDGE1, EDGE2, NORMAL )

      CALL CHKOUT ( 'DSKN02' )
      RETURN
      END

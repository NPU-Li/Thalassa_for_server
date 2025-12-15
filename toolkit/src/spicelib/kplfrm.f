C$Procedure KPLFRM ( Kernel pool frame IDs )

      SUBROUTINE KPLFRM ( FRMCLS, IDSET )

C$ Abstract
C
C     Return a SPICE set containing the frame IDs of all reference
C     frames of a given class having specifications in the kernel pool.
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
C     CELLS
C     FRAMES
C     KERNEL
C     NAIF_IDS
C     SETS
C
C$ Keywords
C
C     FRAME
C     SET
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'frmtyp.inc'
      INCLUDE 'ninert.inc'
      INCLUDE 'nninrt.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRMCLS
      INTEGER               IDSET  ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRMCLS     I   Frame class.
C     IDSET      O   Set of ID codes of frames of the specified class.
C
C$ Detailed_Input
C
C     FRMCLS   is an integer code specifying the frame class or
C              classes for which frame ID codes are requested.
C              The applicable reference frames are those having
C              specifications present in the kernel pool.
C
C              FRMCLS may designate a single class or "all
C              classes."
C
C              The include file frmtyp.inc declares parameters
C              identifying frame classes. The supported values
C              and corresponding meanings of FRMCLS are
C
C                 Parameter      Value    Meaning
C                 =========      =====    =================
C                 ALL              -1     All frame classes
C                                         specified in the
C                                         kernel pool. Class 1
C                                         is not included.
C
C                 INERTL            1     Built-in inertial.
C                                         No frames will be
C                                         returned in the
C                                         output set.
C
C                 PCK               2     PCK-based frame
C
C                 CK                3     CK-based frame
C
C                 TK                4     Fixed rotational
C                                         offset ("text
C                                         kernel") frame
C
C                 DYN               5     Dynamic frame
C
C                 SWTCH             6     Switch frame
C
C$ Detailed_Output
C
C     IDSET    is a SPICE set containing the ID codes of all
C              reference frames having specifications present in
C              the kernel pool and belonging to the specified
C              class or classes.
C
C              Note that if FRMCLS is set to INERTL, IDSET
C              will be empty on output.
C
C$ Parameters
C
C     See the INCLUDE file frmtyp.inc.
C
C$ Exceptions
C
C     1)  If the input frame class argument is not defined in
C         frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled.
C
C     2)  If the size of IDSET is too small to hold the requested frame
C         ID set, the error SPICE(SETTOOSMALL) is signaled.
C
C     3)  Frames of class 1 may not be specified in the kernel pool.
C         However, for the convenience of users, this routine does not
C         signal an error if the input class is set to INERTL. In this
C         case the output set will be empty.
C
C     4)  This routine relies on the presence of just three kernel
C         variable assignments for a reference frame in order to
C         determine that that reference frame has been specified:
C
C            FRAME_<frame name>       = <ID code>
C            FRAME_<ID code>_NAME     = <frame name>
C
C         and either
C
C            FRAME_<ID code>_CLASS    = <class>
C
C         or
C
C            FRAME_<frame name>_CLASS = <class>
C
C         It is possible for the presence of an incomplete frame
C         specification to trick this routine into incorrectly
C         deciding that a frame has been specified. This routine
C         does not attempt to diagnose this problem.
C
C$ Files
C
C     Reference frame specifications for frames that are not built in
C     are typically established by loading frame kernels.
C
C$ Particulars
C
C     This routine enables SPICE-based applications to conveniently
C     find the frame ID codes of reference frames having specifications
C     present in the kernel pool. Such frame specifications are
C     introduced into the kernel pool either by loading frame kernels
C     or by means of calls to the kernel pool "put" API routines
C
C        PCPOOL
C        PDPOOL
C        PIPOOL
C
C     Given a reference frame's ID code, other attributes of the
C     frame can be obtained via calls to the SPICELIB routines
C
C        FRMNAM {Return a frame's name}
C        FRINFO {Return a frame's center, class, and class ID}
C
C     This routine has a counterpart
C
C        BLTFRM
C
C     which fetches the frame IDs of all built-in reference frames.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Display the IDs and names of all reference frames having
C        specifications present in the kernel pool. Group the outputs
C        by frame class. Also fetch and display the entire set of IDs
C        and names using the parameter ALL.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: kplfrm_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name            Contents
C              --------------       --------------------------
C              clem_v20.tf          Clementine FK
C              moon_060721.tf       Generic Lunar SPICE frames
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'clem_v20.tf'
C                                  'moon_060721.tf' )
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM KPLFRM_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'frmtyp.inc'
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               CARDI
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'kplfrm_ex1.tm' )
C
C              INTEGER               NFRAME
C              PARAMETER           ( NFRAME = 1000 )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FRNMLN)    FRNAME
C              CHARACTER*(LNSIZE)    OUTLIN
C
C              INTEGER               I
C              INTEGER               IDSET ( LBCELL : NFRAME )
C              INTEGER               J
C
C        C
C        C     Initialize the frame set.
C        C
C              CALL SSIZEI ( NFRAME, IDSET )
C
C        C
C        C     Load kernels that contain frame specifications.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Fetch and display the frames of each class.
C        C
C              DO I = 1, 7
C
C                 IF ( I .LT. 7 ) THEN
C        C
C        C           Fetch the frames of class I.
C        C
C                    CALL KPLFRM ( I, IDSET )
C
C                    OUTLIN = 'Number of frames of class #: #'
C                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN )
C                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                 ELSE
C        C
C        C           Fetch IDs of all frames specified in the kernel
C        C           pool.
C        C
C                    CALL KPLFRM ( ALL, IDSET )
C
C                    OUTLIN = 'Number of frames in the kernel pool: #'
C                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                 END IF
C
C                 CALL TOSTDO ( ' '    )
C                 CALL TOSTDO ( OUTLIN )
C                 CALL TOSTDO ( '   Frame IDs and names' )
C
C                 DO J = 1, CARDI(IDSET)
C                    CALL FRMNAM ( IDSET(J), FRNAME )
C                    WRITE (*,*) IDSET(J), '  ', FRNAME
C                 END DO
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Number of frames of class 1: 0
C           Frame IDs and names
C
C        Number of frames of class 2: 1
C           Frame IDs and names
C               31002   MOON_PA_DE403
C
C        Number of frames of class 3: 1
C           Frame IDs and names
C              -40000   CLEM_SC_BUS
C
C        Number of frames of class 4: 11
C           Frame IDs and names
C              -40008   CLEM_CPT
C              -40007   CLEM_BSTAR
C              -40006   CLEM_ASTAR
C              -40005   CLEM_LIDAR
C              -40004   CLEM_LWIR
C              -40003   CLEM_NIR
C              -40002   CLEM_UVVIS
C              -40001   CLEM_HIRES
C               31000   MOON_PA
C               31001   MOON_ME
C               31003   MOON_ME_DE403
C
C        Number of frames of class 5: 0
C           Frame IDs and names
C
C        Number of frames of class 6: 0
C           Frame IDs and names
C
C        Number of frames in the kernel pool: 13
C           Frame IDs and names
C              -40008   CLEM_CPT
C              -40007   CLEM_BSTAR
C              -40006   CLEM_ASTAR
C              -40005   CLEM_LIDAR
C              -40004   CLEM_LWIR
C              -40003   CLEM_NIR
C              -40002   CLEM_UVVIS
C              -40001   CLEM_HIRES
C              -40000   CLEM_SC_BUS
C               31000   MOON_PA
C               31001   MOON_ME
C               31002   MOON_PA_DE403
C               31003   MOON_ME_DE403
C
C
C$ Restrictions
C
C     1)  This routine will work correctly if the kernel pool contains
C         no invalid frame specifications. See the description of
C         exception 4 above. Users must ensure that no invalid frame
C         specifications are introduced into the kernel pool, either by
C         loaded kernels or by means of the kernel pool "put" APIs.
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
C-    SPICELIB Version 2.0.0, 08-AUG-2021 (JDR) (NJB)
C
C        Updated to account for switch frame class.
C
C        Edited the header to comply with NAIF standard.
C
C        Updated Example's kernels set to use Clementine PDS archived
C        data.
C
C-    SPICELIB Version 1.1.0, 18-JUN-2015 (NJB)
C
C        Bug fix: previous algorithm failed if the number of frame
C        names fetched from the kernel pool on a single call exceeded
C        twice the kernel variable buffer size. The count of
C        fetched names is now maintained correctly.
C
C-    SPICELIB Version 1.0.0, 22-MAY-2012 (NJB)
C
C-&


C$ Index_Entries
C
C     fetch IDs of reference_frames from the kernel_pool
C
C-&


C
C     SPICELIB functions
C
      INTEGER               SIZEI
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVBUFF ( BUFSIZ )
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(KVNMLN)    KVTEMP
      CHARACTER*(KVNMLN)    KVCODE
      CHARACTER*(KVNMLN)    KVCLAS
      CHARACTER*(FRNMLN)    TMPNAM

      INTEGER               FCLASS
      INTEGER               I
      INTEGER               IDCODE
      INTEGER               L
      INTEGER               M
      INTEGER               N
      INTEGER               TO
      INTEGER               TOTAL
      INTEGER               W

      LOGICAL               FOUND

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'KPLFRM' )

C
C     The output set starts out empty.
C
      CALL SCARDI ( 0, IDSET )

C
C     Check the input frame class.
C
C     This block of code must be kept in sync with frmtyp.inc.
C
      IF  (      ( FRMCLS .GT. SWTCH )
     .      .OR. ( FRMCLS .EQ. 0     )
     .      .OR. ( FRMCLS .LT. ALL   )  ) THEN

         CALL SETMSG ( 'Frame class specifier FRMCLS was #; this '
     .   //            'value is not supported.'                  )
         CALL ERRINT ( '#',  FRMCLS                               )
         CALL SIGERR ( 'SPICE(BADFRAMECLASS)'                     )
         CALL CHKOUT ( 'KPLFRM'                                   )
         RETURN

      END IF

C
C     Initialize the output buffer index. The
C     index is to be incremented prior to each
C     write to the buffer.
C
      TO = 0

C
C     Find all of the kernel variables having names
C     that could correspond to frame name assignments.
C
C     We expect that all frame specifications will
C     include assignments of the form
C
C         FRAME_<ID code>_NAME = <frame name>
C
C     We may pick up some additional assignments that are not part of
C     frame specifications; we plan to filter out as many as possible
C     by looking the corresponding frame ID and frame class
C     assignments.
C
      KVTEMP = 'FRAME_*_NAME'

      CALL GNPOOL ( KVTEMP, 1, BUFSIZ, N, KVBUFF, FOUND )

      TOTAL = 0

      DO WHILE ( N .GT. 0 )

         TOTAL = TOTAL + N

C
C        At least one kernel variable was found by the last
C        GNPOOL call. Each of these variables is a possible
C        frame name. Look up each of these candidate names.
C
         DO I = 1, N
C
C           Attempt to fetch the right hand side value for
C           the Ith kernel variable found on the previous
C           GNPOOL call.
C
            CALL GCPOOL ( KVBUFF(I), 1, 1, M, FRNAME, FOUND )

            IF ( FOUND ) THEN
C
C              We found a possible frame name. Attempt to look
C              up an ID code variable for the name. The assignment
C              for the ID code, if present, will have the form
C
C                 FRAME_<name> = <ID code>
C
C              Create the kernel variable name on the left hand
C              side of the assignment.
C
               KVCODE = 'FRAME_<name>'

               CALL REPMC ( KVCODE, '<name>', FRNAME, KVCODE )

C
C              Try to fetch the ID code.
C
               CALL GIPOOL ( KVCODE, 1, 1, L, IDCODE, FOUND )

               IF ( FOUND ) THEN
C
C                 We found an integer on the right hand side
C                 of the assignment. We probably have a
C                 frame specification at this point. Check that
C                 the variable
C
C                    FRAME_<ID code>_NAME
C
C                 is present in the kernel pool and maps to
C                 the name FRNAME.
C
                  KVNAME = 'FRAME_<code>_NAME'

                  CALL REPMI ( KVNAME, '<code>', IDCODE, KVNAME )

                  CALL GCPOOL ( KVNAME, 1, 1, W, TMPNAM, FOUND )


                  IF ( FOUND ) THEN
C
C                    Try to look up the frame class using a
C                    kernel variable name of the form
C
C                       FRAME_<integer ID code>_CLASS
C
C                    Create the kernel variable name on the left
C                    hand side of the frame class assignment.
C
                     KVCLAS = 'FRAME_<integer>_CLASS'

                     CALL REPMI ( KVCLAS, '<integer>', IDCODE, KVCLAS )

C
C                    Look for the frame class.
C
                     CALL GIPOOL ( KVCLAS, 1, 1, W, FCLASS, FOUND )

                     IF ( .NOT. FOUND ) THEN
C
C                       Try to look up the frame class using a kernel
C                       variable name of the form
C
C                          FRAME_<frame name>_CLASS
C
                        KVCLAS = 'FRAME_<name>_CLASS'

                        CALL REPMC ( KVCLAS, '<name>', FRNAME, KVCLAS )

                        CALL GIPOOL ( KVCLAS, 1, 1, W, FCLASS, FOUND )

                     END IF

C
C                    At this point FOUND indicates whether we found
C                    the frame class.
C
                     IF ( FOUND ) THEN
C
C                       Check whether the frame class is one
C                       we want.
C
                        IF (      ( FRMCLS .EQ. ALL    )
     .                       .OR. ( FRMCLS .EQ. FCLASS )  ) THEN
C
C                          We have a winner. Add it to the output set.
C
C                          First make sure the set is large enough to
C                          hold another element.
C
                           IF ( TO .EQ. SIZEI(IDSET) ) THEN

                              CALL SETMSG ( 'Frame ID set argument '
     .                        //            'IDSET has size #; required'
     .                        //            ' size is at least #. Make '
     .                        //            'sure that the caller of '
     .                        //            'this routine has initial'
     .                        //            'ized IDSET via SSIZEI.'   )
                              CALL ERRINT ( '#', SIZEI(IDSET)          )
                              CALL ERRINT ( '#', TO+1                  )
                              CALL SIGERR ( 'SPICE(SETTOOSMALL)'       )
                              CALL CHKOUT ( 'KPLFRM'                   )
                              RETURN

                           END IF

                           TO        = TO + 1
                           IDSET(TO) = IDCODE

                        END IF
C
C                       End of IF block for processing a frame having
C                       a frame class matching the request.
C
                     END IF
C
C                    End of IF block for finding the frame class.
C
                  END IF
C
C                 End of IF block for finding the frame name.
C
               END IF
C
C              End of IF block for finding the frame ID.
C
            END IF
C
C           End of IF block for finding string value corresponding to
C           the Ith kernel variable matching the name template.
C
         END DO
C
C        End of loop for processing last batch of potential
C        frame names.
C
C        Fetch next batch of potential frame names.
C
         CALL GNPOOL ( KVTEMP, TOTAL+1, BUFSIZ, N, KVBUFF, FOUND )

      END DO
C
C     At this point all kernel variables that matched the frame name
C     keyword template have been processed. All frames of the specified
C     class or classes have had their ID codes appended to IDSET. In
C     general IDSET is not yet a SPICELIB set, since it's not sorted
C     and it may contain duplicate values.
C
C     Turn IDSET into a set. VALIDI sorts and removes duplicates.
C
      CALL VALIDI ( SIZEI(IDSET), TO, IDSET )

      CALL CHKOUT ( 'KPLFRM' )
      RETURN
      END

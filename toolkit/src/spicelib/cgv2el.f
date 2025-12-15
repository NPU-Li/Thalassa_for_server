C$Procedure CGV2EL ( Center and generating vectors to ellipse )

      SUBROUTINE CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )

C$ Abstract
C
C     Form a SPICE ellipse from a center vector and two generating
C     vectors.
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
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSE
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               UBEL
      PARAMETER           ( UBEL    =   9 )

      DOUBLE PRECISION      CENTER (    3 )
      DOUBLE PRECISION      VEC1   (    3 )
      DOUBLE PRECISION      VEC2   (    3 )
      DOUBLE PRECISION      ELLIPS ( UBEL )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CENTER,
C     VEC1,
C     VEC2       I   Center and two generating vectors for an ellipse.
C     ELLIPS     O   The SPICE ellipse defined by the input vectors.
C
C$ Detailed_Input
C
C     CENTER,
C     VEC1,
C     VEC2     are a center and two generating vectors defining
C              an ellipse in three-dimensional space. The
C              ellipse is the set of points
C
C                 CENTER  +  cos(theta) VEC1  +  sin(theta) VEC2
C
C              where theta ranges over the interval (-pi, pi].
C              VEC1 and VEC2 need not be linearly independent.
C
C$ Detailed_Output
C
C     ELLIPS   is the SPICE ellipse defined by the input
C              vectors.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If VEC1 and VEC2 are linearly dependent, ELLIPS will be
C         degenerate. SPICE ellipses are allowed to represent
C         degenerate geometric ellipses.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SPICE ellipses serve to simplify calling sequences and reduce
C     the chance for error in declaring and describing argument lists
C     involving ellipses.
C
C     The set of ellipse conversion routines is
C
C        CGV2EL ( Center and generating vectors to ellipse )
C        EL2CGV ( Ellipse to center and generating vectors )
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a SPICE ellipse given its center and two linearly
C        independent generating vectors of the ellipse.
C
C
C        Example code begins here.
C
C
C              PROGRAM CGV2EL_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 UBEL
C              PARAMETER             ( UBEL =   9 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        CENTER ( 3    )
C              DOUBLE PRECISION        ECENTR ( 3    )
C              DOUBLE PRECISION        ELLIPS ( UBEL )
C              DOUBLE PRECISION        SMAJOR ( 3    )
C              DOUBLE PRECISION        SMINOR ( 3    )
C              DOUBLE PRECISION        VEC1   ( 3    )
C              DOUBLE PRECISION        VEC2   ( 3    )
C
C              INTEGER                 I
C
C        C
C        C     Define the center and two linearly independent
C        C     generating vectors of an ellipse (the vectors need not
C        C     be linearly independent).
C        C
C              DATA                    CENTER / -1.D0,  1.D0, -1.D0 /
C
C              DATA                    VEC1   /  1.D0,  1.D0,  1.D0 /
C
C              DATA                    VEC2   /  1.D0, -1.D0,  1.D0 /
C
C        C
C        C     Create the ELLIPS.
C        C
C              CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
C
C        C
C        C     In a real application, please use SPICELIB API EL2CGV
C        C     to retrieve the center and generating vectors from the
C        C     ellipse structure (see next block).
C        C
C              WRITE(*,'(A)') 'SPICE ellipse:'
C              WRITE(*,'(A,3F10.6)') '  Semi-minor axis:',
C             .                                    ( ELLIPS(I), I=7,9 )
C              WRITE(*,'(A,3F10.6)') '  Semi-major axis:',
C             .                                    ( ELLIPS(I), I=4,6 )
C              WRITE(*,'(A,3F10.6)') '  Center         :',
C             .                                    ( ELLIPS(I), I=1,3 )
C              WRITE(*,*) ' '
C
C        C
C        C     Obtain the center and generating vectors from the
C        C     ELLIPS.
C        C
C              CALL EL2CGV ( ELLIPS, ECENTR, SMAJOR, SMINOR )
C
C              WRITE(*,'(A)') 'SPICE ellipse (using EL2CGV):'
C              WRITE(*,'(A,3F10.6)') '  Semi-minor axis:', SMINOR
C              WRITE(*,'(A,3F10.6)') '  Semi-major axis:', SMAJOR
C              WRITE(*,'(A,3F10.6)') '  Center         :', ECENTR
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        SPICE ellipse:
C          Semi-minor axis:  0.000000  1.414214  0.000000
C          Semi-major axis:  1.414214 -0.000000  1.414214
C          Center         : -1.000000  1.000000 -1.000000
C
C        SPICE ellipse (using EL2CGV):
C          Semi-minor axis:  0.000000  1.414214  0.000000
C          Semi-major axis:  1.414214 -0.000000  1.414214
C          Center         : -1.000000  1.000000 -1.000000
C
C
C     2) Find the intersection of an ellipse with a plane.
C
C
C        Example code begins here.
C
C
C              PROGRAM CGV2EL_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 UBEL
C              PARAMETER             ( UBEL =   9 )
C
C              INTEGER                 UBPL
C              PARAMETER             ( UBPL =   4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        CENTER ( 3    )
C              DOUBLE PRECISION        ELLIPS ( UBEL )
C              DOUBLE PRECISION        NORMAL ( 3    )
C              DOUBLE PRECISION        PLANE  ( UBPL )
C              DOUBLE PRECISION        VEC1   ( 3    )
C              DOUBLE PRECISION        VEC2   ( 3    )
C              DOUBLE PRECISION        XPTS   ( 3, 2 )
C
C              INTEGER                 I
C              INTEGER                 NXPTS
C
C        C
C        C     The ellipse is defined by the vectors CENTER, VEC1, and
C        C     VEC2. The plane is defined by the normal vector NORMAL
C        C     and the CENTER.
C        C
C              DATA                    CENTER /  0.D0,  0.D0,  0.D0 /
C              DATA                    VEC1   /  1.D0,  7.D0,  2.D0 /
C              DATA                    VEC2   / -1.D0,  1.D0,  3.D0 /
C
C              DATA                    NORMAL /  0.D0,  1.D0,  0.D0 /
C
C        C
C        C     Make a SPICE ellipse and a plane.
C        C
C              CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
C              CALL NVP2PL ( NORMAL, CENTER,     PLANE  )
C
C        C
C        C     Find the intersection of the ellipse and plane.
C        C     NXPTS is the number of intersection points; XPTS
C        C     are the points themselves.
C        C
C              CALL INELPL ( ELLIPS,    PLANE,    NXPTS,
C             .              XPTS(1,1), XPTS(1,2)       )
C
C              WRITE(*,'(A,I2)') 'Number of intercept points: ', NXPTS
C
C              DO I = 1, NXPTS
C
C                 WRITE(*,'(A,I2,A,3F10.6)') '  Point', I, ':',
C             .                         XPTS(1,I), XPTS(2,I), XPTS(3,I)
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
C        Number of intercept points:  2
C          Point 1:  1.131371  0.000000 -2.687006
C          Point 2: -1.131371 -0.000000  2.687006
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     center and generating vectors to ellipse
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     SPICE ellipses contain a center vector, a semi-major
C     axis vector, and a semi-minor axis vector.  These are
C     located, respectively, in elements
C
C        CTRPOS through CTRPOS + 1
C
C        MAJPOS through MAJPOS + 1
C
C        MINPOS through MINPOS + 1
C
C
      INTEGER               CTRPOS
      PARAMETER           ( CTRPOS = 1 )

      INTEGER               MAJPOS
      PARAMETER           ( MAJPOS = 4 )

      INTEGER               MINPOS
      PARAMETER           ( MINPOS = 7 )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CGV2EL' )
      END IF

C
C     The center of the ellipse is held in the first three elements.
C
      CALL VEQU ( CENTER, ELLIPS(CTRPOS) )

C
C     Find the semi-axes of the ellipse.  These may be degenerate.
C
      CALL SAELGV ( VEC1, VEC2, ELLIPS(MAJPOS), ELLIPS(MINPOS) )

      CALL CHKOUT ( 'CGV2EL' )
      RETURN
      END

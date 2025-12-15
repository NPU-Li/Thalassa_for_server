C$Procedure VUPACK ( Unpack three scalar components from a vector )

      SUBROUTINE VUPACK ( V, X, Y, Z )

C$ Abstract
C
C     Unpack three scalar components from a vector.
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
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION V ( 3 )
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V          I   Input 3-dimensional vector.
C     X,
C     Y,
C     Z          O   Scalar components of the vector.
C
C$ Detailed_Input
C
C     V        is a double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C     X,
C     Y,
C     Z        are the double precision scalar components of the
C              vector V. The following equalities hold:
C
C                 X = V(1)
C                 Y = V(2)
C                 Z = V(3)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Basically, this is just shorthand notation for the common
C     sequence
C
C        X = V(1)
C        Y = V(2)
C        Z = V(3)
C
C     The routine is useful largely for two reasons. First, it
C     reduces the chance that the programmer will make a "cut and
C     paste" mistake, like
C
C        X = V(1)
C        Y = V(1)
C        Z = V(1)
C
C     Second, it makes conversions between equivalent units simpler,
C     and clearer. For instance, the sequence
C
C        X = V(1) * RPD()
C        Y = V(2) * RPD()
C        Z = V(3) * RPD()
C
C     can be replaced by the (nearly) equivalent sequence
C
C        CALL VSCLIP ( RPD(),  V    )
C        CALL VUPACK ( V,   X, Y, Z )
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that you have an instrument kernel that provides,
C        within a single keyword, the three frequencies used by the
C        instrument, and that you want to use these frequencies
C        independently within your code.
C
C        The following code example demonstrates how to use VUPACK
C        to get these frequencies into independent scalar variables.
C
C        Use the kernel shown below, an IK defining the three
C        frequencies used by an instrument with NAIF ID -999001.
C
C
C           KPL/IK
C
C           File name: vupack_ex1.ti
C
C           The keyword below define the three frequencies used by a
C           hypothetical instrument (NAIF ID -999001). They correspond
C           to three filters: red, green and blue. Frequencies are
C           given in micrometers.
C
C           \begindata
C
C              INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 )
C              INS-999001_FREQ_UNITS = ( 'MICROMETERS'       )
C
C           \begintext
C
C
C           End of IK
C
C
C        Example code begins here.
C
C
C              PROGRAM VUPACK_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         IKNAME
C              PARAMETER           ( IKNAME = 'vupack_ex1.ti' )
C
C              CHARACTER*(*)         KEYWRD
C              PARAMETER           ( KEYWRD = 'INS-999001_FREQ_RGB' )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      DDATA  ( 3 )
C              DOUBLE PRECISION      RED
C              DOUBLE PRECISION      GREEN
C              DOUBLE PRECISION      BLUE
C
C              INTEGER               N
C
C              LOGICAL               FOUND
C
C        C
C        C     Load the instrument kernel.
C        C
C              CALL FURNSH ( IKNAME )
C
C        C
C        C     Get the frequency data from the kernel pool.
C        C
C              CALL GDPOOL ( KEYWRD, 1, 3, N, DDATA, FOUND )
C
C              IF ( FOUND ) THEN
C
C                 CALL VUPACK ( DDATA, RED, GREEN, BLUE )
C                 WRITE(*,'(A,F6.2)') 'Blue  (nm): ', BLUE  * 1000.D0
C                 WRITE(*,'(A,F6.2)') 'Green (nm): ', GREEN * 1000.D0
C                 WRITE(*,'(A,F6.2)') 'Red   (nm): ', RED   * 1000.D0
C
C              ELSE
C
C                 WRITE(*,*) 'No data found in the kernel pool for ',
C             .              KEYWRD
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Blue  (nm): 475.00
C        Green (nm): 550.00
C        Red   (nm): 650.00
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 07-SEP-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Fixed order of operands in equalities presented in
C        $Detailed_Output. Updated code fragments in $Particulars to
C        use in-place vector-scaling API.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     unpack three scalar components from a vector
C
C-&


C
C     Just shorthand, like it says above.
C
      X = V(1)
      Y = V(2)
      Z = V(3)

      RETURN
      END

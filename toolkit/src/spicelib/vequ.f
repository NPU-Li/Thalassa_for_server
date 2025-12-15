C$Procedure VEQU ( Vector equality, 3 dimensions )

      SUBROUTINE VEQU ( VIN, VOUT )

C$ Abstract
C
C     Make one double precision 3-dimensional vector equal to
C     another.
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
C     ASSIGNMENT
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION  VIN   ( 3 )
      DOUBLE PRECISION  VOUT  ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VIN        I   Double precision 3-dimensional vector.
C     VOUT       O   Double precision 3-dimensional vector set equal
C                    to VIN.
C
C$ Detailed_Input
C
C     VIN      is an arbitrary, double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C     VOUT     is a double precision 3-dimensional vector set equal
C              to VIN.
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
C     VEQU simply sets each component of VOUT in turn equal to VIN. No
C     error checking is performed because none is needed.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Lets assume we have a pointing record that contains the
C        start time of an interpolation interval, the components of
C        the quaternion that represents the C-matrix associated with
C        the start time of the interval, and the angular velocity vector
C        of the interval. The following example demonstrates how to
C        extract the time, the quaternion and the angular velocity
C        vector into separate variables for their processing.
C
C
C        Example code begins here.
C
C
C              PROGRAM VEQU_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      AV     ( 3 )
C              DOUBLE PRECISION      QUAT   ( 4 )
C              DOUBLE PRECISION      RECORD ( 8 )
C              DOUBLE PRECISION      TIME
C
C              INTEGER               I
C
C        C
C        C     Define the pointing record. We would normally obtain it
C        C     from, e.g. CK readers or other non SPICE data files.
C        C
C              DATA                  RECORD  /
C             .      283480.753D0,   0.99999622D0,  0.0D0,  0.0D0,
C             .     -0.0027499965D0, 0.0D0,         0.0D0,  0.01D0 /
C
C        C
C        C     Get the time, quaternion and angular velocity vector
C        C     into separate variables.
C        C
C              TIME = RECORD(1)
C
C              CALL VEQUG  ( RECORD(2), 4, QUAT )
C              CALL VEQU   ( RECORD(6),    AV   )
C
C        C
C        C     Display the contents of the variables.
C        C
C              WRITE(*,'(A,F11.3)') 'Time            :', TIME
C
C              WRITE(*,'(A)')       'Quaternion      :'
C              WRITE(*,'(4F15.10)')  QUAT
C              WRITE(*,'(A)')       'Angular velocity:'
C              WRITE(*,'(3F15.10)')  AV
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Time            : 283480.753
C        Quaternion      :
C           0.9999962200   0.0000000000   0.0000000000  -0.0027499965
C        Angular velocity:
C           0.0000000000   0.0000000000   0.0100000000
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
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     assign a 3-dimensional vector to another
C
C-&


      VOUT(1) = VIN(1)
      VOUT(2) = VIN(2)
      VOUT(3) = VIN(3)

      RETURN
      END

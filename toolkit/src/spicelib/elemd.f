C$Procedure ELEMD ( Element of a double precision set )

      LOGICAL FUNCTION ELEMD ( ITEM, A )

C$ Abstract
C
C     Determine whether an item is an element of a double
C     precision set.
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
C     SETS
C
C$ Keywords
C
C     CELLS
C     SETS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER                   LBCELL
      PARAMETER               ( LBCELL = -5 )

      DOUBLE PRECISION ITEM
      DOUBLE PRECISION A      ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item to be tested.
C     A          I   Set to be tested.
C
C     The function returns .TRUE. if ITEM is an element of set A.
C
C$ Detailed_Input
C
C     ITEM     is an item which may or may not be an element of the
C              input set.
C
C     A        is a SPICE set.
C
C$ Detailed_Output
C
C     The function returns .TRUE. if ITEM is a member of the set A, and
C     returns .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The LOGICAL functions ELEMC, ELEMD and ELEMI correspond to the
C     set operator IN in the Pascal language.
C
C     This routine uses a binary search to check for the presence in
C     the set of the specified ITEM.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Check if the elements of a list of double precision numbers
C        belong to a given double precision set.
C
C
C        Example code begins here.
C
C
C              PROGRAM ELEMD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              LOGICAL                 ELEMD
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 LBCELL
C              PARAMETER             ( LBCELL = -5 )
C
C              INTEGER                 LISTSZ
C              PARAMETER             ( LISTSZ   = 6   )
C
C              INTEGER                 SETDIM
C              PARAMETER             ( SETDIM   = 8   )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        A      ( LBCELL:SETDIM )
C              DOUBLE PRECISION        ITEMS  ( LISTSZ        )
C
C              INTEGER                 I
C
C        C
C        C     Set the values of the set and the list of double
C        C     precision numbers.
C        C
C              DATA                  ( A(I), I=1,SETDIM)  /
C             .                            -1.D0, 0.D0, 1.D0,  1.D0,
C             .                             3.D0, 5.D0, 0.D0, -3.D0 /
C
C              DATA                    ITEMS /    6.D0, -1.D0, 0.D0,
C             .                                   2.D0, 3.D0, -2.D0 /
C
C        C
C        C     Validate the set: Initialize the non-empty set, remove
C        C     duplicates and sort the elements.
C        C
C              CALL VALIDD ( SETDIM, SETDIM, A )
C
C        C
C        C     Check if the items in the list belong to the set.
C        C
C              DO I = 1, LISTSZ
C
C                 IF ( ELEMD ( ITEMS(I), A ) ) THEN
C
C                    WRITE(*,'(A,F5.1,A)') 'Item ', ITEMS(I),
C             .                          ' is in the set.'
C
C                 ELSE
C
C                    WRITE(*,'(A,F5.1,A)') 'Item ', ITEMS(I),
C             .                            ' is NOT in the set.'
C
C                 END IF
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
C        Item   6.0 is NOT in the set.
C        Item  -1.0 is in the set.
C        Item   0.0 is in the set.
C        Item   2.0 is NOT in the set.
C        Item   3.0 is in the set.
C        Item  -2.0 is NOT in the set.
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
C     C.A. Curzon        (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is .TRUE. upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
C
C-&


C$ Index_Entries
C
C     element of a d.p. set
C
C-&


C
C     SPICELIB functions
C
      INTEGER               BSRCHD
      INTEGER               CARDD
      LOGICAL               RETURN


C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         ELEMD = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'ELEMD' )
      END IF

C
C     Just a binary search.
C
      ELEMD = ( BSRCHD ( ITEM, CARDD ( A ), A(1) ) .NE. 0 )

      CALL CHKOUT ( 'ELEMD' )
      RETURN
      END

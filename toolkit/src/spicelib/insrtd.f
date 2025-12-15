C$Procedure INSRTD ( Insert an item into a double precision set )

      SUBROUTINE INSRTD ( ITEM, A )

C$ Abstract
C
C     Insert an item into a double precision set.
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

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      ITEM
      DOUBLE PRECISION      A     ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item to be inserted.
C     A         I-O  Insertion set.
C
C$ Detailed_Input
C
C     ITEM     is an item which is to be inserted into the specified
C              set. ITEM may or may not already be an element of the
C              set.
C
C     A        is a SPICE set.
C
C              On input, A may or may not contain the input item as an
C              element.
C
C$ Detailed_Output
C
C     A        on output, contains the union of the input set and the
C              singleton set containing the input item, unless there was
C              not sufficient room in the set for the item to be
C              included, in which case the set is not changed and an
C              error is signaled.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the insertion of the element into the set causes an excess
C         of elements, the error SPICE(SETEXCESS) is signaled.
C
C$ Files
C
C     None.
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
C     1) Create an double precision set for ten elements, insert items
C        to it and then remove the even values.
C
C
C        Example code begins here.
C
C
C              PROGRAM INSRTD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER                 CARDD
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 LBCELL
C              PARAMETER             ( LBCELL = -5 )
C
C              INTEGER                 SETDIM
C              PARAMETER             ( SETDIM   = 10  )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        A      ( LBCELL:SETDIM )
C              DOUBLE PRECISION        EVEN   ( SETDIM        )
C              DOUBLE PRECISION        ITEMS  ( SETDIM        )
C
C              INTEGER                 I
C
C        C
C        C     Create a list of items and even numbers.
C        C
C              DATA                    EVEN  /
C             .                      0.D0,  2.D0,  4.D0,  6.D0,  8.D0,
C             .                     10.D0, 12.D0, 14.D0, 16.D0, 18.D0  /
C
C              DATA                    ITEMS /
C             .                      0.D0,  1.D0,  1.D0,  2.D0,  3.D0,
C             .                      5.D0,  8.D0, 10.D0, 13.D0, 21.D0  /
C
C        C
C        C     Initialize the empty set.
C        C
C              CALL VALIDD ( SETDIM, 0, A )
C
C        C
C        C     Insert the list of double precision numbers into the
C        C     set. If the item is an element of the set, the set is
C        C     not changed.
C        C
C              DO I = 1, SETDIM
C
C                 CALL INSRTD ( ITEMS(I), A )
C
C              END DO
C
C        C
C        C     Output the original contents of set A.
C        C
C              WRITE(*,*) 'Items in original set A:'
C              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
C              WRITE(*,*) ' '
C
C        C
C        C     Remove the even values. If the item is not an element of
C        C     the set, the set is not changed.
C        C
C              DO I = 1, SETDIM
C
C                 CALL REMOVD ( EVEN(I), A )
C
C              END DO
C
C        C
C        C     Output the contents of A.
C        C
C              WRITE(*,*) 'Odd numbers in set A:'
C              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
C              WRITE(*,*) ' '
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Items in original set A:
C           0.0   1.0   2.0   3.0   5.0   8.0  10.0  13.0  21.0
C
C         Odd numbers in set A:
C           1.0   3.0   5.0  13.0  21.0
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
C
C        Code was modified slightly to keep logical structure parallel
C        to that of INSRTC.
C
C        Long error message was updated to include size of
C        set into which insertion was attempted.
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
C     insert an item into a d.p. set
C
C-&


C
C     SPICELIB functions
C
      INTEGER             CARDD
      INTEGER             LSTLED
      INTEGER             SIZED

      LOGICAL             RETURN

C
C     Local variables
C
      INTEGER             CARD
      INTEGER             I
      INTEGER             LAST
      INTEGER             SIZE

      LOGICAL             IN

C
C     Set up the error processing.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'INSRTD' )

C
C     What are the size and cardinality of the set?
C
      SIZE = SIZED ( A )
      CARD = CARDD ( A )

C
C     Find the last element of the set which would come before the
C     input item. This will be the item itself, if it is already an
C     element of the set.
C
      LAST = LSTLED ( ITEM, CARD, A(1) )

C
C     Is the item already in the set? If not, it needs to be inserted.
C
      IF ( LAST .GT. 0 ) THEN
         IN  =  A(LAST) .EQ. ITEM
      ELSE
         IN  =  .FALSE.
      END IF


      IF ( .NOT. IN ) THEN
C
C        If there is room in the set for the new element, then move
C        the succeeding elements back to make room. And update the
C        cardinality for future reference.
C
         IF ( CARD .LT. SIZE ) THEN

            DO I = CARD,  LAST+1,  -1
               A(I+1) = A(I)
            END DO

            A(LAST+1) = ITEM

            CALL SCARDD ( CARD+1,  A )

         ELSE

            CALL SETMSG ( 'An element could not be inserted '   //
     .                    'into the set due to lack of space; ' //
     .                    'set size is #.'                      )
            CALL ERRINT ( '#', SIZE                             )
            CALL SIGERR ( 'SPICE(SETEXCESS)'                    )

         END IF

      END IF

      CALL CHKOUT ( 'INSRTD' )
      RETURN
      END

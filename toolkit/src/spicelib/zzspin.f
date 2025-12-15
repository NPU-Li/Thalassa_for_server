C$Procedure ZZSPIN ( Separation quantity initializer )

      SUBROUTINE ZZSPIN ( TARG1,  SHAPE1, FRAME1,
     .                    TARG2,  SHAPE2, FRAME2,
     .                    OBSRVR, ABCORR,
     .                    BODS,   FRAMES, MXRAD, OBS )

C$ Abstract
C
C     Check/initialize the quantities describing an angular separation
C     between two spherical or point objects.
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
C     ABCORR.REQ
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR
      INTEGER               BODS   (2)
      INTEGER               FRAMES (2)
      DOUBLE PRECISION      MXRAD  (2)
      INTEGER               OBS

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARG1      I   Name for first target
C     SHAPE1     I   Shape corresponding to TARG1
C     FRAME1     I   Body fixed frame for TARG1
C     TARG2      I   Name for second target
C     SHAPE2     I   Shape corresponding to TARG2
C     FRAME2     I   Body fixed frame for TARG2
C     OBSRVR     I   Name for observer body
C     ABCORR     I   Aberration correction flag
C     BODS       O   Array(2) of body/SPK IDs for TARG1 and TARG2
C     FRAMES     O   Array(2) of frame IDs for TARG1 and TARG2
C     MXRAD      O   Array(2) of max radii values for TARG1 and TARG2
C     OBS        O   Body/SPK IDs of OBSRVR
C
C$ Detailed_Input
C
C     TARG1    the string naming the first body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE1   the string naming the geometric model used to
C              represent the shape of the TARG1 body. Models
C              supported by this routine:
C
C                'SPHERE'        Treat the body as a sphere with
C                                radius equal to the maximum value of
C                                BODYnnn_AXES.
C
C                'POINT'         Treat the body as a point;
C                                radius has value zero.
C
C              The SHAPE1 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME1   the string naming the body-fixed reference frame
C              corresponding to TARG1. This routine does not currently
C              use this argument's value, its use is reserved for
C              future shape models. The value 'NULL' will suffice for
C              "POINT" and "SPHERE" shaped bodies.
C
C     TARG2    the string naming the second body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE2   the string naming the geometric model used to
C              represent the shape of the TARG2 body. Models
C              supported by this routine:
C
C                'SPHERE'        Treat the body as a sphere with
C                                radius equal to the maximum value of
C                                BODYnnn_AXES.
C
C                'POINT'         Treat the body as a point;
C                                radius has value zero.
C
C              The SHAPE2 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME2   the string naming the body-fixed reference frame
C              corresponding to TARG2. This routine does not currently
C              use this argument's value, its use is reserved for
C              future shape models. The value 'NULL' will suffice for
C              "POINT" and "SPHERE" shaped bodies.
C
C     OBSRVR   the string naming the observing body. Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to supply to indicate the
C              observer is Earth.
C
C     ABCORR   the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
C
C                 'LT'       "Reception" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case: converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case: converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 The ABCORR string lacks sensitivity to case, leading
C                 and trailing blanks.
C
C$ Detailed_Output
C
C     BODS     the NAIF SPK/body IDs of the two objects for which to
C              determine the angular separation.
C
C     FRAMES   the NAIF frame IDs of the body fixed reference frames
C              of two objects for which to determine the angular
C              separation.
C
C     MXRAD    the maximum radial values of the two objects.
C
C     OBS      the NAIF SPK/body ID of the observer.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the three objects TARG1, TARG2, and OBSRVR are not
C         distinct, the error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     2)  If the SHAPE1 or SHAPE2 values lack a corresponding case
C         block, the error SPICE(BUG) is signaled. This indicates a
C         programming error.
C
C     3)  If the object names for TARG1 TARG2, or OBSRVR cannot resolve
C         to a NAIF body ID, the error SPICE(IDCODENOTFOUND) is
C         signaled.
C
C     4)  If the reference frame associated with TARG1, FRAME1, is not
C         centered on TARG1, or if the reference frame associated with
C         TARG2, FRAME2, is not centered on TARG2, the error
C         SPICE(INVALIDFRAME) is signaled.
C
C     5)  If the frame name for FRAME1 or FRAME2 cannot resolve to a
C         NAIF frame ID, the error SPICE(NOFRAME) is signaled.
C
C     6)  If the body shape for TARG1, SHAPE1, or the body shape for
C         TARG2, SHAPE2, is not recognized, the error
C         SPICE(NOTRECOGNIZED) is signaled.
C
C     7)  If the requested aberration correction ABCORR is not
C         recognized, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Please refer to the Aberration Corrections Required Reading
C     (abcorr.req) for detailed information describing the nature and
C     calculation of the applied corrections.
C
C$ Examples
C
C     None.
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-JAN-2021 (EDW) (JDR)
C
C        Based on code originally in zzgfspu.f.
C
C-&


C$ Index_Entries
C
C     check quantities for apparent relative angular separation
C
C-&


C
C     SPICELIB functions.
C
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               FOUND

C
C     Local parameters
C
      INTEGER               SHAPLN
      PARAMETER           ( SHAPLN = 6 )


C
C     Local Variables
C
      CHARACTER*(SHAPLN)    SHAP1
      CHARACTER*(SHAPLN)    SHAP2

      DOUBLE PRECISION      AXES1 (3)
      DOUBLE PRECISION      AXES2 (3)

      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CTR1
      INTEGER               CTR2
      INTEGER               SHP1
      INTEGER               SHP2

      LOGICAL               ATTBLK ( NABCOR )

C
C     Below we initialize the list of shape names.
C
      INTEGER               SPSHPN
      PARAMETER           ( SPSHPN = 2 )

      CHARACTER*(32)        SVSHAP ( SPSHPN )

C
C     Define integer ID parameters for the shape names in
C     SVSHAP.
C
      INTEGER               POINT
      PARAMETER           ( POINT = 1 )

      INTEGER               SPHERE
      PARAMETER           ( SPHERE = 2 )

      SAVE                  SVSHAP

      DATA                  SVSHAP / 'POINT',
     .                               'SPHERE' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZSPIN' )

C
C     FRAMES argument not currently used.
C
      FRAMES(1) = -1
      FRAMES(2) = -1

      CALL BODS2C ( TARG1, BODS(1), FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 1, '
     .      //         '''#'', is not a recognized name for an '
     .      //         'ephemeris object. The cause of this '
     .      //         'problem may be that you need an updated '
     .      //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', TARG1                              )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZSPIN'                                   )
         RETURN

      END IF


      CALL BODS2C ( TARG2, BODS(2), FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 2, '
     .      //         '''#'', is not a recognized name for an '
     .      //         'ephemeris object. The cause of this '
     .      //         'problem may be that you need an updated '
     .      //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', TARG2                              )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZSPIN'                                )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, OBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The object name for the observer, '
     .      //            '''#'', is not a recognized name for an '
     .      //            'ephemeris object. The cause of this '
     .      //            'problem may be that you need an updated '
     .      //            'version of the SPICE Toolkit.'          )
         CALL ERRCH  ( '#', OBSRVR                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                     )
         CALL CHKOUT ( 'ZZSPIN'                                    )
         RETURN

      END IF


C
C     Confirm the three bodies have unique IDs.
C
      IF      (       ( OBS     .EQ. BODS(1) )
     .           .OR. ( OBS     .EQ. BODS(2) )
     .           .OR. ( BODS(1) .EQ. BODS(2)) ) THEN

         CALL SETMSG( 'All three objects associated '
     .      //        'with an ANGULAR SEPARATION calculation '
     .      //        'must be distinct. The objects whose angular '
     .      //        'separation is of interest were # and #. '
     .      //        'The observer was #.'                       )
         CALL ERRINT( '#', BODS(1)                                )
         CALL ERRINT( '#', BODS(2)                                )
         CALL ERRINT( '#', OBS                                    )
         CALL SIGERR( 'SPICE(BODIESNOTDISTINCT)'                  )
         CALL CHKOUT( 'ZZSPIN'                                    )
         RETURN

      END IF

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSPIN' )
         RETURN
      END IF


C
C     Check first shape.
C
      CALL LJUCRS ( 1, SHAPE1, SHAP1 )

C
C     If we pass the error check, then SHAPE1 exists in SVSHAP.
C
      SHP1 = ISRCHC( SHAP1, SPSHPN, SVSHAP)

      IF ( SHP1 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not recognized.  '
     .      //         'Supported quantities are: '
     .      //         'POINT, SPHERE.'                      )
         CALL ERRCH  ( '#', SHAP1                            )
         CALL SIGERR ( 'SPICE(NOTRECOGNIZED)'                )
         CALL CHKOUT ( 'ZZSPIN'                              )
         RETURN

      ELSE IF ( SHP1 . EQ. POINT ) THEN

         MXRAD(1) = 0.D0

      ELSE IF ( SHP1 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( BODS(1), AXES1 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZSPIN' )
            RETURN
         END IF

         MXRAD(1) = MAX( AXES1(1), AXES1(2), AXES1(3) )

      ELSE

C
C        This code executes only if someone adds a new shape
C        name to SVSHAP then fails to update the SHP1 condition
C        block to respond to the name. Fortran needs SWITCH...CASE.
C
         CALL SETMSG ('Encountered uncoded shape ID for #. '     //
     .                'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAP1                                  )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZSPIN'                                   )
         RETURN

      END IF

C
C     Check second shape.
C
      CALL LJUCRS ( 1, SHAPE2, SHAP2 )

C
C     If we pass the error check, then SHAPE2 exists in SVSHAP.
C
      SHP2 = ISRCHC( SHAP2, SPSHPN, SVSHAP)

      IF ( SHP2 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not '                 //
     .                 'recognized.  Supported quantities are: '   //
     .                 'POINT, SPHERE.'                             )
         CALL ERRCH ( '#', SHAP2                                    )
         CALL SIGERR( 'SPICE(NOTRECOGNIZED)'                        )
         CALL CHKOUT( 'ZZSPIN'                                      )
         RETURN

      ELSE IF ( SHP2 .EQ. POINT ) THEN

         MXRAD(2) = 0.D0

      ELSE IF ( SHP2 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( BODS(2), AXES2 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZSPIN' )
            RETURN
         END IF

         MXRAD(2) = MAX( AXES2(1), AXES2(2), AXES2(3) )

      ELSE

C
C        This code executes only if someone adds a new shape
C        name to SVSHAP then fails to update the SHP2 condition
C        block to respond to the name. Fortran needs SWITCH...CASE.
C
         CALL SETMSG ('Encountered uncoded shape ID for #. '     //
     .                'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAP2                                  )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZSPIN'                                   )
         RETURN

      END IF


C
C     Confirm the center of the input reference frames correspond
C     to the target bodies for non-point, non-spherical bodies.
C
C        FRAME1 centered on TARG1
C        FRAME2 centered on TARG2
C
C     This check does not apply to POINT or SPHERE shapes.
C

      IF ( (SHP1 .NE. POINT)  .AND. (SHP1 .NE. SPHERE) ) THEN

         CALL NAMFRM ( FRAME1, FRAMES(1) )

         CALL FRINFO ( FRAMES(1), CTR1, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', FRAME1                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZSPIN'                                  )
            RETURN

         END IF

         IF ( BODS(1) .NE. CTR1 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' //
     .                'target body #2 is not centered on #2. The '  //
     .                'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', FRAME1                     )
            CALL ERRCH  ( '#2', TARG1                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZSPIN'                         )
            RETURN

         END IF

      END IF


      IF ( (SHP2 .NE. POINT)  .AND. (SHP2 .NE. SPHERE) ) THEN

         CALL NAMFRM ( FRAME2, FRAMES(2) )

         CALL FRINFO ( FRAMES(2), CTR2, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', FRAME2                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZSPIN'                                  )
            RETURN

         END IF

         IF ( BODS(2) .NE. CTR2 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' //
     .                'target body #2 is not centered on #2. The '  //
     .                'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', FRAME2                     )
            CALL ERRCH  ( '#2', TARG2                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZSPIN'                         )
            RETURN

         END IF

      END IF

C
C     Done. Check out. Return.
C

      CALL CHKOUT ('ZZSPIN')
      RETURN
      END

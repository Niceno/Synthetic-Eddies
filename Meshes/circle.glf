# Pointwise V17.2R2 Journal file - Sat Jan 27 12:01:41 2018

#------------------
# Initial settings
#------------------
package require PWI_Glyph 2.17.2
pw::Application setUndoMaximumLevels 5
pw::Application reset
pw::Application markUndoLevel {Journal Reset}
pw::Application clearModified

#------------------------------------------
# Set grid to be 2D (defies logic, but ...
#   ... don't have time to play with it
#------------------------------------------
pw::Application setCAESolver {CGNS} 2
pw::Application markUndoLevel {Set Dimension 2D}
pw::Application setCAESolver {GAMBIT} 2
pw::Application markUndoLevel {Select Solver}

#----------------------------------------
# Constants (parameters) for this script
#----------------------------------------
set PI            3.14159265359
set R             1.0
set B            [expr $R * 0.1]
set C            [expr $R * 0.667]

set Y_PLUS       [expr $R * 0.004]

# Concerning resolution
set N_BNDRY_LAY        9
set N_INNER_LAY        9
set N_CORE            13

# Coordinates origin
set X_0  0.0
set Y_0  $X_0
set Z_0  0.0

# Some helping variables which (most likely) don't need adjustment
set ANGLE_DEG  45
set ANGLE_RAD  [expr $ANGLE_DEG * $PI / 180.0]
set COS_ANGLE  [expr cos($ANGLE_RAD)]

source "delnov.glf"

#------------#
#            #
#   Points   #
#            #
#------------#

#-------------------------
# Point in the centre (1)
#-------------------------
[pw::Point create]  setPoint [list $X_0  \
                                   $Y_0  \
                                   $Z_0]
#----------------------------------------
# Three points defining the core (2,3,4)
#----------------------------------------
[pw::Point create]  setPoint [list [expr $X_0 + $C]  \
                                   $Y_0              \
                                   $Z_0]

[pw::Point create]  setPoint [list [expr $X_0 + $COS_ANGLE * $C]  \
                                   [expr $Y_0 + $COS_ANGLE * $C]  \
                                   $Z_0]

[pw::Point create]  setPoint [list $X_0              \
                                   [expr $Y_0 + $C]  \
                                   $Z_0]
#-----------------------------------------------------------------
# Two points defining the upper limit of the boundary layer (5,6)
#-----------------------------------------------------------------
[pw::Point create]  setPoint [list [expr $R - $B]  \
                                   $Y_0            \
                                   $Z_0]

[pw::Point create]  setPoint [list $X_0            \
                                   [expr $R - $B]  \
                                   $Z_0]
#------------------------------------
# Two points defining the wall (7,8)
#------------------------------------
[pw::Point create]  setPoint [list $R     \
                                   $Y_0   \
                                   $Z_0]

[pw::Point create]  setPoint [list $X_0   \
                                   $R     \
                                   $Z_0]


#-----------#
#           #
#   Lines   #
#           #
#-----------#

#--------------------------
# Create lines in the core
#--------------------------
Delnov_Create_Line_From_Points_Name "point-1" "point-2" "core-1"
Delnov_Create_Arc_Name "point-2" "point-3" [expr $X_0 - $R*1.0] $Y_0 $Z_0 "core-2"
Delnov_Create_Arc_Name "point-3" "point-4" $X_0 [expr $Y_0 - $R*1.0] $Z_0 "core-3"
Delnov_Create_Line_From_Points_Name "point-1" "point-4" "core-4"

#---------------------------------
# Create lines in the inner layer
#---------------------------------
Delnov_Create_Line_From_Points_Name "point-2" "point-5" "inner-1"
Delnov_Create_Line_From_Points_Name "point-4" "point-6" "inner-2"

#------------------------------------
# Create lines in the boundary layer
#------------------------------------
Delnov_Create_Line_From_Points_Name "point-5" "point-7" "bndlay-1"
Delnov_Create_Line_From_Points_Name "point-6" "point-8" "bndlay-2"

#----------------------------
# Two arcs to wrap things up
#----------------------------
Delnov_Create_Arc_Name "point-5" "point-6" $X_0 $Y_0 $Z_0 "circle-inn-1"
Delnov_Create_Arc_Name "point-7" "point-8" $X_0 $Y_0 $Z_0 "circle-out-1"

#----------------------------------------
# Handle resolutions and grid stretching
#----------------------------------------

# Set dimension for the core
set core_only [Delnov_Get_Entities_By_Name_Pattern "core"]
Delnov_Set_Dimension $core_only $N_CORE

# Set dimension for the boundary layer
set con_blay [Delnov_Get_Entities_By_Name_Pattern "bndlay"]
Delnov_Set_Dimension $con_blay $N_BNDRY_LAY
Delnov_Set_End_Spacing $con_blay $Y_PLUS

# Set dimension for the inner layer
set inner_only [Delnov_Get_Entities_By_Name_Pattern "inner"]
Delnov_Set_Dimension $inner_only $N_INNER_LAY

set con [lindex $con_blay 0]
set spc [Delnov_Get_Begin_Spacing $con]
Delnov_Set_End_Spacing $inner_only $spc

# Set azymuthal dimension
set azym_only [Delnov_Get_Entities_By_Name_Pattern "circle"]
Delnov_Set_Dimension $azym_only [expr $N_CORE + $N_CORE - 1]

#-------------#
#             #
#   Domains   #
#             #
#-------------#

#---------------------------------
# Create domains by edge assembly
#---------------------------------
Delnov_Create_Structured_Domain "core-1"  \
                                "core-2"  \
                                "core-3"  \
                                "core-4"
Delnov_Create_Structured_Domain "inner-1"                  \
                                "circle-inn-1"             \
                                 "inner-2"                 \
                                 [list "core-3" "core-2"]
Delnov_Create_Structured_Domain "bndlay-1"      \
                                "circle-out-1"  \
                                "bndlay-2"      \
                                "circle-inn-1"

#---------------------------------------
# Copy them around to get a full circle
#---------------------------------------
Delnov_Mirror_Entities_By_Name_Pattern "dom" "x"
Delnov_Mirror_Entities_By_Name_Pattern "dom" "y"

#-----------------------------#
#                             #
# Specify boundary conditions #
#                             #
#-----------------------------#
Delnov_Introduce_Bnd_Conds [list "WALL" "PERIODIC"]

set bc [pw::BoundaryCondition getByName "WALL"]
$bc apply [list [list [pw::GridEntity getByName "dom-3" ]          \
                      [pw::GridEntity getByName "circle-out-1"]]   \
                [list [pw::GridEntity getByName "dom-6" ]          \
                      [pw::GridEntity getByName "circle-out-2"]]   \
                [list [pw::GridEntity getByName "dom-9" ]          \
                      [pw::GridEntity getByName "circle-out-3"]]   \
                [list [pw::GridEntity getByName "dom-12" ]         \
                      [pw::GridEntity getByName "circle-out-4"]]]

#--------------------------
#
# Export data for analysis
#
#--------------------------

# Select all the blocks ...
set domains_only [Delnov_Get_Entities_By_Name_Pattern "dom"]

# ... and export them
set export [pw::Application begin CaeExport [pw::Entity sort $domains_only]] 
  $export initialize -type CAE {circle.neu}
  if {![$export verify]} {
    error "Data verification failed."
  }
  $export write
$export end
unset export

puts "Finished"

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
set H             1.0
set W             6.28

set Y_PLUS       [expr $H * 0.004]

# Concerning resolution
set N_NORM       41
set N_SPAN       81

# Coordinates origin
set X_0  0.0
set Y_0  $X_0
set Z_0  0.0

source "delnov.glf"

#------------#
#            #
#   Points   #
#            #
#------------#
[pw::Point create]  setPoint [list $X_0  \
                                   $Y_0  \
                                   $Z_0]
[pw::Point create]  setPoint [list [expr $X_0 + $W]  \
                                   $Y_0              \
                                   $Z_0]
[pw::Point create]  setPoint [list [expr $X_0 + $W]  \
                                   [expr $Y_0 + $H]  \
                                   $Z_0]
[pw::Point create]  setPoint [list $X_0              \
                                   [expr $Y_0 + $H]  \
                                   $Z_0]

#-----------#
#           #
#   Lines   #
#           #
#-----------#

#--------------------------
# Create lines in the core
#--------------------------
Delnov_Create_Line_From_Points_Name "point-1" "point-2" "span-1"
Delnov_Create_Line_From_Points_Name "point-2" "point-3" "norm-1"
Delnov_Create_Line_From_Points_Name "point-3" "point-4" "span-2"
Delnov_Create_Line_From_Points_Name "point-1" "point-4" "norm-2"

# Set dimension for the core
set norm_only [Delnov_Get_Entities_By_Name_Pattern "norm"]
Delnov_Set_Dimension $norm_only $N_NORM

# Set spacing for the first near-wall cell
Delnov_Set_End_Spacing $norm_only $Y_PLUS

# Set dimension for the core
set span_only [Delnov_Get_Entities_By_Name_Pattern "span"]
Delnov_Set_Dimension $span_only $N_SPAN

#-------------#
#             #
#   Domains   #
#             #
#-------------#
Delnov_Create_Structured_Domain "span-1" "norm-1" "span-2" "norm-2"

#----------------------------------------
# Copy them around to get a full channel
#----------------------------------------
Delnov_Mirror_Entities_By_Name_Pattern "dom" "y"

#--------------------------
#
# Export data for analysis
#
#--------------------------

# Select all the blocks ...
set domains_only [Delnov_Get_Entities_By_Name_Pattern "dom"]

# ... and export them
set export [pw::Application begin CaeExport [pw::Entity sort $domains_only]] 
  $export initialize -type CAE {rectangle.neu}
  if {![$export verify]} {
    error "Data verification failed."
  }
  $export write
$export end
unset export

puts "Finished"

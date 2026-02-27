#For updates and bug fixes, please see the actively maintained version of DBDCalc:
#https://github.com/ram-24-mp/DBDCalc

#packages needed
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(rstudioapi))

rm(list = ls(all.names = TRUE))

interval_constant = readline(prompt = "Is your interval thickness constant (True or False) ")

# core dimensions, in cm
if (interval_constant == TRUE) {
  
  dim_defaults = readline(prompt = "Accept Core Dimension Defaults (True or False) ")
  
  if (dim_defaults == TRUE) {
    internal_diameter       = 10.0
    internal_diameter_uncer = 0.1
    surface_area            = pi * (internal_diameter / 2)^2
    surface_area_uncer      = pi * internal_diameter * internal_diameter_uncer / 2
    interval_thickness      = 1
  } else {
    internal_diameter       = as.numeric(readline(prompt = "Enter Diameter (default 10.0) "))
    internal_diameter_uncer = as.numeric(readline(prompt = "Enter Uncertainty (default 0.1) "))
    surface_area            = pi * (internal_diameter / 2)^2
    surface_area_uncer      = pi * internal_diameter * internal_diameter_uncer / 2
    interval_thickness      = as.numeric(readline(prompt = "Enter Interval Thickness (default 1) "))
  }
  
  core_length = as.numeric(readline(prompt = "Enter Core Length "))
  
  # intervals and mid-depths for constant thickness
  n_intervals <- ceiling(core_length / interval_thickness)
  
  mid_depths <- (seq_len(n_intervals) - 0.5) * interval_thickness
  # clamp last midpoint to core_length if the last interval is partial
  mid_depths[mid_depths > core_length] <- (core_length + (core_length - interval_thickness * (n_intervals - 1))) / 2
  
  DBD_table_rows <- n_intervals
  
  DBD_table <- array(
    data = NA,
    dim = c(DBD_table_rows, 17),
    dimnames = list(NULL, c(
      "Mid Depth z(i) (cm)", "Wet Weight (g)","Wet Weight Uncertainty (g)", "Dry Weight (g)", "Dry Weight Uncertainty (g)", "Water Weight (g)", "Water Weight Uncertainty (g)",
      "Water %","Water % Uncertainty (%)", "Estimated Sediment Volume (g/cm^3)", "Estimated Sediment Volume Uncertainty (g/cm^3)",
      "Estimated Water Volume (g/cm^3)", "Estimated Water Volume Uncertainty (g/cm^3)", "Porosity", "Porosity Uncertainty", "Dry Bulk Density (g/cm^3)", "Dry Bulk Density Uncertainty (g/cm^3)"
    ))
  )
  
  DBD_table[, 1] <- mid_depths
  
} else { # interval thickness is variable
  
  how_many_thick = as.numeric(readline(prompt = "How many different interval thicknesses are used in the core? "))
  
  interval_transitions_list = c()
  for (i in 1:(how_many_thick - 1)) {
    interval_transitions_list[i] =
      as.numeric(readline(prompt = paste("Enter interval thickness transition depth ", i, ": ")))
  }
  
  interval_thicknesses_list = c()
  for (i in 1:how_many_thick) {
    interval_thicknesses_list[i] =
      as.numeric(readline(prompt = paste("Enter interval thickness ", i, ": ")))
  }
  
  dim_defaults = readline(prompt = "Accept Core Dimension Defaults (True or False) ")
  
  if (dim_defaults == TRUE) {
    internal_diameter       = 10.0
    internal_diameter_uncer = 0.1
    surface_area            = pi * (internal_diameter / 2)^2
    surface_area_uncer      = pi * internal_diameter * internal_diameter_uncer / 2
  } else {
    internal_diameter       = as.numeric(readline(prompt = "Enter Diameter (default 10.0) "))
    internal_diameter_uncer = as.numeric(readline(prompt = "Enter Uncertainty (default 0.1) "))
    surface_area            = pi * (internal_diameter / 2)^2
    surface_area_uncer      = pi * internal_diameter * internal_diameter_uncer / 2
  }
  
  core_length = as.numeric(readline(prompt = "Enter Core Length "))
  
  interval_thicknesses <- unlist(interval_thicknesses_list)
  interval_transitions <- unlist(interval_transitions_list)
  
  # Segment boundaries: from 0, through transitions, to core_length
  bounds <- c(0, interval_transitions, core_length)
  
  # Length of each depth segment
  segment_lengths <- diff(bounds)
  
  # Number of intervals in each segment (rounded up)
  intervals_per_segment <- ceiling(segment_lengths / interval_thicknesses)
  
  # Base number of intervals
  n_intervals <- sum(intervals_per_segment)
  
  DBD_table_rows <- n_intervals
  
  DBD_table <- array(
    data = NA,
    dim = c(DBD_table_rows, 17),
    dimnames = list(NULL, c(
      "Mid Depth z(i) (cm)", "Wet Weight (g)","Wet Weight Uncertainty (g)", "Dry Weight (g)", "Dry Weight Uncertainty (g)", "Water Weight (g)", "Water Weight Uncertainty (g)",
      "Water %","Water % Uncertainty (%)", "Estimated Sediment Volume (g/cm^3)", "Estimated Sediment Volume Uncertainty (g/cm^3)",
      "Estimated Water Volume (g/cm^3)", "Estimated Water Volume Uncertainty (g/cm^3)", "Porosity", "Porosity Uncertainty", "Dry Bulk Density (g/cm^3)", "Dry Bulk Density Uncertainty (g/cm^3)"
    ))
  )
  
  # 1) Build boundary depths
  boundary_depths <- numeric(0)
  
  for (i in seq_along(interval_thicknesses)) {
    t     <- interval_thicknesses[i]
    start <- bounds[i]
    end   <- bounds[i + 1]
    
    seg_bounds <- seq(from = start, to = end - 1e-9, by = t)
    if (tail(seg_bounds, 1) < end) seg_bounds <- c(seg_bounds, end)
    
    if (length(boundary_depths) > 0) seg_bounds <- seg_bounds[-1]
    
    boundary_depths <- c(boundary_depths, seg_bounds)
  }
  
  stopifnot(length(boundary_depths) == n_intervals + 1)
  
  # mid-depths from adjacent boundaries
  mid_depths <- (boundary_depths[-length(boundary_depths)] + boundary_depths[-1]) / 2
  stopifnot(length(mid_depths) == n_intervals)
  
  DBD_table[, 1] <- mid_depths
}
print("select mass data")
file_path=file.choose()
mass_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric")

wet_weights = readline(prompt = "Do you have both wet and dry sediment weights? (True or False) ")
has_debris= readline(prompt = "Does organic debris make any significant contribution to core sediment? (True or False) ")
DBD_table <- as.data.frame(DBD_table, stringsAsFactors = FALSE)
if (wet_weights ==TRUE & has_debris==FALSE){
  has_particle_density = readline(prompt = "Do you have a measured particle density? (True or False) ")
  if (has_particle_density==TRUE){
    particle_density= as.numeric(readline(prompt = "Enter Particle Density (g/cm^3) "))
    particle_density_uncer= as.numeric(readline(prompt = "Enter Particle Density Uncertainty (g/cm^3) "))
  }else{
    particle_density=2.65
    print("default Particle Density used, 2.65 g/cm^3")
    particle_density_uncer=0.01
  }
  DBD_table[,2:5]=mass_dat[,1:4]
  DBD_table[,6]=DBD_table[,2]-DBD_table[,4]
  DBD_table[,8]=(DBD_table[,6]/DBD_table[,2])*100
  DBD_table[,10]=DBD_table[,4]/particle_density
  DBD_table[,12]=DBD_table[,6]
  DBD_table[,14]=(DBD_table[,12]/(DBD_table[,10]+DBD_table[,12]))
  DBD_table[,16]=(1-DBD_table[,14])*particle_density
  
  #uncertainties
  DBD_table[,7]=sqrt(DBD_table[,3]^2+DBD_table[,5]^2)
  DBD_table[,9]=DBD_table[,8]*sqrt((DBD_table[,7]/DBD_table[,6])^2+(DBD_table[,3]/DBD_table[,2])^2)
  DBD_table[,11]=DBD_table[,10]*sqrt((DBD_table[,5]/DBD_table[,4])^2+(particle_density_uncer/particle_density)^2)
  DBD_table[,13]=DBD_table[,7]
  #verify all uncertainties with Brent, but particularly the following two
  DBD_table[,15]=DBD_table[,14] * (DBD_table[,10]/(DBD_table[,10]+DBD_table[,12])) * sqrt((DBD_table[,11]/DBD_table[,10])^2+(DBD_table[,13]/DBD_table[,12])^2)
  DBD_table[,17]=DBD_table[,16]*sqrt((DBD_table[,15]/(1-DBD_table[,14]))^2+(particle_density_uncer/particle_density)^2)
  
}else{
  DBD_table <- DBD_table[, -c(2,3,6,7,8,9,10,11,12,13), drop = FALSE]
  # insert two new columns after column 4
  new_names <- c("Interval Thickness (cm)", "Ideal Volume (cm^3)", "Ideal Volume Uncertainty (cm^3)")
  
  DBD_table <- cbind(
    DBD_table[, 1:3, drop = FALSE],
    matrix(NA, nrow = nrow(DBD_table), ncol = 3,
           dimnames = list(NULL, new_names)),
    DBD_table[, 4:ncol(DBD_table), drop = FALSE]
  )
  DBD_table[,2:3]=mass_dat[,1:2]
  if(interval_constant==TRUE){
    DBD_table[,4]=interval_thickness
  }else{
    for (i in 1:nrow(DBD_table)){
      DBD_table[i,4]=boundary_depths[i+1]-boundary_depths[i]
    }
  }
  DBD_table[,5]=DBD_table[,4]*surface_area
  DBD_table[,6]=DBD_table[,4]*surface_area_uncer
  DBD_table[,9]=DBD_table[,2]/DBD_table[,5]
  DBD_table[,10]=DBD_table[,9]*sqrt((DBD_table[,6]/DBD_table[,5])^2+(DBD_table[,3]/DBD_table[,2])^2)
  
  if (has_debris==TRUE){
    DBD_table <- DBD_table[, -c(7,8), drop = FALSE]
  }
  has_particle_density = readline(prompt = "Do you have a measured particle density? (True or False) ")
  if (has_particle_density==TRUE){
    particle_density= as.numeric(readline(prompt = "Enter Particle Density (g/cm^3) "))
    particle_density_uncer= as.numeric(readline(prompt = "Enter Particle Density Uncertainty (g/cm^3) "))
  }else{
    particle_density=2.65
    print("default Particle Density used, 2.65 g/cm^3")
    particle_density_uncer=0.01
  }
  if (has_debris==TRUE){
    #don't execute porosity calculations
  }else{
    DBD_table[,7]=1-(DBD_table[,9]/particle_density)
    DBD_table[,8]=DBD_table[,7]*sqrt((particle_density_uncer/particle_density)^2+(DBD_table[,10]/DBD_table[,9])^2)
  }
}

print("select destination folder")
destination_folder <- rstudioapi::selectDirectory("Select Destination Folder")

new_folder_path <- file.path(destination_folder, "DBD results")

# Create the new folder
dir.create(new_folder_path, showWarnings = FALSE)

# Navigate into the new directory
setwd(new_folder_path)
# Save the data frame as a CSV file in the new folder
write.csv(DBD_table, "dbd_results.csv", row.names = FALSE, na = "")

pdf("DBD_plot.pdf", width = 8, height = 6)

DBD_plot=ggplot(DBD_table, aes(x = `Dry Bulk Density (g/cm^3)`, y = `Mid Depth z(i) (cm)`)) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = `Dry Bulk Density (g/cm^3)` - `Dry Bulk Density Uncertainty (g/cm^3)`,
        xmax = `Dry Bulk Density (g/cm^3)` + `Dry Bulk Density Uncertainty (g/cm^3)`),
    height = 0
  ) +
  scale_y_reverse() +
  labs(
    title = "Dry Bulk Density verses Depth",
    x = "DBD (g/cm^3)",
    y = "Mid-Depth (cm)"
  ) +
  theme_minimal()

print(DBD_plot)
dev.off()

if ("Porosity" %in% names(DBD_table)) {
pdf("Porosity_plot.pdf", width = 8, height = 6)

Porosity_plot=ggplot(DBD_table, aes(x = `Porosity`, y = `Mid Depth z(i) (cm)`)) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = `Porosity` - `Porosity Uncertainty`,
        xmax = `Porosity` + `Porosity Uncertainty`),
    height = 0
  ) +
  scale_y_reverse() +
  labs(
    title = "Porosity verses Depth",
    x = "Porosity",
    y = "Mid-Depth (cm)"
  ) +
  theme_minimal()

print(Porosity_plot)
dev.off()
}
save.image(file = "DBD_environment.RData")
rm(list = ls(all.names = TRUE))
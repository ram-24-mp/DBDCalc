# For updates and bug fixes, please see the actively maintained version of DBDCalc:
# https://github.com/ram-24-mp/DBDCalc

# script startup
script_version <- "1.1"

required_packages <- c(
  "jsonlite",
  "readxl",
  "writexl",
  "ggplot2",
  "rstudioapi",
  "zoo"
)

min_versions <- c(
  rlang = "1.1.7",
  vctrs = "0.6.5"
)

get_latest_r_version <- function() {
  urls <- c(
    "https://cran.r-project.org/bin/windows/base/release.htm",
    "https://cran.r-project.org/bin/macosx/",
    "https://cran.r-project.org/"
  )
  
  for (u in urls) {
    txt <- tryCatch(readLines(u, warn = FALSE), error = function(e) NULL)
    if (is.null(txt)) next
    x <- paste(txt, collapse = "\n")
    
    m <- regmatches(
      x,
      gregexpr("\\bR[- ]([0-9]+\\.[0-9]+\\.[0-9]+)\\b", x, perl = TRUE)
    )[[1]]
    
    if (length(m) == 0) next
    
    vers <- sub("^R[- ]", "", m)
    vers <- unique(vers[nzchar(vers)])
    if (length(vers) == 0) next
    
    return(as.character(sort(numeric_version(vers), decreasing = TRUE)[1]))
  }
  
  NA_character_
}

check_r_version_current <- function() {
  current_r <- as.character(getRversion())
  latest_r <- get_latest_r_version()
  
  if (is.na(latest_r) || !nzchar(latest_r)) {
    message("Could not check whether R is up to date.")
    return(invisible(FALSE))
  }
  
  if (utils::compareVersion(current_r, latest_r) < 0) {
    stop(
      paste0(
        "Your R version is out of date (current: ", current_r,
        ", latest: ", latest_r, ").\n",
        "Please update R, restart R, and run the script again."
      ),
      call. = FALSE
    )
  }
  
  message("R is up to date.")
  invisible(TRUE)
}

check_installed_version <- function(pkg, min_version) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  utils::compareVersion(as.character(utils::packageVersion(pkg)), min_version) >= 0
}

install_missing_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(missing) == 0) return(invisible(FALSE))
  
  install.packages(missing, dependencies = TRUE)
  
  stop(
    paste0(
      "Installed missing packages: ",
      paste(missing, collapse = ", "),
      "\nPlease restart R and run the script again."
    ),
    call. = FALSE
  )
}

check_outdated_packages <- function(version_requirements) {
  outdated <- names(version_requirements)[
    !vapply(
      names(version_requirements),
      function(pkg) check_installed_version(pkg, version_requirements[[pkg]]),
      logical(1)
    )
  ]
  
  if (length(outdated) > 0) {
    stop(
      paste0(
        "These packages are too old and must be updated in a fresh R session: ",
        paste(outdated, collapse = ", "),
        "\nRun:\n",
        "install.packages(c(",
        paste(sprintf('"%s"', outdated), collapse = ", "),
        "), dependencies = TRUE)\n",
        "Then restart R and run the script again."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

load_required_packages <- function(pkgs) {
  for (pkg in pkgs) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(TRUE)
}

check_for_script_update <- function(current_version) {
  latest_version <- tryCatch({
    x <- jsonlite::fromJSON(
      "https://api.github.com/repos/ram-24-mp/DBDCalc/releases/latest"
    )
    sub("^v", "", trimws(x$tag_name))
  }, error = function(e) NA_character_)
  
  if (is.na(latest_version) || latest_version == "") {
    message("Could not check for script updates.")
    return(invisible(FALSE))
  }
  
  if (utils::compareVersion(latest_version, current_version) > 0) {
    message(sprintf(
      "A newer version is available: %s (current: %s)",
      latest_version, current_version
    ))
    return(invisible(TRUE))
  }
  
  message("Script is up to date.")
  invisible(FALSE)
}

check_r_version_current()

if ("rlang" %in% loadedNamespaces()) {
  loaded_rlang <- as.character(getNamespaceVersion("rlang"))
  if (utils::compareVersion(loaded_rlang, min_versions[["rlang"]]) < 0) {
    stop(
      paste0(
        "An outdated 'rlang' namespace is already loaded in this R session (",
        loaded_rlang,
        ").\n",
        "Restart R before running this script.\n",
        "Do not source this script again in the same session after updating packages.\n",
        "Run install.packages(c(\"rlang\", \"vctrs\", \"ggplot2\"), dependencies = TRUE)"
      ),
      call. = FALSE
    )
  }
}

install_missing_packages(required_packages)
check_outdated_packages(min_versions)
load_required_packages(required_packages)
check_for_script_update(script_version)

rm(list = ls(all.names = TRUE))

interval_constant = readline(prompt = "Is your interval thickness constant (True or False) ")

# core dimensions, in cm
if (interval_constant == TRUE) {
  print ("Diameter 10.0 +/- 0.1 cm")
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
  print ("Diameter 10.0 +/- 0.1 cm")
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
mass_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric", .name_repair = "unique_quiet")

wet_weights = readline(prompt = "Do you have both wet and dry sediment weights? (True or False) ")
DBD_table <- as.data.frame(DBD_table, stringsAsFactors = FALSE)
if (wet_weights ==TRUE){
  constant_density = readline(prompt = "Do you assume constant particle density? (True or False) ")
  if (constant_density==TRUE){
  has_particle_density = readline(prompt = "Do you have a measured particle density? (True or False) ")
  if (has_particle_density==TRUE){
    particle_density= as.numeric(readline(prompt = "Enter Particle Density (g/cm^3) "))
    particle_density_uncer= as.numeric(readline(prompt = "Enter Particle Density Uncertainty (g/cm^3) "))
  }else{
    has_clay = readline(prompt = "Do you have a weight percent clay and organic matter? (True or False) ")
    if (has_clay==TRUE){
      clay_per=as.numeric(readline(prompt = "Enter Clay Percentage (%) "))
      OM_per=as.numeric(readline(prompt = "Enter OM Percentage (%) "))
      particle_density=2.652+0.216*(clay_per/100)-2.237*(OM_per/100)
      particle_density_uncer=0.041
    }
    particle_density=2.65
    print("default Particle Density used, 2.65 g/cm^3")
    particle_density_uncer=0.1
  }
  }else{
    has_particle_density = readline(prompt = "Do you have measured particle densities? (True or False) ")
    if (has_particle_density==TRUE){
      print("select particle density data")
      file_path=file.choose()
      density_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric", .name_repair = "unique_quiet")
      DBD_table=cbind(DBD_table, "Particle Density (g/cm^3)", "Particle Density Uncertainty (g/cm^3)")
      DBD_table[,18]=density_dat[,1]
      DBD_table[,19]=density_dat[,2]
    }else{
      print("select clay and organic matter data")
      file_path=file.choose()
      clay_OM_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric", .name_repair = "unique_quiet")
      DBD_table=cbind(DBD_table, "Particle Density (g/cm^3)", "Particle Density Uncertainty (g/cm^3)", "Clay Weight Percent (%)", "OM Weight Percent (%)")
      DBD_table[,20]=density_dat[,1]
      DBD_table[,21]=density_dat[,2]
      DBD_table[,18]=2.652+0.216*(DBD_table[,20]/100)-2.237*(DBD_table[,21]/100)
      DBD_table[,19]=0.041
    }
  }

  DBD_table[,2:5]=mass_dat[,1:4]
  DBD_table[,6]=DBD_table[,2]-DBD_table[,4]
  DBD_table[,8]=(DBD_table[,6]/DBD_table[,2])*100
  if (constant_density==TRUE){
  DBD_table[,10]=DBD_table[,4]/particle_density
  }else{
    DBD_table[,10]=DBD_table[,4]/DBD_table[,18]
  }
  DBD_table[,12]=DBD_table[,6]
  DBD_table[,14]=(DBD_table[,12]/(DBD_table[,10]+DBD_table[,12]))
  if (constant_density==TRUE){
    DBD_table[,16]=(1-DBD_table[,14])*particle_density
  }else{
    DBD_table[,16]=(1-DBD_table[,14])*DBD_table[,18]
  }
  
  
  #uncertainties
  DBD_table[,7]=sqrt(DBD_table[,3]^2+DBD_table[,5]^2)
  DBD_table[,9]=DBD_table[,8]*sqrt((DBD_table[,7]/DBD_table[,6])^2+(DBD_table[,3]/DBD_table[,2])^2)
  if (constant_density==TRUE){
  DBD_table[,11]=DBD_table[,10]*sqrt((DBD_table[,5]/DBD_table[,4])^2+(particle_density_uncer/particle_density)^2)
  }else{
    DBD_table[,11]=DBD_table[,10]*sqrt((DBD_table[,5]/DBD_table[,4])^2+(DBD_table[,19]/DBD_table[,18])^2)
  }
  DBD_table[,13]=DBD_table[,7]
  #verify all uncertainties with Brent, but particularly the following two
  DBD_table[,15]=DBD_table[,14] * (DBD_table[,10]/(DBD_table[,10]+DBD_table[,12])) * sqrt((DBD_table[,11]/DBD_table[,10])^2+(DBD_table[,13]/DBD_table[,12])^2)
  if (constant_density==TRUE){
  DBD_table[,17]=DBD_table[,16]*sqrt((DBD_table[,15]/(1-DBD_table[,14]))^2+(particle_density_uncer/particle_density)^2)
  }else{
    DBD_table[,17]=DBD_table[,16]*sqrt((DBD_table[,15]/(1-DBD_table[,14]))^2+(DBD_table[,19]/DBD_table[,18])^2)
  }
  
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
  
  constant_density = readline(prompt = "Do you assume constant particle density? (True or False) ")
  if (constant_density==TRUE){
  has_particle_density = readline(prompt = "Do you have a measured particle density? (True or False) ")
  if (has_particle_density==TRUE){
    particle_density= as.numeric(readline(prompt = "Enter Particle Density (g/cm^3) "))
    particle_density_uncer= as.numeric(readline(prompt = "Enter Particle Density Uncertainty (g/cm^3) "))
  }else{
    has_clay = readline(prompt = "Do you have a weight percent clay and organic matter? (True or False) ")
    if (has_clay==TRUE){
      clay_per=as.numeric(readline(prompt = "Enter Clay Percentage (%) "))
      OM_per=as.numeric(readline(prompt = "Enter OM Percentage (%) "))
      particle_density=2.652+0.216*(clay_per/100)-2.237*(OM_per/100)
      particle_density_uncer=0.041
    }
    particle_density=2.65
    print("default Particle Density used, 2.65 g/cm^3")
    particle_density_uncer=0.1
  }
  }else{
    has_particle_density = readline(prompt = "Do you have measured particle densities? (True or False) ")
    if (has_particle_density==TRUE){
      DBD_table=cbind(DBD_table, "Particle Density (g/cm^3)", "Particle Density Uncertainty (g/cm^3)")
      print("select particle density data")
      file_path=file.choose()
      density_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric", .name_repair = "unique_quiet")
      DBD_table[,11]=density_dat[,1]
      DBD_table[,12]=density_dat[,2]
    }else{
      print("select clay and organic matter data")
      file_path=file.choose()
      clay_OM_dat=read_excel(file_path, col_names = TRUE, col_types = "numeric", .name_repair = "unique_quiet")
      DBD_table=cbind(DBD_table, "Particle Density (g/cm^3)", "Particle Density Uncertainty (g/cm^3)", "Clay Weight Percent (%)", "OM Weight Percent (%)")
      DBD_table[,13]=density_dat[,1]
      DBD_table[,14]=density_dat[,2]
      DBD_table[,11]=DBD_table[,18]=2.652+0.216*(DBD_table[,13]/100)-2.237*(DBD_table[,14]/100)
      DBD_table[,12]=0.041
    }
  }
  if (constant_density==TRUE){
    DBD_table[,7]=1-(DBD_table[,9]/particle_density)
    DBD_table[,8]=DBD_table[,7]*sqrt((particle_density_uncer/particle_density)^2+(DBD_table[,10]/DBD_table[,9])^2)
  }else{
    DBD_table[,7]=1-(DBD_table[,9]/DBD_table[,11])
    DBD_table[,8]=DBD_table[,7]*sqrt((DBD_table[,12]/DBD_table[,11])^2+(DBD_table[,10]/DBD_table[,9])^2)
    }
  }

# Fill missing uncertainties by bracketing nearest known uncertainty values
propagate_uncertainty_linear <- function(u) {
  n <- length(u)
  for (i in seq_len(n)) {
    if (is.na(u[i])) {
      left_index <- i - 1
      while (left_index >= 1 && is.na(u[left_index])) {
        left_index <- left_index - 1
      }
      
      right_index <- i + 1
      while (right_index <= n && is.na(u[right_index])) {
        right_index <- right_index + 1
      }
      
      # need valid brackets on BOTH sides
      if (left_index >= 1 && right_index <= n) {
        delta_y1 <- u[left_index]
        delta_y2 <- u[right_index]
        x1 <- left_index
        x2 <- right_index
        
        w <- (i - x1) / (x2 - x1)  # same as your partial_der
        u[i] <- sqrt(((1 - w) * delta_y1)^2 + (w * delta_y2)^2)
      }
    }
  }
  u
}

# ---- main logic ----
if ("Dry Bulk Density (g/cm^3)" %in% names(DBD_table) &&
    anyNA(DBD_table$`Dry Bulk Density (g/cm^3)`)) {
  
  ans <- tolower(trimws(readline(
    prompt = "Do you want to interpolate missing DBD and Porosity values? (True or False) "
  )))
  do_it <- isTRUE(as.logical(ans))
  
  if (do_it) {
    
    # ---- DBD ----
    if ("Dry Bulk Density Uncertainty (g/cm^3)" %in% names(DBD_table)) {
      DBD_table$`Dry Bulk Density Uncertainty (g/cm^3)` <-
        propagate_uncertainty_linear(DBD_table$`Dry Bulk Density Uncertainty (g/cm^3)`)
    }
    
    DBD_table$`Dry Bulk Density (g/cm^3)` <-
      na.approx(DBD_table$`Dry Bulk Density (g/cm^3)`, na.rm = FALSE)
    
    # ---- Porosity (only if columns exist) ----
    if ("Porosity" %in% names(DBD_table)) {
      
      if ("Porosity Uncertainty" %in% names(DBD_table)) {
        DBD_table$`Porosity Uncertainty` <-
          propagate_uncertainty_linear(DBD_table$`Porosity Uncertainty`)
      }
      
      DBD_table$Porosity <- na.approx(DBD_table$Porosity, na.rm = FALSE)
    }
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
writexl::write_xlsx(DBD_table, "dbd_results.xlsx")

pdf("DBD_plot.pdf", width = 8, height = 6)

DBD_plot=ggplot(DBD_table, aes(x = `Dry Bulk Density (g/cm^3)`, y = `Mid Depth z(i) (cm)`)) +
  geom_point() +
  geom_errorbar(
    aes(xmin = `Dry Bulk Density (g/cm^3)` - `Dry Bulk Density Uncertainty (g/cm^3)`,
        xmax = `Dry Bulk Density (g/cm^3)` + `Dry Bulk Density Uncertainty (g/cm^3)`),
    width = 0,
    orientation = "y"
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
  geom_errorbar(
    aes(xmin = `Porosity` - `Porosity Uncertainty`,
        xmax = `Porosity` + `Porosity Uncertainty`),
    width = 0,
    orientation = "y"
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

constants_table <- data.frame(
  constant = c(
    "core diameter (cm)",
    "core diameter uncertainty (cm)",
    "surface area (cm^2)",
    "surface area uncertainty (cm^2)",
    "interval thickness (cm)",
    "full core length (cm)",
    "particle density (g/cm^3)",
    "particle density uncertainty (g/cm^3)"
  ),
  value = I(vector("list", 8)),   # list-column
  stringsAsFactors = FALSE
)

# 2) Fill in scalar entries as 1-length numerics
constants_table$value[[1]]  <- internal_diameter
constants_table$value[[2]]  <- internal_diameter_uncer
constants_table$value[[3]]  <- surface_area
constants_table$value[[4]]  <- surface_area_uncer

# 3) This row can now hold a numeric vector (float list)
if (exists("interval_thicknesses_list")) {
  constants_table$value[[5]]  <- interval_thicknesses_list   # vector allowed here
} else {
  constants_table$value[[5]]  <- interval_thickness
}

constants_table$value[[6]] <- core_length
if (constant_density==TRUE){
  constants_table$value[[7]] <- particle_density
  constants_table$value[[8]] <- particle_density_uncer
}else{
  constants_table$value[[7]] <- "variable"
  constants_table$value[[8]] <- "variable"
}


# 4) Make a CSV-friendly copy (so write.csv still works cleanly)
constants_table_csv <- constants_table

constants_table_csv$value <- vapply(
  constants_table$value,
  function(x) {
    # x might be a numeric, or a list element containing a numeric/vector
    if (is.list(x)) {
      x <- unlist(x, recursive = FALSE, use.names = FALSE)
    }
    # Now x is atomic: length 1 (scalar) or >1 (vector)
    if (length(x) <= 1) {
      as.character(x)
    } else {
      paste(x, collapse = ";")  # works for either case
    }
  },
  character(1)
)

writexl::write_xlsx(constants_table_csv, "constants_table.xlsx")


save.image(file = "DBD_environment.RData")

rm(list = ls(all.names = TRUE))

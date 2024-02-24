#' @export

new_project <- function(location, year, prefix = NULL, location_abbreviation = NULL, source_root){

  # Create project name
  if(is.null(location_abbreviation)){
    location_abbreviation <- tolower(gsub("\\s", "", location))
  }
  project_name <- paste0(prefix, location_abbreviation, year)

  # Settings
  source_sub_dirs <- c("data_final", "data_intermediate", "data_manual", "data_raw", "R")
  project_sub_dirs<- c("data_final", "data_manual", "R")

  cat(
    "CREATING NEW PROJECT", "\n\n",
    "  Location     : ", location, "\n",
    "  Year         : ", year, "\n",
    "  Project name : ", project_name, "\n\n",
    sep=""
  )

  # Get options
  syncback_exe <- getOption("misterRS.syncback_exe")
  if(is.null(syncback_exe)) stop("Could not find Syncback executable")

  projects_root <- getOption("misterRS.projects_root")
  if(is.null(projects_root)) stop("Could not find project root directory")

  # Get project log
  project_register_path <- getOption("misterRS.project_register")
  project_register <- read.csv(project_register_path)
  project_register[["created"]] %<>% as.POSIXct()

  if(project_name %in% project_register[["project"]]){

    cat("Project has already been created")

  }else{

    # Last project
    last_project <- project_register[["project"]][which.max(project_register[["created"]])]
    last_project_dir <- file.path(projects_root, last_project)
    cat("  Last project used as template: ", last_project, "\n\n", sep = "")
    if(!dir.exists(last_project_dir)) stop("Could not find folder for last project: ", last_project_dir)

    # Last QGIS file
    last_project_qgz <- file.path(last_project_dir, paste0(last_project, ".qgz"))
    if(!file.exists(last_project_qgz)) stop("Could not find QGIS project for last project: ", last_project_qgz)

    # Last R files
    last_project_R_files <- .R_project_files(last_project_dir, last_project)
    last_project_Rproj <- file.path(last_project_dir, paste0(last_project, ".Rproj"))
    if(!all(file.exists(last_project_R_files))) stop("Missing R files for last project:", last_project_dir)
    if(!all(file.exists(last_project_Rproj))) stop("Missing Rproj file for last project:", last_project_dir)

    # Make directories
    source_dir <- file.path(source_root, project_name)
    project_dir <- file.path(projects_root, project_name)

    cat("  Creating source directories", "\n    ",
        source_dir, "\n", sep="")

    dir.create(source_dir, showWarnings = FALSE)
    for(source_sub_dir in source_sub_dirs){
      cat("    ... ", source_sub_dir, "\n", sep = "")
      dir.create(file.path(source_dir, source_sub_dir), showWarnings = FALSE)
    }
    cat("\n")

    cat("  Creating project directories", "\n    ",
        project_dir, "\n", sep="")
    dir.create(project_dir, showWarnings = FALSE)
    for(project_sub_dir in project_sub_dirs){
      cat("    ... ", project_sub_dir, "\n", sep = "")
      dir.create(file.path(project_dir, project_sub_dir), showWarnings = FALSE)
    }
    cat("\n")

    cat("  Copying QGIS file", "\n")
    new_project_qgz <-  file.path(source_dir, paste0(project_name, ".qgz"))
    file.copy(last_project_qgz, new_project_qgz)

    cat("  Copying R files", "\n")
    new_project_R_files <- .R_project_files(source_dir, project_name)
    new_project_Rproj <- file.path(source_dir, paste0(project_name, ".Rproj"))
    file.copy(last_project_R_files, new_project_R_files)
    file.copy(last_project_Rproj, new_project_R_files)

    cat("  Creating scratch file", "\n")
    new_scratch_file <- file.path(source_dir, "R", paste0(project_name, "_0_scratch.R"))
    .create_scratch(new_scratch_file, source_dir, project_name)

    cat("  Creating log file", "\n")
    new_project_log <- file.path(source_dir, paste0(project_name, ".md"))
    .create_log(new_project_log, location, year)

    # Add project to log
    cat("  Updating project register", "\n")
    project_register %<>% rbind(data.frame(project = project_name, created = Sys.Date()))

    write.csv(project_register, project_register_path, row.names = FALSE)

    # Launching Syncback
    cat("  Launching Syncback", "\n")
    shell(shQuote(syncback_exe), wait = FALSE)

    cat("  Launching QGIS", "\n")
    shell(paste("start", shQuote(""), shQuote(new_project_qgz)), wait = FALSE)

    cat("  Launching R", "\n")
    shell(paste("start", shQuote(""), shQuote(new_project_Rproj)), wait = FALSE)
    cat("\n")

    cat("  Reminders:", "\n",
        "    ✓ Syncback: update folder paths and run a test", "\n",
        "    ✓ QGIS project: change projection system", "\n",
        "    ✓ R files: check for special cases that applied to last project", "\n",
        "\n",
        "FINISHED",
      sep = ""
    )
  }
}

.R_project_files <- function(dir, project_name){

  r_file_suffixes <- c("_1_variables.R", "_2_paths.R", "_3_data.R", "_4_analysis.R")

  file.path(dir, c( paste0("R/", project_name, r_file_suffixes )))

}

.create_log <- function(log_path, location, year){

  # file.create(log_path)
  #
  # fileConn<-file(log_path)
  # writeLines(c(
  #   paste("#", location, year),
  #   "",
  #   paste("##", Sys.Date()),
  #   "",
  #   "Project created"
  #
  # ), fileConn)
  # close(fileConn)

}

.create_scratch <- function(scratch_path, project_dir, project_name){

  file.create(scratch_path)

  fileConn<-file(scratch_path)
    writeLines(c(
    'rm(list = ls(all.names = TRUE))',
    '',
    'setwd("D:/Projects/libraries/misterRS")',
    'devtools::load_all()',
    '',
    '# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
    '',
    paste0('setwd("',project_dir,'")'),
    paste0('source("R/',project_name,'_1_variables.R")'),
    paste0('source("R/',project_name,'_2_paths.R")'),
    paste0('source("R/',project_name,'_3_data.R")')
  ), fileConn)
  close(fileConn)
}

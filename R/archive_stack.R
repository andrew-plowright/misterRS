#' @export

archive_stack <- function(rsds_list, prompt = TRUE, verbose = getOption('misterRS.verbose')){

  if(verbose) cat("ARCHIVING STACK FILES\n\n")

  if(prompt){

    # Get longest ID length (for printing)
    rsds_ids <- sapply(rsds_list, slot, "id")
    rsds_ids_maxchar <- max(nchar(rsds_ids))

    # Create printable lines for archiving preview
    preview <- lapply(rsds_list, function(rs){

      id_name <- paste(stringr::str_pad(rs@id, rsds_ids_maxchar, "right"), ":", crayon::italic(rs@name))
      if(rs@archive){
        crayon::green(paste(" ARCHIVE", id_name))
      }else{
        crayon::red(  paste(" DELETE ", id_name))
      }
    })

    # Print preview of archiving operation
    for(preview_line in preview) cat(preview_line, "\n")

    # Get user confirmation
    user_input <- readline("Confirm? (y) or (n): ")
    if(tolower(user_input) != "y") stop("Canceled archiving", call. = FALSE)

    if(verbose) cat("\n")
  }

  for(i in 1:length(rsds_list)){
    rs <- rsds_list[[i]]

    if(verbose) cat(rs@id, "\n")

    # Delete mosaic
    mosaic_paths <- .ras_aux_files(.rsds_mosaic_path(rs))

    if(length(mosaic_paths) > 1){

      if(verbose) cat("... deleting mosaic\n")
      unlink(mosaic_paths)
    }

    # Delete pyramids
    pyramid_paths <- gsub(rs@ext, "ovr", .rsds_tile_paths(rs))
    pyramid_paths <- pyramid_paths[file.exists(pyramid_paths)]

    if(length(pyramid_paths) > 1){

      if(verbose) cat("... deleting pyramids\n")
      unlink(pyramid_paths)
    }

    # Delete entire dataset
    if(!rs@archive){

      if(verbose) cat("... deleting dataset\n")
      unlink(rs@dir, recursive = TRUE)
    }
  }

  if(verbose) cat("COMPLETE\n")
}


.ras_aux_files <- function(ras_path){

  #ras_path_base <- tools::file_path_sans_ext(ras_path)

  aux_paths <- c(
    ras_path,
    paste0(ras_path, ".ovr"),
    paste0(ras_path, ".aux.xml")
  )

  return(aux_paths[file.exists(aux_paths)])
}

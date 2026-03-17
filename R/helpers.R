
# description -------------------------------------------------------------



# 1. loading all files with specific file extension into workspace --------

load_files_to_env <- function(path, file_ext) {
  
  # Remove leading dot if provided
  file_ext <- sub("^\\.", "", file_ext)
  
  # Map file extensions to read functions
  read_func <- switch(file_ext,
                      "gpkg" = sf::st_read,
                      "csv"  = readr::read_csv,
                      "tsv"  = readr::read_tsv,
                      "xlsx" = readxl::read_excel,
                      "xls"  = readxl::read_excel,
                      "json" = jsonlite::fromJSON,
                      stop("Unsupported file extension: '", file_ext, "'. Supported: gpkg, csv, tsv, xlsx, xls, json")
  )
  
  # List files with extension
  files <- list.files(path, pattern = paste0("\\.", file_ext, "$"), full.names = TRUE)
  
  if (length(files) == 0) {
    message("No .", file_ext, " files found in: ", path)
    return(invisible(NULL))
  }
  
  # Set names (filename without extension)
  names(files) <- tools::file_path_sans_ext(basename(files))
  
  # Load each file into global environment
  purrr::walk2(files, names(files), ~assign(.y, read_func(.x), envir = .GlobalEnv))
  
  message("Loaded ", length(files), " object(s): ", paste(names(files), collapse = ", "))
  invisible(names(files))
}


# 2. remove double cols of etn df -----------------------------------------

remove_double_cols <- function(animals){
  animals$tag_serial_number <- gsub(",.*","", animals$tag_serial_number)
  animals$tag_type <- gsub(",.*","", animals$tag_type)
  animals$tag_subtype <- gsub(",.*","", animals$tag_subtype)
  animals$acoustic_tag_id <- gsub(",.*","", animals$acoustic_tag_id)
  return(animals)
}

# 3. save .rds file -------------------------------------------------------


save_data <- function(data, folder){
  base::saveRDS(data, file = paste0(folder, "/" , deparse(substitute(data)), ".rds"))
}

# 4. load .rds file -------------------------------------------------------


load_data <- function(filestring, folder){
  data <- base::readRDS(file = paste0(folder, "/", filestring, ".rds"))
  return(data)
}


# 5. save ggplot ----------------------------------------------------------

save_plot <- function(plot, folder, file_ext = "pdf", height = 15, width = 18, units = "cm"){
  ggplot2::ggsave(filename = paste0(folder, "/", deparse(substitute(plot)),".", file_ext),
                  height = height, width = width, units = units)
}

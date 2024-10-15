# ----
# CC BY-SA, Eric Mossotti
# ----
# Description ----
# 
# Downloads files from the cloud, calls the file relocating function and cleans
# up the environment.
# 
# ----
source("Scripts/unz_relocate.R")

dl_toDirectory <- function (
        durls,
        tmpzip_dir,
        tmpfile_dir,
        tmpzip_paths,
        tmpfile_paths,
        tmpfile_names
        ) {

  # Create a directory to store the temporary files
  dir.create(tmpzip_dir)
  
  # A simple way to download and relocate those files from the working directory to the file-folder paths created earlier.
  curl::multi_download(durls, 
                       destfiles = tmpzip_paths)
  
  # Create tempFile directory
  dir.create(tmpfile_dir)
  
  # Execute sourced custom function from the unz_relocate.R file
  unz_relocate(fPaths = tmpfile_paths,
               zPaths = tmpzip_paths,
               fNames = tmpfile_names)
  
  # To remove the directory and contents thereof after having finished using
  unlink(tmpzip_dir, 
         recursive = TRUE)
  
}

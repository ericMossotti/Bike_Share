# ----
# Author: Eric Mossotti
# ----
# Helpful because this unzips only the desired files contained inside compressed 
# folders. The unzipped files are then relocated to user defined file paths.
# 
# There might well be existing libraries and methods in R that can do this, 
# but I was not able to readily locate them. 
# ----
unz_relocate <- function (fPaths = tempfile_paths,
                          zPaths = tempZipPaths,
                          fNames = fileNames) {
    for (i in seq(fPaths)) {
        utils::unzip(zPaths[i],
                     fNames[i])
        file.rename(fNames[i],
                    fPaths[i])
    }
}
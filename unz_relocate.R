unz_relocate <- function (x = tempfile_paths,
                          y = tempZipPaths,
                          z = fileNames) {
    for (i in seq(x)) {
        utils::unzip(y[i],
                     z[i])
        file.rename(z[i],
                    x[i])
    }
}
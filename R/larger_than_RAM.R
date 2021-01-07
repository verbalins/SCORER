# Testing handling of larger datasets

disk_frame <- function() {
  disk.frame::setup_disk.frame()
  options(future.globals.maxSize = Inf)

  df <- disk.frame::as.disk.frame(
    FSMJ,
    outdir = file.path(tempdir(), "tmp_FSMJ.df"),
    overwrite = TRUE)

  #NbClust::NbClust(df, diss = null, distance="euclidean", method = "kmeans")
}

monetDB <- function() {


}

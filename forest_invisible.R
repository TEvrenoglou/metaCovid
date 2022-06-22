forest.invisible <- function(...){
  ff <- tempfile()
  grDevices::png(filename=ff)
  res <- metafor::forest(...)
  grDevices::dev.off()
  unlink(ff)
  res
}
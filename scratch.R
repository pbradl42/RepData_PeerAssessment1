testfun <- function(needle, haystack, target) {
  which(colnames(needle)=="steps") -> stepscol
  message(stepscol)
  which( colnames(haystack)=="interval" ) -> intervalcol
  message(which(haystack[, 1] == needle))
  haystack[which(haystack[, intervalcol] == needle[,stepscol]), ]$x -> toReturn
  return(toReturn[])
}
simplefun <- function(biglist) {
  class(biglist)
#  which(colnames(biglist)=="steps") -> stepscol
#  which(colnames(biglist)=="interval") -> intervalcol
#  paste(biglist[, 1], " and ", biglist[, 2], sep = "")
}
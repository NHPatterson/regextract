#'@title Return unique xy identifier
#'@param cardinaldata MSImageSet for data analysis
#'@return character vector with unique XY identifier for matching
genUniqueXYcardinal <- function(cardinaldata){
  xy_card = paste0("X", coord(cardinaldata)$x, "Y", coord(cardinaldata)$y)
  return(xy_card)
}

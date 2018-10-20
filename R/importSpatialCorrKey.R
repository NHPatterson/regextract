#'@export
#'@title Import Spatial Correlation IMS key
#'@description Reads spatial correlation IMS key as data.frame and thresholds pixels below a certain overlap percentage
#'@param filepath path to the spatial correlation IMS key .csv
#'@param overlap_thresh overlap percentage threshold between 0 and 1, set to 0 to load in all
#'@return data.frame of thresholded spatial correlation key

importSpatialCorrKey <- function(filepath, overlap_thresh = 0.5){
  spatialCorrKey <- read.csv(filepath)
  spatialCorrKey <- spatialCorrKey[spatialCorrKey$percentage >= overlap_thresh,]
  return(spatialCorrKey)
}

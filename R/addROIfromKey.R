#'@export
#'@title Add ROIs generated from RegToolboxMSRC's IMS processing
#'@description Adds ROIs to MSImageSet, ROIs are added in the pixelData of the MSImageSet as three vectors. First, the ROI is combined with it's index and name to make a unique identified that is added a logical vector where TRUE pixels are in the ROI regardless of degree of overlap. Another vector is added for the ROI per-pixel overlap where values of 0 indicate no overlap, and values of 1 indicate full overlap. Finally, the integer index of the ROI can be accessed by the '_roi_index' appended vector.
#'
#'@param cardinaldata MSImageSet to which ROIs are added
#'@param ROI_key data.frame imported with importSpatialCorrKey
#'@param overlap_thresh numeric overlap percentage threshold between 0 and 1, set to 0 to load in all
#'@param xy_min logical whether the MSImageSet coordinates are reduced (i.e. min x and y coordinates are equal to 1) or in their original configuration
#'@param show_images logical plot binary masks of each imported ROI (takes longer to process)
#'@return MSImageSet with ROIs are added
#'
#'

addROIfromKey <- function (cardinaldata, ROI_key, overlap_thresh = 0.5, xy_min = F,
                            show_images = F)
{
  #subset ROI key
  threshold_selection <- which(ROI_key$percentage > overlap_thresh)
  threshold_selection <- ROI_key[threshold_selection, ]
  
  #give unique identifier to each ROI
  threshold_selection$roi_name <- gsub(".roi", "", threshold_selection$roi_name)
  threshold_selection$roi_name <- gsub("-", "_", threshold_selection$roi_name)
  
  #get unique XY coordinates
  xy_cardinal <- genUniqueXYcardinal(cardinaldata)
  
  #add an identifier to subset data.frame along
  threshold_selection$unique_rois <- paste0("idx", formatC(threshold_selection$roi_index,
                                                           width = 3, flag = "0"), "_", threshold_selection$roi_name)
  #add ROI metadata
  for (unique_roi in unique(threshold_selection$unique_rois)) {
    print(paste0("Importing ROI for : ", unique_roi))
    
    #subset position data.frame for a given ROI
    extraction_roi <- threshold_selection[threshold_selection$unique_rois ==
                                            unique_roi, ]
    
    #how IMS canvas is organized: still in native X,Y coords (F) or
    #XY minimized coords- lowest X,Y coord is 1,1
    if (xy_min == T) {
      xy_roi <- paste0("X", extraction_roi$x_minimized,
                       "Y", extraction_roi$y_minimized)
    } else {
      xy_roi <- paste0("X", extraction_roi$x_original,
                       "Y", extraction_roi$y_original)
    }
    
    #match positions
    roi_indices <- match(xy_roi, xy_cardinal)
    
    #remove empty stuff
    rm_indices = which(is.na(roi_indices) == T)
    if (length(rm_indices) > 0) {
      roi_indices <- roi_indices[-rm_indices]
      extraction_roi <- extraction_roi[-rm_indices, ]
    }
    
    #add metadata is ROI actually exists
    if(nrow(extraction_roi) > 0){
      #initialize logical vector with ROI name as FALSE
      pData(cardinaldata)[unique_roi] = FALSE
      
      #change to TRUE where ROI is present
      pData(cardinaldata)[roi_indices, unique_roi] = TRUE
      
      #add a vector with the ROI index (1 - n_rois), where ROI is not present it is NA
      pData(cardinaldata)[paste0(unique_roi, "_roi_index")] <- unique(extraction_roi$roi_index)
      
      #overlap value for each pixel based on exact microscopy registration
      #initialized to zero
      pData(cardinaldata)[paste0(unique_roi, "_overlap")] = 0
      
      #add overlap percentage from ROI key where ROI is present
      pData(cardinaldata)[roi_indices, paste0(unique_roi, "_overlap")] = extraction_roi$percentage
      if (show_images == T)
        image(cardinaldata, pData(cardinaldata)[unique_roi][,
                                                            1] ~ x * y, strip = F, main = paste(unique_roi,
                                                                                                ": selection in red"))
    }else{
      print(paste0(unique_roi,' does not intersect with the IMS data'))
    }
  }
  
  return(cardinaldata)
}

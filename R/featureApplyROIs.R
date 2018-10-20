#'@export
#'@title featureApply over imported ROIs
#'@description Applies weighted and non-weighted functions to each ROI, generating a data.frame of ROIs vs intensity
#'@param cardinaldata MSImageSet for data analysis
#'@param func str func to be passed. Must be in namespace or a weighted option (weighted.means,weighted.sds,weighted.medians,weighted.vars,weighted.MADs). Weighted means in this case are weighted by the ROI's overlap value
#'@return data.frame of ROIs & applied function values

featureApplyROIs <- function(cardinaldata, func = 'weighted.mean'){
  #get ROIs
  pdata_colnames <- dimnames(pData(cardinaldata))[[2]]
  pdata_colnames <- pdata_colnames[grepl('idx',pdata_colnames)]
  if(length(pdata_colnames) == 0){
    stop(paste0('No ROIs found in \'',deparse(substitute(cardinaldata)),'\''))
    
  }
  pdata_rois <- pdata_colnames[!grepl('_overlap',pdata_colnames)]
  pdata_rois <- pdata_rois[!grepl('_roi_index',pdata_rois)]
  
  #determine if ROIs are empty, don't iterate over the ones that are empty
  pdata_sums <- c()
  for(idx in 1:length(pdata_rois)){
    weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
    pdata_sums <- c(pdata_sums,sum(weights))
  }
  
  if(length(which(pdata_sums > 0)) == 0 ){
    stop(paste0('No ROIs found in \'',deparse(substitute(cardinaldata)),'\''))
  }else{
    pdata_rois <- pdata_rois[which(pdata_sums > 0)]
  }
    
  #loop through ROIs and get return data
  
  if(func %in% c('weighted.means','weighted.sds','weighted.medians','weighted.vars','weighted.MADs') == T | exists(func) == T){
    out_ROIs <- list()
    for(idx in 1:length(pdata_rois)){
      
      if(func == 'weighted.means'){
        int_mat <- t(iData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]]))
        weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
        weighted_mean <- colWeightedMeans(int_mat, weights)
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], t(weighted_mean))
        
      }
      
      if(func == 'weighted.sds'){
        int_mat <- t(iData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]]))
        weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
        weighted_sds <- colWeightedSds(int_mat, weights)
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], t(weighted_sds))
      }
      
      if(func == 'weighted.medians'){
        int_mat <- t(iData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]]))
        weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
        weighted_medians <- colWeightedMedians(int_mat, weights)
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], t(weighted_medians))
      }
      
      if(func == 'weighted.vars'){
        int_mat <- t(iData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]]))
        weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
        weighted_var <- colWeightedVars(int_mat, weights)
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], t(weighted_var))
      }
      
      if(func == 'weighted.MADs'){
        int_mat <- t(iData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]]))
        weights <- pData(cardinaldata[,pData(cardinaldata)[pdata_rois[idx]][,1]])[paste0(pdata_rois[idx],'_overlap')][,1]
        weighted_mad <- colWeightedMads(int_mat, weights)
        
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], t(weighted_mad))
      }
      
      
      if(grepl('weighted',func) == F){
        out_ROIs[[idx]] <- data.frame(ROI = pdata_rois[idx], intensity = t(featureApply(cardinaldata, .pixel = pData(cardinaldata)[pdata_rois[idx]][,1],.fun=eval(parse(text=func)))))
      }
    }
    out_df <- data.frame(do.call(rbind, out_ROIs))
    colnames(out_df) <- c('ROI',round(Cardinal::mz(cardinaldata),3))
    return(out_df)
  }else{
    stop(paste0('Function \'',func,'\' not recognized'))
  }
}

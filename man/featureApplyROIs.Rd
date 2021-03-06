% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featureApplyROIs.R
\name{featureApplyROIs}
\alias{featureApplyROIs}
\title{featureApply over imported ROIs}
\usage{
featureApplyROIs(cardinaldata, func = "weighted.mean")
}
\arguments{
\item{cardinaldata}{MSImageSet for data analysis}

\item{func}{str func to be passed. Must be in namespace or a weighted option (weighted.means,weighted.sds,weighted.medians,weighted.vars,weighted.MADs). Weighted means in this case are weighted by the ROI's overlap value}
}
\value{
data.frame of ROIs & applied function values
}
\description{
Applies weighted and non-weighted functions to each ROI, generating a data.frame of ROIs vs intensity
}

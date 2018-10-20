# regextract package

This package provides integration of the [regToolboxMSRC](https://github.com/nhpatterson/regToolboxMSRC) outputs into Cardinal for IMS data analysis. 

## What it does
Specifically, this package allows one to load annotations made on microscopy images registered to IMS through the explicit pixel-to-laser-ablation mark workflow outlined in [Advanced Registration and Analysis of MALDI Imaging Mass Spectrometry Measurements](https://pubs.acs.org/doi/10.1021/acs.analchem.8b02884). Following loading of the annotations, the ```featureApplyROIs``` function performs statistical analysis, including weighting by pixel-ROI overlap, on each ROI and returns data tables for further analysis. 

## How to use it
See this [markdown](http://htmlpreview.github.io/?https://github.com/nhpatterson/regextract/example/regextract.html) for installation and usage information.
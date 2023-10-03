Suitability modelling of profitable diversified farming systems
Related Manuscript DOI: XXXX

Authors: Hannah Kamau (hkamau@uni-bonn.de), Shahrear Roman (shahrear@hum.kuet.ac.bd), and Lisa Biber-Freudenberger (lfreuden@uni-bonn.de).

We predict the spatial global distribution of profitable diversified farming systems through niche modelling using maximum entropy (MaxEnt) approach. We used 114 known locations of profitable diversified farming practices obtained from a meta-analysis that compared the financial outcomes of diversified versus simplified farming practices (Sánchez et al., 2022). We then combined the known locations with spatial explicit information on different contextual condition to predict the global distribution of profitable locations. Finally, we combined the predicted global distribution with knowledge about biophysical potential for cropland expansion and intensification.

This R script is meant to perform suitability modelling for species distribution modelling using kuenm package (Cobos et al., 2019).
To do this you require (i) the locations of the observed presence of a species and in this case locations of profitable diversified farming practices. The input data for this study can be obtained here https://doi.org/10.60507/FK2/V13Z99. (ii) A set of constraints. In the context of ecology they are often environmental variables but in this case we used socio-economic conditions. These conditions/variables must be in raster with similar resolution and extent. The eight (8) variables used in this study can be accessed upon request, as the files are too big to upload here. (iii) Download MaxEnt jar file on https://biodiversityinformatics.amnh.org/open_source/maxent/. Finally (iv), For the bivariate maps, you will require raster files of integrated cropland expansion potential and intensification potential. We obtained this files from Zabel et al 2019 accessible on https://doi.org/10.1038/s41467-019-10775-z


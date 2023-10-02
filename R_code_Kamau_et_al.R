##Libraries###
if(! require(devtools)){
  install.packages("devtools")
}
if(!require(kuenm)){
  devtools::install_github("marlonecobos/kuenm")
}
library(kuenm) #maxent modelling
library(raster)
library(grid)
library(gridExtra)
library(lattice)
library(tiff)

wd <- "../exercise_kuenm/"

### Variables preparation ###
# Import databse used for Kamau et al "The Global North is more suited to sustainable, diversified and profitable agricultural systems than the Global South"
# Access link: https://bonndata.uni-bonn.de/privateurl.xhtml?token=1135fe86-1ce7-4b81-a6d4-1f23de006036
# You only need the first sheet "presence" 

data <- read.csv("final_data.csv")
set.seed(1)

split <-  kuenm_occsplit(data, train.proportion = 0.7, method = "random",
                       save = TRUE, name = "final_data")

# 8 raster files with similar specifications (resolution and extent). 
# Access: Upon request as the files are too big to upload here

variables <-  kuenm_varcomb( var.dir = "variables",
                    out.dir = "m_variables", min.number = 8,
                    in.format = "ascii", out.format = "ascii")

### Candidate Models ###

dj <-  "final_data_joint.csv"
dtr <-  "final_data_train.csv"
mvars <- "m_variables"
bcal <-  "bash_cal"
candir <-  "Candidate_models"
regm <-  c(0.5, 1, 2, 3, 4)
#adjusting the regularization multipliers according to Aleksander & Anderson 2013
#they found that a regularization multiplier slightly above default (1) provides for optimal performance
fclass <-  "all"
mxpath <-  "/Your Directory/exercise_kuenm/maxent" #absolute path only 

kuenm_cal(occ.joint = dj, occ.tra = dtr, M.var.dir = mvars, batch = bcal, 
          out.dir = candir, max.memory = 1000, reg.mult = regm, 
          f.clas = fclass, args = NULL,  maxent.path = mxpath, wait = FALSE, run = TRUE)

### Calibration of the Candidate models ### 

ote <-  "final_data_test.csv"
cresdir <-  "Calibration_results"

kuenm_ceval(path = candir, occ.joint = dj, occ.tra = dtr, occ.test = ote,
            batch = bcal, out.eval = cresdir, threshold = 5, rand.percent = 50, 
            iterations = 500, kept = TRUE, selection = "OR_AICc", parallel.proc = F)

### Final models ###

bfmod <-  "bash_model"
moddir <-  "Final_models"

kuenm_mod(occ.joint = dj, M.var.dir = mvars, out.eval = cresdir, 
          batch = bfmod, rep.n = 10, rep.type = "bootstrap", jackknife = TRUE,
          out.dir = moddir, max.memory = 1000, out.format = "cloglog", project = F,
          write.mess = F, write.clamp = F, maxent.path = mxpath, args = NULL, 
          wait = F, run = T)

### Final model evaluation ###
# We skipped this step because of lack of independent locations.  
# Nevertheless, below is the code for it. 

di <-  "final_data_ind.csv"
finmoddir <-  "Fin_model_eval"

kuenm_feval(path = moddir, occ.joint = dj, occ.ind = di, replicates = TRUE, 
            out.eval = finmoddir, threshold = 5, rand.percent = 50, 
            iterations = 500, parallel.proc = TRUE)


### BINARY MAPS ###

fdata_asc <-  raster("exercise_kuenm/Final_models/M_1_F_lq_Set_1/final_data_avg.asc")

# Binary map using balanced trainign omission threshold value
fdata_asc_bto <-  function(x) {
  ifelse(x <=  0.0672, 0,
         ifelse(x >  0.0672, 1, NA)) }
fdata_asc_bto_binary <-  calc(fdata_asc, fun = fdata_asc_bto)

# Binary map using maximum sensitivity plus specificity threshold value
fdata_asc_mtss <-  function(x) {
  ifelse(x <=  0.2685, 0,
         ifelse(x >  0.2685, 1, NA)) }
fdata_asc_mtss_binary <-  calc(fdata_asc, fun = fdata_asc_mtss)

# Binary map using equal trainign sensitivity and specificity threshold value
fdata_asc_etss <-  function(x) {
  ifelse(x <=  0.2608, 0,
         ifelse(x >  0.2608, 1, NA)) }
fdata_asc_etss_binary <-  calc(fdata_asc, fun = fdata_asc_etss)

# Binary map using 10 percentile training presence threshold value
fdata_asc_ptp <-  function(x) {
  ifelse(x <=  0.157, 0,
         ifelse(x >  0.157, 1, NA)) }
fdata_asc_ptp_binary <-  calc(fdata_asc, fun = fdata_asc_ptp)

# stack binary rasters of dfps
fdata_binary <-  stack(fdata_asc_bto_binary, fdata_asc_etss_binary,
                             fdata_asc_mtss_binary, fdata_asc_ptp_binary)
fdata_binarysum <- sum(fdata_binary)  # values between 0 and 4
fdata_binaryavg <-  mean(fdata_binarysum)

tiff("sum.tiff", width = 7.5, height = 6, units = "in", res = 300)
png("sum.png",width=7.5, height = 6, units = "in", res = 300)
plot(fdata_binarysum,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

tiff("average.tiff", width = 7.5, height = 6, units = "in", res = 300)
plot(fdata_binaryavg,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

tiff("fdata_bto.tiff", width = 7.5, height = 6, units = "in", res = 300)
plot(fdata_asc_bto_binary,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

tiff("fdata_mtss.tiff", width = 7.5, height = 6, units = "in", res = 300)
plot(fdata_asc_mtss_binary,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

tiff("fdata_etss.tiff", width = 7.5, height = 6, units = "in", res = 300)
plot(fdata_asc_etss_binary,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

tiff("fdata_ptp.tiff", width = 7.5, height = 6, units = "in", res = 300)
plot(fdata_asc_ptp_binary,frame.plot=F, axes=F, box=F, add=F, legend.width=1, legend.shrink=1)
map(interior = T, add=T)
dev.off()

yl = lapply(list("fdata_bto.tiff", "fdata_mtss.tiff", "fdata_etss.tiff", "fdata_ptp.tiff"), tiff::readTIFF)
gl = lapply(yl, grid::rasterGrob)
tiff("asc_thresholds.tiff", width = 9.2, height = 5.6, units = "in", res = 300)
png("asc_thresholds.png", width = 9.2, height = 5.6, units = "in", res = 300)
do.call(gridExtra::grid.arrange, gl)
dev.off()

### RESPONSE CURVES 
# From the Final models folder

all <-  lapply(list("final_data_Accessibility_2015_2.5.png", "final_data_cell_towers_2019_2.5.png",
                 "final_data_cropland_area_2019_2.5.png", "final_data_gdp_per_capita_ppp_2015_2.5.png",
                 "final_data_nightlight_2013_2.5.png","final_data_pop_density_2020_2.5.png",
                 "final_data_soc_2015_2.5.png", "final_data_voice_2.5.png"),
            png::readPNG)
gall <- lapply(all, grid::rasterGrob)
tiff("response_curves.tiff", width = 9.2, height = 5.6, units = "in", res = 300)
png("response_curves.png", width = 9.2, height = 5.6, units = "in", res = 300)
do.call(gridExtra::grid.arrange, gall)
dev.off()

## SUBSEQUENT MODELS 
# We based our subsequent models on Jackknife's results of the first model. We interogated the variables of importance to the model. 
#The top 5 variables in contribution to the models and possession of unique information, were choosen as input variables for the second round. 
#Suitability predction was then run as in the above lines and the results used to make bivariate maps using spatial maps from Zabel et al., 2019

## BIVARIATE MAPS OF GLOBAL SPATIAL DISTRIBUTION OF PROFITABLE DIVERSIFIED FARMING SYSTEMS WITH BIOPHYSICAL POTENTIAL FOR CROPLAND EXPANSION AND INTENSIFICATION

## Libraries 
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

# The colour matrix and bivariate maps are sourced from:https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html             
## COLOUR MATRIX
colmat <-  function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255),
                 upperright=rgb(130,0,80, maxColorValue=255), 
                 bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), 
                 xlab="x label", ylab="y label")
  {
  my.data <-  seq(0,1,.01)
  my.class <-  classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1 <-  findColours(my.class,c(upperleft,bottomleft))
  my.pal.2 <-  findColours(my.class,c(upperright, bottomright))
  col.matrix <-  matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col -> c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),
       ylim=c(0,1),xaxt="n", yaxt="n",frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1)
  for(i in 1:101){
    col.temp <-  col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
  seqs <-  seq(0,100,(100/nquantiles))
  seqs[1] <-  1
  col.matrix <-  col.matrix[c(seqs), c(seqs)]}

#specifying the number of quantiles
col.matrix <-  colmat(nquantiles=6, upperleft= rgb(0,150,235, maxColorValue=255), upperright=rgb(255,230,15,maxColorValue=255), 
                   bottomleft=rgb(229,228,226,maxColorValue=255), bottomright="red", 
                   xlab="My x label", ylab="My y label")

## BIVARIATE FUNCTION 
bivariate.map <-  function(rasterx,rastery, colormatrix = col.matrix, nquantiles = 10)
{
  quanmean <-  getValues(rasterx)
  temp <-  data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks <-  with(temp, unique(quantile(temp, na.rm = TRUE, probs = c(seq(0,1,1/nquantiles)))))
  r1 <-  within(temp, quantile -> cut(quanmean,breaks = brks, labels = 2:length(brks), include.lowest = TRUE))
  quantr <-  data.frame(r1[,2]) 
  quanvar <-  getValues(rastery)
  temp <-  data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <-  with(temp, unique(quantile(temp, na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))
  r2 <-  within(temp, quantile <- cut(quanvar, breaks = brks,labels = 2:length(brks), include.lowest = TRUE))
  quantr2 <-  data.frame(r2[, 2])
  as.numeric.factor <-  function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <-  colormatrix
  cn <-  unique(colormatrix)
  for(i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <-  1,
           col.matrix2[i] <-  which(col.matrix2[i]==cn)[1])
  }
  
  cols <-  numeric(length(quantr[,1]))
  
  for(i in 1:length(quantr[,1])) {
    a <-  as.numeric.factor(quantr[i,1])
    b <-  as.numeric.factor(quantr2[i,1])
    cols[i] <-  as.numeric(col.matrix2[b,a])
  }
  r <-  rasterx
  r[1:length(r)] <-  cols
  return(r)
}

# Load the ascii files 
# Integrated expansion potential and Intengrated intensification potential raster files are obtained from Zabel et al 2019
# "Global impacts of future cropland expansion and intensification on agricultural markets and biodiversity"
# Doi: 10.1038/s41467-019-10775-z 
                     
figure1 <-  raster("Global_distribution.tif")
cropland_expansion <-  raster("Integrated_expansion_potential.tif")
intensification_potential  <-  raster("Integrated_intensification_potential.tif")

template_raster <-  raster*ncol =8640, nrow =4320)
ext <-  extent(-180,180, -90, 90)
setcrs <-  crs("+proj=longlat+datum=WGS84 +no_defs")
template_raster <-  setExtent(template_raster, ext)
crs(template_raster) <-  setcrs

cropland_expansion_1 <- resample(cropland_expansion, template_raster, method = "bilinear")
intensification_potential_1 <- resample(intensification_potential, template_raster, method = "bilinear")

## Bivariate maps
Figure2 <-  bivariate.map(figure1, intensification_potential_1, 
                         colormatrix = col.matrix, nquantiles = 6)

Figure3 <- bivariate.map(figure1, cropland_expansion_1, 
                         colormatrix = col.matrix, nquantiles = 6)

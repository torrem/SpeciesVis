## Deliniate Gulf of Maine ecological Boundaries

#NMDS stress: 0.3 = arbitrary ordination, 0.2 = good/ok, <0.1 = great, <=0.05 = excellent representation in reduced dimensions

crop = c(-71.2, -66.6, 41.2, 45.6)# Whole dam maine coast
#crop = c(-69, -68, 43.6, 44.6)# Penobscot Bay Region
#crop = c(-71, -70, 41.7, 42.7)# NH/MA Coast

nspecies = 50
neigh = 6

options(scipen=999)

library(vegan)
library(MASS)
library(sp)
library(rgdal)
library(maptools)
library(grid)
library(ggplot2)
library(spatstat)
library(geosphere)
library(gstat)
library(classInt)
library(automap)
library(rgl)
library(raster)

word.tif = function(filename="Word_Figure_%03d.tif", zoom=4, width=35, height=35, pointsize=10, ...) {
  if (!grepl("[.]ti[f]+$", filename, ignore.case=TRUE))
    filename = paste0(filename,".tif")
  tiff(filename=filename, compression="lzw", res=400*zoom,
       width=width, height=height, units='cm', pointsize=pointsize, ...)
}

#font to Times
windowsFonts(Times=windowsFont("TT Times New Roman"))


setwd("C:/Users/Mike/Documents/GOM Ecological Boundaries")

d =  read.csv("C:/Users/Mike/Documents/GOM Ecological Boundaries/Raw Data/trawlMultiBioMass.csv")


d[is.na(d)] <- 0

fall = subset(d, SEASON == 'FALL')
spring = subset(d, SEASON == 'SPRING')

## Rank Species & Subset 
species = fall[,15:ncol(fall)]
species = species[,order(-colSums(species))]
species = species[,1:nspecies]
fall = cbind(fall[,1:14],species)

species = spring[,15:ncol(spring)]
species = species[,order(-colSums(species))]
species = species[,1:nspecies]
spring = cbind(spring[,1:14],species)


##---------select time frame-------------------------_
d = spring


##FALL
#dat = subset(d, CRUISE %in% c('FL00', 'FL01', 'FL02'))
#dat = subset(d, CRUISE %in% c('FL03', 'FL04', 'FL05'))
#dat = subset(d, CRUISE %in% c('FL06', 'FL07', 'FL08'))
#dat = subset(d, CRUISE %in% c('FL09', 'FL10', 'FL11'))
#dat = subset(d, CRUISE %in% c('FL12', 'FL13', 'FL14'))

##FALL
#dat = subset(d, CRUISE %in% c('FL00', 'FL01', 'FL02', 'FL03', 'FL04'))
#dat = subset(d, CRUISE %in% c('FL05', 'FL06', 'FL07', 'FL08', 'FL09'))
#dat = subset(d, CRUISE %in% c('FL10', 'FL11', 'FL12', 'FL13', 'FL14'))


##SPRING
dat = subset(d, CRUISE %in% c('SP01', 'SP02', 'SP03', 'SP04'))
#dat = subset(d, CRUISE %in% c('SP05', 'SP06', 'SP07', 'SP08', 'SP09'))
#dat = subset(d, CRUISE %in% c('SP10', 'SP11', 'SP12', 'SP13', 'SP14'))

#dat = subset(d, CRUISE =='SP01')
#dat = subset(d, CRUISE =='SP02')

##-----------plot tow locations-------------------------_

#read in NGOM shapefile
NGOM = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/NGOM/NGOMshapefile.shp")
COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")


#word.tif('fall10_14')
ggplot(COAST, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill="grey", colour="black")+
  theme_bw(base_size = 24)+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank())+ 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        line = element_blank(), panel.background = element_blank())+
  theme(legend.position="none")+
  geom_point(data=dat, aes(x=LON, y=LAT), colour='blue', pch=19, size=3)+
  coord_cartesian(xlim=c(-71.2, -66.6), ylim=c(41.2, 45.6))+
  #scale_colour_gradientn(colours=matlab.like2(10))+
  #labs(colour = "Log\nAbundance")+
  labs(title="") 




##_______________________________________Multivariate Statistics______________________________

#dat <- subset(dat, DEPTH >= 20 & DEPTH < 40) 
d=dat

##Add Data Source Here
MultiData <- d

#MultiData = subset(MultiData, Month==7)
#add month& day
#MultiData <- cbind(Day = day(dmy(MultiData$Date)), Month = month(dmy(MultiData$Date)),MultiData)

# #condense?
#MultiData <- setDT(MultiData)[, lapply(.SD, sum), by=list(Month, Day, Shoreline.Type)]
#MultiData <-data.frame(MultiData)

MultiDataD <- MultiData[,c(15:(length(MultiData)))]
MultiDataL <- MultiData[,c(1:14)]


MultiDataL <- within(MultiDataL, ID <- paste(CRUISE,STATION, sep='-'))

rownames(MultiDataD) <- MultiDataL$ID


#------------------------2D MDS---------------------------------------------
# #MDS plot
# #word.tif('LS12Multi')
# MultiData_MDS<-metaMDS(MultiDataD,distance="bray",k=2,trymax=50)
# plot(MultiData_MDS, display="sites", type="n", xlab="", ylab="", yaxt='n', xaxt='n')
# points(MultiData_MDS, display = "sites")
# text(MultiData_MDS, display="sites", pos=3, offset=0.5, cex=0.7)
# #ordispider(MultiData_MDS,groups=MultiDataL$Shoreline.Type, label=FALSE)
# #legend("topright", levels(MultiDataL$Shoreline.Type), pch = c(1,2), inset=c(0.02,.02))
# #points(MultiData_MDS, display = "sites", pch = as.numeric(MultiDataL$Shoreline.Type))
# #dev.off()


#------------------------3D MDS---------------------------------------------
#run MDS
MultiData_MDS<-metaMDS(MultiDataD,distance="bray",k=3,trymax=10)

#format MDS output for plot
MultiData_MDS$points[,1] -> x
MultiData_MDS$points[,2] -> y
MultiData_MDS$points[,3] -> z
da <- data.frame(x,y,z)
names <- rownames(da)
rownames(da) <- NULL

##3D MDS Plot
open3d()
plot3d(da, size=1, type='s', xlab="", ylab="", zlab="")

f = vegdist(MultiDataD, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE) 
ff =as.matrix(f)


sites <- scores(MultiData_MDS, display = "sites")

x = sites[,1]+ abs(min(sites[,1]))
x = data.frame(round(x/max(x)*100,0))

y = sites[,2]+ abs(min(sites[,2]))
y = data.frame(round(y/max(y)*100,0))

z = sites[,3]+ abs(min(sites[,3]))
z = data.frame(round(z/max(z)*100,0))

d = data.frame(x,y,z)

open3d()
plot3d(d, type='s', size = 1)

d$stations = rownames(d)
rownames(d) = NULL
d = d[,c(4,1,2,3)]
colnames(d)[2:4] = c('x','y','z')

d[d == 0] <- 1

##only for 2d mds
#plot(d$y~d$x)
#text(d$x,d$y, d$stations)

# #----Color gradient
# fun_xy <- function(x, y){
#   
#   R <- (x+1)/2
#   G <- (1-x)/2
#   B <- (y+1)/2
#   A <- 1- 0.5*exp(-(x^2+y^2)/0.2)
#   
#   rgb(R, G, B, A) #"A" corresponds to white spot in middle
#   
# }
# 
# z <- outer(seq(-1,1,length=100), seq(-1,1,length=100), FUN = fun_xy)
# 
# grid.newpage()
# grid::grid.raster(z)
# 
# color = rep(NA,nrow(d))
# for (i in 1:nrow(d)){
#   color[i] = z[d[i,2],d[i,3]]
# 
#    }
# 
# d = cbind(d,color)

a = 100

#x <- 0:a
# y <- 0:a
# z <- 0:a
# plot3d(x,y,z, type="p",xlab="x", ylab="y", zlab="z")

perms = expand.grid(1:a, 1:a, 1:a)

d$col = rep(NA,nrow(d))

for (i in 1:nrow(d)){
d$col[i] = rgb(d[i,2],d[i,3],d[i,4],maxColorValue=a)
}

open3d()

d$zz = rep(1,nrow(d))


plot3d(d[2:4], size=1, type='s', xlab="", ylab="", zlab="", col=d$col)

d$LON = dat$LON
d$LAT = dat$LAT

open3d()
dd = d 
plot3d(dd[c(7:8,6)],size=20, type='p', xlab="", ylab="", zlab="", col=d$col)

#play3d(spin3d(axis=c(1,0,0), rpm=5), duration=10)



#---------------------------------------------Color Gradient Map-------------------------------------------


#make lon-lat the coords
#coordinates(d)=~LON+LAT
d <-data.frame(d)
dcol = d$col

ddd = d
coordinates(ddd) = ~LON + LAT

col = d$col

#word.tif('Color Gradient Map SP 1')
spplot(COAST, fill = 'grey', zcol = "ID", colorkey=F,
       scales=list(draw=T),
       sp.layout=list(ddd, first=FALSE, col = col, pch = 15, cex=2),
       xlim=c(-71.2, -66.6), ylim=c(42.5, 45))
#dev.off()

# ##------------------3D Gradient Map----------------------------------
# 
# 
# 
# 
# GOMras <- raster("C:/Users/Mike/Documents/Scallop/Data/GIS/Depth/ne_atl_crm_v1.asc")
# 
# dep <- crop(GOMras, crop)
# 
# dep <- as.matrix(dep)
# 
# 
# pcrp <- p
# pcdf = data.frame(pcrp@coords,pcrp@data) 
# hh = rasterFromXYZ(pcdf[,c(1:2, 3)]) 
# hh <- crop(hh, crop)
# 
# ##since hh is a different resolution than the elevation matrix 
# ##need to resample hh matrix onto same grid as elevation matrix
# resampleFactor <- (dim(hh)[1]/dim(dep)[1])
# inputRaster = hh
# inCols <- ncol(inputRaster)
# inRows <- nrow(inputRaster)
# resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
# extent(resampledRaster) <- extent(inputRaster)
# resampledRaster <- resample(inputRaster,resampledRaster,method='bilinear')
# 
# hh <-resampledRaster
# 
# hh <- as.matrix(hh)
# dim(hh)
# 
# 
# 
# 





#---------------------------2D Heat Map-----------------------------------------------------------
dd <-data.frame(d)
dd$ratio = rep(NA, nrow(dd))

pb <- txtProgressBar(min = 0, max = nrow(dd), style = 3)
for (i in 1:nrow(dd)){
 x = dd[i,] #select point 
 y = dd #all other points

 
 
 nn = data.frame(rep(NA, nrow(y)))
 nn$ID = 1:nrow(nn)
 colnames(nn)[1] = 'dist'
 
 for (k in 1:nrow(y)){
   n = distm(c(x$LON, x$LAT), c(y$LON[k], y$LAT[k]), fun=distHaversine)
   nn$dist[[k]] = n
 }
nn = nn[order(nn$dist),]
 
geoDist = sum(nn$dist[1:neigh]) 

call = nn[1:neigh,]$ID

## Change MDS plot dist to dissimilarity ***## 
#mdsDist= sum( sqrt(  (dd[call,2]-x$x)^2 + (dd[call,3]-x$y)^2   )  )## MDS plot distance

mdsDist = sum(ff[i,(call[1:neigh])]) ##dissimilarity matrix
 
rat = mdsDist/geoDist 

dd$ratio[i] = rat
  
setTxtProgressBar(pb, i)
}



#-------------------------RWB color pallet-----------------------------------------_

#blue-white-red colors
color.palette = function(steps, n.steps.between=NULL, ...){
  
  if(is.null(n.steps.between)) n.steps.between = rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps = cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB = matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] = col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals = seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] = vals
    }
  }
  
  new.steps = rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal = colorRampPalette(new.steps, ...)
  return(pal)
}
steps = c("blue", "white", "red")
pal = color.palette(steps, space="rgb")
col = pal

#trellis.par.get() # need this to enlarge font size on color bar



#-----------Kriging and plot--------_
ddd = dd


coordinates(dd) = ~LON + LAT

xrange = range(dd$LON)
yrange = range(dd$LAT)

grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))

gridded(grid) = ~longitude + latitude


kriging_result = autoKrige(ratio~1, dd, new_data = grid, maxdist = 0.15)#use this function to automatically find good variogram values

#plot(kriging_result, col=pal)

v = variogram(ratio ~ 1, dd)

v.g = vgm(nugget = kriging_result$var_model$psill[1], psill = kriging_result$var_model$psill[2], 
          range = kriging_result$var_model$range[2], model = "Sph") 

v.f = fit.variogram(object = v, model = v.g)  #Fit variogram 

plot(v, model = v.f)
# 
g = gstat(formula = ratio ~ 1, model = v.f, data = dd, maxdist = 0.1)  

p = predict(g, newdata = grid)

col = pal

hist(dd$ratio)

hh = classIntervals(dd$ratio, 35, style = "jenks")

# #word.tif('Heat Map SP3')
spplot(p, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
       col.regions = col,
       #at = ((min(hh$brks)*10):(max(hh$brks)*10))/10, 
       at = hh$brks,
       scales=list(draw=TRUE),
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = c(crop[1], crop[2]), ylim = c(crop[3], crop[4]))
# #dev.off()


col <- colorRampPalette(c("blue", "white", "red"), bias = 1)


#word.tif('var_forecast2')
spplot(p,
       col.regions = col,sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
       at = ((min(hh$brks)*50):(max(hh$brks)*50))/50, 
       #at = (0:160)/160,
       scales=list(draw=TRUE),
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = c(-75, -66), ylim = c(38, 45))
#dev.off()





# writeWebGL(dir = "webGL", filename = file.path(dir, "index.html"), 
#            template = system.file(file.path("WebGL", "template.html"), package = "rgl"),
#            snapshot = TRUE, font = "Arial")













# #---------------------------3D Heat Map-----------------------------------------------------------
# GOMras <- raster("C:/Users/Mike/Documents/Scallop/Data/GIS/Depth/ne_atl_crm_v1.asc") 
# #GOMras <- raster("C:/Users/Mike/Documents/Scallop/Data/GIS/North Atlantic Shelf Bathymetry GIS/NAS.asc") 
# 
# 
# dep <- crop(GOMras, crop)
# 
# 
# #plot(dep)
# 
# 
# dep <- as.matrix(dep)
# 
# 
# pcrp <- p
# pcdf = data.frame(pcrp@coords,pcrp@data) 
# hh = rasterFromXYZ(pcdf[,c(1:2, 3)]) 
# hh <- crop(hh, crop)
# 
# ##since hh is a different resolution than the elevation matrix 
# ##need to resample hh matrix onto same grid as elevation matrix
# resampleFactor <- (dim(hh)[1]/dim(dep)[1])
# inputRaster = hh
# inCols <- ncol(inputRaster)
# inRows <- nrow(inputRaster)
# resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
# extent(resampledRaster) <- extent(inputRaster)
# resampledRaster <- resample(inputRaster,resampledRaster,method='bilinear')
# 
# hh <-resampledRaster
# 
# hh <- as.matrix(hh)
# dim(hh)
# 
# ##----surface plot----------------------_
# z<-dep 
# mult=z
# mult[z >= 0] <- 1 
# mult[z < 0] <- 2 #multiplyer to add emphasis to depth
# z <- z  * mult
# 
# invz <--z #takes the inverse of z to display map correctly
# x<-10*(1:nrow(z))
# y<-10*(1:ncol(z))
# zlim<-range(z)
# zlen<-zlim[2]-zlim[1]+1
# 
# ##Assign color
# col=rep("#7F7F7F", length(dep))
# 
# 
# 
# HSI_Vec = as.vector(hh)
# HSI_Vec[is.na(HSI_Vec)] <- 8000 #chaging to smaller kirging radius created a few gaps under land surfaces.. change to 0
# Ele_Vec = as.vector(dep)
# 
# #Need to change this block to vectorized commands to handle larger raster size!!
# pb <- txtProgressBar(min = 0, max = length(HSI_Vec), style = 3)
# system.time(
#   for (i in 1:length(HSI_Vec)){
#     if (0 <= HSI_Vec[i] & HSI_Vec[i] < 0.0001) {col[i]=pal(9)[1]}
#     if (0.0001 < HSI_Vec[i] & HSI_Vec[i] < 0.0002) {col[i]=pal(9)[2]}
#     if (0.0002 < HSI_Vec[i] & HSI_Vec[i] < 0.0003) {col[i]=pal(9)[3]}
#     if (0.0003 < HSI_Vec[i] & HSI_Vec[i] < 0.0004) {col[i]=pal(9)[4]}
#     if (0.0004 < HSI_Vec[i] & HSI_Vec[i] < 0.0005) {col[i]=pal(9)[6]}
#     if (0.0005 < HSI_Vec[i] & HSI_Vec[i] < 0.0006) {col[i]=pal(9)[7]}
#     if (0.0006 < HSI_Vec[i] & HSI_Vec[i] < 0.0007) {col[i]=pal(9)[8]}
#     if (0.0007 < HSI_Vec[i] & HSI_Vec[i] < 0.0008) {col[i]=pal(9)[9]}
#     if (0.0008 < HSI_Vec[i] & HSI_Vec[i] < 0.0009) {col[i]=pal(9)[9]}
#     if (0.0009 < HSI_Vec[i] & HSI_Vec[i] <= 0.001) {col[i]=pal(9)[9]}
#     if(HSI_Vec[i] == 8000) {col[i="#7F7F7F"]}
#     # update progress bar
#     setTxtProgressBar(pb, i)
#     # print(i)
#   })
# close(pb)
# 
# 
# for (i in 1:length(HSI_Vec)){
#   if (Ele_Vec[i] >=1) {col[i]="#006400"}
# }
# 
# open3d()
# rgl.surface(x,y,invz,color=col,alpha=1,back="lines", specular="black")
# #terrain3d(x,y,invz,color=col,alpha=0.5,back="lines")
# #box3d()
# 
# 

###---------------------------------------------------END------------------------------------------------------------------------

# col = pal
# 
# hist(dd$ratio)
# 
# hh = classIntervals(dd$ratio, 35, style = "jenks")
# 
# spplot(kriging_result, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
#        col.regions = col,
#        at = hh$brks,
#        scales=list(draw=TRUE),
#        zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
#        xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))
# 
# 
# 
# spplot(kriging_result, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
#        col.regions = col,
#        at = hh$brks,
#        scales=list(draw=TRUE),
#        zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
#        xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# --------------------------------------------------------------------------------------
#   coordinates(dd) = ~LON + LAT
# 
# xrange = range(dd$LON)
# yrange = range(dd$LAT)
# 
# grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
#                   latitude = seq(from = yrange[1], to = yrange[2], by = .004))
# 
# gridded(grid) = ~longitude + latitude
# 
# 
# kriging_result = autoKrige(ratio~1, dd, new_data = grid, maxdist = 0.15)
#   
# 
# 
#     v = variogram(ratio ~ 1, dd)
# v.g = vgm(nugget = 0.003, psill = 0.0197, range = 0.9, model = "Sph") 
# v.f = fit.variogram(object = v, model = v.g)  #Fit variogram 
# plot(v, model = v.f)
# 
# g = gstat(formula = HSImed ~ 1, model = v.f, data = med.HSI, maxdist = 0.1)  
# 
# xrange = range(med.HSI$longitude)
# yrange = range(med.HSI$latitude)
# 
# grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
#                   latitude = seq(from = yrange[1], to = yrange[2], by = .004))
# gridded(grid) = ~longitude + latitude
# 
# p = predict(g, newdata = grid)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #-------------------------------------test-------------------------------------
# 
# 
# library(automap)
# loadMeuse()
# # Ordinary kriging
# kriging_result = autoKrige(zinc~1, meuse, meuse.grid)
# plot(kriging_result)
# # Universal kriging
# kriging_result = autoKrige(zinc~soil+ffreq+dist, meuse, meuse.grid)
# plot(kriging_result)
# 
# 
# kriging_result = autoKrige(zinc~1, meuse, meuse.grid)






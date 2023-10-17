
# Load Packages -----------------------------------------------------------

library(raster)
library(rgeos)
library(rnaturalearth)
library(RColorBrewer)

# Get Data ----------------------------------------------------------------

#global precipitation data from worldclim
prec = getData(name='worldclim', var='prec', res=5)

#outlines of US States
usa = ne_states(iso_a2 = 'us')

#R logo
logo_fn = system.file("external/rlogo.grd", package="raster")
logo = stack(logo_fn) 

#Albers Equal Area North American projection
albers_NA = crs("ESRI:102008")


#Precipitation color palette
blues = colorRampPalette(brewer.pal(9, 'Blues'))

# Filtering ---------------------------------------------------------------

not_contiguous = c('Alaska', 'Hawaii') 
voi = c('name', 'type', 'region', 'postal')

#removing extraneous data for easier plotting
c_usa = usa[which(!usa$name %in% not_contiguous), voi]


# Spatial comparison ------------------------------------------------------

#convert raster to lon/lat to match USA data
logo_ll = projectRaster(logo, crs=crs(usa))

#convert raster to polygons so we can use rgeos functionality
logo_sp = rasterToPolygons(logo_ll)

#ask whether the R logo intersects 
if (gIntersects(logo_sp, usa)) {
  print('The R logo is in the United States!')
} else {
  print('The R logo is not in the United States')
}


# Area calculation --------------------------------------------------------

#area of each cell of the logo
logo_cell_area = area(logo_ll)

#sum up area of all the cells
logo_area = sum(getValues(logo_cell_area))

#project contiguous usa polygons
usa_albers = spTransform(c_usa, albers_NA)

# get area of polygons
usa_area = gArea(usa_albers)

if (usa_area > logo_area) {
  print('The contiguous United States is bigger than the R logo.')
} else {
  print('The R logo is bigger than the contiguous United States!')
}




# Precipitation Seasonality in the United States --------------------------

nlayers(prec) #data is monthly

range(getValues(prec), na.rm=TRUE) #data is likely in mm

#crop precip data to contiguous us extent and then mask it to create the correct shape
prec_usa = mask(crop(prec, c_usa), c_usa)

#project precip to albers projection
prec_albers = projectRaster(prec_usa, crs=albers_NA)

#convert mm to inches
prec_in = prec_albers*0.0393701

#calculate Seasonality (coefficient of variation for each cell)
seasonality = cv(prec_in)

#plot seasonality with state outlines
par(mfrow=c(1,1))
plot(seasonality, yaxt="n",  xaxt="n", col=blues(120),
     main='Precipitation Seasonality\n(coefficient of variation)')
plot(usa_albers, add=TRUE)



# Investigate Precipitation Throughout the Seasons ------------------------

#quarter labels
seasons = c('Winter', 'Spring', 'Summer', 'Fall')

#index to group months by quarter
quarters = c(1, 1, rep(2:4, each=3), 1)

#sum by quarter
prec_q = stackApply(prec_in, indices=quarters, fun=sum)

#set values greater than 30 to be 30 so we can plot on the same scale
prec_q_30 = prec_q
prec_q_30[prec_q_30>30] = 30

#plot precipitation totals for all four quarters
par(mfrow=c(2,2), mai=c(0, 0, 0.2, 0))
for (i in 1:4) {
  plot(prec_q_30[[i]], zlim=c(0, 30), yaxt="n",  xaxt="n", main=seasons[i],
       col=blues(60), colNA='white', axes=FALSE)
  plot(usa_albers, add=TRUE)
}


# Averaging by State ------------------------------------------------------

#extract average precip by month for each state
usa_precip0 = extract(prec_in, usa_albers, fun=mean, na.rm=TRUE, df=TRUE)

#rename precip columns with their month names
names(usa_precip0) = c('ID', month.name)

#add precip data to spdf
usa_precip = cbind(usa_albers, usa_precip0)

#plot monthly precipitation averages
spplot(usa_precip, zcol=month.name, as.table=TRUE, col.regions=blues(16))

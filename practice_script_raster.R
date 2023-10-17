library(raster)
library(rgeos)
library(rnaturalearth)

prec = getData(name='worldclim', var='prec', res=5)

usa = ne_states(iso_a2 = 'us')

logo_fn = system.file("external/rlogo.grd", package="raster")
logo = stack(logo_fn) 

albers_NA = crs("ESRI:102008")

#filtering
not_contiguous = c('Alaska', 'Hawaii')
voi = c('name', 'type', 'region', 'postal')

c_usa = usa[which(!usa$name %in% not_contiguous), voi]

### Spatial Comparison
logo_ll = projectRaster(logo, crs=crs(usa))

logo_sp = rasterToPolygons(logo_ll)


if (gIntersects(logo_sp, usa)) {
  print('The R logo is in the United States!')
} else {
  print('The R logo is not in the United States')
}

### Area calculation
logo_cell_area = area(logo_ll)

logo_area = sum(getValues(logo_cell_area))

usa_albers = spTransform(c_usa, albers_NA)
usa_area = gArea(usa_albers)

if (usa_area > logo_area) {
  print('The contiguous United States is bigger than the R logo.')
} else {
  print('The R logo is bigger than the contiguous United States!')
}




# Precipitation Seasonality in the United States --------------------------

nlayers(prec) #data is monthly

range(getValues(prec), na.rm=TRUE) #data is likely in mm

prec_usa = mask(crop(prec, c_usa), c_usa) 

prec_albers = projectRaster(prec_usa, crs=albers_NA)

prec_in = prec_albers*0.0393701 #convert to inches

seasonality = cv(prec_in)

usa_albers = spTransform(c_usa, albers_NA)

plot(seasonality, yaxt="n",  xaxt="n",
     main='Precipitation Seasonality\n(coefficient of variation)')
plot(usa_albers, add=TRUE)



# Investigate Precipitation Throughout the Seasons ------------------------

seasons = c('Winter', 'Spring', 'Summer', 'Fall')

quarters = c(4, 4, rep(1:3, each=3), 4)

prec_q = stackApply(prec_in, indices=quarters, fun=sum)

prec_q_30 = prec_q
prec_q_30[prec_q_30>30] = 30

brks = seq(0, 30, by=5)

par(mfrow=c(2,2), mai=c(0, 0, 0.2, 0))
for (i in 1:4) {
  plot(prec_q_30[[i]], zlim=c(0, 30), yaxt="n",  xaxt="n", main=seasons[i])
  plot(usa_albers, add=TRUE)
}


# Averaging by State ------------------------------------------------------

usa_precip0 = extract(prec_in, usa_albers, fun=mean, na.rm=TRUE, df=TRUE)

names(usa_precip0) = c('ID', month.name)

usa_precip = cbind(usa_albers, usa_precip0)

spplot(usa_precip, zcol=month.name, as.table=TRUE)

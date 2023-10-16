library(raster)
library(rgeos)


lux_fn = system.file("external/lux.shp", package="raster")
lux = shapefile(lux_fn, layer='lux')

bio = getData(name='worldclim', var='bio', res=10)

logo_fn = system.file("external/rlogo.grd", package="raster")
logo = stack(logo_fn) 

f <- system.file("ex/elev.tif", package="terra")
elev = raster(f)

### Spatial Comparison
logo_ll = projectRaster(logo, crs=crs(bio))

logo_sp = rasterToPolygons(logo_ll)


if (gIntersects(logo_sp, lux)) {
  print('The R logo is in Luxembourg!')
} else {
  print('The R logo is not in Luxembourg.')
}

### Area calculation
logo_cell_area = area(logo_ll)

logo_area = sum(getValues(logo_cell_area))

lux_merc = spTransform(lux, crs(logo))
lux_area = gArea(lux_merc)

if (lux_area > logo_area) {
  print('Luxembourg is bigger than the R logo.')
} else {
  print('The R logo is bigger than Luxembourg!')
}

#16-17/12

utm32 = crs("epsg:25832")

compareRaster(bio, elev)

bio5 = resample(bio, elev)

compareRaster(bio5, elev)

lux_env = stack(elev, bio5$bio1/10, bio5$bio12)

lux_env = mask(lux_env, lux)

avg_env = extract(lux_env, lux, fun=mean, na.rm=TRUE)


lux_env_proj = projectRaster(lux_env, crs=utm32)

lux_proj = spTransform(lux, utm32)




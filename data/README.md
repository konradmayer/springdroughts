# Data download and preprocessing

## ERSST V5

Monthly mean data of sea surface temperature was obtained from NOAA ([ERSST5](https://psl.noaa.gov/data/gridded/data.noaa.ersst.v5.html)) and 
preprocessed to obtain annual March-April mean values at a spatial and 
temporal subset:

```
ncpdq -a -lat sst.mnmean.nc tmp.nc
ncks -x -v time_bnds tmp.nc tmp.nc
cdo sellonlatbox,-180,180,0,90 tmp.nc tmp.nc
cdo selyear,1854/2021 tmp.nc ersst.v5.1854-2021_remap.nc

```

# 20CR:

Data of the [20th century reanalysis Project Version 2](https://psl.noaa.gov/data/20thC_Rean/) were used,
geopotential height extracted for 500hPa at the northern hemisphere and annual mean March-April values calculated.

```
for year in {1851..2014}
do
cdo sellevel,500 hgt.${year}.nc tmp.nc
cdo sellonlatbox,-180,180,0,90 tmp.nc tmp.nc
cdo invertlat tmp.nc z500.${year}.nh.nc
done

cdo cat z500.*.nh.nc tmp.nc                  
cdo monmean tmp.nc tmp.nc
cdo yearmean -selmon,3,4 tmp.nc tmp.nc
ncks -x -v time_bnds tmp.nc tmp.nc
ncwa -a bnds tmp.nc 20CR_z500_1851-2014_nh_MarApr.nc

```

# ERA5

Monthly mean geopotential data from the European Centre for Medium Range Weather Forecast Reanalysis Version 5 (ERA5) was obtained via the [climate data store (CDS)](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels-monthly-means?tab=overview) for [variable 129](https://apps.ecmwf.int/codes/grib/param-db?id=129) for the northern hemisphere.
Downloaded data were preprocessed to remap geopotential onto the grid of the 20CR data, and calculate annual mean March and April values:

```
cdo -b F64 mergetime era5_geopotential_500hPa_*.nc tmp.nc
cdo remapbil,20CR_z500_1851-2014_nh_MarApr.nc tmp.nc tmp.nc
cdo yearmean -selmon,3,4 tmp.nc tmp.nc
ncks -x -v time_bnds tmp.nc tmp.nc
ncwa -a bnds tmp.nc ERA5_gp500_1979-2020_nh_MarApr.nc
```

# HISTALP precipitation

Precipitation totals from the [HISTALP dataset](https://www.zamg.ac.at/histalp/) are 
available online for stations as well as gridded dataset. However, for the aggregated 
HISTALP region AT6 (lowland), only annual or seasonal aggragates are available.
This is why the March-April sums are provided as `data/R01_LOW_MarApr.txt` within this 
repository.

# EAWR reconstruction

The EAWR reconstruction was obtained from the Author content [provided on ResearchGate](https://www.researchgate.net/publication/312040329_Decadal_changes_in_North_Atlantic_atmospheric_circulation_patterns_recorded_by_sand_spits_since_1800_CE_-_Historical_EAWR_reconstructions/link/586bccc808ae329d62121413/download) as supplement to:


Poirier, Clément & Tessier, Bernadette & Chaumillon, Eric & Bertin, X. & Fruergaard, Mikkel & Mouazé, Dominique & Noël, Suzanne & Weill, Pierre & Woppelmann, Guy. (2017). Decadal changes in North Atlantic atmospheric circulation patterns recorded by sand spits since 1800 CE - Historical EAWR reconstructions. 

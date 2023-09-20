#######Get_Env_Data B
# Get data sequence number two : 
# just download the data from the thredds server via opendap
# Adapted from EcoCast by Heather Welch (UCSC/NOAA)


############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"
############# ----------------------------> End ################

Get_Env_Data_B=function(path,source_path){ 
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/BenioffRuns")
  staticdir=glue("{path}/static_variables")
  temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  
  flyersdir=glue("{outdir}/flyers")
  rastersdir=glue("{outdir}/rasters")
  mapssdir=glue("{outdir}/maps")
  latestdir=glue("{outdir}/latest")
  latestweekdir=glue("{outdir}/latest_week")
  latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  
  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()-1
  # get_date="2019-12-14"
  
  tmpdir=glue("{temp}/temp_",get_date,sep="")
  finaldir=glue("{envdir}/{get_date}")
  
  ############ 3. define functions
  
  handle_the_data=function(get_date,var,template,save_var,finaldir){ 
    print(var)
    
    ref_date <- dmy('02-01-2011')
    new_date <- as.Date(get_date)
    days <- as.numeric(difftime(new_date, ref_date))
    stamp=glue("Hours since 2011-01-02T00:00:00Z: {days}")
    print(get_date)
    print(stamp)
    
      my_url = glue("https://oceanmodeling.ucsc.edu/thredds/dodsC/ccsra_2016a_phys_agg_derived_vars/fmrc/CCSRA_2016a_Phys_ROMS_Derived_Variables_Aggregation_best.ncd?{var}[{days}:1:{days}][0:1:180][0:1:185],lat_rho[0:1:180][0:1:185],lon_rho[0:1:180][0:1:185],time[0:1:1]")
      nc.data=nc_open(my_url)
    print("grabbing dimensions and variables")
    lat <- ncvar_get(nc.data,'lat_rho')
    lon <- ncvar_get(nc.data,'lon_rho')
    nrows <- length(lat); ncols <- length(lon)
   
    tmp.array <- ncvar_get(nc.data, var)
    fillvalue <- ncatt_get(nc.data, var, "_FillValue")
    print("creating matrix for day of interest")
    
    dat1=list()
    dat1$x=lon
    dat1$y=lat
    dat1$z=t(tmp.array)
    
    r <-raster(
      dat1$z,
      xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
      ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
      crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    r2=flip(r,2)
    
    #Force to template res
    r3 <- raster::resample(r2, template)  
    extent(r3)=extent(template)
    # return(r3)
    writeRaster(r3,glue("{finaldir}/{save_var}.grd"),overwrite=T)
    
    if(var=="sst"|var=="ssh"){
      rasSD=focal(r3,w=matrix(1,nrow=7,ncol = 7),fun=sd,na.rm=T) ## create SD
      writeRaster(rasSD,glue("{finaldir}/{save_var}_sd.grd"),overwrite=T)
    }
  }

  
  ############ 4. define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  studyarea=st_read(glue("{staticdir}/sa_square_coast3.shp"))
  studyarea <- sf:::st_zm(studyarea$geom)
  studyarea=as(studyarea, "Spatial")
  
  ############ 5. download data
  print("**************************************************************************************")
  print(paste0("Starting script Get_Env_Data_B.R,"," Time is ",Sys.time()))
  tryCatch(
    expr ={
  handle_the_data(get_date = get_date, var="sst", template=template, save_var="sst",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="bbv_200", template=template, save_var="bv",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="curl", template=template, save_var="curl",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="ild_05", template=template, save_var="ild",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="ssh", template=template, save_var="ssh",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="su", template=template, save_var="su",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="sustr", template=template, save_var="sustr",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="sv", template=template, save_var="sv",finaldir = finaldir)
  handle_the_data(get_date = get_date, var="svstr", template=template, save_var="svstr",finaldir = finaldir)
  
  eke=(raster(glue("{finaldir}/su.grd"))^2+raster(glue("{finaldir}/sv.grd"))^2)/2%>%log();writeRaster(eke,glue("{finaldir}/EKE.grd"),overwrite=T)
    },
  error = function(e){
    message(glue("Variables not available {get_date}"))
    print(e)
  }
  )
  print("**************************************************************************************")
  
}

Get_Env_Data_B(path=path,source_path=source_path)


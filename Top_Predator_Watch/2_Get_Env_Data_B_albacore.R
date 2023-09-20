#######Get_Env_Data B
# Get data sequence number two : 
# just download the data from the thredds server via opendap
# Adapted from EcoCast by Heather Welch (UCSC/NOAA)

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/OLE/operationalization_albacore"

## path to the load libraries r script ("/loadlib-new-albacore.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects_new/scratch_workspaces/scratch_heather/10a_operationalization_albacore/V2"
############# ----------------------------> End ################

Get_Env_Data_B=function(path,source_path){ 
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new-albacore.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- paste(path,"/model_runs/",sep="")
  logdir=paste(outdir,"logs/",sep="")
  staticdir=glue("{path}/static_variables/")
  benioffdir=glue("{path}/shiny_www")
  moddir=glue("{path}/models/")
  
  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()
  # get_date="2022-10-11"
  
  # tmpdir=glue("{temp}/temp_",get_date,sep="")
  finaldir=glue("{envdir}/{get_date}")
  
  logfile = paste(logdir,"log_",get_date,".txt",sep="") 
  sink(logfile, type=c("output","message"),append = TRUE) #set all output to templog
  
  ############ 3. define functions
  getTimePosition <- function(conn,get_date){
    # get time index for a given date from time axis in an opendap connection. Only works for a few examples of time units.
    # kludgy, should use udunits library, but works for now
    #parameters
    ## conn: netcdf file or remote opendap connection, obtained with nc_open
    ## get_date: date to get index for "yyyy-mm-dd", example: get_date="2022-10-11"
    ntimes=conn$dim$time$len
    times=conn$dim$time$vals
    timeUnits=conn$dim$time$units
    tulen=nchar(timeUnits)
    punct=substr(timeUnits,tulen-2,tulen-2)
    tinc=substr(timeUnits,1,10)
    # days_since yyyy-mm-dd
    if(tinc=="days since"){
      if(punct=="-")timeOrigin=substr(timeUnits,tulen-9,tulen) ## for time units like "days since yyyy-mm-dd"
      if(punct==":")timeOrigin=substr(timeUnits,tulen-18,tulen-9) ## for time units like "days since yyyy-mm-dd 00:00:00"
      times_date=as.Date(times,origin = timeOrigin)
    }
    # seconds since yyyy-mm-dd hh:mm:ss
    if(tinc=="seconds si"){
      timeOrigin=substr(timeUnits,tulen-18,tulen-9) ## for time units like "seconds since 1981-01-01 00:00:00"
      times_date=as.POSIXct(times,origin = timeOrigin,tz = "UTC") %>% as.Date()
    }
    # hours since yyyy-mm-dd hh:mm:ss
    if(tinc=="hours sinc"){
      timeOrigin=substr(timeUnits,tulen-18,tulen-9) ## for time units like "hours since 1981-01-01 00:00:00"
      times_date=as.POSIXct(times*3600,origin = timeOrigin,tz = "UTC") %>% as.Date()
    }
    nearest_date_position=(which.min(abs(times_date-as.Date(get_date))))
    nearest_date=times_date[nearest_date_position]
    how_different=difftime(as.Date(get_date),nearest_date,units = "days") %>% as.numeric(.,units="days")
    notation_date=nearest_date	
    return(c(nearest_date_position,nearest_date,how_different,notation_date))
  }
  
  getDepthPosition <- function(conn,depth){
    # returns nearest depth index to given depth from depth axis in an opendap connection
    #parameters
    ## conn: netcdf file or remote opendap connection, obtained with nc_open
    ## depth: depth to extract in same units as in the file
    ndepth=conn$dim$depth$len
    depths=conn$dim$depth$vals
    nearest_depth_position=(which.min(abs(depths-depth)))
    return(nearest_depth_position)
  }
  
  subsetLatLon <- function(conn,var,box,date_position,depth_start,ndepths){
    #returns a raster of data extracted from a netcdf file or opendap connection
    #parameters
    ## conn: netcdf file or remote opendap connection, obtained with nc_open
    ## box: region to extract
    ##	vector of xmin,xmax,ymin,ymax ** xmin xmax always expressed in 0-360 degrees **
    ##	data can be 0-360 or -180- 180, latitude can be north to south and can extract across dateline
    ## example:  box=c(100,295,-60,60)
    ## date_position: date index to extract
    ## depth_start: depth index to start the extract, or if no depth set to -1
    ## ndepths: number of depths to extract, ignored if depth_start set to -1
    ## subsetting lat
    if(conn$dim$lat$vals[1]>conn$dim$lat$vals[2]){ ## if latitude is ordered north to south
      lat_start=which.min(abs(conn$dim$lat$vals+box[3]))
      lat_end=which.min(abs(conn$dim$lat$vals+box[4]))
      lat_n_to_s=TRUE
    } else{ 						## if latitude is ordered south to north
      lat_end=which.min(abs(conn$dim$lat$vals+box[3]))
      lat_start=which.min(abs(conn$dim$lat$vals++box[4]))
      lat_n_to_s=FALSE
    }
    lat <- conn$dim$lat$vals[lat_start:lat_end]
    nrows <- length(lat);
    
    if (min(conn$dim$lon$vals)<0){ ## if we're dealing with -180 to 180 data
      ## 3 cases: 1) only need data from west of dateline 2) only need data from east of dateline 3) need data across dateline
      ## for case 1) and 2) we just need one call, two calls for case 3)
      if(box[1]<180 && box[2] >180){ ##case 3)
        x1=box[1] ## positive for part east of dateline
        x2=box[2]-360 ## negative for part west of dateline
        lon_start1=which.min(abs(conn$dim$lon$vals-x1))
        lon_end1=which.max(conn$dim$lon$vals)
        lon_start2=1
        lon_end2=which.min(abs(conn$dim$lon$vals-x2))
        lon1 <- conn$dim$lon$vals[lon_start1:lon_end1]
        ncols1 <- length(lon1)
        if(depth_start<0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,date_position),c(ncols1,nrows,1))
        if(depth_start>=0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,depth_start,date_position),c(ncols1,nrows,ndepths,1))
        lon2 <- conn$dim$lon$vals[lon_start2:lon_end2]
        ncols2 <- length(lon2)
        if(depth_start<0)tmp.array2 <- ncvar_get(conn, varid=var,c(lon_start2,lat_start,date_position),c(ncols2,nrows,1))
        if(depth_start>=0)tmp.array2 <- ncvar_get(conn, varid=var,c(lon_start2,lat_start,depth_start,date_position),c(ncols2,nrows,ndepths,1))
        if(ndepths<=1){
        r1 <- raster(t(tmp.array1),xmn=range(lon1)[1],xmx=range(lon1)[2],ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        r2 <- raster(t(tmp.array2),xmn=range(lon2)[1]+360,xmx=range(lon2)[2]+360,ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        }else if(ndepths>1){
          r1 <- raster::brick(
            aperm(tmp.array1,c(2,1,3)),
            xmn=range(lon1)[1],
            xmx=range(lon1)[2],
            ymn=range(lat)[1],
            ymx=range(lat)[2],
            crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% 
            mean(.,na.rm=T)
          r2 <- raster::brick(
            aperm(tmp.array2,c(2,1,3)),
                              xmn=range(lon2)[1]+360,
                              xmx=range(lon2)[2]+360,
                              ymn=range(lat)[1],
                              ymx=range(lat)[2],
                              crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))%>%
            mean(.,na.rm=T)
        }
        res(r2) <- c(xres(r1),yres(r1))
        r <- raster::merge(r2, r1)
      }
      if(box[1]<180 && box[2] <180){ ##case 1)
        x1=box[1] ## both east of 0, west of dateline
        x2=box[2] 
        lon_start1=which.min(abs(conn$dim$lon$vals-x1))
        lon_end1=which.min(abs(conn$dim$lon$vals-x2))
        lon1 <- conn$dim$lon$vals[lon_start1:lon_end1]
        ncols1 <- length(lon1)
        if(depth_start<0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,date_position),c(ncols1,nrows,1))
        if(depth_start>=0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,depth_start,date_position),c(ncols1,nrows,ndepths,1))
        r <- raster(t(tmp.array1),xmn=range(lon1)[1],xmx=range(lon1)[2],ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      }
      if(box[1]>180 && box[2] >180){ ##case 3)
        x1=box[1]-360 
        x2=box[2]-360 ## both west of zero, east of dateline
        lon_start1=which.min(abs(conn$dim$lon$vals-x1))
        lon_end1=which.min(abs(conn$dim$lon$vals-x2))
        lon1 <- conn$dim$lon$vals[lon_start1:lon_end1]
        ncols1 <- length(lon1)
        if(depth_start<0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,date_position),c(ncols1,nrows,1))
        if(depth_start>=0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,depth_start,date_position),c(ncols1,nrows,ndepths,1))
        r <- raster(t(tmp.array1),xmn=range(lon1)[1]+360,xmx=range(lon1)[2]+360,ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      }
    } else { ## we're dealing with 0-360 data
      x1=box[1]
      x2=box[2]
      lon_start1=which.min(abs(conn$dim$lon$vals-x1))
      lon_end1=which.min(abs(conn$dim$lon$vals-x2))
      lon1 <- conn$dim$lon$vals[lon_start1:lon_end1]
      ncols <- length(lon1)
      if(depth_start<0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,date_position),c(ncols,nrows,1))
      if(depth_start>=0)tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,depth_start,date_position),c(ncols1,nrows,ndepths,1))
      r <- raster(t(tmp.array1),xmn=range(lon1)[1],xmx=range(lon1)[2],ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    }
    if(!lat_n_to_s){
      return(flip(r))
    }
    else{
      return(r)
    }
  }
  
  

  
  ############ 4. define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  box=c(100,295,-60,60) ## xmin,xmax,ymin,ymax ** always express xmin xmax here in 0-360 **
  
  ############ 5. download data
  print("**************************************************************************************")
  print(paste0("Starting script Get_Env_Data_B.R,"," Time is ",Sys.time()))
  tryCatch(
    expr ={
      
      #get sst ####
      # sst_url="https://hwelch:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-ANOM-V2" ## depreciated 12/15/2022
      sst_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2"
      sst_conn=nc_open(sst_url)
      dates=getTimePosition(sst_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      nearest_depth_position=-1
      if (how_different<8){
      r=subsetLatLon(sst_conn,"analysed_sst",box,nearest_date_position,nearest_depth_position,1)
      r=r-273.15 ## convert to celsius
      r2 <- raster::resample(r, template)  
      extent(r2)=extent(template)
      r_mean=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
      writeRaster(r_mean,glue("{finaldir}/sst.grd"),overwrite=T)
      rasSD=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=sd,na.rm=TRUE)
      extent(rasSD)=extent(template)
      writeRaster(rasSD,glue("{finaldir}/sst_sd.grd"),overwrite=T)
    } else{
      print(glue("Not grabbing SST data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days")) 
    }
      nc_close(sst_conn)
      
      #get PP ####
      pp_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/global-analysis-forecast-bio-001-028-daily"
      pp_conn=nc_open(pp_url)
      dates=getTimePosition(pp_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      depth=200
      nearest_depth_position=getDepthPosition(pp_conn,depth)
      if (how_different<8){
        r=subsetLatLon(pp_conn,"nppv",box,nearest_date_position,1,nearest_depth_position)
        r2 <- raster::resample(r, template)
        extent(r2)=extent(template)
        r_mean=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_mean,glue("{finaldir}/PPupper200m.grd"),overwrite=T)
      } else{
        print(glue("Not grabbing PP data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days"))
      }

      nc_close(pp_conn)
      
      #get sla eke adt ####
      eke_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/dataset-duacs-nrt-global-merged-allsat-phy-l4"
      eke_conn=nc_open(eke_url)
      dates=getTimePosition(eke_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      nearest_depth_position=-1
      if (how_different<8){
        rSLA=subsetLatLon(eke_conn,"sla",box,nearest_date_position,nearest_depth_position,0)
        rADT=subsetLatLon(eke_conn,"adt",box,nearest_date_position,nearest_depth_position,0)
        rU=subsetLatLon(eke_conn,"ugosa",box,nearest_date_position,nearest_depth_position,0)
        rV=subsetLatLon(eke_conn,"vgosa",box,nearest_date_position,nearest_depth_position,0)
      
        r2SLA <- raster::resample(rSLA, template) 
        r2ADT <- raster::resample(rADT, template) 
        r2U <- raster::resample(rU, template) 
        r2V <- raster::resample(rV, template) 
        
        extent(r2SLA)=extent(template)
        extent(r2ADT)=extent(template)
        extent(r2U)=extent(template)
        extent(r2V)=extent(template)
        
        r_meanSLA=focal(r2SLA,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_meanSLA,glue("{finaldir}/sla.grd"),overwrite=T)
        
        r_meanADT=focal(r2ADT,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_meanADT,glue("{finaldir}/adt.grd"),overwrite=T)
        
        eke=1/2*(r2U^2+r2V^2)
        l.eke=log10(eke + .001)
        r_mean=focal(l.eke,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_mean,glue("{finaldir}/eke.grd"),overwrite=T)
        
      } else{
        print(glue("Not grabbing sla eke adt data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days")) 
      }
      
      nc_close(eke_conn)
      
      #get chl ####
      chl_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/cmems_obs-oc_glo_bgc-plankton_nrt_l4-gapfree-multi-4km_P1D"
      chl_conn=nc_open(chl_url)
      dates=getTimePosition(chl_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      nearest_depth_position=-1
      if (how_different<8){
        r=subsetLatLon(chl_conn,"CHL",box,nearest_date_position,nearest_depth_position,0)
        r=log10(r+0.001) ## log chla
        r2 <- raster::resample(r, template)  
        extent(r2)=extent(template)
        r_mean=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_mean,glue("{finaldir}/l.chl.grd"),overwrite=T)
      } else{
        print(glue("Not grabbing CHL data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days")) 
      }
      
      nc_close(chl_conn)
      
      #get mld ####
      # mld_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/global-analysis-forecast-phy-001-024" ## ---> depreciated
      mld_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_anfc_0.083deg_P1D-m"
      # ftp://nrt.cmems-du.eu/Core/GLOBAL_ANALYSISFORECAST_PHY_001_024/cmems_mod_glo_phy_anfc_0.083deg_P1D-m
      # https://nrt.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_anfc_0.083deg_P1D-m
      mld_conn=nc_open(mld_url)
      dates=getTimePosition(mld_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      nearest_depth_position=-1
      if (how_different<8){
        r=subsetLatLon(mld_conn,"mlotst",box,nearest_date_position,nearest_depth_position,0)
        r2 <- raster::resample(r, template)  
        extent(r2)=extent(template)
        r_mean=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_mean,glue("{finaldir}/mld.grd"),overwrite=T)
      } else{
        print(glue("Not grabbing MLD data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days")) 
      }
      
      nc_close(mld_conn)
      
      #get oxy ####
      oxy_url="https://CMEMESusername:CMEMESpassword@nrt.cmems-du.eu/thredds/dodsC/global-analysis-forecast-bio-001-028-daily"
      oxy_conn=nc_open(oxy_url)
      dates=getTimePosition(oxy_conn,get_date)
      nearest_date_position=dates[1];nearest_date=dates[2];how_different=dates[3];notation_date=dates[4]
      depth=200
      nearest_depth_position=getDepthPosition(oxy_conn,depth)
      if (how_different<8){
        r=subsetLatLon(oxy_conn,"o2",box,nearest_date_position,nearest_depth_position,1)
        r2 <- raster::resample(r, template)
        extent(r2)=extent(template)
        r_mean=focal(r2,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE) ## resampling to 1.25
        writeRaster(r_mean,glue("{finaldir}/oxy200m.grd"),overwrite=T)
      } else{
        print(glue("Not grabbing oxy200m data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days"))
      }
      
      nc_close(oxy_conn)
      
    },
  error = function(e){
    message(glue("Variables not available {get_date}"))
    print(e)
  }
  )
  print("**************************************************************************************")
  # close(logfile)
  sink(NULL)
  
}

Get_Env_Data_B(path=path,source_path=source_path)


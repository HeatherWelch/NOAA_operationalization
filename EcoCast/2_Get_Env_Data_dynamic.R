#######Get_Env_Data B
# Get data sequence number two : 
#1. see which dynamic variables are missing
#2. if none are missing, run EcoCast
#3. attempt to download missing variables
#4. see which dynamic variables are still missing after download attempt
#5. if none are missing, run EcoCast

#Script only runs if final products are missing (will never overwrite completed products)
#RUN THIS SCRIPT AS MANY TIMES AS DESIRED OVER THE COURSE OF THE DAY

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/Ecocast/operationalization_ecocast"

## path to the load libraries r script (path to script only, /0_load_libraries.R appended in function)
source_path="/Users/heatherwelch/Dropbox/Ecocast/EcoCast-private/Code/Operationalizing_code_V4"


Get_Env_Data_B=function(path,source_path){
  
  ############ 1. Define directories
  source(paste0(source_path,"/0_load_libraries.R"))
  source(glue("{source_path}/predict_CIs_fcn.R"))
  source(glue("{source_path}/plot_EcoCast_fcn.R"))
  envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
  outdir <- paste(path,"/EcoCastRuns/",sep="")
  logdir=paste(outdir,"logs/",sep="")
  staticdir=paste0(path,"/static_variables/")
  ecocastdir=paste(outdir,"output/",sep="")
  moddir<-paste(path,"/ModRepFiles/",sep="")
  logodir=glue("{path}/logo/")
  
  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()
  most_recent=as.character(get_date-1) ##change for each user
  get_date_composite=get_date-4
  year=get_date %>% as.character() %>% substr(.,start=1,stop=4)
  month=get_date %>% as.character() %>% substr(.,start=6,stop=7)
  
  tmpdir=paste(path,"/Real_time_netcdfs_raw/temp_",get_date,sep="")
  finaldir=paste(envdir,get_date,sep="")
  
  logfile = paste(logdir,"log_",get_date,".txt",sep="") 
  sink(logfile, type=c("output","message"),append = TRUE) #set all output to templog
  
  # C. Define species weightings
  namesrisk<-c("Blue shark bycatch","Blue sharks","Sea lions","Leatherbacks","Swordfish")
  ecocastrisk<-c(-0.1,-0.1,-0.05,-0.9,0.9) #upweight swordfish a bit
  bycatchrisk<-c(-0.1,-0.1,-0.1,-0.7,0) #all non-target species

  ############ 2. load functions
  get_EnvData <- function(most_recent,get_date,get_date_composite,namesrisk,ecocastrisk,bycatchrisk,envdir,tmpdir,finaldir,path){
    
    ############ 2. Load functions
    ### A. Pauses system for a period of time to allow url requests to go through
    waitfor <- function(x){
      p1 <- proc.time()
      Sys.sleep(x)
      print(proc.time() - p1) # The cpu usage should be negligible
    }
    
    ### B. Copies gri and grd files from previous days
    copyy=function(folder){
      path_grd=paste(envdir,folder,"/",final_name,".grd",sep="")
      file.copy(path_grd,finaldir)
      path_gri=paste(envdir,folder,"/",final_name,".gri",sep="")
      file.copy(path_gri,finaldir)
    }
    
    ### C. Checks if a URL exists and returns true or false
    URLexists=function(urls){
      b=url.exists(urls,curl = getCurlHandle(.opts = RCurl::curlOptions(ssl.verifypeer=FALSE)))
      return(b)
    }
    
    ### D. An acquire function to grab envt data from erddap
    acquire_erddap=function(urls,name,final_name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
      envir = parent.frame()
      presence=URLexists(urls=urls)
      if(URLexists(urls=urls)==TRUE){
        file = paste(tmpdir,"/",name,".nc",sep="")
        print(paste("Beginning download of ",name,". Placing it in a temp directory: ",tmpdir,sep=""))
        f = CFILE(file,mode="wb")
        curlPerform(url=urls,writedata=f@ref,noprogress=FALSE, .opts = RCurl::curlOptions(ssl.verifypeer=FALSE))
        close(f)
        waitfor(3)
      }
    }
    
    ### E. An acquire function to grab envt data from CMEMS and AVISO (these have a slightly different format than erddap and therefore need a seperate download method) ### DEPRECIATED
    # acquire_cmems_aviso=function(url,date,userpwd,name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
    #   filenames=getURL(url, userpwd = userpwd,
    #                    ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,dirlistonly = TRUE) ## this is clunky by necessity. The CMEMS files are named by the date they were uploaded to the ftp site, therefore there is no way to predict the actual name of the file for the date we are interested in. So we go a roundabout way:
    #   waitfor(3)
    #   list_filenames=unlist(strsplit(filenames,".gz")) ## get a list of all the files in the CMEMS directory
    #   string=grep(date,list_filenames,value=TRUE)
    #   if(length(string)>0){
    #     string=gsub("[^[:alnum:]_.]", "", string) ## it is impossible to get rid of trailing backslashes, therefore this mess
    #     data=getBinaryURL(paste(url,string,".gz",sep=""),userpwd = userpwd,ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,noprogress=FALSE) # grab data behind url
    #     waitfor(3)
    #     con <- file(paste(tmpdir,"/",name,".nc.gz",sep=""), open = "wb") # write data to a file
    #     writeBin(data,con)
    #     waitfor(3)
    #     close(con)
    #     gunzip(paste(tmpdir,"/",name,".nc.gz",sep=""),ext="gz", FUN=gzfile) # unzip the file
    #   }
    # }
    
    ### E. An acquire function to grab envt data from CMEMS and AVISO (these have a slightly different format than erddap and therefore need a seperate download method) 
    acquire_cmems_aviso=function(url,date,userpwd,name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
      filenames=getURL(url, userpwd = userpwd,
                       ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,dirlistonly = TRUE) ## this is clunky by necessity. The CMEMS files are named by the date they were uploaded to the ftp site, therefore there is no way to predict the actual name of the file for the date we are interested in. So we go a roundabout way:
      waitfor(3)
      list_filenames=unlist(strsplit(filenames,".nc")) ## get a list of all the files in the CMEMS directory
      string=grep(date,list_filenames,value=TRUE)
      if(length(string)>0){
        string=gsub("[^[:alnum:]_.]","", string) ## it is impossible to get rid of trailing backslashes, therefore this mess
        data=getBinaryURL(paste(url,string,".nc",sep=""),userpwd = userpwd,ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,noprogress=FALSE) # grab data behind url
        waitfor(3)
        con <- file(paste(tmpdir,"/",name,".nc",sep=""), open = "wb") # write data to a file
        writeBin(data,con)
        waitfor(3)
        close(con)
      }
    }
    
    
    ############ 5. Define global objects
    #these are grabbed from sla_mean.grd in /EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-04
    template=raster() ##create template for resampling
    res(template)=0.2487562
    ncol(template)=201
    nrow(template)=201
    xmin(template)=-149.875
    xmax(template)=-99.875
    ymin(template)=10.125
    ymax(template)=60.125
    projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
    studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
    
    print("**************************************************************************************")
    print(paste0("Starting script 3_get_Env_Data_B.R,"," Time is ",Sys.time()))
    print("********************CHECKING TO SEE IF FILES ARE MISSING FROM TODAY********************")
    
    
    ############ 11.Get a list of the paths of the env variables for get_date, or the most recent path if missing
    FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
    FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
    FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
    FileList_final=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$",full.names = TRUE) # start of final list to pass to preCIs script
    return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing)
    
    ############ 11. Run EcoCast if no files are missing
    if(length(FileList_missing)==0){
      print(paste("********************No dynamic variables are missing from ",get_date,". Starting the rest of the EcoCast scripts. Time is ",Sys.time(),"********************",sep=""))
      
      print("Running function predict_CIs")
      predCIs_master(get_date=get_date,envdir = envdir,moddir=moddir,outdir = outdir,path = path,final_path_list=return_list,staticdir=staticdir)
      print(paste("Finished running function predict_CIs. ","Time is ",Sys.time(),sep=""))
      
      #Now run function to make the plots
      print("Running function plot_EcoCast")
      Run_ecocast(get_date=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,final_path_list=return_list,logodir=logodir,studyarea=studyarea,staticdir=staticdir)
      print(paste("Finished running function plot_EcoCast. ","Time is ",Sys.time(),sep=""))
      
    }else{
      print(paste("********************Files are missing from ",get_date," attempting to download now."," Time is ",Sys.time(),"********************",sep=""))}
    
    ############ 11. Attempt to download missing files
    if(length(FileList_missing)>0){
      
      ############ 3. Variable 1,2, &3: NRT MSLA SSH and U&V (now all hosted within one netcdf)
      if(("sla.grd" %in% FileList_missing)==TRUE){
        print("Downloading and preparing NRT MSLA SSH")
        print("Downloading and preparing NRT MSLA u&v")
        date=paste("l4_",gsub("-","",get_date),sep="") # get date in correct format for ftp search
        url <- paste0("ftp://nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046/dataset-duacs-nrt-global-merged-allsat-phy-l4","/",year,"/",month,"/")
        userpwd <- "CMEMESusername:CMEMESpassword"
        name="MSLA_all"
        acquire_cmems_aviso(url=url,date=date,userpwd=userpwd,name=name)
        ## process any new files
        if(file.exists(paste(tmpdir,"/MSLA_all.nc",sep=""))==TRUE){
          print(paste("Preparing MSLAh: standardizing extent and placing file in ",finaldir,sep=""))
          MSLAh=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="sla")
          r=raster::resample(MSLAh, template, method="bilinear")
          rsd=focal(r, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
          writeRaster(r,paste(finaldir,"/sla",sep=""))
          writeRaster(rsd,paste(finaldir,"/sla_sd",sep=""))
        }
        if(file.exists(paste(tmpdir,"/MSLA_all.nc",sep=""))==TRUE){
          print(paste("Preparing MSLAuv: standardizing extent, calculating l.eke and placing file in ",finaldir,sep=""))
          MSLAu=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="ugosa")
          MSLAv=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="vgosa")
          rU=raster::resample(MSLAu, template, method="bilinear") 
          rV=raster::resample(MSLAv, template, method="bilinear")
          eke<-1/2*(rU^2+rV^2)
          l.eke <- log(eke + 0.001)
          writeRaster(l.eke,paste(finaldir,"/l.eke_mean",sep=""))
        }
      }
      
      ############ 5. Variable 4: Wind  --------------------------------> depreciated ####
      # if(("ywind.grd" %in% FileList_missing)==TRUE){
      #   wind=paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOwDlyP_LonPM180.nc?v[(",get_date_composite,"T00:00:00Z):1:(",get_date_composite,"T00:00:00Z)][(10):1:(10)][(10):1:(60)][(-150):1:(-100)]",sep="")
      #   name= "ncdcOwDly"
      #   final_name= "ywind"
      #   acquire_erddap(urls=wind,name=name,final_name=name)
      #   ## process any new files
      #   if(file.exists(paste(tmpdir,"/ncdcOwDly.nc",sep=""))==TRUE){
      #     print(paste("Preparing ncdcOwDly: standardizing extent and placing file in ",finaldir,sep=""))
      #     ncdcOwDly=raster(paste(tmpdir,"/ncdcOwDly.nc",sep=""),varname="v")
      #     r=raster::resample(ncdcOwDly, template, method="bilinear")
      #     writeRaster(r,paste(finaldir,"/ywind",sep=""))
      #   }
      # }
      ############  --------------------------------> depreciated ####
      
      ############ 5. Variable 4: Wind
      if(("ywind.grd" %in% FileList_missing)==TRUE){
        wind=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQMwind1day.nc?y_wind[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(10)][(10):1:(60)][(210):1:(260)]",sep="")
        name= "erdQMwind1day"
        final_name= "ywind"
        acquire_erddap(urls=wind,name=name,final_name=name)
        ## process any new files
        if(file.exists(paste(tmpdir,"/erdQMwind1day.nc",sep=""))==TRUE){
          print(paste("Preparing erdQMwind1day: standardizing extent and placing file in ",finaldir,sep=""))
          erdQMwind1day=raster(paste(tmpdir,"/erdQMwind1day.nc",sep=""),varname="y_wind")
          r=raster::resample(rotate(erdQMwind1day), template, method="bilinear")
          writeRaster(r,paste(finaldir,"/ywind",sep=""))
        }
      }
      
      ############ 6. Variable 5: sst (GHRSST1)  --------------------------------> depreciated ####
      # if(("analysed_sst.grd" %in% FileList_missing)==TRUE){
      #   sst=paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
      #   name= "jplG1SST"
      #   acquire_erddap(urls=sst,name=name,final_name=name)
      # }
      ############ --------------------------------> depreciated ####
      
      # ############ Variable 5: sst (jplMURSST41)  --------------------------------> depreciated following emails with Scott and Gang ####
      # if(("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
      #   sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
      #   name= "jplMURSST41"
      #   acquire_erddap(urls=sst,name=name,final_name=name)
      # }
      
      ############ Variable 5: sst (NOAA_DHW_5km) 
      if(("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer
        sst=paste("https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?CRW_SST[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(60):1:(10)][(-150):1:(-100)]",sep="")
        name= "NOAA_DHW"
        acquire_erddap(urls=sst,name=name,final_name=name)
      }
      
      ############ Variable 5.1: sst (noaacwBLENDEDsstDNDaily) ## commenting this out until I can figure out the problem
      # if(!file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer
      #   sst=glue("https://coastwatch.noaa.gov//erddap/griddap/noaacwBLENDEDsstDNDaily.nc?analysed_sst%5B({get_date}T12:00:00Z):1:({get_date}T12:00:00Z)%5D%5B(9.025):1:(60.025)%5D%5B(-151.975):1:(-99.975)%5D")
      #   name= "noaacwBLENDEDsstDNDaily"
      #   acquire_erddap(urls=sst,name=name,final_name=name)
      # }
      # 
        # ############ Variable 5.1: sst (erdMWsstd1day_LonPM180) DEPRECIATED
      # if(!file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
      #   sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1sstd1day.nc?sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(60):1:(10)][(-150):1:(-100)]",sep="")
      #   name= "erdMH1sstd1day"
      #   acquire_erddap(urls=sst,name=name,final_name=name)
      # }
      
      ############ Variable 5.2: sst (jplUKMO_OSTIAv20)
      if(!file.exists(paste(tmpdir,"/noaacwBLENDEDsstDNDaily.nc",sep="")) & !file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
        sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplUKMO_OSTIAv20.nc?analysed_sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
        name= "jplUKMO_OSTIAv20"
        acquire_erddap(urls=sst,name=name,final_name=name)
      }
      
      
      ## process any new SST files  --------------------------------> depreciated ####
      # if(file.exists(paste(tmpdir,"/jplG1SST.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
      #   print("Preparing jplG1SST: regridding to .25 x .25 degree resolution")
      #   jplG1SST=raster(paste(tmpdir,"/jplG1SST.nc",sep=""),varname="SST")
      #   r=raster::resample(jplG1SST, template, method="bilinear")
      #   print(paste("jplG1SST regridding complete, placing final file in",finaldir,sep=""))
      #   writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
      #   print("Preparing jplG1SST: calculating standard deviation at native resolution")
      #   rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
      #   print("Preparing jplG1SST: regridding standard deviation layer to .25 x .25 degree resolution")
      #   rsdr=raster::resample(rsd, template, method="bilinear")
      #   print(paste("regridding complete, placing final file in",finaldir,sep=""))
      #   writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
      # }
      ## --------------------------------> depreciated ####
      
      if(file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
        print("Preparing NOAA_DHW: regridding to .25 x .25 degree resolution")
        jplG1SST=raster(paste(tmpdir,"/NOAA_DHW.nc",sep=""),varname="CRW_SST")
        r=raster::resample(jplG1SST, template, method="bilinear")
        print(paste("NOAA_DHW regridding complete, placing final file in",finaldir,sep=""))
        writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
        print("Preparing NOAA_DHW: calculating standard deviation at native resolution")
        rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
        print("Preparing NOAA_DHW: regridding standard deviation layer to .25 x .25 degree resolution")
        rsdr=raster::resample(rsd, template, method="bilinear")
        print(paste("regridding complete, placing final file in",finaldir,sep=""))
        writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
      }
      
      ## commenting this out until I can figure out the problem
      # if(file.exists(paste(tmpdir,"/noaacwBLENDEDsstDNDaily.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
      #   print("Preparing noaacwBLENDEDsstDNDaily: regridding to .25 x .25 degree resolution")
      #   noaacwBLENDEDsstDNDaily=raster(paste(tmpdir,"/noaacwBLENDEDsstDNDaily.nc",sep=""),varname="analysed_sst")
      #   r=raster::resample(noaacwBLENDEDsstDNDaily, template, method="bilinear")
      #   print(paste("noaacwBLENDEDsstDNDaily regridding complete, placing final file in",finaldir,sep=""))
      #   writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
      #   print("Preparing noaacwBLENDEDsstDNDaily: calculating standard deviation at native resolution")
      #   rsd=focal(noaacwBLENDEDsstDNDaily, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
      #   print("Preparing noaacwBLENDEDsstDNDaily: regridding standard deviation layer to .25 x .25 degree resolution")
      #   rsdr=raster::resample(rsd, template, method="bilinear")
      #   print(paste("regridding complete, placing final file in",finaldir,sep=""))
      #   writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
      # }
      
      if(file.exists(paste(tmpdir,"/jplUKMO_OSTIAv20.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
        print("Preparing jplUKMO_OSTIAv20: regridding to .25 x .25 degree resolution")
        jplG1SST=raster(paste(tmpdir,"/jplUKMO_OSTIAv20.nc",sep=""),varname="analysed_sst")
        r=raster::resample(jplG1SST, template, method="bilinear")
        print(paste("jplUKMO_OSTIAv20 regridding complete, placing final file in",finaldir,sep=""))
        writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
        print("Preparing jplUKMO_OSTIAv20: calculating standard deviation at native resolution")
        rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
        print("Preparing jplUKMO_OSTIAv20: regridding standard deviation layer to .25 x .25 degree resolution")
        rsdr=raster::resample(rsd, template, method="bilinear")
        print(paste("regridding complete, placing final file in",finaldir,sep=""))
        writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
      }
      
      ############ 7. Variable 6: DINEOF CHLA
      
      if(("l.blendChl.grd" %in% FileList_missing)==TRUE){
        chl=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc?chlor_a%5B(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-100)]",sep="")
        name= "nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily"
        final_name="l.blendChl"
        acquire_erddap(urls=chl,name=name,final_name=final_name)
        
        ############ 8. Variable 7: nesdisVHNnoaaSNPPnoaa20chlaDaily
        if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==F){ #only download if there isn't a DINEOF layer
          chlVI=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc?chl_oci[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-100)]",sep="")
          name= "nesdisVHNnoaaSNPPnoaa20chlaDaily"
          final_name= "erdVH3chla"
          acquire_erddap(urls=chlVI,name=name,final_name=final_name)
        }
        
        ############ 8. Variable 8: erdVHNchla1day
        if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==F & file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""))==F){ #only download if there isn't a DINEOF or non-DINEOF layer
          chlVI2=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla1day.nc?chla[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-110.00625)]",sep="")
          name= "erdVHNchla1day"
          final_name= "erdVHNchla1day"
          acquire_erddap(urls=chlVI2,name=name,final_name=final_name)
        }
        
        ###  --------------------------------> depreciated ####
      # if(("l.blendChl.grd" %in% FileList_missing)==TRUE){
      #   chl=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day_LonPM180.nc?chlorophyll[(",get_date_composite,"T00:00:00Z):1:(",get_date_composite,"T00:00:00Z)][(0):1:(0)][(10):1:(60)][(-150):1:(-100)]",sep="")
      #   name= "erdMBchla"
      #   final_name="l.blendChl"
      #   acquire_erddap(urls=chl,name=name,final_name=final_name)
      #   
      #   ############ 8. Variable 7: chla VIIRS
      #   if(file.exists(paste(tmpdir,"/erdMBchla.nc",sep=""))==TRUE){ #only download if there is a new MODIS layer to gap fill
      #     chlVI=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH3chla1day.nc?chla[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(60):1:(10)][(-150):1:(-100)]",sep="")
      #     name= "erdVH3chla"
      #     final_name= "erdVH3chla"
      #     acquire_erddap(urls=chlVI,name=name,final_name=final_name)
      #   }
      ###  --------------------------------> depreciated ####
        
        ## process any new files
        # Scenario 1: new layer for nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily
        if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==TRUE){
          print("Preparing nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily")
          chla=raster(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""),varname="chlor_a")
          print("Preparing erdMBchla: regridding to .25 x .25 degree resolution and taking the log")
          r=log(raster::resample(chla, template, method="bilinear")+0.001)
          print(paste("nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily regridding complete, placing final file in",finaldir,sep=""))
          writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
        }
        
        # Scenario 2: new layer for nesdisVHNnoaaSNPPnoaa20chlaDaily
        if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""))==TRUE){
          print("Preparing nesdisVHNnoaaSNPPnoaa20chlaDaily")
          chla=raster(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""),varname="chl_oci")
          print("Preparing nesdisVHNnoaaSNPPnoaa20chlaDaily: regridding to .25 x .25 degree resolution and taking the log")
          r=log(raster::resample(chla, template, method="bilinear")+0.001)
          print(paste("nesdisVHNnoaaSNPPnoaa20chlaDaily regridding complete, placing final file in",finaldir,sep=""))
          writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
        }
        
        # Scenario 3: new layer for erdVHNchla1day
        if(file.exists(paste(tmpdir,"/erdVHNchla1day.nc",sep=""))==TRUE){
          print("Preparing erdVHNchla1day")
          chla=raster(paste(tmpdir,"/erdVHNchla1day.nc",sep=""),varname="chla")
          print("Preparing erdVHNchla1day: regridding to .25 x .25 degree resolution and taking the log")
          r=log(raster::resample(chla, template, method="bilinear")+0.001)
          print(paste("erdVHNchla1day regridding complete, placing final file in",finaldir,sep=""))
          writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
        }
        
       
        
        ###  --------------------------------> depreciated ####
        # # Scenario 1: new layers for MODIS and VIIRS
        # if(file.exists(paste(tmpdir,"/erdMBchla.nc",sep=""))==TRUE && file.exists(paste(tmpdir,"/erdVH3chla.nc",sep=""))==TRUE){
        #   print("Preparing erdMBchla: blending with erdVH3chla")
        #   erdMBchla=raster(paste(tmpdir,"/erdMBchla.nc",sep=""),varname="chlorophyll")
        #   erdVH3chla=raster(paste(tmpdir,"/erdVH3chla.nc",sep=""),varname="chla")
        #   erdVH3chla_resample=raster::resample(erdVH3chla, erdMBchla, method="bilinear")
        #   blend=cover(erdMBchla,erdVH3chla_resample) #fill NAs in MB with values in VH3: check with VIIRS expert if this is the right product
        #   print("Preparing erdMBchla: regridding to .25 x .25 degree resolution and taking the log")
        #   r=log(raster::resample(blend, template, method="bilinear")+0.001)
        #   print(paste("erdMBchla regridding complete, placing final file in",finaldir,sep=""))
        #   writeRaster(r,paste(finaldir,"/l.blendChl",sep=""))
        # }
        # 
        # # Scenario 2: new layer for MODIS, no new layer for VIIRS
        # if(file.exists(paste(tmpdir,"/erdMBchla.nc",sep=""))==TRUE && file.exists(paste(tmpdir,"/erdVH3chla.nc",sep=""))==FALSE){
        #   print("Preparing erdMBchla: regridding to .25 x .25 degree resolution and taking the log (no new erdVH3chla layer to blend with)")
        #   erdMBchla=raster(paste(tmpdir,"/erdMBchla.nc",sep=""),varname="chlorophyll")
        #   r=log(raster::resample(erdMBchla, template, method="bilinear")+0.001)
        #   print(paste("erdMBchla regridding complete, placing final file in",finaldir,sep=""))
        #   writeRaster(r,paste(finaldir,"/l.blendChl",sep=""))
        # }
        ###  --------------------------------> depreciated ####
      }
      
    }
    
    ############ 11.Get a list of the paths of the env variables for get_date, or the most recent path if missing
    FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
    FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
    FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
    FileList_final=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$",full.names = TRUE) # start of final list to pass to preCIs script
    return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing)
    
    ############ 11. Run EcoCast if no files are missing
    filelist=list.files(paste(path,"/EcoCastRuns/output/mean",sep=""),pattern=as.character(get_date),full.names = TRUE)
    if(length(FileList_missing)==0 & length(filelist)==0){
      print(paste("********************No dynamic variables are missing from ",get_date,". Starting the rest of the EcoCast scripts. Time is ",Sys.time(),"********************",sep=""))
      
      print("Running function predict_CIs")
      predCIs_master(get_date=get_date,envdir = envdir,moddir= moddir,outdir = outdir,path = path,final_path_list=return_list,staticdir=staticdir)
      print(paste("Finished running function predict_CIs. ","Time is ",Sys.time(),sep=""))
      
      #Now run function to make the plots
      print("Running function plot_EcoCast")
      Run_ecocast(get_date=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,final_path_list=return_list,logodir=logodir,studyarea=studyarea,staticdir=staticdir)
      print(paste("Finished running function plot_EcoCast. ","Time is ",Sys.time(),sep=""))
      
    }
    if((length(FileList_missing)>0)==TRUE){
      print(paste("********************Files are still missing from ",get_date," after download attempt. Will attempt to download in one hour. Missing files: ********************",sep=""))
      print(FileList_missing)
    }
    
    print("**************************************************************************************")
  }
  
## check to see if a final ecocast product exists, if not, run get_EnvData()
filelist=list.files(paste(path,"/EcoCastRuns/output/mean",sep=""),pattern=as.character(get_date),full.names = TRUE)
if(length(filelist)==0){
  get_EnvData(most_recent=most_recent,get_date=get_date,get_date_composite=get_date_composite,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,envdir=envdir,tmpdir=tmpdir,finaldir=finaldir,path=path)
  #warnings()
}
}

Get_Env_Data_B(path = path,source_path = source_path)



#### script to make latest week files for Dales website

#will copy 7 most recent files into a folder and rename w latest1, latest2 etc for comprability w dales website

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/Ecocast/operationalization_ecocast"

## path to the load libraries r script (path to script only, /0_load_libraries.R appended in function)
source_path="/Users/heatherwelch/Dropbox/Ecocast/EcoCast-private/Code/Operationalizing_code_V4"

latest_folder=function(path,source_path){
  
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
  fromdir=paste0(ecocastdir,"mean/")
  latest_week=paste0(ecocastdir,"mean/latest_week/")
  
  if(!file.exists(latest_week)){dir.create(latest_week)}
  
  mean=list.files(paste0(latest_week),full.names = T)
  lapply(mean,function(x)file.remove(x))
  
  get_dates=c(Sys.Date(),Sys.Date()-1,Sys.Date()-2,Sys.Date()-3,Sys.Date()-4,Sys.Date()-5,Sys.Date()-6) %>% unlist()
  
  a=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[1],.,value=T)
  b=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[2],.,value=T)
  c=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[3],.,value=T)
  d=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[4],.,value=T)
  e=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[5],.,value=T)
  f=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[6],.,value=T)
  g=list.files(paste0(ecocastdir,"/mean"),pattern = "mean_product.png") %>% grep(get_dates[7],.,value=T)
  
  products=list(a,b,c,d,e,f,g) %>% unlist()
  #products=list(b,c,d,e,f,g) %>% unlist()
  lapply(products,function(x) file.copy(paste(fromdir,x,sep=""),latest_week))
  
  file.rename(paste0(latest_week,a),paste0(latest_week,"latest.png"))
  file.rename(paste0(latest_week,b),paste0(latest_week,"latest1.png"))
  file.rename(paste0(latest_week,c),paste0(latest_week,"latest2.png"))
  file.rename(paste0(latest_week,d),paste0(latest_week,"latest3.png"))
  file.rename(paste0(latest_week,e),paste0(latest_week,"latest4.png"))
  file.rename(paste0(latest_week,f),paste0(latest_week,"latest5.png"))
  file.rename(paste0(latest_week,g),paste0(latest_week,"latest6.png"))
  
}

latest_folder(path = path,source_path = source_path)






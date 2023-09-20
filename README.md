# NOAA_operationalization
Operationalization code for EcoCast, WhaleWatch2.0, and Top Predator Watch

## EcoCast
https://coastwatch.pfeg.noaa.gov/ecocast/  
### scripts 
**1. 1_Get_Env_Data_static.R**: Create final and temporary envdirs, acquire all static variables  
**2. 2_Get_Env_Data_dynamic.R**: Acquire and process dynamic varaiables  
**3. 3_Get_Env_Data_dynamicLag.R**: Acquire and process recent dynamic varaiables in case they are missing for target date  
**4. 4_run_EcoCast.R.R**: Run the EcoCast models over newly acquired data  
**5. 5_latest_week.R**: Package up latest data to serve    

## Top Predator Watch
https://oceanview.pfeg.noaa.gov/top-predator-watch/  
### scripts 
**1. 1_Get_Env_Data_A_albacore.R**: Create final and temporary envdirs, acquire all static variables  
**2. 2_Get_Env_Data_B_albacore.R**: Acquire and process dynamic varaiables  
**3. 3_Run_Models_OLE.R**: Run predator models over newly acquired data  
**4. 4_Create_Flyer_OLE.R.R**: Create latest flyer to serve  

## WhaleWatch2.0
https://coastwatch.pfeg.noaa.gov/projects/whalewatch2/about_whalewatch2.html   
### scripts 
**1. 1_Get_Env_Data_A.R**: Create final and temporary envdirs, acquire all static variables  
**2. 2_Get_Env_Data_B.R**: Acquire and process dynamic varaiables  
**3. 3_Run_Models.R**: Run blue whales models over newly acquired data  
**4. 4_Create_Flyer.R**: Create latest flyer to serve  
**5. 5_latest.R**: Packaged up latest data to serve  
**6. 6_indicators.R**: Update time-series indicators  





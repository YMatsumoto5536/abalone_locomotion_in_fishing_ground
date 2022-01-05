library(RNetLogo)
library(Directional)
library(pracma)

#Open Netlogo
nl.path<-"C:/Program Files/NetLogo 6.1.0/app" # choose the directory where netlogo-6.1.0.jar exists
nl.jarname<-"netlogo-6.1.0.jar" 
NLStart(nl.path,nl.jarname=nl.jarname)

#function for running the simulation
#following files should be included in the current directory
##locomotion_distance_list
##depth_splin.asc
##kelp_map0516.asc
##kelp_map0711.asc
##kelp_map920.asc
##kelp_map1205.asc
##kelp_map0326.asc


locomotion_model_NL<-function(x_place,y_place,year,day,intercept,model_shape){
  kaisou_coef_list<-read.csv("locomotion_distance_list")
  depth_kaisou_result<-data.frame(depth = NULL,kaisou = NULL, test = NULL)
  
  for (j in 1:10000){
    
    NLCommand("setup")
    NLCommand("set x_place", x_place)
    NLCommand("set y_place", y_place)
    NLCommand("set day", day)
    NLCommand("set intercept",intercept)
    NLCommand("set depth_value", 6)
    NLCommand("set kaisou_distance",0)
    NLCommand("set model_shape",model_shape)
    depth<-NLReport("depth_value")%>%unlist()%>%round(digits = 0)
    kaisou<-NLReport("kaisou_distance")%>%unlist()%>%round(digits = 0)
    
    y<-kaisou_coef_list$x[kaisou + 1] 
    scale_parameter<- y / model_shape
    NLCommand("set scale_parameter", scale_parameter)
    
    df<-data.frame(depth = NULL,kaisou = NULL, x = NULL, y = NULL, test = NULL)#
    NLDoCommand(1,"go")
    
    for (i in 1:day){
      angle<-Directional::rvonmises(1,-0.01,0.98, rads = TRUE)/(pi/180)
      
      
      depth<-NLReport("depth_value")
      kaisou<-NLReport("kaisou_distance")%>%unlist()%>%round(digits = 0)
      
      y<-kaisou_coef_list$x[kaisou + 1] 
      scale_parameter<- y / model_shape
      
      NLCommand("set angle", angle)
      NLCommand("set scale_parameter", scale_parameter)
      NLDoCommand(1,"go")
      
      x<-NLReport("x_abalone")%>%unlist()
      y<-NLReport("y_abalone")%>%unlist()
      df2<-data.frame(depth = depth,kaisou = kaisou, x = x, y = y, test = j)
      
      df<-rbind(df,df2)
      
    }
    
    depth_kaisou_result<-rbind(depth_kaisou_result,df)
  }
  
  write.csv(depth_kaisou_result,paste("fiil the directry pass where you want to save the result with here",
                                      x_place,"_",y_place,year,file_no,".csv", sep = ""))
  
}


#Running the simulation
locomotion_model_NL(87,149,2019,730,1.15719,1.271647)
#close Netlogo
NLQuit()


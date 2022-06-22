help_data=function(data,type){
  
  if(type=="Outpatients"){
    
    data=subset(data,data$Research_question %in% c("Mild outpatients","Outpatients"))
    
  }else{
    data=subset(data,data$Research_question %!in% c("Mild outpatients","Outpatients"))
    
  }
  
  
}
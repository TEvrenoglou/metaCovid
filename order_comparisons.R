order_comparisons=function(data){


N=length(data)
name=list()

for(i in 2:N){
name[[i]]=names(data[[i]])[order(names(data[[i]]))] 
data[[i]]=data[[i]][name[[i]]]
data[[1]]=""
}

return(data)
}



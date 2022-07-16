group_by_value = function(vec, value){
  minimo = vec[1]
  
  sapply(vec, function(x){
    if(x > minimo + value){
      minimo = x
    }
    return(minimo)
    
  })
}
get_significant_correlations = function(correlation_matrix, significant_level){
    
    larg_matrix = dim(correlation_matrix)[2]    
  
    map(1:larg_matrix, function(indice){
      filtro_correlacoes = abs(correlation_matrix[,indice])>significant_level
      return(correlation_matrix[filtro_correlacoes, indice])
  })  
}


part_variables_by_correlation = function(correlation_matrix, significant_level){
  matrix_names_cols = colnames(correlation_matrix)
  
  correlation_matrix_signific = get_significant_correlations(correlation_matrix, significant_level)
  
  partition_variables = list('nao_correlacionadas' = list(), 
                             'correlacionadas' = list())
  
  indice = 1
  
  for(var_of_correlation_matrix in correlation_matrix_signific){

    if(length(var_of_correlation_matrix)==1){
      partition_variables$nao_correlacionadas = append(partition_variables$nao_correlacionadas,
                                                          matrix_names_cols[indice])
    }else{
      
      partition_variables$correlacionadas = append(partition_variables$correlacionadas, 
                                                   list(var_of_correlation_matrix))
      
      len_vetor = length(partition_variables$correlacionadas)
      
      names(partition_variables$correlacionadas)[len_vetor] = matrix_names_cols[indice]
    }

    indice = indice + 1
  }
  
  return(partition_variables)
  
}
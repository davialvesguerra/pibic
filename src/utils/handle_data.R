get_matrix_significant_correlations = function(matrix_correlation, significant_level){
    
    larg_matrix = dim(matrix_correlation)[2]    
  
    map(1:larg_matrix, function(indice){
      filter_correlacoes = abs(matrix_correlation[,indice])>significant_level
      return(matrix_correlation[filter_correlacoes, indice])
  })  
}

clear_vector_signif_vars = function(vector_signif_vars, name_vector){
  
  #removing na´s and setting a shape [,1] in the vector
  vector_signif_vars = sapply(list(vector_signif_vars), function(x) x[!is.na(x)]) %>% t %>% t
  
  vec_row_names = row.names(vector_signif_vars)
  vec_filter_name_diff_name_vector = name_vector != vec_row_names
  
  vector_signif_vars = vector_signif_vars[vec_filter_name_diff_name_vector, 1]
  
  return(vector_signif_vars)
}


part_variables_by_correlation = function(matrix_correlation, significant_level){
  
  vector_names_signif_vars = matrix_names_cols = colnames(matrix_correlation)
  list_signif_vars = get_matrix_significant_correlations(matrix_correlation, significant_level)
  
  names(list_signif_vars) = matrix_names_cols
  
  list_partition_vars_by_group = list('nao_correlacionadas' = list(),
                             'correlacionadas' = list())

  
  indice = 1
  for(vector_signif_vars in list_signif_vars){
    
    name_vector = vector_names_signif_vars[indice]
    vector_signif_vars = clear_vector_signif_vars(vector_signif_vars, name_vector)
    
  
    if(length(vector_signif_vars)==0){
      list_partition_vars_by_group$nao_correlacionadas = append(list_partition_vars_by_group$nao_correlacionadas,
                                                          name_vector)
      
    }else{
      list_partition_vars_by_group$correlacionadas = append(list_partition_vars_by_group$correlacionadas,
                                                   list(vector_signif_vars ))

      len_vetor = length(list_partition_vars_by_group$correlacionadas)
      names(list_partition_vars_by_group$correlacionadas)[len_vetor] = name_vector
    }
    
    indice = indice + 1
  }
  
  return(list_partition_vars_by_group)
}





calcule_multiple_coef_contigency = function(df){
  
  num_cols = dim(df)[2]
  
  df_coef_contingency = lapply(1:num_cols, function(fixed_var){
    sapply(df, function(actual_var){ 
      
      matrix_contingency_table = table(unlist(df[fixed_var]),unlist(actual_var), useNA = 'no') 
      
      #library(DescTools)
      return(ContCoef(matrix_contingency_table))
      
    })  
  }) %>% as.data.frame
  
  colnames(df_coef_contingency) = row.names(df_coef_contingency)
  
  return(as.matrix(df_coef_contingency))
  
}



find_list_enough_vars_correlated = function(list_vars_correlated){
  
  vec_vars_correlated_with_someone = c()
  vec_all_possibles_vars_correlated = c()
  vec_names_vars_correlated = names(list_vars_correlated)
  
  cont_from_all_vars = 1
  cont_from_only_vars_correlated = 1
  for(vec_actual_vars in list_vars_correlated){
    
    vec_actual_names = names(vec_actual_vars)
    vec_index_new_vars_correlatead = !(vec_actual_names %in% vec_all_possibles_vars_correlated)
    
    vec_all_possibles_vars_correlated = append(vec_all_possibles_vars_correlated, 
                                           vec_actual_names[vec_index_new_vars_correlatead])
    

    if(mean(vec_index_new_vars_correlatead)!=1 || cont_from_all_vars == 1){
      vec_vars_correlated_with_someone[cont_from_only_vars_correlated] = vec_names_vars_correlated[cont_from_all_vars]
      cont_from_only_vars_correlated = cont_from_only_vars_correlated + 1
    }
    
    if(length(vec_all_possibles_vars_correlated) == length(vec_names_vars_correlated)){
      break
    }
    
    cont_from_all_vars = cont_from_all_vars + 1
  }
  
  return(vec_vars_correlated_with_someone)
}



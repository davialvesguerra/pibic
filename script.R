setwd('D:/pibic')

options(scipen=999)

pacman::p_load('tidyverse')

df_treino = read.csv('bancos/train.csv')
df_test = read.csv('bancos/test.csv')

colnames(df_treino) == colnames(df_test)

col_num = sapply(df_treino, typeof) == "integer" 
df_treino = df_treino[col_num]
df_treino


x1 = unlist(df_treino['GrLivArea']) %>% as.vector
x2 = unlist(df_treino['LotArea']) %>% as.vector
x3 = unlist(df_treino['MasVnrArea']) %>% as.vector

y = unlist(df_treino['SalePrice']) %>% as.vector

model = lm(y~x1)
data.frame(x1,y) %>% 
  ggplot(aes(x1,y))+
  geom_point()+
  geom_smooth(method=lm, se=F)


y_pred = predict(model, data.frame(x1,x2,x3))

residuo = (y-y_pred)



plot(residuo)





model = lm(y~x2)
data.frame(x2,y) %>% 
  ggplot(aes(x2,y))+
  geom_point()+
  geom_smooth(method=lm, se=F)






model = lm(y~x3+x2+x1)
data.frame(x3,y) %>% 
  ggplot(aes(x3,y))+
  geom_point()+
  geom_smooth(method=lm, se=F)

mse = y_pre






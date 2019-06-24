scy <-
function(usedcombinations,yearusedcombinations,hide_color=NA){
usedcombinations<-as.matrix(usedcombinations)
cores_usadas<-sort(unique(c(usedcombinations)))
if(!is.na(hide_color[1])){cores_usadas<- setdiff(cores_usadas,hide_color)}
anos_estudados<-sort(unique(c(yearusedcombinations)),decreasing = T)
matrix_usados<-matrix(0,ncol=length(cores_usadas),nrow=length(anos_estudados))
colnames(matrix_usados)<-cores_usadas
rownames(matrix_usados)<-anos_estudados
for(l in 1:length(anos_estudados)){
dados_ano <-subset(usedcombinations, yearusedcombinations==anos_estudados[l]) # Criar tabela do ano
for(c in 1:length(cores_usadas)){ # fazer as contas para dentro da tabela
cores_usadas_ano<-c(dados_ano)
n_cor_usada<-length(which(cores_usadas_ano==cores_usadas[c]))
matrix_usados[l,c]<-n_cor_usada}}
return(matrix_usados)}

erc <-
function(usedcombinations,yearusedcombinations,currentyear=NA,yearsurvival=NA,lifespan = NA,hide_color=NA){
#Validar dados
if(is.na(currentyear)){
currentyear<-as.numeric(format(Sys.time(), "%Y"))
warning(paste("You do not provide the current year, it was assume as",currentyear))
 
}
if(is.na(yearsurvival)){stop("You must provide an estimation of year survival")}
matrix_usados<-scy(usedcombinations=usedcombinations,
yearusedcombinations=yearusedcombinations,hide_color=hide_color)
matrix_restam<-matrix(NA,nrow=length(matrix_usados[,1]),ncol=length(matrix_usados[1,]))
colnames(matrix_restam)<-colnames(matrix_usados); rownames(matrix_restam)<-rownames(matrix_usados)
anos_estudados<-sort(unique(c(yearusedcombinations)),decreasing = T)
# Estimativa de quantas anilhas restam
for(l in 1:length(matrix_usados[,1])){
time_since_band<-currentyear-anos_estudados[l]
for(c in 1:length(matrix_usados[1,])){
matrix_restam[l,c]<-round((matrix_usados[l,c]*(yearsurvival^time_since_band)),0)}}
# Caso foi fornecido o lifespan, remover os que ja morreram
if(!is.na(lifespan)){
matrix_restam<-matrix_restam[-which(rownames(matrix_restam)<currentyear-lifespan),]}
return(matrix_restam)}

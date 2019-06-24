esdataset <-
function(tag_sheet,emptyname,g1=NA, g2=NA, g3=NA, g4=NA, g5=NA, g6=NA){
para_rodar<-as.matrix(tag_sheet)
resposta<-as.matrix(tag_sheet)
for(i in 1:length(para_rodar[,1])){
tab_sin_aux<-escombination(para_rodar[i,],emptyname,g1, g2, g3, g4, g5, g6)
resposta<-unique(rbind(resposta,tab_sin_aux))}
rownames(resposta)<-1:length(resposta[,1])
return(resposta)}

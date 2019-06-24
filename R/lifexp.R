lifexp <-
function(ntag,colorsname,nspecial=0, name1="Metal",name2="EMPTY", location1=1, location2=2,nspecial1=1, nspecial2=1,currentyear=NA,yearsurvival=1,lifespan=NA,yearusedcombinations,usedcombinations,speed=1,ignorecolor=NA){
if(!length(usedcombinations[,1])==length(yearusedcombinations)){
stop("Review your databse, all combinations must have the information of the year")}
if(is.na(currentyear)){
currentyear<-as.numeric(format(Sys.time(), "%Y"))
warning(paste("You do not provide the current year, it was assume as",currentyear))
 
}
if(is.na(yearsurvival)){stop("You must provide an estimation of year survival")}
if(nspecial>2){stop("Maximum of 2 special band allowed")}
colorsname<-as.vector(colorsname)
if(!is.na(lifespan)){
usedcombinations<-subset(usedcombinations[,1:4], yearusedcombinations>(currentyear-lifespan))
yearusedcombinations<-subset(yearusedcombinations, yearusedcombinations>(currentyear-lifespan))}
matrix_usados<-scy(usedcombinations=usedcombinations,
yearusedcombinations=yearusedcombinations)
matrix_restam<-erc(usedcombinations=usedcombinations,
yearusedcombinations=yearusedcombinations,currentyear=currentyear,yearsurvival=yearsurvival)
if(!is.na(ignorecolor)[1]){
aux_remocao=NULL
for(i in 1:length(colnames(matrix_usados))){
for(z in 1:length(ignorecolor)){
if(intersect(colnames(matrix_usados), ignorecolor[z]) == colnames(matrix_usados)[i])
{aux_remocao<-c(aux_remocao,i)}}}
matrix_restam<-matrix_restam[,-aux_remocao]}
total_restante_nat<- colSums(matrix_restam)
corection_factor<-total_restante_nat/max(total_restante_nat)
ambiente_aux<-rep(0,length(colorsname))
names(ambiente_aux)<-colorsname
cores_compartilhadas<-intersect(names(ambiente_aux), names(corection_factor))
for(i in 1:length(cores_compartilhadas)){
posicao_correcao<-which(names(corection_factor)==cores_compartilhadas[i])
posicao_ambiente<-which(names(ambiente_aux)==cores_compartilhadas[i])
ambiente_aux[posicao_ambiente]<-corection_factor[posicao_correcao]}
total_ambiente<-round((1-(speed*ambiente_aux)),5) 
if(nspecial==0){amostra<-sample(x=names(total_ambiente),size=ntag,prob=total_ambiente)}else{
amostra<-rep(NA,ntag)
if(length(location1)==1){auxpst<-location1}else{auxpst<-sample(x=location1,size=nspecial1)}
amostra[auxpst]<-name1
if(nspecial==2){
sobroup<-intersect(which(is.na(amostra)), location2)
if(length(sobroup)==1){auxpst<-sobroup}else{auxpst<-sample(x=sobroup,size=nspecial2)}
amostra[auxpst]<-name2}
para_pintar<-which(is.na(amostra))
amostra[para_pintar]<-sample(x=names(total_ambiente),size=length(para_pintar),prob=total_ambiente)}
amostra<-as.vector(amostra)
return(amostra)}

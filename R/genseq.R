genseq <-
function(ncombinations=100, ntag=4,colorsname,gen_method="allequal",usedcombinations=NA, colorsf=NA, nspecial=0, name1="Metal",name2="EMPTY", location1=1,location2=2,nspecial1=1,nspecial2=1, emptyused=FALSE,emptyname="EMPTY", currentyear=NA,yearsurvival=1,lifespan=NA,iotf=FALSE, yearusedcombinations=NA,speed=1, ignorecolor=NA,g1=NA, g2=NA, g3=NA, g4=NA, g5=NA, g6=NA,parameterslist=NA){
# Checking data input
if(ncombinations<1){stop("You must request a positive number of combinations")}
if(nspecial>2){stop("Maximum of 2 special band allowed")}
### Checking pre-used combinations
# Checking iotf input data
if(iotf==TRUE){
if(is.na(lifespan)){stop("You must provide lifespan information")}
if(is.na(yearusedcombinations[1]))
{stop("Provide information of when combinations were used (yearusedcombinations argument)")}
if(is.na(currentyear)){currentyear<-as.numeric(format(Sys.time(), "%Y"))
warning(paste("You do not provide the current year, it was assume as",currentyear))
 
}
# Remove older than lifespan
idade_uso<-currentyear-yearusedcombinations
usedcombinations<-usedcombinations[which(idade_uso<=lifespan),]}
UniqComb<-unique(usedcombinations)
UniqComb<-as.matrix(UniqComb)
ncorusada<-length(UniqComb[1,])
if(!ncorusada==ntag&!is.na(UniqComb[1,1])){stop("
Inconsistence: ntag to not match with the number of tags used in pre-used combinations
For further possibilities see: arguments emptyused and emptyname in help(genseq)")}
# Checking for duplicates in pre-used combinations
if(is.na(UniqComb[1,1])){UniqComb<-matrix(data=NA,nrow =1, ncol = ntag)}else{
if(length(UniqComb[,1])>1){tdob<-
length(usedcombinations[,1])-length(UniqComb[,1])
if(tdob>0){warning(paste("Warning: Your pre-used data has non-exclusive combinations.
The difference between total number of pre used combinations and exclusive is",tdob)); }}}
mcombs<-(length(colorsname)^ntag)-length(UniqComb[,1])
# Check if ncombinations is greater than the maximum number of possible combinations
if(mcombs<ncombinations){
stop("Limitation on new possible combinations, ncombinations must be lower than", mcombs)}
# Define parameters for combination generation
if(is.na(parameterslist)){
if(gen_method=="allequal"){
parameterslist<-list(ntag=ntag,colorsname=colorsname,nspecial=nspecial, name1=name1,name2=name2,
 location1=location1, location2=location2,nspecial1=nspecial1,nspecial2=nspecial2)}else{
if(gen_method=="vfrequency"){
parameterslist<-list(ntag=ntag,colorsf=colorsf,colorsname=colorsname,nspecial=nspecial,
 name1=name1,name2=name2, location1=location1, location2=location2,
nspecial1=nspecial1,nspecial2=nspecial2)
}else{
if(gen_method=="lifexp"){
if(!length(usedcombinations[,1])==length(yearusedcombinations)){
stop("Review your databse, all combinations must have the information of the year")}
if(is.na(currentyear)){
currentyear<-as.numeric(format(Sys.time(), "%Y"))
warning(paste("You do not provide the current year, it was assume as",currentyear))
 
}
if(is.na(yearsurvival)){stop("You must provide an estimation of year survival")}
parameterslist<-list(ntag=ntag,colorsname=colorsname,nspecial=nspecial, name1=name1,name2=name2,
 location1=location1, location2=location2,nspecial1=nspecial1, nspecial2=nspecial2,
currentyear=currentyear, yearsurvival=yearsurvival,lifespan=lifespan, 
yearusedcombinations=yearusedcombinations, usedcombinations=usedcombinations,
speed=speed,ignorecolor=ignorecolor)
}else{stop("For methods out of 'GenTag' package, inform parameters in parameterslist argument")}}}}
# Output
resposta<-matrix(data=NA,nrow =ncombinations, ncol = ntag)
# Combinations generator
while(sum(!is.na(resposta[,1])) < ncombinations ) {
SComb<-do.call(gen_method, parameterslist)# Sample a combination
ftl <-rbind(subset(UniqComb,!is.na(UniqComb[,1])),subset(resposta,!is.na(resposta[,1])))
tag_sheet.test<-rbind(ftl,SComb)
if(emptyused==TRUE){
SiSComb<-escombination(row_under_review=SComb,
emptyname=emptyname,g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6)
SiSComb<-as.matrix(SiSComb)
SComb<-matrix(SComb,ncol=length(SComb))
SComb_com_sinonimos<-rbind(SComb,SiSComb)
SComb_com_sinonimos<-unique(SComb_com_sinonimos)
tag_sheet.test<-rbind(ftl,SComb_com_sinonimos)}
test.duplicada<-sum(duplicated(tag_sheet.test))
if(test.duplicada==0){
nova.linha<-min(which(is.na(resposta[,1])==TRUE))
resposta[nova.linha,]<-SComb }}
rownames(resposta)<-1:length(resposta[,1])
colnames(resposta)<-paste0("Tag_",1:length(resposta[1,]))
if(length(usedcombinations)>1){colnames(resposta)<-colnames(usedcombinations)}
return(resposta)}

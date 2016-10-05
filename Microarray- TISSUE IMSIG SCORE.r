setwd("") 
y=read.table("", header=T, row.names=1, sep="\t")

pdata=read.csv("tissue.csv", header=T)
macrophages <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Macrophages", 1]),]
monocytes <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Monocytes", 1]),]
bcell <- y[rownames(y) %in% as.character(pdata[pdata$cell == "B cells", 1]),]
neutrophils <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Neutrophils", 1]),]
nkcells <- y[rownames(y) %in% as.character(pdata[pdata$cell == "NK cells", 1]),]
plasmacells <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Plasma cells", 1]),]
platelets <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Platelets", 1]),]
Tcells <- y[rownames(y) %in% as.character(pdata[pdata$cell == "T cells", 1]),]
Ribosomes <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Ribosomes", 1]),]
IFN <- y[rownames(y) %in% as.character(pdata[pdata$cell == "IFN", 1]),]
CellCycle <- y[rownames(y) %in% as.character(pdata[pdata$cell == "Cell Cycle", 1]),]

macrophage=sd(colMeans(macrophages))
monocyte=sd(colMeans(monocytes))
bcells=sd(colMeans(bcell))
neutrophil=sd(colMeans(neutrophils))
nkcell=sd(colMeans(nkcells))
plamacell=sd(colMeans(plasmacells))
platelet=sd(colMeans(platelets))
tcell=sd(colMeans(Tcells))
Ribosome=sd(colMeans(Ribosomes))
IFNs=sd(colMeans(IFN))
CellCycles=sd(colMeans(CellCycle))
SD= rbind(macrophage,monocyte,bcells,neutrophil,nkcell,plamacell,platelet,tcell,Ribosome, IFNs, CellCycles)
colnames(SD)="SD"

#Calculate average correlation value

y_macr=cor(t(macrophages),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
macrophage=m2["Median"]

y_macr=cor(t(monocytes),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
monocyte=m2["Median"]

y_macr=cor(t(bcell),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
bcells=m2["Median"]

y_macr=cor(t(neutrophils),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
neutrophil=m2["Median"]

y_macr=cor(t(nkcells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
nkcell=m2["Median"]

y_macr=cor(t(plasmacells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
plamacell=m2["Median"]

y_macr=cor(t(platelets),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
platelet=m2["Median"]

y_macr=cor(t(Tcells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
tcell=m2["Median"]

y_macr=cor(t(Ribosomes),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
Ribosome=m2["Median"]

y_macr=cor(t(IFN),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
IFNs=m2["Median"]

y_macr=cor(t(CellCycle),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m2=m1[,3]
m2=summary(m2)
CellCycles=m2["Median"]



R= rbind(macrophage,monocyte,bcells,neutrophil,nkcell,plamacell,platelet,tcell,Ribosome, IFNs, CellCycles)
colnames(R)="Average Correlation"

#Edges Calculator

for (i in 50:99)
{
y_macr=cor(t(macrophages),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq > (as.numeric(paste(0,i, sep="."))))
macrophage= dim(m1)[1]


y_macr=cor(t(monocytes),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
monocyte= dim(m1)[1]



y_macr=cor(t(bcell),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
bcells= dim(m1)[1]


y_macr=cor(t(neutrophils),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
neutrophil= dim(m1)[1]



y_macr=cor(t(nkcells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
nkcell= dim(m1)[1]



y_macr=cor(t(plasmacells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
plamacell= dim(m1)[1]



y_macr=cor(t(platelets),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
platelet= dim(m1)[1]



y_macr=cor(t(Tcells),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
tcell= dim(m1)[1]


y_macr=cor(t(Ribosomes),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
Ribosome= dim(m1)[1]


y_macr=cor(t(IFN),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
IFNs= dim(m1)[1]


y_macr=cor(t(CellCycle),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
CellCycles= dim(m1)[1]

assign(paste("Edge", i-49, sep="_"), rbind(macrophage,monocyte,bcells,neutrophil,nkcell,plamacell,platelet,tcell,Ribosome, IFNs, CellCycles))
}
Edge = Edge_1
for (i in 2:50) Edge <- cbind(Edge, get(paste("Edge",i, sep="_")))


#Nodes Calculator
all <- y[rownames(y) %in% as.character(pdata[pdata$type == "tissue", 1]),]
dim(all)

for (i in 50:99)
{
y_macr=cor(t(all),method="pearson")
y_macr [lower.tri(y_macr,diag=TRUE)]<-NA
m =as.data.frame(as.table(y_macr))
m1=subset(m,!is.na(m $Freq))
m1= subset(m1, m1 $Freq>(as.numeric(paste(0,i, sep="."))))
m2=unique(m1['Var1'])
m3=unique(m1['Var2'])
colnames(m2)[1]="gene"
colnames(m3)[1]="gene"
m4=rbind(m2,m3)
m5=unique(m4)
 
macrophages <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Macrophages", 1]),]
monocytes <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Monocytes", 1]),]
bcell <- m5[m5$gene %in% as.character(pdata[pdata$cell == "B cells", 1]),]
neutrophils <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Neutrophils", 1]),]
nkcells <- m5[m5$gene %in% as.character(pdata[pdata$cell == "NK cells", 1]),]
plasmacells <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Plasma cells", 1]),]
platelets <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Platelets", 1]),]
Tcells <- m5[m5$gene %in% as.character(pdata[pdata$cell == "T cells", 1]),]
Ribosomes <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Ribosomes", 1]),]
IFN <- m5[m5$gene %in% as.character(pdata[pdata$cell == "IFN", 1]),]
CellCycle <- m5[m5$gene %in% as.character(pdata[pdata$cell == "Cell Cycle", 1]),]

macrophage=length(macrophages)
monocyte=length(monocytes)
bcells=length(bcell)
neutrophil=length(neutrophils)
nkcell=length(nkcells)
plamacell=length(plasmacells)
platelet=length(platelets)
tcell=length(Tcells)
Ribosome=length(Ribosomes)
IFNs=length(IFN)
CellCycles=length(CellCycle)

assign(paste("Node", i-49, sep="_"), rbind(macrophage,monocyte,bcells,neutrophil,nkcell,plamacell,platelet,tcell,Ribosome, IFNs, CellCycles))
}
Node = Node_1
for (i in 2:50) Node <- cbind(Node, get(paste("Node",i, sep="_")))
final=cbind(SD,R)

pdata1=read.csv("pdata_edge.csv", header=T, row.names=1)
final1=cbind(final,pdata1)

for (i in 1: 50)
{
score= ((final1[,2]/final1[,1])*(Edge[,i]/final1[,4])*(Node[,i]/final1[,6]))*100
assign(paste("score", i, sep = "_"), score)
}
score = score_1
for (i in 2:50) score <- cbind(score, get(paste("score",i, sep="_")))

colnames(score)= c(0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99
)

library(plyr)
macrophages= 1- ((count(score[1,] < 20)[2,2])/50)
monocytes= 1- ((count(score[2,] < 20)[2,2])/50)
bcells= 1- ((count(score[3,] < 20)[2,2])/50)
neutrophils=1- ((count(score[4,] < 20)[2,2])/50)
nkcells=1- ((count(score[5,] < 20)[2,2])/50)
plasmacells=1- ((count(score[6,] < 20)[2,2])/50)
platelets=1- ((count(score[7,] < 20)[2,2])/50)
tcells=1- ((count(score[8,] < 20)[2,2])/50)
Ribosomes=1- ((count(score[9,] < 20)[2,2])/50)
IFNs=1- ((count(score[10,] < 20)[2,2])/50)
CellCycle=1- ((count(score[11,] < 20)[2,2])/50)

FinalScore= cbind(macrophages,monocytes,bcells,neutrophils,nkcells,plasmacells,platelets,tcells,Ribosomes,IFNs,CellCycle)
colnames(FinalScore)= c("Macrophages", "Monocytes", "B cells", "Neutrophils", "NK cells", "Plasma cells", "Platelets", "T cells", "Traslational activity", "Interferon signalling", "Cell cycle")
write.table(FinalScore,file="ImSig Score-Microarray-Tissue.txt", sep="\t")

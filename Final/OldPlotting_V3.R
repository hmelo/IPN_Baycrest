plot(plsca.res$TExPosition.Data$fi,pch=20,col=add.alpha("grey80"),cex=.5,asp=NA,axes=F)
abline(h=0)
abline(v=0)

fi.1or2 <- which(rowSums(fi.bsrs$sig.boot.ratios[,1:2])>0)
fi.1 <- setdiff(fi.1or2,which(fi.bsrs$sig.boot.ratios[,2]))
fi.2 <- setdiff(fi.1or2,which(fi.bsrs$sig.boot.ratios[,1]))
fi.1and2 <- which(fi.bsrs$sig.boot.ratios[,1]==1 & fi.bsrs$sig.boot.ratios[,2]==1)

points(plsca.res$TExPosition.Data$fi[fi.1,],col="green",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fi[fi.1,],rownames(plsca.res$TExPosition.Data$fi[fi.1,]),col="green",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fi[fi.2,],col="purple",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fi[fi.2,],rownames(plsca.res$TExPosition.Data$fi[fi.2,]),col="purple",pch=20,pos=3)

#points(plsca.res$TExPosition.Data$fi[fi.1and2,],col="red",pch=20,cex=2)
#text(plsca.res$TExPosition.Data$fi[fi.1and2,],rownames(plsca.res$TExPosition.Data$fi[fi.1and2,]),col="red",pch=20,pos=3)


## OK now plot 1 & 2 with grey80 dots then sig dots
plot(plsca.res$TExPosition.Data$fj,pch=20,col=add.alpha("grey80"),cex=.5,asp=NA,axes=F)
abline(h=0)
abline(v=0)

fj.1or2 <- which(rowSums(fj.bsrs$sig.boot.ratios[,1:2])>0)
fj.1 <- setdiff(fj.1or2,which(fj.bsrs$sig.boot.ratios[,2]))
fj.2 <- setdiff(fj.1or2,which(fj.bsrs$sig.boot.ratios[,1]))
fj.1and2 <- which(fj.bsrs$sig.boot.ratios[,1]==1 & fj.bsrs$sig.boot.ratios[,2]==1)

points(plsca.res$TExPosition.Data$fj[fj.1,],col="green",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.1,],rownames(plsca.res$TExPosition.Data$fj[fj.1,]),col="green",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fj[fj.2,],col="purple",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.2,],rownames(plsca.res$TExPosition.Data$fj[fj.2,]),col="purple",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fj[fj.1and2,],col="red",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.1and2,],rownames(plsca.res$TExPosition.Data$fj[fj.1and2,]),col="red",pch=20,pos=3)


## OK now plot 1 & 2 with grey80 dots then sig dots
plot(plsca.res$TExPosition.Data$fi[,c(2:3)],pch=20,col=add.alpha("grey80"),cex=.5,asp=NA,axes=F)
abline(h=0)
abline(v=0)

fi.2or3 <- which(rowSums(fi.bsrs$sig.boot.ratios[,2:3])>0)
fi.2 <- setdiff(fi.2or3,which(fi.bsrs$sig.boot.ratios[,3]))
fi.3 <- setdiff(fi.2or3,which(fi.bsrs$sig.boot.ratios[,2]))
fi.2and3 <- which(fi.bsrs$sig.boot.ratios[,2]==1 & fi.bsrs$sig.boot.ratios[,3]==1)

points(plsca.res$TExPosition.Data$fi[fi.2,c(2:3)],col="green",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fi[fi.2,c(2:3)],rownames(plsca.res$TExPosition.Data$fi[fi.2,c(2:3)]),col="green",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fi[fi.3,c(2:3)],col="purple",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fi[fi.3,c(2:3)],rownames(plsca.res$TExPosition.Data$fi[fi.3,c(2:3)]),col="purple",pch=20,pos=3)

#points(plsca.res$TExPosition.Data$fi[fi.2and3,c(2:3)],col="red",pch=20,cex=2)
#text(plsca.res$TExPosition.Data$fi[fi.2and3,c(2:3)],rownames(plsca.res$TExPosition.Data$fi[fi.2and3,c(2:3)]),col="red",pch=20,pos=3)

## OK now plot 1 & 2 with grey80 dots then sig dots
plot(plsca.res$TExPosition.Data$fj[,c(2:3)],pch=20,col=add.alpha("grey80"),cex=.5,asp=NA,axes=F)
abline(h=0)
abline(v=0)

fj.2or2 <- which(rowSums(fj.bsrs$sig.boot.ratios[,2:3])>0)
fj.2 <- setdiff(fj.2or2,which(fj.bsrs$sig.boot.ratios[,3]))
fj.3 <- setdiff(fj.2or2,which(fj.bsrs$sig.boot.ratios[,2]))
fj.2and3 <- which(fj.bsrs$sig.boot.ratios[,2]==1 & fj.bsrs$sig.boot.ratios[,3]==1)

points(plsca.res$TExPosition.Data$fj[fj.2,c(2:3)],col="green",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.2,c(2:3)],rownames(plsca.res$TExPosition.Data$fj[fj.2,c(2:3)]),col="green",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fj[fj.3,c(2:3)],col="purple",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.3,c(2:3)],rownames(plsca.res$TExPosition.Data$fj[fj.3,c(2:3)]),col="purple",pch=20,pos=3)

points(plsca.res$TExPosition.Data$fj[fj.2and3,c(2:3)],col="red",pch=20,cex=2)
text(plsca.res$TExPosition.Data$fj[fj.2and3,c(2:3)],rownames(plsca.res$TExPosition.Data$fj[fj.2and3,c(2:3)]),col="red",pch=20,pos=3)
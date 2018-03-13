# Question 1
NAm2 = read.table("NAm2.txt", header=TRUE)

names=unique(NAm2$Pop)
npop=length(names)
coord=unique(NAm2[,c("Pop","long","lat")]) #coordinates for each pop
colPalette=rep(c("black","red","cyan","orange","brown","blue","pink","purple","darkgreen"),3)
pch=rep(c(16,15,25),each=9)
plot(coord[,c("long","lat")],pch=pch,col=colPalette,asp=1)
# asp allows to have the correct ratio between  axis  longitude and latitude
# Then the map is not deformed  
legend("bottomleft",legend=names,col=colPalette,lty=-1,pch=pch,cex=.75,ncol=2,lwd=2)
library(maps);map("world",add=T)

NAaux = NAm2[,-c(1:7)]
#sink("longLog.txt")
y <- lm(long ~ ., data = NAaux) # NAaux[, c(1:3)]
summary(y)
#sink()

prcomp(rank=2, NAm2[,-c(1:8)])

# Look at : https://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues

pcaNAm2 = prcomp(NAm2[,-c(1:8)], scale=TRUE)

caxes=c(5,6)
plot(pcaNAm2$x[,caxes],col="white")
for (i in 1:npop) {
  print(names[i])
  lines(pcaNAm2$x[which(NAm2[,3]==names[i]),caxes],
        type="p",col=colPalette[i],pch=pch[i])
}
legend("bottomleft",legend=names,col=colPalette,lty=-
         1,pch=pch,cex=.75,ncol=3,lwd=2)

prop = summary(pcaNAm2)$importance[2,]
res = rep(0, 494)
x=0
for (i in 1:494) {
  x = x + prop[i]
  res[i] = x - i/494
}

plot(c(1:494), res, type="l")
i = which(res == max(res))
i
res[i] + i/494

sample(c(rep(0:9, each=49),1,2,3,4))

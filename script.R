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


#Question 4
latlongaxes=c(1:250)
lmlat <- lm(NAm2$lat~pcaNAm2$x[,latlongaxes])
lmlong <- lm(NAm2$long~pcaNAm2$x[,latlongaxes])
plot(lmlong$fitted.values,lmlat$fitted.values,col="white", asp=1)
for (i in 1:npop) {
  print(names[i])
  lines(lmlong$fitted.values[which(NAm2[,3]==names[i])],lmlat$fitted.values[which(NAm2[,3]==names[i])],type="p",col=colPalette[i],pch=pch[i]
  )
}
legend("bottomleft",legend=names,col=colPalette,lty=-1,pch=pch,cex=.75,ncol=2,lwd=2)
map("world",add=T)
# values <- which(NAm[,3]==names[i])
m1 <- matrix(c(lmlong$fitted.values, lmlat$fitted.values), ncol = 2, byrow = FALSE)
m2 <- matrix(c(NAm2$long, NAm2$lat), ncol=2)
dist <- fields::rdist.earth(m1, m2, miles=FALSE)
mea <- mean(diag(dist))


cross.validation <- function(naxes) {
  set = sample(c(rep(0:9, each=49),1,2,3,4))
  #b1
  predictedCoord = data.frame(matrix(ncol=2, nrow=length(set)))
  colnames(predictedCoord) <- c("longitude", "latitude")
  
  pcalong=data.frame(cbind(long=NAm2[,c("long")],pcaNAm2$x[,c(1:naxes)]))
  pcalat=data.frame(cbind(lat=NAm2[,c("lat")],pcaNAm2$x[,c(1:naxes)]))
  pca=pcaNAm2$x[,c(1:naxes)]
  
  for (i in 0:9) {
    lmlat <- lm(lat~ ., dat=pcalat, subset=set != i)
    lmlong <- lm(long~ ., data = subset(pcalong, set != i))
    predictedCoord[set == i,"latitude"] <- predict(lmlat, data.frame(subset(pca, set == i)))
    predictedCoord[set == i,"longitude"] <- predict(lmlong, data.frame(subset(pca, set == i)))
  }
  m1 <- matrix(c(predictedCoord[,"longitude"], predictedCoord[,"latitude"]), ncol=2)
  m2 <- matrix(c(NAm2$long, NAm2$lat), ncol=2)
  dist <- fields::rdist.earth(m1, m2, miles=FALSE)
  return(mean(diag(dist)))
}
res <- NULL
for (i in seq(2,440,by=10)) {
  res <- c(res, cross.validation(i))
}
plot(seq(2,440,by=10), res, type="l")

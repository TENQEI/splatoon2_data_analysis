library(PEIP)
library(scales)
#library(RColorBrewer)

#read csv file from ikaWidget
ikaw = read.csv("ikaWidgetCSV_*.csv") #"*" is replacable
#select record: SplatZones with available X Power
sz = ikaw[grep("splat_zonesgachi",ikaw$gameKey)]
sz = sz[sz$xPower!=0,]

#evaluation of winning: Counts
d = as.vector(sz$myCount - sz$otherCount)
win = as.vector(sz$win)
for(i in 1:length(win)){
    if(win[i] == 0) win[i] = -1
}

#player X power over 8-player mean X power
xpw = cbind(sz$xPower,sz$gachiEstimateXPower)
xpo = (xpw-mean(xpw))/mean(xpw)

#players" records are (K)ill, (A)ssist, (D)eath, (S)pecial and (P)aintpoints
#player record
p1 = cbind(sz$playerKill,sz$playerAssist,sz$playerDeath,sz$playerSpecial,sz$playerPaintPoint)

#teammates record
alp = cbind(sz$alpha1Kill,sz$alpha1Assist,sz$alpha1Death,sz$alpha1Special,sz$alpha1PaintPoint,sz$alpha2Kill,sz$alpha2Assist,sz$alpha2Death,sz$alpha2Special,sz$alpha2PaintPoint,sz$alpha3Kill,sz$alpha3Assist,sz$alpha3Death,sz$alpha3Special,sz$alpha3PaintPoint)

#opponent record
bet = cbind(sz$bravo1Kill,sz$bravo1Assist,sz$bravo1Death,sz$bravo1Special,sz$bravo1PaintPoint,sz$bravo2Kill,sz$bravo2Assist,sz$bravo2Death,sz$bravo2Special,sz$bravo2PaintPoint,sz$bravo3Kill,sz$bravo3Assist,sz$bravo3Death,sz$bravo3Special,sz$bravo3PaintPoint,sz$bravo4Kill,sz$bravo4Assist,sz$bravo4Death,sz$bravo4Special,sz$bravo4PaintPoint)

#sum for average teammates/opponents records
asum = alp[,1:5]+alp[,6:10]+alp[,11:15]
bsum = bet[,1:5]+bet[,6:10]+bet[,11:15]+bet[,16:20]

#regularization
rg <- function(i){
    j = (i-mean(i))/mean(i)
    return(j)
}
g = rg(cbind(xpo,p1,asum,bsum))

#leat square liner regression (100 iterations)
m = cgls(Gmat=g, dee=d, niter=50)

#plot trade-off curve
#plot(m$eta,m$rho,type="l")

#check if successfully prediction
pred = g %*% m$X
ck = vector(length = ncol(pred))
for(i in 1:ncol(pred)){
    ck[i] = table(sign(pred[,i] * win))[2]/length(win)
}
png(file = 'splatoon2_model_sccess_rate.png', width = 800, height = 600)
plot(ck, xlab = "iterations", ylab = "success rate", yaxt ="n", type="l")
axis(2, at=pretty(ck), lab=paste0(pretty(ck)*100,"%"))
title("cgls prediction success rate")
dev.off()

#take log scale and select inversion of lower residuals
mx = m$X[,which(ck>0.85)]
cko = ck[which(ck>0.85)]
for(i in 1:length(mx)){
    if(mx[i]==0) {mx[i]=0}
    if(mx[i]>0) {mx[i]=log(mx[i],base=10)}
    if(mx[i]<0) mx[i]=-log(-mx[i],base=10)
}

#create labels
lb = c("Xpower", paste0("player",c("K","A","D","S","P")), paste0("ateam",c("K","A","D","S","P")), paste0("bteam",c("K","A","D","S","P")))

#plot transparent points
png(file = "splatoon2_sz_win_rate_model.png",  width = 960, height = 720)
plot(x = c(1,16), y = c(-5,5), type="n", ylab = "correlation (log scale)", xlab = "model parameters", xaxt = "n", yaxt = "n", asp = 1)
axis(1, at=1:16, lab=lb)
axis(2, at=-5:5, lab=c(-10^(5:1),0,10^(1:5)))
title("X Rank Splat-Zones Win-Rate Correlation")
abline(v = seq(from=1.5,to=15.5,by=1), col = "grey", lty=2)
abline(v = c(1.5,6.5,11.5), col = "blue", lty=2)
abline(h = seq(-5,5), col = "grey", lty = 2)
abline(h = 0, col = "grey", lwd = 2)
for(i in 1:nrow(mx)){
    for(j in 1:ncol(mx)){
        if(mx[i,j]>0) col = "green"
        else col = "red"
        points(i,mx[i,j], pch = 16, col = alpha(col, 0.2 + (cko[j] - mean(cko))*5), cex = 2)
    }
}
text(x = 15, y  = 4.8, labels = paste0('numbers of\nselected models\n(success > 85%)\n',length(cko)), xpd = T)
text(x = 15, y  = 4.1, labels = 'inversion type: cgls', xpd = T)
dev.off()

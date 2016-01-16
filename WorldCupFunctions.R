# Note: I developed this function from code created by Rocio Titiunik, Devin Caughey, and Jas Sekhon.

BalancePlot=function(Data, Treat, Covariates, Names.To.Print, Shade.Color = "blanchedalmond", 
                     Title = "Balance Plot", Title.Size = 1.2, na.rm = FALSE, 
                     Built.In.Tests = c("T.Test"), Point.Color = "red", Other.Tests = NULL, 
                     pch = NULL, Year.Covariates = NULL, Different.Test=NULL, Observational.Data = NULL, 
                     Observational.Treat = NULL, Observational.Point.Color = "blue", 
                     Sample.Name = "Sub-Sample", O.Name = "All Units", Legend = FALSE, 
                     Paired = FALSE, Observational.Paired = FALSE) 
{
    if (length(Treat) == 1) {
        Treat = Data[, Treat]
    }
    if (length(Observational.Treat) == 1) {
        Observational.Treat = Observational.Data[, Treat]
    }
    if (na.rm == FALSE) {
        if (any(is.na(cbind(Data[, Covariates], Treat))) == TRUE) {
            return("NAs Detected. Must set na.rm=TRUE")
        }
    }
    mar = par()$mar
    op = par(mar = c(3, 432/28+5/28 * length(Covariates), 0, 1))
    t = Data[Treat == 1, ]
    o.t = Observational.Data[Observational.Treat == 1, ]
    c = Data[Treat == 0, ]
    o.c = Observational.Data[Observational.Treat == 0, ]
    sample = rbind(t, c)
    o.sample = rbind(o.t, o.c)
    options(scipen = 100, digits = 4)
    covs = cbind(Covariates, Names.To.Print)
    r = 1000
    varline = 9.5
    nline = 6
    tline = 4.5
    cline = 1
    plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(1, nrow(covs) + 
                                                          3), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
    mtext(text = c("Variable\nName", "Treatment\nMean", "Control\nMean"), 
          side = 2, font = 2, line = c(varline + 2, tline + 1.2, 
                                       cline + 0.53), adj = 0.5, las = 2, at = nrow(covs) + 
              1.18, cex = 0.7)
    mtext(text = Title, side = 2, font = 2, adj = .55, 
          las = 2, at = nrow(covs) + 2.78, cex = Title.Size)
    for (i in 1:nrow(covs)) {
        aty = nrow(covs) - i + 1
        mtext(text = covs[i, 2], side = 2, line = varline, adj = 1, 
              las = 2, at = aty, cex = 0.7)
        if(length(Different.Test)==0){      
        meanT = signif(mean(sample[Treat == 1, covs[i, 1]], na.rm = TRUE))}
        if(length(Different.Test)==1){      
        meanT = Different.Test(Data[,covs[i,1]])[1]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(meanT) < 0.1) {
            meanT = signif(meanT, digits = 1)
        }
        if (abs(meanT) > 0.1 & abs(meanT) < 10) {
            meanT = signif(meanT, digits = 3)
        }
        if (abs(meanT) > 10 & abs(meanT) < 100) {
            meanT = signif(meanT, digits = 3)
        }
               if (abs(meanT) > 100 & abs(meanT) < 1000) {
            meanT = signif(meanT, digits = 4)
        }
        if (abs(as.numeric(meanT)) >= 0.1 & abs(as.numeric(meanT)) < 
                1 & nchar(meanT) == 3) {
            meanT = paste(meanT, sep = "")
        }
        if (meanT > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanT = format(meanT, digits = 2, big.mark = ",")
        }
        if(length(Different.Test)==0){  
        meanC = signif(mean(sample[Treat == 0, covs[i, 1]], na.rm = TRUE))}
               if(length(Different.Test)==1){      
        meanC = Different.Test(Data[,covs[i,1]])[2]}
        if (covs[i, 1] %in% Year.Covariates) {
            meanC = signif(meanC, digits = 4)
        }
        if (abs(meanC) < 0.1) {
            meanC = signif(meanC, digits = 1)
        }
        if (meanC > 0.1 & abs(meanC) < 10) {
            meanC = signif(meanC, digits = 3)
        }
        if (abs(meanC) > 10 & abs(meanC) < 100) {
            meanC = signif(meanC, digits = 3)
        }
                if (abs(meanC) > 100 & abs(meanC) < 1000) {
            meanC = signif(meanC, digits = 4)
        }
        if (as.numeric(meanC)%%0.1 == 0 & abs(as.numeric(meanC)) < 
                1) {
            meanC = paste(meanC, sep = "")
        }
        if (meanC > 1000 & !(covs[i, 1] %in% Year.Covariates)) {
            meanC = prettyNum(meanC, big.mark = ",")
        }
        mtext(text = c(meanT, meanC), side = 2, line = c(tline, 
                                                         cline - 0.6), adj = 1, las = 2, at = aty, cex = 0.7)
        if (aty%%2 == 1) {
            polygon(x = c(0, 0, 1, 1), y = c(aty - 0.5, aty + 
                                                 0.5, aty + 0.5, aty - 0.5), border = FALSE, col = Shade.Color)
        }
                if (length(Different.Test)==1 & length(o.sample) > 
                0) {
            p4 = Different.Test(Observational.Data[,covs[i,1]])[3]
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
        
        if ("T.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            if (all(c(o.t[, covs[i, 1]], o.c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(o.t[, covs[i, 1]], 
                                                    o.c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = PermutationTest(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = PermutationTest(o.t[, covs[i, 1]], o.c[, 
                                                                 covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Observational.Point.Color, 
                       x = p1, y = aty)
            }
            else {
                if (Observational.Paired == FALSE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]])$p.value
                if (Observational.Paired == TRUE) 
                    p3 = t.test(o.t[, covs[i, 1]], o.c[, covs[i, 
                                                              1]], paired = TRUE)$p.value
                points(pch = 18, col = Observational.Point.Color, 
                       x = p3, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests & length(o.sample) > 
                0) {
            p4 = ks.test(o.t[, covs[i, 1]], o.c[, covs[i, 1]])$p.value
            points(pch = 17, col = Observational.Point.Color, 
                   x = p4, y = aty)
        }
                        if (length(Different.Test)==1) {
            p4 = Different.Test(Data[,covs[i,1]])[3]
            points(pch = pch, col = Point.Color, 
                   x = p4, y = aty)
        }
        if ("T.Test" %in% Built.In.Tests) {
            if (all(c(t[, covs[i, 1]], c[, covs[i, 1]]) %in% 
                        c(0, 1)) & sum(as.numeric(c(t[, covs[i, 1]], 
                                                    c[, covs[i, 1]]))) < 10) {
                if (Paired == FALSE) 
                    p1 = PermutationTest(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]])
                if (Paired == TRUE) 
                    p1 = PermutationTest(t[, covs[i, 1]], c[, 
                                                             covs[i, 1]], Paired = TRUE)
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
            else {
                if (Paired == FALSE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
                if (Paired == TRUE) 
                    p1 = t.test(t[, covs[i, 1]], c[, covs[i, 1]], 
                                paired = TRUE)$p.value
                points(pch = 18, col = Point.Color, x = p1, y = aty)
            }
        }
        if ("KS.Test" %in% Built.In.Tests) {
            p2 = ks.test(t[, covs[i, 1]], c[, covs[i, 1]])$p.value
            points(pch = 17, col = Point.Color, x = p2, y = aty)
        }
        if (length(Other.Tests) > 0) {
            if (length(o.sample) > 0) {
                for (j in 1:length(Other.Tests)) {
                    fun = Other.Tests[[j]]
                    px = as.numeric(fun(covs[i, 1]))
                    points(pch = pch[j], col = Observational.Point.Color, 
                           x = px, y = aty)
                }}
            for (z in 1:length(Other.Tests)) {
                fun = Other.Tests[[z]]
                px = as.numeric(fun(covs[i,1]))
                points(pch = pch[z], col = Point.Color, x = px, 
                       y = aty)
            }
        }
    }
    segments(x0 = 0, x1 = 0, y0 = 0.49, y1 = nrow(covs) + 0.48)
    segments(x0 = 0, x1 = 1, y0 = 0.49, y1 = 0.49)
    segments(x0 = c(0.05, 0.1), x1 = c(0.05, 0.1), y0 = 0.5, 
             y1 = nrow(covs) + 0.48, lty = "dotted")
    mtext(side = 1, at = c(0, 0.05, 0.1, 1), text = c("0", ".05", 
                                                      ".1", "1"), cex = 0.7, line = -0.2 + 5/nrow(covs) - 0.01 * 
              nrow(covs))
    mtext(side = 1, line = 1.2, at = 0.5, text = "p-value")
    if (Legend == TRUE) {
        par(xpd = TRUE)
        if (length(Built.In.Tests) == 2 & length(o.sample) > 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("t-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", O.Name, 
                                                                               ")", sep = ""), paste("t-test (", Sample.Name, 
                                                                                                     ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                                                                           ")", sep = "")), pch = c(18, 17, 18, 17), col = c(Observational.Point.Color, 
                                                                                                                                                                             Observational.Point.Color, Point.Color, Point.Color))
        }
        if (length(Built.In.Tests) == 1 & "T.Test" %in% Built.In.Tests & 
                length(o.sample) > 0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("t-test (", 
                                                 O.Name, ")", sep = ""), paste("t-test (", Sample.Name, 
                                                                               ")", sep = "")), pch = c(18, 18), col = c(Observational.Point.Color, 
                                                                                                                         Point.Color))
        }
        if (length(Built.In.Tests) == 1 & "KS.Test" %in% Built.In.Tests & 
                length(o.sample) > 0) {
            legend(-1.3, 0.4, cex = 0.6, c(paste("KS-test (", 
                                                 O.Name, ")", sep = ""), paste("KS-test (", Sample.Name, 
                                                                               ")")), pch = c(17, 17), col = c(Observational.Point.Color, 
                                                                                                               Point.Color))
        }
        if (length(Built.In.Tests) == 2 & length(o.sample) == 
                0) {
            legend(-1.3, 0.4, cex = 0.6, c("t-test", "KS-test"), 
                   pch = c(18, 17), col = c(Point.Color, Point.Color))
        }
    }
    op = par(mar = mar)
}



PermutationTest=function(Treatment, Control, Paired=FALSE, Simulations=10000, na.rm=FALSE, Output="p"){

if(na.rm==FALSE){if(any(is.na(c(Treatment,Control)))==TRUE){return("NAs Detected. Must set na.rm=TRUE")}}

if(Paired==TRUE){
differences=Treatment-Control
differences=differences[!is.na(differences)]
new.t.stats=rep(0,Simulations)
for(i in 1:Simulations){
assignment=sample(c(-1,1),length(differences), replace=TRUE)
new.t.stats[i]=mean(assignment*differences)
}
pvalue=length(which(abs(new.t.stats)>=abs(mean(differences))))/Simulations
}

if(Paired==FALSE){
treatmeant=Treatment[!is.na(Treatment)]
Control=Control[!is.na(Control)]
new.t.stats=rep(0,Simulations)
for(i in 1:Simulations){
assignment=sample(c(rep(0,length(Control)),rep(1,length(Treatment))),length(c(Treatment,Control)), replace=FALSE)
new.t=c(Treatment,Control)[assignment==1]
new.c=c(Treatment,Control)[assignment==0]
new.t.stats[i]=mean(new.t)-mean(new.c)
}
pvalue=length(which(abs(new.t.stats)>=abs(mean(Treatment)-mean(Control))))/Simulations
}

est=mean(Treatment)-mean(Control)
if(Output=="p") return(pvalue)

if(Output=="full") return(cbind(paste("Estimate=",est,colapse=""),paste("Two-tailed p-value=",pvalue,colapse="")))}



RDPlot=function (X, Y, C = 0, xlim = range(X), ylim = "Automatic", xlab = "Forcing Variable",
    ylab = "Outcome", Main = "Regression Discontinuity Plot",
    Plot.Means = TRUE, Plot.Raw.Data = FALSE, Mean.Colors = c("blue",
        "red"), Raw.Data.Colors = c("lightblue", "pink"), Point.Size = 0.25,
    Shade.Color = "gray87", Window = "None", Plot.p.value = TRUE,
    Tick.Marks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3,
        4, 5, 6, 7, 8), Bandwidth = 2, NBoots = "Automatic", Breaks = "Automatic",
    Parametric = FALSE,Smoother="Kernel", Kernel=NULL
, Poly.Order=NULL, Cluster = NULL, Jitter = FALSE, Loc.p.value = "BR",...)
{
    if (length(Cluster) != 0 & Parametric == TRUE) {
        if (length(names(table(as.vector(Cluster)))[table(as.vector(Cluster)) ==
            max(table(as.vector(Cluster)))]) != length(unique(Cluster))) {
            return("Clusters Must Be of Equal Size for Parametric Bootstrapping")
        }
    }
    if (length(Breaks) == 1) {
        Breaks = c(seq(min(X) - 1e-05, C, by = (C - min(X) +
            1e-05)/10), seq(max(X)/10, max(X), by = max(X)/10))
    }
    bins = cut(X, breaks = Breaks)
    dat = aggregate(Y ~ bins, FUN = mean)
    midpoints = rep(0, length(Breaks) - 1)
    for (i in 1:length(midpoints)) {
        midpoints[i] = (Breaks[i] + Breaks[i + 1])/2
    }
    midpoints = midpoints[as.numeric(dat[[1]])]
    negative.midpoints = midpoints[midpoints < C]
    positive.midpoints = midpoints[midpoints > C]
    pointsize = rep(0, length(Breaks) - 1)
    for (i in 1:length(pointsize)) {
        pointsize[i] = length(which(X > Breaks[i] & X <= Breaks[i +
            1]))
    }
    pointsize = Point.Size * sqrt(pointsize[as.numeric(dat[[1]])])
    if (length(ylim) == 1 & Plot.Raw.Data == TRUE) {
        ylim = range(Y)
    }
    if (length(ylim) == 1 & Plot.Raw.Data == FALSE) {
        ylim = range(dat[, 2])
    }
    if (Plot.Means == TRUE) {
        plot(negative.midpoints, dat[, 2][midpoints < C], xlab = xlab,
            ylab = ylab, main = Main, col = Mean.Colors[1], xlim = xlim,
            ylim = ylim, pch = 16, cex = pointsize[midpoints <
                C], xaxt = "n", ... )
        points(positive.midpoints, dat[, 2][midpoints > C], col = Mean.Colors[2],
            pch = 16, cex = pointsize[midpoints > C])
    }
    if (Plot.Means == FALSE) {
        plot(X[X < C], Y[X < C], xlab = xlab, ylab = ylab, main = Main,
            col = "white", xlim = xlim, ylim = ylim, pch = 16,
            cex = pointsize[midpoints < C], xaxt = "n")
        points(X[X > C], Y[X > C], col = "white", pch = 16, cex = pointsize[midpoints >
            C],...)
    }

if(NBoots=="Automatic"){
if(length(X)>200){NBoots=100}
if(length(X)<=200){NBoots=200}
if(length(X)<150){NBoots=500}
if(length(X)<100){NBoots=1000}
if(length(X)<70){NBoots=2000}
if(length(X)<50){NBoots=3000}
if(length(X)<30){NBoots=50000}
if(length(X)<20){NBoots=10000}


}


    bootstrapmatrix1 = matrix(0, nrow = 100 * NBoots, ncol = 2)
    if (Parametric == FALSE) {
        for (i in 1:NBoots) {
            if (length(Cluster) == 0) {
                randompoints = sample(seq(1, sum(X < C)), sum(X <
                  C), replace = TRUE)
                new.X = X[X < C][randompoints]
                new.Y = Y[X < C][randompoints]
            }
            if (length(Cluster) != 0) {
                randomClusters = sample(unique(Cluster[X < C]),
                  length(unique(Cluster[X < C])), replace = TRUE)
                new.X = c()
                for (j in 1:length(randomClusters)) {
                  new.X = c(new.X, X[X < C][Cluster[X < C] ==
                    randomClusters[j]])
                }
                new.Y = c()
                for (j in 1:length(randomClusters)) {
                  new.Y = c(new.Y, Y[X < C][Cluster[X < C] ==
                    randomClusters[j]])
                }
            }



weight.function=function(v,p){
if(length(Kernel)==0){Kernel="Tricube"}
if(Kernel=="Box"){return(rep(1,length(v)))}
if(Kernel=="Triangular"){return(Bandwidth-abs(v-p))}
if(Kernel=="Epanechnikov"){return(3/4(1-((v-p)/(Bandwidth))^2))}
if(Kernel=="Quartic"){return(15/16*((1-((v-p)/(Bandwidth))^2)^2))}
if(Kernel=="Triweight"){return(35/32*((1-((v-p)/(Bandwidth))^2)^3))}
if(Kernel=="Tricube"){return(70/81*((1-(abs(v-p)/(Bandwidth))^3)^3))}}


if(Smoother=="Nearest Neighbor"){

Smoother="Kernel"
Kernel="Box"

}

if(Smoother=="Kernel"){

smoother=function(x,y,c,bw){
if(c<mean(x)){range=c(c,max(X))}
if(c>mean(x)){range=c(min(X),c)}
xs=seq(range[1],range[2],by=(range[2]-range[1])/99)
ys=rep(0,length(xs))
for(k in 1:length(xs)){
obsx=x[x<(xs[k]+bw)&x>(xs[k]-bw)]
obsy=y[x<(xs[k]+bw)&x>(xs[k]-bw)]
if(length(obsx)==0){ys[k]=Y[which.min(abs(X-xs[k]))]}
if(length(obsx)>=1){
weights=weight.function(obsx,xs[k])
ys[k]=sum(obsy*weights)/sum(weights)
}}
combined=data.frame(cbind(xs,ys))
colnames(combined)=c("x","y")
return(combined)
}}


if(Smoother=="Local Linear"){

smoother=function(x,y,c,bw){
if(c<mean(x)){range=c(c,max(X))}
if(c>mean(x)){range=c(min(X),c)}
xs=seq(range[1],range[2],by=(range[2]-range[1])/99)
ys=rep(0,length(xs))
for(k in 1:length(xs)){
obsx=x[x<(xs[k]+bw)&x>(xs[k]-bw)]
design=cbind(1,obsx)
obsy=y[x<(xs[k]+bw)&x>(xs[k]-bw)]
if(length(unique(obsx))<=1){ys[k]=Y[which.min(abs(X-xs[k]))]}
if(length(unique(obsx))>1){
weights=diag(weight.function(design[,2],xs[k]))
beta=solve(t(design)%*%weights%*%design)%*%t(design)%*%weights%*%obsy
ys[k]=beta[1]+beta[2]*xs[k]}
}
combined=data.frame(cbind(xs,ys))
colnames(combined)=c("x","y")
return(combined)
}}




if(Smoother=="Local Polynomial"){

if(length(Poly.Order)==0){Poly.Order=4}

smoother=function(x,y,c,bw){
if(c<mean(x)){range=c(c,max(X))}
if(c>mean(x)){range=c(min(X),c)}
xs=seq(range[1],range[2],by=(range[2]-range[1])/99)
ys=rep(0,length(xs))
for(k in 1:length(xs)){
obsx=x[x<(xs[k]+bw)&x>(xs[k]-bw)]
if(length(unique(obsx))<=(Poly.Order+1)){ys[k]=Y[which.min(abs(X-xs[k]))]}
if(length(unique(obsx))>(Poly.Order+1)){
design=cbind(1,obsx,obsx^2,obsx^3,obsx^4,obsx^5,obsx^6,obsx^7,obsx^8,obsx^9)
design=design[,1:(Poly.Order+1)]
obsy=y[x<(xs[k]+bw)&x>(xs[k]-bw)]
weights=diag(weight.function(design[,2],xs[k]))
beta=solve(t(design)%*%weights%*%design)%*%t(design)%*%weights%*%obsy
vect=c(1,xs[k],xs[k]^2,xs[k]^3,xs[k]^4,xs[k]^5,xs[k]^6,xs[k]^7,xs[k]^8,xs[k]^9)[1:(Poly.Order+1)]
ys[k]=sum(beta*vect)}
}
combined=data.frame(cbind(xs,ys))
colnames(combined)=c("x","y")
return(combined)
}}


            regression = smoother(new.X, new.Y,c=C,bw=Bandwidth)







            bootstrapmatrix1[(1 + (100 * (i - 1))):(100 * i),
                1:2] = cbind(regression$x, regression$y)

        }
    }
    if (Parametric == TRUE) {
        get.resid = function(x, y, x.close, y.hat) {
            return(y - y.hat[which.min(abs(x - x.close))])
        }



        regression = smoother(X[X < C], Y[X < C],c=C,bw=Bandwidth)


        residuals = rep(0, length(X[X < C]))
        for (i in 1:sum(X < C)) {
            residuals[i] = get.resid(X[X < C][i], Y[X < C][i],
                regression$x, regression$y)
        }
        for (i in 1:NBoots) {
            if (length(Cluster) == 0) {
                randompoints = sample(seq(1, sum(X < C)), sum(X <
                  C), replace = TRUE)
                new.X = X[X < C]
                new.Y = rep(0, sum(X < C))
                for (k in 1:sum(X < C)) {
                  new.Y[k] = regression$y[which.min(abs(X[X <
                    C][k] - regression$x))] + residuals[randompoints[k]]
                }
            }
            if (length(Cluster) != 0) {
                if (length(names(table(as.vector(Cluster)))[table(as.vector(Cluster)) ==
                  max(table(as.vector(Cluster)))]) == length(unique(Cluster))) {
                  new.X = X[X < C]
                  randomClusters = sample(unique(Cluster[X <
                    C]), length(unique(Cluster[X < C])), replace = TRUE)
                  new.residuals = c()
                  for (j in 1:length(randomClusters)) {
                    new.residuals = c(new.residuals, residuals[Cluster[X <
                      C] == randomClusters[j]])
                  }
                  new.residuals = sample(new.residuals, length(new.residuals))
                  new.Y = rep(0, sum(X < C))
                  for (k in 1:sum(X < C)) {
                    new.Y[k] = regression$y[which.min(abs(X[X <
                      C][k] - regression$x))] + new.residuals[k]
                  }
                }
            }






            regression2 = smoother(new.X, new.Y,c=C,bw=Bandwidth)




            bootstrapmatrix1[(1 + (100 * (i - 1))):(100 * i),
                1:2] = cbind(regression2$x, regression2$y)
        }
    }
    lowerreg = cbind(bootstrapmatrix1[1:100, ])
    upperreg = cbind(bootstrapmatrix1[1:100, ])
    for (i in 1:100) {
        pointsforthatx = bootstrapmatrix1[bootstrapmatrix1[,
            1] == bootstrapmatrix1[i, 1], ]
        lowerreg[i, 1:2] = c(pointsforthatx[1, 1], quantile(pointsforthatx[,
            2], prob = 0.025, na.rm = TRUE))
        upperreg[i, 1:2] = c(pointsforthatx[1, 1], quantile(pointsforthatx[,
            2], prob = 0.975, na.rm = TRUE))
    }
    lowerreg = lowerreg[which(!is.na(lowerreg[, 2])), ]
    upperreg = upperreg[which(!is.na(upperreg[, 2])), ]
    polygon(c(upperreg[, 1], rev(upperreg[, 1]), upperreg[1,
        1]), c(upperreg[, 2], rev(lowerreg[, 2]), upperreg[1,
        2]), border = NA, col = Shade.Color)
    lines(lowerreg, lty = 2)
    lines(upperreg, lty = 2)
    bootstrapmatrix2 = matrix(0, nrow = 100 * NBoots, ncol = 2)
    if (Parametric == FALSE) {
        for (i in 1:NBoots) {
            if (length(Cluster) == 0) {
                randompoints = sample(seq(1, sum(X > C)), sum(X >
                  C), replace = TRUE)
                new.X = X[X > C][randompoints]
                new.Y = Y[X > C][randompoints]
            }
            if (length(Cluster) != 0) {
                randomClusters = sample(unique(Cluster[X > C]),
                  length(unique(Cluster[X > C])), replace = TRUE)
                new.X = c()
                for (j in 1:length(randomClusters)) {
                  new.X = c(new.X, X[X > C][Cluster[X > C] ==
                    randomClusters[j]])
                }
                new.Y = c()
                for (j in 1:length(randomClusters)) {
                  new.Y = c(new.Y, Y[X > C][Cluster[X > C] ==
                    randomClusters[j]])
                }
            }




                regression = smoother(new.X, new.Y, c=C, bw=Bandwidth)



            bootstrapmatrix2[(1 + (100 * (i - 1))):(100 * i),
                1:2] = cbind(regression$x, regression$y)
        }
    }
    if (Parametric == TRUE) {
        get.resid = function(x, y, x.close, y.hat) {
            return(y - y.hat[which.min(abs(x - x.close))])
        }

     regression2 = smoother(X[X > C], Y[X > C],c=C,bw=Bandwidth)


        residuals = rep(0, length(X[X > C]))
        for (i in 1:sum(X > C)) {
            residuals[i] = get.resid(X[X > C][i], Y[X > C][i],
                regression$x, regression$y)
        }
        for (i in 1:NBoots) {
            if (length(Cluster) == 0) {
                randompoints = sample(seq(1, sum(X > C)), sum(X >
                  C), replace = TRUE)
                new.X = X[X > C]
                new.Y = rep(0, sum(X > C))
                for (k in 1:sum(X > C)) {
                  new.Y[k] = regression$y[which.min(abs(X[X >
                    C][k] - regression$x))] + residuals[randompoints[k]]
                }
            }
            if (length(Cluster) != 0) {
                new.X = X[X < C]
                randomClusters = sample(unique(Cluster[X > C]),
                  length(unique(Cluster[X > C])), replace = TRUE)
                new.residuals = c()
                for (j in 1:length(randomClusters)) {
                  new.residuals = c(new.residuals, residuals[Cluster[X >
                    C] == randomClusters[j]])
                }
                new.residuals = sample(new.residuals, length(new.residuals))
                new.Y = rep(0, sum(X > C))
                for (k in 1:sum(X > C)) {
                  new.Y[k] = regression$y[which.min(abs(X[X >
                    C][k] - regression$x))] + new.residuals[k]
                }
            }

            regression2 = smoother(new.X, new.Y,c=C,bw=Bandwidth)



            bootstrapmatrix2[(1 + (100 * (i - 1))):(100 * i),
                1:2] = cbind(regression2$x, regression2$y)
        }
    }
    lowerreg = cbind(bootstrapmatrix2[1:100, ])
    upperreg = cbind(bootstrapmatrix2[1:100, ])
    for (i in 1:100) {
        pointsforthatx = bootstrapmatrix2[bootstrapmatrix2[,
            1] == bootstrapmatrix2[i, 1], ]
        lowerreg[i, 1:2] = c(pointsforthatx[1, 1], quantile(pointsforthatx[,
            2], prob = 0.025, na.rm = TRUE))
        upperreg[i, 1:2] = c(pointsforthatx[1, 1], quantile(pointsforthatx[,
            2], prob = 0.975, na.rm = TRUE))
    }
    lowerreg = lowerreg[which(!is.na(lowerreg[, 2])), ]
    upperreg = upperreg[which(!is.na(upperreg[, 2])), ]
    polygon(c(upperreg[, 1], rev(upperreg[, 1]), upperreg[1,
        1]), c(upperreg[, 2], rev(lowerreg[, 2]), upperreg[1,
        2]), border = NA, col = Shade.Color)
    lines(lowerreg, lty = 2)
    lines(upperreg, lty = 2)
    if (Plot.Raw.Data == TRUE & Jitter == FALSE) {
        points(X[X < C], Y[X < C], col = Raw.Data.Colors[1],
            pch = 16)
        points(X[X > C], Y[X > C], col = Raw.Data.Colors[2],
            pch = 16)
    }
    if (Plot.Raw.Data == TRUE & Jitter == TRUE) {
        JX = jitter(X[X < C])
        JX[JX > C] = 2 * C - JX[JX > C]
        X[X < C] = JX
        JX = jitter(X[X > C])
        JX[JX < C] = 2 * C - JX[JX < C]
        X[X > C] = JX
        points(X[X < C], jitter(Y[X < C]), col = Raw.Data.Colors[1],
            pch = 16)
        points(X[X > C], jitter(Y[X > C]), col = Raw.Data.Colors[2],
            pch = 16)
    }
    if (Plot.Means == TRUE) {
        points(negative.midpoints, dat[, 2][midpoints < C], col = Mean.Colors[1],
            pch = 16, cex = pointsize[midpoints < C])
        points(positive.midpoints, dat[, 2][midpoints > C], col = Mean.Colors[2],
            pch = 16, cex = pointsize[midpoints > C])
    }


    lines(smoother(X[X < C], Y[X < C],c=C,bw=Bandwidth))
    lines(smoother(X[X > C], Y[X > C],c=C,bw=Bandwidth))



    estimate=smoother(X[X > C], Y[X > C],c=C,bw=Bandwidth)[2][[1]][smoother(X[X > C], Y[X > C],c=C,bw=Bandwidth)[1][[1]]==C]-smoother(X[X < C], Y[X < C],c=C,bw=Bandwidth)[2][[1]][smoother(X[X < C], Y[X < C],c=C,bw=Bandwidth)[1][[1]]==C]


    abline(0, 0)
    abline(v = C)
    if (length(Window) == 2) {
        abline(v = Window[1], lty = 2)
        abline(v = Window[2], lty = 2)
    }
    axis(1, at = Tick.Marks, labels = Tick.Marks)
    p = 2 * length(which(bootstrapmatrix2[which(bootstrapmatrix2[,
        1] == C), 2] - bootstrapmatrix1[which(bootstrapmatrix1[,
        1] == C), 2] < 0))/NBoots
    if(p>1){p=2-p}    
    p.text = paste("p=", signif(p, digits = 2), collapse = "")
    p.text = gsub("\\s", "", p.text)
    if (Plot.p.value == TRUE) {
        if (Loc.p.value == "BR" & length(ylim) == 1) {
            location = c(xlim[2] - (xlim[2] - xlim[1])/20, min(dat[,
                2]) + (max(dat[, 2]) - min(dat[, 2]))/25)
        }
        if (Loc.p.value == "BR" & length(ylim) == 2) {
            location = c(xlim[2] - (xlim[2] - xlim[1])/20, y = ylim[1])
        }
        if (Loc.p.value == "TR" & length(ylim) == 1) {
            location = c(xlim[2] - (xlim[2] - xlim[1])/20, max(dat[,
                2]) - (max(dat[, 2]) - min(dat[, 2]))/25)
        }
        if (Loc.p.value == "TR" & length(ylim) == 2) {
            location = c(xlim[2] - (xlim[2] - xlim[1])/20, y = ylim[2])
        }
        if (Loc.p.value == "BL" & length(ylim) == 1) {
            location = c(xlim[1] + (xlim[2] - xlim[1])/20, min(dat[,
                2]) + (max(dat[, 2]) - min(dat[, 2]))/25)
        }
        if (Loc.p.value == "BL" & length(ylim) == 2) {
            location = c(xlim[1] + (xlim[2] - xlim[1])/20, y = ylim[1])
        }
        if (Loc.p.value == "TR" & length(ylim) == 1) {
            location = c(xlim[1] + (xlim[2] - xlim[1])/20, max(dat[,
                2]) - (max(dat[, 2]) - min(dat[, 2]))/25)
        }
        if (Loc.p.value == "TR" & length(ylim) == 2) {
            location = c(xlim[1] + (xlim[2] - xlim[1])/20, y = ylim[2])
        }
        text(location[1], location[2], p.text, cex = 0.75)
    }
  output=list(estimate,signif(p, digits = 2))
  names(output)=c("Estimate","p.value")

if(NBoots<10000){print(paste("To increase the precision of the p-value, increase the number of bootstrapped samples. Currently, NBoots =",NBoots))}

if(sum(X[X<(C+Bandwidth)&X>C])<100|sum(X[X<C&X>(X-Bandwidth)])<100){if(Smoother=="Local Linear"|Smoother=="Local Polynomial"){print("Note: Local linear regression and local polynomial regression can have high variablility in cases where there is not a large sample size around the cut-piont. Kernel Reggression may be more appropiate.")}}

if(sum(X==C)>0){print("Note: Some observations are on the cut-point. These units have been dropped from the test.")}


return(output)
 
}




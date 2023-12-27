################################################################# 
#####      Illustration of DEA on Queensland hospitals      #####
################################################################# 

rm(list=ls())
graphics.off()


################### Data Process ###################

# Read data
data <- read.csv("QLD.csv")
names(data)[names(data)=="HOSID"] <- "id"
names(data)[names(data)=="Yeardummy"] <- "Year"

# Convert to panel data
library(plm)
paneldata<- pdata.frame(data, c("id","Year"))

attach(data)
# Input/Output for "Benchmarking"
X = as.matrix(cbind(BEDS, Agglabours, SUPP))
Y = as.matrix(Aggout)

# Input/Output for "FEAR"
Xt = t(X)
Yt = t(Y)

################### Technical Efficiency ###################

attach(data)

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Output-oriented
dea.crs.out.bench = 1/dea(X, Y, RTS="crs", ORIENTATION="out")$eff # CRS
dea.vrs.out.bench = 1/dea(X, Y, RTS="vrs", ORIENTATION="out")$eff # VRS
dea.nirs.out.bench = 1/dea(X, Y, RTS="drs", ORIENTATION="out")$eff # NIRS
dea.fdh.out.bench = 1/dea(X, Y, RTS="fdh", ORIENTATION="out")$eff # FDH

# Input-oriented
dea.crs.in.bench = dea(X, Y, RTS="crs", ORIENTATION="in")$eff
dea.vrs.in.bench = dea(X, Y, RTS="vrs", ORIENTATION="in")$eff
dea.nirs.in.bench = dea(X, Y, RTS="drs", ORIENTATION="in")$eff
dea.fdh.in.bench = dea(X, Y, RTS="fdh", ORIENTATION="in")$eff

# Summarize the estimations
Effi <- as.data.frame(cbind(dea.crs.out.bench, dea.vrs.out.bench, 
                            dea.nirs.out.bench, dea.fdh.out.bench,
                            dea.crs.in.bench, dea.vrs.in.bench, 
                            dea.nirs.in.bench, dea.fdh.in.bench))
summary(Effi)

################### Cost/Revenue/Profit Efficiency ###################

attach(data)

# Generate artificial matrix of prices
w <- t(as.matrix(c(1,2,3)))
p <- as.matrix(4)

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Cost efficiency
xopt = cost.opt(X, Y, w, RTS='crs') #CRS
cobs <- X %*% t(w) # Observed Cost
copt <- xopt$x %*% t(w) # Optimal Cost
dea.crs.cost = copt/cobs # cost efficiency

# Revenue efficiency
yopt = revenue.opt(X, Y, p, RTS='crs') #CRS
yobs <- Y %*% p # Observed Revenue
yopt <- yopt$y %*% p # Optimal Revenue
dea.crs.revenue = yobs/yopt # Revenue efficiency

# Profit efficiency
popt = profit.opt(X, Y, w, p, RTS='crs') #CRS
pobs <- Y %*% p -X %*% t(w) # Observed Profit
popt <- popt$y %*% p - popt$x %*% t(w) # Optimal Profit
dea.crs.revenue = pobs/popt # Profit efficiency

# Other RTS assumptions can be analogously applied.

################### MPI ###################

attach(data)

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Use period 1 and 2 as an example
X0 <- as.matrix(X[Year==1,])
X1 <- as.matrix(X[Year==2,])

Y0 <- as.matrix(Y[Year==1,])
Y1 <- as.matrix(Y[Year==2,])

ID0 <- as.matrix(id[Year==1])
ID1 <- as.matrix(id[Year==2])

mpi.crs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.vrs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "vrs", ORIENTATION = "in")$m
mpi.nirs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "drs", ORIENTATION = "in")$m
mpi.crs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "out")$m
mpi.vrs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "vrs", ORIENTATION = "out")$m
mpi.nirs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "drs", ORIENTATION = "out")$m

# Summarize the estimations
MPI <- as.data.frame(cbind(mpi.crs.in, mpi.vrs.in,
                            mpi.nirs.in, mpi.crs.out,
                            mpi.vrs.out, mpi.nirs.out))
summary(MPI)

# Estimate with DEA estimators (Fare et al. 1992)
# Same results by manual MPI (when implosion, using: (X2,Y2))
dea00<-dea(X0, Y0, RTS="crs", ORIENTATION="out", XREF=X0, YREF=Y0)$eff
dea11<-dea(X1, Y1, RTS="crs", ORIENTATION="out", XREF=X1, YREF=Y1)$eff
dea10<-dea(X0, Y0, RTS="crs", ORIENTATION="out", XREF=X1, YREF=Y1)$eff
dea01<-dea(X1, Y1, RTS="crs", ORIENTATION="out", XREF=X0, YREF=Y0)$eff

mpi.dea<-sqrt(dea01/dea00*dea11/dea10)
summary(mpi.dea) # Same results as above
summary(mpi.crs.out)

# Heatmap for MPI
IDs = cbind(data$id, data$NetworkID, rep(1,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==1,])
X1 <- as.matrix(X[Year==2,])
Y0 <- as.matrix(Y[Year==1,])
Y1 <- as.matrix(Y[Year==2,])
ID0 <- as.matrix(id[Year==1])
ID1 <- as.matrix(id[Year==2])
mpi.crs.in.1 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.1 = cbind(mpi.crs.in.1, IDs)

IDs = cbind(data$id, data$NetworkID, rep(2,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==2,])
X1 <- as.matrix(X[Year==3,])
Y0 <- as.matrix(Y[Year==2,])
Y1 <- as.matrix(Y[Year==3,])
ID0 <- as.matrix(id[Year==2])
ID1 <- as.matrix(id[Year==3])
mpi.crs.in.2 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.2 = cbind(mpi.crs.in.2, IDs)

IDs = cbind(data$id, data$NetworkID, rep(3,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==3,])
X1 <- as.matrix(X[Year==4,])
Y0 <- as.matrix(Y[Year==3,])
Y1 <- as.matrix(Y[Year==4,])
ID0 <- as.matrix(id[Year==3])
ID1 <- as.matrix(id[Year==4])
mpi.crs.in.3 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.3 = cbind(mpi.crs.in.3, IDs)

mpi.heat = as.data.frame(rbind(mpi.crs.in.1, mpi.crs.in.2, mpi.crs.in.3))
colnames(mpi.heat) <- c("mpi.crs.in", "id", "HHS", "Period")

require("ggplot2")
require("hrbrthemes")
ggplot(mpi.heat, aes(as.character(HHS), Period, fill= mpi.crs.in)) + 
  geom_tile() +
  xlab("Local Hospital Networks in Queensland")+
  ylab("Period")+
  scale_fill_distiller(palette = "GnBu")+
  scale_y_discrete(limit = c("12/13-13/14","13/14-14/5","14/15-15/16"))+
  guides(fill=guide_legend(title="MPI (CRS, input-oriented)"))+
  theme_bw(base_size = 16)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white")) 

################### Bias-correction ###################

attach(data)

# Use the functions by Wilson (2020)
library(FEAR)

# Bias-corrected efficiency (CRS-Output-oriented)
Bootstrap.fear = boot.sw98(Xt, Yt, NREP = 2000, RTS = 3, ORIENTATION = 2, alpha = 0.05, CI.TYPE=2)
cdea = 1/Bootstrap.fear$dhat.bc  

# Compare the original and bias-corrected estimates
dea = dea(Xt, Yt, RTS=3, ORIENTATION=2)
cdea = cbind(cdea,rep(1,380))
dea = cbind(dea,rep(0,380))

correction = as.data.frame(rbind(cdea,dea))
colnames(correction) = c("DEA","method")

correction$Estimates[correction$method==1]='Bias-corrected'
correction$Estimates[correction$method==0]='Original'

################### Kernel density plot ###################

attach(correction)
require("ggplot2")
.df <- na.omit(data.frame(x = correction$DEA))
.nbins <- pretty(range(.df$x), n = nclass.FD(.df$x), min.n = 1)
.dea <- ggplot(data = .df, aes(x = x, y = ..density..)) +
  # Epanechnikov kernel and CV bandwidth
  geom_density(
    kernel = "gaussian",
    bw = "ucv",
    alpha = 0.5,
    aes(color = Estimates, fill = Estimates)
  ) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Estimated inefficiency") +
  ylab("Estimated Density") +
  labs(colour = "Estimates",
       shape = "Estimates",
       fill = "Estimates") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans")
print(.dea)
rm(.df, .nbins)

# Use the functions by Simm and Besstremyannaya (2020)
library(rDEA)
Bootstrap.rDEA = dea.robust(X, Y, W=NULL, model="output", RTS="constant", B=2000, alpha=0.05, bw="bw.ucv")

################### Aggregate ###################

# Generate artificial matrix of prices
p <- as.matrix(4)

# Calculate weight
weight = p%*%Yt/sum(p%*%rowSums(Yt))

# Aggregate
aggregate = sum(dea.crs.out.bench%*%t(weight))
aggregate
mean(dea.crs.out.bench)

################### SW07 ###################

attach(data)

# Use the functions by Simm and Besstremyannaya (2020)
library(rDEA)

# Define environmental variables
Z = as.matrix(cbind(TEACH, Small, Remote))

# Output-oriented & CRS
sw07 = dea.env.robust(X, Y, W=NULL, Z, "output", RTS="constant", L1=100, L2=2000, alpha=0.05)
sw07$beta_hat_hat
sw07$beta_ci

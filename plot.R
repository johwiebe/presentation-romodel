library(ggplot2)
library(tikzDevice)
library(tidyr)
setwd("~/projects/curves")
imgdir <- "~/presentations/curves/img/"
unc_sets = c('bonferroni', 'gershgorin', 'brauer')
n_dims = c(1, 2, 3, 12)
det_min = c(0.145234, 0.374931, 0.642423, 2.393172)
det_mean = c(0.266425, 0.573120, 0.914646, 3.202747)
det001 = c(0.2788912, 0.5984289, 0.953812, 3.3571974)
det003 = c(0.273794, 0.587648, 0.936478, 3.292506)
det005 = c(0.266489, 0.573157, 0.914722, 3.203296)
det008 = c(0.243789, 0.533956, 0.864863, 2.939391)
dfdet <- data.frame(det001, det003, det005, det008)
dfdet[,"0.01"] <- dfdet$det001/dfdet$det001
dfdet[,"0.03"] <- dfdet$det003/dfdet$det001
dfdet[,"0.05"] <- dfdet$det005/dfdet$det001
dfdet[,"0.08"] <- dfdet$det008/dfdet$det001
dfdet$det001 <- NULL
dfdet$det003 <- NULL
dfdet$det005 <- NULL
dfdet$det008 <- NULL
dfdet$ndim <- n_dims
dfdet$ndim <- as.factor(paste0("T = ", dfdet$ndim))
dfdet
dfdetlong <- gather(dfdet, Noise, Objective, c('0.01', '0.03', '0.05', '0.08'))
dfdetlong 
dfdetlongi <- dfdetlong
dfdetlongi$x <- 0
dfdetlong$x <- 1
dfdetlong <- rbind(dfdetlong, dfdetlongi)

df <- data.frame(matrix(ncol=10, nrow=0))
names(df) <- c('confidence', 'feasibility', 'mean', 'min', 'noise', 'objective', 'opt', 'time', 'uncset', 'ndim')
for(u in unc_sets){
  print(u)
  for(i in 1:length(n_dims)){
    n <- n_dims[i]
    dfi <- read.csv(paste0('results/', u, '_', n, 'dim_20data.csv'))
    dfi$X <- NULL
    dfi$uncset <- u
    dfi$Set <- u
    dfi$ndim <- n
    dfi$Noise <- as.factor(dfi$noise)
    dfi$ndim <- as.factor(paste0("T = ", dfi$ndim))
    dfi$Mean <- (dfi$mean - det_mean[i])/det_mean[i]
    dfi$Min <- (dfi$min - det_min[i])/det_min[i]
    dfi$chi2 <- qchisq(dfi$confidence, df=n)
    dfi$Obj <- dfi$objective/det001[i]
  df <- rbind(df, dfi)
  }
}
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme <- theme_classic() +
  theme(#legend.position = "top",
        legend.margin=margin(t=-0.1,b=-0.4, unit='cm'),
        strip.background=element_rect(colour="white", fill="white")) 

## PLOT FEASIBILITY ##
dim = c(4.26, 3.08)
tikz(paste0(imgdir, "feas.tex"), width=dim[1], height=dim[2])
ggplot(df, aes(x=confidence, y=feasibility, color=Set, linetype=Noise)) +
  theme +
  geom_abline(slope=1, intercept=0, linetype="dotted") +
  geom_abline(slope=0.5, intercept=0.5, linetype="dotted") +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_line(size=1) + 
  # coord_fixed() +
  # geom_point(size=0.8, alpha=0.5) +
  # xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  xlab("Confidence $1 - \\alpha$") +
  ylab("Feasibility") +
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1.0), labels=c('0', '0.25', '0.5', '0.75', '1')) +
  facet_wrap(~ndim, ncol=2) +
  scale_color_manual(values=cbPalette, labels=c("Bonf.", "Brauer", "Gersh.")) + 
  guides(linetype=FALSE)
dev.off()

## PLOT Objective ##
tikz(paste0(imgdir, "obj.tex"), width=dim[1], height=dim[2])
ggplot(df, aes(x=confidence, y=Obj, color=Noise, linetype=Set)) +
  theme +
  geom_line(data=dfdetlong, aes(x=x, y=Objective, group=Noise), linetype=1, color="black") +
  geom_line(size=1) +
  #geom_point(size=0.8, alpha=0.5) + 
  # xlim(c(0, 1)) +
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1.0), labels=c('0', '0.25', '0.5', '0.75', '1')) +
  #ylim(c(0, 1.2)) +
  xlab("Confidence $1 - \\alpha$") +
  ylab("Worst case profit $\\psi$") +
  facet_wrap(~ndim, ncol=2) +
  scale_color_manual(values=cbPalette) +
  scale_linetype_discrete(labels=c("Bonf.", "Brauer", "Gersh.")) 
  
dev.off()


## PLOT SETS ##
t <- seq(0,1000)*2*pi/1000
mu <- c(0.467207, 0.4566559)
x <- sin(t)*0.114565 + mu[1]
y <- cos(t)*0.114565 + mu[2]
df <- data.frame(x=x, y=y)
df$Set <- "Gersh."
cov <- matrix(c(0.00172968, 0.000461, 0.000461, 0.001704), nrow = 2)
R <- chol(solve(cov))/sqrt(5.99)
mat <- solve(R) %*% rbind(sin(t), cos(t))
x <- mat[1,] + mu[1]
y <- mat[2,] + mu[2]
dfi <- data.frame(x=x, y=y)
dfi$Set <- "True ellipse"
df <- rbind(df, dfi)
dx = 0.093219
dy = 0.092524
xcurr = c(0.723708, 0.746712)

x <- seq(1,100)/100*0.3 + 0.3
b = 0.559
y <- (b - xcurr[1]*x)/xcurr[2]
dflines <- data.frame(x=x, y=y)
dflines$Set = "Gersh."
b = 0.542
y <- (b - xcurr[1]*x)/xcurr[2]
dflinesi <- data.frame(x=x, y=y)
dflinesi$Set <- "Bonf."
dflines <- rbind(dflines, dflinesi)
line_size <- 1

b = xcurr[1]*mu[1] + xcurr[2]*mu[2]
y <- (b - xcurr[1]*x)/xcurr[2]
dfdet <- data.frame(x=x, y=y)

tikz(paste0(imgdir, "sets.tex"), width=dim[1], height=dim[2])  #2.75)
ggplot(df, aes(x=x, y=y, color=Set)) + 
  theme +
  coord_fixed() +
  geom_point(size=0.1) +
  geom_line(data=df[df$Set == "True ellipse",], alpha=0.1) +
  geom_point(aes(x=mu[1], y=mu[2]), color="black", size=0.8) +
  geom_segment(aes(x=mu[1]-dx, y=mu[2]-dy, xend=mu[1]+dx, yend=mu[2]-dy, color="Bonf."), size=line_size) +
  geom_segment(aes(x=mu[1]-dx, y=mu[2]+dy, xend=mu[1]+dx, yend=mu[2]+dy, color="Bonf."), size=line_size) +
  geom_segment(aes(x=mu[1]-dx, y=mu[2]-dy, xend=mu[1]-dx, yend=mu[2]+dy, color="Bonf."), size=line_size) +
  geom_segment(aes(x=mu[1]+dx, y=mu[2]-dy, xend=mu[1]+dx, yend=mu[2]+dy, color="Bonf."), size=line_size) +
  geom_line(data=dflines, linetype=2, size=1) +
  geom_line(data=dfdet, color="black", linetype=2, size=1) +
  xlab("$p_1$") +
  ylab("$p_2$") +
  xlim(c(0.3, 0.6)) +
  ylim(c(0.3, 0.6)) +
  annotate("text", x = 0.51, y = 0.49, label = "25\\%") +
  annotate("text", x = 0.44, y = 0.42, label = "25\\%") +
  scale_color_manual(values=cbPalette)
dev.off()

    


## PLOT WORST CASE ## n_dims = c(1, 2, 3, 12)
df <- data.frame(matrix(ncol=5, nrow=0))
names(df) <- c('confidence', 'obj_d_eval', 'obj_r_eval', 'obj_r_model', 'obj_d_model')
for(i in 1:length(n_dims)){
    n <- n_dims[i]
    dfi <- read.csv(paste0('results/worst_', n, 'dim_20data.csv'))
    dfi$X <- NULL
    dfi$Noise <- as.factor(dfi$noise)
    dfi$T <- as.factor(paste0("T = ", n))
  df <- rbind(df, dfi)
}
for(i in 1:length(n_dims)){
    n <- n_dims[i]
    dfi <- read.csv(paste0('results/worst_', n, 'dim_20data_007and8noise.csv'))
    dfi$X <- NULL
    dfi$Noise <- as.factor(dfi$noise)
    dfi$T <- as.factor(paste0("T = ", n))
  df <- rbind(df, dfi)
}
df$improv <- (df$obj_r_eval - df$obj_d_eval)/abs(df$obj_d_eval)*100
df <- df[!df$noise==0.07,]

tikz(paste0(imgdir, "worst.tex"), width=dim[1], height=dim[2])
ggplot(df, aes(x=confidence, y=improv, color=Noise, linetype=Noise, shape=Noise)) +
  theme +
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1.0), labels=c('0', '0.25', '0.5', '0.75', '1')) +
  geom_line(size=1) +
  # geom_point(size=1.2) +
  xlab("Confidence $1 - \\alpha$") +
  ylab("Improvement worst case") +
  coord_cartesian(ylim=c(0, 10))+
  # ylim(0,20) +
  facet_wrap(~T,ncol=2) +
  scale_color_manual(values=cbPalette)
dev.off()


## PLOT GP ##
df <- read.csv('results/gp.csv')
df$x <- df$xmax
df$ytrue <- exp(-df$x)
dfdata <- read.csv('results/gpdata.csv')

linesize <- 1
tikz(paste0(imgdir, "gp.tex"), width=3.26, height=2.0)
ggplot(df, aes(x=x)) + 
  theme +
  theme(legend.position=c(0.75, 0.75)) +
  geom_line(aes(y=ymean, linetype="Prediction"), size=linesize) +
  geom_line(aes(y=ytrue, linetype="True"), size=linesize) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha=0.1) +
  geom_point(data=dfdata, aes(y=y, shape="Data"), size=0.8) +
  geom_line(aes(y=ymin, linetype="Confidence"), size=linesize) +
  geom_line(aes(y=ymax, linetype="Confidence"), size=linesize) +
  xlim(c(0,5.2)) + 
  ylim(c(-0.1, 1)) +
  xlab("Supply x") +
  ylab("Price p") +
  scale_linetype_manual("", values=c(3,1,2)) +
  scale_shape_manual("", values=c(1,2,3), guide=guide_legend(vjust=-3.3))
dev.off()


## Plot confidence sets ##
library(MASS)
library(stats)
Sigma <- matrix(c(0.1, 0.07, 0.07, 0.1), 2, 2)
mu <- c(1, 1)
dfconf <- data.frame(mvrnorm(n = 200, mu, Sigma))
names(dfconf) <- c("x", "y")
t <- seq(0,1000)*2*pi/1000
R <- chol(solve(Sigma))/sqrt(qchisq(0.67, 2))
mat <- solve(R) %*% rbind(sin(t), cos(t))
x <- mat[1,] + mu[1]
y <- mat[2,] + mu[2]
dfi <- data.frame(x=x, y=y)
dfi$Set <- "True ellipse"

tikz("~/papers/curves/img/confidence.tex", width=3.26, height=2.4)
ggplot(dfconf, aes(x=x, y=y)) +
  theme +
  geom_point(shape=1, size=0.8) + 
  geom_point(data=dfi, size=0.2) +
  xlab("$z_1$") +
  ylab("$z_2$") + 
  geom_hline(yintercept = mu[2] + sqrt(0.1*qchisq(0.67, 1))) +
  geom_hline(yintercept = mu[2] - sqrt(0.1*qchisq(0.67, 1))) +
  geom_segment(x=0.45, y=0.7, xend=0.45, yend=1.3, arrow=arrow(ends="both", length = unit(0.03, "npc"))) +
  geom_curve(x=0.60, y=1.4, xend=0.75, yend=1.15, curvature=0.2, arrow=arrow(length = unit(0.03, "npc")), size=0.5) +
  annotate("text", x=0.38, y=1, label = "$\\mathcal{E}^{\\alpha}_1(x_2)$", angle=90) +
  annotate("text", x=0.60, y=1.45, label = "$\\mathcal{E}^{\\alpha}_2(x_1, x_2)$") +
  xlim(c(0.35, 1.55)) +
  ylim(c(0.35, 1.55))
dev.off()

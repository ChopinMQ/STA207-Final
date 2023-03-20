library(car)
library(lme4)
library(lmerTest)
library(haven)
library(dplyr)
library(qwraps2)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(plotly)
library(car)
library(stats)
library(mice)
library(VIM)
library(lattice)
library(tidyverse)
library(readxl)
library(tidyr)
library(fdapace)
library(dplyr)
library(lubridate)
library(fdaconcur)
library(ggplot2)
library(locpol)
library(KernSmooth)
library(ggpubr)
library(tidyr)
library(reshape2)
library(magrittr)
library(plot3D)
library(MASS)
library(boot)
library(foreach)
library(doParallel)
library(parallel)

options(qwraps2_markup = "markdown", digits=2)

setwd("C:/Users/18337/OneDrive/桌面/winter quarter 2023/STA207/Final")
session=list()
for(i in 1:5){
  session[[i]]=readRDS(paste('./session',i,'.rds',sep=''))
  print(session[[i]]$mouse_name)
  print(session[[i]]$date_exp)
}

# total 214 trials
id = 2
session[[3]]$feedback_type[id]
session[[3]]$contrast_left[id]
session[[3]]$contrast_right[id]
length(session[[3]]$time[[id]])
session[[3]]$time[[id]]
dim(session[[3]]$spks[[id]])

###########################################################
ID=1
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

# Obtain the firing rate 
firingrate=numeric(n.trials)
for(i in 1:n.trials){
  firingrate[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}
firingrate

###########################################################
# combine the 5 dataset
j = 1

Firings = c()
Left = c()
Right = c()
Date = c()
for (ses in 1:5){
  n.trials=length(session[[ses]]$spks)
  n.neurons=dim(session[[ses]]$spks[[1]])[1]
  for(i in 1:n.trials){
    firingrate=sum(session[[ses]]$spks[[i]])/n.neurons/t
    Firings[j] = firingrate
    Left[j] = session[[ses]]$contrast_left[i]
    Right[j] = session[[ses]]$contrast_right[i]
    Date[j] = session[[ses]]$date_exp
    j = j + 1
  }
}
data = data.frame(matrix(ncol=4, nrow=j-1))
colnames(data) = c('Firings', 'Left', 'Right','Date')
data["Firings"] = Firings
data["Left"] = Left
data["Right"] = Right
data["Date"] = Date
data$Left = as.factor(data$Left)
data$Right = as.factor(data$Right)
data$Date = as.factor(data$Date)
head(data)
table(data$Left, data$Right)
data$Firings
# summary
our_summary =
  list("Firing rate" =
         list("min"       = ~ min(Firings, na.rm=TRUE),
              "median"    = ~ median(Firings, na.rm=TRUE),
              "max"       = ~ max(Firings, na.rm=TRUE),
              "lower quatile" = ~ quantile(Firings, 0.25, na.rm=TRUE),
              "upper quantile" = ~ quantile(Firings, 0.75, na.rm=TRUE),
              "NA values' number" = ~ sum(is.na(Firings))),
       "Left contrast" =
         list("Left Contrast=1" = ~ sum(Left==1),
              "Left Contrast=0.5" = ~ sum(Left==0.5),
              "Left Contrast=0.25" = ~ sum(Left==0.25),
              "Left Contrast=0" = ~ sum(Left==0),
              "NA values' number" = ~ sum(is.na(Left))),
       "Right Constrast" =
         list("Right Contrast" = ~ sum(Right==1),
              "Right Contrast=0.5" = ~ sum(Right==0.5),
              "Right Contrast=0.25" = ~ sum(Right==0.25),
              "Right Contrast=0" = ~ sum(Right==0),
           "NA values' number" = ~ sum(is.na(Right)))
      )
undo.mice.dat = data%>%dplyr::select(Firings, Left, Right, )
whole = summary_table(undo.mice.dat, our_summary)
whole



ggplot(data, aes(x = Left, y = Firings, color = Firings)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 23, fill = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_wrap(~ Firings, ncol = 1, scales = "free_y") +
  theme_classic() +
  labs(x = "Factor 1", y = "Response") +
  ggtitle("Interaction Plot with Two Factors")


model1 = lmerTest::lmer(Firings~Left*Right+(1|Date), data=data)
model2 = lm(Firings~Left*Right, data=data)
anova(model)
anova(model1, model2, "F")
library(knitr)
kable(anova(model1, model2, "F"), digits = 3)

# Exploratory analysis
head(data)
library(ggplot2)
library(dplyr)

df = data %>% group_by(Left, Right)
df_filtered <- df %>% filter(Right == '1')
df_filtered

# Plot with fixed Left and varying Right
ggplot(df, aes(x = Spike, y = Value, color = Right)) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ Left, ncol = 2) +
  labs(title = "Smoothing lines with fixed Left and varying Right",
       x = "Spike", y = "Value")
library(ggplot2)


# Group by Left and Right
grouped_df <- data %>% group_by(Left, Right) %>% arrange(Left, Right)

# Filter data for Right = 1 and different levels of Left
R0df = grouped_df %>% filter(Right == 0 & Left %in% c(0, 0.25, 0.5, 1))
R025df = grouped_df %>% filter(Right == 0.25 & Left %in% c(0, 0.25, 0.5, 1))
R05df = grouped_df %>% filter(Right == 0.5 & Left %in% c(0, 0.25, 0.5, 1))
R1df = grouped_df %>% filter(Right == 1 & Left %in% c(0, 0.25, 0.5, 1))
L0df = grouped_df %>% filter(Left == 0 & Right %in% c(0, 0.25, 0.5, 1))
L025df = grouped_df %>% filter(Left == 0.25 & Right %in% c(0, 0.25, 0.5, 1))
L05df = grouped_df %>% filter(Left == 0.5 & Right %in% c(0, 0.25, 0.5, 1))
L1df = grouped_df %>% filter(Left == 1 & Right %in% c(0, 0.25, 0.5, 1))
# Plot smoothing curves for each level of Left
R0 = ggplot(R0df, aes(x = seq_along(Firings), y = Firings, group = Left, color = Left)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Right = 0") +
  theme_minimal()
R025 = ggplot(R025df, aes(x = seq_along(Firings), y = Firings, group = Left, color = Left)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Right = 0.25") +
  theme_minimal()
R05 = ggplot(R05df, aes(x = seq_along(Firings), y = Firings, group = Left, color = Left)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Right = 0.5") +
  theme_minimal()
R1 = ggplot(R1df, aes(x = seq_along(Firings), y = Firings, group = Left, color = Left)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Right = 1") +
  theme_minimal()
L0 = ggplot(L0df, aes(x = seq_along(Firings), y = Firings, group = Right, color = Right)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Left = 0") +
  theme_minimal()
L025 = ggplot(L025df, aes(x = seq_along(Firings), y = Firings, group = Right, color = Right)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Left = 0.25") +
  theme_minimal()
L05 = ggplot(L05df, aes(x = seq_along(Firings), y = Firings, group = Right, color = Right)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Left = 0.5") +
  theme_minimal()
L1 = ggplot(L1df, aes(x = seq_along(Firings), y = Firings, group = Right, color = Right)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Index", y = "Value", title = "Fixed Left = 1") +
  theme_minimal()

figure = ggarrange(R0, R025,R05,R1,L0,L025,L05,L1,
                   ncol = 4, nrow = 2)
figure
filename = "C:\\Users\\18337\\OneDrive\\桌面\\winter quarter 2023\\STA207\\Final\\Eight.png"
ggsave(filename, device="png", width = 20, height = 14, units = "cm")

w = data$Firings
e = data$Left
o = data$Right

interaction.plot(e,o,w,xlab="Left",ylab="Firings rate", main="Interaction")
# smoothing

Firings = c()
Left = c()
Right = c()
Date = c()
for (ses in 1:5){
  n.trials=length(session[[ses]]$spks)
  n.neurons=dim(session[[ses]]$spks[[1]])[1]
  for(i in 1:n.trials){
    firingrate=sum(session[[ses]]$spks[[i]])/n.neurons/t
    Firings[j] = firingrate
    Left[j] = session[[ses]]$contrast_left[i]
    Right[j] = session[[ses]]$contrast_right[i]
    Date[j] = session[[ses]]$date_exp
    j = j + 1
  }
}
length(colSums(session[[1]]$spks[[1]]))
colSums(session[[1]]$spks[[1]])[1]
yt = c()
for (i in 1:length(colSums(session[[1]]$spks[[1]]))){
  yt[i] = colSums(session[[1]]$spks[[1]])[i]
}

deg = 1
kernel = EpaK
t = 1: 39
thBwSel = thumbBw(t, yt, deg, kernel)
fit = locpoly(t, yt, drv=0, degree=1, bandwidth=thBwSel)
g = ggplot(as.data.frame(fit), aes(x=x, y=y)) + geom_point()+
  geom_point(as.data.frame(yt), mapping=aes(c(1: 39), yt), size=3, shape=16) +
  geom_line(col="#000000", size=2.5) + labs(x='Time Bins', y='Spikes Number')
g
filename = "C:\\Users\\18337\\OneDrive\\桌面\\winter quarter 2023\\STA207\\Final\\Smooth.png"
ggsave(filename, device="png", width = 20, height = 14, units = "cm")

# fanova
library(fda)
library(fdANOVA)

gait.data.frame <- as.data.frame(gait)
x.gait <- as.matrix(gait.data.frame[, 1:39])

# vector of group labels
group.label.gait <- rep(1:3, each = 13)
fanova1 <- fanova.tests(x.gait, group.label.gait)
summary(fanova1)


gait.data.frame <- as.data.frame(gait)
x.gait <- vector("list", 2)
x.gait[[1]] <- as.matrix(gait.data.frame[, 1:39])
x.gait[[2]] <- as.matrix(gait.data.frame[, 40:78])
# vector of group labels
group.label.gait <- rep(1:3, each = 13)
fmanova1 <- fmanova.ptbfr(x.gait, group.label.gait)
summary(fmanova1)
# the tests based on a basis function representation with non-default parameters
set.seed(123)
fmanova2 <- fmanova.ptbfr(x.gait, group.label.gait, int = c(0.025, 0.975), B = 5000,
                          basis = "b-spline", criterion = "eBIC", commonK = "mean",
                          minK = 5, maxK = 20, norder = 4, gamma.eBIC = 0.7)
summary(fmanova2)
# the tests based on a basis function representation
# with predefined basis function representation
library(fda)
fbasis <- create.fourier.basis(c(0, nrow(x.gait[[1]])), 17)
own.basis <- vector("list", 2)
own.basis[[1]] <- Data2fd(1:nrow(x.gait[[1]]), x.gait[[1]], fbasis)$coefs
own.basis[[2]] <- Data2fd(1:nrow(x.gait[[2]]), x.gait[[2]], fbasis)$coefs
own.cross.prod.mat <- diag(rep(1, 17))
set.seed(123)
fmanova3 <- fmanova.ptbfr(group.label = group.label.gait,
                          B = 1000, basis = "own",
                          own.basis = own.basis,
                          own.cross.prod.mat = own.cross.prod.mat)
summary(fmanova3)
library(fda)
fbasis <- create.bspline.basis(c(0, nrow(x.gait[[1]])), 20, norder = 4)
own.basis <- vector("list", 2)
own.basis[[1]] <- Data2fd(1:nrow(x.gait[[1]]), x.gait[[1]], fbasis)$coefs
own.basis[[2]] <- Data2fd(1:nrow(x.gait[[2]]), x.gait[[2]], fbasis)$coefs
own.cross.prod.mat <- inprod(fbasis, fbasis)
set.seed(123)
fmanova4 <- fmanova.ptbfr(group.label = group.label.gait,
                          B = 1000, basis = "own",
                          own.basis = own.basis,
                          own.cross.prod.mat = own.cross.prod.mat)
summary(fmanova4)

# residuals
fit = model1
resid <- residuals(fit)
fitted <- fitted(fit)
df <- data.frame(resid = resid, fitted = fitted)

ggplot(df, aes(x = fitted, y = resid)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values Plot")
# obtain math scaled scores in the 1st grade with teachers as the unit
filename = "C:\\Users\\18337\\OneDrive\\桌面\\winter quarter 2023\\STA207\\Final\\homo.png"
ggsave(filename, device="png", width = 20, height = 14, units = "cm")

####################### FDA
L25 = c()
L5 = c()
L1 = c()
R25 = c()
R5 = c()
R1 = c()

ses = 1
n.trials=length(session[[ses]]$spks)
n.neurons=dim(session[[ses]]$spks[[1]])[1]
dimensions = dim(session[[ses]]$spks[[1]])[2]
for(i in 1:n.trials){
  if (session[[ses]]$contrast_left[i] == 1){
    L1[i]=1
    L25[i] = 0
    L5[i] = 0
  }else if (session[[ses]]$contrast_left[i] == 0.5){
    L1[i]=0
    L25[i] = 0
    L5[i] = 1
  }else if(session[[ses]]$contrast_left[i] == 0.25){
    L1[i]=0
    L25[i] = 1
    L5[i] = 0
  }else{
    L1[i]=0
    L25[i] = 0
    L5[i] = 0
  }
  if (session[[ses]]$contrast_right[i] == 1){
    R1[i]=1
    R25[i] = 0
    R5[i] = 0
  }else if (session[[ses]]$contrast_right[i] == 0.5){
    R1[i]=0
    R25[i] = 0
    R5[i] = 1
  }else if(session[[ses]]$contrast_right[i] == 0.25){
    R1[i]=0
    R25[i] = 1
    R5[i] = 0
  }else{
    R1[i]=0
    R25[i] = 0
    R5[i] = 0
  }
}


L25R25 = L25*R25
L25R5 = L25*R5
L25R1 = L25*R1
L5R25 = L5*R25
L5R5 = L5*R5
L5R1 = L5*R1
L1R25 = L1*R25
L1R5 = L1*R5
L1R1 = L1*R1
mat = matrix(nrow=n.trials, ncol=39)



for(i in 1:n.trials){
  l = colSums(session[[ses]]$spks[[i]])
  mat[i,] = l
}
fdad = as.data.frame(mat)
fdad['L1'] = L1
fdad['L5'] = L5
fdad['L25'] = L25
fdad['R1'] = R1
fdad['R25'] = R25
fdad['R5'] = R5
fdad['L25R25'] = L25R25
fdad['L25R5'] = L25R5
fdad['L25R1'] = L25R1
fdad['L5R25'] = L5R25
fdad['L5R5'] = L5R5
fdad['L5R1'] = L5R1
fdad['L1R25'] = L1R25
fdad['L1R5'] = L1R5
fdad['L1R1'] = L1R1

head(fdad)
list_vec=function(df, n){
  emp_list = c()
  for (i in 1: n){
    tmp = c()
    for (j in 1:39){
      tmp[j] = df[i,j]
    }
    emp_list[[i]] = tmp
  }
  return(emp_list)
}

y.list = list_vec(fdad, nrow(fdad))
vec = 1: 39
tdf = data.frame(t(replicate(nrow(fdad), vec)))
t.list = list_vec(tdf, nrow(fdad))
Y = list(Ly=y.list, Lt=t.list)
outGrid = seq(1, 39, by=1)
vars = list(X_1 = fdad[,40], X_2 = fdad[,41],X_3 = fdad[,42], 
            X_4 = fdad[,43],X_5 = fdad[,44], X_6 = fdad[,45],
            X_7 = fdad[,46], X_8 = fdad[,47],X_9 = fdad[,48], 
            X_10 = fdad[,49], X_11 = fdad[,50],X_12 = fdad[,51],
            X_13 = fdad[,52], X_14 = fdad[,53],X_15 = fdad[,54],
            Y=Y)
res = ConcurReg(vars, outGrid, kern='gauss',
                measurementError=TRUE, diag1D='none', useGAM = FALSE, returnCov=TRUE)

plot(res$beta[9,],type='l')
### bootstrap
boots = function(x){
  res = c()
  s = sample(nrow(fdad), nrow(fdad), replace = TRUE)
  d = fdad[s,]
  list_vec=function(df, n){
    emp_list = c()
    for (i in 1: n){
      tmp = c()
      for (j in 1:39){
        tmp[j] = df[i,j]
      }
      emp_list[[i]] = tmp
    }
    return(emp_list)
  }
  y.list = list_vec(d, nrow(d))
  vec = 1: 39
  tdf = data.frame(t(replicate(nrow(d), vec)))
  t.list = list_vec(tdf, nrow(d))
  Y = list(Ly=y.list, Lt=t.list)
  outGrid = seq(1, 39, by=1)
  vars = list(X_1 = d[,40], X_2 = d[,41],X_3 = d[,42], 
              X_4 = d[,43],X_5 = d[,44], X_6 = d[,45],
              X_7 = d[,46], X_8 = d[,47],X_9 = d[,48], 
              X_10 = d[,49], X_11 = d[,50],X_12 = d[,51],
              X_13 = d[,52], X_14 = d[,53],X_15 = d[,54],
              Y=Y)
  try({res = ConcurReg(vars, outGrid, kern='gauss',
                  measurementError=TRUE, diag1D='none', useGAM = FALSE, returnCov=TRUE)})

  if (length(res) != 0){
    return (c(res$beta[7,], res$beta[8,], res$beta[9,]))
  }else{
    return (1)
  }
}

x1 = boots(1)
x2 = boots(2)

cl = makeCluster(20)
clusterExport(cl, "fdad")
clusterEvalQ(cl, {
  library(fdaconcur)
})
x.list = sapply(1:100, list)
system.time({res_list = parLapply(cl, x.list , boots)})

no.one = c()
flag = 1
for (i in 1:length(res_list)){
  if (length(res_list[[i]]) > 10){
    no.one[[flag]] = res_list[[i]]
    flag = flag + 1
  }
}
quant.beta1 = function(l){
  
  upper.q = c()
  lower.q = c()
  for (i in 1:77){
    colu = c()
    for (j in 1: length(l)){
      colu[j] = l[[j]][i] 
    }
    print(colu)
    upper.q[i-1] = sort(colu)[round(length(l)*0.975)]
    lower.q[i-1] = sort(colu)[round(length(l)*0.025)]
  }
  return(list("UP"=upper.q, "LO"=lower.q))
}

qbeta1 = quant.beta1(no.one)

qbeta1$UP
qbeta1$LO
qbeta1LO = qbeta1$LO[41:76]
qbeta1UP = qbeta1$UP[41:76]

# Create data frames for each of the curves
df1 = data.frame(x = 2:37, y = res$beta[8,][2:37])
df2 = data.frame(x = 2:37, y = qbeta1LO)
df3 = data.frame(x = 2:37, y = qbeta1UP)

# Compute the lower and upper bounds of the ribbon
lower_bound = qbeta1LO
upper_bound = qbeta1UP

# Combine the bounds into a data frame
df_bounds = data.frame(x = 2:37, ymin = lower_bound, ymax = upper_bound)
ggplot() +
  geom_ribbon(data = df_bounds, aes(x = x, ymin = ymin, ymax = ymax), fill = "#ADD8E6") +
  geom_line(data = df1, aes(x = x, y = y), color = "red") +
  geom_line(data = df2, aes(x = x, y = y), color = "blue") +
  geom_line(data = df3, aes(x = x, y = y), color = "blue") + ylab('Beta function')+
  xlab('Time bins')
filename = "C:\\Users\\18337\\OneDrive\\桌面\\winter quarter 2023\\STA207\\Final\\beta1.png"
ggsave(filename, device="png", width = 20, height = 14, units = "cm")
quant.beta1 = function(l){
  
  upper.q = c()
  lower.q = c()
  for (i in 1:length(res_list[[1]])){
    colu = c()
    for (j in 1: length(l)){
      colu[j] = l[[j]][i] 
    }
    print(colu)
    upper.q[i-1] = sort(colu)[round(length(l)*0.975)]
    lower.q[i-1] = sort(colu)[round(length(l)*0.025)]
  }
  return(list("UP"=upper.q, "LO"=lower.q))
}

qbeta1 = quant.beta1(no.one)

qbeta1$UP
qbeta1$LO
qbeta1LO = qbeta1$LO[79:115]
qbeta1UP = qbeta1$UP[79:115]

# Create data frames for each of the curves
df1 = data.frame(x = 2:38, y = res$beta[9,][2:38])
df2 = data.frame(x = 2:38, y = qbeta1LO)
df3 = data.frame(x = 2:38, y = qbeta1UP)

# Compute the lower and upper bounds of the ribbon
lower_bound = qbeta1LO
upper_bound = qbeta1UP

# Combine the bounds into a data frame
df_bounds = data.frame(x = 2:38, ymin = lower_bound, ymax = upper_bound)
ggplot() +
  geom_ribbon(data = df_bounds, aes(x = x, ymin = ymin, ymax = ymax), fill = "#ADD8E6") +
  geom_line(data = df1, aes(x = x, y = y), color = "red") +
  geom_line(data = df2, aes(x = x, y = y), color = "blue") +
  geom_line(data = df3, aes(x = x, y = y), color = "blue") + ylab('Beta function')+
  xlab('Time bins')
filename = "C:\\Users\\18337\\OneDrive\\桌面\\winter quarter 2023\\STA207\\Final\\beta2.png"
ggsave(filename, device="png", width = 20, height = 14, units = "cm")
# prediction
library(caret)

test.max1 = c()
test.max2 = c()
test.mean1 = c()
test.mean2 = c()
test.var1 = c()
test.var2 = c()
train.max1 = c()
train.max2 = c()
train.mean1 = c()
train.mean2 = c()
train.var1 = c()
train.var2 = c()
t.stimulil = c()
t.stimulir = c()
tr.stimulil = c()
tr.stimulir = c()
t.res = c()
tr.res = c()

k = 1
for (ses in 1:5){
  n.trials=length(session[[ses]]$spks)
  n.neurons=dim(session[[ses]]$spks[[1]])[1]
  bins = dim(session[[ses]]$spks[[1]])[2]
  if (ses == 1){
    for(i in 1:n.trials){
      test.max1[i] = max(colSums(session[[ses]]$spks[[i]]))
      test.max2[i] = max(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      test.mean1[i] = mean(colSums(session[[ses]]$spks[[i]]))
      test.mean2[i] = mean(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      test.var1[i] = var(colSums(session[[ses]]$spks[[i]]))
      test.var2[i] = var(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      t.stimulil[i] = session[[ses]]$contrast_left[i]
      t.stimulir[i] = session[[ses]]$contrast_right[i]
      t.res[i] = session[[ses]]$feedback_type[i]
    }
  }else{
    for(i in 1:n.trials){
      train.max1[k] = max(colSums(session[[ses]]$spks[[i]]))
      train.max2[k] = max(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      train.mean1[k] = mean(colSums(session[[ses]]$spks[[i]]))
      train.mean2[k] = mean(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      train.var1[k] = var(colSums(session[[ses]]$spks[[i]]))
      train.var2[k] = var(colSums(session[[ses]]$spks[[i]])[as.integer(bins*0.5):bins])
      tr.stimulil[k] = session[[ses]]$contrast_left[i]
      tr.stimulir[k] = session[[ses]]$contrast_right[i]
      tr.res[k] = session[[ses]]$feedback_type[i]
      k = k + 1
    }
  }
}

train = data.frame("max1"=train.max1)
train["max2"] = train.max2
train["mean1"] = train.mean1
train["mean2"] = train.mean2
train["var1"] = train.var1
train["var2"] = train.var2
train["stimulil"] = as.factor(tr.stimulil)
train["stimulir"] = as.factor(tr.stimulir)
train["res"] = as.factor(tr.res)
head(train)

test = data.frame("max1"=test.max1)
test["max2"] = test.max2
test["mean1"] = test.mean1
test["mean2"] = test.mean2
test["var1"] = test.var1
test["var2"] = test.var2
test["stimulil"] = as.factor(t.stimulil)
test["stimulir"] = as.factor(t.stimulir)
test["res"] = as.factor(t.res)
head(test)

library(randomForest)
rf = randomForest(res~., data=train, proximity=TRUE, ntrees=10000)

p1 = predict(rf, train)
confusionMatrix(p1, train$res)

p2 = predict(rf, test, type="prob")
p2
p3 = predict(rf, test)
c = confusionMatrix(p3, test$res)
kable(c$table)
result.roc <- roc(test$res, p2[,1]) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)








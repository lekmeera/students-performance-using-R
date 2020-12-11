```{r}
dat<- read.csv('D:\\Data Science\\Projects\\performance\\sperformance-dataset.csv',na = '?')
dat
```
```{r}
colnames(dat)
```
```{r}
levels(dat$ï..school) <- sub("GP", "1", levels(dat$ï..school))
levels(dat$sex) <- sub("F", "1", levels(dat$sex))
levels(dat$sex) <- sub("M", "2", levels(dat$sex))
levels(dat$address) <- sub("R", "1", levels(dat$address))
levels(dat$address) <- sub("U", "2", levels(dat$address))
levels(dat$famsize) <- sub("GT3", "1", levels(dat$famsize))
levels(dat$famsize) <- sub("LE3", "2", levels(dat$famsize))
levels(dat$Pstatus) <- sub("T", "1", levels(dat$Pstatus))
levels(dat$Pstatus) <- sub("A", "2", levels(dat$Pstatus))
dat
```
```{r}
Mjob<-as.factor(c("at_home","services","health","teacher","other"))
Mjob
unclass(Mjob)
dat
```
```{r}
must_convert<-sapply(dat,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
dat2<-sapply(dat[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(dat[,!must_convert],dat2)   # complete data.frame with all variables put together
out
```
```{r}
dat2
colnames(dat2)
colnames(out)
colnames(dat)
```
```{r}
data(out)
corr <- round(cor(out), 1)
head(corr[, 1:6])
```
```{r}
sapply(out, mean, na.rm=TRUE)
```
```{r}
#Describe data
library(Hmisc)
describe(out)

#Summary
summary(out)
```
```{r}
library(corrplot)
library(Hmisc)
matriz <-rcorr(as.matrix(out), type=c("spearman"))
matriz
```
```{r}
#correlation matrix
corr_mat <- cor(out[,3:ncol(out)])
corrplot(corr_mat, order = "hclust", tl.cex = .5, addrect = 9)
```
```{r}
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(corr_mat, col=col, symm=TRUE)
```
```{r}
#histogram
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(out[,i], main=names(out)[i])
}
```
```{r}
# density plot for each attribute
par(mfrow=c(3,3))
complete_cases <- complete.cases(out)
for(i in 1:9) {
  plot(density(out[complete_cases,i]), main=names(out)[i])
}

```
```{r}
# boxplots for each attribute
par(mfrow=c(3,3))
for(i in 1:9) {
  boxplot(out[,i], main=names(out)[i])
}
```
```{r}
barplot(dat2,
        main="Performance of Students",
        border="red",
        col="blue",
        density=100
)
```

```{r}
#scatter plot
jittered_x <- sapply(out[,1:9], jitter)
pairs(jittered_x, names(out[,1:9]), col=out$pG3)
```
```{r}
#pie chart
pie(out$Pstatus,
    labels=as.character(out$Pstatus),
    main="Parent Status",
    col=c("red","yellow"),
    border="brown",
    clockwise=TRUE
)
```

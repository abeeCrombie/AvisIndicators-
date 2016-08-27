require(quantmod)

####Input####

dat.1           = getSymbols("wmt", auto.assign=F)
colnames(dat.1) = c("Open","High","Low","Close","Volume","Adjusted")
bbands.close = BBands(dat.1[,"Close"], n=100, sd=3)
tail(bbands.close)
dat.2 = cbind(dat.1,bbands.close)
inBB = as.numeric(ifelse((dat.2$Close > dat.2$up | dat.2$Close < dat.2$dn) ,1,0))
sum(inBB, na.rm = TRUE)
length(inBB[!is.na(inBB)])
pct_of_time_outside_bb = sum(inBB, na.rm = TRUE) / length(inBB[!is.na(inBB)])
print(pct_of_time_outside_bb)

## create a for loop and get the higest percentage of time within some bollinger band 


## Create MA envelopes 
sma150 = as.numeric(runMean(dat.1$Close, 150))
sma150_p3 = as.numeric(runMean(dat.1$Close, 150)*1.03)
sma150_m3 = as.numeric(runMean(dat.1$Close, 150)*.97)
dat.2 = cbind(dat.1,SMA150 =sma150,SMA150P =sma150_3, SMA150M = sma150_m3)
ema_150 = EMA(dat.1$Close, n = 150)
ema_150_h = EMA(dat.1$High, n= 150)
dat.3 = cbind(dat.1,ema_150, ema_150_h)

inSMA = as.numeric(ifelse((dat.1$Close < sma150_p3 & 
                             dat.1$Close > sma150_m3 ),1,0  )   )
dat.2 =cbind(dat.2, inSMAscore= inSMA)
pct_of_time_inside_SMA = sum(inSMA, na.rm = TRUE) / length(inSMA[!is.na(inSMA)])

###inEMA 
SMA_to_test = c(9,10,15,20,30,50,60,80,90,100,120,130,140,150,180,200,250,300)
PCT_to_test = c(1,2,3,4,5)
Pct_of_time_inside_SMA = matrix(nrow = length(SMA_to_test), ncol = length(PCT_to_test))
Sum_touches = matrix(nrow = length(SMA_to_test), ncol = length(PCT_to_test))
for(i in 1:length(SMA_to_test)){  # for(k in 1:length(runStarts)){
  for( j in 1:length(PCT_to_test)) {
  sma_loopV = as.numeric(runMean(dat.1$Close, SMA_to_test[i]))
  sma_loopV_p3 = as.numeric(runMean(dat.1$Close, SMA_to_test[i])*(1 +PCT_to_test[j]*.01))
  sma_loopV_m3 = as.numeric(runMean(dat.1$Close, SMA_to_test[i])*(1 -PCT_to_test[j]*.01))
  
  dat.5 = cbind(dat.1,SMA =sma_loopV, SMA_P =sma_loopV_p3, 
                SMA_M = sma_loopV_m3) 
  inSMA_loop = as.numeric(ifelse((dat.5$Close < sma_loopV_p3 & 
                               dat.5$Close > sma_loopV_m3 ),1,0  )   )
  touches = as.numeric(ifelse(  (dat.5$High > (sma_loopV_p3 * .99) 
                               & dat.5$Close < (sma_loopV_p3 * 1.01) | 
                                 (dat.5$Low < (sma_loopV_m3 *1.01)  & dat.5$Close > (sma_loopV_m3 *.99) ))
                               ,1,0))
  
  dat.5 = cbind(dat.5, InSMA =inSMA_loop, touches = touches)
  Pct_of_time_inside_SMA[i,j] = 100 *sum(inSMA_loop, na.rm = TRUE) / length(inSMA_loop[!is.na(inSMA_loop)])
  ##Pct_of_time_inside_SMA[i,j] = i*j
  Sum_touches[i,j] = sum(touches, na.rm = TRUE) / length(touches[!is.na(touches)])
  
  print(i)
  print(j)
  }
}
#Sum_touches[Pct_of_time_inside_SMA <.30]=0
# Pct_of_time_inside_SMA[Pct_of_time_inside_SMA < 70] = 0
combined = ifelse(Sum_touches<.30,0,Sum_touches) * 
  ifelse(Pct_of_time_inside_SMA < 70, 0, Pct_of_time_inside_SMA) 
colnames(combined) = c(paste0("Pct_", (PCT_to_test)))
rownames(combined) = c(paste0("SMA_", (SMA_to_test)))

## moving average bounce test 

## if price has been above a moving average for a lot of time (%) and has some touches 

aboveSMA = as.numeric(ifelse(dat.5$Low > sma150, 1,0     ))
dat.5 = cbind(dat.5, AboveSMA = aboveSMA)
Pct_above_SMA = sum(aboveSMA, na.rm = TRUE) / length(aboveSMA[!is.na(aboveSMA)])
Tests_on_SMA
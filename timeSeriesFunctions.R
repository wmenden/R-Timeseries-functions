rm(list=ls(all=TRUE)); 
# Loading necessary libraries      
library(quantmod)
library(forecast)
library(TSA)
library(tseries)
library(fGarch)
library(ggplot2)

#    **** About Project Code ****
#    User must have the following r packages installed:
#           quantmod
#           forecast
#           TSA
#    The program is started calling projectMain() from the console.
#    code will throw errors; as error handling has yet to be included.
#    
#    Models are judged against like models based on their respective
#    AICc. 
#    The best model is judged based on the lowest MSE of the forecasts.  As well as 
#    the percent correct of the direction of the forecast period.
#    Forecasts are based ## periods ahead, supplied by user, using the following methodology    
#           1) After each forecast the SSE is determine for that forecast.                     
#           2) The actual price observed is added back into the time
#              series and the next price is forecast repeating step 1.
#           3) The MSE of the forecast is generated as the basis for
#              model comparison
#           4) Direction correct is calculated. (1 correct, 0 incorrect)

# Function to retrieve stock symbols of interest
getSecurities <- function(){
      
      prompt = TRUE
      stocks <- vector()
      cat("\nStock symbols must be in CAPITAL LETTERs\n")
      while (prompt == TRUE){            
            stock<- readline("Enter Ticker symbol: ")
            stocks <- c(stocks,stock)
            response <- readline("Do you want to enter another symbol? (y/n) ")
            if (response != "y")
                  prompt <- FALSE            
      }      
      # returns a vector of stock symbols
      return(stocks)
}

projectMain <- function(){
      # Loading necessary libraries      
      library(quantmod)
      library(forecast)
      library(TSA)
      library(tseries)
      library(fGarch)
      library(ggplot2)
      
      prompt <- TRUE
      stocklist <- getSecurities()
      
      # creating new environment
      myEnv <- new.env()
      # Setting date range for environment
      cat("Enter start date and end date as yyyy-mm-dd\n")
      startDate = as.Date(readline("Enter start date: ")) 
      endDate = as.Date(readline("Enter end date: "))
      # retrieving adjusted stock prices from yahoo finance
      getSymbols(stocklist, envir=myEnv ,src = "yahoo", 
           from = startDate, to = endDate)
      count <- 0
      while(prompt == TRUE){
            
            if (count == 0){
                  cat(stocklist)
                  symb <- readline("Enter Ticker symbol from list: ")
                  adj.dat <-(Ad(get(symb, envir=myEnv)))
                  modFor1 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                        a= numeric(0), b= numeric(0), c = numeric(0))
                  modFor2 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                        a= numeric(0), b= numeric(0), c = numeric(0))
                  modFor3 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                        a= numeric(0), b= numeric(0), c = numeric(0))
                  modFor4 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                        a= numeric(0), b= numeric(0), c = numeric(0))
            }
            
            cat("*** MAIN Menu ***\n")
            cat("1 -- Explore data\n")
            cat("2 -- Compare Models\n")
            cat("3 -- Generate ARIMA\n")
            cat("4 -- Generate ARFIMA\n")
            cat("5 -- Generate SARIMA\n")
            cat("6 -- Generate GARCH\n")
            cat("7 -- Select another security\n")
            cat("8 -- Select another time frame\n")
            cat("9 -- Exit\n\n\n")
            
            input <- readline("Select 1 -- 9: ")
            switch(input,
                 "1"={exploreMyData(adj.dat)},
                 "2"={comp <- data.frame()
                      comp <- getModComp(modFor1,modFor2,modFor3,modFor4)
                      print(comp)
                      cat("\n\n")},
                 "3"={modFor1 <- getMyArima(adj.dat)
                      print(modFor1)
                      cat("\nSum of squared errors\n")
                      sse <-sum(modFor1[,3])
                      cat(sse)
                      cat("\n\n")
                      mse <- sse/length(modFor1[,3])
                      cat("Mean Squared Error\n")
                      cat(mse)
                      cat("\n\n")
                      pCorct<- sum(modFor1[,6])/length(modFor1[,6])
                      cat("Percent in correct direction\n")
                      cat(pCorct)
                      cat("\n\n")},
                 "4"={modFor2 <- getMyArfima(adj.dat)
                      print(modFor2)
                      cat("\nSum of squared errors\n")
                      sse <-sum(modFor2[,3])
                      cat(sse)
                      cat("\n\n")
                      mse <- sse/length(modFor2[,3])
                      cat("Mean Squared Error\n")
                      cat(mse)
                      cat("\n\n")
                      pCorct<- sum(modFor2[,6])/length(modFor2[,6])
                      cat("Percent in correct direction\n")
                      cat(pCorct)
                      cat("\n\n")},
                 "5"={modFor3 <- getMyArimaSeason(adj.dat)
                      print(modFor3)
                      cat("\nSum of squared errors\n")
                      sse <-sum(modFor3[,3])
                      cat(sse)
                      cat("\n\n")
                      mse <- sse/length(modFor3[,3])
                      cat("Mean Squared Error\n")
                      cat(mse)
                      cat("\n\n")
                      pCorct<- sum(modFor3[,6])/length(modFor3[,6])
                      cat("Percent in correct direction\n")
                      cat(pCorct)
                      cat("\n\n")},
                 "6"={modFor4 <- getMyGarch(adj.dat)
                      print(modFor4)
                      cat("\nSum of squared errors\n")
                      sse <-sum(modFor4[,3])
                      cat(sse)
                      cat("\n\n")
                      mse <- sse/length(modFor4[,3])
                      cat("Mean Squared Error\n")
                      cat(mse)
                      cat("\n\n")
                      pCorct<- sum(modFor4[,6])/length(modFor4[,6])
                      cat("Percent in correct direction\n")
                      cat(pCorct)
                      cat("\n\n")},
                 "7"={cat(stocklist)
                      symb <- readline("Enter Ticker symbol from list: ")
                      adj.dat <-(Ad(get(symb, envir=myEnv)))},
                 "8"={myEnv <- new.env()
                      cat("Enter start date and end date as yyyy-mm-dd\n")
                      startDate = as.Date(readline("Enter start date: ")) 
                      endDate = as.Date(readline("Enter end date: "))
                      # retrieving adjusted stock prices from yahoo finance
                      getSymbols(stocklist, env = myEnv, src = "yahoo", 
                                 from = startDate, to = endDate)
                      cat(stocklist)
                      symb <- readline("Enter Ticker symbol from list: ")
                      adj.dat <-(Ad(get(symb, envir=myEnv)))
                      modFor1 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                            a= numeric(0), b= numeric(0), c = numeric(0))
                      modFor2 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                            a= numeric(0), b= numeric(0), c = numeric(0))
                      modFor3 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                            a= numeric(0), b= numeric(0), c = numeric(0))
                      modFor4 <- data.frame(x= numeric(0), y= numeric(0), z = numeric(0),
                                            a= numeric(0), b= numeric(0), c = numeric(0))},
                 "9"={prompt <- FALSE},
                 cat("\nInvalid Entry!\n"))
            count <- count + 1
                 
      }
      
      
}

# returns volatility  of the security of interest
# must provide log returns
getVol <- function(data){
      vol <- sd(data)*sqrt(252)
}

# Data Exploration
exploreMyData <- function(data){
      
      symbolName <- substr(names(data),1,4)
      print(plot(data, main = symbolName, 
           ylab = symbolName))     
      readline(prompt = "Pause. Press <Enter> to continue...")
      returnName <- paste("Log returns ", symbolName)
      returnName2 <- paste("Square of log returns ", symbolName)
                  
      r.temp <- diff(log(data))
      r.temp <- na.omit(r.temp)
                  
      print(plot(r.temp, main = returnName))
      readline(prompt = "Pause. Press <Enter> to continue...")
                  
      qqnorm(r.temp,main=returnName)
      qqline(r.temp)
      readline(prompt = "Pause. Press <Enter> to continue...")
                  
      print(shapiro.test(as.numeric(r.temp)))
      cat("This p-value tells you what the chances are that the sample comes from a normal distribution.\n")
      cat("The lower this value, the smaller the chance.\n")
      readline(prompt = "Pause. Press <Enter> to continue...")

      hist(as.ts(r.temp), breaks=25, freq=FALSE, main = returnName)
      curve(dnorm(x, mean=mean(r.temp), sd=sd(r.temp)), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      cat("Skewness of distribution\n")
      print(skewness(r.temp))
      cat("\ndistribution with skewness > 0 - most values concentrated to left of mean\n")
      cat("distribution with skewness < 0 - most values concentrated to right of mean\n")
      cat("Kurtosis of Distribution\n")
      print(kurtosis(r.temp))
      cat("\nA distribution with kurtosis > 3 is called heavy-tailed\n")
      cat("Whereas it is light-tailed with kurtosis < 3\n")
      readline(prompt = "Pause. Press <Enter> to continue...")
                  
      acf(as.ts(r.temp), main = returnName)
      readline(prompt = "Pause. Press <Enter> to continue...")
                  
      pacf(as.ts(r.temp), main = returnName)
      readline(prompt = "Pause. Press <Enter> to continue...")
                  
      print(jarque.bera.test(as.ts(r.temp)))
                  
      r.tempSQ <- r.temp^2
                  
      acf(as.ts(r.tempSQ), main = returnName2)
      readline(prompt = "Pause. Press <Enter> to continue...")
      pacf(as.ts(r.tempSQ), main = returnName2)
      readline(prompt = "Pause. Press <Enter> to continue...")
            
      vol <- getVol(r.temp)
      print("volatility for entire data set")
      print(vol)
            

}

getMyArima <- function(data){
      
      arimaDat <- as.ts(data)
      pred <- as.numeric(readline("How many days would you like to predict? "))
      
      modelArima <- NULL 
      tempTs <- NULL
      fcArm <- NULL
      modForecast <- vector()
      Prior_Close <- vector()

      for(i in pred:1){
            
            endNew <- length(arimaDat)- i

            arimaTrainTS <- window(arimaDat, end = endNew )
            modelArima <- auto.arima(arimaTrainTS, seasonal=FALSE)
            myfor<- forecast(modelArima, h=1)
            
            modForecast <- append(modForecast,myfor$mean)
            Prior_Close<- append(Prior_Close, arimaTrainTS[endNew])
      }
      
      tempTS <- window(arimaDat, start = length(arimaDat)- pred+1,
                       end= length(arimaDat))
      forecastErr <- (tempTS- modForecast)^2
      
      fcArm <- data.frame()
      Correct_Dir <- vector()
      Actual_Price <- tempTS
      Model_Price <- round(modForecast, digits=2)
      Squared_Error <- forecastErr
      Actual_Change <- Actual_Price - Prior_Close
      Model_Change <- Model_Price -Prior_Close
      for(w in 1:length(Actual_Change)){
            if(Actual_Change[w]<0 && Model_Change[w]<0)
                  Correct_Dir[w]<- 1
            else if (Actual_Change[w]>0 && Model_Change[w]>0)
                  Correct_Dir[w]<-1
            else
                  Correct_Dir[w]<- 0
      }
      
      fcArm <- cbind(Actual_Price,Model_Price,Squared_Error,
                     Actual_Change, Model_Change, Correct_Dir)

      
      plot(modelArima$residual,main = "Final ARIMA Residuals", type="p",
           ylab="Residuals")
      print(summary(modelArima))      
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      qqnorm(modelArima$residual,main="ARIMA residuals QQ Plot")
      qqline(modelArima$residual)
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      acf(modelArima$residual, main = "ACF of Residuals")
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      #augmented dickey fuller lower the p value the better
      print(adf.test(modelArima$residual))
      boxmodArima <- Box.test(modelArima$residual,lag=20,
                            type="Ljung-Box",fitdf=7)
      print(boxmodArima)
      cat("\n")
      cat("The Ljung-Box test statistic (X-squared) gets larger as the sample\n")
      cat("auto-correlations of the residuals get larger.  The p-value is the\n")
      cat("probability of getting a value as large as or larger than that observed\n")
      cat("under the null hypothesis that the true innovations are independent.\n")
      cat("Therefore a small p-value is evidence against independence.\n\n")
      readline(prompt = "Pause. Press <Enter> to continue...")
      

      return(fcArm)
}

getMyArfima <- function(data){
      
      arfimaDat <- as.ts(data)
      pred <- as.numeric(readline("How many days would you like to predict? "))
      
      modelArfima <- NULL
      tempTs <- NULL
      modForecast <- vector()
      Prior_Close <- vector()
      
      for (i in pred:1){
            
            endNew <- length(arfimaDat)- i

            arfimaTrainTS <- window(arfimaDat, end = endNew )
            modelArf <- arfima(arfimaTrainTS)
            
            myfor<- forecast(modelArf, h=1)
            
            modForecast <- append(modForecast,myfor$mean)
            Prior_Close<- append(Prior_Close, arfimaTrainTS[endNew])
            
      }
      
      tempTS <- window(arfimaDat, start = length(arfimaDat)- pred+1,
                       end= length(arfimaDat))
      forecastErr <- (tempTS- modForecast)^2
      
      fcArf <- data.frame()
      Correct_Dir <- vector()
      Actual_Price <- tempTS
      Model_Price <- round(modForecast,digits=2)
      Squared_Error <- forecastErr
      Actual_Change <- Actual_Price - Prior_Close
      Model_Change <- Model_Price -Prior_Close
      for(w in 1:length(Actual_Change)){
            if(Actual_Change[w]<0 && Model_Change[w]<0)
                  Correct_Dir[w]<- 1
            else if (Actual_Change[w]>0 && Model_Change[w]>0)
                  Correct_Dir[w]<-1
            else
                  Correct_Dir[w]<- 0
      }
      
      
      fcArf <- cbind(Actual_Price,Model_Price,Squared_Error, 
                     Actual_Change, Model_Change, Correct_Dir)
      
      plot(modelArf$residual, main="Final ARFIMA Residuals",type="p",
           ylab="Residuals")
      print(summary(modelArf))
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      qqnorm(modelArf$residual,main="ARFIMA residuals QQ Plot")
      qqline(modelArf$residual)
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      acf(modelArf$residual, main = "ACF of Residuals")
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      #augmented dickey fuller lower the p value the better
      print(adf.test(modelArf$residual))
      boxmodArf <- Box.test(modelArf$residual,lag=20,
                          type="Ljung-Box",fitdf=7)
      print(boxmodArf)
      cat("\n")
      cat("The Ljung-Box test statistic (X-squared) gets larger as the sample\n")
      cat("auto-correlations of the residuals get larger.  The p-value is the\n")
      cat("probability of getting a value as large as or larger than that observed\n") 
      cat("under the null hypothesis that the true innovations are independent.\n")
      cat("Therefore a small p-value is evidence against independence.\n\n")
      readline(prompt = "Pause. Press <Enter> to continue...")
      

      return(fcArf)
      
}

getMyArimaSeason <- function(data){
      
      sarimaDat <- as.ts(data)
      beg <- 2
      end <- readline("Enter maximum seasonality factor: ")
      pred <- as.numeric(readline("How many periods would you like to forecast? "))
    
      modelSar <- NULL 
      tempTs <- NULL
      modForecast <- vector()
      Prior_Close <- vector()
      
      for (i in pred:1){
            
            myAICc <- numeric()
            
            endNew <- length(sarimaDat)- i
            sarimaTrainTS <- window(sarimaDat, end = endNew )
            
            
            for (j in beg:end){
                  sarimaTrainTS <- ts(sarimaTrainTS,freq=j)
                  model <-auto.arima(sarimaTrainTS)
                  if (length(myAICc) == 0){
                        myAICc <- model$aicc 
                        modelSar <- model
                  }
                  else{
                        if(model$aicc < myAICc){                              
                              myAICc <- model$aicc 
                              modelSar <- model 
                              
                        }              
                  }
                  
            }
            
            myfor<- forecast(modelSar, h=1)
            
            modForecast <- append(modForecast,myfor$mean)
            Prior_Close<- append(Prior_Close, sarimaTrainTS[endNew])
            
      }

      tempTS <- window(sarimaDat, start = length(sarimaDat)- pred+1,
                       end= length(sarimaDat))
      forecastErr <- (tempTS- modForecast)^2
      fcSar <- data.frame()
      Correct_Dir <- vector()
      Actual_Price <- tempTS
      Model_Price <- round(modForecast, digits=2)
      Squared_Error <- forecastErr
      Actual_Change <- Actual_Price - Prior_Close
      Model_Change <- Model_Price -Prior_Close
      for(w in 1:length(Actual_Change)){
            if(Actual_Change[w]<0 && Model_Change[w]<0)
                  Correct_Dir[w]<- 1
            else if (Actual_Change[w]>0 && Model_Change[w]>0)
                  Correct_Dir[w]<-1
            else
                  Correct_Dir[w]<- 0
      }
      
      
      fcSar <- cbind(Actual_Price,Model_Price,Squared_Error, 
                     Actual_Change, Model_Change, Correct_Dir)
           
      plot(modelSar$residual, type="p")
      print(summary(modelSar))
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      qqnorm(modelSar$residual,main="SARIMA residuals QQ Plot")
      qqline(modelSar$residual)
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      acf(modelSar$residual, main = "ACF of Residuals")
      readline(prompt = "Pause. Press <Enter> to continue...")

      print(adf.test(modelSar$residual))
      cat("augmented dickey fuller lower the p value the better")
      boxmodSar <- Box.test(modelSar$residual,lag=20,
                              type="Ljung-Box",fitdf=7)
      print(boxmodSar)
      cat("\n")
      cat("The Ljung-Box test statistic (X-squared) gets larger as the sample\n")
      cat("auto-correlations of the residuals get larger.  The p-value is the\n")
      cat("probability of getting a value as large as or larger than that observed\n") 
      cat("under the null hypothesis that the true innovations are independent.\n")
      cat("Therefore a small p-value is evidence against independence.\n\n")
      readline(prompt = "Pause. Press <Enter> to continue...")
      
      
      return (fcSar)
      
}

getMyGarch <- function(data){
      
      garchDat <- as.ts(data)      
      pred <- as.numeric(readline("How many periods would you like to forecast? "))
      
      
      prompt <- TRUE
      
      while(prompt==TRUE){
            
            myAIC <- NULL
            modelGarch <- NULL 
            tempTs <- NULL
            modForecast <- vector()
            modDir <- vector()
            Prior_Close <- vector()
            resp <- readline("Would you like to do ARMA Garch? (y/n) ")
            P <- 0
            Q <- 0
            
            for (i in pred:1){
                  
                  endNew <- length(garchDat)- i
                  garDatTrain <- window(garchDat, end= endNew)
                  r.GDat <-  diff(log(garDatTrain))
                  r.GDat <- na.omit(r.GDat)
                  # getting parameters returned by auto arima with d set to zero
                  if(resp=="y"){
                        garARMA <- auto.arima(r.GDat,d=0, seasonal=FALSE)
                        P <- garARMA$arma[1]
                        Q <- garARMA$arma[2]
                  }
                  
                  for (p in 1:2){
                        for(q in 1:2){
                              
                              if(resp=="y"){ #ARMA GARCH model
                                    model=garchFit(formula=substitute(~arma(P,Q)+garch(p,q),list(P=P,Q=Q,p=p,q=q)), data=r.GDat,
                                                   trace=F,cond.dist="std")
                              }
                              else {  # GARCH model
                                    model=garchFit(formula=substitute(~garch(p,q),list(p=p,q=q)), data=r.GDat,
                                                   trace=F,cond.dist="std") 
                              }
                              
                              # Determining best model by AIC
                              if (length(myAIC) == 0){
                                    myAIC <- model@fit$ics[1]
                                    modelGarch<- model
                              }
                              else{
                                    if(model@fit$ics[1] < myAIC){                              
                                          myAIC <- model@fit$ics[1] 
                                          modelGarch <- model 
                                          
                                    }              
                              }
                        }
                  }
            
                  
                  
                  myfor<- predict(modelGarch, n.ahead=1)
                  lastPrice <- garDatTrain[endNew]                  
                  chnge <- exp(myfor$meanForecast)
                  forecastPrice <- lastPrice * chnge     
                  modForecast <- append(modForecast,forecastPrice)
                  Prior_Close<- append(Prior_Close, garDatTrain[endNew])
                  
            }
            
            
            tempTS <- window(garchDat, start = length(garchDat)- pred+1,
                             end= length(garchDat))
            forecastErr <- (tempTS- modForecast)^2
            
            fcGar <- data.frame() 
            Correct_Dir <- vector()
            Actual_Price <- tempTS
            Model_Price <- round(modForecast, digits=2)
            Squared_Error <- forecastErr
            Actual_Change <- Actual_Price - Prior_Close
            Model_Change <- Model_Price -Prior_Close
            for(w in 1:length(Actual_Change)){
                  if(Actual_Change[w]<0 && Model_Change[w]<0)
                        Correct_Dir[w]<- 1
                  else if (Actual_Change[w]>0 && Model_Change[w]>0)
                        Correct_Dir[w]<-1
                  else
                        Correct_Dir[w]<- 0
            }
            
            fcGar <- cbind(Actual_Price,Model_Price,Squared_Error, Actual_Change, Model_Change, Correct_Dir)
            
           
            
            plot(residuals(modelGarch), type="p")
            print(summary(modelGarch))           
            readline(prompt = "Pause. Press <Enter> to continue...")
            
            qqnorm(residuals(modelGarch),main="GARCH residuals QQ Plot")
            qqline(residuals(modelGarch))
            readline(prompt = "Pause. Press <Enter> to continue...")
            
            acf(residuals(modelGarch), main = "ACF of Residuals")
            readline(prompt = "Pause. Press <Enter> to continue...")
            
            boxmodGar <- Box.test(residuals(modelGarch),lag=20,
                                   type="Ljung-Box",fitdf=7)
            print(boxmodGar)
            cat("\n")
            cat("The Ljung-Box test statistic (X-squared) gets larger as the sample\n")
            cat("auto-correlations of the residuals get larger.  The p-value is the\n")
            cat("probability of getting a value as large as or larger than that observed\n") 
            cat("under the null hypothesis that the true innovations are independent.\n")
            cat("Therefore a small p-value is evidence against independence.\n\n")
            
            
            response <- readline(prompt = "Would you like to run another model? (y/n)")
            
            if(response != "y"){
                  prompt <- FALSE
            }
                  
      
      }
      
      return(fcGar)
      
}

getModComp <- function(data1=NULL,data2=NULL,data3=NULL,data4=NULL){

      compareDF <- data.frame(x=character(),y=numeric(),z=numeric(),a=numeric())
      Model_Type <- vector()
      SSE <- numeric()
      MSE <- numeric()
      Percent_Correct <- numeric()
      
      if(length(data1[,1]) > 0){
            sseArima <-sum(data1[,3])
            mseArima <- sseArima/length(data1[,3])
            pCorct<- sum(data1[,6])/length(data1[,6])
            Model_Type <- append(Model_Type, "ARIMA")
            SSE <- append(SSE,sseArima)
            MSE <- append(MSE,mseArima)
            Percent_Correct <- append(Percent_Correct,pCorct)
      }
      if(length(data2[,1]) > 0){
            sseArfima <-sum(data2[,3])
            mseArfima <- sseArfima/length(data2[,3])
            pCorct<- sum(data2[,6])/length(data2[,6])
            Model_Type <- append(Model_Type, "ARFIMA")
            SSE <- append(SSE,sseArfima)
            MSE <- append(MSE,mseArfima)
            Percent_Correct <- append(Percent_Correct,pCorct)
      }
                
      if(length(data3[,1]) > 0){
            sseSarima <-sum(data3[,3])
            mseSarima <- sseSarima/length(data3[,3])
            pCorct<- sum(data3[,6])/length(data3[,6])
            Model_Type <- append(Model_Type, "SARIMA")
            SSE <- append(SSE,sseSarima)
            MSE <- append(MSE,mseSarima)
            Percent_Correct <- append(Percent_Correct,pCorct)
      }
            
      if(length(data4[,1]) > 0){
            sseGarch <-sum(data4[,3])
            mseGarch <- sseGarch/length(data4[,3])
            pCorct<- sum(data4[,6])/length(data4[,6])
            Model_Type <- append(Model_Type, "GARCH")
            SSE <- append(SSE,sseGarch)
            MSE <- append(MSE,mseGarch)
            Percent_Correct <- append(Percent_Correct,pCorct)
      }
            

      compareDF <- cbind(Model_Type,SSE,MSE,Percent_Correct)
      
      return(compareDF)
}


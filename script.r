source('./r_files/flatten_HTML.r')


calculate_bootstrap <- function(weights,values, B){
    means = list()


    samp_arr <- 1:length(values)
    for(i in 1:B){
        idx <- sample(samp_arr, length(values), replace=T)
        means[i] <- sum(weights[idx]*values[idx])/sum(weights[idx])
    }
    return(as.numeric(means))
}

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
#g = qplot(Day,Quantity, data = Values, main = Sys.time());

#Get number of resamples setting
B=10000
if(exists("settings_bootstrap_params_resamples"))
{
  B = settings_bootstrap_params_resamples
}


#Get confidence interval setting
ci=95
if(exists("settings_bootstrap_params_confint"))
{
    ci = settings_bootstrap_params_confint
}

#Get number of bins setting
nb=50
if(exists("settings_bootstrap_params_numbins"))
{
    nb = settings_bootstrap_params_numbins
}


####################################################

############# Create and save widget ###############
#p = ggplotly(g);


#Get histogram
if(exists("weight")==FALSE){
    weight <- values*0.0+1.0
}

dat <- calculate_bootstrap(weight[,1],values[,1], B)

histinfo <- hist(as.numeric(dat), plot = FALSE,breaks=nb)
tot_counts <- sum(histinfo$counts)
cum_sum <- cumsum(histinfo$counts)/tot_counts


yend <- 1.0*max(histinfo$counts/tot_counts)


p <- plot_ly(alpha = 0.6,type="bar")

p <- add_trace(p, x= histinfo$mids, y = histinfo$counts/tot_counts, type="bar", name="Probability")
p <- add_trace(p, x= histinfo$mids, y = cum_sum, visible = "legendonly", color=I("green"), type="bar", name="Cumulative")


#Add Confidence Interval if ci setting < 100 and > 0
if(ci < 100 && ci > 0){
    #Calculate left CI
    leftx = 0.0
    yy=0.01*(100-ci)/2
    for (i in 1:length(cum_sum)){

       
        if ( cum_sum[i] > yy){

            leftx = (yy-cum_sum[i-1])*(histinfo$mids[i]-histinfo$mids[i-1])/(cum_sum[i]-cum_sum[i-1]) + histinfo$mids[i-1]
            #leftx = 0.5*(histinfo$mids[i]+histinfo$mids[i-1])
            break
        }
    }

    #Calculate right CI
    rightx = 0.0
    yy=1 - 0.01*(100-ci)/2
    for (i in 1:length(cum_sum)){
        if ( cum_sum[i] > yy){
            rightx = (yy-cum_sum[i-1])*(histinfo$mids[i]-histinfo$mids[i-1])/(cum_sum[i]-cum_sum[i-1]) + histinfo$mids[i-1]
            break
        }
    }


    p <- add_segments(p, x = c(leftx,rightx), xend = c(leftx,rightx), y=c(0,0),yend=c(yend,yend), visible = "legendonly", color=I("red"), name="Conf. Int.")
}

meanval = mean(dat)
p <- add_segments(p, x = meanval, xend = meanval, y = 0, yend = yend, visible = "legendonly", color=I("black"), name="Mean")
p <- layout(p, title="Bootstrap Estimate",barmode = "overlay")

internalSaveWidget(p, 'out.html');
####################################################

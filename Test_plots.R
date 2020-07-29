library(ggplot2)
library(plotly)
# Histogramme avec la courbe de distribution et ligne verticale

df <- data.frame(
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5))))

boot_plot <- ggplot(df, aes(x=weight)) + geom_density(alpha=.2, fill="red")+ 
             geom_segment(aes(x=55,y=0,xend=55,yend=max(weight)),data=df,color="blue", linetype="dashed", size=1)+labs(title="Weight histogram plot",x="Weight(kg)", y = "Count")

boot_plot

### We have a pb with the geom_segmant function (replaces the geom_vline function with bounds)

dpb <- ggplot_build(boot_plot)

x1 <- min(which(dpb$data[[1]]$x >=55))
#x2 <- max(which(dpb$data[[1]]$x <=1000000))

print(dpb$data[[1]])

final_plot <- boot_plot +
  geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:length(dpb$data[[1]]$x)],
                            y=dpb$data[[1]]$y[x1:length(dpb$data[[1]]$y)]),
            aes(x=x, y=y), fill="green",alpha=0.3) + geom_histogram(aes(y=..density..), colour="black", fill="white",alpha=0.2)

ggplotly(final_plot)


### Interactive graph ###

#,group=1,text=paste("Weight: ",weight))

p <- ggplot(data = df, aes(x = Date, y = Revenue, group = 1,
                           text = paste("Date: ", Date,
                                        "<br>Revenue: $", Revenue,
                                        "<br>Target: $", Target)
)) +
  geom_line(colour = "grey", aes(Date, Target)) +
  geom_line(colour = "#408FA6")
ggplotly(p, tooltip = "text")
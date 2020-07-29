library(dplyr)
library(plotly)

#pal <- c("red", "blue", "green")
#pal <- setNames(pal, c("-1", "0", "1"))

#fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, colors = pal)

alpha <- 0.05
  
x0 <- rnorm(1000)
x <- sort(x0, decreasing = FALSE)
xmin <- quantile(x,0.05)
xmax <- quantile(x,0.95)

y <- rep(0,length(x))
for (i in 1:length(x)){
  if (x[i]<=xmin){
    y[i]<--1
  }else if(x[i]>=xmax){
    y[i]<- 1
  }
}

df <- data.frame(norm=x,pos=y)

fit <- density(df$norm)
df0 <- data.frame(fitx=fit$x,fity=fit$y)


df1 <- df0 %>% filter (fitx<=xmin)
df2 <- (df0 %>% filter (fitx>=xmin)) %>% filter (fitx<=xmax)
df3 <- df0 %>% filter (fitx>=xmax)


plot_ly(x=df$norm,type = "histogram", name = "Histogram : Difference \n of means in resamples")%>% 
  add_trace(x = df1$fitx, y = df1$fity, type = "scatter", mode = "lines", fill = "tozeroy", fillcolor='rgba(255,0,0,0.5)',yaxis = "y2", name = paste0("Values in the \n",100*alpha,"th percentile")) %>% 
  add_trace(x = df2$fitx, y = df2$fity, type = "scatter", mode = "lines", fill = "tozeroy", fillcolor='rgba(180,180,180,0.5)',yaxis = "y2",name="Middle values") %>% 
  add_trace(x = df3$fitx, y = df3$fity, type = "scatter", mode = "lines", fill = "tozeroy", fillcolor='rgba(0,255,0,0.5)',yaxis = "y2", name = paste0("Values beyond the \n",100*(1-alpha),"th percentile")) %>% 
  add_trace(x = list(xmin,xmin+0.000000001), y = list(0,max(df1$fity)), type = "scatter", mode = "lines", line = list(width=5, color = "red"),fill = "tozeroy", fillcolor='rgba(0,0,255,0.5)',yaxis = "y2", name = "Alpha") %>% 
  add_trace(x = list(xmax,xmax+0.000000001), y = list(0,max(df3$fity)), type = "scatter", mode = "lines", line = list(width=5, color = "black"),fill = "tozeroy", fillcolor='rgba(0,0,255,0.5)',yaxis = "y2", name = "1-Alpha") %>% 
  add_trace(x = list(0.5,0.5+0.000000001), y = list(0,max(df2$fity)), type = "scatter", mode = "lines", line = list(width=5, color = "green"),fill = "tozeroy",yaxis = "y2", name = "Observed inital value") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))


  add_lines(x = xmin, y=seq(0,100), name = "alpha", line = list(width=10, color = "black"), hoverinfo='x', showlegend = FALSE) %>%
  #add_segments(x = xmin, xend = xmin, y = 0, yend = 10)%>%
  add_annotations(x = xmin, y =10, xref = "x", yref = "y", text = "Alpha",
                  xanchor = "right", showarrow = F, textangle=0, font = list(color="black")) %>%
  #add_lines(x = xmax, y=seq(0,100), name = "1-alpha", line = list(width=2, color = "green"), hoverinfo='x', showlegend = FALSE) %>%
  add_annotations(x = xmax, y =100, xref = "x", yref = "y", text = "1-Alpha",
                  xanchor = "left", showarrow = F, textangle=0, font = list(color="black"))






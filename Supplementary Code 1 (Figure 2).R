library(plotly)
x=seq(-1,1,length.out=100) #Creat a grid of inputs
dm=expand.grid(x,x) # design matrix of inputs (1000 observation, 2 variables)
y= dm[,1]^2 + dm[,2]^2 # Non-linear relationship between inputs (X) and outputs (Y)


df <- as.data.frame(cbind(dm,y)) #Design matrix in input space
feat <- as.data.frame(cbind(dm[,1]^2,dm[,2]^2, y)) #Apply mapping to in design matrix elements

f1 <- list(size = 16, color = "black")
a <- list(showticklabels = TRUE, tickfont = f1)

#Contour Plot in Input Space
plot_ly(
  x = c(df[,1]), 
  y = c(df[,2]), 
  z = c(df$y),  
  type = "contour",  contours = list(showlabels = TRUE, start = 0, end = 2, size = 0.2))%>%
  layout(xaxis = a, yaxis = a)

#Contour Plot in Feature Space
plot_ly(
  x = c(feat[,1]), 
  y = c(feat[,2]), 
  z = c(feat$y),  
  type = "contour",  contours = list(showlabels = TRUE, coloring = 'heatmap',  start = 0, end = 2, size = 0.2))%>%
  layout(xaxis = a, yaxis = a)

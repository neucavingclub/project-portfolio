#creating a pdf a file where the plot will be saved
pdf("visualization.pdf")

#loading the data into our dataframe
d <- read.csv("Fitness_Data.csv")  

#creating the box plot function
create_boxplot <- function(){

#dependent variable - interval/measurement data
y <- d$Steps    

#independent variable - categorical data/nominal
x <- d$Diet.Type  

#plotting the graph and setting naming the x and y axis
boxplot(y~x,
        main="Plot for steps over 130 days depending on diet type",            #main chart title
        
        ylab ="Steps",                                                                                         #y-axis label which is steps
        xlab = "Diet Type",                                                                                    #x-axis label which is diet type
        las = 1,                                                                                               #rotates values on the y axis 
        col = c("#fcba03","green","cyan"),                                                                     #setting three color codes for the three categories
        boxlty = 0,                                                                                            #removing the border line for the box plots
        whisklty = 3,                                                                                          #size of the whiskers
        staplelwd = 1.5,                                                                                       #dotted line width
        pch = 19,                                                                                              #shape of the outliers
		    cex.axis = 0.75,                                                                                       #setting the font size of the axis 
		    cex.lab = 1.2                                                                                          #setting the font size of the labels
        )
		
#adding a legend to the plot		
legend (x="topright",                                                                                          #setting the position of our legend
        legend = c("Cutting", "Maintain", "Reverse Dieting"),                                                  #creating the three legends
        bty = "n",                                                                                             #remove borderlines 
        fill = c("#fcba03","green","cyan"),                                                                    #adding color to the three categories in the legend
        cex = 0.6,                                                                                             #setting the size of our legend
        title = "Diet Type"                                                                                    #title of the legend
		)	
		}
		
#creating a histogram function
create_histogram <- function(){

#selecting or dependent variable
y <- d$Steps				       

#plotting the histogram

h <- hist(y,                                                                                                   #selecting the interval to plot
          10,
          main = "Frequency of steps for different diet types over 130 days",             #main chart title
          xlab = "Steps",                                                                                      #x-axis label
          ylab = "Frequency",                                                                                  #y-axis label
          col = "beige",                                                                                       #setting the color of the bars to be beige
          las = 1,                                                                                             #rotates the y-axis
          cex.axis = 0.75                                                                                      #initialising the font size to 75%
         )

x <- seq(min(y), max(y),length(y)) 

yn <- dnorm(x, mean=mean(y), sd=sd(y))

box.size <- diff(h$mids[1:2]) * length(y)

yn <- yn * box.size

#plotting the overlaying curve with colour blue and line width set to 200%

lines(x, yn, col="blue", lwd=2)                   

}		

#function calling of the boxplot
create_boxplot() 
 
#function calling of the histogram with overlaying curve 
create_histogram()   
                                              
#saving the plot
dev.off()





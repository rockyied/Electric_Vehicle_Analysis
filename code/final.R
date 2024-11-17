rm(list=ls())
#laoding dataset
data <- read.csv("C:/Users/Rakes/OneDrive/Desktop/DI/Electric_Vehicle_Population_Data.csv")
#cleaning 
data$CAFV_Eligibility_Short <- ifelse(grepl("Eligible", data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility), "Eligible", "Not Eligible/Unknown")
data <- data[data$Model.Year <= 2023, ]
data$access_days_time <- NULL
data$Base.MSRP <- NULL
data$Legislative.District <- NULL
data$VIN..1.10. <- NULL
data$access_code <- NULL
data <- data[data$State == "WA", ]
data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility <- NULL
data$Vehicle.Location <- NULL
data$X2020.Census.Tract <- NULL







#Impute Values from web
#A unique list of make-model combinations with zero electric range
zero_range_vehicles <- unique(data[data$Electric.Range == 0, c('Make', 'Model')])

electric_range_data <- data.frame(
  Make = c(
    "TESLA", "TESLA", "VOLKSWAGEN", "CHEVROLET", "TESLA", "RIVIAN", "FORD", "RIVIAN", "AUDI", "FORD",
    "KIA", "NISSAN", "KIA", "SUBARU", "BMW", "CHEVROLET", "HYUNDAI", "BMW", "FORD",
    "HYUNDAI", "MERCEDES-BENZ", "POLESTAR", "TESLA", "PORSCHE", "CADILLAC", "MERCEDES-BENZ", "VOLVO", "AUDI",
    "MINI", "GENESIS", "BMW", "HYUNDAI", "NISSAN", "VOLVO", "MERCEDES-BENZ", "GENESIS", "AUDI", "AUDI",
    "LUCID", "LEXUS", "MERCEDES-BENZ", "TOYOTA", "HYUNDAI", "JAGUAR", "RIVIAN", "AUDI", "MERCEDES-BENZ", "AUDI",
    "KIA", "GENESIS"
  ),
  Model = c(
    "MODEL Y", "MODEL 3", "ID.4", "BOLT EV", "MODEL X", "R1S", "F-150", "R1T", "E-TRON", "MUSTANG MACH-E",
    "EV6", "LEAF", "NIRO", "SOLTERRA", "IX", "BOLT EUV", "IONIQ 5", "I4", "TRANSIT",
    "KONA ELECTRIC", "EQB-CLASS", "PS2", "MODEL S", "TAYCAN", "LYRIQ", "EQS-CLASS SEDAN", "XC40", "E-TRON GT", "HARDTOP",
    "G80", "I3", "IONIQ 6", "ARIYA", "C40", "EQS-CLASS SUV", "GV60", "Q4", "E-TRON SPORTBACK", "AIR", "RZ 450E", "EQE-CLASS SUV",
    "BZ4X", "IONIQ", "I-PACE", "EDV", "RS E-TRON GT", "EQE-CLASS SEDAN", "Q8", "SOUL EV", "GV70"
  ),
  Electric.Range = c(
    330, 333, 275, 259, 348, 260, 320, 400, 285, 312,
    310, 212, 253, 228, 324, 247, 303, 307, 108,
    258, 221, 307, 405, 206, 307, 347, 190, 249, 114,
    282, 153, 270, 265, 205, 285, 321, 302, 242, 384,
    196, 235, 318, 155, 246, 161, 232, 280, 300, 280, 236
  )
)

#Updating the Electric.Range values in the original data frame
for (i in 1:nrow(electric_range_data)) {
  make <- electric_range_data$Make[i]
  model <- electric_range_data$Model[i]
  range <- electric_range_data$Electric.Range[i]
  
  data$Electric.Range[data$Make == make & data$Model == model] <- range
}













mean(data$Electric.Range)
median(data$Electric.Range)
quantile(data$Electric.Range,0.50)
max(data$Electric.Range) - min(data$Electric.Range)
quantile(data$Electric.Range, 0.75) - quantile(data$Electric.Range, 0.25)
mean(abs(data$Electric.Range-mean(data$Electric.Range)))
var(data$Electric.Range)
sd(data$Electric.Range)
#consumer behaviour and preference
#barplot
library(ggplot2)
ggplot(data, aes(x=Make, fill=Make)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Popularity of EV Makes", x="Make", y="Count") +
  scale_fill_viridis_d()

#technological Advancements Impact
#line plot
library(ggplot2)

# Calculate the average electric range for each model year
avg_range_by_year <- aggregate(Electric.Range ~ Model.Year, data = data, mean)

# Create the line plot with a trend line
ggplot(avg_range_by_year, aes(x=Model.Year, y=Electric.Range)) +
  geom_line(color="blue") +
  geom_smooth(method="lm", se=FALSE, color="red") + # Adding a linear regression trend line
  labs(title="Average Electric Range Over Model Years with Trend Line",
       x="Model Year",
       y="Average Electric Range (miles)") +
  theme_minimal()

#Strategic Alignment with Emissions Goals
#Colored Heatmap
library(ggplot2)
library(viridis)

# Calculate the average electric range for each county and model year
avg_range_by_county_year <- aggregate(Electric.Range ~ County + Model.Year, data = data, mean)

# Creating the heat map with fully vertical x-axis labels
ggplot(avg_range_by_county_year, aes(x = County, y = as.factor(Model.Year), fill = Electric.Range)) +
  geom_tile() +
  scale_fill_viridis(name = "Avg Electric Range (miles)", option = "C") +
  labs(title = "Average Electric Range by County and Model Year",
       x = "County", y = "Model Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))  # Fully vertical x-axis labels













#3D Scatter Plot of Electric Range, Model Year, and Electric Vehicle Type
library(plotly)

# Creating the 3D scatter plot
plot <- plot_ly(data, x = ~Model.Year, y = ~Electric.Range, z = ~Make, type = "scatter3d", mode = "markers",
                marker = list(size = 10, opacity = 0.8, color = ~Electric.Range, colorscale = 'Viridis'),
                text = ~Make, # Adding Make as hover text
                hoverinfo = 'text+x+y+z') %>%
  layout(title = "3D Scatter Plot: Model Year, Electric Range, and Make of EV",
         scene = list(xaxis = list(title = "Model Year"),
                      yaxis = list(title = "Electric Range"),
                      zaxis = list(title = "Make")))

# Show the plot
plot

summary(data)

#probability distribution plot Electric.Range distribution
library(ggplot2)

ggplot(data, aes(x=Electric.Range)) +
  geom_histogram(binwidth=10, fill="blue") +
  labs(title="Distribution of Electric Range", x="Electric Range (miles)", y="Frequency")




#applied
library(dplyr)



# Probability distribution of electric vehicle types across counties
prob_dist <- data %>%
  group_by(County, Electric.Vehicle.Type) %>%
  summarise(Count = n()) %>%
  mutate(Probability = Count / sum(Count))

print(prob_dist)

# Calculating the likelihood based on make
likelihood_make <- data %>%
  group_by(Make) %>%
  summarise(Eligible_Count = sum(CAFV_Eligibility_Short == 'Eligible'),
            Total_Count = n(),
            Likelihood = Eligible_Count / Total_Count)

# Calculating the likelihood based on electric range
likelihood_range <- data %>%
  group_by(Electric.Range) %>%
  summarise(Eligible_Count = sum(CAFV_Eligibility_Short == 'Eligible'),
            Total_Count = n(),
            Likelihood = Eligible_Count / Total_Count)

# Display the likelihoods
print(likelihood_make)
print(likelihood_range)

#statistical analysis
# Calculate the average electric range
average_range <- mean(data$Electric.Range, na.rm = TRUE)

# Display the average range
print(average_range)
# Calculate the average electric range per city
average_range_city <- data %>%
  group_by(City) %>%
  summarise(Average_Range = mean(Electric.Range, na.rm = TRUE))

# Display the averages per state
print(average_range_city)

#hypothesis testing
# Perform a t-test
t.test(Electric.Range ~ Electric.Vehicle.Type, data = data)
# Perform a t-test
t.test(Electric.Range ~ CAFV_Eligibility_Short, data = data)

#regression
# Linear regression Electric Range Influenced by Model Year
model_year_regression <- lm(Electric.Range ~ Model.Year, data = data)

# Summary of the regression model
summary(model_year_regression)
#plot
library(ggplot2)

# Scatter plot with regression line
ggplot(data, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Electric Range vs Model Year",
       x = "Model Year",
       y = "Electric Range")






#Classification Vehicles Based on CAFV Eligibility
library(caret)
library(dplyr)

# Convert CAFV_Eligibility_Short to a binary response variable
data$CAFV_binary <- as.numeric(data$CAFV_Eligibility_Short == "Eligible")

# Selecting relevant variables for classification
# (You can modify this based on the variables you think are relevant)
classification_data <- data %>%
  select(CAFV_binary, Electric.Range, Model.Year, Make, County, State)




# Splitting the data into training and test sets
set.seed(123) # for reproducibility
splitIndex <- createDataPartition(classification_data$CAFV_binary, p = .70, list = FALSE, times = 1)
trainData <- classification_data[ splitIndex,]
testData <- classification_data[-splitIndex,]
model <- glm(CAFV_binary ~ Electric.Range + Model.Year + Make + County, data = trainData, family = "binomial")
# Making predictions on the test set
predictions <- predict(model, testData, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
confusionMatrix(as.factor(predictions), as.factor(testData$CAFV_binary))









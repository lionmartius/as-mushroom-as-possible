# What do we need for successful woodland expansion?
# - As Mushroom as possible
# Ectomycorrhizal grove formation and implications for woodland expansion 
# SOIL DATA
# Lion R. Martius
# Edinburgh, 23/09/2024

soil <- read.csv('data/soil_data.csv')
library(ggplot2)


soil$Site_Type[soil$Site_Type == "NoTarget"] <- "no_target"
soil$Site_Type[soil$Site_Type == "Target"] <- "target"
soil$pH <- as.numeric(soil$pH)
soil$PO4 <- as.numeric(soil$PO4)
soil$C.N <- as.numeric(soil$C.N)

# soil PO4 needs to be transformed from mg/Linto mg/kg ((P/20)/4)1000 

soil$PO4 <- ((soil$PO4/20)/4)*1000

# Plot soil data by Site_Type as violin plots
# Variables: OM_depth, pH, soil_moisture, PO4, C.N

windows();ggplot(soil, aes(x = Site_Type, y = OM_depth)) + geom_violin() + geom_boxplot(width = 0.1) + 
  labs(title = "Soil data by Site_Type", x = "Site_Type", y = "OM_depth") + theme_minimal()

windows();ggplot(soil, aes(x = Site_Type, y = pH)) + geom_violin() + geom_boxplot(width = 0.1)

windows();ggplot(soil, aes(x = Site_Type, y = soil_moisture_perc)) + geom_violin() + geom_boxplot(width = 0.1)

windows();ggplot(soil, aes(x = Site_Type, y = PO4)) + geom_violin() + geom_boxplot(width = 0.1)

windows();ggplot(soil, aes(x = Site_Type, y = C.N)) + geom_violin() + geom_boxplot(width = 0.1)

# Use two-sample T test to test whether there is a significant difference in the means of soil parameters between target and 
# no_target sites for the variables: OM_depth, pH, soil_moisture_perc, PO4, C.N

t.test(OM_depth ~ Site_Type, data = soil)
t.test(PO4 ~ Site_Type, data = soil)

df <- data.frame(
    Variable = character(),
    t = numeric(),
    `mean no target` = numeric(),
    `mean target` = numeric(),
    dof = numeric(),
    pvalue = numeric(),
    stringsAsFactors = FALSE
)
i <- 0

col_to_be_tested <- c("OM_depth", "pH", "soil_moisture_perc", "PO4", "C.N")
for (col in col_to_be_tested) {
    i <- i + 1

    test <- t.test(soil[[col]] ~ soil$Site_Type)

    df[i, "Variable"] <- col
    df[i, "t"] <- test$statistic
    df[i, "mean no target"] <- test$estimate[1]
    df[i, "mean target"] <- test$estimate[2]
    df[i, "dof"] <- test$parameter
    df[i, "pvalue"] <- test$p.value

    print(shapiro.test(soil[[col]]))

}

#
summary(soil$pH)
sd(soil$pH, na.rm = T)

summary(soil$soil_moisture_perc)
sd(soil$soil_moisture_perc, na.rm = T)

summary(soil$C.N)
sd(soil$C.N, na.rm = T)

summary(soil[soil$Site_Type == 'target',]$PO4, na.rm = T)
sd(soil[soil$Site_Type == 'target',]$PO4, na.rm = T)
summary(soil[soil$Site_Type == 'no_target',]$PO4, na.rm = T)
sd(soil[soil$Site_Type == 'no_target',]$PO4, na.rm = T)

summary(soil$OM_depth)
sd(soil$OM_depth, na.rm = T)

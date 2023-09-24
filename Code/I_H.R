summary(IndianHouses)
model <- lm(BHK~Area,IndianHouses)
summary(model)
attach(IndianHouses)
g_model <- glm(BHK~Area+Price,family = "gaussian")
summary(g_model)

model_p <- lm(BHK~Price)
summary(model_p)

anova(model,g_model,method= "Chi")
AIC(g_model)
p_model <- glm(BHK~Area+Locality+Price,family = "poisson",IndianHouses)
summary(p_model)
AIC(p_model)
#model with family gaussian is a better model as the aic is lower in comparison to the model fitted with the poisson

model_2 <- lm(Bathroom~Area,IndianHouses)
summary(model_2)
 
gmode <- glm(Bathroom~Area+Furnishing,family = "gaussian",IndianHouses)
summary(gmode)

model_3 <- lm(Area~Locality,IndianHouses)
summary(model_3)

model_4 <- lm(Area~Bathroom,IndianHouses)
summary(model_4)
#The adjusted R square for model_3 is 0.2296 which stats that 22.96% of the error is explained
#The adjusted R square for model_4 is 0.2858 which stats that 28.58% of the error is explained 
#In real life both the model are bad as their R_square is very low
#But if we have to choose any one the linear model we'll go for the model_4 since the model explain more error than model_3


model_5 <- lm(Price~Type,IndianHouses)
summary(model_5)

model_6 <- lm(Price~Area,IndianHouses)
summary(model_6)

#

gl <- glm(Price~Type+Area+Bathroom+BHK+Parking+Status,family = "gaussian",IndianHouses)
summary(gl)

gl$aic #(44226.14)

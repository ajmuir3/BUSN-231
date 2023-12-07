# Question 1 - Multiple Linear Regression

#1.1 Develop a model that allows to predict the annual salary of a given BigCom employee using all predictors given in the data. Copy and paste your R code here. . Hint: Use lm() and summary().
df_1 = read.csv("q1_salary_data.csv")

multiple_linear_regression_model = lm(salary ~ experience + years_employed + education + gender + department + supervised, data = df_1)
summary(multiple_linear_regression_model)

#1.2 Using a model
experience = 10
years_employed = 5
education = 4
gender = 0
department = 2
supervised = 12
salary = 19589.47 + 621.06 * experience + -106.55 * years_employed + 1631.83 * education + -1654.07 * gender + 2134.29 * department + supervised *  134.01
salary

#1.5
residuals_df1 = resid(multiple_linear_regression_model)
plot(x= df_1$salary, y=residuals_df1)
abline(h=0, col="red")

#1.6
shapiro.test(residuals_df1)

#1.7
ggpairs(df_1, columns = c('experience','years_employed','education','gender','department','supervised'))

# Question 2 - ComparingTwo Populations – Matched Pairs (Wilcoxon Signed Rank Sum Test)
str(q2_spam)
df_2 <- data.frame(q2_spam)

shapiro.test(df_2$before_filter - df_2$after_filter)

wilcox.test(df_2$before_filter,df_2$after_filter,mu=0, alternative = "less", paired=TRUE)

# Question 3 - Morethan Two Outcomes with Proportions (Chi-square Goodness-of-fit Test) HW#10
str(internet_searches)
df_3 <- table(internet_searches)

chisq.test(df_3, p = c(0.42,0.21,0.19,0.18))

# Question 4 - Comparing Two Populations – Independent Samples (t-Test) HW#9
str(q4_baby_food)
df_4 <- data.frame(q4_baby_food)

shapiro.test(df_4$similac-df_4$infamil)

mean_infamil = mean(df_4$infamil)
mean_similac = mean(df_4$similac)
mean_infamil
mean_similac

t.test(df_4$similac, df_4$infamil, mu =0, alternative = "greater")

# Question 5 - ComparingTwo Populations – Matched Pairs (t-Test)
str(q5_tires)
df_5 <- data.frame(q5_tires)

shapiro.test(df_5$RadialTires - df_5$BeltedTires)

mean_radial = mean(df_5$RadialTires)
mean_belted = mean(df_5$BeltedTires)
mean_radial
mean_belted

t.test(df_5$RadialTires, df_5$BeltedTires, mu =0, alternative = "two.sided", paired = TRUE)

# Question 6 - ComparingTwo Populations – Matched Pairs (Sign Test)
str(q6_exercise)
df_6 <- data.frame(q6_exercise)

install.packages("BSDA")
library(BSDA)

SIGN.test(q6_exercise$before, q6_exercise$after_3_days, md=0, alternative = "two.sided")

# Question 7 - ComparingThree or More Populations (Kruskal-Wallis)
str(q7_concrete_strength)
df_7 <- data.frame(q7_concrete_strength)

shapiro.test(df_7$two_days)
shapiro.test(df_7$seven_days)
shapiro.test(df_7$twenty_eight_days)

stacked_df_7 = stack(df_7)

kruskal.test(values ~ ind, data = stacked_df_7)

# Question 8 - ComparingTwo Populations – Independent Samples (Wilcoxon Rank Sum Test)
str(q8_sleep)
df_8 <- data.frame(q8_sleep)

shapiro.test(df_8$men)
shapiro.test(df_8$women)

wilcox.test(df_8$men,df_8$women, mu=0, alternative = "two.sided", paired = FALSE)

# Bonus - ComparingThree or More Populations (ANOVA)
str(headlights)
df_3 <- table(headlights)
df_3

p1 = shapiro.test(headlights$V1)
p2 = shapiro.test(headlights$V2)
p3 = shapiro.test(headlights$V3)
p4 = shapiro.test(headlights$V4)

p1
p2 
p3
p4

stacked_headlights = stack(headlights)
anova_headlights = aov(values ~ ind, data = stacked_headlights)
summary(anova_headlights)

tukey_headlights <- TukeyHSD(anova_headlights)
par(mar=c(1,1,1,1))
plot(tukey_headlights)


# Bonus - Chi Squared Independence Test

str(office_communications)
df_10 <- table(office_communications)
df_10

cross_table_10 <- table(office_communications$Age.Group,office_communications$Preferred.Communication)
cross_table_10

chisq.test(cross_table_10, correct = FALSE)

# Definitions




shapiro.test(q8_sleep$women)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarized
# groupnames : vector of column names to be used as
# grouping variables

#install.packages("readxl")
#install.packages("gridExtra")

library(readxl)
library(gridExtra)


df <- read_excel("./251013.xlsx", sheet = "Sheet1")

Time <- seq(from = 0, by = 1, length.out = 217)

#T7_A
T7_A <- df[, c("A1","A2","A3")]
df_T7_A <- data.frame(Time, T7_A)
df_T7_A_all <- data.frame(Time, Mean=apply(T7_A, 1, mean), SD=apply(T7_A, 1, sd))

p_T7_A <- ggplot(df_T7_A_all, aes(x=Time, y=Mean, ymin=0, ymax=1.3)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), color='#cacfd2', width=.2, position=position_dodge(0.05)) + 
  geom_line() +
  geom_point(size = 0.01) +
  theme_classic(base_size = 11) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

#POP_A
POP_A <- df[, c("A4","A5","A6")]
df_POP_A <- data.frame(Time, POP_A)
df_POP_A_all <- data.frame(Time, Mean=apply(POP_A, 1, mean), SD=apply(POP_A, 1, sd))

p_POP_A <- ggplot(df_POP_A_all, aes(x=Time, y=Mean, ymin=0, ymax=1.3)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), color='#cacfd2', width=.2, position=position_dodge(0.05)) + 
  geom_line() +
  geom_point(size = 0.01) +
  theme_classic(base_size = 11) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

#Empty_A
Empty_A <- df[, c("A7","A8","A9")]
df_Empty_A <- data.frame(Time, Empty_A)
df_Empty_A_all <- data.frame(Time, Mean=apply(Empty_A, 1, mean), SD=apply(Empty_A, 1, sd))

p_Empty_A <- ggplot(df_Empty_A_all, aes(x=Time, y=Mean, ymin=0, ymax=1.3)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), color='#cacfd2', width=.2, position=position_dodge(0.05)) + 
  geom_line() +
  geom_point(size = 0.01) +
  theme_classic(base_size = 11) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

#H20_A
H20_A <- df[, c("A10","A11","A12")]
df_H20_A <- data.frame(Time, H20_A)
df_H20_A_all <- data.frame(Time, Mean=apply(H20_A, 1, mean), SD=apply(H20_A, 1, sd))

p_H20_A <- ggplot(df_H20_A_all, aes(x=Time, y=Mean, ymin=0, ymax=1.3)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), color='#cacfd2', width=.2, position=position_dodge(0.05)) + 
  geom_line() +
  geom_point(size = 0.01) +
  theme_classic(base_size = 11) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


png(filename = "Fig5_251013.png", width = 300, height = 200, units = "px", res = 100)

grid.arrange(p_T7_A, p_POP_A, p_Empty_A, p_H20_A, ncol = 2)

dev.off()

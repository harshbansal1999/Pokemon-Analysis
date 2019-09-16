#Importing the library
library(tidyverse)

# Import the dataset and convert variables
pokedex <- read_csv("pokedex.csv",col_types = cols(name = col_factor(),
                                     type = col_factor(),
                                     is_legendary = col_factor()))
#Examine the data
head(pokedex,6)
str(pokedex)

# Prepare the data
legendary=pokedex[pokedex$is_legendary==1,]
legendary$percentage_male[is.na(legendary$percentage_male)]=0
print(head(legendary))

#Plotting the pokemon on basis of height and weight
legend_by_heightweight_plot <- pokedex %>%
  ggplot(aes(x=height_m,y=weight_kg)) +
  geom_point(aes(x=height_m,y=weight_kg,color=is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m>7.5 | weight_kg>600,
                               as.character(name), '')),
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x=16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokémon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

legend_by_heightweight_plot

#Preparing the plot on basis of type of pokemon
legend_by_type <- pokedex %>%
  group_by(type) %>%
  mutate(is_legendary = as.numeric(is_legendary) - 1) %>%
  summarise(prop_legendary = mean(is_legendary)) %>%
  ungroup() %>%
  mutate(type = fct_reorder(type,prop_legendary))

legend_by_type_plot <- legend_by_type %>%
  ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) +
  geom_col() +
  labs(title = "Legendary Pokemon by type") +
  coord_flip() +
  guides(fill = FALSE)

legend_by_type_plot


#Preparing the plot on basis of stats of each
legend_by_stats <- pokedex  %>%
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>%
  gather(key = fght_stats, value = value, -is_legendary)

legend_by_stats_plot <- legend_by_stats %>%
  ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~fght_stats) +
  labs(title = "Pokemon fight statistics",
       x = "Legendary status") +
  guides(fill = F)

legend_by_stats_plot

set.seed(1234)

#Splitting of Data
n=length(pokedex)*0.6
sample_rows=sample(n,replace=T)

pokedex_train <- pokedex  %>% filter(row_number() %in% sample_rows)

pokedex_test <- pokedex  %>% filter(!row_number() %in% sample_rows)

#Preparing the Model
#install.packages('rpart')
#install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

# Fit decision tree
model_tree <- rpart(is_legendary ~ attack + defense + height_m +
                      hp + sp_attack + sp_defense + speed + type + weight_kg,
                    data = pokedex,
                    method = "class",
                    na.action = na.omit)

# Plot decision tree
rpart.plot(model_tree)

library(randomForest)

# Fit Random Forest
model_forest <- randomForest(is_legendary ~ attack + defense + height_m +
                               hp + sp_attack + sp_defense + speed + type + weight_kg,
                             data = pokedex,
                             importance = TRUE,
                             na.action = na.omit)
model_forest


#install.packages('ROCR')
library(ROCR)

# Prediction and performance objects for the decision tree
probs_tree <- predict(model_tree, pokedex_test, type = "prob")
pred_tree <- prediction(probs_tree[,2], pokedex_test$is_legendary)
perf_tree <- performance(pred_tree, "tpr", "fpr")


# Prediction and performance objects for the random Forest
probs_forest <- predict(model_forest, pokedex_test, type = "prob")
pred_forest <- prediction(probs_forest[,2], pokedex_test$is_legendary)
perf_forest <- performance(pred_forest, "tpr", "fpr")


#Plottig the ROC curve to show the difference
plot(perf_tree, col = "red", main = "ROC curves")
plot(perf_forest, add = TRUE, col = "blue")
legend(x = "bottomright",
       legend = c("Decision Tree", "Random Forest"),
       fill = c("red", "blue"))

importance_forest <- importance(model_forest)
importance_forest

varImpPlot_forest <- varImpPlot(model_forest)
varImpPlot_forest

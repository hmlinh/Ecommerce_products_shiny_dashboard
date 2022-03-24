# Import the library and load the dataset
library(tidyverse)      
shoes <- read_csv("shoes_original.csv")
shoes <- as_tibble(shoes)


# Format the dataset
shoes <- shoes %>%
  #rename columns
  mutate(
    remove = category,            
    category = subcategory,
    short_desc = name,
    original_price = raw_price,
    likes = likes_count,
    newly_added = is_new
  )  %>% 
  
  #add a new column based on the condition of other two columns
  mutate(
    color_selectable = if_else(     
      (!is.na(.$variation_0_color) 
       & !is.na(.$variation_1_color)), 
      TRUE, 
      FALSE
    )
  ) %>% 
  
  #rename categories
  mutate(
    category = recode(
      category,  
      "Mocassins" = "Derbies & Mocassins",
      "Sandales" = "Sandales & Mules",
      "Sandals" = "Sandales & Mules",
      "Sneakers & Baskets" = "Baskets",
      "Bottes & Chaussures montantes" = "Bottes & Bottines",
      "Bottes" = "Bottes & Bottines",
      "Claquettes & Tongs" = "Chaussons",
      "Slipper" = "Chaussons",
      "Plate-formes" = "Plateforme",
      "Pumps" = "Escarpins",
      "Flat & Loafers" = "Derbies & Mocassins",
      "ACCESSOIRES CHAUSSURES" = "Accessoires chaussures"
    )
  ) %>% 
  
  filter(
    !(category %in% c("CHAUSSURES HOMME", "Chaussures de jeunesse"))
  ) %>% 
  
  #replace 0 in "original_price" with value in "current_price"
  mutate(
    original_price = if_else(      
      original_price == 0, 
      current_price, 
      original_price
    )
  ) %>% 
  
  mutate(
    category = as.factor(category),
    brand = as.factor(brand)
  ) %>%
  
  select(
    model, category, brand, short_desc, 
    newly_added, color_selectable, likes, 
    original_price, current_price, discount
  ) 


# Remove duplicate rows
shoes <- shoes[!duplicated(shoes$model),] 


# Save the dataset
save(shoes, file = "shoes.RData") 
#write.csv(shoes, file = "shoes_cleaned.csv")




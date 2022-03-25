library(tidyverse)
library(shiny)
library(shinydashboard)
library(reactable)
library(viridis)
library(plotly)
library(ggpointdensity)
library(corrr)
library(tidytext)
library(tm)
library(fresh) 


load("shoes.RData")

# Customize theme for the dashboard
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#3a435e"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#313e50",
    dark_hover_bg = "#3D4B5F",
    dark_color = "#f2f4f3"
  ),
  adminlte_global(
    content_bg = "#F2F5FC",
    box_bg = "#F2F5FC", 
    info_box_bg = "#FFF"
  )
)

ui <- dashboardPage(
  title = "Ecommerce Products",
  #HEADER
  dashboardHeader(
    title = tagList(tags$img(src= 'https://freesvg.org/img/1517505968.png', 
                             height = '30', 
                             width ='30'),
                    tags$span("Ecommerce", 
                              style = "font-weight: bold"),
                    tags$span("Products")
    ), 
    
    tags$li(class="dropdown",
            tags$a(href="https://www.linkedin.com/in/my-linh-ho-a483a01b6/", 
                   icon("linkedin"), 
                   target="_blank")
    ),
    
    tags$li(class="dropdown",
            tags$a(href="https://github.com/hmlinh", 
                   icon("github"), 
                   target="_blank")
    )
  ), #End of Header
  
  #SIDEBAR     
  dashboardSidebar(
    width = 300,
    #Sidebar Menu
    sidebarMenu(
      menuItem("Introduction", tabName = "intro",  icon = icon('info-circle')),
      menuItem("Explore", tabName = "explore", icon = icon("compass")),
      menuItem("Analysis", tabName = "analyse",  icon = icon('chart-line')),
      menuItem("Dataset", tabName = "data",  icon = icon('database'))),
    
    #Widgets
    sliderInput("price", h4("Price on Website"),              # Widget 1: price selection
                min = 0, max = 150, 
                value = c(0, 150), step = 1, sep = ""),
    sliderInput("like", h4("Number of Likes"),                # Widget 2: like selection
                min = 0, max = 21600, 
                value = c(0, 21600), step = 1, sep = ""),
    checkboxGroupInput("cat", h4("Category"),                 # Widget 3: category selection
                       choices = unique(shoes$category),
                       selected = c('Derbies & Mocassins', 'Baskets', 
                                    'Bottes & Bottines', 'Sandales & Mules', 
                                    'Chaussons', 'Escarpins', 'Chaussures de ville', 
                                    'Accessoires chaussures', 'Plateforme',
                                    'Chaussures de sport'))
  ), # End of Sidebar
  
  #BODY            
  dashboardBody(
    # Theme
    use_theme(mytheme),
    
    # UI Tabs
    tabItems(
      ##Introduction Tab
      tabItem(tabName ="intro", 
              h1("Welcome to my Shiny Dashboard!"),
              fluidRow(
                column(8, 
                       h4("This dashboard will help you have some insight about shoe product 
                         listing from an E-commerce retail website called", 
                         tags$a(href="https://www.newchic.com/fr/", "newchic.com/fr.", target="_blank"),  
                         "This retailer is an online shopping brand based 
                         in Hong Kong and offers a wide range of clothing, shoes, bags,
                         accessories, cosmetics at a low price. Most of their products 
                         have no brand and are made in China, hence they can provide 
                         customers with many low-cost options.", align = "justify"),
                       
                       h4("The dataset contains more than 10,000 products with 10 features, 
                         which are", em("model ID,	category,	brand, short description,	newly added,
                         color selectable, likes,	original price,	current price, and	
                         discount."), align = "justify"),
                       
                       h4(strong("What kind of insight this Dashboard provides?"), "It aims to
                         find what customers like and also if there is anything that determines
                         the preference of customers towards a specific category. Therefore, 
                         the retailer can know what type of products they 
                         should offer to customers so they can optimize their inventory by 
                         investing properly in products that customers want.", align = "justify"),
                       
                       h4(strong("So how can you get insight from this Dashboard?"), "The left sidebar 
                         will lead you to explore and analyse the dataset as well as customize 
                         your exploration thanks to the sliders and checkbox widgets. 
                         More specifically, by clicking on each tab of the sidebar, you will see 
                         subsections that include graphs, tables and explanations. In", em("Explore"), 
                         "section, you can see the distribution of each variable as well as the category 
                         comparison. The", em("Analysis"), "section offers more understanding about 
                         prices of products, correlation between features and text mining.", align = "justify"),
                       h3("Have fun exploring shoes!!", icon("shoe-prints"))
                ),
                column(4, br(),
                       valueBoxOutput("box1", width = 12),
                       infoBoxOutput("box2", width = 12),
                       valueBoxOutput("box3", width = 12)
                )
              ),
      ),  #End of Introduction Tab
      
      #Explore Tab
      tabItem(tabName ="explore", 
              tabBox(
                height = "920px", width = "400px",
                tabPanel("Features Distribution",
                         h3("Here are the graphs that show the distribution 
                                              of the features in the dataset."),
                         
                         selectInput("graph", label = h4("Select feature"), 
                                     choices = list("Category" = "ca", 
                                                    "Brand" = "br",
                                                    "Newly added" = "ne",
                                                    "Color selectable" = "co",
                                                    "Likes" =  "li",
                                                    "Original price" = "or",
                                                    "Current Price" = "cu",
                                                    "Discount" = "di"), 
                                     selected = "li"),
                         
                         # Create panel for each feature to be visible or not based on the selection of the selectbox
                         # Category
                         conditionalPanel(
                           condition = "input.graph == 'ca'",   
                           fluidRow(column(12,
                                           plotOutput("catplot"),
                                           h4(em("Derbie & Mocassins"), "has the most products shown up on the website, 
                                              then", em("Baskets, Bottes & Bottines"), "and", em("Sandales & Mules"), 
                                              "relatively."))
                           )
                         ),
                         
                         # Brand
                         conditionalPanel(
                           condition = "input.graph == 'br'",  
                           fluidRow(
                             column(12,
                                    plotOutput("brand"),
                                    h4("From the distribution of", em('brand'), "feature, we can see one 
                                       interesting point is that most of the shoes on the website 
                                       have no brand and there are very few brands shown up here, 
                                       they are not wellknown also. That can explain why products
                                       appeared on this website are relatively cheap."))
                           )
                         ),
                         
                         # Newly added
                         conditionalPanel(
                           condition = "input.graph == 'ne'", 
                           plotOutput("new")
                         ),
                         
                         #Color selectable
                         conditionalPanel(
                           condition = "input.graph == 'co'", 
                           plotOutput("color")
                         ),
                         
                         #Likes
                         conditionalPanel(
                           condition = "input.graph == 'li'",                   
                           fluidRow(
                             column(8,
                                    plotOutput("like1", height = 300)),
                             
                             column(4, 
                                    br(),br(),br(),br(),
                                    h4("It's clear that", em('likes'), "feature has a fat tailed 
                                      distribution, which potentially causes bias in the process
                                      of data analysis. To get more insight about this feature, 
                                      let's see the boxplot below!", align = "justify"))
                           ), 
                           
                           h4("The boxplot shows that there are a lot 
                              of outliers in this dataset. This is not a good sign!
                              Therefore the median like will be used for the analysis of
                              this dataset instead of the mean.", align = "justify"),
                           
                           plotOutput("like2", height = 350)
                         ),
                         
                         # Current Price
                         conditionalPanel(
                           condition = "input.graph == 'cu'",
                           plotOutput("cur_price")
                         ),
                         
                         # Original Price
                         conditionalPanel(
                           condition = "input.graph == 'or'", 
                           plotOutput("org_price")
                         ),
                         
                         # Discount
                         conditionalPanel(
                           condition = "input.graph == 'di'", 
                           plotOutput("discount")
                         )
                ), #End of Features Distribution
                
                tabPanel("Category Comparison ",
                         plotOutput("catplot2", height = 600),
                         
                         h3("This visualisation shows the median likes, the average
                            current price (USD) and also the number of products per category."),
                         
                         h4("The median likes are illustrated by the height of bars, the average current price
                            is illustrated by the height of small black circles (dots) moving along
                            the dash line, and the number of product is showed thanks to colors.", 
                            br(), "Although", em("Derbie & Mocassins"), "stands in the first place for
                            the number of products, it is not the most preferred category 
                            on this website. The same with", em("Baskets"), "and", em("Sandales & Mules"), 
                            "whereas", em("Escarpins"), "and",  em('Accessoires chaussures'), "get the most 
                            likes despite of the fact that they don't have lots of products 
                            available on the website, even in the case of", em('Accessoires chaussures'), "which has 
                            the least products. More surprisingly, this category aslo has the cheapest average price.", 
                            aligh = "justify")
                ) # End of Category Comparison
              )
      ), # End of Explore Tab
      
      #Analysis Tab
      tabItem(tabName ="analyse", 
              tabBox(
                height = "920px", width = "400px",
                tabPanel("Price Analysis",
                         h3("The graph shows how much the price change after discount, generally 50% lower 
                            for all the categories."),
                         plotOutput("catplot3"),
                         
                         h4("Here we have the table that shows each category's best price range which is 
                           calculated from current price range having the highest median likes. From this table company 
                           can have better insight of how to adjust the original price as 
                           well as price after discount according to customers' preferences.",
                           br(),
                           "For instance, the price range for", em("Baskets"), "category that is mostly 
                           preferred by customers is 0-10 USD, although the average prices on 
                           website is more expensive, at 78 USD orignally and 36 USD after discount. 
                           This means that 0-10 USD might be a better price range for this category.",
                           br(),
                           "This column also suggests that company should consider to increase 
                           the price of", em("Accessoires Chaussures"), "since it has relatively high median number of 
                           likes but still much cheaper compare to other categories."),
                         
                         reactableOutput("price_tb")
                ), # End of Price Analysis
                
                tabPanel("Correlation",
                         fluidRow(
                           column(6, 
                                  plotOutput("corr1")
                           ),
                           column(6, 
                                  br(), br(), br(), 
                                  h4("There is no strong linear correlation between", em("likes"), 
                                     "and other features in this dataset, which means no 
                                    feature can explain the variation of the number of 
                                    likes for products in a linear way. This may be 
                                    explained by the huge number of outliers in the  
                                    dataset that mess up statistical assumptions. However 
                                    we can see that current price is strongly correlated 
                                    with original price since the current price is calculated 
                                    with discount rate. Besides, there is a relatively strong 
                                    negative correlation between", em("discount rate"), "and",
                                    em("current price."), "This means the higher the discount 
                                    rate is, the lower the current price is, which totally makes sense.", 
                                    align = "justify")
                           )
                         ),
                         
                         h4("Let's take a look at the correlation scatterplots of", em("Likes"), 
                            "and", em("Current Price"), "where", em("Likes"), "is log10 transformed. 
                            However, we still see no correlation,", em("Current Price"), " cannot be 
                            used to predict the number of", em("Likes.")),
                         
                         fluidRow(
                           column(6, 
                                  box(title = "Of all products",
                                      width = NULL,
                                      status = "primary", solidHeader = TRUE,
                                      plotOutput("corr2", height = 350))
                           ),
                           column(6, 
                                  box(title = "Of each category",
                                      width = NULL, 
                                      status = "primary", solidHeader = TRUE,
                                      plotOutput("corr3", height = 350))
                           )
                         )
                ), # End of Correlation
                
                tabPanel("Text Mining",
                         h3("We had enough of numbers and figures already, let's 
                           have a little bit more fun with words!"),
                         
                         selectInput("text_plots", label = h4("Select category"), 
                                     choices = list('Derbies & Mocassins' = "derbies", 
                                                    'Baskets' = "baskets",
                                                    'Bottes & Bottines' = "bottes",
                                                    'Sandales & Mules' = "sandales",
                                                    'Chaussons' =  "chaussons",
                                                    'Escarpins' = "escarpins",
                                                    'Chaussures de ville' = "ville",
                                                    'Accessoires chaussures' = "accessoire",
                                                    'Plateforme' = "plateforme",
                                                    'Chaussures de sport' = 'sport'), 
                                     selected = "bottes"),
                         
                         # Create panel for each category to be visible or not based on the selection of the selectbox
                         # Derbies & Mocassins
                         conditionalPanel(
                           condition = "input.text_plots == 'derbies'", 
                           plotOutput("derbies_plot", height = 460)
                         ),
                         
                         # Baskets
                         conditionalPanel(
                           condition = "input.text_plots == 'baskets'", 
                           plotOutput("baskets_plot", height = 460)
                         ),
                         
                         # Bottes & Bottines
                         conditionalPanel(
                           condition = "input.text_plots == 'bottes'", 
                           plotOutput("bottes_plot", height = 460)
                         ),
                         
                         # Sandales & Mules
                         conditionalPanel(
                           condition = "input.text_plots == 'sandales'", 
                           plotOutput("sandales_plot", height = 460)
                         ),
                         
                         # Chaussons
                         conditionalPanel(
                           condition = "input.text_plots == 'chaussons'",
                           plotOutput("chaussons_plot", height = 460)
                         ),
                         
                         # Escarpins
                         conditionalPanel(
                           condition = "input.text_plots == 'escarpins'", 
                           plotOutput("escarpins_plot", height = 460)
                         ),
                         
                         # Chaussures de ville
                         conditionalPanel(
                           condition = "input.text_plots == 'ville'", 
                           plotOutput("ville_plot", height = 460)
                         ),
                         
                         # Accessoires chaussures
                         conditionalPanel(
                           condition = "input.text_plots == 'accessoire'", 
                           plotOutput("accessoire_plot", height = 460)
                         ),
                         
                         # Plateforme
                         conditionalPanel(
                           condition = "input.text_plots == 'plateforme'", 
                           plotOutput("plateforme_plot", height = 460)
                         ),
                         
                         # Chaussures de sport
                         conditionalPanel(
                           condition = "input.text_plots == 'sport'", 
                           plotOutput("sport_plot", height = 460) 
                         ),
                         
                         h4("The lollipop plot above shows word frequency in", em("short description"), 
                            "feature for products that have more than 100 likes. Appeared 
                           on the y axis the words that are the most commonly used to 
                           describe shoe products of each category on the website.",
                           tags$a(href="https://www.tidytextmining.com/tfidf.html", "tf-idf", target="_blank"),
                           "on x axis which means", em("term frequency – inverse document frequency"), 
                           "is a measure used to evaluate how important a word is in a collection of shoe description
                           of a category. Thanks to these lists of words, we can see which term is used to describe the most 
                           liked products. This may be helpful for company to decide which products 
                           to focus on selling on their website and also for their marketing team 
                           to know how to write more attracting description for shoes.", icon("grin-wink"))
                ) #End of Text Mining
              )
      ), # End of Analysis Tab
      
      # Dataset Tab
      tabItem(tabName ="data", 
              reactableOutput("pt")   
      ) # End of Dataset tab
    ) # End of UI Tabs
  ) # End of Body
)

server <- function(input, output){
  # Creates the dynamic data   
  data <- reactive({                             
    shoes %>%                                    
      filter(current_price >= input$price[1], # Filter Current price
             current_price <= input$price[2],
             likes >= input$like[1],          # Filter Likes
             likes <= input$like[2],
             category %in% input$cat)         # Filter Category
  })
  
  # Render boxes in Introduction Tab
  output$box1 <- renderValueBox({
    shoes <- data()
    valueBox(
      value = round(mean(shoes$current_price),digits = 2), 
      subtitle =  "$ Price on average", 
      icon = icon("money-bill-alt", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$box2 <- renderInfoBox({
    shoes <- data()
    infoBox(
      value = data() %>% 
        filter(likes == max(likes)) %>% 
        select(category),
      title = "Most likes product",
      icon = icon("heart", lib = "font-awesome"),
      color = "navy",
    )
  })
  
  output$box3<- renderValueBox({
    shoes <- data()
    valueBox(
      value = round(mean(shoes$discount), digits = 2), 
      subtitle =  " % Discount on average", 
      icon = icon("tag", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  # Render table in Dataset Tab
  output$pt <- renderReactable({reactable(data(),
                                          columns = list(
                                            model = colDef(name = "Model ID"),
                                            category = colDef(name = "Category"),
                                            brand = colDef(name = "Brand", 
                                                           minWidth = 80),
                                            short_desc = colDef(name = "Short description", 
                                                                minWidth = 200),
                                            newly_added = colDef(name = "Newly added", 
                                                                 minWidth = 80, 
                                                                 align = "center",
                                                                 cell = function(value) {
                                                                   # Render as an X mark or check mark
                                                                   if (value == FALSE) "\u274c No" 
                                                                   else "\u2714\ufe0f Yes"
                                                                 }),
                                            color_selectable = colDef(name = "Color selectable", 
                                                                      minWidth = 90, 
                                                                      align = "center",
                                                                      cell = function(value) {
                                                                        # Render as an X mark or check mark
                                                                        if (value == FALSE) "\u274c No" 
                                                                        else "\u2714\ufe0f Yes"
                                                                      }),
                                            likes = colDef(name = "Likes", 
                                                           align = "center"
                                            ),
                                            original_price = colDef(name = "Original price", 
                                                                    align = "center",
                                                                    format = colFormat(prefix = "$")),
                                            current_price = colDef(name = "Current Price", align = "center",
                                                                   format = colFormat(prefix = "$")),
                                            discount = colDef(name = "Discount", align = "center", 
                                                              format = colFormat(suffix = "%"))
                                          ),
                                          filterable = TRUE
  )
  })
  
  # Render table in Price Analysis
  price_compare <- reactive({ 
    price_med <- data() %>% 
      group_by(category) %>% 
      summarise(avg_original_price = round(mean(original_price), digits = 1),
                avg_current_price = round(mean(current_price), digits = 1),
                "0-10" = median(likes[current_price>=0 & current_price<=10]),
                "10-20" = median(likes[current_price>10 & current_price<=20]),
                "20-30" = median(likes[current_price>20 & current_price<=30]),
                "30-40" = median(likes[current_price>30 & current_price<=40]),
                "40-50" = median(likes[current_price>40 & current_price<=50]),
                "50-60" = median(likes[current_price>50 & current_price<=60]),
                "60-70" = median(likes[current_price>60 & current_price<=70]),
                "70-80" = median(likes[current_price>70 & current_price<=80]),
                "80-90" = median(likes[current_price>80 & current_price<=90]),
                "90-100" = median(likes[current_price>90 & current_price<=100]),
                "100-110" = median(likes[current_price>100 & current_price<=110]),
                "110-120" = median(likes[current_price>110 & current_price<=120]),
                "120-130" = median(likes[current_price>120 & current_price<=130]),
                "130-140" = median(likes[current_price>130 & current_price<=140]),
                "140-150" = median(likes[current_price>140 & current_price<=150]))
    
    price_med$best_price_range<-colnames(price_med)[apply(price_med,1,which.max)]
    best_price_med <- price_med %>% select(c(category, 
                                             avg_original_price, avg_current_price,
                                             best_price_range))
  }) 
  
  output$price_tb <- renderReactable({reactable(price_compare(),
                                                defaultPageSize = 5,
                                                columns = list(
                                                  category = colDef(name = "Category", minWidth = 100),
                                                  avg_original_price = colDef(
                                                    name = "Average original price",
                                                    format = colFormat(prefix = "$"),
                                                    align = "center", 
                                                    minWidth = 80),
                                                  avg_current_price = colDef(
                                                    name = "Average current price", 
                                                    format = colFormat(prefix = "$"),
                                                    minWidth = 80, 
                                                    align = "center"),
                                                  best_price_range = colDef(
                                                    name = "Best price range", 
                                                    format = colFormat(prefix = "$"),
                                                    align = "center",
                                                    style = list(background = "#ECBEAF")
                                                  )
                                                )
  )
  })
  
  # Render plots in Features Distribition
  # Category plot
  output$catplot <- renderPlot({
    filter_category() %>% 
      ggplot(aes(x = nb_products, y = reorder(category, nb_products))) + 
      geom_col(fill="#C06C84")  +
      theme_minimal()+
      labs(title = "Number of Products by Categories",
           x = "",
           y = "") +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.y = element_text(size = 12))
  })
  
  # Brand plot
  output$brand <- renderPlot({
    data() %>% 
      group_by(brand) %>% 
      summarise(nb_products = n()) %>%
      ggplot(aes(y = nb_products, x = reorder(brand, nb_products))) + 
      geom_col(fill="#C06C84")  +
      theme_minimal()+
      labs(title = "Number of Products by Brands",
           x = "",
           y = "") +
      theme(plot.title = element_text(face = "bold", size = 14),   
            axis.text.x = element_text(angle= 50, hjust=1))
  })
  
  # Likes plots
  output$like1 <- renderPlot({
    data() %>% 
      ggplot(aes(x = likes)) + 
      geom_histogram(fill="#C06C84", color="black", bins=40) +
      labs(x = "") +
      theme_minimal()
  })
  
  output$like2 <- renderPlot({
    data() %>% 
      ggplot(aes(x=likes, y=category, fill=category)) + 
      geom_boxplot(fill="#C06C84") +
      labs(x = "", y= "Categories") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12))
  })
  
  # Current price plot
  output$cur_price <- renderPlot({
    data() %>% 
      ggplot(aes(x=current_price)) +
      geom_density(fill="#6C5B7B", color="#e9ecef", alpha=0.8)+
      theme_minimal()+
      labs(x = "")
  })
  
  # Original price plot
  output$org_price <- renderPlot({
    data() %>% 
      ggplot(aes(x=original_price)) +
      geom_density(fill="#6C5B7B", color="#e9ecef", alpha=0.8)+
      theme_minimal()+
      labs(x = "")
  })
  
  # Discount plot
  output$discount <- renderPlot({
    data() %>% 
      ggplot(aes(x=discount)) +
      geom_density(fill="#6C5B7B", color="#e9ecef", alpha=0.8)+
      theme_minimal()+
      labs(x = "")
  })
  
  # Color selectable plot
  color_select <- reactive({ 
    colour <- data() %>% 
      group_by(color_selectable) %>% 
      count() 
    # Compute percentages
    colour$fraction <- colour$n / sum(colour$n)
    
    # Compute the cumulative percentages (top of each rectangle)
    colour$ymax <- cumsum(colour$fraction)
    
    # Compute the bottom of each rectangle
    colour$ymin <- c(0, head(colour$ymax, n=-1))
    
    # Compute label position
    colour$labelPosition <- (colour$ymax + colour$ymin) / 2
    
    # Compute a good label
    colour$label <- paste0(colour$color_selectable, "\n value: ", colour$n)
    colour <- as.data.frame(colour)
  }) 
  
  output$color <- renderPlot({
    color_select() %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=color_selectable)) +
      geom_rect() +
      geom_text(x=2.5, # x here controls label position (inner / outer)
                aes(y=labelPosition, label=label, color=color_selectable), 
                size=4) + 
      scale_fill_manual(values = c("#E9967A", "#6C5B7B")) +
      scale_color_manual(values = c("#E9967A", "#6C5B7B")) +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      theme_void() +
      labs(title = "Can the product be selected based on colors?") +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            legend.position = "none",)
  })
  
  # Newly added plot
  newly <- reactive({ 
    neww <- data() %>% 
      group_by(newly_added) %>% 
      count() 
    # Compute percentages
    neww$fraction <- neww$n / sum(neww$n)
    
    # Compute the cumulative percentages (top of each rectangle)
    neww$ymax <- cumsum(neww$fraction)
    
    # Compute the bottom of each rectangle
    neww$ymin <- c(0, head(neww$ymax, n=-1))
    
    # Compute label position
    neww$labelPosition <- (neww$ymax + neww$ymin) / 2
    
    # Compute a good label
    neww$label <- paste0(neww$newly_added, "\n value: ", neww$n)
    neww <- as.data.frame(neww)
  }) 
  
  output$new <- renderPlot({
    newly() %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=newly_added)) +
      geom_rect() +
      geom_text( x=2.5, # x here controls label position (inner / outer)
                 aes(y=labelPosition, label=label, color=newly_added), 
                 size=4) + 
      scale_fill_manual(values = c("#E9967A", "#6C5B7B")) +
      scale_color_manual(values = c("#E9967A", "#6C5B7B")) +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
      labs(title = "Is the product new arrival?")
  })
  
  # Render plot in Category Comparison 
  filter_category <- reactive({ 
    dat1 <- data() %>% 
      group_by(category) %>% 
      summarise(nb_products = n(),
                median_likes = median(likes),
                avg_current_price = mean(current_price),
                avg_original_price = mean(original_price)) %>% 
      arrange(desc(median_likes)) %>% 
      select(c(category, nb_products, median_likes, avg_current_price))
  }) 
  
  output$catplot2 <- renderPlot({
    #Reference: https://www.r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html
    filter_category() %>%                   
      ggplot() +
      # Make custom panel grid
      geom_hline(
        aes(yintercept = y), 
        data.frame(y = c(0:4)*50),
        color = "lightgrey"
      ) + 
      # Add bars to represent the median number of likes
      # str_wrap(region, 5) wraps the text so each line has at most 5 characters
      # (but it doesn't break long words!)
      geom_col(
        aes(
          x = reorder(str_wrap(category, 5), median_likes),
          y = median_likes,
          fill = nb_products
        ),
        position = "dodge2",
        show.legend = TRUE,
        alpha = .9
      ) +
      
      # Add dots to represent the average current price
      geom_point(
        aes(
          x = reorder(str_wrap(category, 5), median_likes),
          y = avg_current_price
        ),
        size = 4,
        color = "gray12"
      ) +
      
      # Lollipop shaft for average current price per category
      geom_segment(
        aes(
          x = reorder(str_wrap(category, 5), median_likes),
          y = 0,
          xend = reorder(str_wrap(category, 5), median_likes),
          yend = 200
        ),
        linetype = "dashed",
        color = "gray12"
      ) + 
      
      # Make it circular!
      coord_polar() +
      
      # Annotate the bars and the lollipops so the reader understands the scaling
      ggplot2::annotate(
        x = 9.8, 
        y = 100,
        label = "Avg price (USD) ",
        geom = "text",
        angle = -65,
        color = "gray12",
        size = 5
      ) +
      ggplot2::annotate(
        x = 10, 
        y = 190,
        label = "Median nb likes",
        geom = "text",
        angle = 23,
        color = "gray12",
        size = 5
      ) +
      # Annotate custom scale inside plot
      ggplot2::annotate(
        x = 10.7, 
        y = 60, 
        label = "50", 
        geom = "text", 
        color = "gray12",
        size = 5
      ) +
      ggplot2::annotate(
        x = 10.7, 
        y = 110, 
        label = "100", 
        geom = "text", 
        color = "gray12",
        size = 5
      ) +
      ggplot2::annotate(
        x = 10.7, 
        y =160, 
        label = "150", 
        geom = "text", 
        color = "gray12",
        size = 5
      ) +
      ggplot2::annotate(
        x = 10.7, 
        y = 210, 
        label = "200", 
        geom = "text", 
        color = "gray12",
        size = 5
      ) +
      # Scale y axis so bars don't start in the center
      scale_y_continuous(
        limits = c(-50, 210),
        expand = c(0, 0),
        breaks = c(0, 50, 100, 150, 200)
      ) + 
      # New fill and legend title for number of products per category
      scale_fill_gradientn(
        "Number of Products",
        colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
      ) +
      # Make the guide for the fill discrete
      guides(
        fill = guide_colorsteps(
          barwidth = 20, barheight = .8, title.position = "top", title.hjust = .5
        )
      ) +
      theme(
        # Remove axis ticks and text
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        # Use gray text for the category names
        axis.text.x = element_text(color = "gray12", size = 13),
        # Move the legend to the bottom
        legend.position = "bottom"
      ) +
      
      # Customize general theme
      theme(
        # Make the background white and remove extra grid lines
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank()
      ) 
  })
  
  # Render plot in Price Analysis
  output$catplot3<- renderPlot({
    data() %>% 
      group_by(category) %>% 
      summarise(nb_products = n(),
                median_likes = median(likes),
                avg_current_price = mean(current_price),
                avg_original_price = mean(original_price)) %>% 
      arrange(desc(median_likes)) %>% 
      select(category, avg_current_price, avg_original_price)  %>% 
      pivot_longer(cols = -category,
                   names_to = "Prices",
                   values_to = "USD") %>% 
      ggplot(aes(fill=Prices, x=category, y = USD)) + 
      geom_bar(position="dodge", stat="identity") +
      theme_minimal() +
      scale_fill_manual(values = c("#F67280", "#6C5B7B"),
                        labels = c("Avg current price", "Avg original price")) +
      theme(axis.text.x = element_text(angle= 50, size = 13, hjust=1),
            axis.text.y = element_text(size = 13)) +
      labs(x = "") 
  })
  
  # Render plots in Correlation 
  # Correlation matrix
  corr_mtx <- reactive({  
    shoes_corr <- data() %>% 
      mutate(brand = if_else(         #replace Nan value in "brand" with 0, else with 1
        is.na(brand), 0, 1)
      )  
    cols <- sapply(shoes_corr, is.logical)
    shoes_corr[,cols] <- lapply(shoes_corr[,cols], as.numeric)
    
    shoes_corr %>%
      select(likes, original_price, current_price, discount, 
             color_selectable, newly_added, brand) %>%
      correlate(diagonal = 1) %>%
      pivot_longer(
        cols = -term,
        names_to = "colname",
        values_to = "corr"
      ) %>%
      mutate(rowname = fct_inorder(term),
             colname = fct_inorder(colname)) 
  }) 
  
  output$corr1<- renderPlot({
    corr_mtx() %>% 
      ggplot(aes(rowname, fct_rev(colname),
                 fill = corr)) +
      geom_tile() +
      geom_text(aes(label = round(corr, 2))) +
      coord_fixed() +
      labs(x = NULL, y = NULL) +
      theme(panel.border = element_rect(color = NA, fill = NA)) +
      scale_fill_distiller(palette = "RdBu", limit=c(-0.6,1.3))+
      theme(axis.text.x = element_text(angle= 50, hjust=1, size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  # Scatter plots
  output$corr2<- renderPlot({
    data() %>% 
      ggplot(aes(x = current_price, y = likes)) + 
      geom_pointdensity(adjust=0.1) +
      scale_color_viridis()+
      geom_smooth() +
      theme_minimal()+
      scale_y_log10() +
      labs(x = "Current Price (USD)",
           y = "Likes")
  })
  
  output$corr3<- renderPlot({
    data() %>% 
      ggplot(aes(x = current_price, y = likes)) + 
      geom_pointdensity(size=0.4) +
      scale_color_viridis()+
      facet_wrap(vars(category), ncol = 2) + 
      scale_y_log10() + 
      theme_bw()+
      theme(legend.position = "none") +
      labs(x = "Current Price (USD)",
           y = "Likes")
  })
  
  # Render plots in Text Mining
  bind_tf_idf_toplike <- reactive({ 
    dat2 <- data() %>% 
      filter(likes>100) %>% 
      unnest_tokens(word, short_desc) %>% 
      group_by(category, word) %>% 
      count(word) %>%
      bind_tf_idf(word, category, n) %>% 
      group_by(category)
  }) 
  
  # Derbies & Mocassins 
  output$derbies_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Derbies & Mocassins") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Baskets
  output$baskets_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Baskets") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Bottes & Bottines
  output$bottes_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Bottes & Bottines") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Sandales & Mules 
  output$sandales_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Sandales & Mules") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Chaussons 
  output$chaussons_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Chaussons") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Escarpins
  output$escarpins_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Escarpins") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Chaussures de ville 
  output$ville_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Chaussures de ville") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Accessoires chaussures 
  output$accessoire_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Accessoires chaussures") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Plateforme
  output$plateforme_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Plateforme") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
  # Chaussures de sport
  output$sport_plot<- renderPlot({
    bind_tf_idf_toplike() %>% 
      filter(category=="Chaussures de sport") %>%
      slice_max(tf_idf, n = 20) %>% 
      ggplot(aes(y=tf_idf, x=fct_reorder(word, tf_idf))) +
      geom_segment(aes(xend=fct_reorder(word, tf_idf), yend=0), color="#3a435e") +
      geom_point(size=2.5, color="#F67280", 
                 fill=alpha("orange", 0.3),
                 alpha=0.7, shape=21, stroke=3) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      labs(x = "",
           y = "tf_idf") 
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

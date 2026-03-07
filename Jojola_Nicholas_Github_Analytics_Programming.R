library(dplyr)
library(ggplot2)
library(DiagrammeR)


#Flowchart

grViz("
digraph EcoMartDB {

  graph [
    layout = dot,
    rankdir = TB,
    bgcolor = white,
    nodesep = 0.5,
    ranksep = 0.8
  ]

  node [
    shape = box,
    style = filled,
    fillcolor = lightblue,
    color = black,
    fontname = Helvetica,
    fontsize = 16,
    margin = 0.15
  ]

  edge [
    color = black,
    arrowsize = 0.8,
    penwidth = 1.2
  ]

  country [label = 'dim_country\\ncountry_id (PK)\\ncountry_name\\nregion']
  channel [label = 'dim_channel\\nchannel_id (PK)\\nsales_channel']
  priority [label = 'dim_priority\\npriority_id (PK)\\norder_priority']
  product [label = 'dim_product\\nproduct_id (PK)\\nitem_type']
  attr_def [label = 'attribute_definition\\nattribute_id (PK)\\nattribute_name\\ndata_type']

  orders [
    label = 'fact_orders\\norder_fact_id (PK)\\norder_id\\nproduct_id (FK)\\ncountry_id (FK)\\nchannel_id (FK)\\npriority_id (FK)\\norder_date\\nship_date\\nunits_sold\\nunit_price\\nunit_cost\\ntotal_revenue\\ntotal_cost\\ntotal_profit',
    fillcolor = gray90
  ]

  attr_val [
    label = 'product_attribute_value\\nproduct_id (PK, FK)\\nattribute_id (PK, FK)\\nvalue_text\\nvalue_number\\nvalue_date'
  ]

  { rank = same; country; channel; priority; product; attr_def }
  { rank = same; orders; attr_val }

  country -> orders
  channel -> orders
  priority -> orders
  product -> orders

  product -> attr_val
  attr_def -> attr_val
}
")

#Pseudocode
#•	Begin
#•	Import comma-separated value data into a data frame, SET as df 
#•	Identify duplicate rows
#•	remove duplicates from df
#•	Group data from df by state, 
#•	For each numeric value calculate mean, median, max, and min
#•	Filter df by debt-to-equity column where the number is less than 0
#•	Save filtered rows in neg_df
#•	For each row in df, if total revenue is equal to 0 set debt to income to na, otherwise set
#debt-to-income column to long-term-debt divided by revenue
#•	Select debt-to-income column for new data frame set as d_2_I
#•	Concatenate d_2_I data frame to df 
#•	Output data from final_df  data frame
#•	End

#Flow Chart	Pseudo Code	Logic
#Begin	Begin	Start of program
#Read CSV and set to Df	Import comma-separated value data into a data frame, SET as df	This establishes a structured container for manipulation.
#Check for duplicate rows
#Identify duplicate rows
#Detect and remove duplicates in the df to ensure data integrity before calculations
#Are there duplicates? (1/2)	remove duplicates from df
#Duplicates detected? No: Proceed
#Duplicates detected? Yes: Remove Duplicates
#Group Data by State, Calculate Mean, Median, Min, Max	1. Group Data by state 2. For each numeric value, calculate mean, median, max, and min
#Group data by state to then perform a statistical summary on the data. This aggregation step is performed before filtering, ensuring that summary statistics reflect the complete dataset rather than a subset.
#Are there negative debt to equity entries? Yes: Save to Neg_df. No: Proceed	1.Filter df by debt-to-equity column where the number is less than 0. 2. Save filtered rows in neg_df

#The program identifies companies with negative debt-to-equity ratios and stores them in a separate data frame (neg_df). This separation allows for outliers to be analyzed independently 
#Is total revenue = 0 Yes: Set debt to income to Na. No: Compute debt to income as total long term debt / total revenue	For each row in df, if total revenue is equal to 0 set debt to income to na, otherwise set
#debt-to-income column to long-term-debt divided by revenue	The debt-to-income ratio is then introduced to a decision structure. Any company with a revenue of 0 will be labeled Na for its debt-to-income ratio, because dividing by 0 is not possible. Otherwise, the ratio is calculated by long-term debt divided by total revenue
#Concatenate d_2_I to df, set as final_df	Concatenate d_2_I data frame to df	The debt-to-income ratio is concatenated back into the original dataset for easy queries.
#Print final_df	Output data from final_df  data frame	Print a readable dataframe with all pertinent calculations
#End	End	End Program

#The program's logic follows a structured data processing pipeline designed to ensure accuracy, integrity, and analytical validity. 

#1.	Import CSV data into a data frame (df). This establishes a structured container for manipulation.
#2.	Detect and remove duplicates in the df to ensure data integrity before calculations. This will prevent measures of central tendency and other calculations from being inaccurate later in the program. 
#3.	The program then groups the data by state to compute descriptive statistics: mean, median, minimum, and maximum for all numeric variables. This aggregation step is performed before filtering, ensuring that summary statistics reflect the complete dataset rather than a subset. 
#4.	Next, the program identifies companies with negative debt-to-equity ratios and stores them in a separate data frame (neg_df). This separation allows for outliers to be analyzed independently without modifying the cleaned primary dataset.
#5.	The debt-to-income ratio is then introduced to a decision structure. Any company with a revenue of 0 will be labeled Na for its debt-to-income ratio, because dividing by 0 is not possible. Otherwise, the ratio is calculated by long-term debt divided by total revenue. 
#6.	This conditional logic ensures mathematical accuracy and prevents errors. 
#7.	Lastly, the debt-to-income ratio is concatenated back into the original dataset for easy queries. 

#Overall, Flowcharts offer a visual representation
#that outlines the program's sequence of actions 
#and decision points, facilitating a comprehensive 
#understanding of its logic and structure. 
#Meanwhile, pseudocode serves as a bridge 
#between human-readable language and actual code,
#articulating the algorithm's logic in a simplified,
#syntax-free manner. (Western Governors University, n.d.).
#Using both flowcharts and pseudocode provides 
#comprehensive algorithmic documentation, 
#offering multiple perspectives 

df <- read.csv("/Users/nicholasjojola/Desktop/MSDADS/analytical programming/D598 Data Set.csv")

#removes duplicate entries

df <- df %>% distinct()
#shows duplications with true or false output.
print(df)

#Grouped data and summarized

g_data <- df %>%
  group_by(Business.State) %>%
  summarize(across(where(is.numeric), list(mean = ~ mean (.x, na.rm = TRUE),
    median = ~median (.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm= TRUE))))
print(g_data)
#filtered debt to equity by negative
neg_df <- df %>%
  filter(Debt.to.Equity < 0)
print(neg_df)
#Debt to Income creation
d_2_I <- df %>%
  mutate(
        debt.to.income = ifelse(
          Total.Revenue == 0, 
          NA_real_, 
          Total.Long.term.Debt/Total.Revenue
        )
      ) %>%
      select(debt.to.income)
print(d_2_I)

#Concatination of debt to income into the original df
final_df <- bind_cols(d_2_I, df)
print(final_df)
#Visualization
ggplot(data = final_df, aes(x=Business.State, y=Total.Revenue)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggplot(data = final_df, aes(x=Business.State, y=debt.to.income, fill = Business.State)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))
ggplot(data = final_df, aes(x = Business.State, y = debt.to.income, color = Business.State)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.background = element_rect(fill = "white"))
ggplot(data = final_df, aes(x=Total.Long.term.Debt, y=Total.Equity)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = TRUE, color = "red")
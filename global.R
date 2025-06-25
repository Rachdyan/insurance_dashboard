# package <- c('shiny', 'shinydashboard', 'tibble', 'dplyr', 'apexcharter',
#   'DT', 'lubridate', 'shinyWidgets',
#   'ggplot2', 'ggrepel', 'scales', 'shinyjs', 'glue',
#   'plotly', 'faq', 'tidyr', 'gridExtra', 'grid', 'ggtext', 'shinyBS',
#   'viridis', 'bsplus', 'ggthemes', 'stringr', 'shinycssloaders', 'png',
#   'purrr', 'openxlsx', 'gtable')
#
#
# for (i in package){
#   print(i)
#   if(require(i, character.only=TRUE)){
#     print(paste(i, "is loaded correctly"))
#   } else{
#     print(paste("trying to install", i))
#     install.packages(i)
#     if(require(i, character.only=TRUE)){
#       print(paste(i, "installed and loaded"))
#     } else{
#       stop(paste("could not install", i))
#     }
#   }
# }
#
#
# c('shiny', 'shinydashboard', 'tibble', 'dplyr', 'apexcharter',
#   'DT', 'lubridate', 'shinyWidgets',
#   'ggplot2', 'ggrepel', 'scales', 'shinyjs', 'glue',
#   'plotly', 'faq', 'tidyr', 'gridExtra', 'grid', 'ggtext', 'shinyBS',
#   'viridis', 'bsplus', 'ggthemes', 'stringr', 'shinycssloaders', 'png',
#   'purrr', 'openxlsx', 'gtable')

library(shiny)
library(shinydashboard)
library(tibble)
library(apexcharter)
library(DT)
library(lubridate)
library(shinyWidgets)
library(ggplot2)
library(ggrepel)
library(scales)
library(shinyjs)
library(glue)
library(plotly)
library(faq)
library(tidyr)
library(gridExtra)
library(grid)
library(ggtext)
library(shinyBS)
library(viridis)
library(bsplus)
library(ggthemes)
library(stringr)
library(shinycssloaders)
library(png)
library(purrr)
library(openxlsx)
library(gtable)
library(dplyr)
library(tibble)
library(shinymanager)

# credentials <- data.frame(
#   user = c("testuser"), # mandatory
#   password = c("test2024"), # mandatory
#   admin = c(TRUE)
# )

all_data <- readRDS("./data/all_data.RDS")

company_db <- all_data %>%
  distinct(companycode, shortname) %>%
  filter(!is.na(shortname)) %>%
  arrange(shortname)

group_db <- all_data %>%
  distinct(groupcode, groupname) %>%
  filter(!is.na(groupname)) %>%
  arrange(groupname)


overview_table_single_firm_df <- readRDS("./data/overview_table_single_firm.RDS") %>%
  left_join(company_db,  by = join_by("Firm Name" == "shortname")) %>%
  relocate(companycode, .before = everything())

overview_table_group_df <- readRDS("./data/overview_table_group.RDS") %>%
  left_join(group_db) %>%
  relocate(groupcode, .before = everything())



## COMPANY TABLE DATA ------
# CALCULATE FIRM LAST YEAR STATISTIC
firm_last_year <- all_data %>%
  dplyr::group_by(companycode, shortname, identifier, year) %>%
  summarize(value = sum(value)) %>%
  filter(year == 2022)

premium_affiliatedfminsco_firm = all_data %>% filter(identifier == "01")
losses_paid_affiliatedfminsco_firm = all_data %>% filter(identifier == "05")

loss_ratio_firm <-
  left_join(losses_paid_affiliatedfminsco_firm, premium_affiliatedfminsco_firm, relationship = "many-to-many",
    by = c("shortname","fullname", "groupname", "year")) %>%
  filter(year == 2022) %>%
  select(shortname, fullname, groupname, year, value.x, value.y) %>%
  group_by(shortname, year) %>%
  summarize(value.x = sum(value.x, na.rm = T), value.y = sum(value.y, na.rm = T)) %>%
  mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
  mutate(lossratio = value.x / value.y)

firm_combined_lastyear <-
  left_join(company_db, firm_last_year, relationship = "many-to-many") %>%
  pivot_wider(names_from = identifier, values_from = value) %>%
  rename(`Written Premium Last Year` = `01`,
    `Earned Premium Last Year` = `02`,
    `Total Losses Last Year` = `05`) %>%
  select(companycode, shortname, `Written Premium Last Year`, `Earned Premium Last Year`, `Total Losses Last Year`)

firm_combined_lastyear_stat <-
  left_join(firm_combined_lastyear, loss_ratio_firm, relationship = "many-to-many") %>%
  select(-c(year, value.x, value.y)) %>%
  rename(`Loss Ratio Last Year` = `lossratio`) %>%
  mutate(`Loss Ratio Last Year` = round(`Loss Ratio Last Year`, 4))

group_firm_db <- all_data %>%
  distinct(groupcode, groupname, companycode, shortname) %>%
  filter(!is.na(shortname)) %>%
  arrange(groupname)

group_firm_modal_db <- left_join(group_firm_db, firm_combined_lastyear_stat) %>%
  select(1:5) %>%
  rename(`LY Premiums` = 'Written Premium Last Year')

#
# ## CALCULATE GROUP LAST YEAR STATISTIC
# group_last_year <- all_data %>%
#   dplyr::group_by(groupcode, groupname, identifier, year) %>%
#   summarize(value = sum(value)) %>%
#   filter(year == 2022)
#
# premium_affiliatedfminsco_group <- all_data %>% filter(identifier == "01")
# losses_paid_affiliatedfminsco_group <- all_data %>% filter(identifier == "05")
#
# loss_ratio_group <-
#   left_join(losses_paid_affiliatedfminsco_group, premium_affiliatedfminsco_group, relationship = "many-to-many",
#     by = c("shortname","fullname", "groupname", "year")) %>%
#   filter(year == 2022) %>%
#   select(shortname, fullname, groupname, year, value.x, value.y) %>%
#   group_by(groupname, year) %>%
#   summarize(value.x = sum(value.x, na.rm = T), value.y = sum(value.y, na.rm = T)) %>%
#   mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
#   mutate(lossratio = value.x / value.y)
#
# group_combined_lastyear <-
#   left_join(group_db, group_last_year, relationship = "many-to-many") %>%
#   pivot_wider(names_from = identifier, values_from = value) %>%
#   rename(`Written Premium Last Year` = `01`,
#     `Earned Premium Last Year` = `02`,
#     `Total Losses Last Year` = `05`) %>%
#   select(groupcode, groupname, `Written Premium Last Year`, `Earned Premium Last Year`, `Total Losses Last Year`)
#
# group_combined_lastyear_stat <-
#   left_join(group_combined_lastyear, loss_ratio_group, relationship = "many-to-many") %>%
#   select(-c(year, value.x, value.y)) %>%
#   rename(`Loss Ratio Last Year` = `lossratio`) %>%
#   mutate(`Loss Ratio Last Year` = round(`Loss Ratio Last Year`, 4))


## WIKI DATA ------
wiki_db <- all_data %>%
  distinct(shortname) %>%
  mutate(`Written Premiums` = "Written Premiums Test", `Earned Premiums` = "Earned Premiums Test", `Losses Paid` = "Losses Paid Test") %>%
  pivot_longer(c(`Written Premiums`, `Earned Premiums`, `Losses Paid`), names_to = 'question', values_to = 'answer')

firm_overview_raw <- read.csv("data/firm_overview_data.csv", sep = ";")

firm_overview_db <- all_data %>%
  dplyr::distinct(shortname) %>%
  left_join(firm_overview_raw, by = c("shortname" = "Entity.Name")) %>%
  mutate(Wiki.Text = ifelse(is.na(Wiki.Text), "", Wiki.Text))


global_wiki_db <- tibble(question = 'Title', answer = 'content')

## MGA DATA --------

mga_overview_data <- read.csv("data/mga_list.csv")  %>%
  rowid_to_column() %>%
  rename(mga_index = rowid)

mga_index_data <- mga_overview_data %>%
  select(mga_index, Firm) %>%
  rename(MGA.Name = Firm)

mga_detail <- read.csv("data/mga_detail.csv")
mga_export_table <- read.csv("data/mga_export_table.csv")[, -c(2:4)]

mga_complete_data <- left_join(mga_detail, mga_export_table, by = join_by(MGA.Name == Firm)) %>%
  select(-Size.x) %>%
  rename(Size = Size.y) %>%
  left_join(mga_index_data)


mga_news <- read.csv("data/news_articles.csv") %>%
  filter(Headline != "") %>%
  mutate(Headline =  sapply(Headline, function(x) paste(strwrap(x, 45), collapse = "<br>"))) %>%
  rename(`Title` = `Headline`) %>%
  select(-X.) %>%
  mutate(Headline = glue('<a href = "{Link}" target = "_blank"> {Title} </a>'))

## GEOGRAPHY DATA -------
geography_raw_df <- read.csv("./data/geography_premiums.csv", check.names = FALSE)
geography_pivot_df <- pivot_longer(geography_raw_df, cols = 2:15, names_to = "year", values_to = "premium_billions")



## GGPLOT THEME -------

universal_graph_theme <-  theme_economist() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title=element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 8.5),
    axis.title.x = element_blank(),
    axis.text = element_text(size=11),
    axis.text.y = element_text(margin = margin(r=4), hjust = 1),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey"),
    axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
    axis.line.x.bottom = element_line(linewidth = 0.5))

universal_graph_theme_with_legend <- function(legend_x = 0.2, legend_y = 0.9){
  theme_economist() + theme(panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title=element_blank(),
    legend.key.height= unit(0.35, 'cm'),
    legend.key.width= unit(0.35, 'cm'),
    legend.position = c(legend_x, legend_y),
    legend.text=element_text(size=7),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 8.5),
    axis.text.y = element_text(margin = margin(r=4), hjust = 1),
    axis.title.x = element_blank(), axis.text = element_text(size=8.5),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey"),
    axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
    axis.line.x.bottom = element_line(linewidth = 0.5))
}


single_graph_export_theme <- theme(
  plot.title = element_markdown(box.color = "black",
    size = 9,
    linewidth = 0.5,
    hjust = 0.5,
    r = unit(0, "mm"), linetype = 1,
    padding = unit(3, "mm"),
    margin = margin(0,0,7,0)),
  plot.margin=margin(10,10,10,10))


fix_legend_plotly <- function(plotly_data){
  # Get the names of the legend entries
  df <- data.frame(id = seq_along(plotly_data$x$data), legend_entries = unlist(lapply(plotly_data$x$data, `[[`, "name")))
  # Extract the group identifier
  df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
  # Add an indicator for the first entry per group
  df$is_first <- !duplicated(df$legend_group)

  for (i in df$id) {
    # Is the layer the first entry of the group?
    is_first <- df$is_first[[i]]
    # Assign the group identifier to the name and legendgroup arguments
    plotly_data$x$data[[i]]$name <- df$legend_group[[i]]
    plotly_data$x$data[[i]]$legendgroup <- plotly_data$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) plotly_data$x$data[[i]]$showlegend <- T
  }
  return(plotly_data)
}


## GROB FUNCTION -------
table_to_grob <- function(data, colhead_cex = 0.7){
  tableGrob(data, rows = NULL,
    theme=ttheme_default(base_size = 10,
      colhead = list(fg_params=list(cex = colhead_cex)),
      core = list(fg_params=list(cex = 0.8)),
    padding = unit(c(3, 3), "mm")))
}

generate_table_col_name <- function(type, min_year, max_year){
  years <- min_year:max_year
  all_name <- character(0)
  for(name in type){
    current_name <- paste0(name, " " , years)
    all_name <- c(all_name, current_name)
  }
  all_name
}







#### ROC visualization app ####
## John D. Lee 
## 2/15/2020


## Configure workspace
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(caret)
library(patchwork)
library(plotROC)
library(pROC)

set.seed(888) # To ensure consistent results from non-deterministic procedures
rm(list = ls()) # Removes all variables


## Read data
SF.df = data_frame(obs = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   pred = c(.9, .8, .3, .9, .8, .7, .8, .8, .4, .9, .6, .7, 
                            .1, .1, .2, .7, .4, .5, .3, .1, .2, .6, .4, .4 ))

## Calculate AUC
SF.roc = roc(predictor = SF.df$pred, 
             response = SF.df$obs, 
             AUC = TRUE, ci = TRUE)
SF.roc$auc


## Add threshold to all rows
tp_fp_threshold.df = SF.df %>% rowwise() %>% 
    mutate(threshold = list(seq(from = 0, to = 1, by = .1))) %>%
    unnest(cols = c(threshold))


## Calculate true positive and false positve fraction for all thresholds
tp_fp_threshold.df = tp_fp_threshold.df %>% group_by(threshold) %>% 
    summarise(total_obs1 = sum(obs == 1),
              total_obs0 = sum(obs == 0),
              tp = sum(obs == 1 & pred >= threshold)/total_obs1,
              fp = sum(obs == 0 & pred >= threshold)/total_obs0)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Receiver Operator Characteristic (ROC) analysis"),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            setSliderColor(c("darkgrey"), c(1)),
            sliderInput("threshold",
                        "Theshold value: All items above this level are indicated as positive cases.",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .1, playButton),
            ## Application description
            "The left plot with the filled circles shows the distribution of positive cases. 
            The lower plot with the open circles shows the distribution of negative cases. 
            Each circle represents a separate observation.",
            br(), br(),
            "The shaded area indicate the cases where the evidence is above the threshold for indicating
            the cases as positive.
            The stepped line indicates the cummulative density of the cases indicated as positive--
            the true positive fraction for the graph on the left and the false positive fraction for the graph 
            on the bottom.",
            br(), br(),
            "The ROC curve shows the true positive fraction against the false postive fraction for different thresholds."
        ),

        # Show a ROC plot 
        mainPanel(
           plotOutput("ROC_Plot"),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ROC_Plot <- renderPlot({
        tp_fp.df = tp_fp_threshold.df %>% filter(near(threshold, input$threshold)) # Filter based on slider
        
        tp.plot = ggplot(SF.df %>% filter(obs == 1),
                         aes(x = pred)) + 
            annotate("rect", xmin = Inf, xmax =  tp_fp.df$threshold, ymin = -Inf, ymax = Inf, 
                     colour = "grey80", alpha = .2) +
            geom_vline(xintercept = tp_fp.df$threshold, colour = "darkgrey") +
            geom_hline(yintercept = tp_fp.df$tp, colour = "darkred") +
            stat_ecdf(geom = "step") +
            geom_dotplot(binwidth = .1, dotsize = .5, stackgroups = TRUE, 
                         method = "dotdensity", binpositions = "all") +
            coord_equal() +
            lims(x = c(0, 1), y = c(0, 1)) +
            labs(x = "Strength of evidence", y = "True positive fraction", title = "Positive cases") +
            theme(plot.margin = margin(0, 0, 0, 0, "cm"))
        
        
        fp.plot = ggplot(SF.df %>% filter(obs == 0),
                         aes(x = pred)) + 
            annotate("rect", xmin = Inf, xmax =  tp_fp.df$threshold, ymin = -Inf, ymax = Inf, 
                     colour = "grey80", alpha = .2) +
            geom_vline(xintercept = tp_fp.df$threshold, colour = "darkgrey") +
            geom_hline(yintercept = tp_fp.df$fp, colour = "darkred") +
            stat_ecdf(geom = "step") +
            geom_dotplot(binwidth = .1, dotsize = .5, stackgroups = TRUE, fill = "white", binpositions = "all") +
            coord_flip() +
            lims(x = c(0, 1), y = c(0, 1)) +
            labs(x = "Strength of evidence", y = "False positive fraction", title = "Negative cases") +
            theme(plot.margin = margin(0, 0, .1, .1, "cm"))
        
        
        roc.plot =ggplot(data = SF.df, aes(d = obs, m = pred)) + 
            geom_abline(colour = "grey60") +
            geom_hline(yintercept = tp_fp.df$tp, colour = "darkred") +
            geom_vline(xintercept = tp_fp.df$fp, colour = "darkred") +
            geom_roc(linealpha = .5, pointalpha = .5, labels = TRUE, labelsize = 3) +
            annotate("point", y = tp_fp.df$tp, x = tp_fp.df$fp, colour = "darkred", size = 3) +
            annotate("text", x = .5, y = .375, hjust = 0,
                     label = paste("AUC =", round(SF.roc$auc, 2))) +
            coord_equal() +
            labs(x = "", y = "", title = "ROC Curve") +
            theme(plot.margin = margin(0, 0, 0,0, "cm"))
 
        combined.plot = tp.plot + roc.plot + plot_spacer() + fp.plot +
            plot_layout(ncol = 2)
        combined.plot
    }, height = 700, width = 700 )

}

# Run the application 
shinyApp(ui = ui, server = server)
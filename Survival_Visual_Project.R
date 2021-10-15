###################################################################
# Libraries
###################################################################
library(shiny)
library(shinythemes)
library(ggplot2)
library(transformr)
library(tidyr)
library(plotly)
library(ggthemes)
library(DT)
theme_set(theme_bw())

###################################################################
# UI
###################################################################
ui <- navbarPage(theme = shinytheme("cerulean"),
                 
                 title = "Introduce and Apply Survival Analysis using our Methods Class Concepts",
                 
###################################################################
# Introductory Tab
###################################################################                
                 tabPanel(withMathJax(),
                          
                          title = "Understanding Survival Analysis",
                          
                          h1("Why Survival Analysis?"),
                          h2("Background"),
                          p("If we are interested in modeling the a outcome variable that can be described as time until an event occurs, the collection of statistical methods that fall under survival analysis are the most appropriate. The most obvious example of this would be time until death, but the event of interest does not necessarily need to be negative, where time to response from an intervention in a clinical trial would be a positive outcome, or treatment duration as an outcome would be more neutral. Thus, the response variable is often reffered to as survival time, failure time, or event time, and is most often continuous."),
                          
                          h2("Project Motivation"),
                          p("Given our varying backgrounds in programming and epidemiology and the availability of a covid-related dataset which we wanted to further explore, we aimed to expand on concepts we learned in class by starting with the survival function and building on it to relate it to some very basic applied survival analysis, and then apply those concepts to the covid-related dataset and present our results interactively through a R Shiny-based web-application."),
                          
                          h2("Introductory Terms"),
                          p("It is first important to clarify basic terms and issues regarding survival analysis."),
                          tags$ul(
                            tags$li(withMathJax(tags$b("Survival Time Response", style = "font-size:18px;"),
                                                tags$br(),
                                                sprintf("The survival time reponse outcome variable is usually continuous and always greater equal zero. It is important to emphasize that while for some subjects me might know the exact time of the event, for other subjects we may have incomplete information on the survival time. These imcomplete observations where the responses are imcompletely determined are known as censored responses. These censored responses are one of the primary reasons why traditional regression methods are not appropriate in this situation, because according to Madigan (cite madigan lecture), the time to event response has skewed distribution that is strictly positive, often times the probability of surviving past a given time point might be the focus instead of the expected event time, and finally regression using the hazard function can be more informative than linear regression for understanding the failure process (cite madigan again)."))),
                            tags$li(withMathJax(tags$b("Censoring", style = "font-size:18px;"),
                                                tags$br(),
                                                sprintf("Censored observations are the incompletely determined observations for subject, and censoring happens if we have some information of the event time of our subject but do not know about the exact event time. It is important to mention that in order to be able to conduct survival analysis, the censoring mechanism and survival mechanism must be independently cosidered. The most common type of censoring is right censoring, which is when sruvival time is incomplete on the right side of the follow-up period. The main reasons for right-censoring to occur are either withdrawal from the study, loss to follow-up during the study, or a subject not experiencing the event within the study timeframe."), 
                                                tags$br(),
                                                tags$br(),
                                                tags$ul(tags$b("Types of Right-Censoring", style = "font-size:16px;"),
                                                        tags$br(),
                                                        tags$br(),
                                                        tags$li(tags$b("Type I: "), sprintf("Type I censoring can be either fixed or random. Fixed type I censoring happens when the study ends after a certain amount of time and everyone for whom the event was not observed is censored at the time of the study ending. Random Type I censoring is also when the study is designed to end after a certain amount of time, but different censored subhjects have different censoring times.")),
                                                        tags$br(),
                                                        tags$li(tags$b("Type II:"), sprintf("Type II censoring is when a study ends after a pre-defined number of events if observed, and so all subjects for which the event was not observed until the study ending are censored.")),
                                                        tags$br(),
                                                ),
                                                sprintf("Below we have an example of eight subjects survival data, where the blue circles experienced the event of interest and the red circles were censored at that point in time. We observe that subjects 2 and 8 were censored prematurely and subjects 5 and 6 were censored due to the fact of the study ending at 10 years. For the case of this basic introduction to survival analysis, we will focus on right censoring (will we need to explain more than right sensoring for the actual analysis though?)")),
                                                tags$br(),
                                                hr(),
                                                plotlyOutput("censor_plot", width = "90%"),
                                                tags$br(),
                                                hr()),
                          ),
                          tags$br(),
                          
                          
                          
                          h1("The Survival Function"),
                          h2("Intruduction"),
                          withMathJax(sprintf("From our class, we know that given a valid probability density function (pdf), we have the cumulative distribution function (CDF) of random variable X is defined as $$F(x) = P(X \\leq x)$$ where the associated survival function can then be calculated as $$S(x) = P(X > x) = 1 - F(x).$$")),
                          
                          tags$ul(withMathJax(
                                    tags$b("Some Survival Function Properties", style = "font-size:16px;"),
                                    tags$br(),
                                    tags$br(),
                                    tags$li(sprintf("For a time value \\(x\\), the survival function yields the probability of surviving past time \\(x\\).")),
                                    tags$br(),
                                    tags$li(sprintf("The survival function is a non-increasing function that ranges from \\(t = 0\\) to \\(\\infty \\), where \\(S(t)=1\\) at \\(x=0\\) and \\(S(t) \\rightarrow 0 \\) as \\(t \\rightarrow \\infty \\). These values intuitively make sense, as the probability of surviving past time 0 being 1 is a precondition for the survival function to be calculated for a subject, and as time goes to infinity survival (and followingly the survival function) both go to zero.")),
                                    tags$br(),
                                    tags$li(sprintf("According to Madigan (cite Madigan), the survival function as we outline and in theory is continuous, but for practical applications it is most often observed for time that is discrete (minutes, hours, days).")),
                                    tags$br()
                                  )
                          ),
                          
                          withMathJax(sprintf("For several pdf's of different distributions we can visualize both the CDF and associated Survival function, as can be experimented with below.")),
                          
                          h2("Visualizations for Several Distributions"),
                          
                          h3("The Normal Distribution"),
                          withMathJax(sprintf("As we learned in class, the normal distribution is one of the most important distributions in statistics, as many models and tests assume and required normality of the data in order to be used. Additionally, we have also seen the importance of the normal distribution in the context of the central limit theorem, where sample means (regardless of underlying distribution) behave approximately normal. For a random variable \\(X \\sim Normal(\\mu,\\sigma^2)\\) with mean \\(\\mu\\) and standard deviation \\(\\sigma\\), we have the normal pdf as $$f(x) = \\frac{e^{-(x-\\mu^2)/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ Integrating the normal distribution yields the normal cdf, where it is important to mention that given that the integral does not have a closed form and so it is numerically evaluated and denoted as $$F(x) = \\int_{-\\infty}^{x}\\frac{e^{-(x-\\mu^2)/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ It then follows that the survival function is not closed form and is also expressed in terms of an integral as $$S(x) = 1- \\int_{-\\infty}^{x}\\frac{e^{-(x-\\mu^2)/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ Below, we have interactive plots of the normal pdf, cdf, and survival function, where \\(\\mu\\) and standard deviation \\(\\sigma\\) can be modified to see the effects on the respective functions.")),
                         
                          hr(),
                          tags$br(),
                          
                          sidebarLayout(
                            sidebarPanel(
                              tags$h4("Distribution Parameters"),
                              fluidRow(
                                column(12,sliderInput(inputId="mu_norm",
                                                     label=withMathJax(sprintf("Location Parameter \\(\\mu\\)")),
                                                     min = -4, max = 4, value = 0
                                       ))),
                              fluidRow(
                                column(12,sliderInput(inputId="sdev_norm",
                                                      label=withMathJax(sprintf("Scale Parameter \\(\\sigma\\)")),
                                                      min = 1, max = 10, value = 1))
                              ),
                              width = 3
                            ),
                            mainPanel(
                              column(4,plotOutput("normal_pdf", height = "300px")),
                              column(4,plotOutput("normal_cdf", height = "300px")),
                              column(4,plotOutput("normal_survival", height = "300px")),
                              width = 9
                            )
                          ),
                          
                          tags$br(),
                          hr(),
                          
                          h3("The Exponential Distribution"),
                          
                          withMathJax(sprintf("From our class (and homeworks), we first learned about the exponential distribution through an application, where the random variable of time in years from diagnosis until death of persons with a specific kind of cancer followed an exponential density. In fact, the expontial distribution is often used to model the time until a specific event occurs (Cite Ross page 209). If \\(X \\sim Exponential(\\lambda)\\) with rate parameter \\(\\lambda\\), we then have the associated pdf for \\(x \\geq 0 \\) as $$f(x) = \\lambda e^{-\\lambda x}.$$ Integrating the pdf yields the exponential cdf as $$F(x) = \\int_{0}^{x} \\lambda e^{-\\lambda t}dt = 1 - e^{-\\lambda x}.$$ The associated survival function can be found by taking subtracting the cdf from 1 as $$S(x) = 1 - F(x) = e^{-\\lambda x}.$$ Similar as for the normal distribution, we have interactive plots of the exponential pdf, cdf, and survival function below, where the rate \\(\\lambda\\) can be modified to see the effects on the respective functions.")),
                          
                          hr(),
                          tags$br(),
                          
                          sidebarLayout(
                            sidebarPanel(
                              tags$h4("Distribution Parameters"),
                              fluidRow(
                                column(12,sliderInput(inputId="lambda_exp",
                                                      label=withMathJax(sprintf("Rate Parameter \\(\\lambda\\)")),
                                                      min = 1, max = 10, value = 1
                                ))),
                              width = 3
                            ),
                            mainPanel(
                              column(4,plotOutput("exponential_pdf", height = "300px")),
                              column(4,plotOutput("exponential_cdf", height = "300px")),
                              column(4,plotOutput("exponential_survival", height = "300px")),
                              width = 9
                            )
                          ),
                          
                          tags$br(),
                          hr(),
                         
                          h3("The Uniform Distribution"),
                          
                          withMathJax(sprintf("The uniform distribution has the same density value for every value over a given range, and so if \\(X \\sim U(\\alpha , \\beta)\\) for an interval \\(\\alpha < x < \\beta \\), we then have the associated pdf as $$f(x) = \\frac{1}{\\beta - \\alpha}$$ Integrating the pdf yields the exponential cdf as $$F(x) =  \\frac{x-\\alpha}{\\beta-\\alpha}.$$ Again, the associated survival function can be found by taking subtracting the cdf from 1 as $$S(x) = 1 - F(x) = \\frac{\\beta-x}{\\beta-\\alpha}.$$ Similar to both the exponential and normal distributions, we have interactive plots of the exponential pdf, cdf, and survival function below, where parameters \\(\\alpha\\) and \\(\\beta\\) can be modified to see the effects on the respective functions.")),
                          
                          hr(),
                          tags$br(),
                          
                          sidebarLayout(
                            sidebarPanel(
                              tags$h4("Distribution Parameters"),
                              fluidRow(
                                column(12,sliderInput(inputId="unif_params",
                                                      label=withMathJax(sprintf("Parameters \\(\\alpha)\\) and \\(\\beta)\\)")),
                                                      min = 0, max = 10, value = c(3,7)
                                ))),
                              width = 3
                            ),
                            mainPanel(
                              column(4,plotOutput("uniform_pdf", height = "300px")),
                              column(4,plotOutput("uniform_cdf", height = "300px")),
                              column(4,plotOutput("uniform_survival", height = "300px")),
                              width = 9
                            )
                          ),
                          
                          tags$br(),
                          hr(),
                        
                        
                          h1("Non-parametric Estimation of the Survival Function"),
                          p(withMathJax(sprintf("According to Madigan (cite Madigan), assuming that every subject follows the same survival function allows for straightforward approaches for estimating the survival function \\(S(t)\\). Furthermore, it is important to determine if parametric assumptions can be made for estimating the survival distribution. In our case, we will not make any parametric assumptions and focus on the nonparametric Kaplan-Meier Estimator to not overextend the scope of this project."))),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("It is important to emphasize that if no subjects are censored, then a direct non-parametric estimator of the survival function would be to use the empirical cdf \\(F(t)\\) to yield \\(S(t)\\) through \\(S(t) = 1 - F(t)\\), similar to when we first outlined how the survival function was derived at the beginning of this section. Now if we have multiple right-censored subjects, then using the empirical cdf is no longer an option and simply discarding those subjects with incomplete information would reduce the sample size, which is where using the Kaplan-Meier product limit estimator becomes relevant"))),
                          
                          h2("Kaplan Meier Survival Curves"),
                          
                          p(withMathJax(sprintf("Kaplan-Meier survival curves can be interpreted as the probability of surviving a certain length od time, where time is considered in multiple small intervals (CITE Estimation of Survival Probabilities.Analysing survival data from clinical trials and observational studies Marubini, VALSECCHI). According to Goel, Khanna, and Kishore (cite year), there are three primary analysis assumptions that need to be considered."))),
                          
                          tags$ul(withMathJax(
                            tags$br(),
                            tags$li(tags$b("Assumption I: "), sprintf("Any subjects with censored survival times are assumed to have the same survival probabilities as subjects who are not censored.")),
                            tags$br(),
                            tags$li(tags$b("Assumption II: "), sprintf("Time of recruitment does not affect survival probability, or in other words time of recruitment in the study does not affect survival probability.")),
                            tags$br(),
                            tags$li(tags$b("Assumption III: "), sprintf("Given a discrete time-scale when measuring events on subjects, events are assumed to happen at the specificed time.")),
                            tags$br()
                          )
                          ),
                          
                          p(withMathJax(sprintf("It is important to emphasize assumption III, since the the theoretical survival function is actually continuous but the time interval is discrete Thus, it intuitively follows that shorter time intervals between measurements on subjects are beneficial in more accurately estimate survival (CITE GOEL, KHANNA, KISHORE). Calculating the Kaplan-Meier Estimate invloved evaluating probabilities of an event at a given time-point, where these each following probability is multiplied with the previous probability to yield the final survival estimate. According to GOEL et. al (CITE GOEL, KHANNA, and KISHORE), the survival probability at any given time point can be evaluated as $$S_{t} =  \\frac{\\text{ Number of at risk subjects at time interval t - Number of subjects for which the event occured in time interval t}}{\\text{Number of at risk subjects in time interval t.}}.$$ Now, to calculate the value of the survival of the next time interval \\(S_{t+1}\\), we have  $$S_{t+1} = S_{t} \\cdot (\\frac{\\text{ Number of at risk subjects at time interval t+1 - Number of subjects for which the event occured in time interval t+1}}{\\text{Number of at risk subjects in time interval t+1.}}).$$ Thus, survival probabilities are calculated starting with \\(S_{0} = 1\\), and then the survival probability of each sucessive time interval is then computed by the (second) formula above. One way to understand the calculations of survival probabilities with real data would be using a table, as we can see below, where we have example data by Sullivan (cite Sullivan, Boston University Source) as "))),
                          
                          h3("Kaplan-Meier Survival Probility Life Table Example"),
                          p(withMathJax(sprintf("GO THROUGH CALCULATION OF FIRST 3 SURVIVAL PROBABILITIES (RIGHTMOST COLUMN AND THEN REFER TO TABLE."))),
                          DT::dataTableOutput("mytable"),
                          
                          h3("Kaplan-Meier Plot Example"),
                          
                          p(withMathJax(sprintf("TALK ABOUT HOW DATA ARE ACTUALLY DIFFERENT FORMAT AND SHOWCASE (AT LEAST PART) OF THE PREVIOUS DATASET IN THE FORMAT USED TO MAKE KM PLOT"))),
                          
                          
                          h1("The Hazard Function"),
                          h3("bla"),
                          
                          h1("Cumulative Hazard"),
                          h3("bla"),
                 ),

###################################################################
# Burning-In Tab
###################################################################
                 tabPanel(title = "A Covid Related Application Example",
                          
                          h1("Exploratory data analysis (another way to incorporate concepts from our methods class -->)"),
                          h3("bla"),
                          h1("kaplan Meier Curves"),
                          h3("bla"),
                 ),

###################################################################
# References Tab
###################################################################
                 tabPanel(title = "References",
                          h1("References"),
                          h3("bla"),
                          h1("Aknowledgement"),
                          h3("bla"),
                          h1("R Packages"),
                          h3("bla"),
                          
            ),
            fluid=FALSE                 
)


###################################################################
#Server
###################################################################
server <- function(input, output) {
  
  
  ###################################################################
  #Censoring Plot
  ###################################################################
  subject_number <- c(1:12)
  number_of_years <- c(4,3,7,8,10,10,3,5,1,3,9,2)
  censor_indicator <- as.factor(c("Event Occured","Censored","Event Occured","Event Occured","Censored","Censored","Event Occured","Censored","Censored","Event Occured","Event Occured","Censored"))
  
  df <- data.frame(subject_number, number_of_years,censor_indicator)
  
  censor_plot <- ggplot() +
    geom_bar(data=df, aes(x=subject_number,y=number_of_years), stat='identity', width=0.2) +
    geom_point(data=df, aes(x=subject_number,y=number_of_years, color=censor_indicator), size=6) +
    scale_x_continuous(labels=as.character(subject_number),breaks=subject_number) +
    coord_flip() + 
    theme_minimal() +
    labs(title = "Right-Censored Survival Data",
         color="Censoring") +
    xlab("Subject Number") + 
    ylab("Number of Years")
  
  output$censor_plot <- renderPlotly({ggplotly(censor_plot)})

  ###################################################################
  # Normal PDF, CDF, and Survival Fct
  ###################################################################
  
  # Normal pdf
  normal_pdf <- function(x) {
    dnorm(x, mean=input$mu_norm, sd=input$sdev_norm)
  }
  
  output$normal_pdf<-renderPlot({
    ggplot(data.frame(x = c(-10, 10)), aes(x = x)) +
      stat_function(fun = normal_pdf) +
      theme_few() +
      ggtitle("Normal PDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Normal cdf
  normal_cdf <- function(x) {
    pnorm(x, mean=input$mu_norm, sd=input$sdev_norm)
  }
  
  output$normal_cdf<-renderPlot({
    ggplot(data.frame(x = c(-10, 10)), aes(x = x)) +
      stat_function(fun = normal_cdf) +
      theme_few() +
      ggtitle("Normal CDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Normal Survival Fct
  normal_survival <- function(x) {
    1 - pnorm(x, mean=input$mu_norm, sd=input$sdev_norm)
  }
  
  output$normal_survival<-renderPlot({
    ggplot(data.frame(x = c(-10, 10)), aes(x = x)) +
      stat_function(fun = normal_survival ) +
      theme_few() +
      ggtitle("Normal Survival Function") +
      xlab("x") + 
      ylab("density")
  })
  
  ###################################################################
  # Exponential PDF, CDF, and Survival Fct
  ###################################################################
  
  # Exponential pdf
  exponential_pdf <- function(x) {
    dexp(x, rate=input$lambda_exp)
  }
  
  output$exponential_pdf<-renderPlot({
    ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = exponential_pdf) +
      theme_few() +
      ggtitle("Exponential PDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Exponential cdf
  exponential_cdf <- function(x) {
    pexp(x, rate=input$lambda_exp)
  }
  
  output$exponential_cdf<-renderPlot({
    ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = exponential_cdf) +
      theme_few() +
      ggtitle("Exponential CDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Exponential Survival Fct
  exponential_survival <- function(x) {
    1 - pexp(x, rate=input$lambda_exp)
  }
  
  output$exponential_survival<-renderPlot({
    ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = exponential_survival) +
      theme_few() +
      ggtitle("Exponential Survival Function") +
      xlab("x") + 
      ylab("density")
  })
  
  ###################################################################
  # Uniform PDF, CDF, and Survival Fct
  ###################################################################
  
  # Uniform pdf
  uniform_pdf <- function(x) {
    dunif(x, min=min(input$unif_params), max=max(input$unif_params))
  }
  
  output$uniform_pdf<-renderPlot({
    ggplot(data.frame(x = c(0, 10)), aes(x = x)) +
      stat_function(fun = uniform_pdf) +
      theme_few() +
      ggtitle("Uniform PDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Uniform cdf
  uniform_cdf <- function(x) {
    punif(x, min=min(input$unif_params), max=max(input$unif_params))
  }
  
  output$uniform_cdf<-renderPlot({
    ggplot(data.frame(x = c(0, 10)), aes(x = x)) +
      stat_function(fun = uniform_cdf) +
      theme_few() +
      ggtitle("Uniform CDF") +
      xlab("x") + 
      ylab("density")
  })
  
  
  # Uniform Survival Fct
  uniform_survival <- function(x) {
    1 - punif(x, min=min(input$unif_params), max=max(input$unif_params))
  }
  
  output$uniform_survival<-renderPlot({
    ggplot(data.frame(x = c(0, 10)), aes(x = x)) +
      stat_function(fun = uniform_survival) +
      theme_few() +
      ggtitle("Uniform Survival Function") +
      xlab("x") + 
      ylab("density")
  })
  
  
  ###################################################################
  #  Boston University Kaplan-Meier Data Visualization Example
  ###################################################################
  
  time <- c(0,1,2,3,5,6,9,10,11,12,13,14,17,18,19,21,23,24)
  N_t <- c(20,20,19,18,17,16,15,14,13,12,11,10,9,7,6,5,4,3)
  D_t <- c(0,1,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,0)
  C_t <- c(0,0,1,0,0,1,1,1,1,1,1,0,1,1,1,1,0,3)
  S_t <- c(1,0.95,0.95,0.897,0.844,0.844,0.844,0.844,0.844,0.844,0.844,0.760,0.676,0.676,0.676,0.676,0.507,0.507)
  
  output$mytable = DT::renderDataTable({
    df <- data.frame(time, N_t, D_t, C_t, S_t)
    names(df) <- c("Time in Years", "Number at Risk Nt", "Number of Deaths Dt", "Number Censored Ct", "Survival Probility St")
    df
  }, options = list(pageLength = 25, info = FALSE, rownames= FALSE, class="compact"))
  
  
  
}

###################################################################
#App Construction
###################################################################
shinyApp(server = server, ui = ui)
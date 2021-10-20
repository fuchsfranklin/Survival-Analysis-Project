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
library(survival)
library(survminer)
theme_set(theme_bw())

###################################################################
# UI
###################################################################
ui <- navbarPage(theme = shinytheme("cerulean"),
                 
                 title = "An Introduction and Application of Basic Surival Analysis Ideas",
                 
###################################################################
# Introductory Tab
###################################################################                
                 tabPanel(withMathJax(),
                          
                          title = "Understanding Survival Analysis",
                          
                          h1("Understanding Survival Analysis Basics"),
                          
                          hr(),
                          
                          h4("Tiffany Hsieh, Franklin Fuchs, Bowen Chen"),
                          h4("Methods in Biostatistics I - Final Project"),
                          
                          hr(),
                          
                          h2("Project Motivation"),
                          p("Given the backgrounds of our project members in programming and epidemiology, we aimed to create a project where we could use combine concepts and ideas we learned in our methods course with our respective skillsets to adress a real-world problem from multiple angles. Given the availability of a publicly available covid-related survival dataset which we wanted to further explore, we aimed to expand on concepts we learned in class by outlining several very basic survival analysis concepts in a non-mathematical manner and then applying those concepts to the aforementioned dataset. Finally, we present our results through this R Shiny web-application in order provide a presentation and deliverable format of results that is compact, portable, interactive, and hopefully more fun than a traditional report. From the biostatistics final exam/project instruction document, the suggested topics we thus focus on are the following."),
                          
                          tags$ul(
                            tags$br(),
                            tags$li(tags$b("(1)"), sprintf("Take any concept that is taught in class and expand on it in terms of applications, data visualization, new pedagogical approaches, methodological questions, simulations.")),
                            tags$br(),
                            tags$li(tags$b("(2)"), sprintf("Biostatistical evaluation of evidence of efficacy and safety COVID-19 vaccines.")),
                            tags$br(),
                            tags$li(tags$b("(3)"), sprintf("Analysis of any data set with emphasis on biostatistical methods.")),
                          ),

                          h2("Survival Analysis Background"),
                          p("If we are interested in modeling an outcome variable that can be described as time until an event occurs, the collection of statistical methods that fall under survival analysis are the most appropriate [CITE APPROPRIATE]. The most obvious example of this would be time until death, but the event of interest does not necessarily need to be negative. Time to full recovery from an intervention in a clinical trial would be an example of a positive outcome, or considering treatment duration as an outcome would be a more neutral example. The response variable is often referred to as survival time, failure time, or event time, and is most often continuous in theory."),
                          
                          h2("Introductory Terms"),
                          p("Since we aim to introduce the basic survival analysis concepts without in applied manner and using the statistical concepts we covered in our methods class a the basis, we start by clarifying several basic terms and issues in the following section."),
                          tags$ul(
                            tags$li(withMathJax(tags$b("Survival Time Response", style = "font-size:18px;"),
                                                tags$br(),
                                                sprintf("The outcome variable is known as survival time response and is usually continuous. Intuitively, it also makes sense that the survival time response is always greater than or equal to zero. It is important to emphasize that we might know the exact time of the event for some subjects in our analysis, we might have incomplete information on the survival time response for other subjects. In the case where have some but not all information regarding the survival time response, we term the subjects or reponses as censored. According to Madigan [CITE MADIGAN LECTURE], these censored are the primary reason why traditional regression methods cannot by utilized in the context of survival analysis problems. Regarding time to response, the probability of surviving past a given time point is equally as or often more imporant than determining the expected event time itself, for which methods of survival analysis and the survival function (similar to how we have learned about it in class) can be estimated and used to yield more effective results about understanding the failure process than linear regression [CITE MADIGAN LECTURE]."))),
                            tags$li(withMathJax(tags$b("Censoring", style = "font-size:18px;"),
                                                tags$br(),
                                                sprintf("As mentioned in the previous subsection, a censored observation is a incompletely determined observation for a single subject, where censoring occurs if we have some information of the event time of our subject but do not know about the exact event time. According to Madigan, [CITE MADIGAN LECTURE], the censoring mechanism and survival mechanism must be independently cosidered to be able to conduct survival analysis. The most common type of censoring is right censoring, which is when survival time is incomplete on the right side of the follow-up period. For an intuitive visualization of a collection of data where some some subjects have right-censored observations and some subjects do not, refer to the interactive plot below (where elements can be clicked on for further information). The primary reasons why right-censoring occur are either withdrawal from the study, loss to follow-up during the study, or a subject not experiencing the event within the study timeframe. Although there exist other types of censoring, they are out of the scope of this project."), 
                                                tags$br(),
                                                tags$br(),
                                                tags$ul(tags$b("Types of Right-Censoring", style = "font-size:16px;"),
                                                        tags$br(),
                                                        tags$br(),
                                                        tags$li(tags$b("Type I: "), sprintf("Type I censoring can be either fixed or random. Fixed type I censoring happens when the study ends after a certain amount of time and everyone for whom the event was not observed is censored at the time of the study ending [CITE MADIGAN LECTURE]. Random Type I censoring also occurs when a study is designed to end after a certain amount of time, but different censored subjects have differing censoring times, meaning that not all subjects are censored at the same point in time. Thus, the dropout is considered random for type I censoring.")),
                                                        tags$br(),
                                                        tags$li(tags$b("Type II:"), sprintf("Type II censoring is when a study ends after a pre-defined number of events are observed, and so all subjects for which the event was not observed until the study ending are censored [CITE MADIGAN LECTURE]. Thus, type II censoring is considered fixed.")),
                                                        tags$br(),
                                                ),
                                                sprintf("Below we have an interactive visualization of the survival time outcome for twelve subjects, where the blue circles experienced the event of interest at that point in time and the red circles were censored at that point in time. We observe that subjects 2, 8, 9, and 12 were censored prematurely and subjects 5 and 6 were censored due to the study ending at the time point of 10 years in our example. In this example and further throughout the part of the project that is about understanding several basic survival analysis concepts, we focus exclusively on right-censoring as mentioned earlier.")),
                                                tags$br(),
                                                hr(),
                                                plotlyOutput("censor_plot", width = "90%"),
                                                tags$br(),
                                                hr()),
                          ),
                          tags$br(),
                          
                          
                          
                          h1("The Survival Function"),
                          h2("Intruduction"),
                          withMathJax(sprintf("From our methods class, we know that given a valid probability density function, we then have cumulative distribution function of random variable \\(X\\) defined as follows. $$F(x) = P(X \\leq x).$$ Conveneniently, we also know that given the cumulative distribution function, we can then yield the associated survival function as follows. $$S(x) = P(X > x) = 1 - F(x).$$")),
                          
                          
                          withMathJax(h2("")),
                          tags$ul(withMathJax(
                                    tags$br(),
                                    tags$li(sprintf("For a time value \\(x\\), the survival function yields the probability of surviving past time \\(x\\), which puts the calculation that a probabilty is larger than some value \\(x\\) intuitively in a survival context.")),
                                    tags$br(),
                                    tags$li(sprintf("The survival function is a non-increasing function that ranges from \\(t = 0\\) to \\(\\infty \\), where \\(S(t)=1\\) at \\(x=0\\) and \\(S(t) \\rightarrow 0 \\) as \\(t \\rightarrow \\infty \\). These values intuitively make sense, as the probability of surviving past time 0 being 1 is a precondition for the survival function to be calculated for a given subject, and as time goes to infinity, the respective survival probility goes to zero.")),
                                    tags$br(),
                                    tags$li(sprintf("In theory and as we have seen in class, the survival function is continuous, but for practical applications time scale that is observed is most often discrete, such as in terms of minutes, hours, or days [CITE MADIGAN LECTURE]. This is because continuous monitoring of subjects is often too costly, too invasive, or simply not viable within the context of the study/participants.")),
                                    tags$br()
                                  )
                          ),
                          
                          withMathJax(sprintf("Now, it would be quite interesting to consider the behaviour of the survival function, and since it is directly dependent on the cumulutaive distribution function and probability density function, this allows for some interesting survival function behaviour visualizations in the context of distributions we learned about in class, such as for the normal, exponential, and uniform distributions. Therefore, what we do in the following section is plot the probability density function, cumulative distribution function, and survival function next to each other with modifiable parameters, so that changes in density and parameters can be more intuitively be seen and understood for the respective distribution and survival functions.")),
                          
                          tags$br(),
                          tags$br(),
                          
                          h2("Visualization of \\(S(x)\\) for Several Distributions"),
                          
                          h3("The Normal Distribution"),
                          withMathJax(sprintf("As we learned in class, the normal distribution is one of the most important distributions in statistics, as many models and tests assume and require normality of the data in order to be used. Additionally, we have also seen the importance of the normal distribution in the context of the central limit theorem, where sample means regardless of their underlying distribution behave approximately normal, or standard normal when standardized. For a random variable \\(X \\sim Normal(\\mu,\\sigma^2)\\) with mean \\(\\mu\\) and standard deviation \\(\\sigma\\), we have the normal pdf as $$f(x) = \\frac{e^{-(x-\\mu)^{2}/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ Integrating the normal distribution yields the normal cumulative distribution function, where it is important to mention that given that the integral does not have a closed form, and so it is usually numerically evaluated. Since it does not have a closed form, we will denote the cumulative distribution in terms of the integral of the probability density function as $$F(x) = \\int_{-\\infty}^{x}\\frac{e^{-(x-\\mu)^{2}/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ It then follows that the survival function is not closed form, and so it is also expressed in terms of an integral as $$S(x) = 1- \\int_{-\\infty}^{x}\\frac{e^{-(x-\\mu)^{2}/(2\\sigma^2)}}{\\sigma\\sqrt{2\\pi}}.$$ Below, we have each of the previously outlined functions plotted, where parameters \\(\\mu\\) and \\(\\sigma\\) can be modified on a slider to see the respective visual changes in the density, cumulative distribution, and most importantly survival function.")),
                         
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
                          
                          withMathJax(sprintf("Another distribution we were introduced to in class is the exponential distribution, where we first looked at an exponential random variable in the context of time in years from diagnosis until death of persons with a specific kind of cancer in our lecture. In fact, the expontial distribution is often used to model the time until a specific event occurs [CITE ROSS PAGE 209]. For a random variable \\(X \\sim Exponential(\\lambda)\\) with parameter \\(\\lambda\\), we then have the associated pdf for values of \\(x \\geq 0 \\) as $$f(x) = \\lambda e^{-\\lambda x}.$$ Integrating the probability density function yields the exponential cumulative distribution function as $$F(x) = \\int_{0}^{x} \\lambda e^{-\\lambda t}dt = 1 - e^{-\\lambda x}.$$ The associated survival function can be found by taking subtracting the cdf from 1 as $$S(x) = 1 - F(x) = e^{-\\lambda x}.$$ Below, we again have each of the previously outlined functions plotted, where the parameter \\(\\lambda\\) can be modified on a slider to see the respective visual changes in the density, cumulative distribution, and most importantly survival function.")),
                          
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
                          
                          withMathJax(sprintf("The final distribution from class that we consider is the uniform distribution, which has the same density value for every value over a given range, and so for a random variable \\(X \\sim U(\\alpha , \\beta)\\) for values of \\(x\\) in the interval \\(\\alpha < x < \\beta \\), we then have the associated probability density function as $$f(x) = \\frac{1}{\\beta - \\alpha}.$$ Integrating the probability density function yields the uniform cumulative distribution function as $$F(x) =  \\frac{x-\\alpha}{\\beta-\\alpha}.$$ Finally, the associated survival function can be found by subtracting the cumulative distribution from 1 as $$S(x) = 1 - F(x) = \\frac{\\beta-x}{\\beta-\\alpha}.$$ Below, we again have each of the previously outlined functions plotted, where the interval-determining parameters \\(\\alpha\\) and \\(\\beta\\) can be modified on a slider to see the respective visual changes in the density, cumulative distribution, and most importantly survival function.")),
                          
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
                        
                        
                          h1("Non-Parametric Estimation of the Survival Function"),
                          p(withMathJax(sprintf("Assuming that every subject follows the same survival function allows for straightforward approaches for estimating the survival function \\(S(t)\\) [CITE MADIGAN LECTURE]. Additionally, it is important to determine if parametric assumptions can be made for estimating the survival function. For the sake of limiting the scope of this project, we will not make any parametric assumptions and focus on one primary non-parametric estimation approach."))),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("It is important to emphasize that if no subjects are censored, then a direct non-parametric estimator of the survival function would be to use the empirical cdf \\(F(t)\\) to yield \\(S(t)\\) through \\(S(t) = 1 - F(t)\\), exactly as we outlined the survival function be derived from the distribution function in theory as discussed at the beginning of this section. If we have multiple right-censored subjects, then using the empirical cdf is no longer an option and simply discarding those subjects with incomplete information would reduce the sample size. oftentimes to a problematic extent, which is where Kaplan-Meier product limit estimator as outlined in the next section becomes relevant."))),
                          
                          h2("Kaplan Meier Survival Curves"),
                          
                          p(withMathJax(sprintf("From an applied perspective, kaplan-meier survival curves can be interpreted as the probability of surviving a certain length of time, where time is considered in multiple small intervals [CITE Estimation of Survival Probabilities.Analysing survival data from clinical trials and observational studies Marubini, VALSECCHI]. According to Goel, Khanna, and Kishore (cite year), there are three primary assumptions that need to be considered regarding Kaplan Meier as follows."))),
                          
                          tags$ul(withMathJax(
                            tags$br(),
                            tags$li(tags$b("Assumption I: "), sprintf("Any subjects with censored survival times are assumed to have the same survival probabilities as subjects who are not censored.")),
                            tags$br(),
                            tags$li(tags$b("Assumption II: "), sprintf("Time of recruitment does not affect survival probability, or in other words when someone was recruited into the study does not affect the survival probability.")),
                            tags$br(),
                            tags$li(tags$b("Assumption III: "), sprintf("Given a discrete time-scale when measuring events on subjects, events are assumed to happen at the specificed time. It is important to mention that event though we know the theoretical survival function to be continuous, the actual time intervals are discrete, and so it follows that shorter time intervals between measurements on subjects are better approximate the survival function and increase accuracy [CITE GOEL, KHANNA, KISHORE].")),
                            tags$br()
                          )),
                          
                          
                          p(withMathJax(sprintf("Given the survival probability of \\(S(0)=1\\) as we outlined in the Properties of \\(S(x)\\) section, we have for"))),
                                                
                          # tags$ul(withMathJax(
                          #   tags$li(tags$b("\\(N_{t+1}=\\)  "), sprintf("Number of at risk subjects at time interval \\(t+1\\)")),
                          #   tags$li(tags$b(" \\(N_{t+1}=\\) "), sprintf("Number of subjects for which the event occured in time interval \\(t+1\\)")),
                          # )),
                          
                          tags$ul(withMathJax(
                            tags$li(tags$b("\\(N_{t+1}=\\)"), sprintf("Number of at risk subjects at time interval \\(t+1\\)")),
                            tags$li(tags$b("\\(D_{t+1}=\\)"), sprintf("Number of subjects for which the event occured in time interval \\(t+1\\)"))
                          )),
                          
                          p(withMathJax(sprintf("Thus, for any time point \\(t+1\\) where \\(t>0\\) we can evaluate the survival probability as $$S_{t+1} = S_{t} \\cdot \\frac{N_{t+1} - D_{t+1}}{N_{t+1}}.$$"))),
                          
                          tags$br(),
        
                          h3("Kaplan-Meier Survival Probility Calculation and Curve Plotting"),
                          
                          p(withMathJax(sprintf("One way to better understand the calculations of survival probabilities is with an actual example, where we will calculate the first three probilities for the life table provided by Sullivan [cite Sullivan, Boston University Source] as to illustrate survival probability calculations to set up the Kaplan-Meier Curve. It is important to mention that data for analyses (including survival analysis) have a format where each row corresponds to one individual, but for the sake of this project a life table is more illustrative"))),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("Given \\(S(0)=1\\) and all 20 subjects at risk at time 0, we observe for the next time row and time point that there was one death, so we now have $$S_{1} = S_{0} \\cdot \\frac{N_{1} - D_{1}}{N_{1}} = 1 \\cdot \\frac{20 - 1}{20} = 0.95.$$ Now for the second time point we observe that nobody died, but someone was censored, but this does not change the survival probability because the censored subject is accounted for since they are not subtracted in the numerator. Thus, we have the following survival probability as $$S_{2} = S_{1} \\cdot \\frac{N_{2} - D_{2}}{N_{2}} = 0.95 \\cdot \\frac{19 - 0}{19} = 0.95.$$ To calculate the survival probability for the third year in our life table, we again observe that one person died, and so we have $$S_{3} = S_{2} \\cdot \\frac{N_{3} - D_{3}}{N_{3}} = 0.95 \\cdot \\frac{18 - 1}{18} = 0.897.$$ Thus, for every consequent row the survival probability can be calculated, which yields the rightmost column of the table below."))),
                          
                          tags$br(),
                          hr(),
                          
                          h4("Life Table Data Example"),
                          DT::dataTableOutput("mytable"),
                          
                          hr(),
                          tags$br(),
                          
                          p(withMathJax(sprintf("Finally, plotting the probabilities over the time in years yields the following (interactive) plot known as the Kaplan-Meier Survival Curve. On the x-axis we have time, and on the y-axis we have the respective survival probabilities which we calculated for in table. The ticks or symbols on the curve indicate either when a subject was censored or when an event occured for a subject."))),
                          
                          tags$br(),
                          hr(),
                          plotlyOutput("survplot", height = "500px"),
                          tags$br(),
                          hr(),
                          
                          h1("The Hazard Function"),
                          h3("bla"),
                          
                          h1("Cumulative Hazard"),
                          h3("bla"),
                 ),

###################################################################
# Burning-In Tab
###################################################################
                 tabPanel(title = "A Covid Related Application Example",
                          
                          h1("Exploratory data analysis"),
                          h3("bla"),
                          h1("kaplan Meier Curves"),
                          h3("bla"),
                 ),

###################################################################
# References Tab
###################################################################
                 tabPanel(title = "References and Member Aknowledgements",
                          
                          
                          h1("Member Aknowledgements"),
                          p("We had the following division of work among our members."),
                          tags$ul(withMathJax(
                            tags$li(tags$b("Tiffany: "), sprintf("Tiffany found, cleaned, explored, applied survival analysis to, and interpreted results related to the covid-related dataset.")),
                            tags$li(tags$b("Franklin: "), sprintf("Franklin designed and programmed the web-application, created the interactive plots, outlined the basic basic theory up until Kaplan-Meier.")),
                            tags$li(tags$b("Bowen: "), sprintf("Outlined the theory regarding the hazard function, cumulative hazard, and chi-square test, and related it to the analyses of Tiffany."))
                          )),
                          
                          h1("References"),
                          h4("Papers"),
                          tags$ul(
                            tags$li("S. Ross (2014), Introduction to Probability Models (Eleventh Edition). Academic Press. https://doi.org/10.1016/B978-0-12-407948-9.00012-8")
                          ),
                          h4("Books"),
                          tags$ul(
                            tags$li("S. Ross (2014), Introduction to Probability Models (Eleventh Edition). Academic Press. https://doi.org/10.1016/B978-0-12-407948-9.00012-8")
                          ),
                          h4("Websites"),
                          tags$ul(
                            tags$li("Sullivan, L. Survival Analysis. Boston University School of Public Health, https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_survival/BS704_Survival_print.html Accessed 10 October 2021."),
                            tags$li("Madigan D. Statistics W2025/W4025: Methods in Applied Statistics, http://www.stat.columbia.edu/~madigan/W2025/ Accessed 5 October 2021.")
                          ),
                          
                          h4("R Packages"),
                          tags$ul(withMathJax(
                            tags$li(sprintf("shiny")),
                            tags$li(sprintf("shinythemes")),
                            tags$li(sprintf("ggplot2")),
                            tags$li(sprintf("transformr")),
                            tags$li(sprintf("tidyr")),
                            tags$li(sprintf("plotly")),
                            tags$li(sprintf("ggthemes")),
                            tags$li(sprintf("DT")),
                            tags$li(sprintf("survival")),
                            tags$li(sprintf("survminer"))
                          )),
                          
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
  #  Boston University Kaplan-Meier Table Example
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
  }, options = list(pageLength = 25, info = FALSE, rownames= FALSE, class="compact"), rownames= FALSE)
  
  
  ###################################################################
  #  Boston University Kaplan-Meier Curve Example
  ###################################################################
  
  df_mod <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("time", "status")
  colnames(df_mod) <- x
  
  df_mod[nrow(df_mod) + 1,] = c(1,1)
  df_mod[nrow(df_mod) + 1,] = c(2,0)
  df_mod[nrow(df_mod) + 1,] = c(3,1)
  df_mod[nrow(df_mod) + 1,] = c(5,1)
  df_mod[nrow(df_mod) + 1,] = c(1,0)
  df_mod[nrow(df_mod) + 1,] = c(9,0)
  df_mod[nrow(df_mod) + 1,] = c(10,0)
  df_mod[nrow(df_mod) + 1,] = c(11,0)
  df_mod[nrow(df_mod) + 1,] = c(12,0)
  df_mod[nrow(df_mod) + 1,] = c(13,0)
  df_mod[nrow(df_mod) + 1,] = c(14,1)
  df_mod[nrow(df_mod) + 1,] = c(17,1)
  df_mod[nrow(df_mod) + 1,] = c(17,0)
  df_mod[nrow(df_mod) + 1,] = c(18,0)
  df_mod[nrow(df_mod) + 1,] = c(19,0)
  df_mod[nrow(df_mod) + 1,] = c(21,0)
  df_mod[nrow(df_mod) + 1,] = c(23,1)
  df_mod[nrow(df_mod) + 1,] = c(24,0)
  df_mod[nrow(df_mod) + 1,] = c(24,0)
  df_mod[nrow(df_mod) + 1,] = c(24,0)
  
  lsurv <- survfit(Surv(time, status) ~ 1, df_mod)
  ggsurvplot(lsurv, data = df_mod, conf.int = FALSE)
  
  survplot <- ggsurvplot(lsurv, data = df_mod, 
                         conf.int = FALSE,
                         title = "Kaplan-Meier Survival Curve Example Curve",
                         legend = "none",
                         palette = c("#2E9FDF"),
                         ggtheme = theme_bw() 
                         )
  
  output$survplot <- renderPlotly({ggplotly(survplot[[1]])})
  
}

###################################################################
#App Construction
###################################################################
shinyApp(server = server, ui = ui)
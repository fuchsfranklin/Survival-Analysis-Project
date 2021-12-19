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
library(dplyr)
library(tidyverse)
library(haven)
library(gt)
library(gtsummary)
library(knitr)

theme_set(theme_bw())

# One time load and clean of covid data
data_R <- read.csv("COVID_VEtrial_practicedata_primarystage1.csv")
covid<-data_R[!is.na(data_R$BbindSpike)&!is.na(data_R$BbindRBD)&!is.na(data_R$BbindN)&!is.na(data_R$Day57bindSpike)&!is.na(data_R$Day57bindRBD)&!is.na(data_R$Day57bindN),]
covid$Trt <- factor(covid$Trt, levels = c(0, 1), labels = c("Not Vaccinated", "Vaccinated"))
covid$infect <- factor(covid$EventIndPrimaryD57, levels = c(0, 1), labels = c("Not Infected", "Infected"))

#data_sub <- covid[,c("X","Trt","EthnicityHispanic","Black","Asian","Sex","BMI","Age","EventTimePrimaryD57","EventIndPrimaryD57","infect","BbindSpike")]
data_sub <- covid[,c("X","Trt","EthnicityHispanic","Black","Asian","NatAmer","PacIsl","Multiracial","Other","Sex","BMI","Age","EventTimePrimaryD57","EventIndPrimaryD57","infect","BbindSpike")]

data_sub2 <- data.frame(data_sub)

data_sub2$Sex<-ifelse(data_sub2$Sex==1,"Male","Female")
data_sub2$EthnicityHispanic<-ifelse(data_sub2$EthnicityHispanic==1,"Hispanic","Non-Hispanic")
data_sub2$Age<-as.numeric(data_sub2$Age)
data_sub2$Age_c <- cut(data_sub2$Age, breaks = c(0,30,40,50 ,65,Inf))
data_sub2$White <- ifelse(data_sub$Black==0,1,0) 

colnames(data_sub2) <- c("X","Trt","Hispanic","Black","Asian","Native","PacificIslander","Multiracial","Other","Sex","BMI","Age","EventTimePrimaryD57","EventIndPrimaryD57","infect","BbindSpike","Age_c","White")

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
                          h5("Methods in Biostatistics Final Project"),
                          
                          hr(),
                          
                          navlistPanel(widths = c(2, 10), fluid = FALSE,
                            tabPanel("1. Project Motivation",
                              h1("Project Motivation"),
                              p("Given the backgrounds of our project members in programming and epidemiology, we aimed to create a project where we could combine concepts and ideas we learned in our methods course with our respective skillsets to adress a real-world problem from multiple angles. Given the availability of a publicly available covid-related survival dataset which we wanted to further explore, we aimed to expand on concepts we learned in class by outlining several very basic survival analysis concepts in a non-mathematical manner and then applying those concepts to the aforementioned dataset. Finally, we present our results through this R Shiny web-application in order provide a presentation and deliverable format of results that is compact, portable, interactive, and hopefully more fun than a traditional report. From the biostatistics final exam/project instruction document, the suggested topics we thus focus on are the following."),
                              
                              tags$ul(
                                tags$br(),
                                tags$li(tags$b("(1)"), sprintf("Take any concept that is taught in class and expand on it in terms of applications, data visualization, new pedagogical approaches, methodological questions, simulations.")),
                                tags$br(),
                                tags$li(tags$b("(2)"), sprintf("Biostatistical evaluation of evidence of efficacy and safety COVID-19 vaccines.")),
                                tags$br(),
                                tags$li(tags$b("(3)"), sprintf("Analysis of any data set with emphasis on biostatistical methods.")),
                              ),
                            ),
                            
                            tabPanel("2. Survival Analysis Background",
                              h1("Survival Analysis Background"),
                              p("If we are interested in modeling an outcome variable that can be described as time until an event occurs, the collection of statistical methods that fall under survival analysis are the most appropriate to consider (Clark et al., 2003). The most obvious example of this would be time until death, but the event of interest does not necessarily need to be negative. Time to full recovery from an intervention in a clinical trial would be an example of a positive outcome, or considering treatment duration as an outcome would be a more neutral example. The response variable is often referred to as survival time, failure time, or event time, and is most often continuous in theory."),
                              
                              h2("Introductory Terms"),
                              p("Since we aim to introduce the basic survival analysis concepts without in applied manner and using the statistical concepts we covered in our methods class a the basis, we start by clarifying several basic terms and issues in the following section."),
                              tags$ul(
                                tags$li(withMathJax(tags$b("Survival Time Response", style = "font-size:18px;"),
                                                    tags$br(),
                                                    sprintf("The outcome variable is known as survival time response and is usually continuous. Intuitively, it also makes sense that the survival time response is always greater than or equal to zero. It is important to emphasize that we might know the exact time of the event for some subjects in our analysis, we might have incomplete information on the survival time response for other subjects. In the case where have some but not all information regarding the survival time response, we term the subjects or reponses as censored. According to Madigan (Madigan, 2013), these censored are the primary reason why traditional regression methods cannot by utilized in the context of survival analysis problems. Regarding time to response, the probability of surviving past a given time point is equally as or often more imporant than determining the expected event time itself, for which methods of survival analysis and the survival function (similar to how we have learned about it in class) can be estimated and used to yield more effective results about understanding the failure process than linear regression (Madigan, 2013)."))),
                                tags$li(withMathJax(tags$b("Censoring", style = "font-size:18px;"),
                                                    tags$br(),
                                                    sprintf("As mentioned in the previous subsection, a censored observation is a incompletely determined observation for a single subject, where censoring occurs if we have some information of the event time of our subject but do not know about the exact event time. The censoring mechanism and survival mechanism must be independently cosidered to be able to conduct survival analysis. The most common type of censoring is right censoring, which is when survival time is incomplete on the right side of the follow-up period. For an intuitive visualization of a collection of data where some some subjects have right-censored observations and some subjects do not, refer to the interactive plot below (where elements can be clicked on for further information (Madigan, 2013). The primary reasons why right-censoring occur are either withdrawal from the study, loss to follow-up during the study, or a subject not experiencing the event within the study timeframe. Although there exist other types of censoring, they are out of the scope of this project."), 
                                                    tags$br(),
                                                    tags$br(),
                                                    tags$ul(tags$b("Types of Right-Censoring", style = "font-size:16px;"),
                                                            tags$br(),
                                                            tags$br(),
                                                            tags$li(tags$b("Type I: "), sprintf("Type I censoring can be either fixed or random. Fixed type I censoring happens when the study ends after a certain amount of time and everyone for whom the event was not observed is censored at the time of the study ending (Madigan, 2013). Random Type I censoring also occurs when a study is designed to end after a certain amount of time, but different censored subjects have differing censoring times, meaning that not all subjects are censored at the same point in time. Thus, the dropout is considered random for type I censoring.")),
                                                            tags$br(),
                                                            tags$li(tags$b("Type II:"), sprintf("Type II censoring is when a study ends after a pre-defined number of events are observed, and so all subjects for which the event was not observed until the study ending are censored (Madigan, 2013). Thus, type II censoring is considered fixed.")),
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
                            ),
                              
                            tabPanel("3. The Survival Function",  
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
                                        tags$li(sprintf("In theory and as we have seen in class, the survival function is continuous, but for practical applications time scale that is observed is most often discrete, such as in terms of minutes, hours, or days (Madigan, 2013). This is because continuous monitoring of subjects is often too costly, too invasive, or simply not viable within the context of the study/participants.")),
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
                              
                              withMathJax(sprintf("Another distribution we were introduced to in class is the exponential distribution, where we first looked at an exponential random variable in the context of time in years from diagnosis until death of persons with a specific kind of cancer in our lecture. In fact, the expontial distribution is often used to model the time until a specific event occurs (Ross, 2007). For a random variable \\(X \\sim Exponential(\\lambda)\\) with parameter \\(\\lambda\\), we then have the associated pdf for values of \\(x \\geq 0 \\) as $$f(x) = \\lambda e^{-\\lambda x}.$$ Integrating the probability density function yields the exponential cumulative distribution function as $$F(x) = \\int_{0}^{x} \\lambda e^{-\\lambda t}dt = 1 - e^{-\\lambda x}.$$ The associated survival function can be found by taking subtracting the cdf from 1 as $$S(x) = 1 - F(x) = e^{-\\lambda x}.$$ Below, we again have each of the previously outlined functions plotted, where the parameter \\(\\lambda\\) can be modified on a slider to see the respective visual changes in the density, cumulative distribution, and most importantly survival function.")),
                              
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
                            ),
                            
                            tabPanel("4. Non-Parametric Estimation of the Survival Function",
                              h1("Non-Parametric Estimation of the Survival Function"),
                              p(withMathJax(sprintf("Assuming that every subject follows the same survival function allows for straightforward approaches for estimating the survival function \\(S(t)\\) (Madigan, 2013). Additionally, it is important to determine if parametric assumptions can be made for estimating the survival function. For the sake of limiting the scope of this project, we will not make any parametric assumptions and focus on one primary non-parametric estimation approach."))),
                              
                              tags$br(),
                              
                              p(withMathJax(sprintf("It is important to emphasize that if no subjects are censored, then a direct non-parametric estimator of the survival function would be to use the empirical cdf \\(F(t)\\) to yield \\(S(t)\\) through \\(S(t) = 1 - F(t)\\), exactly as we outlined the survival function be derived from the distribution function in theory as discussed at the beginning of this section. If we have multiple right-censored subjects, then using the empirical cdf is no longer an option and simply discarding those subjects with incomplete information would reduce the sample size. oftentimes to a problematic extent, which is where Kaplan-Meier product limit estimator as outlined in the next section becomes relevant."))),
                              
                              h2("Kaplan Meier Survival Curves"),
                              
                              p(withMathJax(sprintf("From an applied perspective, kaplan-meier survival curves can be interpreted as the probability of surviving a certain length of time, where time is considered in multiple small intervals. According to Kishore, Goel, and Khanna (2010), there are three primary assumptions that need to be considered regarding Kaplan Meier as follows."))),
                              
                              tags$ul(withMathJax(
                                tags$br(),
                                tags$li(tags$b("Assumption I: "), sprintf("Any subjects with censored survival times are assumed to have the same survival probabilities as subjects who are not censored.")),
                                tags$br(),
                                tags$li(tags$b("Assumption II: "), sprintf("Time of recruitment does not affect survival probability, or in other words when someone was recruited into the study does not affect the survival probability.")),
                                tags$br(),
                                tags$li(tags$b("Assumption III: "), sprintf("Given a discrete time-scale when measuring events on subjects, events are assumed to happen at the specificed time. It is important to mention that event though we know the theoretical survival function to be continuous, the actual time intervals are discrete, and so it follows that shorter time intervals between measurements on subjects are better approximate the survival function and increase accuracy (Kishore et al., 2010).")),
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
            
                              h3("Kaplan-Meier Survival Probability Calculation and Curve Plotting"),
                              
                              p(withMathJax(sprintf("One way to better understand the calculations of survival probabilities is with an actual example, where we will calculate the first three probilities for the life table provided by Sullivan (n.d.) to illustrate survival probability calculations to set up the Kaplan-Meier Curve. It is important to mention that data for analyses (including survival analysis) have a format where each row corresponds to one individual, but for the sake of this project a life table is more illustrative"))),
                              
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
                            ),
                            
                            tabPanel("5. Survival Probability Confidence Interval Estimates",
                                     h1("Survival Probability Confidence Interval Estimates"),
                                     p(withMathJax(sprintf("Now that we have calculated survival probabilities and plotted the survival curve, we have to account for uncertainty and the fact the calculated survival probabilities are estimates of the true survival probabilities, and so for \\(\\text{SE} = \\text{Standard Error}\\) we have a confidence interval according to the structure that we learned in class as $$\\text{Estimate} \\pm Z_{1-\\frac{\\alpha}{2}} \\cdot \\text{SE}_{\\text{Estimate}}.$$ According to Sullivan (n.d.), one way to effectively provide an survival estimate of the standard error is according to Greenwoods formula, where the standard error is defined as $$SE(S_{t}) = S_{t} \\sqrt{\\sum \\frac{D_t}{N_t \\cdot (N_t - D_t)}},$$ where we are taking the sum of \\(\\frac{D_t}{N_t \\cdot (N_t - D_t)}\\) cumulatively until the time point of interest."))),
                                     
                                     h2("Confidence Interval Calculations and Plotting"),
                                     p(withMathJax(sprintf("Thus, we have the confidence interval for time \\(t\\) as $$S_t \\pm Z_{1-\\frac{\\alpha}{2}} \\cdot S_{t} \\sqrt{\\sum \\frac{D_t}{N_t \\cdot (N_t - D_t)}}.$$ Using the example data by Sullivan (n.d.) from the previous sub-section, we can thus illustratively calculate the 95 Percent Confidence Intervals for several time points starting with \\(t=1\\) as $$S_1 \\pm Z_{0.975} \\cdot S_{1} \\sqrt{\\sum \\frac{D_1}{N_1 \\cdot (N_1 - D_1)}} = 0.95 \\pm 1.96 \\cdot 0.049 = [0.85, 1.00].$$ For time \\(t=2\\) we observe \\(\\frac{D_2}{N_2 \\cdot (N_2 - D_2)} = 0\\), since we did not observe any deaths at this time (\\(D_{2} = 0\\)), but since we take the cumulative sum of terms up until the respective point in time, we thus have the same Confidence Interval as the previous point in time as $$S_2 \\pm Z_{0.975} \\cdot S_{2} \\sqrt{\\sum \\frac{D_2}{N_2 \\cdot (N_2 - D_2)}} = 0.95 \\pm 1.96 \\cdot 0.049 = [0.85, 1.00].$$ For \\(t=3\\), we observe one death (\\(D_{3}=1\\)), and so both the survival probability (\\(S_{3}\\)) and the respective confidence interval bounds change to yield $$S_3 \\pm Z_{0.975} \\cdot S_{3} \\sqrt{\\sum \\frac{D_3}{N_3 \\cdot (N_3 - D_3)}} = 0.897 \\pm 1.96 \\cdot 0.069 = [0.76, 1.00].$$ Regarding interpretation of the previously calculated confidence intervals, it is important to mention that even though the upper bound would actually yield values larger than 1, they are truncated to 1, since survival probabilities larger than 1 would not make sense. We also intuitively see that as deaths happen as between times \\(t=2\\) and \\(t=3\\), the lower bound decreases in accordance with the decreasing survival probability."))),
                                     
                                     tags$br(),
                                     
                                     p(withMathJax(sprintf("Finally, we plot the previously calculated confidence interval, where the default value is \\(\\alpha=0.05\\) as was used for the calculations above, but can be varied below in order to see how different choices of the pre-determined type 1 error \\(\\alpha\\) can affect the resulting final confidence interval in the plot below."))),
                                     
                                     hr(),
                                     tags$br(),
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h4("Type I Error"),
                                         fluidRow(
                                           column(12,sliderInput(inputId="error_param",
                                                                 label=withMathJax(sprintf("Parameter \\(\\alpha\\)")),
                                                                 min = 0, max = 1, value = 0.05
                                           ))),
                                         width = 3
                                       ),
                                       mainPanel(
                                         plotOutput("survplotCI"),
                                         width = 9
                                       )
                                     ),
                                     
                                     tags$br(),
                                     hr(),
                                     
                            ),
                              
                            tabPanel("6. The Hazard Function and Cumulative Hazard",
                              h1("The Hazard Function and Cumulative Hazard"),
                              p(withMathJax(sprintf("For a distribution of T, the hazard function is defined as $$\\lambda(t) = lim_{dt \\rightarrow 0}\\frac{P \\{t \\leq T < t + dt | T \\geq t\\}}{dt}.$$ The denomintor \\(dt\\) gives us the interval \\([t,t + dt)\\). The numerator is the conditional probability of the event happening in this particular interval. By taking the limit to zero, we can get an instant rate of occurence, which can be written as $$\\lambda (t) = \\frac{f(t)}{S(t)}.$$ Since we know that \\(S(t) = 1 - F(t)\\) and \\(F(t) = \\int f(t) dt\\) from earlier sections, we have that $$\\lambda (t) = - \\frac{d}{dt}logS(t),$$ which by some algebra and integration can be shown to yield $$S(t) = exp \\{- \\int_{0}^{t}\\lambda(x)dx\\}.$$ The index number of e is known as the cumulative hazard, which is the sum of risk during the interval from 0 to t. Thus, by knowing either the hazard or survival function, we can calculate the other by using the equations above."))),
                            ),
                            
                            tabPanel("7. The Chi-Squared Distribution and Test",  
                              h1("The Chi-Squared Distribution and Test"),
                              h2("The Chi-Squared Distribution"),
                              p(withMathJax(sprintf("The Chi-squared distribution is a special case of gamma distribution. A gamma distribution of \\(\\alpha = 2\\) and \\(n = 2\\beta\\) equals to a Chi-squared distribution with \\(n\\) degrees of freedom. If we want to yield \\(n\\) random samples from a normal distribution, the chi-squared distribution is a summary of the square of these random samples. The degrees of freedom (\\(df\\)) is the number of the random samples(\\(n\\)). Also, the mean of the Chi-squared distribution is its degrees of freedom."))),
                              
                              tags$br(),
                              
                              h2("The Chi-Squared Test"),
                              
                              p(withMathJax(sprintf("We have that \\(O_{i}\\) means the observed value, \\(E_{i}\\) means the expected value, and so for a chi-squared test, the The chi-square statistic is defined as $$ \\chi_{c}^{2} = \\sum \\frac{(O_{i}-E_{i})^{2}}{E_{i}}.$$ The Chi-squared distribution can be used to estimate the confidence interval for the true variance of normal distribution. It can also be used to perform a Chi-squared test, which we will use to assess our result from the covid related application example."))),
                              
                              tags$br(),
                              
                              p(withMathJax(sprintf("The Chi-squared test is usually used to assess different kinds of data comparison including goodness of fit, homogeneity, and independence. In our example, we will use it to assess the independence of our result."))),
                              
                              p(withMathJax(sprintf("To perform a Chi-squared test, we need to do the following procedures."))),
                              
                              tags$hr(),
                              
                              tags$ol(
                                tags$li(withMathJax(sprintf("Define the null and alternative hypotheses based on the research."))),
                                tags$br(),
                                tags$li(withMathJax(sprintf("Calculate the Chi-squared statistic using the definition above."))),
                                tags$br(),
                                tags$li(withMathJax(sprintf("Determine the alpha value, where the confidence interval is defined as 100(1 - \\(\\alpha\\)) Percent. Usually, we set alpha to 0.05 (confidence interval=0.95)."))),
                                tags$br(),
                                tags$li(withMathJax(sprintf("Get the degrees of freedom(df) based on the research data."))),
                                tags$br(),
                                tags$li(withMathJax(sprintf("Get the p-value. It can show us whether the test result is significant or not."))),
                                tags$br(),
                                tags$li(withMathJax(sprintf("Compare the Chi-squared statistic with the critical value from Chi-squared distribution, determined by degrees of freedom(df) and the confidence interval(or alpha value). ")))
                              ),
                              
                              tags$hr(),
                              
                              p(withMathJax(sprintf("If the test result is smaller than the critical value, then we fail to reject the null hypothesis. On the other hand, if it is greater than the critical value, we reject the null hypothesis. "))),
                            ),
                            
                            tabPanel("8. Comparing Survival Curves and The Log Rank Test",
                                     
                                     h1("Comparing Survival Curves and The Log Rank Test"),
                                     p(withMathJax(sprintf("The log-rank test is a large-sample chi-square test which can be used to determine whether the Kaplan-Meier curves for two or more groups are statistically equivalent or not. When saying 'statistically equivalent' Kaplan-Meier curves, we mean that there is no evidence that suggest a difference of survival curves in the true population. Since the log-rank test is a chi-square test, it makes use of observed versus expected cell counts over categories of outcomes which are defined by each of the ordered failure times for the entire set of data being analyzed."))),
                                     
                                     h2("Log-Rank Test for Two Groups"),
                                     p(withMathJax(sprintf("The null hypothesis \\(H_0\\) being tested should always be that there is no difference between the two survival curves. The idea is to compare two groups of, for example, treatment and placebo. We assume that for each ordered failure time \\((t_f)\\) in the entire dataset, we have the numbers of subjects \\((m_{if})\\) failing at that time, separately by group \\((i=1,2)\\), followed by the numbers of subjects \\((n_{if})\\) in the risk set at that time, also separately by group \\((i=1,2)\\). Therefore, the following cell counts can be calculated as $$ e_{1f}=(\\frac{n_{1f}}{n_{1f}+n_{2f}}) \\cdot (m_{1f} + m_{2f}) \\\\ \\text{and} \\\\ e_{2f}=(\\frac{n_{2f}}{n_{1f}+n_{2f}}) \\cdot (m_{1f} + m_{2f}),$$ where the term \\((\\frac{n_{1f}}{n_{1f}+n_{2f}})\\) is the proportion in the risk set and the term \\((m_{1f}+m_{2f})\\) is the number of failures over both groups respectively. Thus, the formula above calculates the expected number at time \\(f\\) as the multiplication of proportion in risk set at time \\(f\\) and number of failures over both groups at time \\(f\\)."))),
                                     tags$br(),
                                     p(withMathJax(sprintf("When we are calculating the log-rank test statistic, we need to calculate the sum of all the differences between the observed and expected counts of all failure times \\((f)\\) as $$ O_i - E_i = \\sum_{f=1}^{n} (m_{if} - e_{if}), $$ and divide this difference by the estimated variance of the summed observed minus expected score, which is $$ \\text{Var}(O_i - E_i) = \\sum_{j} \\frac{n_{1f}n_{2f}(m_{1f} + m_{2f})(n_{1f} + n_{2f} + m_{1f} - m_{2f})}{(n_{1f} + n_{2f})^{2}(n_{1f} + n_{2f} - 1)}, $$ where \\(n\\) here is equal to the number of failure times and \\((i=1,2)\\). Therefore, we have the under the null hypothesis\\(H_0\\) we have the log-rank statistic as $$\\text{Log-rank statistic} = \\frac{(O_2 - E_2)^2}{\\text{Var}(O_2 - E_2)}, $$ which is approximately chi-square distributed with one degree of freedom. The p-value for the log-rank test can be determined from tables of the chi-square distribution. Then, we can determine whether we should reject our null hypothesis or not, or in other terms, whether or not there is a statistically difference between the two survival curves."))), 
                                     tags$br(),
                                     p(withMathJax(sprintf("Finally, it is important to mention that the approximation of the log-rank statistic can be calculated by the sum of the square of difference between the observed and expected values divided by the expected value for each group as $$ \\chi^{2} \\approx \\sum_{i}^{\\text{# of groups}} \\frac{(O_i - E_i)^{2}}{E_i}, $$ which we know to correspond to the chi-square statistic from the family of chi-square tests in class and also from the previous section on the chi-square distribution. "))),
                                     
                                     h2("The Log-Rank Test for Several Groups"),
                                     p(withMathJax(sprintf("The log-rank test can also be used to compare the survival curves of three or more groups. The null hypothesis \\(H_0\\) in this case is that all survival curves are the same. In this case, the calculation of the log-rank statistic involves both variances and covariances of summed observed minus expected scores for each group and is best described using linear algebra. We do not further cover the theory for the log-rank test for several groups, as it is not used in the applied part and thus out of the scope of this project."))),
                            ),
                          ),
                 ),

###################################################################
# Application Tab
###################################################################
                 tabPanel(title = "A Covid Related Application Example",
                          
                          #uiOutput('markdown')
                          
                          h1("A Moderna Covid Vaccine Survival Analysis Example"),
                          
                          h3("Introduction"),
                          tags$br(),
                          
                          p(withMathJax(sprintf("For the final project of 652, we have made some modifications and expansions regarding this COVID-19 dataset:"))),
                          
                          tags$ol(withMathJax(
                            tags$br(),
                            tags$li(sprintf("A more thorough background of the data we chose to analyze.")),
                            tags$br(),
                            tags$li(sprintf("A more detailed table of baseline characteristics.")),
                            tags$br(),
                            tags$li(sprintf("Confidence intervals for KM curves.")),
                            tags$br(),
                            tags$li(sprintf("Demonstrate how to obtain the Log rank statistics using the COVID-19 dataset.")),
                            tags$br(),
                            tags$li(sprintf("Log rank test to compare the survival distrubutions of two populations.")),
                            tags$br(),
                            tags$li(sprintf("Data application of Cox PH model.")),
                            tags$br()
                          )),
                          
                          h3("Analysis Overview and Summary"),
                          
                          tags$ul(withMathJax(
                            tags$br(),
                            tags$li(tags$b("Objective and Data Application:"), sprintf("In this section of the project, we aimed to better understand a covid-19 vaccine trial dataset (United States Government (USG)/COVID-19 Response Team phase 3 trials) to demonstrate some concepts we outlined in the previous section to evaluate the effect of Moderna vaccine.")),
                            tags$br(),
                            tags$li(tags$b("Data Source:"), sprintf("https://github.com/CoVPN/correlates_reporting_usgcove_archive")),
                            tags$br(),
                            tags$li(tags$b("Current Published Research on the Data:"), sprintf("DOI: 10.1126/science.abm3425")),
                            tags$br(),
                            tags$li(tags$b("Data Background"), sprintf("This is a phase 3 clinical trial-The Coronavirus Efficacy (COVE) phase 3 trial (NCT04470427) of the mRNA-1273 COVID-19 vaccine data set that aims to assess correlates of risk and correlates of protection for COVID-19 using the neutralizing and binding antibodies. The pressing challenge posed by the demand of doses in diverse populations and the emmergence of viral variants call for the need of effetive COVID-19 vaccines. However, to achieve clinically relevent endpoints in clinical trials is usually time consuming. Therefore, immunological markers that can be used as surrogate endpoints to predict vaccine efficacy would expedite the approval process for our pressing need for COVID vaccines.")),
                            tags$br(),
                            tags$li(tags$b("Inclusion Eligibility:"), sprintf("Persons 18 years of age or older with no known history of SARS-CoV-2 infection and with locations or circumstances that put them at an appreciable risk of SARS- CoV-2 infection, a high risk of severe Covid-19, or both.")),
                            tags$br(),
                            tags$li(tags$b("Source population:"), sprintf("randomized controlled trial (Vaccine: 15,000; Placebo: 15,000)")),
                            tags$br(),
                            tags$li(tags$b("Study Population:"), sprintf("subjects with full data of baseline antibody titer. N= 2,857")),
                            tags$br()
                          )),
                          
                          h3("Data Cleaning"),
                          
                          tags$br(),
                          
                          # p(withMathJax(sprintf("We first consider the names of the variables as follows."))),
                          # 
                          # tags$br(),
                          #               
                          # verbatimTextOutput("vtout"),
                          # 
                          # tags$br(),
                          
                          p(withMathJax(sprintf("We first only include subjects with full data of baseline antibody titer and the relevant variables into our study dataset and remove all NA values."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout1"),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("We then make a two-by-two table and of Moderna vaccination status and Covid-19 infection. It is important to mention that only people who were fully vaccinated with two dose of Moderna vaccine were considered as vaccinated. We can see in the table below that most of the people who were infected were not vaccinated with the moderna vaccine."))), 
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout2"),
                          
                          tags$br(),
                          
                          gt_output('table1'),
                          
                          tags$br(),
                          
                          h3("Data Analysis and Results Visualization"),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("From the two-by-two table above, we can see that the vaccinated people have a lower risk of getting infected with COVID-19, so we want to perform a Chi-squared test whether the differnece of infection rate among the two groups is statistically significant. Thusm we perform the chi-square test to determine the association between vaccination status and Covid-19 infection."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout3"),
                          
                          verbatimTextOutput("vtout3sub"),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("From the chi-square, we can see that the low p-value and associated high chi-square statistic indicate that there is a strong association between vaccination status and covid infection. Now, we create baseline characteristics: We selected sex, ethnicity, age, and race as baseline covariates, where age was converted into 4 dummy variables. Thus, since baseline characteristics do not vary by vaccination status, it is less likely that we have confounding effects for our data."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout4"),
                          
                          tags$br(),
                          
                          gt_output('table2'),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("We now regard the followup time and the probability of survival accordingly, and print them for the first 10 observations."))),
                          
                          tags$br(),
                    
                          verbatimTextOutput("vtout5"),
                          
                          verbatimTextOutput("vtout5sub"),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("The overall Kaplan-Meier estimate for the survival of Covid infection based on the previous probabilities is as follows."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout6"),
                          
                          
                          hr(),
                          tags$br(),
                          plotOutput("survplot1", height = "600px"),
                          tags$br(),
                          hr(),
                          
                          p(withMathJax(sprintf("From the plot above, we can see that without stratifying any variables, as the post entry time increase, the probability of survival (not getting infected with COVID) decreases."))),
                          tags$br(),
                          p(withMathJax(sprintf("Now, the Kaplan-Meier estimate for the cumulative incidence of Covid infection is as follows."))),
                          
                          hr(),
                          tags$br(),
                          plotOutput("survplot2", height = "600px"),
                          tags$br(),
                          hr(),
                          
                          verbatimTextOutput("vtout7"),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("The Kaplan-Meier estimate for the survival of Covid infection stratified by vaccination status is as follows. Stratifying the dataset using vaccination status, the further apart the two survival curves become as post entry time increases. The 95%% confidence intervals serve as a good visualization tool to detect the difference between the two groups regrarding COVID-19 infection probability."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout8"),
                          
                          hr(),
                          tags$br(),
                          plotOutput("survplot3", height = "600px"),
                          tags$br(),
                          hr(),
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf("The Kaplan-Meier estimate for the survival of Covid infection stratified by ethnicity (Hispanic or nonhispanic) is as follows. We did not find ethnicity to be relevant predictor regarding covid infection."))),
                          
                          tags$br(),
                          
                          verbatimTextOutput("vtout9"),
                          
                          hr(),
                          tags$br(),
                          plotOutput("survplot4", height = "600px"),
                          tags$br(),
                          hr(),
                          
                          
                          tags$br(),
                          
                          p(withMathJax(sprintf(" Demonstration of deriving the Log-rank statistics $$ 
\\begin{array}{llllllllll}
\\hline
& & \\text{# failures} &  & \\text{# in risk set} & & \\text{# expected} & & \\text{Observed-Expected} & \\\\ 
\\hline
f & t_f & m_{1f} & m_{2f}  & n_{1f} & n_{2f} & e_{1f} & e_{1f} & m_{1f} - e_{1f} & m_{2f} - e_{2f} \\\\ 
1 & 18 & 0 & 1 & 10 & 10 & (10/20) \\cdot 1 & (10/20) \\cdot 1 & -0.5 & 0.5 \\\\
2 & 36 & 0 & 1 & 10 & 9 & (10/19) \\cdot 1 & (9/19) \\cdot 1 & -0.53 & 0.53 \\\\
3 & 76 & 0 & 1 & 10 & 8 & (10/18) \\cdot 1 & (8/18) \\cdot 1 & -0.56 & 0.56 \\\\
4 & 85 & 0 & 1 & 10 & 7 & (10/17) \\cdot 1 & (7/17) \\cdot 1 & -0.59 & 0.59 \\\\
5 & 90 & 0 & 1 & 10 & 6 & (10/16) \\cdot 1 & (6/16) \\cdot 1 & -0.63 & 0.63 \\\\
6 & 112 & 0 & 1 & 10 & 5 & (10/15) \\cdot 1 & (5/15) \\cdot 1 & -0.67 & 0.67 \\\\
7 & 128 & 0 & 1 & 10 & 4 & (10/14) \\cdot 1 & (4/14) \\cdot 1 & -0.71 & 0.71 \\\\
8 & 162 & 0 & 1 & 10 & 3 & (10/13) \\cdot 1 & (3/13) \\cdot 1 & -0.77 & 0.77 \\\\
\\text{Totals} &  & 0 & 8 & &  & 4.95 \\cdot 1 & 3.05 \\cdot 1 & -4.96 & 4.96 \\\\
\\hline
\\end{array} $$, which would thus yield $$\\text{Log-rank statistic} = \\frac{(O_2 - E_2)^2}{\\text{Var}(O_2 - E_2)} = \\frac{(4.96)^2}{1.83} = 13.47$$, which we compute in R as "))),
                verbatimTextOutput("vtout10"),
                verbatimTextOutput("vtout11"),

                 p(withMathJax(sprintf("Perform Log rank test (vaccination status) Using the vaccination status and ethnicity that we plotted our KM curves previously, we now implement the log-rank test to formally conduct between-groups significant test. Below we first conduct a log-rank test that equally weights observations over the entire follow-up time. With \\(rho = 0\\), is the log-rank or Mantel-Haenszel test; with \\(rho = 1\\) is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test."))),
                 verbatimTextOutput("vtout12"),
                 verbatimTextOutput("vtout13"),
                 p(withMathJax(sprintf("Perform Log rank test (vaccination status- weighted)"))),
                 verbatimTextOutput("vtout14"),
                 verbatimTextOutput("vtout15"),
                 p(withMathJax(sprintf("Perform Log rank test (Ethnicity)"))),
                 verbatimTextOutput("vtout16"),
                 verbatimTextOutput("vtout17"),
                 p(withMathJax(sprintf("Now to further explore this time-to-event dataset, we may want to know how much does one variable or multiple variables accounts for the effect of multiple variables. Here, we want to know how much does the status of vaccination affect the 'hazard' of being infected with COVID-19."))),

                 tags$br(),

                 p(withMathJax(sprintf("Cox PH model"))),
                 verbatimTextOutput("vtout18"),
                 verbatimTextOutput("vtout19"),
                 p(withMathJax(sprintf("In this univariate Cox analysis, the covariate 'Vaccination status' has a p-value <0.001, and a hazard ratio 0.0378, indicating a strong relationship between the vacciation status and decreased risk of COVID-19 infection. The hazard ratio has a similar interpretation of the strength of the effect as an odds ratio. For example, a hazard ratio of 0.04 can be interpreted as comparing to the unvaccinated group, people in the vaccinated group have 0.04 times the hazard of being infected with COVID-19."))),
                
              ),

###################################################################
# References Tab
###################################################################
                 tabPanel(title = "References",
                          
                          h1("References"),
                          h4("Papers"),
                          tags$ul(
                            tags$li("Team, U. C.-. 19 R. T. / C. P. N. (CoVPN) B., Gilbert, P. B., Fong, Y., Benkeser, D., Andriesen, J., Borate, B., Carone, M., Carpp, L. N., Diaz, I., Fay, M. P., Fiore-Gartland, A., Hejazi, N. S., Huang, Y., Huang, Y., Hyrien, O., Janes, H. E., Juraska, M., Li, K., Luedtke, A., . Follmann, D.. (2020). USG COVID-19 Response Team / CoVPN Vaccine Efficacy Trial Immune Correlates Statistical Analysis Plan (Version 12). figshare. https://doi.org/10.6084/m9.figshare.13198595.v12"),
                            tags$li("Kishore, J., Goel, M., & Khanna, P. (2010). Understanding survival analysis: Kaplan-Meier estimate. International Journal of Ayurveda Research, 1(4), 274. https://doi.org/10.4103/0974-7788.76794"),
                            tags$li("Clark, T. G., Bradburn, M. J., Love, S. B., & Altman, D. G. (2003). Survival Analysis Part I: Basic concepts and first analyses. British Journal of Cancer, 89(2), 232–238. https://doi.org/10.1038/sj.bjc.6601118")
                          ),
                          h4("Books"),
                          tags$ul(
                            tags$li("S. Ross (2014), Introduction to Probability Models (Eleventh Edition). Academic Press."),
                            tags$li("Ettore Marubini, & Maria Grazia Valsecchi. (2005). Analysing survival data from clinical trials and observational studies. Wiley. ")
                          ),
                          h4("Websites"),
                          tags$ul(
                            tags$li("Sullivan, L. Survival Analysis. Boston University School of Public Health, https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_survival/BS704_Survival_print.html Accessed 10 October 2021."),
                            tags$li("Madigan, D. (2013). Statistics W2025/W4025: Methods in Applied Statistics. Columbia University, http://www.stat.columbia.edu/~madigan/W2025/ Accessed 5 October 2021.")
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
                            tags$li(sprintf("survminer")),
                            tags$li(sprintf("dplyr")),
                            tags$li(sprintf("tidyverse")),
                            tags$li(sprintf("haven")),
                            tags$li(sprintf("gt")),
                            tags$li(sprintf("gtsummary")),
                            tags$li(sprintf("knitr"))
                          )),
                          
            ),
            fluid=FALSE                 
)


###################################################################
#Server
###################################################################
server <- function(input, output) {
  
  
  
  ###################################################################
  # Tiffany Plots and Tables
  ###################################################################
  table1 <- data_sub %>%
                select(Trt,infect) %>%
                tbl_summary(type = Trt ~ "categorical",
                            by = infect,
                            percent = "row") %>% 
                as_gt()
  
  output$table1 <- render_gt(table1)
  
  
  table2 <-   data_sub2 %>% 
    select(Sex, Hispanic,Age_c, White,Black,Asian,Native,PacificIslander,Multiracial,Other,Trt) %>% 
    mutate_if(is.labelled, as_factor) %>% 
    tbl_summary(by = "Trt",
                percent = "col",
                missing_text = "*Missing") %>% 
    add_overall(last = TRUE) %>% 
    modify_spanning_header(
      everything()~ "**Table 1**")  %>% 
    as_gt()
  
  output$table2 <- render_gt(table2)
  
  lf_table <- survfit(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~1, data = data_sub2)
  
  survplot1 <- ggsurvplot(fit = lf_table,
                          data = data_sub2,
                          conf.int = TRUE,
                          # Add axis label & title
                          title = "Kaplan-Meier Estimate of Covid Infection",
                          subtitle = " (N=2,857) ",
                          xlab = "Post entry Time (Days)",
                          ylab = "Probability of Survival",
                          #  Add median survival line - 'both horizontal & vertical'
                          #surv.median.line = "hv",
                          legend = "none"
  )
  
  output$survplot1 <- renderPlot({survplot1[[1]]})
  
  survplot2 <- ggsurvplot(fit = lf_table,
              data = data_sub2,
              conf.int = TRUE,
             # Add axis label & title
             title = "Kaplan-Meier Estimate of Cumulative Incidence of Covid Infection",
             subtitle = " (N=2,857)",
             xlab = "Post entry Time (Days)",
             ylab = "Probability of Mortality",
             #  Add median survival line - 'both horizontal & vertical'
             #surv.median.line = "hv",
             legend = "none",
             fun = "event"
  )
  
  output$survplot2 <- renderPlot({survplot2[[1]]})
  
  km_vaccine<- survfit(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Trt, data = data_sub) 
  
  survplot3 <- ggsurvplot(fit = km_vaccine,
                          data = data_sub2,
                          conf.int = TRUE,
                          pval = TRUE, 
                          # Add axis label & title
                          title = "Kaplan-Meier Estimate of Covid Infection",
                          subtitle = " (N=2,857) ",
                          xlab = "Post entry Time (Days)",
                          ylab = "Probability of Survival"
  )
  
  output$survplot3 <- renderPlot({survplot3[[1]]})
  
  km_vaccine_eth<- survfit(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Hispanic, data = data_sub2) 
  
  survplot4 <- ggsurvplot(fit = km_vaccine_eth,
                          # Add axis label & title
                          data = data_sub2,
                          conf.int = TRUE,
                          pval = TRUE, 
                          title = "Kaplan-Meier Estimate of Covid Infection",
                          subtitle = " (N=2,857) ",
                          xlab = "Post entry Time (Days)",
                          ylab = "Probability of Survival"
  )
  
  output$survplot4 <- renderPlot({survplot4[[1]]})
  
  ###################################################################
  # Tiffany Code
  ###################################################################
  
  output$vtout <- renderPrint({
    cat("colnames(data_R)", sep = "\n")
  })
  
  output$vtout1 <- renderPrint({
    cat("covid<-data_R[!is.na(data_R$BbindSpike)&!is.na(data_R$BbindRBD)&!is.na(data_R$BbindN)&!is.na(data_R$Day57bindSpike)&!is.na(data_R$Day57bindRBD)&!is.na(data_R$Day57bindN),]",
        'covid$Trt <- factor(covid$Trt, levels = c(0, 1), labels = c("Not Vaccinated", "Vaccinated"))',
        'covid$infect <- factor(covid$EventIndPrimaryD57, levels = c(0, 1), labels = c("Not Infected", "Infected"))',
        'data_sub <- covid[,c("X","Trt","EthnicityHispanic","Black","Asian","Sex","BMI","Age","EventTimePrimaryD57","EventIndPrimaryD57","infect","BbindSpike")]'
        ,sep = "\n")
  })
  
  output$vtout2 <- renderPrint({
    cat("data_sub %>%",
        '  select(Trt,infect) %>%',
        '  tbl_summary(type = Trt ~ "categorical",',
        '              by = infect)'
        ,sep = "\n")
  })
  
  output$vtout3 <- renderPrint({
    cat("chisq.test(data_sub$Trt,data_sub$EventIndPrimaryD57)", sep = "\n")
  })
  
  output$vtout3sub <- renderPrint({
    cat("## ",
        "##  Pearson's Chi-squared test with Yates' continuity correction",
        "## ",
        "## data:  data_sub$Trt and data_sub$EventIndPrimaryD57",
        "## X-squared = 1045.3, df = 1, p-value < 2.2e-16",sep = "\n")
  })
  
  output$vtout4 <- renderPrint({
    cat('data_sub$Sex<-ifelse(data_sub$Sex==1,"Male","Female")',
        'data_sub$EthnicityHispanic<-ifelse(data_sub$EthnicityHispanic==1,"Hispanic","Non-Hispanic")',
        '',
        'data_sub$Age<-as.numeric(data_sub$Age)',
        'data_sub$Age_c <- cut(data_sub$Age, breaks = c(0,30,40,50 ,65,Inf))',
        'data_sub %>% ',
        '  select(Sex, EthnicityHispanic, Age_c, Trt) %>% ',
        '  mutate_if(is.labelled, as_factor) %>% ',
        '  tbl_summary(by = "Trt",',
        '              percent = "row",',
        '              missing_text = "*Missing") %>% ',
        '  add_overall(last = TRUE) %>% ',
        '  modify_spanning_header(',
        '    everything()~ "**Table 1 - Row percent**") '
        ,sep = "\n")
  })
  
  output$vtout5 <- renderPrint({
    cat("lf_table <- survfit(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~1, data = data_sub) ", 
        "summary(lf_table, censored = TRUE)",sep = "\n")
  })
  
  output$vtout5sub <- renderPrint({
    cat("## Call: survfit(formula = Surv(EventTimePrimaryD57, EventIndPrimaryD57) ~ ", 
        "##     1, data = data_sub)",
        "## ",
        "##  time n.risk n.event survival std.err lower 95% CI upper 95% CI",
        "##     3   2857       0    1.000 0.00000        1.000        1.000",
        "##     6   2856       0    1.000 0.00000        1.000        1.000",
        "##     7   2855       1    1.000 0.00035        0.999        1.000",
        "##     8   2696       9    0.996 0.00116        0.994        0.999",
        "##     9   2687       7    0.994 0.00152        0.991        0.997",
        "##    10   2680      10    0.990 0.00191        0.986        0.994",
        sep = "\n")
  })
  
  output$vtout6 <- renderPrint({
    cat('ggsurvplot(fit = lf_table,',
        '           title = "Kaplan-Meier Estimate of Covid Infection",',
        '           subtitle = " (N=2,857) ",',
        '           xlab = "Post entry Time (Days)",',
        '           ylab = "Probability of Survival",',
        '           legend = "none"'
        ,sep = "\n")
  })
  
  output$vtout7 <- renderPrint({
    cat('ggsurvplot(fit = lf_table,',
        '           title = "Kaplan-Meier Estimate of Cumulative Incidence of Covid Infection",',
        'subtitle = " (N=2,857)",',
        'xlab = "Post entry Time (Days)",',
        'ylab = "Probability of Mortality",',
        'legend = "none",',
        'fun = "event"'
        ,sep = "\n")
  })
  
  output$vtout8 <- renderPrint({
    cat('km_vaccine<- survfit(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Trt, data = data_sub) ',
        'ggsurvplot(fit = km_vaccine,',
        '           title = "Kaplan-Meier Estimate of Covid Infection",',
        '           subtitle = " (N=2,857) ",',
        '           xlab = "Post entry Time (Days)",',
        '           ylab = "Probability of Survival"',
        ')'
        ,sep = "\n")
  })
  
  output$vtout9 <- renderPrint({
    cat('km_vaccine_eth<- survfit(Surv(EventTimePrimaryD57,',
        'EventIndPrimaryD57)~EthnicityHispanic, data = data_sub)',
        'ggsurvplot(fit = km_vaccine_eth,',
        '           title = "Kaplan-Meier Estimate of Covid Infection",',
        '           subtitle = " (N=2,857) ",',
        '           xlab = "Post entry Time (Days)",',
        '           ylab = "Probability of Survival"',
        ')'
        ,sep = "\n")
  })
  
  output$vtout10 <- renderPrint({
    cat('draw <- function(n){',
        '  vax_sam<-sample(which(data_sub$Treatment=="Vaccinated"),n)',
        '  unvax_sam<-sample(which(data_sub$Treatment=="Not Vaccinated"),n)',
        '  all <- c(vax_sam,unvax_sam)',
        '  df<- data_sub[all,c("X","Treatment","EventTimePrimaryD57","EventIndPrimaryD57")]',
        '}',
        'set.seed(100)',
        'sample<-draw(10)',
        'variance_h <- function(n1f,n2f,m1f,m2f){',
        '  var_hat <- n1f*n2f*(m1f+m2f)*(n1f+n2f-m1f-m2f)/((n1f+n2f)^2*(n1f+n2f-1))',
        '}',
        'n1f <- c(10,10,10,10,10,10,10,10)',
        'n2f<-c(10,9,8,7,6,5,4,3)',
        'm1f<-rep(0,8)',
        'm2f<-rep(1,8)',
        'df<-data.frame(m1f,m2f,n1f,n2f)',
        'var_sum <-0',
        'var_all<-rep(0,8)',
        'for (i in 1:8) {',
        '  var_all[i]<-variance_h(n1f[i],n2f[i],m1f[i],m2f[i])',
        '  var_sum<-var_all[i]+var_sum',
        '}',
        'logtankstat<- 4.96^2/var_sum',
        'logtankstat'
        ,sep = "\n")
  })

  output$vtout11 <- renderPrint({
    cat("## [1] 13.46831",
        sep = "\n")
  })
  
  output$vtout12 <- renderPrint({
    cat("survdiff(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Treatment, data = data_sub)",
        sep = "\n")
  })
  
  output$vtout13 <- renderPrint({
    cat('## Call:',
        '## survdiff(formula = Surv(EventTimePrimaryD57, EventIndPrimaryD57) ~ ',
        '##     Treatment, data = data_sub)',
        '## ',
        '##                             N Observed Expected (O-E)^2/E (O-E)^2/V',
        '## Treatment=Not Vaccinated 1596     1011      453       686      1228',
        '## Treatment=Vaccinated     1261       55      613       508      1228',
        '## ',
        '##  Chisq= 1228  on 1 degrees of freedom, p= <2e-16',
        sep = "\n")
  })
  
  output$vtout14 <- renderPrint({
    cat("survdiff(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Treatment, data = data_sub,rho = 1)",
        sep = "\n")
  })
  
  output$vtout15 <- renderPrint({
    cat('## Call:',
        '## survdiff(formula = Surv(EventTimePrimaryD57, EventIndPrimaryD57) ~ ',
        '##     Treatment, data = data_sub, rho = 1)',
        '## ',
        '##                             N Observed Expected (O-E)^2/E (O-E)^2/V',
        '## Treatment=Not Vaccinated 1596    809.5      373       512      1132',
        '## Treatment=Vaccinated     1261     44.5      481       396      1132',
        '## ',
        '##  Chisq= 1132  on 1 degrees of freedom, p= <2e-16',
        sep = "\n")
  })
  
  output$vtout16 <- renderPrint({
    cat("survdiff(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Hispanic, data = data_sub)",
        sep = "\n")
  })
  
  output$vtout17 <- renderPrint({
    cat('## Call:',
        '## survdiff(formula = Surv(EventTimePrimaryD57, EventIndPrimaryD57) ~ ',
        '##     Hispanic, data = data_sub)',
        '## ',
        '##                             N Observed Expected (O-E)^2/E (O-E)^2/V',
        '## Hispanic=Hispanic      312      105      117     1.322      1.49',
        '## Hispanic=Non-Hispanic 2545      961      949     0.164      1.49',
        '## ',
        '##  Chisq= 1.5  on 1 degrees of freedom, p= 0.2',
        sep = "\n")
  })
  
  output$vtout18 <- renderPrint({
    cat("summary(coxph(Surv(EventTimePrimaryD57, EventIndPrimaryD57)~Treatment, data = data_sub))",
        sep = "\n")
  })
  
  output$vtout19 <- renderPrint({
    cat('## Call:',
        '## coxph(formula = Surv(EventTimePrimaryD57, EventIndPrimaryD57) ~ ',
        '##     Treatment, data = data_sub)',
        '## ',
        '##   n= 2857, number of events= 1066 ',
        '## ',
        '##                         coef exp(coef) se(coef)      z Pr(>|z|)   ',
        '## TreatmentVaccinated -3.27515   0.03781  0.13887 -23.59   <2e-16 ***',
        '## ---',
        "## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
        '## ',
        '##                     exp(coef) exp(-coef) lower .95 upper .95',
        '## TreatmentVaccinated   0.03781      26.45    0.0288   0.04964',
        '## ',
        '## Concordance= 0.756  (se = 0.006 )',
        '## Likelihood ratio test= 1393  on 1 df,   p=<2e-16',
        '## Wald test            = 556.2  on 1 df,   p=<2e-16',
        '## Score (logrank) test = 1229  on 1 df,   p=<2e-16',
        sep = "\n")
  })
  
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
                         title = "Kaplan-Meier Survival Curve Example",
                         legend = "none",
                         palette = c("#2E9FDF"),
                         ggtheme = theme_bw() 
                         )
  
  output$survplot <- renderPlotly({ggplotly(survplot[[1]])})
  
  
  output$survplotCI <- renderPlot({  
      lsurv2 <- survfit(Surv(time, status) ~ 1, df_mod, conf.int= (1 - input$error_param))
      ggsurvplot(lsurv2, data = df_mod, 
             conf.int = TRUE,
             title = "Survival Curve Confidence Interval Example",
             legend = "none",
             palette = c("#2E9FDF"),
             ggtheme = theme_bw())
      })
  
  ###################################################################
  #  Tiffany's Analysis Inclusion
  ###################################################################
  
  
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML(knit('covid_application2.rmd', quiet = TRUE)))
  # })
  
}

###################################################################
#App Construction
###################################################################
shinyApp(server = server, ui = ui)
\name{RentInvestCalculatoR}
\alias{RentInvestCalculatoR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Rents are based on different market circumstances (see PART C). If you are investing into an apartment additional value-increasing components, you are allowed to increase the rent (art. 14 VMWG). Therefore you need to take account the investment in CHF, and how much it's value-adding. Most of the time this is a rate between 50 and 70 \%. Only if you invest in something completely new (i.e. that wasn't there in a former version) or if it is something which supports ecology of the building you can take 100 \%. Because lifespan of elements differ it has to be taken into account as well. On the webpage https://www.mietrecht.ch/index.php?id=32 you can look up different lifespans that are commonly accepted.The maintenance rate differs on the component. Some need no maintenance during the warranty and other never (for example a dishwasher or a carpet). Therefore you can choose a maintenance rate between 0 and 10 \% by a Suppreme court ruling.
}
\usage{
RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
To calculate the changings due to investments we need from the user 
  \item{act_rent_CHF}{actual net rent in CHF
%%     ~~Describe \code{x} here~~
}
  \item{investment_CHF}{total investment in CHF
%%     ~~Describe \code{x} here~~
}
  \item{increasing_share}{value-increasing share in \% 
%%     ~~Describe \code{x} here~~
}
  \item{lifespan}{lifespan in years
%%     ~~Describe \code{x} here~~
}
  \item{maintenance_rate}{maintenance allowance in \%, set to 10 \% by default due to a Supreme Court ruling
%%     ~~Describe \code{x} here~~
}
}


\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
The returned values are the actual data such as:
\item{value_incr_share_CHF}{the value increasing share in CHF}
\item{depreciation_CHF}{the depreciation to the investment in CHF}
\item{interest_CHF}{the interests to the investment in CHF}
\item{maintenance_CHF = 10}{the maintenance taken into account in CHF}
\item{total_add_rent_monthly_CHF}{the total added rent per month in CHF}
\item{total_new_rent_monthly_CHF}{the new total rent per month in CHF}

Inside a vector
\item{return_invest (c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF = 10, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)}{vector of the calculated parts of the new rent}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\references{
%% ~put references to the literature/web site here ~
Topic related informations about rents in Switzerland:
https://www.mietrecht.ch

General R:
https://stat.ethz.ch/R-manual/
http://www.wolferonline.de/xmlR/XMLinR.pdf
https://de.wikibooks.org/wiki/GNU_R
https://stackoverflow.com/

Creating a package:
https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/
http://portal.stats.ox.ac.uk/userdata/ruth/APTS2012/Rcourse10.pdf
https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages
https://support.rstudio.com/hc/en-us/articles/200486518

Writing documentation:
https://support.rstudio.com/hc/en-us/articles/200532317-Writing-Package-Documentation

Debugging:
https://stat.ethz.ch/pipermail/r-help/2014-May/374864.html
https://stackoverflow.com/questions/26697727/what-does-error-in-namespaceexportns-exports-undefined-exports-mean

Publishing on a GitHub-Account:
https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/

}

\author{
%%  ~~who you are~~
Ruth Peterhans ruth.maria.peterhans@usi.ch, Giuliano Giovanardi, giuliano.giovanardi@usi.ch
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
# EXAMPLE for standard use
# ----------------------------------------------------------------------------------------------------------
# You put in the needed data of your actual rent and of the investment into the calculator.
# If you don't give a value for the maintenance rate, the calculator uses 10 % by default

# To test it for a actual rent of 1000 CHF, an investment of 100000 CHF,
# a value increasing share of 50 % and a lifespan of 12 years:
RentInvestCalculatoR(1000,100000,50,12)


# EXAMPLE with a quasi zero value for actual_rent (e.g. for calculating initial rents of new buildings)
# ----------------------------------------------------------------------------------------------------------
# The starting values of the actual rent basically can't be zero. If you would like to calculate the initial
# rent for a new building, you have to set the actual rent to a very small value. Of course you can also use
# it, if you buy an apartment and you want to know how much rent you should get out of it. The calculator
# gives you here an approximation, because you have to set an average lifespan over all components of the
# building (see also EXAMPLE with different components for more precise calculations).

# To test it for a new building let's say we would like to calculate the rent for a unit (apartment).
# The unit is valid an investment of 500000 CHF,  the value increasing share is 100 % because there was no
# value before, the average lifespan will be 50 years and the maintenance is set to 8 % because in the
# in the first years you still have guarantee and would not spend as much on maintenance.
RentInvestCalculatoR(0.000001,500000,100,50,8)


# EXAMPLE with different components
# ----------------------------------------------------------------------------------------------------------
# If you would like to do the calculation for different components, you can use the calculator of course
# different times and add the values up. You can look up different lifespans of components on
# https://www.mietrecht.ch/index.php?id=32 (unfortunately only available in German language).

# Let's say you have after a total renovation (including energetic and ecological) for example
# following investments with different lifespans an maintenance efforts and you would like to know
# the sum of over all changes due to this investments:

# investment_name  |   act_rent_CHF | investment_CHF  | increasing_share | lifespan    |  maintenance_rate
#  structure of building      0.001        250000           100 %            100 years     10 % by default
#  windows                    0.001         40000           100 %             20 years      8 %
#  kitchen                    0.001         30000            70 %             15 years      8 %
#  bathroom                   0.001         50000            70 %             30 years      8 %
#  floors                     0.001         25000            50 %             12 years      8 %

building <- RentInvestCalculatoR(0.001, 250000, 100, 100, 10)
windows <- RentInvestCalculatoR(0.001, 40000, 100, 20, 8)
kitchen <- RentInvestCalculatoR(0.001, 30000, 70, 15, 8)
bathroom <- RentInvestCalculatoR(0.001, 50000, 70, 30, 8)
floors <- RentInvestCalculatoR(0.001, 25000, 50, 12, 0)

# And then to sum up all changes and give an answer
total_invest <- as.data.frame(rbind(building,windows, kitchen, bathroom, floors))
cat("The Total change in rent per month due to all investments is", sum(total_invest$total_add_rent_monthly_CHF))

%% ~~##---- Should be DIRECTLY executable !! ----~~~
%% ~~##-- ==>  Define data, use random,~~~
%% ~~##--	or do  help(data=index)  for the standard data sets.~~~

%% ~~## The function is currently defined as~~~
%% ~~function (x) ~~~
%% ~~{~~~
%% ~~  }~~~


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

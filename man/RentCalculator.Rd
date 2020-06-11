\name{RentCalculatoR}
\alias{RentCalculatoR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RentCalculatoR
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
With the package RentCalculatoR you can calculate the change in rent of residential real estate according to Swiss law. Changes in value-adding investments, depreciation, interests and reference interest rates can be taken into account. As well as changings due to changes in the market such as inflation, mortgage and cost increments. 

We provide an Example that uses all functions of the RentCalculatoR in one task. See below.

For further information and more specific examples please consult the help pages of the specific function inside the package:

?RentInformations
?RentInvestCalculatoR
?inf_rate_change
?mortgage_rate_change
?cost_incr_change
?RentCalculatorNewCircum

}
\usage{

}
%- maybe also 'usage' for other objects documented here.

\arguments{
}


\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned

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
##################################################################################################
# EXAMPLE for use of all functions in the RentCalculatoR in one task (PART A to C in the code)
# ----------------------------------------------------------------------------------------------------------
# If you have an apartment that is totally renovated and you have to calculate the new
# rent due to the investments and also due to changings in the market you can use
# the RentInvestCalculatoR together with the RentCalculatorNewCircum very easy.
# You take the two as data frames functions and add them to a new variable.

# Let's say you invested 100000 CHF with a value increasing share of 50 % and a lifespan of 12 years,
# while the rent before the investment was 1000 CHF.

invest <- as.data.frame(RentInvestCalculatoR(1000,100000,50,12))

# Let's say the last adjustments to inflation was on the 01.10.2016, the one to mortgage on the 3.9.2013
# and to the costs on 1.6.2019 you use (supposing the flat rate for the cost increase is 1 %):

market <- as.data.frame(RentCalculatorNewCircum(1000, "2016-10-01", "2013-09-03", "2019-06-01", 1))

# and then add the data with new added monthly value of the investment into the subset of market and
# add up the the sum of market changes (row 1 to 4) and the monthly added rent due to investment
# together to a new line which gives you the TOTAL result:

OverAllNewRent <- rbind(market[(1:4),],
                        c("The change in rent per month due to investment is:", invest$total_add_rent_monthly_CHF),
                        c("And the new TOTAL rent per month in CHF is:", sum(as.numeric(market[(1:4),2]))
                          +invest$total_add_rent_monthly_CHF)
)


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

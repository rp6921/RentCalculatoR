\name{RentCalculatorNewCircum}
\alias{RentCalculatorNewCircum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RentCalculatorNewCircum
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function combines all the elements that affect the change in rent due to changings in the market circumstances like inflation rates (C.1), mortgage rates (C.2) or cost increasing (C.3). Because they are ale relatively expressed to the acutal rent and to the market changes, they can be combined into one. This will lead to actualization in the rent regarding to all market circumstances.
}
\usage{
RentCalculatorNewCircum(act_rent_CHF, inflation_rate_last_date, reference_last_date, cost_incr_last_date, flat_rate = 0.5)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
To calculate the changings due to new market circumstances we need from the user 
  \item{act_rent_CHF}{actual net rent in CHF
%%     ~~Describe \code{x} here~~
}
  \item{inflation_rate_last_date}{the date, when the inflation rate was adjusted the last time 
%%     ~~Describe \code{x} here~~
}
  \item{reference_last_date}{the date, when the mortgage was adjusted the last time 
%%     ~~Describe \code{x} here~~
}
  \item{cost_incr_last_date}{the date, when the cost increasing was adjusted the last time 
%%     ~~Describe \code{x} here~~
}
  \item{flat_rate = 0.5}{the used flat rate between 0 and 1 \%, by default it is 0.5 \%
%%     ~~Describe \code{x} here~~
}
}


\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
The returned values are the actual data such as:
\item{act_rent_CHF}{the actual rent per month in CHF}
\item{inflation_CHF}{the change in rent per month due to inflation in CHF}
\item{mortgage_CHF}{the change in rent per month due to mortgage in CHF}
\item{costs_CHF}{the change in rent per month due to cost increase in CHF}
\item{total_new}{the new total rent per month in CHF}
\item{total_change}{the total change in rent per month in CHF}

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

# Examples to PART C.4 Change in market circumstances
# ----------------------------------------------------------------------------------------------------------
# If you would like to calculate all changings due to changings in market circumstances
# together you can use the RentCalculatorNewCircum function. This combines all PART C.1
# - C.3 in one. It gives you an output and a dataset of the single changings, the total
# new rent and the total changings.

# For example if your actual rent is 1000 CHF and the last adjustments to inflation was on the
# 01.10.2016, the one to mortgage on the 3.9.2013 and to the costs on 1.6.2019 you use
# (supposing the flat rate for the cost increase is 1 %):

RentCalculatorNewCircum(1000, "2016-10-01", "2013-09-03", "2019-06-01", 1)



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

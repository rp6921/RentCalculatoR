\name{mortgage_rate_change}
\alias{mortgage_rate_change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mortgage_rate_change
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The law allows you to calculate the amount of rent new, if there is a change in the mortgage rent (article 12a and 13 VMWG, Tenancy Law Regulation). Therefore you have to know the mortgage rates, which are calculated and published by the Department of Housing (https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html).
To calculate the change you have to know the actual mortgage, which we are elaborating with the RentInformations function. Then you need to know the last date, when the mortgage rate was updated on your rent (reference_last_date). Because the changing have different levels by art. 13 VMWG and also by changing to the positive or negative, the code is a bit more complex. We explain more of the details inside the commented code.
}
\usage{
mortgage_rate_change(act_rent_CHF, reference_last_date)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
To calculate the changings due to changings in mortgage rates we need from the user 
  \item{act_rent_CHF}{actual net rent in CHF
%%     ~~Describe \code{x} here~~
}
  \item{reference_last_date}{the date, when the mortgage rate was the last time adjusted
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
\item{total_add_rent_monthly_CHF}{the total added rent per month in CHF}
\item{total_new_rent_monthly_CHF}{the new total rent per month in CHF}
\item{mortgage_change}{the change in mortgage in \%}
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


# Examples to PART C.2 Mortgage change
# ----------------------------------------------------------------------------------------------------------
# If you like to know the change in your rent due to a change in mortgage. You need to add
# the actual rent in CHF (act_rent_CHF) and the date of the last adjustment due to mortgage
# as date in the format "Years-month-day" (inflation_rate_last_date).

# The function returns you a summary of your actual rent in CHF (act_rent_CHF), the added
# monthly rent in CHF due to mortgage change (total_add_rent_monthly_CHF) and the total new
# monthly rent in CHF (total_new_rent_monthly_CHF). It gives you also the percentage of the
# change in mortgage in % (mortgage_change).

# To know what your actual rent of CHF 1000 changes, when last updated on 3.9.2013 you type:

mortgage_rate_change(1000, "2013-09-03")

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

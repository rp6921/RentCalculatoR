\name{inf_rate_change}
\alias{inf_rate_change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{inf_rate_change
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The law allows you to calculate 40 \% of the inflation rate into the rent price (article 16 VMWG, Tenancy Law Regulation). The inflation rate includes a certain concept of the national consumer price index (LIK). It measures the price development of goods and services that are important for private households. It indicates the extent to which consumers have to increase or decrease spending when prices change in order to keep consumption volume constant (https://www.bfs.admin.ch/bfs/it/home/statistiche/prezzi/rilevazioni/lik/ipc.html). The Tenancy law tells to use this index, which also include rents, to calculate the changes in rents.Therefore we use the inflation index tables from the Department of Statistics (xlsx download, https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master) like we already described in ?RentInformations, which gives us the actual inflation rate. Therefore we need to know of the user the last data of the inflation rate adjustment (inflation_rate_last_date).

}
\usage{
inf_rate_change(act_rent_CHF, inflation_rate_last_date)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
To calculate the changings due to changings in inflation we need from the user 
  \item{act_rent_CHF}{actual net rent in CHF
%%     ~~Describe \code{x} here~~
}
  \item{inflation_rate_last_date}{the date, when the inflation rate was the last time adjusted
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

# Examples to PART C.1 Inflation rates
# ----------------------------------------------------------------------------------------------------------
# If you like to know the change in your rent due to inflation. You need to add the actual
# rent in CHF (act_rent_CHF) and the date of the last adjustment due to inflation as date
# in the format "Years-month-day" (inflation_rate_last_date).

# The function returns you a summary of your actual rent in CHF (act_rent_CHF), the added
# monthly rent in CHF due to inflation (total_add_rent_monthly_CHF) and the total new
# monthly rent in CHF (total_new_rent_monthly_CHF). It gives you also the allowed inflation
# rate due to the 40 % allowed change (allowed_inflation_rate).

# To know what your actual rent of CHF 1000 changes, when last updated on 1.6.2019 you type:

inf_rate_change(1000, "2019-06-01")

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

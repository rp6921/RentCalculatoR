\name{cost_incr_change}
\alias{cost_incr_change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cost_incr_change
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The good practice allows you to add the changes due to cost increase up to 1 \% per year. The calculator needs the actual rent in CHF (act_rent_CHF) and the date of the last adjustment of the rent due to cost increase (cost_incr_last_date). The function uses whole months to to calculate the difference. So if you don't know exactly the day of the last adjustment, it doesn't matter. Additionally you can also declare which rate between 0 and 1 \% you like to use (flat_rate). By default its set to 0.5 \%.
}
\usage{
cost_incr_change(act_rent_CHF, cost_incr_last_date, flat_rate = 0.5)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
To calculate the changings due to changings in cost increases we need from the user 
  \item{act_rent_CHF}{actual net rent in CHF
%%     ~~Describe \code{x} here~~
}
  \item{cost_incr_last_date}{the date, when the costs were the last time adjusted
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
\item{total_add_rent_monthly_CHF}{the total added rent per month in CHF}
\item{total_new_rent_monthly_CHF}{the new total rent per month in CHF}
\item{cost_diff_rate*flat_rate}{the change in cost increases per year in \%}
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
# Examples to PART C.3 Cost increase
# ----------------------------------------------------------------------------------------------------------
# If the last adjustment of the rent was in june 2019, the actual rent in CHF is 1000 and you let the flat rate
# at the default level of 0.5 %, you use:
cost_incr_change(1000, "2019-06-01")

# If you like to have the same but with a flat rate of 1 % you use:
cost_incr_change(1000, "2019-06-01", 1)




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

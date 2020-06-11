\name{RentInformations}
\alias{RentInformations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RentInformations
%%  ~~function to do ... ~~

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The RentIformations funtion is part of the RentCalculatoR package and gets the actual relevant information about rents in Switzerland from the system or from scraping of the webpage of the Department of Statistics or the Department of Housing. It takes 1. current relevant and official mortgage rent (in \%'); from https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html, 2. the current date for the calculation on inflation and on general cost increases, 3. the official information about the inflation rates from xlsx-file on https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master/ 
}
\usage{
RentInformations()
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{none}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
The returned values are the actual data such as:
\item{act_mortgage}{the actual mortgage rent}
\item{act_date}{the actual date}
\item{act_inf_rate}{the actual inflation in points}
Inside a vector
\item{act_data(act_mortgage, act_date, act_infl_rate)}{vector of the actual data}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\references{
%% ~put references to the literature/web site here ~

https://stat.ethz.ch/R-manual/
http://www.wolferonline.de/xmlR/XMLinR.pdf
https://de.wikibooks.org/wiki/GNU_R
https://stackoverflow.com/ ~}

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
# If you would like to know what the actual relevant data for rents is and save it to a variable act_results.
act_results <- RentInformations()

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

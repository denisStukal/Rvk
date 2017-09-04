\name{searchNewsfeed}


\alias{searchNewsfeed}


\title{
Search for Posts Mentioning a String
}


\description{
Returns a data.frame with publicly available information on all VK posts that mention a specific query.
}


\usage{
searchNewsfeed(query, access_token, start_time = NULL, end_time = NULL, verbose = FALSE)
}


\arguments{
  \item{query}{A string to search in VK posts. }
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{start_time}{Start date and time (UTC) for posts to retrieve. If NULL (default), a day before the current moment is assumed.}
  \item{end_time}{End date and time (UTC) for posts to retrieve. If NULL (default), the current moment is assumed.}
  \item{verbose}{Boolean: suppress informational messages if FALSE (default).}
}


\details{
Time is specified as \href{https://en.wikipedia.org/wiki/Coordinated_Universal_Time}{UTC}. 

}


\value{
A data.frame with information on posts and their attachments. 

}


\references{
VK API Methods: Newsfeed (\url{https://vk.com/dev/newsfeed.search})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
newposts <- searchNewsfeed(query = 'moscow', access_token = mytoken)
str(newposts)
}
}


\keyword{methods}


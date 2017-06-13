\name{getUserWallSearchCount}


\alias{getUserWallSearchCount}


\title{
Count Posts/Replies from a User's Page that Mention a Query String
}


\description{
Returns the number of posts/replies that mention a query string.
}


\usage{
getUserWallSearchCount(user_id, query, access_token)
}


\arguments{
  \item{user_id}{  A numeric user ID. Available from the user's page (see Details). }
  \item{query}{ A string to search on a user's page. }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

}


\value{
Numeric (the number of posts/replies that mention a query string)
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.search})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
myquery = 'Obama'
dat <- getUserWallSearchCount(user_id = userid, query = myquery, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


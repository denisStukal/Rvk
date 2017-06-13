\name{getGroupWallSearchCount}


\alias{getGroupWallSearchCount}


\title{
Count Posts/Replies from a Group Page that Mention a Query String
}


\description{
Returns the number of posts/replies that mention a query string.
}


\usage{
getGroupWallSearchCount(group_id, query, access_token)
}


\arguments{
  \item{group_id}{  A numeric ID of the VK group. Available from the group's page (see Details). }
  \item{query}{ A string to search on a group page. }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
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
groupid = AAAAAAAA
myquery = 'Obama'
dat <- getGroupWallSearchCount(group_id = groupid, query = myquery, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


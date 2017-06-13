\name{getGroupWallLikes}


\alias{getGroupWallLikes}


\title{
Get Likes to Posts from a Group Page
}


\description{
Returns a list of vectors with user IDs who liked posts on a group page. Each vector of user IDs corresponds to a post those users liked. 
}


\usage{
getGroupWallLikes(group_id, access_token, verbose = FALSE)
}


\arguments{
  \item{group_id}{ A numeric ID for the VK group. Available from the group's page (see Details). }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
  \item{verbose}{Boolean: if FALSE (default), informational messages are suppressed.}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}

\value{
A list of vectors with user IDs who liked posts on a group page. Each vector of user IDs corresponds to a post those users liked. 
}


\references{
VK API Methods: Likes (\url{https://vk.com/dev/likes.getList})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
dat <- getGroupWallLikes(group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


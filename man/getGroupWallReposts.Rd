\name{getGroupWallReposts}


\alias{getGroupWallReposts}


\title{
Get Reposts of Posts from a Group Page
}


\description{
Returns a data.frame with information about reposts of posts on a group page. 
}


\usage{
getGroupWallReposts(group_id, access_token, verbose = FALSE)
}


\arguments{
  \item{group_id}{A numeric ID of a VK group. Available from the group's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken).}
  \item{verbose}{Boolean: if FALSE (default), informational messages are suppressed.}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
Returns a data.frame with columns:
\item{repost_id}{A numeric repost ID}
\item{reposter_id}{A numeric user ID who made the repost}
\item{receiver_id}{A numeric user ID to whose page the repost was made}
\item{date}{Date of the repost}
\item{text}{Text of the repost}
\item{num_comments}{Number of comment the repost received}
\item{num_likes}{Number of likes the repost received}
\item{num_reposts}{Number of reposts of the repost}
\item{num_views}{Number of views of the repost}
\item{reposted_user_id}{}
\item{repost_of_post_id}{}
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.getReposts})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
dat <- getGroupWallReposts(group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


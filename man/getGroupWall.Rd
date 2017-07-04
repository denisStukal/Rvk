\name{getGroupWall}


\alias{getGroupWall}


\title{
Get Posts from a Group's Wall
}


\description{
Returns a data.frame with publicly available information about posts on a group's wall.
}


\usage{
getGroupWall(group_id, access_token, num_posts = 'all', verbose = FALSE)
}


\arguments{
  \item{group_id}{A numeric ID for the VK group. Available from the group's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken).}
  \item{num_posts}{Number of posts to retrieve.}
  \item{verbose}{Boolean: suppress informational messages if FALSE (default).}
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call.

}


\value{
A data.frame with columns:
\item{id}{Post ID}
\item{date}{Date when a post was made}
\item{date_POSIXct}{Number of seconds since January 1, 1970 to the moment when a post was made}
\item{text}{Post text}
\item{comments_count}{Number of comments to a post}
\item{likes_count}{Number of likes of a post}
\item{reposts_count}{Number of reposts of a post}
\item{reposted}{Flag for a repost (1 if a post is a repost) }
\item{attachment}{Flag for attachments in a post}
Additional columns with information about reposts and attachments. See References for details. 
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
postid = BBBBBB
dat <- getGroupWall(group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


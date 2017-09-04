\name{getGroupMostCommentingUsers}


\alias{getGroupMostCommentingUsers}


\title{
Get Users who Leave Most Comments on a Group Page
}


\description{
Returns a data.frame with IDs of users who commented posts on a group page, and the number of times they left a comment. The data.frame is sorted by the number of comments: most commenting users first.
}


\usage{
getGroupMostCommentingUsers(group_id, access_token, num_posts = 'all', 
    num_users = 'all', verbose = FALSE)
}


\arguments{
  \item{group_id}{A numeric ID for the VK group. Available from the group's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{num_posts}{Number of posts to consider.}
  \item{num_users}{Number of top users with most comments to report. Either 'all' (default) or numeric.}
  \item{verbose}{Boolean: print informational messages if TRUE (default).}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
A data.frame with columns:
\item{group_id}{User ID who left comments.}
\item{num_comments}{Number of comments a given users left on the retrieved number of posts.}
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
# Show 10 users with most comments on a group page
dat <- getGroupMostCommentingUsers(group_id = groupid, access_token = mytoken, num_users = 10) 
str(dat)
}
}


\keyword{methods}


\name{getGroupMostLikingUsers}


\alias{getGroupMostLikingUsers}


\title{
Get Users who Leave Most Likes on a Group Page
}


\description{
Returns a data.frame with IDs of users who liked posts on a group page, and the number of times they left a like. The data.frame is sorted by the number of likes: most liking users first.
}


\usage{
getGroupMostLikingUsers(group_id, access_token, num_users = 'all', verbose = FALSE)
}


\arguments{
  \item{group_id}{A numeric ID for the VK group. Available from the group's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{num_users}{Number of top users with most likes to report. Either 'all' (default) or numeric.}
  \item{verbose}{Boolean: suppress informational messages if FALSE (default).}
}


\details{
During function execution, one is requested to specify the number of posts to retrieve. The list of users and the number of likes are based on the retrieved posts only. 

Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
A data.frame with columns:
\item{user_id}{User ID who left likes.}
\item{num_likes}{Number of likes a given users left on the retrieved number of posts.}
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
# Show 10 users with most likes on a group page
dat <- getGroupMostLikingUsers(group_id = groupid, access_token = mytoken, num_users = 10) 
str(dat)
}
}


\keyword{methods}


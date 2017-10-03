\name{getUserMostRepostingUsers}


\alias{getUserMostRepostingUsers}


\title{
Get Users who Make Most Reposts from a User's Page
}


\description{
Returns a data.frame with IDs of users who reposted posts from a user's page, and the number of times they made a repost. The data.frame is sorted by the number of reposts: most reposting users first.
}


\usage{
getUserMostRepostingUsers(user_id, access_token, num_posts = 'all', 
    num_users = 'all', verbose = FALSE)
}


\arguments{
  \item{user_id}{A numeric user ID. Available from the user's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{num_posts}{Number of posts to consider.}
  \item{num_users}{Number of top users with most reposts to report. Either 'all' (default) or numeric.}
  \item{verbose}{Boolean: print informational messages if TRUE (default).}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 

The command returns negative IDs for VK groups and positive IDs for individual users.
}


\value{
A data.frame with columns:
\item{user_id}{User ID who made reposts.}
\item{num_reposts}{Number of reposts a given user made from the retrieved number of posts.}
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
userid = AAAAAAAA
# Show 10 users with most reposts on a user's page
dat <- getUserMostRepostingUsers(user_id = userid, access_token = mytoken, num_users = 10) 
str(dat)
}
}


\keyword{methods}


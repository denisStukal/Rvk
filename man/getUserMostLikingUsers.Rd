\name{getUserMostLikingUsers}


\alias{getUserMostLikingUsers}


\title{
Get Users who Leave Most Likes on a User's Page
}


\description{
Returns a data.frame with IDs of users who liked posts on a user's page, and the number of times they left a like. The data.frame is sorted by the number of likes: most liking users first.
}


\usage{
getUserMostLikingUsers(user_id, access_token, num_posts = 'all', 
    num_users = 'all', verbose = FALSE)
}


\arguments{
  \item{user_id}{A numeric user ID. Available from the user's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{num_posts}{Number of posts to use when counting likes. Either 'all' (default) or a positive integer.}
  \item{num_users}{Number of top users with most likes to report. Either 'all' (default) or numeric.}
  \item{verbose}{Boolean: suppress informational messages if FALSE (default).}
}


\details{
During function execution, one is requested to specify the number of posts to retrieve. The list of users and the number of likes are based on the retrieved posts only. 

Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

The command returns negative IDs for VK groups and positive IDs for individual users.

}


\value{
A data.frame with columns:
\item{user_id}{User ID who left likes.}
\item{num_likes}{Number of likes a given user left on the retrieved number of posts.}
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
userid = AAAAAAAA
# Show 10 users with most likes on a user's page
dat <- getUserMostLikingUsers(user_id = userid, access_token = mytoken, num_users = 10) 
str(dat)
}
}


\keyword{methods}


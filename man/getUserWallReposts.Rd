\name{getUserWallReposts}


\alias{getUserWallReposts}


\title{
Get Reposts of Posts from a User's Page
}


\description{
Returns a data.frame with information about reposts of posts on a user's page. 
}


\usage{
getUserWallReposts(user_id, access_token, num_posts = 'all', verbose = FALSE)
}


\arguments{
  \item{user_id}{A numeric ID of a VK user. Available from the user's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken).}
  \item{num_posts}{Number of posts to consider. Either 'all' (default) or a positive integer.}
  \item{verbose}{Boolean: if FALSE (default), informational messages are suppressed.}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 
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
userid = AAAAAAAA
dat <- getUserWallReposts(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


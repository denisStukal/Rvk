\name{getUserPostLikes}


\alias{getUserPostLikes}


\title{
Get IDs of all Users who Liked a Post on a User's Page
}


\description{
Returns a vector of user IDs who liked a post on a user's page.
}


\usage{
getUserPostLikes(user_id, post_id, access_token)
}


\arguments{
  \item{user_id}{A numeric user ID. Available from the user's page (see Details).}
  \item{post_id}{A numeric ID of a post on a user's page.}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 
 
}


\value{
A vector of user IDs.
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
postid = BBBBBB
dat <- getUserPostLikes(user_id = userid, 
    post_id = postid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


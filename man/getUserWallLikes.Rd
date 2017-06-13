\name{getUserWallLikes}


\alias{getUserWallLikes}


\title{
Get Likes to Posts from a User Page
}


\description{
Returns a list of vectors with user IDs who liked posts on a user page. Each vector of user IDs corresponds to a post those users liked. 
}


\usage{
getUserWallLikes(user_id, access_token, verbose = FALSE)
}


\arguments{
  \item{user_id}{ A numeric ID of a user. Available from the users's page (see Details). }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
  \item{verbose}{Boolean: if FALSE (default), informational messages are suppressed.}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

}

\value{
A list of vectors with user IDs who liked the user's posts. Each vector of user IDs corresponds to a post those users liked. 
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
dat <- getUserWallLikes(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


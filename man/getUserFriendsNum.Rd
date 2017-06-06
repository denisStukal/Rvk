\name{getUserFriendsNum}


\alias{getUserFriendsNum}


\title{
Get Number of Friends of a User
}


\description{
Returns the number of friends a user has.
}


\usage{
getUserFriendsNum(user_id, access_token)
}


\arguments{
  \item{user_id}{A numeric id for the user whose friends are requested. Available from the user's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

Universities, schools, and jobs are sorted from last to first by the start year (if available). Last position is returned by default.
}


\value{
Integer (number of followers)
}


\references{
VK API Methods: Friends (\url{https://vk.com/dev/friends.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
dat <- getUserFriendsNum(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


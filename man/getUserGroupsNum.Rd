\name{getUserGroupsNum}


\alias{getUserGroupsNum}


\title{
Get Number of Groups a User is a Member of
}


\description{
Returns the number of groups a user is a member of.
}


\usage{
getUserGroupsNum(user_id, access_token)
}


\arguments{
  \item{user_id}{A numeric id for the user whose friends are requested. Available from the user's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

}


\value{
Integer (number of groups)
}


\references{
VK API Methods: Groups (\url{https://vk.com/dev/groups.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
dat <- getUserGroupsNum(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


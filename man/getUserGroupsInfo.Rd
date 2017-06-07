\name{getUserGroupsInfo}


\alias{getUserGroupsInfo}


\title{
Get Information on Groups a User Belongs to
}


\description{
Returns a data.frame with information about groups which a user belongs to. 
}


\usage{
getUserGroupsInfo(user_id, access_token)
}



\arguments{
  \item{user_id}{A numeric id for the user whose friends are requested. Available from the user's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

Universities, schools, and jobs are sorted from last to first by the start year (if available). Last position is returned by default.

}


\value{
A data.frame with columns (can vary due to information availability):
\item{id}{Group ID}
\item{name}{Group name}
\item{screen_name}{Group screen name}
\item{is_closed}{Flag for group type (0 - open, 1 - closed, 2 - private)}
\item{type}{Group, or public page, or event }
\item{photo_50}{URL address for the 50px-wide group logo}
\item{...}{Other fields if available}
For more information about fields, see References.
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
dat <- getUserFriendsInfo(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


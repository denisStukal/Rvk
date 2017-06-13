\name{getUserFriendsInfo}


\alias{getUserFriendsInfo}


\title{
Get Information on a User's Friends
}


\description{
Returns a data.frame with publicly available information about friends of a user.
}


\usage{
getUserFriendsInfo(user_id, access_token)
}


\arguments{
  \item{user_id}{Numeric ID of the user whose friends are requested. Available from the user's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

Universities, schools, and jobs are returned as data.frames.

}


\value{
A data.frame with columns (can vary due to information availability):
\item{id}{User ID of a friend}
\item{first_name}{Friend's first name}
\item{last_name}{Friend's last name}
\item{sex}{Friend's sex (available options: female, male)}
\item{screen_name}{Friend's screen name}
\item{bdate}{Friend's birth date}
\item{city}{City code the friend specified in the "Contacts" section}
\item{country}{Country code the friend specified in the "Contacts" section}
\item{relation}{Friend's relationship status (1 - single; 2 - in a relationship; 3 - engaged; 4 - married; 5 - it's complicated; 6 - actively searching; 7 - in love; 8 - in a civil union; 0 - not specified.)}
\item{...}{Other fields if available}
For more information about fields, see References.
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
dat <- getUserFriendsInfo(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


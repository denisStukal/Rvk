\name{getMultiUserInfo}


\alias{getMultiUserInfo}


\title{
Get Information about Multiple Users
}


\description{
Returns a data.frame with publicly available information about multiple users. 
}


\usage{
getMultiUserInfo(user_ids, access_token)
}


\arguments{
  \item{user_ids}{Vector of numeric user IDs. Available from users' pages (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

Universities, schools, and jobs are returned as data.frames.

}


\value{
A data.frame with columns (can vary due to information availability):
\item{id}{User ID}
\item{first_name}{User's first name}
\item{last_name}{User's last name}
\item{sex}{User's sex (available options: female, male)}
\item{screen_name}{User's screen name}
\item{bdate}{User's birth date}
\item{city}{City code the user specified in the "Contacts" section}
\item{country}{Country code the user specified in the "Contacts" section}
\item{relation}{User's relationship status (1 - single; 2 - in a relationship; 3 - engaged; 4 - married; 5 - it's complicated; 6 - actively searching; 7 - in love; 8 - in a civil union; 0 - not specified.)}
\item{...}{Other fields if available}
}


\references{
VK API Methods: Users (\url{https://vk.com/dev/users.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userids = c(AAAAAAAA, BBBBBB, CCCCCCC)
dat <- getMultiUserInfo(user_ids = userids, access_token = mytoken)
str(dat)
}
}


\keyword{methods}


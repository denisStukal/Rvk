\name{makeAccessToken}


\alias{makeAccessToken}

\title{
Create VK Access Token
}


\description{
Creates a permanent VK access token using Implicit Flow.
}


\usage{
makeAccessToken(token_file_name)
}


\arguments{
  \item{token_file_name}{File name to save access token}
}

\details{
Getting user access token involves several steps:

Step 0. Login your VK account. If you don't have one yet, sign up here: \url{https://vk.com/}

Step 1. Go to VK Developers page (\url{https://vk.com/dev}) and choose My Apps on the top panel. If you haven't created an application yet, create one by clicking Create an Application on the top right of the page. Give it a title and choose Standalone Application. After you enter you phone number in the pop-up window, you will receive a text message with a confirmation code. Enter the code, and you will be redirected to a page with information on your application. Choose Settings on the top left panel and remember your Application ID (7-digits code).

Step 2. Run Rvk::makeAccessToken(token_file_name). It will create a file with the name you choose to store your user access token. The function will first ask whether you read these instructions: type y. Then type Application ID from Step 1. You will be redirected to a web page requesting access to your account for your application. Choose Allow. You will be redirected to a new page. Copy its URL and paste into R after \dQuote{Paste it here}. Press Enter. 

Step 3. Congrats! You created your user access token and saved it in a file with a name you specified in your working directory. Do **NOT** share your access token with anybody else and keep it in a safe place! Otherwise you can loose access to your account. Load your user access token as shown in the Examples section below.
}



\references{
VK API Documentation: Implicit Flow for User Access Token (\url{https://vk.com/dev/implicit_flow_user})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
# Create the mytoken_file file in the working directory
makeAccessToken('mytoken_file')

# Read user access token from mytoken_file into memory
mytoken <- readLines(con = 'mytoken_file') 
}
}


\keyword{methods}


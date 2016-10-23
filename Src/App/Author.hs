module App.Author

( authorApp
) where

import           API
import           Data.Time
import           Model
import           Servant

authorApp :: Server AuthorAPI
authorApp =
    getAuthors :<|>
    getAuthor  :<|>
    postAuthor

-- getAuthors :: Handler [Author]
getAuthors = return [gwen]

-- getAuthor :: AuthorId -> Handler Author
getAuthor _ = return gwen

-- postAuthor :: Author -> Handler Author
postAuthor _ = return gwen

-- gwen :: Author
gwen = Author "Gwen" "Lofman" "the best" "lofman.co/img/profile.jpg"

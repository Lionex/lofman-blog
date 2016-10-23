module App.Blog
( blogApp
) where

import           API
import           Data.Time
import           Model
import           Servant

blogApp :: Server BlogAPI
blogApp =
    getBlogs :<|>
    getBlog  :<|>
    postBlog

-- getBlogs :: Handler [BlogPost]
getBlogs = return [post]

-- getBlog :: BlogPostId -> Handler BlogPost
getBlog _ = return post

-- postBlog :: BlogPost -> Handler BlogPost
postBlog _ = return post

post :: BlogPost
post = BlogPost "title" "Essay" "this is an essay" 124123 12

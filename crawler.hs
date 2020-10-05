{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import System.FilePath
import System.Directory
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Foldable
import Network.Wreq
import qualified Data.ByteString.Lazy.Internal as BI
import qualified Data.Text as T
import Text.HTML.Scalpel

data Page = Blob URL
          | Tree [URL]
            deriving (Show, Eq)

spider :: String -> String -> IO (Maybe [String])
spider base path = scrapeURL (base++path) jq `andThen` nextLayer base

andThen :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
andThen a b = runMaybeT $ MaybeT a >>= MaybeT . b

nextLayer :: String -> Page -> IO (Maybe [String])
nextLayer base (Blob u) = do
    let url = base ++ u
    let p = ("master/" ++) . T.unpack . last . T.splitOn "/master/" . T.pack $ url
    putStrLn $ url ++ " -> " ++ p
    resp <- get url
    createDirectoryIfMissing True $ takeDirectory p
    writeFile p $ BI.unpackChars $ resp ^. responseBody
    return $ Just [u]
nextLayer base (Tree us) = foldMap (spider base) us

jq :: Scraper String Page
jq = blob <|> tree
    where blob = fmap Blob $ chroot ("a"@:["id"@="raw-url"]) href
          tree = fmap Tree $ chroots ("div"@:["role"@="rowheader"] // "a"@:[hasClass "link-gray-dark"]) href
          href = attr "href" "a"

main :: IO ()
main = do
    let base = "https://github.com"
    let path = "/godhand4826/dotfiles/"
    a <- spider base path
    -- print a
    return ()


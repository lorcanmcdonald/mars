-- |Types representing items entered at the Mars command line
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Mars.Command
where
import Data.Aeson
import Data.Maybe
import qualified Data.Vector as Vector
import Network.URL
import Mars.Types
-- import Tests.Mars.Arbitraries()
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text

prependToQuery :: Query -> Query -> Query
prependToQuery (Query a) (Query b) = Query (a ++ b)

-- | The initial state
initialState :: State
initialState = State { url      = Nothing
                     , path     = Query []
                     , document = Nothing
                     }

-- |Output a command in a format similar to how it would have be entered by the user
renderCommand :: Command -> Text.Text
renderCommand (Get Nothing)  = Text.pack "get"
renderCommand (Get (Just u)) = Text.append (Text.pack "get ") (Text.pack $ exportURL u)
renderCommand (Cat [])  = Text.pack "cat"
renderCommand (Cat l) = Text.append (Text.pack "cat ") (Text.intercalate (Text.pack " ") $ map renderQuery l)
renderCommand (Ls Nothing)   = Text.pack "ls"
renderCommand (Ls (Just a))  = Text.append (Text.pack "ls ") (renderQuery a)
renderCommand (Save f)       = Text.append (Text.pack "save ") f
renderCommand (Load f)       = Text.append (Text.pack "load ") f
renderCommand (Update q val) = Text.pack "update " |++| renderQuery q |++| Text.pack " " |++| Text.pack ( ByteString.unpack $ encode val)
renderCommand Href           = Text.pack "href"
renderCommand Pwd            = Text.pack "pwd"
renderCommand (Cd a)         = Text.append (Text.pack "cd ") (renderQuery a)

(|++|) :: Text.Text -> Text.Text -> Text.Text
(|++|) = Text.append

-- |Output a query in a format that would have been entered in the interpreter
renderQuery :: Query -> Text.Text
renderQuery (Query l) = Text.intercalate (Text.pack "/") $ map renderQueryItem l

-- |A text version of a QueryItem
renderQueryItem :: QueryItem -> Text.Text
renderQueryItem (NamedItem n) = n
renderQueryItem (IndexedItem i) = Text.pack (show i)
renderQueryItem WildCardItem = Text.pack "*"
renderQueryItem LevelAbove = Text.pack ".."

fromValue :: Value -> Maybe CollectionValue
fromValue (Object o) = Just $ O o
fromValue (Array a)  = Just $ A a
fromValue _            = Nothing

toValue :: CollectionValue -> Value
toValue (O o) = Object o
toValue (A a) = Array a

emptyObjectCollection :: CollectionValue
emptyObjectCollection = case object [] of
                        Object o -> O o
                        _ -> undefined
moveUp :: Query -> Query
moveUp (Query q) =  Query $ reverse $ drop 1 $ reverse q

modifyDoc :: CollectionValue -> Query -> Value -> Either CollectionValue CollectionValue
modifyDoc (A a) (Query []) _              = Left . A $ a
modifyDoc (O o) (Query []) _              = Left . O $ o
modifyDoc (A a) (Query [IndexedItem x]) v = Right . A . Vector.update a . Vector.fromList $ [(x, v)]
modifyDoc (O o) (Query [NamedItem n]) v   = Right . O $ Map.insert n v o
modifyDoc cv (Query (_:xs)) v             = modifyDoc cv (Query xs) v

simplifyQuery :: Query -> Query
simplifyQuery (Query l) = Query $ reverse $ foldr simplify [] $ reverse l
    where
        simplify :: QueryItem -> [QueryItem] -> [QueryItem]
        simplify (LevelAbove) processed    = drop 1 processed
        simplify item processed   = item:processed

queryDoc :: CollectionValue -> Query -> [Value]
queryDoc v q = queryDoc' (simplifyQuery q) v
    where
        queryDoc' :: Query -> CollectionValue -> [Value]
        queryDoc' (Query []) v'               = [ toValue v' ]
        queryDoc' (Query (IndexedItem q':qs)) (A v') =  case (Vector.!?) v' q' of
                                                Nothing         -> []
                                                Just (Array n)  -> queryDoc' (Query qs) (A n)
                                                Just (Object n) -> queryDoc' (Query qs) (O n)
                                                Just a -> [a]
        queryDoc' (Query (IndexedItem _: _)) (O _) = []
        queryDoc' (Query (NamedItem q':qs)) (O v')  =  case Map.lookup q' v' of
                                                Nothing  -> []
                                                Just (Array n)  -> queryDoc' (Query qs) (A n)
                                                Just (Object n) -> queryDoc' (Query qs) (O n)
                                                Just a -> [a]
        queryDoc' (Query (NamedItem _: _)) (A _) = []
        queryDoc' (Query (WildCardItem:qs)) (O v')   =  concatMap (queryDoc' (Query qs)) $ mapMaybe fromValue $ Map.elems v'
        queryDoc' (Query (WildCardItem:qs)) (A v')   =  concatMap (queryDoc' (Query qs)) $ mapMaybe fromValue $ Vector.toList v'
        queryDoc' (Query (LevelAbove:_)) _ = [] -- This seems like a poor way to fail?

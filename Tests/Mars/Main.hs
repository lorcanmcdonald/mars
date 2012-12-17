import Data.Monoid
import Debug.Trace
import Mars.Command
import Mars.Types
import Mars.Parser
import Data.Aeson.Types
import Tests.Mars.Arbitraries()
import Test.Framework (defaultMain, testGroup, Test)
import Test.QuickCheck ((==>), elements)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO()
main = defaultMain tests


tests :: [Test]
tests = [
            testGroup "Query Tests"
                [ testProperty "command parse unparse" prop_command_parse
                , testProperty "query parse unparse" prop_query_parse
                , testProperty "move up shortens" prop_move_up_shorten
                , testProperty "modifyDoc modifies Document" prop_modifyDoc_modifies
            ]
        ]

prop_command_parse :: Command -> Bool
prop_command_parse c = case parser (renderCommand c) of
                Left _      -> trace (show . renderCommand $ c) False
                Right []    -> trace (show . renderCommand $ c) False
                Right (x:_) -> x == c

prop_query_parse :: Query -> Bool
prop_query_parse q = case parseQuery (renderQuery q) of
                Left _ -> False
                Right qry  -> qry == q

prop_move_up_shorten :: Query -> Bool
prop_move_up_shorten q = len(moveUp q) <= len q
    where
        len (Query l) = length l

prop_modifyDoc_modifies doc q new = all id [isCollection doc] ==> (queryDoc updatedDoc q == [new])
            where
                updatedDoc = modifyDoc doc q new
                isCollection (Array _)  = True
                isCollection (Object _) = True
                isCollection _          = False


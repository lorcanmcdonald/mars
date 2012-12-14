import Data.Attoparsec
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
--                , testProperty "prepend increases length" prop_prepend_inc
--                , testProperty "down and back up" prop_down_up
                , testProperty "move up shortens" prop_move_up_shorten
                --, testProperty "commandParser and handparser the same" prop_comp_parsers
                --, testProperty "state parse unparse" prop_state_parse
                , testProperty "modifyDoc modifies Document" prop_modifyDoc_modifies
            ]
        ]

prop_command_parse :: Command -> Bool
prop_command_parse c = case parser (renderCommand c) of
                Fail _ _ _   -> False
                Partial _    -> False
                Done _ []    -> False
                Done _ (x:_) -> x == c

--prop_state_parse :: State -> Bool
--prop_state_parse state = state == (decode $ encode state)

prop_query_parse :: Query -> Bool
prop_query_parse q = case parseQuery (renderQuery q) of
                Fail _ _ _ -> False
                Partial _  -> False
                Done _ qry -> qry == q

prop_move_up_shorten :: Query -> Bool
prop_move_up_shorten q = len(moveUp q) <= len q
    where
        len (Query l) = length l

prop_modifyDoc_modifies doc q new = (all id [isCollection doc]) ==> (queryDoc updatedDoc q == [new])
            where
                updatedDoc = modifyDoc doc q new
                isCollection (Array _)  = True
                isCollection (Object _) = True
                isCollection _          = False


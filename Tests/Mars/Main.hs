import Debug.Trace
import Mars.Command
import Mars.Types
-- import Data.Aeson.Types
import Tests.Mars.Arbitraries()
import Test.Framework (defaultMain, testGroup, Test)
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
                -- , testProperty "modifyDoc modifies Document" prop_modifyDoc_modifies
            ]
        ]

prop_command_parse :: Command -> Bool
prop_command_parse c = case parser (renderCommand c) of
                Left _      -> False
                Right []    -> False
                Right (x:_) -> x == c

--prop_state_parse :: State -> Bool
--prop_state_parse state = state == (decode $ encode state)

prop_query_parse :: Query -> Bool
prop_query_parse q = case parseQuery (renderQuery q) of
                Left _ -> False
                Right qry  -> qry == q

prop_move_up_shorten :: Query -> Bool
prop_move_up_shorten q = len(moveUp q) <= len q
    where
        len (Query l) = length l

-- prop_modifyDoc_modifies :: Value -> Query -> Value -> Bool
-- prop_modifyDoc_modifies doc q new = case modifyDoc doc q new of
--                                     Left _ ->trace (show $ queryDoc doc q) $ queryDoc doc q == [] -- If the
--                                                                    --  modification fails check to see if it
--                                                                    --  should have failed
--                                     Right updatedDoc -> queryDoc updatedDoc q == [new]

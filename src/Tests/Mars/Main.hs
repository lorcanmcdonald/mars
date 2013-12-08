import Data.Aeson
import Data.Vector (singleton)
import qualified Data.Text as Text
import Debug.Trace
import Mars.Command
import Mars.Types
import Mars.Parser
import Tests.Mars.Arbitraries()
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Martian Tests" [ unitTests, queryProperties]

queryProperties :: TestTree
queryProperties = testGroup "Query Tests"
                [ testProperty "command parse unparse" prop_command_parse
                , testProperty "query parse unparse" prop_query_parse
                , testProperty "move up shortens" prop_move_up_shorten
                , testProperty "modifyDoc modifies Document" prop_modifyDoc_modifies
            ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
          [ testCase "Can query array" $
            queryDoc (Array . singleton . String . Text.pack $ "1") (Query [IndexedItem 0]) @?= [String . Text.pack $ "1"]
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

prop_modifyDoc_modifies doc q new = and [isCollection doc] ==> (queryDoc updatedDoc q == [new])
            where
                updatedDoc = modifyDoc doc q new
                isCollection (Array _)  = True
                isCollection (Object _) = True
                isCollection _          = False


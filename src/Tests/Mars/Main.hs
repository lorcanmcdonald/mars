{-#LANGUAGE OverloadedStrings#-}
import Data.Aeson
import qualified Data.Vector  as Vector
import qualified Data.HashMap.Strict as HashMap
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
            ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
          [ testCase "Can query array" $
            queryDoc (Array . Vector.fromList $ [ "1", "2", "3", "4"])
                     (Query [IndexedItem 3]) @?= ["4"]
          , testCase "Can query long array" $
            queryDoc (Array . Vector.singleton $ "1")
                     (Query [IndexedItem 0]) @?= ["1"]
          , testCase "Can query nested arrays" testNestedArray
          , testCase "Modify document" $
            modifyDoc (Array (Vector.fromList [Number 1, Number 2, Number 3])) (Query [IndexedItem 2]) (Number 4) @?= Array (Vector.fromList [Number 1, Number 2, Number 4])
          ]

testNestedArray = queryDoc value query @?= [ "a"]
    where
        value = Array (Vector.fromList [Array (Vector.fromList ["a"])])
        query = Query [IndexedItem 0, IndexedItem 0]

testNestedObject = queryDoc value query @?= [ "b"]
    where
        value = Object (
            HashMap.fromList
            [
                ("a", Object (
                    HashMap.fromList [
                        ("b", "Test")
                    ]
                ))
            ])
        query = Query [NamedItem "a", NamedItem "b"]

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


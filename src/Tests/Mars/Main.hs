{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import Data.String.Conv
import qualified Data.Vector as Vector
import Mars.Command
import Mars.Parser
import Mars.Types
import Test.Tasty
import Data.Text (Text)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Mars.Arbitraries ()

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Martian Tests"
    [ unitTests,
      queryProperties
    ]

queryProperties :: TestTree
queryProperties =
  testGroup
    "Query Tests"
    [ testProperty "command parse unparse" prop_command_parse,
      testProperty "query parse unparse" prop_query_parse,
      testProperty "move up shortens" prop_move_up_shorten
    ]

parseCase :: String -> [Command] -> TestTree
parseCase s q = testCase s $ parser (toS s) @?= Right q

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testGroup
        "Parsing Commands"
        [ parseCase "ls" [Ls (Query [])],
          testCase "ls *" $
            parser "ls *"
              @?= Right
                [Ls (Query [Glob . NonEmpty.fromList $ [AnyCharMultiple]])],
          testCase
            "ls b*"
            $ parser "ls b*" @?= Right [Ls (Query [Glob . NonEmpty.fromList $ [LiteralString "b", AnyCharMultiple]])]
        ],
      testGroup
        "Parsing Queries"
        [ testCase "empty query" $
            parseQuery "" @?= Right (Query [])
        ],
      testGroup
        "Glob Patterns"
        [ testCase "exact match" $
            globKeys
              ( HashMap.fromList
                  [ ("a", Number 1),
                    ("b", Number 2)
                  ]
              )
              (NonEmpty.fromList [LiteralString "a"])
              @?= ["a"]
        ],
      testGroup
        "General"
        [ testCase "Can query long array" $
            queryDoc
              (Array . Vector.fromList $ ["1", "2", "3", "4"])
              (Query [Glob (NonEmpty.fromList [LiteralString "3"])])
              @?= ["4"],
          testCase "Can query array" $
            queryDoc
              (Array . Vector.singleton $ "1")
              (Query [Glob (NonEmpty.fromList [LiteralString "0"])])
              @?= ["1"],
          testCase "Can query nested arrays" testNestedArray,
          testCase "Can query nested objects" testNestedObject,
          testCase "Modify document" $
            modifyDoc
              (Array (Vector.fromList [Number 1, Number 2, Number 3]))
              (Query [Glob (NonEmpty.fromList [LiteralString "2"])])
              (Number 4)
              @?= Array (Vector.fromList [Number 1, Number 2, Number 4]),
          testCase "Can list items using wildcard" $
            queryDoc
              ( Object . HashMap.fromList $
                  [ ("beer", Number 1),
                    ("bear", Number 2),
                    ("cart", Number 3)
                  ]
              )
              (Query [Glob . NonEmpty.fromList $ [LiteralString "b", AnyCharMultiple]])
              @?= [Number 2, Number 1] -- TODO Ordering of keys in HashMap is not stable, test is brittle
        ]
    ]

testNestedArray :: Assertion
testNestedArray = queryDoc v q @?= ["a"]
  where
    v = Array (Vector.fromList [Array (Vector.fromList ["a"])])
    q =
      Query
        [ Glob (NonEmpty.fromList [LiteralString "0"]),
          Glob (NonEmpty.fromList [LiteralString "0"])
        ]

testNestedObject :: Assertion
testNestedObject = queryDoc v q @?= [toJSON ("Test" :: Text)]
  where
    v =
      Object . HashMap.fromList $
        [ ("a", Object . HashMap.fromList $ [("b", "Test")])
        ]
    q =
      Query
        [ Glob (LiteralString "a" NonEmpty.:| []),
          Glob (LiteralString "b" NonEmpty.:| [])
        ]

prop_command_parse :: Command -> Bool
prop_command_parse c = case parser . renderCommand $ c of
  Left _ -> False
  Right [] -> False
  Right (x : _) -> x == c

prop_query_parse :: Query -> Bool
prop_query_parse q = case parseQuery (renderQuery q) of
  Left _ -> False
  Right qry -> qry == q

prop_move_up_shorten :: Query -> Bool
prop_move_up_shorten q = len (moveUp q) <= len q
  where
    len (Query l) = length l

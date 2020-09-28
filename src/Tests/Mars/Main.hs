{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Mars.Command
import Mars.Command.Cat
import Mars.Command.Cd
import Mars.Command.Ls
import Mars.Command.Pwd
import Mars.Parser
import Mars.Query
import Mars.Renderable
import Mars.Types
import Test.Tasty
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
      stateTests,
      queryProperties,
      evaluationTests
    ]

queryProperties :: TestTree
queryProperties =
  testGroup
    "Query Tests"
    [ testProperty "command parse unparse" prop_command_parse,
      testProperty "query parse unparse" prop_query_parse,
      testProperty "move up shortens" prop_move_up_shorten
    ]

parseCase :: String -> [Operation] -> TestTree
parseCase s q = testCase s $ parser (toS s) @?= Right q

newtype WrappedText = WrappedText Text
  deriving (Show)

instance Arbitrary WrappedText where
  arbitrary =
    WrappedText . toS
      <$> listOf
        (elements (['A' .. 'Z'] <> ['a' .. 'z']))
      `suchThat` (not . null)

evaluationTests :: TestTree
evaluationTests =
  testGroup
    "Evaluation"
    [ testProperty
        "colored text contains text"
        prop_contains_colored_text
    ]

prop_contains_colored_text :: ANSIColour -> WrappedText -> Bool
prop_contains_colored_text color (WrappedText text) =
  (length . Text.breakOnAll text . ansiColor color $ text) /= 0

stateTests :: TestTree
stateTests =
  testGroup
    "State Updates"
    [ testGroup
        "cd"
        [ test_cd_existing,
          test_cd_nonexisting
        ],
      testGroup "ls" [test_ls_top_level, test_ls_second_level]
    ]

initMarsState :: Text -> MarsState
initMarsState t =
  MarsState
    { path = DefaultLocation,
      document = fromJust . decode . toS $ t
    }

test_cd_existing :: TestTree
test_cd_existing =
  testCase
    "cd to existing object"
    $ newPath @?= q
  where
    (ChangePath newPath) = evalCommand oldState (Cd q)
    q = fromRight (error "parseQuery test_cd_existing") . parseQuery $ "a"
    oldState = initMarsState "{\"a\": {}}"

test_cd_nonexisting :: TestTree
test_cd_nonexisting =
  testCase
    "cd to existing object"
    $ newPath @?= path oldState
  where
    (ChangePath newPath) = evalCommand oldState (Cd q)
    q = fromRight (error "parseQuery test_cd_nonexisting") . parseQuery $ "b"
    oldState = initMarsState "{\"a\": {}}"

test_ls_top_level :: TestTree
test_ls_top_level =
  testCase
    "ls should print entries for top level"
    $ result @?= DirectoryEntries [DirectoryEntry (ItemName "a") MarsBool]
  where
    result = evalCommand state (Ls q)
    q = fromRight (error "aef322") . parseQuery $ ""
    state = initMarsState "{\"a\": true}"

test_ls_second_level :: TestTree
test_ls_second_level =
  testCase
    "ls should print entries for second level"
    $ result
      @?= DirectoryEntries
        [ DirectoryEntry (ItemName "ann") MarsBool,
          DirectoryEntry (ItemName "barry") MarsNumber
        ]
  where
    result = evalCommand state (Ls q)
    q = fromRight (error "aef322") . parseQuery $ "a"
    state = initMarsState "{\"a\": {\"ann\": true, \"barry\": 1}}"

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testGroup
        "Parsing Commands"
        [ parseCase "ls" [OpLs . Ls $ DefaultLocation],
          parseCase "cat" [OpCat . Cat $ []],
          parseCase "pwd" [OpPwd Pwd],
          parseCase "cd" [OpCd . Cd $ DefaultLocation],
          parseCase
            "ls *"
            [ OpLs . Ls $
                ( Query . NonEmpty.fromList $
                    [ Glob . NonEmpty.fromList $
                        [AnyCharMultiple]
                    ]
                )
            ],
          parseCase
            "ls b*"
            [ OpLs . Ls
                $ Query
                  . NonEmpty.fromList
                $ [ Glob . NonEmpty.fromList $
                      [LiteralString "b", AnyCharMultiple]
                  ]
            ]
        ],
      testGroup
        "Parsing Queries"
        [ testCase "empty query" $
            parseQuery "" @?= Right DefaultLocation
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
              (Query . NonEmpty.fromList $ [Glob (NonEmpty.fromList [LiteralString "3"])])
              (Array . Vector.fromList $ ["1", "2", "3", "4"])
              @?= ["4"],
          testCase "Can query array" $
            queryDoc
              (Query . NonEmpty.fromList $ [Glob (NonEmpty.fromList [LiteralString "0"])])
              (Array . Vector.singleton $ "1")
              @?= ["1"],
          testCase "Can query nested arrays" testNestedArray,
          testCase "Can query nested objects" testNestedObject,
          testCase "Modify document" $
            modifyDoc
              (Array (Vector.fromList [Number 1, Number 2, Number 3]))
              (Query . NonEmpty.fromList $ [Glob (NonEmpty.fromList [LiteralString "2"])])
              (Number 4)
              @?= Array (Vector.fromList [Number 1, Number 2, Number 4]),
          testCase "Can list items using wildcard" $
            queryDoc
              (Query . NonEmpty.fromList $ [Glob . NonEmpty.fromList $ [LiteralString "b", AnyCharMultiple]])
              ( Object . HashMap.fromList $
                  [ ("beer", Number 1),
                    ("bear", Number 2),
                    ("cart", Number 3)
                  ]
              )
              @?= [Number 2, Number 1] -- TODO Ordering of keys in HashMap is not stable, test is brittle
        ]
    ]

testNestedArray :: Assertion
testNestedArray = queryDoc q v @?= ["a"]
  where
    v = Array (Vector.fromList [Array (Vector.fromList ["a"])])
    q =
      Query . NonEmpty.fromList $
        [ Glob (NonEmpty.fromList [LiteralString "0"]),
          Glob (NonEmpty.fromList [LiteralString "0"])
        ]

testNestedObject :: Assertion
testNestedObject = queryDoc q v @?= [toJSON ("Test" :: Text)]
  where
    v =
      Object . HashMap.fromList $
        [ ("a", Object . HashMap.fromList $ [("b", "Test")])
        ]
    q =
      Query . NonEmpty.fromList $
        [ Glob (LiteralString "a" NonEmpty.:| []),
          Glob (LiteralString "b" NonEmpty.:| [])
        ]

prop_command_parse :: Operation -> Bool
prop_command_parse (OpCat c) = case parser . render $ c of
  Right ((OpCat x) : _) -> x == c
  _ -> False
prop_command_parse (OpCd c) = case parser . render $ c of
  Right ((OpCd x) : _) -> x == c
  _ -> False
prop_command_parse (OpLoad c) = case parser . render $ c of
  Right ((OpLoad x) : _) -> x == c
  _ -> False
prop_command_parse (OpLs c) = case parser . render $ c of
  Right ((OpLs x) : _) -> x == c
  _ -> False
prop_command_parse (OpPwd c) = case parser . render $ c of
  Right ((OpPwd x) : _) -> x == c
  _ -> False
prop_command_parse (OpSave c) = case parser . render $ c of
  Right ((OpSave x) : _) -> x == c
  _ -> False
prop_command_parse (OpSet c) = case parser . render $ c of
  Right ((OpSet x) : _) -> x == c
  _ -> False

prop_query_parse :: Query -> Bool
prop_query_parse q = case parseQuery . render $ q of
  Left _ -> False
  Right qry -> qry == q

prop_move_up_shorten :: Query -> Bool
prop_move_up_shorten q = case moveUp q of
  Just shorter -> len shorter < len q
  Nothing -> True
  where
    len DefaultLocation = 1
    len (Query l) = length l

module Test.Calendar.Week
    ( testWeek
    ) where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Test.Tasty
import Test.Tasty.HUnit

testDay :: TestTree
testDay =
    testCase "day" $ do
        let day = fromGregorian 2018 1 9
        assertEqual "" (ModifiedJulianDay 58127) day
        assertEqual "" (2018, 2, 2) $ toWeekDate day
        assertEqual "" Tuesday $ dayOfWeek day

allDaysOfWeek :: [DayOfWeek]
allDaysOfWeek = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

testAllDays :: String -> (DayOfWeek -> IO ()) -> TestTree
testAllDays name f = testGroup name $ fmap (\wd -> testCase (show wd) $ f wd) allDaysOfWeek

testSucc :: TestTree
testSucc = testAllDays "succ" $ \wd -> assertEqual "" (toEnum $ succ $ fromEnum wd) $ succ wd

testPred :: TestTree
testPred = testAllDays "pred" $ \wd -> assertEqual "" (toEnum $ pred $ fromEnum wd) $ pred wd

testSequences :: TestTree
testSequences =
    testGroup
        "sequence"
        [ testCase "[Monday .. Sunday]" $ assertEqual "" allDaysOfWeek [Monday .. Sunday]
        , testCase "[Wednesday .. Wednesday]" $ assertEqual "" [Wednesday] [Wednesday .. Wednesday]
        , testCase "[Sunday .. Saturday]" $
          assertEqual "" [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday] [Sunday .. Saturday]
        , testCase "[Thursday .. Wednesday]" $
          assertEqual "" [Thursday, Friday, Saturday, Sunday, Monday, Tuesday, Wednesday] [Thursday .. Wednesday]
        , testCase "[Tuesday ..]" $
          assertEqual
              ""
              [ Tuesday
              , Wednesday
              , Thursday
              , Friday
              , Saturday
              , Sunday
              , Monday
              , Tuesday
              , Wednesday
              , Thursday
              , Friday
              , Saturday
              , Sunday
              , Monday
              , Tuesday
              ] $
          take 15 [Tuesday ..]
        , testCase "[Wednesday, Tuesday ..]" $
          assertEqual
              ""
              [ Wednesday
              , Tuesday
              , Monday
              , Sunday
              , Saturday
              , Friday
              , Thursday
              , Wednesday
              , Tuesday
              , Monday
              , Sunday
              , Saturday
              , Friday
              , Thursday
              , Wednesday
              ] $
          take 15 [Wednesday,Tuesday ..]
        , testCase "[Sunday, Friday ..]" $
          assertEqual "" [Sunday, Friday, Wednesday, Monday, Saturday, Thursday, Tuesday, Sunday] $
          take 8 [Sunday,Friday ..]
        , testCase "[Monday,Sunday .. Tuesday]" $
          assertEqual "" [Monday, Sunday, Saturday, Friday, Thursday, Wednesday, Tuesday] [Monday,Sunday .. Tuesday]
        , testCase "[Thursday, Saturday .. Tuesday]" $
          assertEqual "" [Thursday, Saturday, Monday, Wednesday, Friday, Sunday, Tuesday] [Thursday,Saturday .. Tuesday]
        ]

testReadShow :: TestTree
testReadShow = testAllDays "read show" $ \wd -> assertEqual "" wd $ read $ show wd

testWeek :: TestTree
testWeek = testGroup "Week" [testDay, testSucc, testPred, testSequences, testReadShow]

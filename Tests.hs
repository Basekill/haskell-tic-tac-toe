module Tests (Tests.main) where
import IC.TestSuite
import TicTacToe hiding (main)


gameOverTestCases
   = [ testBoard1 ==> True
     , testBoard2 ==> False
     , testBoard3 ==> True
     ]

isFullTestCases
   = [ testBoard1 ==> False
     , testBoard2 ==> False
     , testBoard3 ==> False
     , testBoard4 ==> True
     ]

parsePositionTestCases
   = [
       ("0 2") ==> (Just (0,2))
     , ("0 -8") ==> (Just (0,-8))
     , ("-4 1") ==> (Just (-4,1))
     , ("0 %1") ==> (Nothing)
     , ("") ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     , ("(0,0)") ==> (Nothing)
     , ("01 023") ==> (Just (1,23))
     , ("0000 -012") ==> (Just (0, -12))
     ]

parseSizeTestCases
   = [
       ("1") ==> (Just 1)
     , ("3") ==> (Just 3)
     , ("0") ==> (Nothing)
     , ("-2") ==> (Nothing)
     , ("0 3") ==> (Nothing)
     , ("2 1") ==> (Nothing)
     , ("0 %1") ==> (Nothing)
     , ("") ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     , ("(0,0)") ==> (Nothing)
     ]

tryMoveTestCases
  = [
      (X,(0,0),testBoard2) ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2) ==> (Just ([Taken X,Empty,Empty,Taken O],2))
    , (O,(3,3),testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken O,
                                Empty,Taken X,Taken X,Taken O,Empty,Empty,Taken
                                X,Taken O,Taken X,Empty,Taken O],4))
    , (X,(4,4),testBoard3) ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken X,
                                       Taken O,Empty,Taken X,Taken X,Empty,
                                       Empty,Empty,Taken X,Taken O,Taken O,
                                       Taken O,Taken X,Empty,Empty,Taken X,
                                       Taken X,Empty,Taken O,Empty,Taken X],
                                       5))
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase "gameOver" (gameOver)
               gameOverTestCases
    , TestCase "isFull" (isFull)
               isFullTestCases
    , TestCase "parsePosition" (parsePosition)
               parsePositionTestCases
    , TestCase "parseSize" (parseSize)
               parseSizeTestCases
    , TestCase "tryMove" (uncurry3 tryMove)
               tryMoveTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests

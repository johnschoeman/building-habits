module HabitTest exposing (..)

import Expect
import Habit exposing (HabitLog, completedToday, isEqual)
import Test exposing (..)
import Time exposing (Posix, utc)


testSuite : Test
testSuite =
    describe "Habit"
        [ describe "isEqual"
            [ test "when the letters are different, it returns false" <|
                \_ ->
                    Expect.false "" (isEqual "Asdf" "fdsa")
            , test "when the cases are different, it returns true" <|
                \_ ->
                    Expect.true "" (isEqual "Asdf" "asdf")
            , test "when there is trailing or leading whitespace, it returns true" <|
                \_ ->
                    Expect.true "" (isEqual " asdf \n" "asdf")
            ]
        , describe "completedToday"
            [ test "when the habit was completed today, it return true" <|
                \_ ->
                    let
                        now : Posix
                        now =
                            Time.millisToPosix 1234

                        habit =
                            "A"

                        habitLog : HabitLog
                        habitLog =
                            [ { date = now
                              , habit = "A"
                              }
                            ]
                    in
                    Expect.true "" <| completedToday Time.utc habit now habitLog
            , test "when the habit was not completed today, it returns false" <|
                \_ ->
                    let
                        now : Posix
                        now =
                            Time.millisToPosix 1234

                        habit =
                            "A"

                        habitLog : HabitLog
                        habitLog =
                            [ { date = Time.millisToPosix 123456789
                              , habit = "A"
                              }
                            ]
                    in
                    Expect.false "" <| completedToday Time.utc habit now habitLog
            ]
        ]

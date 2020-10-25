module Styles.Forms exposing (..)

import Styles.Typography as Typography


input : String
input =
    Typography.textInput ++ " text-center px-2 mb-2"


select : String
select =
    "rounded-md border border-gray-300 px-2 py-2 bg-white leading-5 font-medium text-gray-800 hover:text-gray-500 focus:outline-none focus:border-blue-300 focus:shadow-outline-blue active:bg-gray-50 active:text-gray-800 transition ease-in-out duration-150"

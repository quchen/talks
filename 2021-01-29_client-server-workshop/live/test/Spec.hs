import Test.QuickCheck
import Data.Text as T

import Lib

main :: IO ()
main = verboseCheck (\action -> deserialize (serialize action) == Right action )


instance Arbitrary ClientAction where
    arbitrary = oneof
        [ pure Quit
        , do randomString <- arbitrary
             pure (Message (T.pack randomString))
        ]

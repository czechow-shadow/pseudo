import Protolude

import Test.Tasty

import MessageProp


main :: IO ()
main = defaultMain $ testGroup "X" [MessageProp.tests]

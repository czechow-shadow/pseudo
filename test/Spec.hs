import Protolude

import Test.Tasty

import LibProp



main :: IO ()
main = defaultMain $ testGroup "X" [LibProp.tests]

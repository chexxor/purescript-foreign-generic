module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, readJSON, readProp, write)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert, assert', ASSERT)
import Test.Types (IntList(..), RecordTest(..), Tree(..), TupleArray(..))

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: forall eff. String -> Eff (assert :: ASSERT | eff) Unit
throw = flip assert' false

testRoundTrip
  :: ∀ a eff
   . ( Eq a
     , IsForeign a
     , AsForeign a
     )
  => a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testRoundTrip x2 = do
  let json = unsafeStringify (write x2)
  log json
  case runExcept (readJSON json) of
    Right y -> assert (x2 == y)
    Left err -> throw (show err)

main2 :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main2 = do
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
  testRoundTrip (makeTree 0)
  testRoundTrip (makeTree 5)

newtype NameRecord = NameRecord { name :: String }

-- expect to have "name" field.
-- but what if JS lib surprises us by changing field name to "firstName"?
derive instance genericNameRecord :: Generic NameRecord _

instance isForeignNameRecord :: IsForeign NameRecord where
  -- read :: Foreign -> NameRecord
  --read f = readGeneric defaultOptions { unwrapNewtypes = true } f
  read f = readGeneric defaultOptions { unwrapSingleConstructors = true } f
  --read f = do
  --  n <- readProp "name" f
  --  pure $ NameRecord { name: n }

foreign import x :: Foreign

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let xParsed = (runExcept $ read x) :: Either MultipleErrors NameRecord
  logShow $ map (\(NameRecord n) -> n.name) xParsed
  -- Parse error is this when using Generic, which doesn't tell us the field name on which parse failed:
  -- (Left (NonEmptyList (NonEmpty (ErrorAtIndex 0 (TypeMismatch "String" "Undefined")) Nil)))

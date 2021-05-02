module GraphQL where

import Prelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class Query (t :: Type) (c :: Type -> Type) (r :: Type) | t -> c r where
  renderQuery :: t -> String

class RecordQuery (t :: Type) (c :: Type -> Type) (r :: Row Type) | t -> c r where
  renderRecordQuery :: t -> String

newtype Selected q r
  = Selected { query :: q, selector :: r }

instance selectedQuery ::
  ( RL.RowToList row rl
  , RecordQuery query cons rec
  , SelectedRecord rec rl result
  ) =>
  Query (Selected query (Record row)) cons (Record result) where
  renderQuery (Selected { query }) =
    renderRecordQuery query
      <> "{ "
      <> renderSelector (Proxy :: Proxy rec) (Proxy :: Proxy rl)
      <> "}"

class SelectedRecord (base :: Row Type) (rl :: RL.RowList Type) (rec :: Row Type) | base rl -> rec where
  renderSelector :: Proxy base -> Proxy rl -> String

instance selectedRecordCons ::
  ( IsSymbol name
  , IsSymbol renamed
  , Row.Cons name ty trash base
  , SelectedRecord base tail from'
  , Row.Cons renamed ty from' to
  ) =>
  SelectedRecord base (RL.Cons renamed (N name) tail) to where
  renderSelector _ _ =
    reflectSymbol (Proxy :: Proxy renamed)
      <> ": "
      <> reflectSymbol (Proxy :: Proxy name)
      <> ", "
      <> renderSelector (Proxy :: Proxy base) (Proxy :: Proxy tail)

instance selectedRecordNil :: SelectedRecord base RL.Nil () where
  renderSelector _ _ = mempty

class QueryRecord (rl :: RowList Type) (row :: Row Type) (rec :: Row Type) | rl -> row rec where
  renderQueryRecord :: Proxy rl -> Record row -> String

instance queryRecordCons ::
  ( IsSymbol name
  , Row.Cons name ty trash row
  , Query ty cons out
  , QueryRecord tail row from'
  , Row.Cons name (cons out) from' to
  ) =>
  QueryRecord (RL.Cons name ty tail) row to where
  renderQueryRecord _ rec =
    reflectSymbol (Proxy :: Proxy name)
      <> ": "
      <> renderQuery member
      <> ", "
      <> renderQueryRecord (Proxy :: Proxy tail) rec
    where
    member = Record.get (Proxy :: Proxy name) rec

instance queryRecordNil :: QueryRecord RL.Nil row () where
  renderQueryRecord _ _ = mempty

class Encode a where
  encode :: a -> String

instance encodeString :: Encode String where
  encode str = "\"" <> str <> "\""

instance encodeNumber :: Encode Number where
  encode num = show num

class GenericParameters (rl :: RowList Type) (row :: Row Type) | rl -> row where
  renderParameterRecord :: Proxy rl -> Record row -> String

instance genericParametersCons ::
  ( IsSymbol name
  , Row.Cons name ty trash row
  , Encode ty
  , GenericParameters tail row
  ) =>
  GenericParameters (RL.Cons name ty tail) row where
  renderParameterRecord _ rec =
    reflectSymbol (Proxy :: Proxy name)
      <> ": "
      <> encode member
      <> ", "
      <> renderParameterRecord (Proxy :: Proxy tail) rec
    where
    member = Record.get (Proxy :: Proxy name) rec

instance genericParametersNil :: GenericParameters RL.Nil row where
  renderParameterRecord _ _ = mempty

select :: ∀ q r. q -> r -> Selected q r
select query selector = Selected { query, selector }

infixl 4 select as <:

data N :: Symbol -> Type
data N a
  = N

defaultQuery :: ∀ row rl. RL.RowToList row rl => GenericParameters rl row => String -> Record row -> String
defaultQuery name rec = name <> "(" <> renderParameterRecord (Proxy :: Proxy rl) rec <> ")"

render :: ∀ row row' rl. RL.RowToList row rl => QueryRecord rl row row' => Record row -> String
render rec = "query { " <> renderQueryRecord (Proxy :: Proxy rl) rec <> "}"

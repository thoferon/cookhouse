{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Data.Internal where

import           Prelude
import           GHC.Int

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Trans.Free hiding (Free)

import           Data.List hiding (groupBy, delete)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Time
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Vector             as V

#ifdef DEBUG_SQL
import           System.IO
#endif

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name, Field)
import           Database.PostgreSQL.Simple.FromRow hiding (field)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types

import           Cookhouse.Capabilities
import           Cookhouse.Errors

data Proxy a = Proxy

mkProxy :: a -> Proxy a
mkProxy = const Proxy

type MonadDataLayer m
  = ( MonadSafeAccess CookhouseAccess m
    , MonadError CookhouseError m
    , MonadDatabase m
    , MonadTime m
    )

data DatabaseF a
  = forall p. ToRow p => DBInsert Query [Query] Query [p] ([PureRow] -> a)
  | forall b. DBSelect Query [Query] (SelectModifiers b) ([PureRow] -> a)
  | forall b. DBUpdate Query [(Query, Action)] (SelectModifiers b) (Int64 -> a)
  | forall b. DBDelete Query (SelectModifiers b) (Int64 -> a)

instance Functor DatabaseF where
  fmap f (DBInsert tbl columns ret params g) =
    DBInsert tbl columns ret params (f . g)
  fmap f (DBSelect tbl columns mods g) = DBSelect tbl columns mods (f . g)
  fmap f (DBUpdate tbl pairs   mods g) = DBUpdate tbl pairs   mods (f . g)
  fmap f (DBDelete tbl         mods g) = DBDelete tbl         mods (f . g)

type DatabaseM = Free DatabaseF

class Monad m => MonadDatabase m where
  dbInsert :: ToRow p => Query -> [Query] -> Query -> [p] -> m [PureRow]
  dbSelect :: Query -> [Query] -> SelectModifiers b -> m [PureRow]
  dbUpdate :: Query -> [(Query, Action)] -> SelectModifiers b -> m Int64
  dbDelete :: Query -> SelectModifiers b -> m Int64

instance MonadDatabase (Free DatabaseF) where
  dbInsert tbl columns ret params =
    liftF $ DBInsert tbl columns ret params id
  dbSelect tbl columns mods   = liftF $ DBSelect tbl columns mods   id
  dbUpdate tbl pairs   mods   = liftF $ DBUpdate tbl pairs   mods   id
  dbDelete tbl         mods   = liftF $ DBDelete tbl         mods   id

instance {-# OVERLAPPABLE #-} (MonadDatabase m, MonadTrans t, Monad (t m))
         => MonadDatabase (t m) where
  dbInsert tbl columns ret params = lift $ dbInsert tbl columns ret params
  dbSelect tbl columns mods       = lift $ dbSelect tbl columns mods
  dbUpdate tbl pairs   mods       = lift $ dbUpdate tbl pairs   mods
  dbDelete tbl         mods       = lift $ dbDelete tbl         mods

runDatabaseM :: Connection -> DatabaseM a -> IO (Either CookhouseError a)
runDatabaseM conn action = runExceptT $ foldFree interpreter action
  where
    interpreter :: DatabaseF a -> ExceptT CookhouseError IO a
    interpreter d = do
      eRes <- liftIO $ handle (\(e :: SqlError) -> return $ Left e) $ do
        Right <$> case d of
          DBInsert tbl columns ret params f ->
            f <$> _insert conn tbl columns ret params
          DBSelect tbl columns mods f -> f <$> _select conn tbl columns mods
          DBUpdate tbl pairs   mods f -> f <$> _update conn tbl pairs mods
          DBDelete tbl         mods f -> f <$> _delete conn tbl mods
      either (throwError . SQLError . show) return eRes

newtype TimeF a = GetTime (UTCTime -> a)

instance Functor TimeF where
  fmap f (GetTime g) = GetTime (f . g)

type TimeT = FreeT TimeF

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance Monad m => MonadTime (FreeT TimeF m) where
  getTime = liftF $ GetTime id

instance {-# OVERLAPPABLE #-} (MonadTime m, MonadTrans t, Monad (t m))
         => MonadTime (t m) where
  getTime = lift getTime

runTimeT :: Monad m => UTCTime -> TimeT m a -> m a
runTimeT now = iterT interpreter
  where
    interpreter :: TimeF (m a) -> m a
    interpreter (GetTime f) = f now

data Entity a = Entity
  { entityID  :: EntityID a
  , entityVal :: a
  }

instance (Show a, Show (EntityID a)) => Show (Entity a) where
  show (Entity i val) = "Entity (" ++ show i ++ ") (" ++ show val ++ ")"

instance (Eq a, Eq (EntityID a)) => Eq (Entity a) where
  (==) (Entity i1 val1) (Entity i2 val2) = i1 == i2 && val1 == val2

data PureField = PureField
  { pfTypeName :: BS.ByteString
  , pfValue    :: Maybe BS.ByteString
  } deriving (Show, Eq)

type PureRow = [PureField]

newtype PureRowParser a = PureRowParser
  { runPureRowParser :: [PureField] -> Either String ([PureField], a) }

instance Functor PureRowParser where
  fmap f parser = PureRowParser $ \fields ->
    fmap (fmap f) $ runPureRowParser parser fields

instance Applicative PureRowParser where
  pure x = PureRowParser $ \fields -> Right (fields, x)
  ff <*> fx = PureRowParser $ \fields -> do
    (fields',  f) <- runPureRowParser ff fields
    (fields'', x) <- runPureRowParser fx fields'
    return (fields'', f x)

instance Monad PureRowParser where
  mx >>= f = PureRowParser $ \fields -> do
    (fields', x) <- runPureRowParser mx fields
    runPureRowParser (f x) fields'
  fail = PureRowParser . const . Left

newtype PureFieldParser a
  = PureFieldParser { runPureFieldParser :: PureField -> Either String a }

instance Functor PureFieldParser where
  fmap f parser = PureFieldParser $ \pf ->
    fmap f $ runPureFieldParser parser pf

instance Applicative PureFieldParser where
  pure = PureFieldParser . const . pure
  ff <*> fx = PureFieldParser $ \pf -> do
    runPureFieldParser ff pf <*> runPureFieldParser fx pf

instance Monad PureFieldParser where
  mx >>= f = PureFieldParser $ \pf -> do
    x <- runPureFieldParser mx pf
    runPureFieldParser (f x) pf
  fail = PureFieldParser . const . Left

getPureField :: PureFieldParser PureField
getPureField = PureFieldParser Right

getTypeName :: PureFieldParser BS.ByteString
getTypeName = pfTypeName <$> getPureField

getFieldValue :: PureFieldParser (Maybe BS.ByteString)
getFieldValue = pfValue <$> getPureField

ensureFieldType :: [BS.ByteString] -> PureFieldParser ()
ensureFieldType names = do
  name <- getTypeName
  if name `elem` names
    then return ()
    else fail $
      "type " ++ show name ++ " incompatible with expected type(s): "
              ++ BS.unpack (mconcat (intersperse ", " names))

ensureNotNull :: PureFieldParser BS.ByteString
ensureNotNull = maybe (fail "encountered NULL") return =<< getFieldValue

class PureFromField a where
  pureFromField :: PureFieldParser a

integerField :: PureFieldParser Integer
integerField = do
  ensureFieldType ["integer", "int4"]
  bs <- ensureNotNull
  case reads $ BS.unpack bs of
    (n,""):_ -> return n
    _ -> fail $ "invalid integer: " ++ show bs

instance PureFromField Integer where
  pureFromField = integerField

instance PureFromField Int where
  pureFromField = fromInteger <$> integerField

instance PureFromField Int64 where
  pureFromField = fromInteger <$> integerField

instance PureFromField Double where
  pureFromField = do
    ensureFieldType ["double precision", "float8"]
    bs <- ensureNotNull
    case reads $ BS.unpack bs of
      (d,""):_ -> return d
      _ -> fail $ "invalid double precision: " ++ show bs

instance PureFromField BS.ByteString where
  pureFromField = do
    ensureFieldType ["varchar", "text"]
    ensureNotNull

instance PureFromField String where
  pureFromField = BS.unpack <$> pureFromField

instance PureFromField a => PureFromField (Maybe a) where
  pureFromField = do
    mVal <- getFieldValue
    case mVal of
      Nothing -> return Nothing
      _       -> Just <$> pureFromField

instance PureFromField Bool where
  pureFromField = do
    ensureFieldType ["boolean", "bool"]
    bs <- ensureNotNull
    case bs of
      "t" -> return True
      "f" -> return False
      _   -> fail $ "invalid boolean: " ++ show bs

instance {-# OVERLAPPABLE #-} PureFromField a => PureFromField [a] where
  pureFromField = arrayField

instance PureFromField a => PureFromField (V.Vector a) where
  pureFromField = V.fromList <$> arrayField

arrayField :: PureFromField a => PureFieldParser [a]
arrayField = do
    name <- getTypeName
    bs   <- ensureNotNull
    case BS.splitAt 1 name of
      ("_", name') -> do
        vals <- readAsByteStrings bs
        sequence $ map (parseField name') vals
      _ -> fail $  "invalid array type: " ++ show name

  where
    parseField :: PureFromField a => BS.ByteString -> BS.ByteString
               -> PureFieldParser a
    parseField name bs = do
      let eRes = runPureFieldParser pureFromField $ PureField name (Just bs)
      either fail return eRes

    readAsByteStrings :: BS.ByteString -> PureFieldParser [BS.ByteString]
    readAsByteStrings bs = case BS.splitAt 1 bs of
      ("{","}") -> return []
      ("{",bs') -> readByteStrings id bs'
      _ -> fail $ "invalid array starting with " ++ show (BS.take 30 bs)

    readByteStrings :: ([BS.ByteString] -> [BS.ByteString]) -> BS.ByteString
                    -> PureFieldParser [BS.ByteString]
    readByteStrings f bs = do
      (val, bs') <- readByteString bs
      case BS.splitAt 1 bs' of
        (",", bs'') -> readByteStrings (f . (val :)) bs''
        ("}", "")   -> return $! f [val]
        _ -> fail $ "invalid array around " ++ show (BS.take 30 bs')

    readByteString :: BS.ByteString
                   -> PureFieldParser (BS.ByteString, BS.ByteString)
    readByteString bs = case BS.splitAt 1 bs of
      ("\"", bs') -> readByteString' "" bs'
      _ -> return $ BS.span (\c -> c /= ',' && c /= '}') bs

    readByteString' :: BS.ByteString -> BS.ByteString
                    -> PureFieldParser (BS.ByteString, BS.ByteString)
    readByteString' acc bs = case BS.splitAt 1 bs of
      ("\"", bs') -> return (acc, bs')
      ("\\", bs') -> let (c, bs'') = BS.splitAt 1 bs'
                     in readByteString' (acc <> c) bs''
      (c,    bs') -> readByteString' (acc <> c) bs'

class PureFromRow a where
  pureFromRow :: PureRowParser a

instance PureFromField a => PureFromRow (Only a) where
  pureFromRow = Only <$> field

parsePureRow :: PureFromRow a => PureRow -> Either String a
parsePureRow = fmap snd . runPureRowParser pureFromRow

parsePureRows :: PureFromRow a => [PureRow] -> Either String [a]
parsePureRows = sequence . map parsePureRow

pureField :: PureRowParser PureField
pureField = PureRowParser $ \fields -> case fields of
  fld : rest -> Right (rest, fld)
  []         -> Left "not enough columns"

field :: PureFromField a => PureRowParser a
field = do
  pf <- pureField
  either fail return $ runPureFieldParser pureFromField pf

instance (PureFromField (EntityID a), PureFromRow a)
         => PureFromRow (Entity a) where
  pureFromRow = Entity <$> field <*> pureFromRow

class (PureFromRow a, ToRow a, PureFromField (EntityID a), ToField (EntityID a))
    => Storable a where

  data EntityID a :: *

  tableName  :: Proxy a -> Query
  fieldNames :: Proxy a -> [Query]

  idFieldName :: Proxy a -> Query
  idFieldName _ = "id"

data WhereModifier
  = WhereModifier { wmodTemplate :: Query, wmodValues :: [Action] }
  deriving (Show, Eq)

instance Eq Action where
  (==) a1 a2 = case (a1, a2) of
    (Plain b1, Plain b2) -> BSB.toLazyByteString b1 == BSB.toLazyByteString b2
    (Escape b1, Escape b2)                     -> b1 == b2
    (EscapeByteA b1, EscapeByteA b2)           -> b1 == b2
    (EscapeIdentifier b1, EscapeIdentifier b2) -> b1 == b2
    (Many as1, Many as2)                       -> as1 == as2
    _                                          -> False

instance  Monoid WhereModifier where
  mempty = WhereModifier "" []
  mappend (WhereModifier aquery aactions) (WhereModifier bquery bactions) =
    let q | aquery == "" = bquery
          | bquery == "" = aquery
          | otherwise    = "(" <> aquery <> ") AND (" <> bquery <> ")"
    in WhereModifier q $ aactions ++ bactions

data SelectModifiers t = SelectModifiers
  { selModWhere   :: WhereModifier
  , selModGroupBy :: [Query]
  , selModOrderBy :: [Query]
  , selModLimit   :: Maybe Query
  , selModOffset  :: Maybe Query
  } deriving (Show, Eq)

instance Monoid (SelectModifiers t) where
  mempty = SelectModifiers mempty [] [] Nothing Nothing
  mappend amods bmods = SelectModifiers
    { selModWhere   = selModWhere   amods <> selModWhere   bmods
    , selModGroupBy = selModGroupBy amods <> selModGroupBy bmods
    , selModOrderBy = selModOrderBy amods <> selModOrderBy bmods
    , selModLimit   = selModLimit   amods <> selModLimit   bmods
    , selModOffset  = selModOffset  amods <> selModOffset  bmods
    }

whereWith :: ToField a => Query -> Query -> a -> SelectModifiers t
whereWith op name val =
  let whereMod = WhereModifier (name <> " " <> op <> " ?") [toField val]
  in mempty { selModWhere = whereMod }

(==.) :: ToField a => Query -> a -> SelectModifiers t
(==.) = whereWith "="

(/=.) :: ToField a => Query -> a -> SelectModifiers t
(/=.) = whereWith "<>"

(<.) :: ToField a => Query -> a -> SelectModifiers t
(<.)  = whereWith "<"

(>.) :: ToField a => Query -> a -> SelectModifiers t
(>.)  = whereWith ">"

(<=.) :: ToField a => Query -> a -> SelectModifiers t
(<=.)  = whereWith "<="

(>=.) :: ToField a => Query -> a -> SelectModifiers t
(>=.)  = whereWith ">="

(&&.) :: SelectModifiers t -> SelectModifiers t -> SelectModifiers t
(&&.) = (<>)

(||.) :: SelectModifiers t -> SelectModifiers t -> SelectModifiers t
(||.) amods bmods =
  let WhereModifier aq aactions = selModWhere amods
      WhereModifier bq bactions = selModWhere bmods
      tempMods = amods <> bmods
      q        = "(" <> aq <> ") OR (" <> bq <> ")"
      actions  = aactions ++ bactions
      whereMod = WhereModifier q actions
  in tempMods { selModWhere = whereMod }

infixl 9 ==.
infixl 9 /=.
infixl 9 <.
infixl 9 >.
infixl 9 <=.
infixl 9 >=.

infixl 8 &&.
infixl 7 ||.

belongsTo :: ToField a => Query -> [a] -> SelectModifiers t
 -- Prevent a syntax error
belongsTo _ [] = mempty { selModWhere = WhereModifier "1 = 0" [] }
belongsTo name values =
  let marks    = fromString . concat . intersperse ", "
               . map (const "?") $ values
      actions  = map toField values
      whereMod = WhereModifier (name <> " IN (" <> marks <> ")") actions
  in mempty { selModWhere = whereMod }

isNull :: Query -> SelectModifiers t
isNull name = mempty { selModWhere = WhereModifier (name <> " IS NULL") [] }

isNotNull :: Query -> SelectModifiers t
isNotNull name =
  mempty { selModWhere = WhereModifier (name <> " IS NOT NULL") [] }

groupBy :: Query -> SelectModifiers t
groupBy name = mempty { selModGroupBy = [name] }

asc :: Query -> SelectModifiers t
asc name = mempty { selModOrderBy = [name <> " ASC"] }

desc :: Query -> SelectModifiers t
desc name = mempty { selModOrderBy = [name <> " DESC"] }

limit :: Integer -> SelectModifiers t
limit i = mempty { selModLimit = Just $ fromString $ show i }

offset :: Integer -> SelectModifiers t
offset i = mempty { selModOffset = Just $ fromString $ show i }

selectModifiersToQuery :: SelectModifiers t -> (Query, [Action])
selectModifiersToQuery mods =
  let (whereClause, whereActions) = case selModWhere mods of
        WhereModifier "" _ -> ("", [])
        WhereModifier q actions -> (" WHERE " <> q, actions)

      groupByClause = case selModGroupBy mods of
        [] -> ""
        names -> (" GROUP BY " <>) . mconcat . intersperse ", " $ names

      orderByClause = case selModOrderBy mods of
        [] -> ""
        names -> (" ORDER BY " <>) . mconcat . intersperse ", " $ names

      limitClause  = maybe "" (" LIMIT "  <>) $ selModLimit  mods
      offsetClause = maybe "" (" OFFSET " <>) $ selModOffset mods

      clause = whereClause <> groupByClause <> orderByClause
               <> limitClause <> offsetClause

  in (clause, whereActions)

class IsModifier t a | a -> t where
  toSelectModifiers :: a -> SelectModifiers t

instance IsModifier t (SelectModifiers t) where
  toSelectModifiers = id

class IsField t f | f -> t where
  toFieldName :: f a -> Query

newtype Field f a = Field { unField :: f a }

instance IsField t f => ToField (Field f a) where
  toField = Plain . BSB.byteString . fromQuery . toFieldName . unField

data GenericModifier f
  = forall a. ToField a => f a :==  a
  | forall a. ToField a => f a :/=  a
  | forall a. ToField a => f a :<   a
  | forall a. ToField a => f a :>   a
  | forall a. ToField a => f a :<=  a
  | forall a. ToField a => f a :>=  a
  | forall a. ToField a => f a :==~ f a
  | forall a. ToField a => f a :/=~ f a
  | forall a. ToField a => f a :<~  f a
  | forall a. ToField a => f a :>~  f a
  | forall a. ToField a => f a :<=~ f a
  | forall a. ToField a => f a :>=~ f a
  | GenericModifier f :&& GenericModifier f
  | GenericModifier f :|| GenericModifier f
  | forall a. ToField a => BelongsTo (f a) [a]
  | forall a. IsNull (f a)
  | forall a. IsNotNull (f a)
  | forall a. Asc (f a)
  | forall a. Desc (f a)
  | Limit Integer
  | Offset Integer
  | NoGenericModifier

instance IsField t f => IsModifier t (GenericModifier f) where
  toSelectModifiers gm = case gm of
    f  :==  x -> toFieldName f ==. x
    f  :/=  x -> toFieldName f /=. x
    f  :<   x -> toFieldName f <.  x
    f  :>   x -> toFieldName f >.  x
    f  :<=  x -> toFieldName f <=. x
    f  :>=  x -> toFieldName f >=. x
    f  :==~ x -> toFieldName f ==. Field x
    f  :/=~ x -> toFieldName f /=. Field x
    f  :<~  x -> toFieldName f <.  Field x
    f  :>~  x -> toFieldName f >.  Field x
    f  :<=~ x -> toFieldName f <=. Field x
    f  :>=~ x -> toFieldName f >=. Field x
    m1 :&& m2 -> toSelectModifiers m1 &&. toSelectModifiers m2
    m1 :|| m2 -> toSelectModifiers m1 ||. toSelectModifiers m2
    BelongsTo f xs -> toFieldName f `belongsTo` xs
    IsNull    f    -> isNull $ toFieldName f
    IsNotNull f    -> isNotNull $ toFieldName f
    Asc       f    -> asc  $ toFieldName f
    Desc      f    -> desc $ toFieldName f
    Limit     n    -> limit  n
    Offset    n    -> offset n
    NoGenericModifier -> mempty

instance Monoid (GenericModifier f) where
  mempty  = NoGenericModifier
  mappend = (:&&)

infixl 9 :==
infixl 9 :/=
infixl 9 :<
infixl 9 :>
infixl 9 :<=
infixl 9 :>=

infixl 8 :&&
infixl 7 :||

commaSeparated :: [Query] -> Query
commaSeparated = mconcat . intersperse ", "

logQuery :: MonadIO m => Query -> m ()
#ifdef DEBUG_SQL
logQuery req = liftIO $ hPutStrLn stderr $
  "SQL request: " ++ BS.unpack (fromQuery req)
#else
logQuery _ = return ()
#endif
{-# INLINE logQuery #-}

exhaustFields :: FieldParser a -> RowParser [a]
exhaustFields parser = ($ []) <$> go parser id
  where
    go :: FieldParser a -> ([a] -> [a]) -> RowParser ([a] -> [a])
    go _parser f =
      (do
        x <- fieldWith _parser
        let f' = f . (x :)
        go _parser f')
      <|> return f

pureFieldParser :: FieldParser PureField
pureFieldParser f mBS = do
  name <- typename f
  return $ PureField name mBS

pureFieldsParser :: RowParser [PureField]
pureFieldsParser = exhaustFields pureFieldParser

_insert :: ToRow a => Connection -> Query -> [Query] -> Query -> [a]
        -> IO [PureRow]
_insert conn tbl columns ret vals = do
  let req = "INSERT INTO " <> tbl <> " (" <> commaSeparated columns
            <> ") VALUES (" <> commaSeparated (map (const "?") columns)
            <> ") RETURNING " <> ret
  logQuery req
  returningWith pureFieldsParser conn req vals

_select :: Connection -> Query -> [Query] -> SelectModifiers a -> IO [PureRow]
_select conn tbl columns mods = do
  let (clauses, vals) = selectModifiersToQuery mods
      req = "SELECT " <> commaSeparated columns <> " FROM " <> tbl <> clauses
  logQuery req
  queryWith pureFieldsParser conn req vals

_update :: Connection -> Query -> [(Query, Action)] -> SelectModifiers a
        -> IO Int64
_update conn tbl pairs mods = do
  let fieldTpl         = commaSeparated $ map ((<> " = ?") . fst) pairs
      vals             = map snd pairs
      (clauses, vals') = selectModifiersToQuery mods
      req              = "UPDATE " <> tbl <> " SET " <> fieldTpl <> clauses
  logQuery req
  execute conn req $ vals ++ vals'

_delete :: Connection -> Query -> SelectModifiers a -> IO Int64
_delete conn tbl mods = do
  let (clauses, vals) = selectModifiersToQuery mods
      req = "DELETE FROM " <> tbl <> clauses
  logQuery req
  execute conn req vals

createMany :: (MonadDataLayer m, Storable a) => [a] -> m [EntityID a]
createMany = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> [a] -> m [EntityID a]
    go proxy vals = do
      rows <- dbInsert (tableName proxy) (fieldNames proxy)
                       (idFieldName proxy) vals
      case parsePureRows rows of
        Left  err -> throwError $ SQLInvalidFieldError err
        Right ids -> return $ map fromOnly ids

create :: (MonadDataLayer m, Storable a) => a -> m (EntityID a)
create val = do
  ids <- createMany [val]
  case listToMaybe ids of
    Just i  -> return i
    Nothing -> throwError $
      SQLError "create: SQL statement did not return anything"

select :: (MonadDataLayer m, Storable a, IsModifier a m') => m' -> m [Entity a]
select = go Proxy
  where
    go :: (MonadDataLayer m, Storable a, IsModifier a m') => Proxy a -> m'
       -> m [Entity a]
    go proxy customMods = do
      let mods = toSelectModifiers customMods
      rows <- dbSelect (tableName proxy)
                       (idFieldName proxy : fieldNames proxy) mods
      case parsePureRows rows of
        Left  err  -> throwError $ SQLInvalidFieldError err
        Right vals -> return vals

getMany :: (MonadDataLayer m, Storable a) => [EntityID a] -> m [Entity a]
getMany = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> [EntityID a]
       -> m [Entity a]
    go proxy ids = select $ idFieldName proxy `belongsTo` ids

getMaybe :: (MonadDataLayer m, Storable a) => EntityID a -> m (Maybe a)
getMaybe = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a -> m (Maybe a)
    go proxy i = do
      ents <- select $ idFieldName proxy ==. i <> limit 1
      return $ fmap entityVal $ listToMaybe ents

get :: (MonadDataLayer m, Storable a) => EntityID a -> m a
get i = maybe (throwError SQLRecordNotFoundError) return =<< getMaybe i

countRows :: (MonadDataLayer m, Storable a, IsModifier a m') => Proxy a -> m'
          -> m Int64
countRows proxy customMods = do
  let mods = toSelectModifiers customMods
  rows <- dbSelect (tableName proxy) ["COUNT(*)"] mods
  case listToMaybe rows of
    Nothing -> throwError $
      SQLError "countRows: SQL statement did not return anything"
    Just row -> case parsePureRow row of
      Left  err          -> throwError $ SQLInvalidFieldError err
      Right (Only count) -> return count

(=.) :: ToField a => Query -> a -> (Query, Action)
(=.) q x = (q, toField x)

updateMany :: (MonadDataLayer m, Storable a) => [(Query, Action)]
           -> SelectModifiers a -> m Int64
updateMany = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> [(Query, Action)]
       -> SelectModifiers a -> m Int64
    go proxy []    mods = countRows proxy mods
    go proxy pairs mods = dbUpdate (tableName proxy) pairs mods

updateMany' :: (MonadDataLayer m, Storable a) => [(Query, Action)]
            -> SelectModifiers a -> m ()
updateMany' pairs mods = void $ updateMany pairs mods

update :: (MonadDataLayer m, Storable a) => EntityID a -> [(Query, Action)]
       -> m ()
update = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a
       -> [(Query, Action)] -> m ()
    go proxy i pairs = do
      count <- updateMany pairs $ idToMods proxy i
      when (count == 0) $ throwError SQLRecordNotFoundError
      when (count >  1) $
        throwError $ SQLError "update: Modified more than one record"

    idToMods :: Storable a => Proxy a -> EntityID a -> SelectModifiers a
    idToMods proxy i = idFieldName proxy ==. i

save :: (MonadDataLayer m, Storable a) => EntityID a -> a -> m ()
save = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a -> a -> m ()
    go proxy i val = update i $ zip (fieldNames proxy) (toRow val)

deleteMany :: (MonadDataLayer m, Storable a) => Proxy a -> SelectModifiers a
           -> m Int64
deleteMany proxy mods = dbDelete (tableName proxy) mods

deleteMany' :: (MonadDataLayer m, Storable a) => Proxy a -> SelectModifiers a
            -> m ()
deleteMany' proxy mods = void $ deleteMany proxy mods

delete :: (MonadDataLayer m, Storable a) => EntityID a -> m ()
delete = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a -> m ()
    go proxy i = do
      count <- deleteMany proxy $ idFieldName proxy ==. i
      when (count == 0) $ throwError SQLRecordNotFoundError
      when (count >  1) $
        throwError $ SQLError "delete: Deleted more than one record"

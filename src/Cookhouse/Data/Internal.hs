{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Data.Internal
  ( Int64
  , SelectModifiers
  , mkNamesQuery
  , getConn
  , create
  , save
  , getMaybe
  , get
  , getMany
  , select
  , update
  , delete
  , deleteMany
  , countRows
  , whereWith
  , (==.)
  , (/=.)
  , (<.)
  , (>.)
  , (<=.)
  , (>=.)
  , (&&.)
  , (||.)
  , belongsTo
  , isNull
  , isNotNull
  , groupBy
  , asc
  , desc
  , limit
  , offset
  , (=.)
  , selectModifiersToQuery
  ) where

import           Prelude

import           GHC.Int

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Data.List hiding (groupBy, delete)
import           Data.Maybe
import           Data.Monoid
import           Data.String
#ifdef DEBUG_SQL
import qualified Data.ByteString.Char8 as BS
#endif

#ifdef DEBUG_SQL
import           System.IO
#endif

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
#ifdef DEBUG_SQL
import           Database.PostgreSQL.Simple.Types
#endif

import           Cookhouse.Data.Types
import           Cookhouse.Errors

getConn :: MonadDataLayer m => m Connection
getConn = ask

mkNamesQuery :: Storable a => Proxy a -> Query
mkNamesQuery = mconcat . intersperse ", " . fieldNames

mkMarksQuery :: Storable a => Proxy a -> Query
mkMarksQuery = mconcat . intersperse ", " . map (const "?") . fieldNames

mkNamesWithMarksQuery :: Storable a => Proxy a -> Query
mkNamesWithMarksQuery =
  mconcat . intersperse ", " . map (<> " = ?") . fieldNames

data WhereModifier = WhereModifier Query [Action]

instance Monoid WhereModifier where
  mempty = WhereModifier "" []
  mappend (WhereModifier aquery aactions) (WhereModifier bquery bactions) =
    let q | aquery == "" = bquery
          | bquery == "" = aquery
          | otherwise    = "(" <> aquery <> ") AND (" <> bquery <> ")"
    in WhereModifier q $ aactions ++ bactions

data SelectModifiers = SelectModifiers
  { selModWhere   :: WhereModifier
  , selModGroupBy :: [Query]
  , selModOrderBy :: [Query]
  , selModLimit   :: Maybe Query
  , selModOffset  :: Maybe Query
  }

instance Monoid SelectModifiers where
  mempty = SelectModifiers mempty [] [] Nothing Nothing
  mappend amods bmods = SelectModifiers
    { selModWhere   = selModWhere   amods <> selModWhere   bmods
    , selModGroupBy = selModGroupBy amods <> selModGroupBy bmods
    , selModOrderBy = selModOrderBy amods <> selModOrderBy bmods
    , selModLimit   = selModLimit   amods <> selModLimit   bmods
    , selModOffset  = selModOffset  amods <> selModOffset  bmods
    }

whereWith :: ToField a => Query -> Query -> a -> SelectModifiers
whereWith op name val =
  let whereMod = WhereModifier (name <> " " <> op <> " ?") [toField val]
  in mempty { selModWhere = whereMod }

(==.) :: ToField a => Query -> a -> SelectModifiers
(==.) = whereWith "="

(/=.) :: ToField a => Query -> a -> SelectModifiers
(/=.) = whereWith "<>"

(<.) :: ToField a => Query -> a -> SelectModifiers
(<.)  = whereWith "<"

(>.) :: ToField a => Query -> a -> SelectModifiers
(>.)  = whereWith ">"

(<=.) :: ToField a => Query -> a -> SelectModifiers
(<=.)  = whereWith "<="

(>=.) :: ToField a => Query -> a -> SelectModifiers
(>=.)  = whereWith ">="

(&&.) :: SelectModifiers -> SelectModifiers -> SelectModifiers
(&&.) = (<>)

(||.) :: SelectModifiers -> SelectModifiers -> SelectModifiers
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

belongsTo :: ToField a => Query -> [a] -> SelectModifiers
 -- Prevent a syntax error
belongsTo _ [] = mempty { selModWhere = WhereModifier "1 = 0" [] }
belongsTo name values =
  let marks    = fromString . concat . intersperse ", "
               . map (const "?") $ values
      actions  = map toField values
      whereMod = WhereModifier (name <> " IN (" <> marks <> ")") actions
  in mempty { selModWhere = whereMod }

isNull :: Query -> SelectModifiers
isNull name = mempty { selModWhere = WhereModifier (name <> " IS NULL") [] }

isNotNull :: Query -> SelectModifiers
isNotNull name =
  mempty { selModWhere = WhereModifier (name <> " IS NOT NULL") [] }

groupBy :: Query -> SelectModifiers
groupBy name = mempty { selModGroupBy = [name] }

asc :: Query -> SelectModifiers
asc name = mempty { selModOrderBy = [name <> " ASC"] }

desc :: Query -> SelectModifiers
desc name = mempty { selModOrderBy = [name <> " DESC"] }

limit :: Integer -> SelectModifiers
limit i = mempty { selModLimit = Just $ fromString $ show i }

offset :: Integer -> SelectModifiers
offset i = mempty { selModOffset = Just $ fromString $ show i }

selectModifiersToQuery :: SelectModifiers -> (Query, [Action])
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

catchSQLErrors :: MonadDataLayer m => IO a -> m a
catchSQLErrors f = do
  eRes <- liftIO $ try f
  case eRes of
    Left  err -> throwError $ SQLError $ Just err
    Right res -> return res

create :: (MonadDataLayer m, Storable a) => a -> m (EntityID a)
create val = do
  conn <- getConn
  let proxy = mkProxy val
      req   = "INSERT INTO " <> tableName proxy <> " (" <> mkNamesQuery proxy
              <> ") VALUES (" <> mkMarksQuery proxy <> ") RETURNING "
              <> idFieldName proxy
#ifdef DEBUG_SQL
  liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
  ids <- catchSQLErrors $ returning conn req [val]
  case listToMaybe ids of
    Just (Only i) -> return i
    Nothing       -> throwError $ SQLError Nothing

save :: (MonadDataLayer m, Storable a) => Entity a -> m ()
save entity = do
  conn <- getConn
  let val   = entityVal entity
      proxy = mkProxy val
      req   = "UPDATE " <> tableName proxy <> " SET "
              <> mkNamesWithMarksQuery proxy <> " WHERE " <> idFieldName proxy
              <> " = ?"
#ifdef DEBUG_SQL
  liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
  count <- catchSQLErrors $ execute conn req $ val :. Only (entityID entity)
  when (count /= 1) $ throwError SQLRecordNotFoundError

getMaybe :: (MonadDataLayer m, Storable a) => EntityID a -> m (Maybe a)
getMaybe = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a -> m (Maybe a)
    go proxy i = do
      conn <- getConn
      let req   = "SELECT " <> mkNamesQuery proxy <> " FROM " <> tableName proxy
                  <> " WHERE " <> idFieldName proxy <> " = ? LIMIT 1"
#ifdef DEBUG_SQL
      liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
      res <- catchSQLErrors $ query conn req $ Only i
      return $ listToMaybe res

get :: (MonadDataLayer m, Storable a) => EntityID a -> m a
get i = maybe (throwError SQLRecordNotFoundError) return =<< getMaybe i

getMany :: (MonadDataLayer m, Storable a) => [EntityID a] -> m [Entity a]
getMany = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> [EntityID a]
       -> m [Entity a]
    go proxy ids = select $ idFieldName proxy `belongsTo` ids

instance FromRow a => FromRow (Maybe a) where
  fromRow = fmap Just fromRow <|> (exhaust >> return Nothing)
    where
      exhaust :: RowParser (Maybe a)
      exhaust = do
        n <- numFieldsRemaining
        replicateM_ n $ fieldWith $ \_ _ -> return ()
        return Nothing

select :: (MonadDataLayer m, Storable a) => SelectModifiers -> m [Entity a]
select = fmap catMaybes . go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> SelectModifiers
       -> m [Maybe (Entity a)]
    go proxy mods = do
      conn <- getConn
      let (clauses, vals) = selectModifiersToQuery mods
          req = "SELECT " <> idFieldName proxy <> ", " <> mkNamesQuery proxy
                <> " FROM " <> tableName proxy <> clauses
#ifdef DEBUG_SQL
      liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
      catchSQLErrors $ query conn req vals

(=.) :: ToField a => Query -> a -> (Query, Action)
(=.) q x = (q <> " = ?", toField x)

update :: (MonadDataLayer m, Storable a) => EntityID a -> [(Query, Action)]
       -> m ()
update = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a
       -> [(Query, Action)] -> m ()
    go _ _ [] = return ()
    go proxy i pairs = do
      conn <- getConn
      let assignments = mconcat . intersperse ", " . map fst $ pairs
          req         = "UPDATE " <> tableName proxy <> " SET " <> assignments
                        <> " WHERE " <> idFieldName proxy <> " = ?"
          val         = map snd pairs :. Only i
#ifdef DEBUG_SQL
      liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
      void $ catchSQLErrors $ execute conn req val

delete :: (MonadDataLayer m, Storable a) => EntityID a -> m ()
delete = go Proxy
  where
    go :: (MonadDataLayer m, Storable a) => Proxy a -> EntityID a -> m ()
    go proxy i = do
      conn <- getConn
      let req = "DELETE FROM " <> tableName proxy
                <> " WHERE " <> idFieldName proxy <> " = ?"
#ifdef DEBUG_SQL
      liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
      void $ catchSQLErrors $ execute conn req $ Only i

deleteMany :: (MonadDataLayer m, Storable a) => Proxy a -> SelectModifiers
           -> m ()
deleteMany proxy mods = do
  conn <- getConn
  let (clauses, vals) = selectModifiersToQuery mods
      req = "DELETE FROM " <> tableName proxy <> clauses
#ifdef DEBUG_SQL
  liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
  void $ catchSQLErrors $ execute conn req vals

countRows :: (MonadDataLayer m, Storable a) => Proxy a -> SelectModifiers
          -> m Integer
countRows proxy mods = do
  conn <- getConn
  let (clauses, vals) = selectModifiersToQuery mods
      req = "SELECT COUNT(*) FROM " <> tableName proxy <> clauses
#ifdef DEBUG_SQL
  liftIO $ hPutStrLn stderr $ "SQL request: " ++ BS.unpack (fromQuery req)
#endif
  counts <- catchSQLErrors $ query conn req vals
  case listToMaybe counts of
    Nothing           -> throwError $ SQLError Nothing
    Just (Only count) -> return count

persistent-th-rewrite
=======================

A GHC preprocessor that rewrites persistent TH splices into regular Haskell functions and types.

### Usage

Add `{-# options_ghc -F -pgmF=persistent-th-rewrite-pp #-}`

### Usage

```haskell
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

mkPersist sqlSettings [persistLowerCase|
Account
    email Text
|]
```

becomes

```haskell
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, GADTs #-}
instance PersistEntity Account where
  type PersistEntityBackend Account = SqlBackend
  data Unique Account
  newtype Key Account = AccountKey {unAccountKey :: (BackendKey SqlBackend)}
    deriving newtype (Show,
                      Read,
                      Eq,
                      Ord,
                      PathPiece,
                      ToHttpApiData,
                      FromHttpApiData,
                      PersistField,
                      PersistFieldSql,
                      ToJSON,
                      FromJSON)

  data EntityField Account typ
     = typ ~ Key Account => AccountId
     | typ ~ Text => AccountEmail

  keyToValues               = undefined
  keyFromValues             = undefined
  entityDef                 = undefined
  toPersistFields           = undefined
  fromPersistValues         = undefined
  persistUniqueToFieldNames = undefined
  persistUniqueToValues     = undefined
  persistUniqueKeys         = undefined
  persistFieldDef           = undefined
  persistIdField            = undefined
  fieldLens                 = undefined

instance ToBackendKey SqlBackend Account where
  toBackendKey = undefined
  fromBackendKey = undefined

instance TypeError (NoUniqueKeysError Account) => OnlyOneUniqueKey Account where
  onlyUniqueP _ = undefined

instance TypeError (MultipleUniqueKeysError Account) => AtLeastOneUniqueKey Account where
  requireUniquesP _ = undefined

instance PersistField Account where
  toPersistValue = undefined
  fromPersistValue = undefined

instance PersistFieldSql Account where sqlType _ = SqlString

data Account   = Account {accountEmail :: !Text}
type AccountId = Key Account
```

### Why

Cross-compilation of Haskell code requires TH splices to be executed on the target machine, as opposed to the host.
This can be a non-starter for many projects.

### Limitations

Only `share` is supported. Most functions are undefined. Implementations can be provided, but the specific use case is for front-end code that needs the data families generated, but not the member functions of `PersistEntity`.

### Long term

GHC should split up Template Haskell into pure and impure variants. Allowing pure TH code to be executed on the host, and impure on the target.

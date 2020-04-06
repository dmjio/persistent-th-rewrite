persistent-th-rewrite
=======================

A GHC plugin that rewrites TH splices into regular Haskell functions.

### Usage
```haskell
build-depends: persistent-th-rewrite
ghc-options: -fplugin=GHC.Plugin.PersistentThRewrite
```

### Operation

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
Person
    name String
    age Int
    deriving Show
|]
```

becomes

```haskell
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, GADTs #-}
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

data Person = Person
    { personName :: !String
    , personAge :: !Int
    }
  deriving Show

type PersonId = Key Person

instance PersistEntity Person where
    newtype Key Person = PersonKey (BackendKey SqlBackend)
        deriving (PersistField, Show, Eq, Read, Ord)
    -- A Generalized Algebraic Datatype (GADT).
    -- This gives us a type-safe approach to matching fields with
    -- their datatypes.
    data EntityField Person typ where
        PersonId   :: EntityField Person PersonId
        PersonName :: EntityField Person String
        PersonAge  :: EntityField Person Int

    data Unique Person
    type PersistEntityBackend Person = SqlBackend

    toPersistFields (Person name age) =
        [ SomePersistField name
        , SomePersistField age
        ]

    fromPersistValues [nameValue, ageValue] = Person
        <$> fromPersistValue nameValue
        <*> fromPersistValue ageValue
    fromPersistValues _ = Left "Invalid fromPersistValues input"

    -- Information on each field, used internally to generate SQL statements
    persistFieldDef PersonId = FieldDef
        (HaskellName "Id")
        (DBName "id")
        (FTTypeCon Nothing "PersonId")
        SqlInt64
        []
        True
        NoReference
    persistFieldDef PersonName = FieldDef
        (HaskellName "name")
        (DBName "name")
        (FTTypeCon Nothing "String")
        SqlString
        []
        True
        NoReference
    persistFieldDef PersonAge = FieldDef
        (HaskellName "age")
        (DBName "age")
        (FTTypeCon Nothing "Int")
        SqlInt64
        []
        True
        NoReference
```

### Why

Cross-compilation of Haskell code requires TH splices to be executed on the target machine, as opposed to the host.
This can be a non-starter for many projects.

### Limitations

Only `share` is supported.

### Preprocessor

It is possible to use the executable bundled with this library as a pre-processor.

Add `{-# options_ghc -F -Fpgm=persistent-th-rewrite-pp #-}`

### Long term

GHC should split up Template Haskell into pure and impure variants. Allowing pure TH code to be executed on the host, and impure on the target.


{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow      ((&&&))
import Data.Char          (toLower, toUpper)
import Data.List          (isSuffixOf, isPrefixOf, intercalate)
import Data.List.Split    (splitOn)
import Data.Maybe         (mapMaybe)
import System.Environment (getArgs)
import Text.Printf        (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    original:input:output:_ ->
      go input output

go :: String -> String -> IO ()
go input output = writeFile output (genAll input)

rewrite :: String -> String
rewrite str = do
  let pred = not . isPrefixOf "share"
      (front, back) = (takeWhile pred &&& dropWhile pred) (lines str)
      text = takeWhile (not . isPrefixOf "|]" . dropWhile (/='|')) (drop 1 back)
  unlines front ++ genAll (unlines text) ++ do
    unlines
      $ drop 1
      $ dropWhile (not . isPrefixOf "|]" . dropWhile (/='|'))
      $ drop 1 back

genAll :: String -> String
genAll xs = do
  let recs = parseQQ xs
      insts = intercalate "\n" (genInstances <$> recs)
  intercalate "\n" [intercalate "\n" (fmap show recs), insts]

parseQQ :: String -> [Rec]
parseQQ str
  = mapMaybe process
  $ splitOn (pure []) $ clean (lines str)
    where
      clean :: [String] -> [String]
      clean = dropComments . fmap prune
      dropComments =
        Prelude.filter (not . isPrefixOf "--")
      prune
        = reverse
        . dropWhile (==' ')
        . reverse
        . dropWhile (==' ')

data Rec
  = Rec
  { recName :: String
  , recFields :: [Field]
  } deriving (Eq)

instance Show Rec where
  show Rec {..} = unlines [ "data " <> recName, recBody ]
    where
      recBody = "  { " ++ genFields ++ "  } deriving (Eq, Show)"
      genFields = unlines $ commaify (fmap show recFields)
      commaify [] = []
      commaify (x:xs) = x : fmap ("  , " ++) xs

data Field
  = Field
  { fieldName :: String
  , fieldType :: String
  , fieldParens :: Bool
  } deriving (Eq)

instance Show Field where
  show Field {..}
    = fieldName
    ++ " :: "
    ++ do if fieldParens
            then "!(" <> fieldType <> ")"
            else "!" <> fieldType

extractType :: String -> String
extractType zs = addMaybe . go 0 . prune $ zs
  where
    addMaybe ys =
      if "Maybe" `elem` words zs
        then "Maybe " <> ys
        else ys
    go 0 []  = []
    go n ('(':xs) = '(' : go (n+1) xs
    go n (')':xs) = ')' : go (n-1) xs
    go n (x:xs)   = x : go n xs

prune :: [Char] -> [Char]
prune
  = dropWhile (/='(')
  . reverse
  . dropWhile (/=')')
  . reverse

process :: [String] -> Maybe Rec
process [] = Nothing
process (recName:fields) = Just Rec {..}
  where
    recFields = mapMaybe processField fields
    processField field
      | isPrefixOf "Unique" field = Nothing
      | isPrefixOf "pk" field = do
          let fieldName = lowerCase recName <> "Pk"
              fieldType = words field !! 1
              fieldParens = False
          Just Field {..}
      | isPrefixOf "Primary" field = Nothing
      | "(" `isPrefixOf` (words field !! 1) = do
          let fieldName = lowerCase recName <> upperCase (words field !! 0)
              fieldType = extractType field
              fieldParens = True
          Just Field {..}
      | otherwise =
          case words field of
            [fName,fType, "Maybe"] -> do
              let fieldName = lowerCase recName <> upperCase fName
                  fieldParens = True
                  fieldType =
                    if "Id" `isSuffixOf` fType
                      then "Maybe (Key " <> iterate init fType !! 2 <> ")"
                      else "Maybe " <> fType
              Just Field {..}
            fName:fType:_ -> do
              let fieldName = lowerCase recName <> upperCase fName
                  fieldParens = "Id" `isSuffixOf` fType
                  fieldType =
                    if "Id" `isSuffixOf` fType
                      then "Key " <> iterate init fType !! 2
                      else fType
              Just Field {..}
            _ -> Nothing

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = toLower x : xs

upperCase :: String -> String
upperCase [] = []
upperCase (x:xs) = toUpper x : xs

genInstances :: Rec -> String
genInstances record@Rec {..} =
  unlines
  [ printf "instance PersistEntity %s where" recName
  , printf "data Unique %s" recName
  , printf "  type PersistEntityBackend %s = SqlBackend" recName
  , printf "  newtype Key %s = %sKey {un%sKey :: (BackendKey SqlBackend)}"
      recName recName recName
  , "    deriving newtype (Show,"
  , "                      Read,"
  , "                      Eq,"
  , "                      Ord,"
  , "                      PathPiece,"
  , "                      ToHttpApiData,"
  , "                      FromHttpApiData,"
  , "                      PersistField,"
  , "                      PersistFieldSql,"
  , "                      ToJSON,"
  , "                      FromJSON)"
  , genEntityfield record
  , "  keyToValues               = undefined"
  , "  keyFromValues             = undefined"
  , "  entityDef                 = undefined"
  , "  toPersistFields           = undefined"
  , "  fromPersistValues         = undefined"
  , "  persistUniqueToFieldNames = undefined"
  , "  persistUniqueToValues     = undefined"
  , "  persistUniqueKeys         = undefined"
  , "  persistFieldDef           = undefined"
  , "  persistIdField            = undefined"
  , "  fieldLens                 = undefined"
  , printf "type %sId = Key %s" recName recName
  , printf "instance PersistFieldSql %s where sqlType _ = SqlString" recName
  , printf "instance ToBackendKey SqlBackend %s where\n  toBackendKey = undefined\n  fromBackendKey = undefined" recName
  , printf "instance PersistField %s where\n  toPersistValue = undefined\n  fromPersistValue = undefined" recName
  ]

genEntityfield Rec {..} =
  unlines $
    [ printf "  data EntityField %s typ" recName
    , printf "    = typ ~ Key %s => %sId" recName recName
    ] ++
    [ printf "    | typ ~ %s => %s " fieldType (upperCase fieldName)
    | Field {..} <- recFields
    ]

-- instance PersistEntity Account where
--   type PersistEntityBackend Account = SqlBackend
--   data Unique Account
--   newtype Key Account = AccountKey {unAccountKey :: (BackendKey SqlBackend)}
--     deriving newtype (Show,
--                       Read,
--                       Eq,
--                       Ord,
--                       PathPiece,
--                       ToHttpApiData,
--                       FromHttpApiData,
--                       PersistField,
--                       PersistFieldSql,
--                       ToJSON,
--                       FromJSON)

--   data EntityField Account typ
--      = typ ~ Key Account => AccountId
--      | typ ~ Text => AccountEmail

--   keyToValues               = undefined
--   keyFromValues             = undefined
--   entityDef                 = undefined
--   toPersistFields           = undefined
--   fromPersistValues         = undefined
--   persistUniqueToFieldNames = undefined
--   persistUniqueToValues     = undefined
--   persistUniqueKeys         = undefined
--   persistFieldDef           = undefined
--   persistIdField            = undefined
--   fieldLens                 = undefined

-- instance ToBackendKey SqlBackend Account where
--   toBackendKey = undefined
--   fromBackendKey = undefined

-- instance TypeError (NoUniqueKeysError Account) => OnlyOneUniqueKey Account where
--   onlyUniqueP _ = undefined

-- instance TypeError (MultipleUniqueKeysError Account) => AtLeastOneUniqueKey Account where
--   requireUniquesP _ = undefined

-- instance PersistField Account where
--   toPersistValue = undefined
--   fromPersistValue = undefined

-- instance PersistFieldSql Account where sqlType _ = SqlString

-- data Account   = Account {accountEmail :: !Text}
-- type AccountId = Key Account

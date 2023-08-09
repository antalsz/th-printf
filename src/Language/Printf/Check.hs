{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Printf.Check where

import Control.Monad
import Control.Monad.Error.Class hiding (Error(..))
import Control.Monad.Writer.Class
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES
import Data.Functor
import Data.Sequence (Seq())

import qualified Language.Printf.FormatString as FS
import Language.Printf.FormatString as Reexport (IntegerBase(..), FloatingStyle(..), Case(..))

data IgnoredFlagReason = SupersedingFlag FS.Flag
                       deriving (Eq, Ord, Show, Read)

data NotHaskell = IntFastest
                | WideCharacter
                | WideString
                deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Warning = RepeatedFlag FS.Flag
             | IgnoredFlag IgnoredFlagReason FS.Flag
             | NotHaskell NotHaskell
             | PercentLength FS.Length
             | MixedPositionalAndNonPositionalArguments
             | MeaninglessInHaskell FS.Length FS.Specifier
             deriving (Eq, Ord, Show, Read)

type Warnings = Seq Warning

warn :: MonadWriter Warnings m => Warning -> m ()
warn = tell . pure

data Unsupported = PtrDiff
                 | IntegerWidth Word
                 | LongDouble
                 | DecimalFloat FS.DecimalWidth
                 deriving (Eq, Ord, Show, Read)

data Error = Unsupported Unsupported
           | Incompatible FS.Length FS.Specifier
           | NoAlternativeStyle FS.Specifier
           | IllegalZeroPadding FS.Specifier
           | IllegalLengthModifier FS.Length FS.Specifier
           deriving (Eq, Ord, Show, Read)

type Errors = Seq Error

error_ :: MonadError Errors m => Error -> m a
error_ = throwError . pure

data Padding = Zeros | Spaces
             deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Justification = RightJustify Padding
                   | LeftJustify
                   deriving (Eq, Ord, Show, Read)

data PositiveIndicator = NoIndicator | Plus | Space
                       deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PrintingStyle = Standard | Alternative
                   deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ThousandsSeparators = NoSeparators | ThousandsSeparators
                         deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Flags = Flags { justification       :: Justification
                   , positiveIndicator   :: PositiveIndicator
                   , printingStyle       :: PrintingStyle
                   , thousandsSeparators :: ThousandsSeparators -- POSIX
                   }
           deriving (Eq, Ord, Show, Read)

deduplicateFlags :: MonadWriter Warnings m => [FS.Flag] -> m (EnumSet FS.Flag)
deduplicateFlags = go ES.empty where
  go unique [] = pure unique
  go unique (flag:flags)
    | flag `ES.member` unique = warn (RepeatedFlag flag) *> go unique flags
    | otherwise               = go (ES.insert flag unique) flags

data Supersession = Super | Sub | Neither
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

normalizeFlags :: MonadWriter Warnings m => [FS.Flag] -> m Flags
normalizeFlags flagList = do
  flags <- deduplicateFlags flagList
  let flag = (`ES.member` flags)
      
      supersedes super sub =
        if flag super then
          when (flag sub) (warn $ IgnoredFlag (SupersedingFlag super) sub)
            $> Super
        else pure $ if flag sub then
          Sub
        else
          Neither
      
  justification <- FS.LeftJustify `supersedes` FS.ZeroPadding <&> \case
    Super   -> LeftJustify
    Sub     -> RightJustify Zeros
    Neither -> RightJustify Spaces
  positiveIndicator <- FS.PositivePlus `supersedes` FS.PositiveSpace <&> \case
    Super   -> Plus
    Sub     -> Space
    Neither -> NoIndicator
  let printingStyle = if flag FS.Alternative then Alternative else Standard
  let thousandsSeparators = if flag FS.ThousandsSeparators then ThousandsSeparators else NoSeparators

  pure Flags{..}

data TInteger = AnyIntegral
             | Int
             | Integer
             | Int8
             | Int16
             | Int32
             | Int64
             -- C types – for use with hypothetical extension
             | CSChar -- No CChar?
             | CShort
             | CInt
             | CLong
             | CLongLong
             | CIntMax
             | CSize
             | CPtrdiff
             deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TNatural = AnyNatural
             | Word
             | Natural
             | Word8
             | Word16
             | Word32
             | Word64
             -- C types – for use with hypothetical extension
             | CUChar
             | CUShort
             | CUInt
             | CULong
             | CULongLong
             | CUIntMax
             | CUSize
             | CUPtrdiff -- Same as CPtrdiff
             deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TReal = AnyReal
          | Float -- Extension
          | Double
          -- C types – for use with hypothetical extension
          | CFloat -- Extension
          | CDouble
          deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StringType = Generic
                | HaskellString -- HS extension
                | StrictText -- HS extension
                | LazyText -- HS extension
                deriving (Eq, Ord, Show, Read, Enum, Bounded)

data FunctionType = Simple
                  | Full (Maybe FS.Length)
                  deriving (Eq, Ord, Show, Read)

data Type
  = TInteger TInteger
  | TNatural TNatural IntegerBase
  | Real TReal FloatingStyle Case
  | Character
  | String StringType
  | Pointer
  | Formattable (Maybe FS.Length)
  | Showable
  | Function FunctionType
  | Buffer
  deriving (Eq, Ord, Show, Read)

intWithWidth :: MonadError Errors m => Word -> m TInteger
intWithWidth  8 = pure Int8
intWithWidth 16 = pure Int16
intWithWidth 32 = pure Int32
intWithWidth 64 = pure Int64
intWithWidth wd = error_ . Unsupported $ IntegerWidth wd

wordWithWidth :: MonadError Errors m => Word -> m TNatural
wordWithWidth  8 = pure Word8
wordWithWidth 16 = pure Word16
wordWithWidth 32 = pure Word32
wordWithWidth 64 = pure Word64
wordWithWidth wd = error_ . Unsupported $ IntegerWidth wd

data NormalizedType = Percent | Type Type deriving (Eq, Ord, Show, Read)

normalizeType :: (MonadWriter Warnings m, MonadError Errors m)
              => Maybe FS.Length -> FS.Specifier -> m NormalizedType
normalizeType maybeLength specifier =
  let unsupported      = error_ . Unsupported
      incompatible len = error_ $ Incompatible len specifier
  in
  case specifier of
    FS.Integer FS.SignedDecimal ->
      Type . TInteger <$> case maybeLength of
        Nothing                                    -> pure AnyIntegral
        Just FS.Byte                               -> pure Int8
        Just FS.Short                              -> pure Int16
        Just FS.Long                               -> pure Int32
        Just FS.LongLong                           -> pure Int64
        Just FS.IntMax                             -> pure Integer
        Just FS.Size                               -> pure Int
        Just FS.PtrDiff                            -> unsupported PtrDiff
        Just (FS.IntWithWidth FS.Exact w)          -> intWithWidth w
        Just (FS.IntWithWidth FS.FastestAtLeast w) -> warn (NotHaskell IntFastest) *> intWithWidth w
        Just FS.LongDouble                         -> incompatible FS.LongDouble
        Just (FS.DecimalFloat dw)                  -> incompatible $ FS.DecimalFloat dw
    FS.Integer (FS.Unsigned base) ->
      Type . flip TNatural base <$> case maybeLength of
        Nothing                                    -> pure AnyNatural
        Just FS.Byte                               -> pure Word8
        Just FS.Short                              -> pure Word16
        Just FS.Long                               -> pure Word32
        Just FS.LongLong                           -> pure Word64
        Just FS.IntMax                             -> pure Natural
        Just FS.Size                               -> pure Word
        Just FS.PtrDiff                            -> unsupported PtrDiff
        Just (FS.IntWithWidth FS.Exact w)          -> wordWithWidth w
        Just (FS.IntWithWidth FS.FastestAtLeast w) -> warn (NotHaskell IntFastest) *> wordWithWidth w
        Just FS.LongDouble                         -> incompatible FS.LongDouble
        Just (FS.DecimalFloat dw)                  -> incompatible $ FS.DecimalFloat dw
    FS.FloatingPoint fstyle strCase ->
      (\r -> Type $ Real r fstyle strCase) <$> case maybeLength of
        Nothing                   -> pure AnyReal
        Just FS.Short             -> pure Float -- extension
        Just FS.Long              -> pure Double -- semi-extension
        Just FS.LongDouble        -> unsupported LongDouble
        Just (FS.DecimalFloat dw) -> unsupported (DecimalFloat dw)
        Just len                  -> incompatible len
    FS.Character ->
      Type Character <$ case maybeLength of
        Nothing      -> pure ()
        Just FS.Long -> warn $ NotHaskell WideCharacter
        Just len     -> incompatible len
    FS.String ->
      Type (String Generic) <$ case maybeLength of
        Nothing      -> pure ()
        Just FS.Long -> warn $ NotHaskell WideString
        Just len     -> incompatible len
    FS.SpecificString ->
      Type . String <$> case maybeLength of
        Nothing         -> pure HaskellString
        Just FS.PtrDiff -> pure StrictText -- %tS, t for sTrict Text
        Just FS.Long    -> pure LazyText   -- %lS, l for Lazy text
        -- Maybe
        -- Just FS.Byte    -> pure StrictByteString -- %hS, h is for bytes
        -- Just FS.Short   -> pure LazyByteString   -- %hhS, like strict but longer
        -- Yes
        Just len        -> incompatible len
    FS.Pointer ->
      Type Pointer <$ case maybeLength of
        Nothing  -> pure ()
        Just len -> incompatible len
    FS.Formattable -> -- Could split, %@ / %&, for better warnings
      pure . Type $ Formattable maybeLength
    FS.Showable ->
      Type Showable <$ case maybeLength of
        Nothing  -> pure ()
        Just len -> incompatible len
    FS.Function FS.Simple ->
      Type (Function Simple) <$ case maybeLength of
        Nothing  -> pure ()
        Just len -> incompatible len
    FS.Function FS.Full ->
      pure . Type . Function $ Full maybeLength
    FS.Buffer ->
      Type Buffer <$ case maybeLength of
        Nothing  -> pure ()
        Just len -> incompatible len
    FS.Percent ->
      Percent <$ case maybeLength of
        Nothing  -> pure ()
        Just len -> warn (PercentLength len)
     
    -- Deviation possibilities:
    -- %#x with 0 gives 0x0, and similarly %#o -> 00, %#b -> 0b0, %#X -> 0X0, %#B -> 0B0
    -- %x with -42 gives -2a, and similarly %o -> -52, %b -> 101010, %#x -> -0x2a, ...
    --   (%u remains unsigned)
    -- Might specify strict/lax mode for both of those
    -- data ParsingOptions  = ParsingOptions
    --   { posix      :: NoPOSIX | POSIX
    --   , extensions :: CSpecifiersOnly | IncludeExtensions
    --   }
    -- data PrintingOptions = PrintingOptions
    --   { basePrefix           :: NonzeroPrefix | AlwaysPrefix
    --   , nondecimalSignedness :: NondecimalUnsigned | NondecimalFollowsType
    --   }

-- normalizeSpecifier :: FormatSpecifier str -> (Warnings, Either [Error] (CoolFormatSpecifier str))
-- normalizeFormatString :: FormatString str -> (Warnings, Either [Error] (CoolFormatString str))

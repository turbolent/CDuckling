{-# LANGUAGE ForeignFunctionInterface #-}

module CDuckling where
 
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Data.String
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime.TimeZone.Series
import Data.Maybe
import Data.Some
import Text.Read (readMaybe)
import Control.Concurrent.MVar
import Data.Time.LocalTime.TimeZone.Series
import Data.Text (Text)
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Unsafe
import Data.ByteString.Char8 (unpack)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.HashMap.Strict as HashMap

import Duckling.Core
import Duckling.Types
import Duckling.Resolve
import Duckling.Data.TimeZone
import Duckling.Dimensions.Types


---- Utilities

maybe_to_null :: Maybe a -> IO (StablePtr a)
maybe_to_null = 
  maybe (return $ castPtrToStablePtr (nullPtr :: Ptr a)) newStablePtr


null_to_maybe :: StablePtr a -> IO (Maybe a)
null_to_maybe ptr = 
  if (castStablePtrToPtr ptr == nullPtr) 
  then return Nothing 
  else do 
    value <- deRefStablePtr ptr
    return $ Just value


---- Freeing  
  
foreign export ccall "ducklingFreeObject" freeStablePtr :: StablePtr a -> IO ()

foreign export ccall "ducklingFreeString" free :: CString -> IO ()


---- Dimension

foreign export ccall ducklingParseDimension :: CString -> IO (StablePtr (Some Dimension))
ducklingParseDimension :: CString -> IO (StablePtr (Some Dimension))
ducklingParseDimension c_dimension_name = do
    dimension_name <- unsafePackCString c_dimension_name
    maybe_to_null $ parseDimension dimension_name
  where
    parseDimension :: ByteString -> Maybe (Some Dimension)
    parseDimension dimension_name = fromName $ Text.decodeUtf8 dimension_name
    
foreign export ccall ducklingGetDimensionName :: StablePtr (Some Dimension) -> IO CString
ducklingGetDimensionName :: StablePtr (Some Dimension) -> IO CString
ducklingGetDimensionName c_dimension = do
    dimension <- deRefStablePtr c_dimension
    let name = withSome dimension toName
    newCString $ unpack $ Text.encodeUtf8 name
        
foreign export ccall ducklingGetBuiltinDimensionCreditCardNumber :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionCreditCardNumber :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionCreditCardNumber = newStablePtr (This CreditCardNumber)

foreign export ccall ducklingGetBuiltinDimensionDistance :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionDistance :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionDistance = newStablePtr (This Distance)

foreign export ccall ducklingGetBuiltinDimensionDuration :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionDuration :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionDuration = newStablePtr (This Duration)

foreign export ccall ducklingGetBuiltinDimensionEmail :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionEmail :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionEmail = newStablePtr (This Email)

foreign export ccall ducklingGetBuiltinDimensionAmountOfMoney :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionAmountOfMoney :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionAmountOfMoney = newStablePtr (This AmountOfMoney)

foreign export ccall ducklingGetBuiltinDimensionNumeral :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionNumeral :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionNumeral = newStablePtr (This Numeral)

foreign export ccall ducklingGetBuiltinDimensionOrdinal :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionOrdinal :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionOrdinal = newStablePtr (This Ordinal)

foreign export ccall ducklingGetBuiltinDimensionPhoneNumber :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionPhoneNumber :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionPhoneNumber = newStablePtr (This PhoneNumber)

foreign export ccall ducklingGetBuiltinDimensionQuantity :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionQuantity :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionQuantity = newStablePtr (This Quantity)

foreign export ccall ducklingGetBuiltinDimensionTemperature :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionTemperature :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionTemperature = newStablePtr (This Temperature)

foreign export ccall ducklingGetBuiltinDimensionTime :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionTime :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionTime = newStablePtr (This Time)

foreign export ccall ducklingGetBuiltinDimensionUrl :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionUrl :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionUrl = newStablePtr (This Url)

foreign export ccall ducklingGetBuiltinDimensionVolume :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionVolume :: IO (StablePtr (Some Dimension))
ducklingGetBuiltinDimensionVolume = newStablePtr (This Volume)


---- Lang

foreign export ccall ducklingParseLang :: CString -> IO (StablePtr Lang)
ducklingParseLang :: CString -> IO (StablePtr Lang)
ducklingParseLang c_lang_name = do
    lang_name <- unsafePackCString c_lang_name
    maybe_to_null $ parseLang lang_name
  where
    parseLang :: ByteString -> Maybe Lang
    parseLang lang_name = readMaybe $ Text.unpack $ Text.decodeUtf8 lang_name


---- Region

foreign export ccall ducklingParseRegion :: CString -> IO (StablePtr Region)
ducklingParseRegion :: CString -> IO (StablePtr Region)
ducklingParseRegion c_region_name = do
    region_name <- unsafePackCString c_region_name
    maybe_to_null $ parseRegion region_name
  where
    parseRegion :: ByteString -> Maybe Region
    parseRegion region_name = readMaybe $ Text.unpack $ Text.decodeUtf8 region_name


---- Locale

foreign export ccall ducklingMakeLocale :: StablePtr Lang -> StablePtr Region -> IO (StablePtr Locale)
ducklingMakeLocale :: StablePtr Lang -> StablePtr Region -> IO (StablePtr Locale)
ducklingMakeLocale c_lang c_region = do
    lang <- deRefStablePtr c_lang
    region <- null_to_maybe c_region
    newStablePtr $ makeLocale lang region


---- TimeZoneSeriesMap

foreign export ccall ducklingLoadTimeZoneSeriesMap :: CString -> IO (StablePtr (HashMap Text TimeZoneSeries))
ducklingLoadTimeZoneSeriesMap :: CString -> IO (StablePtr (HashMap Text TimeZoneSeries))
ducklingLoadTimeZoneSeriesMap c_base = do
  base <- unsafePackCString c_base
  tzs <- loadTimeZoneSeries $ Text.unpack $ Text.decodeUtf8 base
  newStablePtr tzs


---- TimeZoneSeries

foreign export ccall ducklingGetTimeZoneSeries :: StablePtr (HashMap Text TimeZoneSeries) -> CString -> IO (StablePtr TimeZoneSeries)
ducklingGetTimeZoneSeries :: StablePtr (HashMap Text TimeZoneSeries) -> CString -> IO (StablePtr TimeZoneSeries)
ducklingGetTimeZoneSeries c_map c_name = do
    map <- deRefStablePtr c_map
    name <- unsafePackCString c_name
    maybe_to_null $ HashMap.lookup (Text.decodeUtf8 name) map


---- Time

asDucklingTime :: TimeZoneSeries -> UTCTime -> DucklingTime
asDucklingTime tzs utcTime =
  DucklingTime $ ZoneSeriesTime (toUTC $ utcToLocalTime' tzs utcTime) tzs

foreign export ccall ducklingGetCurrentTime :: StablePtr TimeZoneSeries -> IO (StablePtr DucklingTime)
ducklingGetCurrentTime :: StablePtr TimeZoneSeries -> IO (StablePtr DucklingTime)
ducklingGetCurrentTime c_time_zone_series = do
    tzs <- deRefStablePtr c_time_zone_series 
    utcNow <- getCurrentTime    
    newStablePtr $ asDucklingTime tzs utcNow

foreign export ccall ducklingMakeTime :: CDouble -> StablePtr TimeZoneSeries -> IO (StablePtr DucklingTime)
ducklingMakeTime :: CDouble -> StablePtr TimeZoneSeries -> IO (StablePtr DucklingTime)
ducklingMakeTime c_posix_time c_time_zone_series = do
    tzs <- deRefStablePtr c_time_zone_series 
    let utcTime = posixSecondsToUTCTime $ realToFrac c_posix_time
    newStablePtr $ asDucklingTime tzs utcTime


---- Parsing

foreign export ccall ducklingParseText :: CString -> StablePtr DucklingTime -> StablePtr Locale -> CBool -> Ptr (StablePtr (Some Dimension)) -> IO CString
ducklingParseText :: CString -> StablePtr DucklingTime -> StablePtr Locale -> CBool -> Ptr (StablePtr (Some Dimension)) -> IO CString
ducklingParseText c_string c_reference_time c_locale c_latent c_dimensions_ptr = do
    text <- Text.decodeUtf8 <$> unsafePackCString c_string
    reference_time <- deRefStablePtr c_reference_time
    locale <- deRefStablePtr c_locale
    c_dimensions <- peekArray0 (castPtrToStablePtr nullPtr) c_dimensions_ptr 
    dimensions <- mapM deRefStablePtr c_dimensions
    let 
      latent = toBool c_latent
      context = Context 
        { referenceTime = reference_time
        , locale = locale
        }
      options = Options { withLatent = latent }
      result = parse text context options dimensions
    newCString $ unpack $ LBS.toStrict $ encode result

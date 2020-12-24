module Util where

import Protolude

readInts :: Text -> [Int]
readInts content = catMaybes $ readMaybe . toS <$> lines content

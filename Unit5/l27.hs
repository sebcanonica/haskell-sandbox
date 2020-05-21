reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just (reverse s)

aMaybeString :: Maybe String
aMaybeString = Just "toto"

aReverseMaybeString = reverse <$> (Just "toto")
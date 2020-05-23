import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char(isPunctuation, isSpace)
import Data.Text as T

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_capitalizationInvariant text = normal == upper && normal == lower
  where normal = isPalindrome text
        upper = isPalindrome (T.toUpper text)
        lower = isPalindrome (T.toLower text)

prop_whitespaceInvariant text = isPalindrome text == isPalindrome noSpace
  where noSpace = T.filter (not.isSpace) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000}  prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_capitalizationInvariant
  quickCheck prop_whitespaceInvariant
  putStrLn "done!"
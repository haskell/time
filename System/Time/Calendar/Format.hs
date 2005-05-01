{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Format where

import System.Locale
import Data.Maybe

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
	formatCharacter :: TimeLocale -> Char -> t -> Maybe String

formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime _ [] _ = ""
formatTime locale ('%':c:cs) t = (formatChar c) ++ (formatTime locale cs t) where
	formatChar '%' = "%"
	formatChar 't' = "\t"
	formatChar 'n' = "\n"
	formatChar _ = case (formatCharacter locale c t) of
		Just s -> s
		_ -> ""
formatTime locale (c:cs) t = c:(formatTime locale cs t)

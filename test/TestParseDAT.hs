{-# OPTIONS -ffi -Wall -Werror #-}

module Main where

import Data.Time
import Data.Time.Clock.TAI
import System.IO

hSafeGetContents :: Handle -> IO String
hSafeGetContents h = do
	eof <- hIsEOF h
	if eof
	 then return []
	 else do
		c <- hGetChar h
		cs <- hSafeGetContents h
		return (c:cs)

tods :: [TimeOfDay]
tods = [
	TimeOfDay 0 0 0,
	TimeOfDay 0 0 0.5,
	TimeOfDay 0 0 1,
	TimeOfDay 0 0 1.5,
	TimeOfDay 0 0 2,
	TimeOfDay 23 59 28,
	TimeOfDay 23 59 28.5,
	TimeOfDay 23 59 29,
	TimeOfDay 23 59 29.5,
	TimeOfDay 23 59 30,
	TimeOfDay 23 59 30.5,
	TimeOfDay 23 59 31,
	TimeOfDay 23 59 31.5,
	TimeOfDay 23 59 32,
	TimeOfDay 23 59 59,
	TimeOfDay 23 59 59.5,
	TimeOfDay 23 59 60,
	TimeOfDay 23 59 60.5
	]

times :: [LocalTime]
times =
	fmap (LocalTime (fromGregorian 1998 04 02)) tods ++
	fmap (LocalTime (fromGregorian 1998 12 30)) tods ++
	fmap (LocalTime (fromGregorian 1998 12 31)) tods ++
	fmap (LocalTime (fromGregorian 1999 01 01)) tods ++
	fmap (LocalTime (fromGregorian 1999 01 02)) tods

main :: IO ()
main = do
	h <- openFile "tai-utc.dat" ReadMode
	s <- hSafeGetContents h
	hClose h
	let lst = parseTAIUTCDATFile s
	mapM_ (\lt -> do
		let utcTime = localTimeToUTC utc lt
		let taiTime = utcToTAITime lst utcTime
		let utcTime' = taiToUTCTime lst taiTime
		if utcTime == utcTime'
		 then putStrLn ((show utcTime) ++ " == " ++ (show taiTime))
		 else putStrLn ("correction: " ++ (show utcTime) ++ " -> " ++ (show taiTime) ++ " -> " ++ (show utcTime'))
		) times

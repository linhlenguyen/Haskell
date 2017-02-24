module PPM3  (make_ppm, save_ppm) where
 
import Colour
 
save_ppm :: FilePath -> [[Colour]] -> IO ()
save_ppm f css = writeFile f $ make_ppm css
 
make_ppm :: [[Colour]] -> String
make_ppm css =
  "P3\n" ++ (show $ length $ head css) ++ " " ++ (show $ length css) ++ " 255\n" ++
  (unlines $ map unwords $ group 15 $ map show $ concatMap colour $ concat css)
 
group _ [] = []
group n xs =
  let (xs0,xs1) = splitAt n xs
  in  xs0 : group n xs1
 
colour (Colour r g b) = [channel r, channel g, channel b]
 
channel :: Double -> Int
channel = floor . (255*) . min 1 . max 0